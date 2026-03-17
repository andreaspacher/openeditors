"""
PLOS editor scraping — parse_plos() + run_plos()

Drop parse_plos into the STRATEGIES dict and call run_plos() from run()
when publisher == "plos" (analogous to run_peerj / run_elife).

URL suffixes tried per journal, in order:
  1. /s/editors-in-chief        → role "Editor-in-Chief"
  2. /s/section-editors         → role "Section Editor"
  3. /s/staff-editors           → role varies / "Staff Editor"
  4. /static/editorial-board    → role "Academic Editor"  (dynamic, paginated)
  5. /s/editorial-board         → role "Academic Editor"
  6. /s/advisory-groups         → role "Advisor"
"""

import logging
import re
import time
from bs4 import BeautifulSoup, NavigableString, Tag
from playwright.sync_api import sync_playwright

LOG = logging.getLogger("openeditors.editors")

# ---------------------------------------------------------------------------
# helpers
# ---------------------------------------------------------------------------

_ORCID_RE = re.compile(r"orcid\.org/(\d{4}-\d{4}-\d{4}-\d{3}[\dX])", re.I)


def _orcid_from_url(href: str) -> str | None:
    if not href:
        return None
    m = _ORCID_RE.search(href)
    return m.group(1) if m else None


def _orcid_from_tag(tag) -> str | None:
    """Return the first ORCID ID found in any <a href> inside *tag*."""
    for a in tag.find_all("a", href=True):
        oid = _orcid_from_url(a["href"])
        if oid:
            return oid
    return None


def _text(tag) -> str:
    return tag.get_text(" ", strip=True) if tag else ""


# ---------------------------------------------------------------------------
# 1.  /s/editors-in-chief
# ---------------------------------------------------------------------------

def _parse_editors_in_chief(html: str) -> list[dict]:
    soup = BeautifulSoup(html, "lxml")
    results = []
    seen = set()
    for h2 in soup.select(".lemur-content article h2"):
        name = _text(h2)
        if name and name not in seen:
            seen.add(name)
            results.append({
                "role": "Editor-in-Chief",
                "full_name": name,
                "orcid": None,
                "affiliation": None,
                "country": None,
                "raw_text": str(h2),
            })
    return results


# ---------------------------------------------------------------------------
# 2.  /s/section-editors
# ---------------------------------------------------------------------------

def _parse_section_editors(html: str) -> list[dict]:
    """
    Each editor lives in a <tr> that contains an <h4> for the name.
    Rows without <h4> are section headings — skip them.
    Structure inside the <td> next to the photo:
        <h4>Name</h4>
        <p>
          [orcid img/link]<br>
          Affiliation<br>
          Country<br>
          Section: …
        </p>
    """
    soup = BeautifulSoup(html, "lxml")
    results = []

    for tr in soup.select(".table-basic tr"):
        h4 = tr.find("h4")
        if not h4:
            continue  # section-label row

        name = _text(h4)
        if not name:
            continue

        orcid = _orcid_from_tag(tr)

        # The <p> after h4 holds affiliation / country
        p = h4.find_next_sibling("p")
        affiliation = country = None
        if p:
            # Collect bare-text pieces separated by <br>
            chunks = []
            for node in p.children:
                if isinstance(node, NavigableString):
                    t = node.strip()
                    if t and t != "\xa0":
                        chunks.append(t)
                elif getattr(node, "name", None) == "br":
                    pass  # separator — we already collected preceding text
                elif getattr(node, "name", None) == "a":
                    pass  # orcid link — already handled
                elif getattr(node, "name", None) == "img":
                    pass
                else:
                    t = node.get_text(" ", strip=True)
                    if t and t != "\xa0" and not t.lower().startswith("section"):
                        chunks.append(t)

            # Re-split on <br> properly
            br_chunks = []
            buf = []
            for node in p.children:
                if getattr(node, "name", None) == "br":
                    t = " ".join(buf).strip()
                    if t:
                        br_chunks.append(t)
                    buf = []
                elif isinstance(node, NavigableString):
                    t = node.strip().strip("\xa0")
                    if t:
                        buf.append(t)
                elif getattr(node, "name", None) not in ("img", "a"):
                    t = node.get_text(" ", strip=True)
                    if t:
                        buf.append(t)
            if buf:
                t = " ".join(buf).strip()
                if t:
                    br_chunks.append(t)

            # Filter out orcid-id lines and "Section:…" lines
            clean = [
                c for c in br_chunks
                if not _ORCID_RE.search(c) and not c.lower().startswith("section")
            ]
            if len(clean) >= 1:
                affiliation = clean[0]
            if len(clean) >= 2:
                country = clean[1]

        results.append({
            "role": "Section Editor",
            "full_name": name,
            "orcid": orcid,
            "affiliation": affiliation,
            "country": country,
            "raw_text": str(tr),
        })

    return results


# ---------------------------------------------------------------------------
# 3.  /s/staff-editors
# ---------------------------------------------------------------------------

def _parse_staff_editors(html: str) -> list[dict]:
    """
    Same table structure as section-editors but:
      - NAME is in <h2> (not <h4>)
      - ROLE is in <em>; fallback "Staff Editor"
    """
    soup = BeautifulSoup(html, "lxml")
    results = []

    for tr in soup.select(".table-basic tr"):
        h2 = tr.find("h2")
        if not h2:
            continue

        name = _text(h2)
        if not name:
            continue

        em = tr.find("em")
        role = _text(em) if em else "Staff Editor"
        if not role:
            role = "Staff Editor"

        orcid = _orcid_from_tag(tr)

        results.append({
            "role": role,
            "full_name": name,
            "orcid": orcid,
            "affiliation": None,
            "country": None,
            "raw_text": str(tr),
        })

    return results


# ---------------------------------------------------------------------------
# 4.  /s/editorial-board  (static version)
# ---------------------------------------------------------------------------

def _parse_editorial_board_static(html: str) -> list[dict]:
    """
    Editors are in div.lemur-content h4 elements.
    The <p> immediately following each h4 may contain:
      - an orcid link
      - an <em> with "Affiliation, Country"
    """
    soup = BeautifulSoup(html, "lxml")
    results = []

    container = soup.select_one("div.lemur-content")
    if not container:
        return results

    for h4 in container.find_all("h4"):
        name = _text(h4)
        if not name:
            continue

        orcid = None
        affiliation = None
        country = None

        p = h4.find_next_sibling()
        # skip non-<p> siblings
        while p and p.name not in ("p", "h4", "h2", "h3"):
            p = p.find_next_sibling()

        if p and p.name == "p":
            orcid = _orcid_from_tag(p)

            em = p.find("em")
            if em:
                aff_text = _text(em)
                # "Affiliation, Country" or "Affiliation Country" separated by comma
                if "," in aff_text:
                    parts = [x.strip() for x in aff_text.rsplit(",", 1)]
                    affiliation = parts[0]
                    country = parts[1]
                else:
                    affiliation = aff_text

        results.append({
            "role": "Academic Editor",
            "full_name": name,
            "orcid": orcid,
            "affiliation": affiliation,
            "country": country,
            "raw_text": str(h4),
        })

    return results


# ---------------------------------------------------------------------------
# 5.  /static/editorial-board  (dynamic, paginated — requires real browser)
# ---------------------------------------------------------------------------

def _parse_editorial_board_dynamic_page(html: str) -> list[dict]:
    """Parse one page of the dynamic editorial board."""
    soup = BeautifulSoup(html, "lxml")
    results = []

    for li in soup.select("ul.editors-list li"):
        strong = li.find("strong")
        if not strong:
            continue

        name = _text(strong)
        if not name:
            continue

        # Role: if <i class="section-editor-icon"> present, or " Section Editor"
        # suffix in the strong's next sibling text
        role = "Academic Editor"
        i_tag = li.find("i", class_="section-editor-icon")
        if i_tag:
            role = "Section Editor"
        else:
            # check text node right after </strong>
            after = strong.next_sibling
            if isinstance(after, NavigableString) and "section editor" in after.lower():
                role = "Section Editor"

        orcid = _orcid_from_tag(li)

        # Affiliation and COUNTRY (all-caps) are bare text nodes after the last <br>
        # Strategy: collect all text nodes split by <br>
        br_chunks = []
        buf = []
        for node in li.children:
            if getattr(node, "name", None) == "br":
                t = " ".join(buf).strip()
                if t:
                    br_chunks.append(t)
                buf = []
            elif isinstance(node, NavigableString):
                t = node.strip()
                if t:
                    buf.append(t)
            elif getattr(node, "name", None) in ("strong", "i", "b"):
                pass  # skip name / icon / bold labels
            elif getattr(node, "name", None) == "span":
                pass  # orcid span — handled separately
            else:
                t = node.get_text(" ", strip=True)
                if t:
                    buf.append(t)
        if buf:
            br_chunks.append(" ".join(buf).strip())

        # Filter: drop orcid lines, "Sections:", "Classifications:" lines
        clean = []
        for c in br_chunks:
            if _ORCID_RE.search(c):
                continue
            if c.lower().startswith("sections:") or c.lower().startswith("classifications:"):
                continue
            if c == name:
                continue
            clean.append(c)

        affiliation = None
        country = None
        if clean:
            affiliation = clean[0]
        if len(clean) >= 2:
            candidate = clean[1]
            # COUNTRY is in ALL CAPS per the spec
            if candidate == candidate.upper() and len(candidate) > 1:
                country = candidate.title()  # store in title case
            else:
                country = candidate

        results.append({
            "role": role,
            "full_name": name,
            "orcid": orcid,
            "affiliation": affiliation,
            "country": country,
            "raw_text": str(li),
        })

    return results


def _total_pages_dynamic(html: str, per_page: int = 50) -> int:
    soup = BeautifulSoup(html, "lxml")
    p = soup.select_one("p.counters")
    if not p:
        return 1
    m = re.search(r"of\s+(\d+)\s+Editor", p.get_text())
    if not m:
        return 1
    total = int(m.group(1))
    return max(1, -(-total // per_page))  # ceiling division


def _fetch_editorial_board_dynamic(url: str, user_agent: str | None, delay: float) -> list[dict]:
    """
    Open a real (non-headless) Playwright browser, paginate through
    /static/editorial-board and collect all editors.
    """
    results = []

    with sync_playwright() as p:
        browser = p.chromium.launch(
            headless=True,
            args=["--disable-blink-features=AutomationControlled"],
        )
        context = browser.new_context(
            user_agent=user_agent,
            viewport={"width": 1280, "height": 900},
            extra_http_headers={"Accept-Language": "en-US,en;q=0.9"},
        )
        page = context.new_page()
        page.add_init_script(
            "Object.defineProperty(navigator, 'webdriver', {get: () => undefined});"
        )

        LOG.info(f"  [dynamic] navigating to {url}")
        page.goto(url, timeout=60000, wait_until="domcontentloaded")
        time.sleep(max(delay, 3))

        # Dismiss cookie consent banner if present
        try:
            cookie_btn = page.locator("#cookie-consent button").first
            cookie_btn.wait_for(state="visible", timeout=5000)
            cookie_btn.click()
            time.sleep(1)
            LOG.info("  [dynamic] dismissed cookie consent")
        except Exception:
            pass  # no banner, continue

        # Determine total pages from first load
        html = page.content()
        n_pages = _total_pages_dynamic(html)
        LOG.info(f"  [dynamic] {n_pages} page(s) to scrape")

        results.extend(_parse_editorial_board_dynamic_page(html))

        for pg in range(2, n_pages + 1):
            # Click the "next page" button
            try:
                next_btn = page.locator('button[data-testid="page-next"]').first
                next_btn.wait_for(state="visible", timeout=10000)
                next_btn.click()
                time.sleep(max(delay, 3))
                html = page.content()
                results.extend(_parse_editorial_board_dynamic_page(html))
                LOG.info(f"  [dynamic] page {pg}/{n_pages}: {len(results)} editors so far")
            except Exception as e:
                LOG.warning(f"  [dynamic] could not advance to page {pg}: {e}")
                break

        browser.close()

    return results


# ---------------------------------------------------------------------------
# 6.  /s/advisory-groups
# ---------------------------------------------------------------------------

def _parse_advisory_groups(html: str) -> list[dict]:
    """
    NAME in <h4>, followed by a <p> whose first text node is the AFFILIATION
    and whose <br>-separated second chunk is the COUNTRY.
    """
    soup = BeautifulSoup(html, "lxml")
    results = []

    for h4 in soup.find_all("h4"):
        name = _text(h4)
        if not name:
            continue

        affiliation = None
        country = None

        p = h4.find_next_sibling("p")
        if p:
            br_chunks = []
            buf = []
            for node in p.children:
                if getattr(node, "name", None) == "br":
                    t = " ".join(buf).strip()
                    if t:
                        br_chunks.append(t)
                    buf = []
                elif isinstance(node, NavigableString):
                    t = node.strip()
                    if t and t != "\xa0":
                        buf.append(t)
                else:
                    t = node.get_text(" ", strip=True)
                    if t:
                        buf.append(t)
            if buf:
                br_chunks.append(" ".join(buf).strip())

            if br_chunks:
                affiliation = br_chunks[0]
            if len(br_chunks) >= 2:
                country = br_chunks[1]

        results.append({
            "role": "Advisor",
            "full_name": name,
            "orcid": None,
            "affiliation": affiliation,
            "country": country,
            "raw_text": str(h4),
        })

    return results


# ---------------------------------------------------------------------------
# Top-level dispatcher  (registered as STRATEGIES["plos"])
# ---------------------------------------------------------------------------

def parse_plos(html: str) -> list[dict]:
    """
    parse_plos is a stub — it is never called directly by the standard loop.
    All real work happens in run_plos(), which fetches each URL suffix
    independently and dispatches to the right sub-parser.
    """
    return []


# ---------------------------------------------------------------------------
# run_plos — replaces the standard per-journal loop for PLOS
# ---------------------------------------------------------------------------

PLOS_SUFFIXES = [
    ("/s/editors-in-chief",     "eic"),
    ("/s/section-editors",      "section"),
    ("/s/staff-editors",        "staff"),
    ("/static/editorial-board", "dynamic"),
    ("/s/editorial-board",      "static_board"),
    ("/s/advisory-groups",      "advisory"),
]


def run_plos(cfg, cur, run_id, session):
    """
    Orchestrates PLOS scraping across all journals and all URL suffixes.
    Returns (total_inserted, total_updated, total_removed).
    """
    from datetime import datetime
    # Import reconcile helper from the main script's namespace at call time
    # (avoids circular-import issues when this file is imported).
    from importlib import import_module
    main = import_module("02_scrape_editors")  # adjust if module name differs

    ua = None
    for k, v in cfg.get("fetch", {}).get("headers", {}).items():
        if k == "user_agent":
            ua = v
    delay = cfg["fetch"]["rate_limit"]["min_seconds_per_request"]

    cur.execute(
        "SELECT journal_id, journal, url FROM journals WHERE publisher_key = %s",
        (cfg["publisher"]["key"],),
    )
    journals = cur.fetchall()
    LOG.info(f"PLOS: {len(journals)} journal(s)")

    total_inserted = total_updated = total_removed = 0

    for journal in journals:
        journal_id = journal["journal_id"]
        journal_url = journal["url"].rstrip("/")
        journal_name = journal["journal"]
        LOG.info(f"  Journal: {journal_name} ({journal_url})")

        all_editors: list[dict] = []

        for suffix, kind in PLOS_SUFFIXES:
            url = journal_url + suffix
            LOG.info(f"    → trying {url}")

            try:
                if kind == "dynamic":
                    editors = _fetch_editorial_board_dynamic(url, ua, delay)
                else:
                    # Use headless browser (same as fetch_page with mode=browser)
                    from fetch_browser import fetch_browser
                    html, tmp, _ = fetch_browser(url, user_agent=ua, headless=True, human_warmup=True)
                    if tmp and tmp.exists():
                        tmp.unlink()

                    # Quick 404 / not-found check
                    soup_check = BeautifulSoup(html, "lxml")
                    title = soup_check.find("title")
                    title_text = _text(title).lower() if title else ""
                    body_text = soup_check.get_text(" ", strip=True).lower()
                    if "404" in title_text or "page not found" in title_text:
                        LOG.info(f"      404 — skipping")
                        time.sleep(delay)
                        continue
                    # Heuristic: if page has very little content it's likely a redirect/404
                    if len(body_text) < 200 and "not found" in body_text:
                        LOG.info(f"      looks like 404 — skipping")
                        time.sleep(delay)
                        continue

                    if kind == "eic":
                        editors = _parse_editors_in_chief(html)
                    elif kind == "section":
                        editors = _parse_section_editors(html)
                    elif kind == "staff":
                        editors = _parse_staff_editors(html)
                    elif kind == "static_board":
                        editors = _parse_editorial_board_static(html)
                    elif kind == "advisory":
                        editors = _parse_advisory_groups(html)
                    else:
                        editors = []

                    time.sleep(delay)

            except Exception as e:
                LOG.warning(f"      Error fetching {url}: {e}")
                continue

            LOG.info(f"      {len(editors)} editor(s) parsed")
            all_editors.extend(editors)

        LOG.info(f"    Total for {journal_name}: {len(all_editors)} editor(s)")

        ins, upd, rem = main.reconcile_editors(
            cur, journal_id, all_editors, run_id, datetime.utcnow()
        )
        LOG.info(f"    +{ins} new, {upd} confirmed, -{rem} removed")
        total_inserted += ins
        total_updated += upd
        total_removed += rem

    return total_inserted, total_updated, total_removed