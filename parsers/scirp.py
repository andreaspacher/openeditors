import logging
import re
import requests
from urllib.parse import urljoin
from bs4 import BeautifulSoup, NavigableString

LOG = logging.getLogger("openeditors.editors")

def parse_scirp(container_html: str) -> list[dict]:
    """
    Parses SCIRP editorial board listings.

    Structure (second div.con_main > div.col-md-12.column):
      h4           → ROLE (e.g. "Editor-in-Chief", "Editorial Board")
      table > tr:
        td[0]      → title prefix (Dr., Prof.) — ignored
        td[1]      → <strong>Name</strong>, Affiliation, Country
    """
    soup = BeautifulSoup(container_html, "lxml")
    results = []

    # Target the second div.con_main block
    con_mains = soup.select("div.con_main")
    if len(con_mains) < 2:
        return results
    block = con_mains[1]

    current_role = "Editorial Board Member"

    for el in block.select("div.col-md-12.column > *"):
        if el.name == "h4":
            role_span = el.find("span")
            current_role = (role_span or el).get_text(strip=True) or current_role
            continue

        if el.name == "table":
            for tr in el.select("tr"):
                tds = tr.find_all("td")
                if len(tds) < 2:
                    continue

                td_info = tds[1]
                name_tag = td_info.find("strong")
                if not name_tag:
                    continue
                full_name = name_tag.get_text(" ", strip=True).strip()
                if not full_name:
                    continue

                # Affiliation: all text in td_info after the <a> wrapping the name
                anchor = td_info.find("a", href=lambda h: h and "editorialboard" in h)
                sibling_start = anchor if anchor else name_tag

                affiliation_parts = []
                for node in sibling_start.next_siblings:
                    if isinstance(node, NavigableString):
                        affiliation_parts.append(str(node))
                    else:
                        affiliation_parts.append(node.get_text(" "))

                affiliation = " ".join(affiliation_parts)
                affiliation = re.sub(r"^[,\s\xa0]+", "", affiliation)
                affiliation = " ".join(affiliation.split()).strip() or None

                results.append({
                    "role": current_role,
                    "full_name": full_name,
                    "affiliation": affiliation,
                })

    return results


def scirp_editorial_url_from_http(journal_url: str, session: requests.Session, headers: dict) -> str | None:
    """
    Fetch the journal page via plain HTTP (bypassing bot detection on headless browser)
    and extract the editorial board URL.
    """
    try:
        r = session.get(journal_url, headers=headers, timeout=30)
        r.raise_for_status()
        soup = BeautifulSoup(r.text, "lxml")
        for a in soup.find_all("a", href=True):
            if "editorialboard" in a["href"]:
                return urljoin(journal_url, a["href"])
    except Exception as e:
        LOG.warning(f"HTTP fetch failed for {journal_url}: {e}")
    return None

def run_scirp(cfg, cur, run_id, session, fetch_page_fn, reconcile_fn, finish_run_fn, debug=False, limit=None, startingjournal=1):
    from datetime import datetime

    cur.execute(
        "SELECT journal_id, journal, url FROM journals WHERE publisher_key = %s",
        (cfg["publisher"]["key"],),
    )
    journals = cur.fetchall()[startingjournal - 1:]
    LOG.info(f"Found {len(journals)} SCIRP journals (starting from #{startingjournal})")

    total_inserted = total_updated = total_removed = 0

    for i, journal in enumerate(journals):
        if limit is not None and i >= limit:
            break

        journal_url = journal["url"]
        slug = journal_url.rstrip("/").rsplit("/", 1)[-1]
        editorial_url = f"https://www.scirp.org/journal/editorialboard?journalabbr={slug}"
        LOG.info(f"[{i+1}/{len(journals)}] {journal['journal']} → {editorial_url}")

        try:
            html = fetch_page_fn(cfg, session, editorial_url)
        except Exception as e:
            LOG.warning(f"Failed to fetch editorial board {editorial_url}: {e}")
            continue

        editors = parse_scirp(html)
        LOG.info(f"  → {len(editors)} editors parsed")

        ins, upd, rem = reconcile_fn(
            cur, journal["journal_id"], editors, run_id, datetime.utcnow()
        )
        LOG.info(f"  → +{ins} new, {upd} confirmed, -{rem} removed")
        total_inserted += ins
        total_updated += upd
        total_removed += rem

    finish_run_fn(cur, run_id, total_inserted, total_updated, total_removed)
    LOG.info(f"Done. +{total_inserted} new, {total_updated} confirmed, -{total_removed} removed. Run {run_id} closed.")
