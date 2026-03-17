import logging
from bs4 import BeautifulSoup

LOG = logging.getLogger("openeditors.editors")

def omics_editorial_url(journal_url: str) -> str:
    """
    https://www.omicsonline.org/agrotechnology.php
    → https://www.omicsonline.org/editorialboard-agrotechnology.php
    """
    parts = journal_url.rstrip("/").rsplit("/", 1)
    if len(parts) == 2:
        return f"{parts[0]}/editorialboard-{parts[1]}"
    return journal_url  # fallback: return unchanged

def run_omics(cfg, cur, run_id, session, fetch_page_fn, reconcile_fn, finish_run_fn, debug=False, limit=None, startingjournal=1):
    from datetime import datetime

    cur.execute(
        "SELECT journal_id, journal, url FROM journals WHERE publisher_key = %s",
        (cfg["publisher"]["key"],),
    )
    journals = cur.fetchall()[startingjournal - 1:]
    LOG.info(f"Found {len(journals)} OMICS journals (starting from #{startingjournal})")

    total_inserted = total_updated = total_removed = 0

    for i, journal in enumerate(journals):
        if limit is not None and i >= limit:
            break

        journal_url = journal["url"]

        if "omicsonline" not in journal_url:
            LOG.info(f"  → Skipping (no 'omicsonline' in URL): {journal_url}")
            continue

        target_url = omics_editorial_url(journal_url)
        LOG.info(f"[{i+1}/{len(journals)}] {journal['journal']} → {target_url}")

        try:
            html = fetch_page_fn(cfg, session, target_url)
        except Exception as e:
            LOG.warning(f"Failed to fetch {target_url}: {e}")
            continue

        editors = parse_omics(html)
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


def parse_omics(container_html: str) -> list[dict]:
    soup = BeautifulSoup(container_html, "lxml")
    results = []

    for tr in soup.select("div.table-responsive table.table tbody tr"):
        tds = tr.select("td")
        td_info = tds[1] if len(tds) > 1 else None
        if not td_info:
            continue

        # --- NAME: try editor-profile link first, then <strong>, then first text line ---
        full_name = None

        profile_link = td_info.find("a", href=lambda h: h and "editor-profile" in h)
        if profile_link:
            full_name = profile_link.get_text(" ", strip=True).strip()

        if not full_name:
            strong = td_info.find("strong")
            if strong:
                full_name = strong.get_text(" ", strip=True).strip()

        if not full_name:
            # Fall back to first non-empty text line in the td
            for br in td_info.find_all("br"):
                br.replace_with("\n")
            first_line = next(
                (l.strip() for l in td_info.get_text().split("\n")
                 if l.strip().replace("\xa0", "")),
                None
            )
            full_name = first_line

        if not full_name:
            continue

        # --- AFFILIATION: td[2] university link, then fall back to lines[-2] ---
        affiliation = None

        if len(tds) > 2:
            uni_link = tds[2].find("a")
            if uni_link:
                affiliation = uni_link.get_text(strip=True)

        if not affiliation:
            p = td_info.find("p")
            if p:
                for br in p.find_all("br"):
                    br.replace_with("\n")
                lines = [
                    l.strip().strip("\xa0")
                    for l in p.get_text().split("\n")
                    if l.strip().replace("\xa0", "").strip()
                ]
                if len(lines) >= 2:
                    affiliation = lines[-2]

        # Also handle the case where content is directly in td (no <p> wrapper)
        if not affiliation:
            for br in td_info.find_all("br"):
                br.replace_with("\n")
            lines = [
                l.strip().strip("\xa0")
                for l in td_info.get_text().split("\n")
                if l.strip().replace("\xa0", "").strip()
            ]
            if len(lines) >= 2:
                affiliation = lines[-2]

        results.append({
            "role": "Editorial Board Member",
            "full_name": full_name,
            "affiliation": affiliation,
        })

    return results
