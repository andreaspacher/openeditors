import re
from bs4 import BeautifulSoup, NavigableString
import logging
from urllib.parse import urljoin
LOG = logging.getLogger("openeditors.editors")

def parse_imedpub(container_html: str) -> list[dict]:
    """
    Parses iMedPub editorial board listings.

    Structure (div.col-lg-9 table tr):
      - Role rows:   td > div.editor_type  → sets current role
      - Editor rows: td > <b>Name</b>, affiliation text
    """
    soup = BeautifulSoup(container_html, "lxml")
    results = []
    current_role = "Editorial Board Member"

    for tr in soup.select("div.col-lg-9 table tr"):
        td = tr.find("td")
        if not td:
            continue

        # Role row
        role_tag = td.find("div", class_="editor_type")
        if role_tag:
            current_role = role_tag.get_text(strip=True)
            continue

        # Editor row
        name_tag = td.find("b")
        if not name_tag:
            continue
        full_name = name_tag.get_text(" ", strip=True).strip()
        if not full_name:
            continue

        # Everything after the <b> tag is the affiliation
        # Collect all text nodes and tags after <b>, strip leading comma/nbsp
        affiliation_parts = []
        for node in name_tag.next_siblings:
            if isinstance(node, NavigableString):
                affiliation_parts.append(str(node))
            else:
                affiliation_parts.append(node.get_text(" "))

        affiliation = " ".join(affiliation_parts).strip()
        # Strip leading punctuation artifacts like ",&nbsp;" or ", "
        affiliation = re.sub(r"^[,\s\xa0]+", "", affiliation)
        # Collapse internal whitespace/newlines
        affiliation = " ".join(affiliation.split()).strip() or None

        results.append({
            "role": current_role,
            "full_name": full_name,
            "affiliation": affiliation,
        })

    return results

def run_imedpub(cfg, cur, run_id, session, fetch_page_fn, reconcile_fn, finish_run_fn, debug=False, limit=None, startingjournal=1):
    from datetime import datetime

    cur.execute(
        "SELECT journal_id, journal, url FROM journals WHERE publisher_key = %s",
        (cfg["publisher"]["key"],),
    )
    journals = cur.fetchall()[startingjournal - 1:]
    LOG.info(f"Found {len(journals)} iMedPub journals (starting from #{startingjournal})")

    total_inserted = total_updated = total_removed = 0

    for i, journal in enumerate(journals):
        if limit is not None and i >= limit:
            break

        target_url = journal["url"].rstrip("/") + "/editors.php"
        LOG.info(f"[{i+1}/{len(journals)}] {journal['journal']} → {target_url}")

        try:
            html = fetch_page_fn(cfg, session, target_url)
        except Exception as e:
            LOG.warning(f"Failed to fetch {target_url}: {e}")
            continue

        editors = parse_imedpub(html)
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

    return None
