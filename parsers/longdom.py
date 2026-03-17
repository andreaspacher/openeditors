import logging
import re
from bs4 import BeautifulSoup

LOG = logging.getLogger("openeditors.editors")

def parse_longdom(container_html: str) -> list[dict]:
    """
    Parses Longdom editorial board listings.

    Structure (main ul > li.list-group-item):
      div.col-sm-10 > p.card-text:
        <strong>Name</strong>, credentials
        <br>Role (optional)
        <br>Title, Department
        <br>University, Country
    """
    soup = BeautifulSoup(container_html, "lxml")
    results = []

    for li in soup.select("main ul li.list-group-item"):
        p = li.select_one("p.card-text")
        if not p:
            continue

        name_tag = p.find("strong")
        if not name_tag:
            continue
        full_name = name_tag.get_text(" ", strip=True).strip()
        if not full_name:
            continue

        for br in p.find_all("br"):
            br.replace_with("\n")
        lines = [
            l.strip().strip("\xa0")
            for l in p.get_text().split("\n")
            if l.strip().replace("\xa0", "").strip()
        ]
        # lines[0] is "Name, credentials" — skip it
        lines = lines[1:]

        # Last two lines = affiliation (e.g. "Title, Dept" + "University, Country")
        affiliation = ", ".join(lines[-2:]) if len(lines) >= 2 else (lines[-1] if lines else None)

        role = "Editorial Board Member"
        role_keywords = ("editor-in-chief", "associate editor", "guest editor",
                         "section editor", "editorial board")
        for line in lines:
            if any(kw in line.lower() for kw in role_keywords):
                role = line.strip()
                break

        results.append({
            "role": role,
            "full_name": full_name,
            "affiliation": affiliation,
        })

    return results

def longdom_editorial_url(journal_url: str) -> str:
    """
    https://www.longdom.org/biochemistry-pharmacology-open-access.html
    → https://www.longdom.org/biochemistry-pharmacology-open-access/editorial-board.html
    """
    return re.sub(r"\.html$", "/editorial-board.html", journal_url)

def run_longdom(cfg, cur, run_id, session, fetch_page_fn, reconcile_fn, finish_run_fn, debug=False, limit=None, startingjournal=1):
    from datetime import datetime

    cur.execute(
        "SELECT journal_id, journal, url FROM journals WHERE publisher_key = %s",
        (cfg["publisher"]["key"],),
    )
    journals = cur.fetchall()[startingjournal - 1:]
    LOG.info(f"Found {len(journals)} Longdom journals (starting from #{startingjournal})")

    total_inserted = total_updated = total_removed = 0

    for i, journal in enumerate(journals):
        if limit is not None and i >= limit:
            break

        target_url = longdom_editorial_url(journal["url"])
        LOG.info(f"[{i+1}/{len(journals)}] {journal['journal']} → {target_url}")

        try:
            html = fetch_page_fn(cfg, session, target_url)
        except Exception as e:
            LOG.warning(f"Failed to fetch {target_url}: {e}")
            continue

        editors = parse_longdom(html)
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
