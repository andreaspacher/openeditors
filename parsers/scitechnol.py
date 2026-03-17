import logging
from bs4 import BeautifulSoup

LOG = logging.getLogger("openeditors.editors")

def parse_scitechnol(container_html: str) -> list[dict]:
    """
    Parses SciTechnol editorial board listings.

    Structure (div.masonry-grid > div.row):
      h4 a         → NAME
      .team-v3-paragraph  → AFFILIATION (may contain <br> or nested tags)
    """
    soup = BeautifulSoup(container_html, "lxml")
    results = []

    for item in soup.select("div.masonry-grid div.row"):
        name_tag = item.select_one("h4 a")
        if not name_tag:
            continue
        full_name = name_tag.get_text(" ", strip=True).strip()
        if not full_name:
            continue

        affiliation = None
        aff_tag = item.select_one(".team-v3-paragraph")
        if aff_tag:
            # Replace <br> with spaces so multi-line affiliation squishes into one string
            for br in aff_tag.find_all("br"):
                br.replace_with(" ")
            affiliation = " ".join(aff_tag.get_text(" ").split()).strip() or None

        results.append({
            "role": "Editorial Board Member",
            "full_name": full_name,
            "affiliation": affiliation,
        })

    return results

def run_scitechnol(cfg, cur, run_id, session, fetch_page_fn, reconcile_fn, finish_run_fn, debug=False, limit=None, startingjournal=1):
    from datetime import datetime

    cur.execute(
        "SELECT journal_id, journal, url FROM journals WHERE publisher_key = %s",
        (cfg["publisher"]["key"],),
    )
    journals = cur.fetchall()[startingjournal - 1:]
    LOG.info(f"Found {len(journals)} SciTechnol journals (starting from #{startingjournal})")

    total_inserted = total_updated = total_removed = 0

    for i, journal in enumerate(journals):
        if limit is not None and i >= limit:
            break

        target_url = omics_editorial_url(journal["url"])  # same transform as OMICS
        LOG.info(f"[{i+1}/{len(journals)}] {journal['journal']} → {target_url}")

        try:
            html = fetch_page_fn(cfg, session, target_url)
        except Exception as e:
            LOG.warning(f"Failed to fetch {target_url}: {e}")
            continue

        editors = parse_scitechnol(html)
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

def omics_editorial_url(journal_url: str) -> str:
    """
    https://www.omicsonline.org/agrotechnology.php
    → https://www.omicsonline.org/editorialboard-agrotechnology.php
    """
    parts = journal_url.rstrip("/").rsplit("/", 1)
    if len(parts) == 2:
        return f"{parts[0]}/editorialboard-{parts[1]}"
    return journal_url  # fallback: return unchanged