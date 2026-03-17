import re
from bs4 import BeautifulSoup
import logging
from datetime import datetime
import time
LOG = logging.getLogger("openeditors.editors")

def parse_peerj(container_html: str) -> list[dict]:
    """
    PeerJ structure (.v-main__wrap .col-md-7, multiple .row.mb-8 blocks):
      .text-h6 a                    → name
      .text-muted.mr-1              → affiliation
      .v-chip__content span.d-sm-none → role
    """
    soup = BeautifulSoup(container_html, "lxml")
    results = []

    for card in soup.select(".row.mb-8"):
        name_el = card.select_one(".text-h6 a")
        if not name_el:
            continue
        full_name = re.sub(r"\s{2,}", " ", name_el.get_text(separator=" ", strip=True)).strip() # to add whitespace
        if not full_name:
            continue

        affiliation_el = card.select_one(".text-muted.mr-1")
        affiliation = affiliation_el.get_text(strip=True) if affiliation_el else None

        role_el = card.select_one(".v-chip__content span.d-sm-none")
        role = role_el.get_text(strip=True) if role_el else None

        results.append({
            "role": role,
            "full_name": full_name,
            "affiliation": affiliation,
            "country": None,
            "raw_text": str(card),
        })

    return results

# ------------------------------------------------------------
# function for PeerJ: pagination
# ------------------------------------------------------------
def run_peerj(cfg, cur, run_id, reconcile_fn, fetch_browser_fn, session):
    LOG.info("run_peerj() entered")
    editors_cfg = cfg.get("editors", {})
    journal_map = editors_cfg.get("journal_map", {})
    delay = cfg["fetch"]["rate_limit"]["min_seconds_per_request"]
    ua = cfg.get("fetch", {}).get("headers", {}).get("user_agent")

    cur.execute(
        "SELECT journal_id, journal FROM journals WHERE publisher_key = %s",
        (cfg["publisher"]["key"],),
    )
    journals = cur.fetchall()

    total_inserted = total_updated = total_removed = 0

    for journal in journals:
        journal_id = journal["journal_id"]
        journal_key = journal_map.get(journal["journal"])
        if not journal_key:
            LOG.warning(f"No journal_map entry for '{journal['journal']}', skipping.")
            continue

        base_url = f"https://peerj.com/search/?type=editor&journal={journal_key}"
        LOG.info(f"Scraping {journal['journal']} → {base_url}")

        all_editors = []
        page = 1

        while True:
            url = f"{base_url}&page={page}"
            LOG.info(f"  Fetching page {page}: {url}")
            try:
                html, tmp, _ = fetch_browser_fn(url, user_agent=ua, headless=True, human_warmup=True)
                if tmp and tmp.exists():
                    tmp.unlink()
            except Exception as e:
                LOG.warning(f"  Failed to fetch {url}: {e}")
                break

            soup = BeautifulSoup(html, "lxml")
            container = soup.select_one(".v-main__wrap .col-md-7")
            if not container:
                LOG.warning(f"  No container found on page {page}")
                break

            editors = parse_peerj(str(container))
            LOG.info(f"  → {len(editors)} editors on page {page}")
            if not editors:
                break

            # Insert new editors immediately so progress is saved on interruption,
            # but skip removals until we have the full picture
            scraped_at = datetime.utcnow()
            page_inserted = 0
            for ed in editors:
                name = ed["full_name"]
                if not name:
                    continue
                cur.execute(
                    """
                    INSERT IGNORE INTO editors
                    (journal_id, full_name, given, family, role, affiliation, country, email,
                     raw_text, source_url, scraped_at, first_seen_at, last_seen_at,
                     first_run_id, last_run_id, extracted_run_id)
                    VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)
                    """,
                    (journal_id, name, ed.get("given"), ed.get("family"),
                     ed["role"], ed["affiliation"], ed.get("country"), ed.get("email"),
                     ed.get("raw_text"), ed.get("source_url"),
                     scraped_at, scraped_at, scraped_at,
                     run_id, run_id, run_id)
                )
                if cur.rowcount:
                    page_inserted += 1
            all_editors.extend(editors)
            LOG.info(f"  → +{page_inserted} new inserts from page {page}")

            # Determine total pages from last pagination button
            page_items = soup.select(".v-pagination__item")
            if not page_items:
                break
            try:
                total_pages = int(page_items[-1].get_text(strip=True))
            except (ValueError, IndexError):
                break

            if page >= total_pages:
                break
            page += 1
            time.sleep(delay)

        # All pages done — now reconcile updates and removals against the full set
        LOG.info(f"  Total editors scraped for {journal['journal']}: {len(all_editors)}")
        scraped_at = datetime.utcnow()
        ins, upd, rem = reconcile_fn(cur, journal_id, all_editors, run_id, scraped_at)
        LOG.info(f"  → +{ins} new (reconcile), {upd} confirmed, -{rem} removed")
        total_inserted += ins + page_inserted  # page_inserted only tracks last page here, see note
        total_updated += upd
        total_removed += rem

    return total_inserted, total_updated, total_removed
