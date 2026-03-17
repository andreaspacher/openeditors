import argparse
import time
import re
import os
import logging
import yaml
import requests
import pymysql
from datetime import datetime
from bs4 import BeautifulSoup
from dotenv import load_dotenv
from pathlib import Path
from fetch_browser import fetch_browser
from playwright.sync_api import sync_playwright
from parsers import (
    STRATEGIES,
    run_elife,
    run_peerj,
    run_plos,
    run_omics,
    run_longdom,
    run_scitechnol,
    run_imedpub,
    run_scirp,
    slugify,
    fetch_page_scrolling
)

load_dotenv()

LOG = logging.getLogger("openeditors.editors")
logging.basicConfig(level=logging.INFO, format="%(asctime)s | %(levelname)s | %(message)s")

# ------------------------------------------------------------
# db
# ------------------------------------------------------------

def db_connect():
    return pymysql.connect(
        host=os.getenv("DB_HOST"),
        port=int(os.getenv("DB_PORT", 3306)),
        user=os.getenv("DB_USER"),
        password=os.getenv("DB_PASSWORD"),
        database=os.getenv("DB_NAME"),
        charset="utf8mb4",
        autocommit=True,
        cursorclass=pymysql.cursors.DictCursor,
    )

# ------------------------------------------------------------
# config
# ------------------------------------------------------------

def load_config(publisher):
    path = Path(f"publisher_configs/{publisher}.yml")
    with open(path, "r", encoding="utf-8") as f:
        return yaml.safe_load(f)

# ------------------------------------------------------------
# fetch
# ------------------------------------------------------------

def fetch_page(cfg, session, url, wait_for_selector=None):
    if cfg.get("editors", {}).get("scroll", {}).get("enabled"):
        return fetch_page_scrolling(cfg, url)

    mode  = cfg.get("fetch", {}).get("mode", "http")
    delay = cfg["fetch"]["rate_limit"]["min_seconds_per_request"]
    headers = {}
    for k, v in cfg.get("fetch", {}).get("headers", {}).items():
        headers["User-Agent" if k == "user_agent" else k] = v

    if mode == "browser":
        ua = headers.get("User-Agent")
        html, tmp, _ = fetch_browser(url, user_agent=ua, headless=True,
                                     human_warmup=True,
                                     wait_for_selector=wait_for_selector)
        if tmp and tmp.exists():
            tmp.unlink()
    else:
        LOG.info(f"GET {url}")
        r = session.get(url, headers=headers, timeout=30)
        r.raise_for_status()
        html = r.text

    time.sleep(delay)
    return html

# ------------------------------------------------------------
# find the editorial board section
# ------------------------------------------------------------

def find_board_content(soup, editors_cfg) -> str | None:
    board_cfg = editors_cfg.get("board_section", {})

    # Direct selector mode (e.g. CUP)
    direct_sel = board_cfg.get("content_selector")
    if direct_sel and not board_cfg.get("title_selector"):
        el = soup.select_one(direct_sel)
        return str(el) if el else None

    # Existing accordion logic
    title_sel = board_cfg.get("title_selector", "div.accordion_title")
    title_text = board_cfg.get("title_text", "Editorial Board")
    content_sel = board_cfg.get("content_selector", "div.accordion_content")

    for title_el in soup.select(title_sel):
        if title_text.lower() in title_el.get_text(strip=True).lower():
            parent = title_el.find_parent()
            if parent and parent in soup.select(content_sel):
                return str(parent)
            for sibling in title_el.find_next_siblings():
                if sibling in soup.select(content_sel):
                    return str(sibling)
            break

    return None

# ------------------------------------------------------------
# scrape run tracking
# ------------------------------------------------------------

def start_run(cur, publisher):
    cur.execute(
        "INSERT INTO scrape_runs (publisher_key, started_at) VALUES (%s, %s)",
        (publisher, datetime.utcnow()),
    )
    return cur.lastrowid

def finish_run(cur, run_id, inserted, updated, removed):
    cur.execute(
        "UPDATE scrape_runs SET finished_at = %s, records_inserted = %s, "
        "records_updated = %s, records_removed = %s WHERE run_id = %s",
        (datetime.utcnow(), inserted, updated, removed, run_id),
    )

# ------------------------------------------------------------
# Tracking changes
# ------------------------------------------------------------
CONFIRMATION_STREAK = 3  # consecutive runs required before committing

### How this works:
# Run 1: editor "Jane Doe" missing → pending(remove, streak=1)
# Run 2: still missing             → pending(remove, streak=2)
# Run 3: still missing             → streak=3 → COMMIT removed_at ✓
#
# Run 1: missing → streak=1
# Run 2: FOUND again               → clear_pending(remove) → streak reset
# Run 3: missing → streak=1  (back to square one, no false removal)
###

def reconcile_editors(cur, journal_id, scraped_editors, run_id, scraped_at):
    inserted = updated = removed = 0

    # ── 1. Load currently active editors ──────────────────────────────
    cur.execute(
        "SELECT editor_id, full_name FROM editors "
        "WHERE journal_id = %s AND removed_at IS NULL",
        (journal_id,)
    )
    active = {row["full_name"]: row["editor_id"] for row in cur.fetchall()}

    scraped = {ed["full_name"]: ed for ed in scraped_editors if ed.get("full_name")}
    scraped_names = set(scraped)

    # ── 2. Load pending changes for this journal ───────────────────────
    cur.execute(
        "SELECT id, full_name, change_type, streak, first_run_id "
        "FROM pending_changes WHERE journal_id = %s",
        (journal_id,)
    )
    pending = {
        (row["full_name"], row["change_type"]): row
        for row in cur.fetchall()
    }

    # ── helpers ────────────────────────────────────────────────────────
    def upsert_pending(name, change_type):
        key = (name, change_type)
        if key in pending:
            new_streak = pending[key]["streak"] + 1
            cur.execute(
                "UPDATE pending_changes "
                "SET streak=%s, last_run_id=%s, updated_at=%s "
                "WHERE id=%s",
                (new_streak, run_id, scraped_at, pending[key]["id"])
            )
            return new_streak
        else:
            cur.execute(
                "INSERT INTO pending_changes "
                "(journal_id, full_name, change_type, "
                " first_run_id, last_run_id, streak, created_at, updated_at) "
                "VALUES (%s,%s,%s,%s,%s,1,%s,%s)",
                (journal_id, name, change_type,
                 run_id, run_id, scraped_at, scraped_at)
            )
            return 1

    def clear_pending(name, change_type):
        cur.execute(
            "DELETE FROM pending_changes "
            "WHERE journal_id=%s AND full_name=%s AND change_type=%s",
            (journal_id, name, change_type)
        )

    # ── 3. Candidates for REMOVAL (active but not scraped) ────────────
    gone = active.keys() - scraped_names
    for name in gone:
        streak = upsert_pending(name, "remove")
        if streak >= CONFIRMATION_STREAK:
            cur.execute(
                "UPDATE editors SET removed_at=%s "
                "WHERE journal_id=%s AND full_name=%s AND removed_at IS NULL",
                (scraped_at, journal_id, name)
            )
            clear_pending(name, "remove")
            removed += 1
        # Cancel any competing "add" pending for this name
        clear_pending(name, "add")

    # ── 4. Candidates for ADDITION (scraped but not active) ───────────
    new_names = scraped_names - active.keys()
    for name in new_names:
        ed = scraped[name]
        streak = upsert_pending(name, "add")
        if streak >= CONFIRMATION_STREAK:
            cur.execute(
                """
                INSERT IGNORE INTO editors
                  (journal_id, full_name, role, affiliation, country, email,
                   raw_text, scraped_at, first_seen_at, last_seen_at,
                   first_run_id, last_run_id, extracted_run_id)
                VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)
                """,
                (journal_id, name,
                 ed["role"], ed["affiliation"], ed.get("country"), ed.get("email"),
                 ed.get("raw_text"),
                 scraped_at, scraped_at, scraped_at,
                 run_id, run_id, run_id)
            )
            clear_pending(name, "add")
            inserted += 1
        # Cancel any competing "remove" pending for this name
        clear_pending(name, "remove")

    # ── 5. Still present → bump last_seen_at, cancel any remove-pending ─
    still_present = scraped_names & active.keys()
    if still_present:
        placeholders = ",".join(["%s"] * len(still_present))
        cur.execute(
            f"UPDATE editors "
            f"SET last_seen_at=%s, last_run_id=%s, scraped_at=%s "
            f"WHERE journal_id=%s AND full_name IN ({placeholders}) "
            f"AND removed_at IS NULL",
            (scraped_at, run_id, scraped_at, journal_id, *still_present)
        )
        updated = cur.rowcount
        for name in still_present:
            clear_pending(name, "remove")  # reappeared → reset strike

    return inserted, updated, removed


# ------------------------------------------------------------
# screenshot for debugging
# ------------------------------------------------------------
def take_screenshot(url, output_path):
    """Take a screenshot of a URL using Playwright"""
    try:
        with sync_playwright() as p:
            browser = p.chromium.launch(headless=True)
            page = browser.new_page()
            page.goto(url, wait_until="domcontentloaded", timeout=60000)
            page.screenshot(path=output_path, full_page=True)
            browser.close()
        LOG.info(f"Screenshot saved to {output_path}")
        return True
    except Exception as e:
        LOG.error(f"Failed to take screenshot of {url}: {e}")
        return False
# ------------------------------------------------------------
# function to replace URLs (e.g., for ASCE, SAGE)
# ------------------------------------------------------------
def build_target_url(url_pattern: str, journal_url: str, editors_cfg: dict) -> str:
    slug_cfg = editors_cfg.get("url_slug_extract")
    url_template = editors_cfg.get("url_template")
    if slug_cfg and url_template:
        match = re.search(slug_cfg["regex"], journal_url)
        journal_slug = match.group(1) if match else ""
        return url_template.replace("{journal_slug}", journal_slug)
    if slug_cfg:
        match = re.search(slug_cfg["regex"], journal_url)
        journal_slug = match.group(1) if match else ""
        return url_pattern.replace("{journal_slug}", journal_slug)
    return url_pattern.replace("{journal_url}", journal_url)

# ------------------------------------------------------------
# main
# ------------------------------------------------------------

def run(publisher, debug=False, limit=None, startingjournal=1):
    cfg = load_config(publisher)
    editors_cfg = cfg.get("editors", {})

    conn = db_connect()
    cur = conn.cursor()

    run_id = start_run(cur, publisher)
    LOG.info(f"Scrape run {run_id} started for publisher '{publisher}'")

    session = requests.Session()

    # --- Section-slug publishers (e.g. eLife) ---
    if "section_slugs" in editors_cfg:
        ins, upd, rem = run_elife(
            cfg, cur, run_id, session,
            strategies=STRATEGIES,
            fetch_page_fn=fetch_page,
            reconcile_fn=reconcile_editors,
            take_screenshot_fn=take_screenshot,
            debug=debug
        )
    elif editors_cfg.get("pagination", {}).get("strategy") == "peerj":
        ins, upd, rem = run_peerj(cfg, cur, run_id, session, reconcile_fn=reconcile_editors, fetch_browser_fn = fetch_browser)
        finish_run(cur, run_id, ins, upd, rem)
        LOG.info(f"Done. +{ins} new, {upd} confirmed, -{rem} removed. Run {run_id} closed.")
        return
    elif cfg["publisher"]["key"] == "plos":      
        ins, upd, rem = run_plos(cfg, cur, run_id, session)
        finish_run(cur, run_id, ins, upd, rem)
        LOG.info(...)
        return
    elif cfg["publisher"]["key"] == "omics":
        ins, upd, rem = run_omics(cfg, cur, run_id, session,
                    fetch_page_fn=fetch_page,
                    reconcile_fn=reconcile_editors,
                    finish_run_fn=finish_run,
                    debug=debug, limit=limit, startingjournal=startingjournal)
        finish_run(cur, run_id, ins, upd, rem)
        LOG.info(...)
        return
    elif cfg["publisher"]["key"] == "longdom":
        run_longdom(cfg, cur, run_id, session,
                    fetch_page_fn=fetch_page,
                    reconcile_fn=reconcile_editors,
                    finish_run_fn=finish_run,
                    debug=debug, limit=limit, startingjournal=startingjournal)
        return
    elif cfg["publisher"]["key"] == "scitechnol":
        run_scitechnol(cfg, cur, run_id, session,
                    fetch_page_fn=fetch_page,
                    reconcile_fn=reconcile_editors,
                    finish_run_fn=finish_run,
                    debug=debug, limit=limit, startingjournal=startingjournal)
        return
    elif cfg["publisher"]["key"] == "iMedPub":
        run_imedpub(cfg, cur, run_id, session,
                    fetch_page_fn=fetch_page,
                    reconcile_fn=reconcile_editors,
                    finish_run_fn=finish_run,
                    debug=debug, limit=limit, startingjournal=startingjournal)
        return
    elif cfg["publisher"]["key"] == "scirp":
        run_scirp(cfg, cur, run_id, session,
                    fetch_page_fn=fetch_page,
                    reconcile_fn=reconcile_editors,
                    finish_run_fn=finish_run,
                    debug=debug, limit=limit, startingjournal=startingjournal)
        return

    # --- Standard per-journal loop ---
    strategy_name = editors_cfg.get("parsing", {}).get("strategy", "h2_then_p")
    parser = STRATEGIES[strategy_name]
    url_pattern = editors_cfg.get("url_pattern", "{journal_url}")

    cur.execute(
        "SELECT journal_id, journal, url FROM journals WHERE publisher_key = %s",
        (cfg["publisher"]["key"],),
    )
    journals = cur.fetchall()
    journals = journals[startingjournal - 1:]
    LOG.info(f"Found {len(journals)} journals to process (starting from #{startingjournal})")

    total_inserted = total_updated = total_removed = 0

    for i, journal in enumerate(journals):
        if limit is not None and i >= limit:
            break

        journal_id = journal["journal_id"]
        journal_url = journal["url"]
        journal_name = journal["journal"]

        LOG.info(f"[{i+1}/{len(journals)}] {journal['journal']} → {journal_url}")

        # Skip non-ScienceDirect URLs
        if editors_cfg.get("filter_url_contains") and \
           editors_cfg["filter_url_contains"] not in journal_url:
            LOG.info(f"  → Skipping (not a ScienceDirect URL)")
            continue

        # Resolve redirect for publishers like Elsevier
        if editors_cfg.get("resolve_redirect"):
            # Create slug from journal name
            slug = slugify(journal_name)
            
            # Build the target URL using the slug
            target_url = f"https://www.sciencedirect.com/journal/{slug}/about/editorial-board"
        else:
            target_url = build_target_url(url_pattern, journal_url, editors_cfg)

        LOG.info(f"  → target: {target_url}")

        try:
            html = fetch_page(cfg, session, target_url)
        except Exception as e:
            LOG.warning(f"Failed to fetch {target_url}: {e}")
            continue

        soup = BeautifulSoup(html, "lxml")
        # Some publishers (e.g. Frontiers) need the full page, not a subsection
        if editors_cfg.get("board_section", {}).get("full_page"):
            board_html = str(soup)
        else:
            board_html = find_board_content(soup, editors_cfg)

        if not board_html:
            LOG.warning(f"No editorial board section found for journal_id={journal_id}")
            continue

        editors = parser(board_html)
        LOG.info(f"  → {len(editors)} editors parsed")

        # Take screenshot if no editors found
        if len(editors) == 0 and debug:
            # Create debug directory if it doesn't exist
            debug_dir = "_debug_screenshots"
            os.makedirs(debug_dir, exist_ok=True)
            
            # Create filename with publisher name
            screenshot_filename = f"zero_{publisher}.png"
            screenshot_path = os.path.join(debug_dir, screenshot_filename)
            
            # Take screenshot
            take_screenshot(target_url, screenshot_path)

        ins, upd, rem = reconcile_editors(cur, journal_id, editors, run_id, datetime.utcnow())
        LOG.info(f"  → +{ins} new, {upd} confirmed, -{rem} removed")
        total_inserted += ins
        total_updated += upd
        total_removed += rem

    finish_run(cur, run_id, total_inserted, total_updated, total_removed)
    LOG.info(f"Done. +{total_inserted} new, {total_updated} confirmed, -{total_removed} removed. Run {run_id} closed.")
    
# ------------------------------------------------------------
# cli
# ------------------------------------------------------------

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--publisher", required=True)
    parser.add_argument("--debug", action="store_true")
    parser.add_argument("--limit", type=int, help="limit number of journals processed")
    parser.add_argument("--startingjournal", type=int, default=1,
                        help="1-based index of journal to start from (default: 1)")
    args = parser.parse_args()

    if args.debug:
        LOG.setLevel(logging.DEBUG)

    run(args.publisher, debug=args.debug, limit=args.limit, startingjournal=args.startingjournal)