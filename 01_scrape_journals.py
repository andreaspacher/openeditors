import argparse
import time
import math
import yaml
import re
import os
import requests
import pymysql
import logging
from bs4 import BeautifulSoup
from dotenv import load_dotenv
from urllib.parse import urljoin
from pathlib import Path
from datetime import date
from fetch_browser import fetch_browser

# ------------------------------------------------------------
# setup
# ------------------------------------------------------------

load_dotenv()

LOG = logging.getLogger("openeditors")

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s | %(levelname)s | %(message)s",
)

ISSN_RE = re.compile(r"\b\d{4}-\d{3}[\dX]\b")

CACHE_DIR = Path("_debug_html")
CACHE_DIR.mkdir(parents=True, exist_ok=True)

SCREENSHOT_DIR = Path("_debug_screenshots")

# ------------------------------------------------------------
# db
# ------------------------------------------------------------

def db_connect():
    return pymysql.connect(
        host=os.getenv("DB_HOST"),
        user=os.getenv("DB_USER"),
        password=os.getenv("DB_PASSWORD"),
        database=os.getenv("DB_NAME"),
        charset="utf8mb4",
        autocommit=True,
        cursorclass=pymysql.cursors.DictCursor
    )

# ------------------------------------------------------------
# config
# ------------------------------------------------------------

def load_config(publisher):
    path = f"publisher_configs/{publisher}.yml"
    with open(path, "r", encoding="utf-8") as f:
        return yaml.safe_load(f)

# ------------------------------------------------------------
# http
# ------------------------------------------------------------

def build_headers(cfg):
    headers = {}
    for k, v in cfg.get("fetch", {}).get("headers", {}).items():
        if k == "user_agent":
            headers["User-Agent"] = v
        else:
            headers[k.replace("_", "-")] = v
    return headers

def fetch(session, url, headers, delay, debug=False):
    LOG.info(f"GET {url}")
    r = session.get(url, headers=headers, timeout=30)
    LOG.info(f"HTTP {r.status_code}")

    if debug:
        fname = CACHE_DIR / f"{re.sub('[^a-zA-Z0-9]', '_', url)[:120]}.html"
        fname.write_text(r.text, encoding="utf-8")
        LOG.debug(f"Saved HTML to {fname}")

    r.raise_for_status()
    time.sleep(delay)
    return r.text

def fetch_page(cfg, session, url, headers, delay, debug=False, headless=True):
    # Check if local file is specified in config
    local_file = cfg.get("journals", {}).get("local_file")
    if local_file:
        LOG.info(f"Using local file override: {local_file}")
        return fetch_local_file(local_file, debug)
    
    mode = cfg.get("fetch", {}).get("mode", "http")
    user_agent = headers.get("User-Agent")

    if mode == "browser":
        human_warmup = cfg.get("fetch", {}).get("human_warmup", True)
        html, screenshot_tmp, _ = fetch_browser(url, user_agent=user_agent, headless=headless, human_warmup=human_warmup)
        return html, screenshot_tmp
    else:
        return fetch(session, url, headers, delay, debug=debug), None

# ------------------------------------------------------------
# parsing helpers
# ------------------------------------------------------------

def extract_total_results(soup, cfg):
    sel = cfg["journals"]["pagination"]["total_results"]["selector"]
    regex = cfg["journals"]["pagination"]["total_results"]["regex"]

    el = soup.select_one(sel)
    if not el:
        raise ValueError(f"Total-results selector not found: {sel}")

    pagination_type = cfg["journals"]["pagination"].get("type")

    if pagination_type == "query_param" and "href" in cfg["journals"]["pagination"]["total_results"].get("source", "text"):
        href = el.get("href", "")
        m = re.search(regex, href)
        if not m:
            raise ValueError(f"Regex did not match href: {href}")
        last_page = int(m.group(1))
        return last_page + 1
    else:
        text = el.get_text(strip=True)
        m = re.search(regex, text)
        if not m:
            raise ValueError(f"Regex did not match total results text: {text}")
        return int(m.group(1).replace(",", ""))

def debug_selector(soup, selector, label):
    matches = soup.select(selector)
    LOG.info(f"Selector '{selector}' ({label}) matched {len(matches)} nodes")
    for i, el in enumerate(matches[:3]):
        LOG.debug(f"{label} sample {i}: {el.get_text(strip=True)[:120]}")

# ------------------------------------------------------------
# fetch locally saved HTMLs
# ------------------------------------------------------------
def fetch_local_file(filepath, debug=False):
    """Read HTML content from a local file."""
    path = Path(filepath)
    if not path.exists():
        raise FileNotFoundError(f"Local HTML file not found: {filepath}")
    
    LOG.info(f"Reading from local file: {filepath}")
    html = path.read_text(encoding='utf-8')
    
    if debug:
        # Also save a copy to debug cache for consistency
        fname = CACHE_DIR / f"local_{path.stem}.html"
        fname.write_text(html, encoding="utf-8")
        LOG.debug(f"Copied local HTML to {fname}")
    
    return html, None  # No screenshot for local files

# ------------------------------------------------------------
# screenshot helper
# ------------------------------------------------------------

def save_screenshot_if_empty(items, screenshot_tmp, publisher):
    if len(items) == 0 and screenshot_tmp and screenshot_tmp.exists():
        SCREENSHOT_DIR.mkdir(parents=True, exist_ok=True)
        dest = SCREENSHOT_DIR / f"{publisher}-{date.today().isoformat()}.png"
        screenshot_tmp.rename(dest)
        LOG.warning(f"0 items found — screenshot saved to {dest}")
    elif screenshot_tmp and screenshot_tmp.exists():
        screenshot_tmp.unlink()

# ------------------------------------------------------------
# category processing function (for two-level scraping)
# ------------------------------------------------------------

def process_category_page(cfg, session, category_url, headers, delay, debug, cur, journal_cfg, headless=True):
    html, screenshot_tmp = fetch_page(cfg, session, category_url, headers, delay, debug=debug, headless=headless)
    soup = BeautifulSoup(html, "lxml")
    
    journal_items = soup.select(journal_cfg["item_selector"])
    save_screenshot_if_empty(journal_items, screenshot_tmp, f"category_{re.sub('[^a-zA-Z0-9]', '_', category_url)[:50]}")
    
    inserted = 0
    for item in journal_items:
        name_el = item.select_one(journal_cfg["fields"]["journal"]["selector"])
        url_field = journal_cfg["fields"].get("url") or journal_cfg["fields"]["journal"]  # ← was listing_cfg
        url_selector = url_field.get("url_selector") or url_field.get("selector")
        url_el = item if not url_selector else item.select_one(url_selector)

        if not name_el or not url_el:
            LOG.debug("Missing name or URL element in item")
            continue

        title = name_el.get_text(strip=True)  # ← was missing
        href = url_el.get(url_field.get("attr", "href"))
        if not href:
            LOG.debug(f"No href for journal '{title}'")
            continue

        full_url = urljoin(cfg["publisher"]["base_url"], href)
        cur.execute(
            "INSERT IGNORE INTO journals (publisher_key, journal, url, issn_all) VALUES (%s, %s, %s, %s)",
            (cfg["publisher"]["key"], title, full_url, None)
        )
        if cur.rowcount:
            inserted += 1
            LOG.debug(f"Inserted: {title}")
    
    return inserted

# ------------------------------------------------------------
# main logic
# ------------------------------------------------------------

def run(publisher, debug=False, limit=None, headless=True, startpage=1):
    cfg = load_config(publisher)
    headers = build_headers(cfg)
    delay = cfg.get("fetch", {}).get("rate_limit", {}).get("min_seconds_per_request", 2)

    session = requests.Session()
    session.headers.update(headers)

    conn = db_connect()
    cur = conn.cursor()

    start_url = cfg["journals"]["start_url"]
    
    # --------------------------------------------------------
    # Check for alphabetical navigation (new approach)
    # --------------------------------------------------------
    if "alphabet_nav" in cfg["journals"]:
        # ALPHABETICAL NAVIGATION: Process each letter
        LOG.info("Using alphabetical navigation (A-Z with pagination per letter)")
        
        alphabet_cfg = cfg["journals"]["alphabet_nav"]
        listing_cfg = cfg["journals"]["listing"]
        pagination = cfg["journals"].get("pagination")
        page_size = cfg["journals"].get("page_size", 200)
        
        letters = alphabet_cfg.get("letters", "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        
        total_inserted = 0
        letters_processed = 0
        
        # Process each letter
        for letter in letters:
            if limit is not None and letters_processed >= limit:
                break
                
            letter = letter.lower()  # Springer uses lowercase in URLs
            LOG.info(f"Processing letter: {letter.upper()}")
            
            # Construct the first page URL for this letter
            letter_url = f"https://link.springer.com/journals/{letter}/1"
            
            # Check if this letter has any journals
            html, screenshot_tmp = fetch_page(cfg, session, letter_url, headers, delay, debug=debug, headless=headless)
            soup = BeautifulSoup(html, "lxml")
            
            # Check if the letter has no journals (optional empty letter detection)
            empty_selector = alphabet_cfg.get("empty_letter_selector")
            if empty_selector and soup.select_one(empty_selector):
                LOG.info(f"Letter {letter.upper()} has no journals, skipping")
                continue
            
            # Determine number of pages for this letter
            if pagination and page_size:
                try:
                    total_results = extract_total_results(soup, cfg)
                    pages_for_letter = math.ceil(total_results / page_size)
                    LOG.info(f"Letter {letter.upper()}: {total_results} journals → {pages_for_letter} pages")
                except Exception as e:
                    LOG.warning(f"Could not determine pagination for letter {letter.upper()}: {e}")
                    pages_for_letter = 1
            else:
                pages_for_letter = 1
            
            # Build all page URLs for this letter, starting from specified page
            page_urls = []
            for page in range(1, pages_for_letter + 1):
                if page == 1:
                    page_urls.append(letter_url)
                else:
                    if pagination and pagination["type"] == "path_based":
                        # Replace the page number at the end
                        page_url = re.sub(r'/\d+$', f'/{page}', letter_url)
                        page_urls.append(page_url)
            
            # Process each page for this letter (respect startpage)
            for page_idx, page_url in enumerate(page_urls, 1):
                if page_idx < startpage:
                    LOG.info(f"Skipping page {page_idx} (startpage={startpage})")
                    continue
                    
                LOG.info(f"Fetching: {page_url}")
                
                html, screenshot_tmp = fetch_page(cfg, session, page_url, headers, delay, debug=debug, headless=headless)
                soup = BeautifulSoup(html, "lxml")
                
                items = soup.select(listing_cfg["item_selector"])
                LOG.info(f"Found {len(items)} journals on this page")
                
                save_screenshot_if_empty(items, screenshot_tmp, f"{publisher}_{letter}")
                
                # Extract journals from this page
                for item in items:
                    name_el = item.select_one(listing_cfg["fields"]["journal"]["selector"])
                    url_field = listing_cfg["fields"].get("url") or listing_cfg["fields"]["journal"]
                    url_selector = url_field.get("url_selector") or url_field.get("selector")
                    
                    # If no selector is provided, URL lives on the item itself
                    url_el = item if not url_selector else item.select_one(url_selector)
                    
                    if not name_el or not url_el:
                        LOG.debug("Missing name or URL element in item")
                        continue
                    
                    # Remove any additional elements from the name
                    for extra in name_el.select(".list__item-additional"):
                        extra.decompose()
                    
                    title = name_el.get_text(strip=True)
                    href = url_el.get(url_field.get("attr", "href"))
                    
                    if not href:
                        LOG.debug(f"No href for journal '{title}'")
                        continue
                    
                    full_url = urljoin(cfg["publisher"]["base_url"], href)
                    
                    cur.execute(
                        """
                        INSERT IGNORE INTO journals
                        (publisher_key, journal, url, issn_all)
                        VALUES (%s, %s, %s, %s)
                        """,
                        (cfg["publisher"]["key"], title, full_url, None)
                    )
                    if cur.rowcount:
                        total_inserted += 1
                        LOG.debug(f"Inserted: {title}")
            
            letters_processed += 1
            LOG.info(f"Completed letter {letter.upper()}, inserted {total_inserted} journals so far")
        
        LOG.info(f"Done. Processed {letters_processed} letters, inserted {total_inserted} journals.")
    
    # --------------------------------------------------------
    # Check if this is a two-level scraping config (with categories)
    # --------------------------------------------------------
    elif "category_listing" in cfg["journals"]:
        # TWO-LEVEL SCRAPING: Categories → Journals
        LOG.info("Using two-level scraping (categories → journals)")
        
        category_cfg = cfg["journals"]["category_listing"]
        journal_cfg = cfg["journals"]["journal_listing"]
        
        # Fetch the main page with category listings
        html, screenshot_tmp = fetch_page(cfg, session, start_url, headers, delay, debug=debug, headless=headless)
        soup = BeautifulSoup(html, "lxml")
        
        # Debug the category selector
        debug_selector(soup, category_cfg["item_selector"], "categories")
        
        # Extract all category URLs
        category_items = soup.select(category_cfg["item_selector"])
        LOG.info(f"Found {len(category_items)} categories")
        
        # Build list of category URLs
        category_urls = []
        for item in category_items:
            url_el = item.select_one(category_cfg["fields"]["category_url"]["selector"])
            if url_el and url_el.get("href"):
                href = url_el.get("href")
                full_url = urljoin(cfg["publisher"]["base_url"], href)
                category_urls.append(full_url)
                if debug:
                    name_el = item.select_one(category_cfg["fields"]["category_name"]["selector"])
                    category_name = name_el.get_text(strip=True) if name_el else "Unknown"
                    LOG.debug(f"Found category: {category_name} -> {full_url}")
        
        # Apply limit if specified (limit number of categories to process)
        if limit is not None:
            category_urls = category_urls[:limit]
        
        LOG.info(f"Will process {len(category_urls)} categories")
        
        # Process each category URL to extract journals
        total_inserted = 0
        categories_processed = 0
        
        for category_url in category_urls:
            LOG.info(f"Processing category {categories_processed + 1}/{len(category_urls)}: {category_url}")
            
            # Pass headless parameter to process_category_page
            inserted = process_category_page(
                cfg, session, category_url, headers, delay, debug, cur, journal_cfg, 
                headless=headless
            )
            total_inserted += inserted
            LOG.info(f"Inserted {inserted} journals from this category")
            
            categories_processed += 1
        
        LOG.info(f"Done. Processed {categories_processed} categories, inserted {total_inserted} journals.")
    
    else:
        # ORIGINAL SINGLE-LEVEL SCRAPING: Direct journals (with pagination)
        LOG.info("Using single-level scraping (direct journals)")
        
        listing_cfg = cfg["journals"]["listing"]
        item_selector = listing_cfg["item_selector"]
        page_size = cfg["journals"].get("page_size")
        pagination = cfg["journals"].get("pagination")
        pagination_type = pagination.get("type") if pagination else None

        # ---- build the list of URLs to scrape ----
        if pagination_type == "alphabet":
            letters = pagination.get("letters", "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
            urls_to_scrape = [start_url.replace("{letter}", letter) for letter in letters]

        else:
            html, screenshot_tmp = fetch_page(cfg, session, start_url, headers, delay, debug=debug, headless=headless)
            soup = BeautifulSoup(html, "lxml")
            debug_selector(soup, item_selector, "journal items")

            if pagination and page_size:
                try:
                    total_or_pages = extract_total_results(soup, cfg)
                    if pagination.get("type") == "query_param" and "href" in pagination["total_results"].get("source", "text"):
                        pages = total_or_pages
                        LOG.info(f"Total pages: {pages}")
                    else:
                        pages = math.ceil(total_or_pages / page_size)
                        LOG.info(f"Total results: {total_or_pages} → {pages} pages")
                except Exception as e:
                    LOG.warning(f"Pagination failed ({e}), falling back to single page")
                    pages = 1
            else:
                pages = 1

            urls_to_scrape = []
            for page in range(startpage - 1, pages):  # Start from startpage-1 (0-based index)
                if page > 0:  # This is not the first page we're adding
                    if pagination and pagination["type"] == "query_param":
                        param = pagination["param"]
                        start = pagination.get("start_at", 0)
                        # page is already adjusted for startpage, so we need to calculate the actual page number
                        actual_page = page + 1  # Convert 0-based index to 1-based page number
                        value = start + actual_page
                        # If param already in URL, replace it; otherwise append it
                        if f"{param}=" in start_url:
                            url = re.sub(rf"{param}=\d+", f"{param}={value}", start_url)
                        else:
                            separator = "&" if "?" in start_url else "?"
                            url = f"{start_url}{separator}{param}={value}"
                    elif pagination and pagination["type"] == "path_based":
                        # page is already adjusted for startpage, so actual_page = page + 1
                        url = re.sub(r'/(\d+)$', f'/{page + 1}', start_url)
                    else:
                        LOG.error("Unsupported pagination type")
                        break
                    urls_to_scrape.append(url)
                else:
                    # This is the first page we're adding (which might not be page 1 if startpage > 1)
                    if startpage > 1:
                        LOG.info(f"Starting from page {startpage}, skipping pages 1-{startpage-1}")
                    # Always include the start_url as the first item, which will be the appropriate page
                    # based on how we constructed the loop
                    if page == 0:  # This is our first iteration
                        # If startpage > 1, we need to construct the URL for the startpage
                        if startpage > 1:
                            if pagination and pagination["type"] == "query_param":
                                param = pagination["param"]
                                start = pagination.get("start_at", 0)
                                value = start + startpage
                                if f"{param}=" in start_url:
                                    url = re.sub(rf"{param}=\d+", f"{param}={value}", start_url)
                                else:
                                    separator = "&" if "?" in start_url else "?"
                                    url = f"{start_url}{separator}{param}={value}"
                            elif pagination and pagination["type"] == "path_based":
                                url = re.sub(r'/(\d+)$', f'/{startpage}', start_url)
                            else:
                                url = start_url
                        else:
                            url = start_url
                        urls_to_scrape.append(url)

        # ---- scrape each URL for journals ----
        pages_scraped = 0
        inserted = 0

        for page_url in urls_to_scrape:
            if limit is not None and pages_scraped >= limit:
                break

            current_page = startpage + pages_scraped
            LOG.info(f"Fetching page {current_page}: {page_url}")
            
            html, screenshot_tmp = fetch_page(cfg, session, page_url, headers, delay, debug=debug, headless=headless)
            soup = BeautifulSoup(html, "lxml")

            items = soup.select(item_selector)
            LOG.info(f"URL {page_url}: {len(items)} items")

            save_screenshot_if_empty(items, screenshot_tmp, publisher)

            for item in items:
                # Handle empty selectors for journal name
                journal_selector = listing_cfg["fields"]["journal"]["selector"]
                if journal_selector:
                    name_el = item.select_one(journal_selector)
                else:
                    name_el = item  # Use the item itself if selector is empty
                
                url_field = listing_cfg["fields"].get("url") or listing_cfg["fields"]["journal"]
                url_selector = url_field.get("url_selector") or url_field.get("selector")
                
                # Handle empty selectors for URL
                if url_selector:
                    url_el = item.select_one(url_selector)
                else:
                    url_el = item  # Use the item itself if selector is empty
                
                if not name_el or not url_el:
                    LOG.debug("Missing name or URL element in item")
                    continue
                
                # Remove any additional elements from the name if needed
                if hasattr(name_el, 'select'):  # Check if it's a BeautifulSoup element
                    for extra in name_el.select(".list__item-additional"):
                        extra.decompose()

                if not name_el or not url_el:
                    LOG.debug("Missing name or URL element in item")
                    continue

                for extra in name_el.select(".list__item-additional"):
                    extra.decompose()

                href = url_el.get(url_field.get("attr", "href"))

                title = name_el.get_text(strip=True)
                href = url_el.get("href")
                if not href:
                    LOG.debug(f"No href for journal '{title}'")
                    continue

                full_url = urljoin(cfg["publisher"]["base_url"], href)

                cur.execute(
                    """
                    INSERT IGNORE INTO journals
                    (publisher_key, journal, url, issn_all)
                    VALUES (%s, %s, %s, %s)
                    """,
                    (cfg["publisher"]["key"], title, full_url, None)
                )
                if cur.rowcount:
                    inserted += 1

            pages_scraped += 1

        LOG.info(f"Done. Inserted {inserted} journals.")
        
# ------------------------------------------------------------
# cli
# ------------------------------------------------------------

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--publisher", required=True)
    parser.add_argument("--debug", action="store_true")
    parser.add_argument("--limit", type=int, help="limit number of pages/categories")
    parser.add_argument("--visible", action="store_true", help="Run browser in visible mode (non-headless)")
    parser.add_argument("--startpage", type=int, default=1, help="Start from this page number (for paginated scraping)")
    args = parser.parse_args()

    if args.debug:
        LOG.setLevel(logging.DEBUG)

    run(args.publisher, debug=args.debug, limit=args.limit, headless=not args.visible, startpage=args.startpage)