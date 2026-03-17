import os
import logging
from datetime import datetime
from bs4 import BeautifulSoup

LOG = logging.getLogger("openeditors.editors")

def parse_elife(container_html: str) -> list[dict]:
    soup = BeautifulSoup(container_html, "lxml")
    results = []
    current_role = None

    for el in soup.find_all(["h3", "h4"]):
        if el.name == "h3" and "list-heading" in el.get("class", []):
            current_role = el.get_text(separator=" ", strip=True)

        elif el.name == "h4" and "about-profile__name" in el.get("class", []):
            name = el.get_text(separator=" ", strip=True)
            if not name:
                continue

            affiliation = None
            country = None

            role_p = el.find_next_sibling("p", class_="about-profile__role")
            if role_p:
                affiliation_raw = role_p.get_text(separator=" ", strip=True)
                # Split country off the last comma-separated token
                parts = [p.strip() for p in affiliation_raw.rsplit(",", 1)]
                if len(parts) == 2:
                    affiliation, country = parts
                else:
                    affiliation = affiliation_raw

            results.append({
                "role": current_role,
                "full_name": name,
                "affiliation": affiliation,
                "country": country,
                "raw_text": str(el.parent),
            })

    return results


# ------------------------------------------------------------
# function for elife (section-specific slugs)
# ------------------------------------------------------------

def run_elife(cfg, cur, run_id, session, strategies, fetch_page_fn, reconcile_fn, take_screenshot_fn=None, debug=False):
    """Special handler for section-slug publishers like eLife."""
    editors_cfg = cfg["editors"]
    url_pattern = editors_cfg["url_pattern"]
    strategy_name = editors_cfg["parsing"]["strategy"]
    parser = strategies[strategy_name]  # ← use the argument, not STRATEGIES

    # Expect exactly one journal row for eLife
    cur.execute(
        "SELECT journal_id, journal FROM journals WHERE publisher_key = %s",
        (cfg["publisher"]["key"],)
    )
    journals = cur.fetchall()
    if not journals:
        LOG.warning("No journal row found for eLife — skipping.")
        return 0, 0, 0

    journal = journals[0]
    journal_id = journal["journal_id"]
    total_inserted = total_updated = total_removed = 0
    all_editors = []

    for slug in editors_cfg.get("section_slugs", []):
        url = url_pattern.replace("{section_slug}", slug) if slug else editors_cfg["start_url"]
        LOG.info(f"Fetching eLife section: {url}")
        try:
            html = fetch_page_fn(cfg, session, url)
        except Exception as e:
            LOG.warning(f"Failed to fetch {url}: {e}")
            continue

        soup = BeautifulSoup(html, "lxml")
        content_sel = editors_cfg["board_section"]["content_selector"]
        container = soup.select_one(content_sel)
        if not container:
            LOG.warning(f"No content found at {url}")
            continue

        editors = parser(str(container))
        for ed in editors:
            ed["source_url"] = url
        LOG.info(f"  → {len(editors)} editors in section '{slug or 'leadership'}'")

        if len(editors) == 0 and debug:
            debug_dir = "_debug_screenshots"
            os.makedirs(debug_dir, exist_ok=True)
            screenshot_path = os.path.join(debug_dir, f"zero_elife_{slug or 'leadership'}.png")
            take_screenshot_fn(url, screenshot_path)
            LOG.info(f"  → screenshot saved to {screenshot_path}")

        all_editors.extend(editors)

    # Deduplicate by full_name before reconciling (a person can appear in multiple sections)
    seen = {}
    for ed in all_editors:
        name = ed["full_name"]
        if name not in seen:
            seen[name] = ed
        # If already seen, you could merge roles here if desired

    scraped_at = datetime.utcnow()
    ins, upd, rem = reconcile_fn(cur, journal_id, list(seen.values()), run_id, scraped_at)
    LOG.info(f"  → +{ins} new, {upd} confirmed, -{rem} removed")
    return ins, upd, rem
