import re
from bs4 import BeautifulSoup
import logging
import time

LOG = logging.getLogger("openeditors.editors")

def parse_frontiers(container_html: str) -> list[dict]:
    """
    Frontiers structure (full page HTML, multiple .Editors__roleGroup blocks):
      h2.Editors__roleGroup__title     → role
      .CardEditor__info (per editor):
        h3.CardEditor__name            → name (stored in lowercase — title-case it)
        p.CardEditor__affiliation__name → affiliation
        p.CardEditor__affiliation__location > span:last-child → country
    """
    soup = BeautifulSoup(container_html, "lxml")
    results = []

    for group in soup.select(".Editors__roleGroup"):
        h2 = group.select_one("h2.Editors__roleGroup__title")
        if not h2:
            continue
        role = h2.get_text(strip=True)

        for card in group.select(".CardEditor__info"):
            name_el = card.select_one("h3.CardEditor__name")
            if not name_el:
                continue
            # Names are stored lowercase in the HTML — title-case them
            full_name = name_el.get_text(separator=" ", strip=True).title()
            # Fix title-case artefacts: "J." → keep, "Of" → "of" etc. is acceptable
            full_name = re.sub(r"\s{2,}", " ", full_name).strip()
            if not full_name:
                continue

            affiliation_el = card.select_one("p.CardEditor__affiliation__name")
            affiliation = affiliation_el.get_text(strip=True) if affiliation_el else None

            country = None
            location_el = card.select_one("p.CardEditor__affiliation__location")
            if location_el:
                spans = location_el.select("span")
                if spans:
                    country = spans[-1].get_text(strip=True)

            results.append({
                "role": role,
                "full_name": full_name,
                "affiliation": affiliation,
                "country": country,
                "raw_text": str(card),
            })

    return results
# ------------------------------------------------------------
# function for Frontiers: scroll until stable-loaded
# ------------------------------------------------------------
def fetch_page_scrolling(cfg, url: str) -> str:
    from playwright.sync_api import sync_playwright
    import random

    scroll_cfg  = cfg.get("editors", {}).get("scroll", {})
    max_wait    = scroll_cfg.get("max_wait_seconds", 120)
    stable_need = scroll_cfg.get("stable_after_attempts", 3)
    pause       = scroll_cfg.get("scroll_pause_seconds", 5)
    post_idle   = scroll_cfg.get("post_networkidle_seconds", 3)
    ua          = cfg.get("fetch", {}).get("headers", {}).get("user_agent", "")

    LOG.info(f"[scroll] fetching {url}")

    with sync_playwright() as pw:
        browser = pw.chromium.launch(
            headless=True,
            args=["--disable-blink-features=AutomationControlled"],
        )
        context = browser.new_context(
            user_agent=ua if ua else None,
            viewport={"width": 1440, "height": 900},   # realistic viewport
            java_script_enabled=True,
        )
        # Mask webdriver flag
        context.add_init_script(
            "Object.defineProperty(navigator, 'webdriver', {get: () => undefined})"
        )
        page = context.new_page()
        page.goto(url, wait_until="domcontentloaded", timeout=60_000)

        try:
            page.wait_for_selector(".CardEditor__info", timeout=20_000)
        except Exception:
            LOG.warning(f"[scroll] no .CardEditor__info on initial load: {url}")
            html = page.content()
            browser.close()
            return html

        deadline     = time.time() + max_wait
        stable_count = 0
        last_count   = -1

        while time.time() < deadline:
            # Get current scroll height and step down incrementally
            scroll_height = page.evaluate("document.body.scrollHeight")
            current_y     = page.evaluate("window.scrollY")
            step          = 600   # px per step — one "screenful"

            # Scroll down in steps until we reach the bottom
            y = current_y + step
            while y < scroll_height + step:
                page.evaluate(f"window.scrollTo({{top: {y}, behavior: 'smooth'}})")
                time.sleep(0.4)   # small gap between steps
                y += step

            # After reaching bottom, wait for new content
            time.sleep(pause)
            try:
                page.wait_for_load_state("networkidle", timeout=10_000)
            except Exception:
                pass
            time.sleep(post_idle)

            current_count = page.locator(".CardEditor__info").count()
            LOG.info(f"[scroll] cards visible: {current_count}")

            if current_count == last_count:
                stable_count += 1
                if stable_count >= stable_need:
                    LOG.info(f"[scroll] stable after {stable_need} attempts — done")
                    break
            else:
                stable_count = 0
                last_count   = current_count

        html = page.content()
        browser.close()

    return html
