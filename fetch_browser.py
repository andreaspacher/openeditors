from playwright.sync_api import sync_playwright
from pathlib import Path
from datetime import date
import time
import random
import tempfile
import logging

LOG = logging.getLogger("openeditors.editors")

def _human_pause(min_s=0.3, max_s=1.2):
    time.sleep(random.uniform(min_s, max_s))


def _random_scroll(page):
    for _ in range(random.randint(3, 7)):
        page.mouse.wheel(0, random.randint(200, 600))
        _human_pause(0.3, 0.9)
    if random.random() < 0.4:
        page.mouse.wheel(0, -random.randint(100, 400))
        _human_pause(0.2, 0.6)


def _random_mouse_moves(page, width=1280, height=800, moves=6):
    for _ in range(moves):
        page.mouse.move(
            random.randint(50, width - 50),
            random.randint(50, height - 50),
            steps=random.randint(5, 20),
        )
        _human_pause(0.1, 0.5)


def fetch_browser(url, user_agent=None, wait=2, timeout=60000, headless=True,
                  human_warmup=True, wait_for_selector=None):
    with sync_playwright() as p:
        browser = p.chromium.launch(
            headless=headless,
            args=["--disable-blink-features=AutomationControlled"],
        )
        context = browser.new_context(
            user_agent=user_agent,
            viewport={"width": 1280, "height": 800},
            extra_http_headers={"Accept-Language": "en-US,en;q=0.9"},
        )
        context.add_cookies([{
            "name": "CookieConsent",
            "value": "{stamp:%27-1%27%2Cnecessary:true%2Cpreferences:true%2Cstatistics:true%2Cmarketing:true}",
            "domain": ".elifesciences.org",
            "path": "/",
        }])

        page = context.new_page()
        page.add_init_script("""
            Object.defineProperty(navigator, 'webdriver', {
                get: () => undefined
            });
        """)

        page.goto(url, timeout=timeout, wait_until="domcontentloaded")

        try:
            page.wait_for_url(lambda u: u != url, timeout=5000)
        except:
            pass

        # Wait for a specific element to appear if requested
        if wait_for_selector:
            try:
                page.wait_for_selector(wait_for_selector, timeout=20000)
            except Exception as e:
                LOG.debug(f"wait_for_selector '{wait_for_selector}' timed out: {e}")

        if human_warmup:
            _human_pause(1.5, 3.0)
            _random_mouse_moves(page)
            _random_scroll(page)
            _human_pause(0.8, 2.0)
        else:
            time.sleep(wait)

        html = page.content()
        final_url = page.url

        tmp = tempfile.NamedTemporaryFile(suffix=".png", delete=False)
        page.screenshot(path=tmp.name, full_page=True)
        screenshot_tmp = Path(tmp.name)

        browser.close()
        return html, screenshot_tmp, final_url