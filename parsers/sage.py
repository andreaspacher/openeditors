import logging
from bs4 import BeautifulSoup
import logging
from datetime import datetime

LOG = logging.getLogger("openeditors.editors")

def parse_sage(container_html: str) -> list[dict]:
    soup = BeautifulSoup(container_html, "html.parser")  # lxml moves h3 elements
    results = []
    current_role = None

    accordion = soup.select_one("div.bs-accordion")
    if not accordion:
        LOG.warning("parse_sage: no div.bs-accordion found in container")
        return results

    for el in accordion.children:
        if getattr(el, "name", None) != "div":
            continue
        if "ed-board-name" in el.get("class", []):
            current_role = el.get_text(strip=True)
            LOG.debug(f"parse_sage: role → {current_role!r}")
        elif "ed-board-table-wrapper" in el.get("class", []):
            for row in el.select("tr"):
                name_el = row.select_one("td.ed-board-member")
                if not name_el:
                    continue
                full_name = name_el.get_text(strip=True)
                if not full_name:
                    continue
                affiliation_el = row.select_one("td.ed-board-member-affiliation")
                affiliation = affiliation_el.get_text(strip=True) if affiliation_el else None
                results.append({
                    "role": current_role,
                    "full_name": full_name,
                    "affiliation": affiliation,
                    "country": None,
                    "raw_text": str(row),
                })

    return results
