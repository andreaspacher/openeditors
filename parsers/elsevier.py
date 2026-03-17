import re
from bs4 import BeautifulSoup

def parse_elsevier(container_html: str) -> list[dict]:
    """
    Elsevier / ScienceDirect structure:
      div.inner
        h3                        → role
        div.row.editor-group (per editor):
          h4                      → name
          p.js-affiliation        → affiliation (last comma-segment = country)
    """
    soup = BeautifulSoup(container_html, "lxml")
    results = []

    for inner in soup.select("div.inner"):
        role_el = inner.select_one("h3")
        role = role_el.get_text(strip=True) if role_el else None

        for editor in inner.select("div.row.editor-group"):
            name_el = editor.select_one("h4")
            if not name_el:
                continue
            full_name = name_el.get_text(separator=" ", strip=True)
            full_name = re.sub(r"\s{2,}", " ", full_name).strip()
            if not full_name:
                continue

            affil_el = editor.select_one("p.js-affiliation")
            affiliation = affil_el.get_text(strip=True) if affil_el else None

            country = None
            if affiliation:
                parts = [p.strip() for p in affiliation.split(",")]
                if parts:
                    country = parts[-1]

            results.append({
                "role": role,
                "full_name": full_name,
                "affiliation": affiliation,
                "country": country,
                "raw_text": str(editor),
            })

    return results
# ------------------------------------------------------------
# function for Elsevier: resolving URL
# ------------------------------------------------------------
def slugify(text: str) -> str:
    """Convert text to URL-friendly slug"""
    # Convert to lowercase and replace non-alphanumeric with hyphens
    slug = re.sub(r'[^a-z0-9]+', '-', text.lower())
    # Remove leading/trailing hyphens
    return slug.strip('-')