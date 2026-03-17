from bs4 import BeautifulSoup

def parse_rsc(container_html: str) -> list[dict]:
    """
    RSC accordion structure:
      #accordionIdThree > .accordion-item
        .accordion-button  → section role (e.g. "Editor-in-Chief", "Advisory board")
        .card (per person):
          Standard cards:  h3 = name, p.bold = role override, p (no .bold, no a) = affiliation
          Advisory board:  p.bold = name, next p = affiliation (no h3)
    Skips "Editorial office" section.
    """
    soup = BeautifulSoup(container_html, "lxml")
    results = []

    for accordion_item in soup.select(".accordion-item"):
        btn = accordion_item.select_one(".accordion-button")
        if not btn:
            continue

        # Strip icon text and whitespace to get section label
        for icon in btn.find_all("i"):
            icon.decompose()
        section_role = btn.get_text(strip=True)

        if "editorial office" in section_role.lower():
            continue

        is_advisory = "advisory board" in section_role.lower()

        for card in accordion_item.select(".card"):
            raw = str(card)

            if is_advisory:
                name_el = card.select_one("p.bold")
                if not name_el:
                    continue
                full_name = name_el.get_text(strip=True)
                if not full_name:
                    continue

                # Affiliation: next sibling <p> that isn't .bold and isn't an <a>
                affiliation = None
                for p in card.select("p:not(.bold)"):
                    if p.find("a"):
                        continue
                    text = p.get_text(strip=True)
                    if text:
                        affiliation = text
                        break

                results.append({
                    "role": "Advisory board member",
                    "full_name": full_name,
                    "affiliation": affiliation,
                    "raw_text": raw,
                })

            else:
                name_el = card.select_one("h3")
                if not name_el:
                    continue
                full_name = name_el.get_text(strip=True)
                if not full_name:
                    continue

                # Role: p.bold overrides section role if non-empty
                role = section_role
                bold_el = card.select_one("p.bold")
                if bold_el:
                    bold_text = bold_el.get_text(strip=True)
                    if bold_text:
                        role = bold_text

                # Affiliation: <p> that is not .bold and has no <a>
                affiliation = None
                for p in card.select("p:not(.bold)"):
                    if p.find("a"):
                        continue
                    text = p.get_text(strip=True)
                    if text:
                        affiliation = text
                        break

                results.append({
                    "role": role,
                    "full_name": full_name,
                    "affiliation": affiliation,
                    "raw_text": raw,
                })

    return results
