from bs4 import BeautifulSoup
def parse_apa(container_html: str) -> list[dict]:
    """
    Parses blocks like:
      <h2>Role</h2>
      <p>Name, credentials<br><em>Affiliation</em></p>
      <p>Name, credentials<br><em>Affiliation</em></p>
      <h2>Next Role</h2>
      ...
    Returns list of dicts with keys: role, full_name, affiliation, raw_text
    """
    soup = BeautifulSoup(container_html, "lxml")
    results = []
    current_role = None

    for el in soup.find_all(["h2", "p"]):
        if el.name == "h2":
            current_role = el.get_text(separator=" ", strip=True)
        elif el.name == "p" and current_role:
            raw = str(el)
            # Split on <br> tags to separate name from affiliation
            # Name is the text node(s) before <em>, affiliation is inside <em>
            em = el.find("em")
            affiliation = em.get_text(separator=" ", strip=True) if em else None

            # Remove em text from the paragraph to get name
            if em:
                em.decompose()
            full_name = el.get_text(separator=" ", strip=True)
            full_name = full_name.strip(",; \n")

            if not full_name:
                continue

            results.append({
                "role": current_role,
                "full_name": full_name,
                "affiliation": affiliation,
                "raw_text": raw,
            })

    return results
