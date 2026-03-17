from bs4 import BeautifulSoup
def parse_asce(container_html: str) -> list[dict]:
    """
    Parses blocks like:
      <h4>Role</h4>
      <p>Name, credentials<br><em>Affiliation</em></p>
    """
    soup = BeautifulSoup(container_html, "lxml")
    results = []
    current_role = None

    for el in soup.find_all(["h4", "p"]):
        if el.name == "h4":
            current_role = el.get_text(separator=" ", strip=True).rstrip(":")
        elif el.name == "p" and current_role:
            # A single <p> can hold multiple editors separated by <br>
            # We split on <br> and treat each chunk as one editor
            raw = str(el)
            # Render br as a sentinel, then split
            for br in el.find_all("br"):
                br.replace_with("\n")
            chunks = el.decode_contents().split("\n")

            for chunk in chunks:
                chunk_soup = BeautifulSoup(chunk, "lxml")
                em = chunk_soup.find("em")
                affiliation = em.get_text(separator=" ", strip=True) if em else None
                if em:
                    em.decompose()
                full_name = chunk_soup.get_text(separator=" ", strip=True).strip(",; ")
                if not full_name:
                    continue
                results.append({
                    "role": current_role,
                    "full_name": full_name,
                    "affiliation": affiliation,
                    "raw_text": raw,
                })

    return results
