from bs4 import BeautifulSoup

def parse_cup(container_html: str) -> list[dict]:
    """
    CUP structure:
      <div class="editorial-position">
        <h6>Role</h6>
        <p class="paragraph_03">Name, <span class="public-address">Affiliation</span>
          <br><a href="mailto:...">email</a>
        </p>
      </div>
    """
    soup = BeautifulSoup(container_html, "lxml")
    results = []

    for block in soup.select("div.editorial-position"):
        h6 = block.find("h6")
        if not h6:
            continue
        role = h6.get_text(separator=" ", strip=True)

        for p in block.select("p.paragraph_03"):
            raw = str(p)

            # Extract email from <a href="mailto:...">
            email = None
            a = p.find("a", href=lambda h: h and h.startswith("mailto:"))
            if a:
                email = a.get_text(strip=True)
                a.decompose()

            # Extract affiliation from <span class="public-address">
            span = p.find("span", class_="public-address")
            affiliation = span.get_text(separator=" ", strip=True) if span else None
            if span:
                span.decompose()

            # Remove <br> tags, get remaining text as name
            for br in p.find_all("br"):
                br.decompose()
            full_name = p.get_text(separator=" ", strip=True).strip(",; ")

            if not full_name:
                continue

            results.append({
                "role": role,
                "full_name": full_name,
                "affiliation": affiliation,
                "email": email,
                "raw_text": raw,
            })

    return results
