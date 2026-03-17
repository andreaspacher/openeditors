import re
from bs4 import BeautifulSoup

def parse_emerald(container_html: str) -> list[dict]:
    """
    Emerald structure:
      .editorial-team > ul > li
        h4  (role)
        ul > li  (one per person)
          text nodes  (name, possibly with title prefix)
          <br>
          <em>Affiliation</em>
          - <em>Country</em>
          <br>
          <a href="mailto:...">email</a>
    """
    soup = BeautifulSoup(container_html, "lxml")
    results = []

    for outer_li in soup.select("ul > li:has(h4)"):
        h4 = outer_li.find("h4")
        if not h4:
            continue
        role = h4.get_text(separator=" ", strip=True)

        inner_ul = outer_li.find("ul")
        if not inner_ul:
            continue

        for li in inner_ul.find_all("li", recursive=False):
            raw = str(li)

            # Email
            email = None
            a = li.find("a", href=lambda h: h and h.startswith("mailto:"))
            if a:
                email = a.get_text(strip=True)
                a.decompose()

            # Affiliation = first <em>, Country = second <em>
            ems = li.find_all("em")
            affiliation = ems[0].get_text(separator=" ", strip=True) if len(ems) > 0 else None
            country = ems[1].get_text(separator=" ", strip=True) if len(ems) > 1 else None
            for em in ems:
                em.decompose()

            # Remove <br> tags and stray " - " separators, get name from remaining text
            for br in li.find_all("br"):
                br.decompose()
            full_name = li.get_text(separator=" ", strip=True)
            # Clean up separators and excess whitespace
            full_name = re.sub(r"\s*-\s*$", "", full_name).strip()
            full_name = re.sub(r"\s{2,}", " ", full_name)

            if not full_name:
                continue

            results.append({
                "role": role,
                "full_name": full_name,
                "affiliation": affiliation,
                "country": country,
                "email": email,
                "raw_text": raw,
            })

    return results
