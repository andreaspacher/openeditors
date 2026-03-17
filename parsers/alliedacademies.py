from bs4 import BeautifulSoup

def parse_alliedacademies(container_html: str) -> list[dict]:
    """
    Parses Allied Academies editor listings.

    Structure:
      <div class="alert ..."><strong>Role</strong></div>
      <li class="list-group-item">
        <b>Name</b><br>
        <p>Occupation<br>
        Department<br>
        University  ← AFFILIATION (last line before city)
        City<br>
        Country.</p>
      </li>
    """
    soup = BeautifulSoup(container_html, "lxml")
    results = []
    current_role = None

    for ul in soup.select("ul.list-group-flush"):
        # Role is in the <strong> inside the alert div preceding the <li>s
        role_tag = ul.select_one("div.alert strong")
        if role_tag:
            current_role = role_tag.get_text(strip=True)

        for li in ul.select("li.list-group-item"):
            name_tag = li.find("b")
            if not name_tag:
                continue
            full_name = name_tag.get_text(strip=True)

            # Collect all <p> tags; skip nbsp-only ones
            p_tags = [
                p for p in li.find_all("p")
                if p.get_text(strip=True).replace("\xa0", "").strip()
            ]
            if not p_tags:
                continue

            # Use only the first meaningful <p>
            p = p_tags[0]

            # Split on <br>, strip whitespace and non-breaking spaces
            for br in p.find_all("br"):
                br.replace_with("\n")
            lines = [
                l.strip().strip("\xa0").rstrip(".")
                for l in p.get_text().split("\n")
                if l.strip().replace("\xa0", "").strip()
            ]

            # Lines are: [occupation?, department?, university?, ..., city, country]
            # Country is always last, city second-to-last
            # University is the line just before city (i.e. lines[-3] if len >= 3)
            country = lines[-1] if len(lines) >= 1 else None
            # lines[-2] is city; lines[-3] is affiliation (university)
            affiliation = lines[-3] if len(lines) >= 3 else None

            results.append({
                "role": current_role,
                "full_name": full_name,
                "affiliation": affiliation,
                "country": country,
            })

    return results
