import re
from bs4 import BeautifulSoup, NavigableString, Tag

def parse_karger(container_html: str) -> list[dict]:
    """
    Parses editorial board blocks of the form:
      <h3>Role</h3>
      Full Name – <em>Affiliation</em>, City, Country<br>

    Optionally with <h4> sub-roles appended in parentheses to the current h3.
    Handles:
      - Anchor tags (e.g. competing interests links) mixed into lines
      - Name/letter bleeding into <em> (e.g. 'Schmitt,<em> University...')
      - Trailing commas/dashes on names (e.g. 'Kobayashi, –')
      - Missing dash separator
      - Country with no preceding city (bare ', Country')
    """
    soup = BeautifulSoup(container_html, "lxml")
    results = []
    current_h3_role = None
    current_role = None

    def extract_country(location_str: str):
        """
        Given ', City, Country' (text after </em>),
        strips leading punctuation and returns the last comma-segment as country.
        """
        s = re.sub(r"^[\s,|]+", "", location_str).strip()
        if not s:
            return None
        parts = [p.strip() for p in s.split(",")]
        parts = [p for p in parts if p]
        return parts[-1] if parts else None

    def clean_name(raw: str) -> str:
        """Strip trailing separators, commas, dashes from a name fragment."""
        return re.sub(r"[\s,;:\-–—|]+$", "", raw).strip().strip(",;")

    def parse_entry(fragment_nodes: list, role: str) -> dict | None:
        """
        Given a list of sibling nodes (NavigableString + Tag) that make up
        one line (everything between the previous <br>/block and the current <br>),
        extract full_name, affiliation, country.
        """
        # Rebuild a mini-soup from the fragment
        fragment_html = "".join(str(n) for n in fragment_nodes)
        fsoup = BeautifulSoup(fragment_html, "lxml")

        # Remove <a> tags entirely (e.g. "Competing Interests" links)
        for a in fsoup.find_all("a"):
            a.decompose()

        em = fsoup.find("em")

        # --- Affiliation: raw text inside <em> ---
        affiliation = None
        if em:
            affiliation = em.get_text(separator=" ", strip=True)

        # --- Location: text nodes strictly AFTER </em> ---
        location_str = ""
        if em:
            for sib in em.next_siblings:
                if isinstance(sib, NavigableString):
                    location_str += str(sib)
                elif isinstance(sib, Tag) and sib.name != "em":
                    # e.g. a stray <a> we didn't catch — grab its text
                    location_str += sib.get_text()
        country = extract_country(location_str)

        # --- Name: everything BEFORE <em> ---
        if em:
            # Collect text up to (but not including) <em>
            pre_text = ""
            for sib in em.previous_siblings:
                pre_text = str(sib) + pre_text
            # Also handle the case where a letter leaked into <em>:
            # e.g. <em>elje General Hospital</em> with "Repše-Fokter, C" before
            # We detect this by checking if affiliation starts lowercase or
            # if the em text doesn't look like an institution
            em_text = em.get_text(strip=True)
            if em_text and em_text[0].islower():
                # The first char of <em> belongs to the name prefix
                # Find where the real affiliation starts (first uppercase run)
                match = re.match(r"^([a-z]+)(.*)", em_text, re.DOTALL)
                if match:
                    leaked, real_affiliation = match.group(1), match.group(2).strip()
                    pre_text = pre_text + leaked
                    affiliation = real_affiliation if real_affiliation else None

            pre_soup = BeautifulSoup(pre_text, "lxml")
            raw_name = pre_soup.get_text(separator=" ", strip=True)
        else:
            raw_name = fsoup.get_text(separator=" ", strip=True)

        full_name = clean_name(raw_name)
        if not full_name:
            return None

        return {
            "role": role,
            "full_name": full_name,
            "affiliation": affiliation,
            "country": country,
        }

    container = soup.find("div") or soup.body or soup

    # Collect children, grouping text+inline nodes between block boundaries
    block_tags = {"h1", "h2", "h3", "h4", "h5", "h6", "p", "div", "br"}
    current_line: list = []

    for el in container.children:
        is_block = isinstance(el, Tag) and el.name in block_tags

        if isinstance(el, Tag) and el.name in {"h3", "h4"}:
            # Flush any pending line (shouldn't normally have one here)
            current_line = []
            text = el.get_text(separator=" ", strip=True).rstrip(":")
            if el.name == "h3":
                current_h3_role = text
                current_role = text
            else:
                current_role = f"{current_h3_role} ({text})" if current_h3_role else text

        elif isinstance(el, Tag) and el.name == "br":
            # End of a line — parse whatever we collected
            if current_line and current_role:
                entry = parse_entry(current_line, current_role)
                if entry:
                    results.append(entry)
            current_line = []

        elif is_block:
            current_line = []

        else:
            # Text node or inline tag (em, a, strong, …)
            current_line.append(el)

    # Flush trailing line if no closing <br>
    if current_line and current_role:
        entry = parse_entry(current_line, current_role)
        if entry:
            results.append(entry)

    return results
