import re
from bs4 import BeautifulSoup

def parse_springernature(container_html: str) -> list[dict]:
    """
    Springer Nature editorial board — two structural variants:

    Option 1 (structured):
      h2                  → role
      h3                  → name
      .u-text-default     → "Institution, City, Country"
                            country = text after last comma

    Option 2 (prose, inside <p> blocks):
      <strong>            → role (may appear mid-<p> when roles are chained)
      Each <br>-delimited segment thereafter is one editor:
        - text before first comma  → name
        - Option 2a: <em> present  → affiliation from <em>
        - Option 2b: no <em>       → everything after first comma → affiliation
          country = text after last comma in affiliation
    """
    soup = BeautifulSoup(container_html, "lxml")
    results = []

    # ------------------------------------------------------------------ #
    # Option 1: structured h2/h3/.u-text-default layout                  #
    # ------------------------------------------------------------------ #
    if soup.find("h2", attrs={"data-test": "editorDisplayRole"}):
        for section in soup.find_all("section"):
            h2 = section.find("h2", attrs={"data-test": "editorDisplayRole"})
            if not h2:
                continue
            role = h2.get_text(strip=True)

            for card in section.select("[data-test='editorListing']"):
                full_name = card.get_text(strip=True)
                if not full_name:
                    continue

                affiliation_el = card.find_next_sibling("div", class_="u-text-default")
                affiliation = None
                country = None
                if affiliation_el:
                    affiliation = affiliation_el.get_text(strip=True)
                    parts = [p.strip() for p in affiliation.split(",")]
                    if len(parts) >= 2:
                        country = parts[-1]

                results.append({
                    "role": role,
                    "full_name": full_name,
                    "affiliation": affiliation,
                    "country": country,
                    "raw_text": str(card),
                })
        return results

    # ------------------------------------------------------------------ #
    # Option 2: prose <p><strong>Role</strong><br>Name, ...<br>...        #
    # ------------------------------------------------------------------ #
    for p in soup.find_all("p"):
        current_role = None

        # Walk child nodes so we respect the order of <strong> and <br> segments
        # Collect runs of content between <br> tags; a <strong> resets the role.
        segments = []   # list of (nodes_between_brs)
        current_nodes = []

        for node in p.children:
            tag = getattr(node, "name", None)
            if tag == "br":
                segments.append(current_nodes)
                current_nodes = []
            else:
                current_nodes.append(node)
        segments.append(current_nodes)  # last segment after final <br>

        for seg_nodes in segments:
            # Check whether this segment opens with a <strong> (role marker)
            non_empty = [n for n in seg_nodes if str(n).strip()]
            if not non_empty:
                continue

            first = non_empty[0]
            if getattr(first, "name", None) == "strong":
                current_role = first.get_text(strip=True)
                # There may be editor text on the same segment after the <strong>
                # (rare but possible); collect remaining nodes as an editor entry
                remaining_nodes = non_empty[1:]
            else:
                remaining_nodes = non_empty

            if not current_role or not remaining_nodes:
                continue

            # Build text + check for <em> (option 2a vs 2b)
            em_el = next(
                (n for n in remaining_nodes if getattr(n, "name", None) == "em"),
                None
            )

            # Full text of this segment (minus the leading <strong> already handled)
            seg_soup = BeautifulSoup(
                "".join(str(n) for n in remaining_nodes), "lxml"
            )

            if em_el:
                # Option 2a: affiliation is inside <em>
                affiliation = em_el.get_text(strip=True)
                # Remove the <em> to isolate the name portion
                seg_soup.find("em").decompose()
                raw_name = seg_soup.get_text(separator=" ", strip=True)
            else:
                # Option 2b: everything after first comma is affiliation
                raw_name = seg_soup.get_text(separator=" ", strip=True)
                affiliation = None

            # Name = text up to first comma, stripped of credentials noise
            comma_idx = raw_name.find(",")
            if comma_idx != -1:
                full_name = raw_name[:comma_idx].strip()
                if em_el is None:
                    affiliation = raw_name[comma_idx + 1:].strip()
            else:
                full_name = raw_name.strip()

            # Honorific / title prefixes to strip (e.g. "Dr. rer. nat.", "Prof.")
            full_name = re.sub(
                r"^(Dr\.?\s*(rer\.\s*nat\.)?\s*|Prof\.?\s*|Professor\s*)",
                "", full_name, flags=re.IGNORECASE
            ).strip()

            if not full_name:
                continue

            # Country = text after last comma in affiliation
            country = None
            if affiliation:
                affiliation = re.sub(r"\s+", " ", affiliation).strip(" ,\u00a0")
                parts = [p.strip() for p in affiliation.split(",")]
                if len(parts) >= 2:
                    country = parts[-1]

            results.append({
                "role": current_role,
                "full_name": full_name,
                "affiliation": affiliation if affiliation else None,
                "country": country,
                "raw_text": "".join(str(n) for n in seg_nodes),
            })

    return results
