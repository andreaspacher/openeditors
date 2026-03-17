import re
from bs4 import BeautifulSoup

def parse_inderscience(container_html: str) -> list[dict]:
    """
    Inderscience structure inside #edboard-content:
      <h3>Role</h3>
      <ul>
        <li><b>Surname</b>, Given Name, Affiliation, Country
          <br>(email.info<img alt="@">domain.com)   ← optional, obfuscated
        </li>
      </ul>

    Name reconstruction: family = <b> text, given = first token after first comma,
    affiliation = next token(s), country = last comma-separated token before email.
    """
    soup = BeautifulSoup(container_html, "lxml")
    results = []
    current_role = None

    for el in soup.find_all(["h3", "ul"]):
        if el.name == "h3":
            current_role = el.get_text(separator=" ", strip=True)
            continue

        if el.name == "ul" and current_role:
            for li in el.find_all("li", recursive=False):
                raw = str(li)

                # --- Email: reconstruct from obfuscated img[@alt="@"] ---
                email = None
                img = li.find("img", alt="@")
                if img:
                    # Text before img = local part, text after = domain
                    # They sit inside a text node run like: "(local.partATdomain.com)"
                    # Easiest: grab all text in the parenthetical span around the img
                    parent_text = img.parent.get_text(separator="", strip=False)
                    # Remove surrounding parens and whitespace
                    email_raw = re.sub(r"[()]", "", parent_text).strip()
                    # Replace the alt text rendered as literal "@" gap
                    # BeautifulSoup renders img as nothing in get_text, so the
                    # local and domain parts end up adjacent — insert "@"
                    # by finding where the split is via the img position
                    before = img.previous_sibling
                    after = img.next_sibling
                    local = before.strip() if before and isinstance(before, str) else ""
                    domain = after.strip() if after and isinstance(after, str) else ""
                    if local and domain:
                        email = f"{local}@{domain}"
                    # Remove the whole parenthetical node so it doesn't pollute name
                    img.parent.decompose() if img.parent.name != "li" else img.decompose()

                # --- Family name from <b> ---
                b = li.find("b")
                if not b:
                    continue
                family = b.get_text(strip=True)
                b.decompose()

                # --- Remaining text: ", Given, Affiliation, Country" ---
                remaining = li.get_text(separator="", strip=False)
                # Strip leading comma/space
                remaining = re.sub(r"^\s*,\s*", "", remaining).strip()

                parts = [p.strip() for p in remaining.split(",")]
                # parts[0]  = given name
                # parts[-1] = country
                # parts[1:-1] = affiliation (rejoin with comma)
                given = parts[0] if parts else ""
                country = parts[-1] if len(parts) > 1 else None
                affiliation = ", ".join(parts[1:-1]) if len(parts) > 2 else None

                full_name = f"{given} {family}".strip() if given else family

                results.append({
                    "role": current_role,
                    "given": given,
                    "family": family,
                    "full_name": full_name,
                    "affiliation": affiliation,
                    "country": country,
                    "email": email,
                    "raw_text": raw,
                })

    return results
