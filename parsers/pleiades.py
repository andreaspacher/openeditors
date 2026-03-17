import re
from bs4 import BeautifulSoup

def parse_pleiades(container_html: str) -> list[dict]:
    """
    Parses Pleiades-style editor listings with format:
    <h4>ROLE</h4>
    <div class="collapse in"> (or just .collapse)
        <table class="table table-condensed table-hover people-att">
            <tr>
                <td>NAME</td>
                <td>AFFILIATION info<br>Optional ORCID: <a href="https://orcid.org/...">ORCID ID</a></td>
            </tr>
        </table>
    </div>
    """
    soup = BeautifulSoup(container_html, "lxml")
    results = []
    
    # Find all h4 elements that likely contain role information
    # They have class "BlockHeader" and contain the role in a span.over-dashed
    for h4 in soup.find_all("h4", class_="BlockHeader"):
        # Extract role from the span with class "over-dashed"
        role_span = h4.find("span", class_="over-dashed")
        if not role_span:
            continue
        
        role = role_span.get_text(strip=True)
        
        # Find the associated content div (next sibling with class "collapse" or "collapse in")
        # The div has an ID that matches the data-target of the h4
        target_id = h4.get("data-target", "").lstrip("#")
        content_div = None
        
        if target_id:
            content_div = soup.find("div", id=target_id)
        else:
            # Fallback: look for the next div with collapse class
            content_div = h4.find_next_sibling("div", class_=lambda c: c and "collapse" in c)
        
        if not content_div:
            continue
        
        # Find the table within the content div
        table = content_div.find("table", class_="people-att")
        if not table:
            continue
        
        # Process each row in the table
        for row in table.find_all("tr"):
            cells = row.find_all("td")
            if len(cells) < 2:
                continue
            
            # Extract name from first cell
            name_cell = cells[0]
            full_name = name_cell.get_text(strip=True)
            
            # Extract affiliation and ORCID from second cell
            affiliation_cell = cells[1]
            
            # Get full text for affiliation
            affiliation_text = affiliation_cell.get_text(separator=" ", strip=True)
            
            # Extract ORCID if present
            orcid = None
            orcid_link = affiliation_cell.find("a", href=lambda h: h and "orcid.org" in h)
            if orcid_link:
                orcid = orcid_link.get_text(strip=True)
                # Also get the full URL if needed
                # orcid_url = orcid_link.get("href")
            
            # Clean up affiliation text by removing ORCID/Scopus parts
            # This is optional - you might want to keep the full text
            affiliation_clean = affiliation_text
            if orcid:
                # Remove ORCID line from affiliation text
                orcid_text = f"ORCID: {orcid}"
                affiliation_clean = affiliation_clean.replace(orcid_text, "").strip()
                # Also remove any standalone ORCID text variations
                affiliation_clean = affiliation_clean.replace("ORCID:", "").replace(orcid, "").strip()
            
            # Clean up multiple spaces and punctuation at ends
            affiliation_clean = re.sub(r'\s+', ' ', affiliation_clean).strip(' ,;')
            
            results.append({
                "role": role,
                "full_name": full_name,
                "affiliation": affiliation_clean if affiliation_clean else None,
                "orcid": orcid,
                "raw_text": str(row),
            })
    
    return results
