"""
relocate_countryname.py
───────────────────────
For editors whose `country` IS NULL, extract the substring after the last
comma in `affiliation` and write it to `country`.

Applies only to editors whose journal belongs to one of the target publishers.

Target publishers:
    apa, cup, imedpub, longdom, sage, scirp, scitechnol

Usage:
    python relocate_countryname.py           # dry-run (preview only)
    python relocate_countryname.py --write   # actually update the DB
"""

import os
import sys
import pymysql
from dotenv import load_dotenv

load_dotenv()

TARGET_PUBLISHERS = ("apa", "cup", "iMedPub", "longdom", "sage", "scirp", "scitechnol")

QUERY = """
    SELECT
        e.editor_id,
        e.affiliation,
        e.country
    FROM editors e
    INNER JOIN journals j ON j.journal_id = e.journal_id
    WHERE j.publisher_key IN %s
      AND e.country IS NULL
      AND e.affiliation IS NOT NULL
      AND e.affiliation LIKE '%%,%%'
"""


def db_connect():
    return pymysql.connect(
        host=os.getenv("DB_HOST"),
        user=os.getenv("DB_USER"),
        password=os.getenv("DB_PASSWORD"),
        database=os.getenv("DB_NAME"),
        charset="utf8mb4",
        autocommit=True,
        cursorclass=pymysql.cursors.DictCursor,
    )


MAX_COUNTRY_LEN = 128  # mirrors the VARCHAR(128) column

def extract_country(affiliation: str) -> str | None:
    """
    Return the trimmed token after the last comma, or None if it looks
    too long or institutional to be a country name.
    """
    parts = affiliation.rsplit(",", 1)
    if len(parts) != 2:
        return None
    candidate = parts[1].strip()
    if not candidate:
        return None
    if len(candidate) > MAX_COUNTRY_LEN:
        return None          # too long for the column — skip
    if len(candidate) > 60:
        return None          # suspiciously long to be a country name — skip
    # Skip tokens that look like institution/address fragments
    institutional_keywords = (
        "university", "institute", "hospital", "college", "school",
        "department", "faculty", "center", "centre", "research",
        "ministry", "street", "avenue", "road", "floor", "suite",
        "p.o.", "po box",
    )
    if any(kw in candidate.lower() for kw in institutional_keywords):
        return None
    return candidate


def main():
    dry_run = "--write" not in sys.argv

    conn = db_connect()

    with conn.cursor() as cur:
        cur.execute(QUERY, (TARGET_PUBLISHERS,))
        rows = cur.fetchall()

    print(f"Rows with NULL country and a comma in affiliation: {len(rows)}\n")

    if not rows:
        print("Nothing to do.")
        conn.close()
        return

    # Build list of (editor_id, affiliation, extracted_country)
    updates: list[tuple[int, str, str]] = []
    skipped: list[tuple[int, str]] = []

    for row in rows:
        country = extract_country(row["affiliation"])
        if country:
            updates.append((row["editor_id"], row["affiliation"], country))
        else:
            skipped.append((row["editor_id"], row["affiliation"]))

    # ── Preview ──────────────────────────────────────────────────────────────
    aff_w = min(max(len(r["affiliation"]) for r in rows), 70)
    print(f"{'AFFILIATION':<{aff_w}}  →  EXTRACTED COUNTRY")
    print("─" * (aff_w + 30))
    for editor_id, affiliation, country in updates:
        display_aff = affiliation if len(affiliation) <= aff_w else affiliation[:aff_w - 1] + "…"
        print(f"{display_aff:<{aff_w}}  →  {country}")

    if skipped:
        print(f"\nSkipped (no extractable country after comma): {len(skipped)}")
        for editor_id, affiliation in skipped:
            print(f"  editor_id={editor_id}  affiliation={affiliation!r}")

    print(f"\n{'─' * 60}")
    print(f"  Rows fetched   : {len(rows)}")
    print(f"  To be updated  : {len(updates)}")
    print(f"  Skipped        : {len(skipped)}")

    # ── Write ─────────────────────────────────────────────────────────────────
    if dry_run:
        print("\nDry-run mode — nothing written.  Re-run with --write to apply.")
    else:
        with conn.cursor() as cur:
            cur.executemany(
                "UPDATE editors SET country = %s WHERE editor_id = %s",
                [(country, editor_id) for editor_id, _, country in updates],
            )
        print(f"\n✔  Updated {len(updates)} rows in editors.country")

    conn.close()


if __name__ == "__main__":
    main()
