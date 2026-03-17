"""
infer_ror.py
────────────
For each editor whose `ror` column is NULL, attempt to resolve the
`affiliation` string to a ROR ID via the ROR affiliation API:

    GET https://api.ror.org/v2/organizations?affiliation=<URL-encoded string>
    GET https://api.ror.org/v2/organizations?affiliation=<URL-encoded string>&single_search

Confidence rule: accept only if the top result has chosen=True.
Does NOT filter by score — per ROR docs, use chosen:true, not score.

If a ROR is found and `country` is NULL, also backfills country from
the ROR metadata.

Requires in .env:
    ROR_CLIENT_ID=...   (sent as Client-Id header)

Usage:
    python infer_ror.py              # dry-run (preview only)
    python infer_ror.py --write      # apply to DB
    python infer_ror.py --debug      # print every row's fate + full URL
    python infer_ror.py --write --debug

Dependencies:
    pip install pymysql python-dotenv requests
"""

import os
import re
import sys
import time
import urllib.parse
import requests
import pymysql
from dotenv import load_dotenv

load_dotenv()

# ── Settings ─────────────────────────────────────────────────────────────────

ROR_API           = "https://api.ror.org/v2/organizations"
REQUEST_DELAY     = 0.15        # seconds between calls (well under 2000/5 min)
MAX_AFFILIATION_LEN = 300       # skip absurdly long strings outright
SCORE_FLOOR       = 0.91        # minimum score even when chosen=True; tune as needed

# ── Junk detection ───────────────────────────────────────────────────────────

# Entire-string exact matches that are clearly not institution names
_ROLE_EXACT = {
    "chair", "vice-chair", "past-chair", "co-chair",
    "secretary", "treasurer", "president", "vice president", "vice-president",
    "member", "corresponding member", "associate member",
    "editor", "reviewer", "fellow", "student", "intern",
    "director", "officer", "consultant", "staff", "liaison",
    "ceo", "cfo", "coo",
}

# Publisher abbreviation + role (e.g. "ASCE Staff", "IEEE Liaison")
_PUBLISHER_ROLE_RE = re.compile(
    r"^(asce|ieee|acm|apa|cup|sage|acs|aps|rsc|agu)\s+"
    r"(staff|liaison|member|fellow|editor|reviewer)$",
    re.I,
)

# Other clear non-institution patterns
_JUNK_RE_LIST = [
    re.compile(r"^\d{4}-\d{4}-\d{4}-\d{3}[\dX]$", re.I),   # bare ORCID
    re.compile(r"orcid\.org",             re.I),             # ORCID URL
    re.compile(r"^https?://",             re.I),             # any URL
    re.compile(r"\b(mr|mrs|ms|dr|prof)\.?\s+[A-Z]", re.I),  # titled person name
]


def looks_like_junk(text: str) -> bool:
    if not text or not text.strip():
        return True
    t = text.strip()
    if len(t) < 3:
        return True
    if len(t) > MAX_AFFILIATION_LEN:
        return True
    if t.lower() in _ROLE_EXACT:
        return True
    if _PUBLISHER_ROLE_RE.match(t):
        return True
    for pat in _JUNK_RE_LIST:
        if pat.search(t):
            return True
    return False


# ── Pre-processing ───────────────────────────────────────────────────────────

_TITLE_PREFIX_RE = re.compile(
    r"^(postdoctoral researcher|postdoc|researcher|professor|dr\.?|prof\.?|"
    r"associate professor|assistant professor|senior lecturer|lecturer|"
    r"visiting scholar|research fellow|fellow|scientist|physician)\s+"
    r"(at|@|,|in)?\s*",
    re.I,
)


def clean_affiliation(text: str) -> str:
    """Strip leading role prefixes so the affiliation API gets a cleaner string."""
    t = text.strip()
    t = _TITLE_PREFIX_RE.sub("", t).strip(" ,")
    return t


# ── ROR API ──────────────────────────────────────────────────────────────────

def _ror_headers() -> dict:
    client_id = os.getenv("ROR_CLIENT_ID", "").strip()
    return {"Client-Id": client_id} if client_id else {}


def _affiliation_url(affiliation: str, single_search: bool = False) -> str:
    """Build the ROR affiliation API URL with a URL-encoded affiliation string."""
    encoded = urllib.parse.quote(affiliation, safe="")
    url = f"{ROR_API}?affiliation={encoded}"
    if single_search:
        url += "&single_search"
    return url


def _parse_ror_response(data: dict) -> tuple[str | None, float, str | None, str | None]:
    """
    Extract (ror_id, score, country) from a ROR affiliation API response.
    Returns (None, 0.0, None, None) if no chosen=True result is present.
    """
    items = data.get("items", [])
    if not items:
        return None, 0.0, None, None

    top = items[0]
    if not top.get("chosen", False):
        return None, top.get("score", 0.0), None, None

    score   = top.get("score", 0.0)
    if score < SCORE_FLOOR:
        return None, score, None, None
    org     = top.get("organization", {})
    ror_url = org.get("id", "")
    ror_id  = ror_url.split("/")[-1] if ror_url else None

    country = None
    locations = org.get("locations", [])
    if locations:
        country = locations[0].get("geonames_details", {}).get("country_name")

    ror_display = None
    for name_obj in org.get("names", []):
        if "ror_display" in name_obj.get("types", []):
            ror_display = name_obj.get("value")
            break

    return ror_id, score, country, ror_display


def query_ror(affiliation: str, debug: bool = False):
    """
    Try the multisearch affiliation endpoint first; if no accepted result,
    retry with &single_search.

    Returns:
        (status, ror_id, score, country, ror_display, error_msg, url_used)

    where status is one of:
        - "matched"
        - "no_match"
        - "api_error"
    """
    for single in (False, True):
        url = _affiliation_url(affiliation, single_search=single)
        try:
            resp = requests.get(url, headers=_ror_headers(), timeout=15)
            resp.raise_for_status()
            data = resp.json()
        except Exception as exc:
            return "api_error", None, 0.0, None, None, str(exc), url

        ror_id, score, country, ror_display = _parse_ror_response(data)
        if ror_id:
            return "matched", ror_id, score, country, ror_display, None, url

    return "no_match", None, 0.0, None, None, None, url


# ── Database ─────────────────────────────────────────────────────────────────

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


def ensure_ror_column(conn):
    columns = {
        "ror": "ALTER TABLE editors ADD COLUMN ror VARCHAR(64) DEFAULT NULL",
        "affiliation_ror": "ALTER TABLE editors ADD COLUMN affiliation_ror VARCHAR(255) DEFAULT NULL",
        "ror_lookup_status": "ALTER TABLE editors ADD COLUMN ror_lookup_status VARCHAR(20) DEFAULT NULL",
        "ror_lookup_attempted_at": "ALTER TABLE editors ADD COLUMN ror_lookup_attempted_at DATETIME NULL",
        "ror_lookup_score": "ALTER TABLE editors ADD COLUMN ror_lookup_score DECIMAL(6,4) NULL",
        "ror_lookup_note": "ALTER TABLE editors ADD COLUMN ror_lookup_note VARCHAR(255) DEFAULT NULL",
        "ror_lookup_affiliation_clean": "ALTER TABLE editors ADD COLUMN ror_lookup_affiliation_clean VARCHAR(300) DEFAULT NULL",
    }

    with conn.cursor() as cur:
        for col, ddl in columns.items():
            cur.execute(f"SHOW COLUMNS FROM editors LIKE '{col}'")
            if not cur.fetchone():
                cur.execute(ddl)
                print(f"✔  Added column editors.{col}")


def fetch_candidates(conn) -> list[dict]:
    with conn.cursor() as cur:
        cur.execute("""
            SELECT editor_id, affiliation, country
            FROM editors
            WHERE ror IS NULL
              AND affiliation IS NOT NULL
              AND affiliation != ''
              AND ror_lookup_status IS NULL
        """)
        return cur.fetchall()


# ── Main ─────────────────────────────────────────────────────────────────────

def main():
    dry_run = "--write" not in sys.argv
    debug   = "--debug" in sys.argv

    conn = db_connect()

    if not dry_run:
        ensure_ror_column(conn)

    rows = fetch_candidates(conn)
    print(f"Candidates (ror IS NULL, affiliation non-empty): {len(rows)}\n")

    if not rows:
        print("Nothing to do.")
        conn.close()
        return

    resolved  = []   # (editor_id, ror_id, score, country_ror, fill_country)
    junk_skip = []   # (editor_id, affiliation)
    no_match  = []   # (editor_id, affiliation, score)
    written   = 0

    for i, row in enumerate(rows):
        editor_id   = row["editor_id"]
        affiliation = row["affiliation"]
        country_db  = row["country"]

        # 1. Junk filter
        if looks_like_junk(affiliation):
            junk_skip.append((editor_id, affiliation))
            if debug:
                print(f"  [JUNK ]  {editor_id}  {affiliation!r}")
            if not dry_run:
                with conn.cursor() as cur:
                    cur.execute("""
                        UPDATE editors
                        SET ror_lookup_status = %s,
                            ror_lookup_attempted_at = NOW(),
                            ror_lookup_note = %s
                        WHERE editor_id = %s
                    """, ("junk", "Skipped by junk filter", editor_id))
            continue
        
        # 2. Clean & query
        query_str = clean_affiliation(affiliation)
        if debug:
            print(f"  [QUERY]  {editor_id}  raw={affiliation!r}  →  query={query_str!r}", flush=True)

        status, ror_id, score, country_ror, ror_display, error_msg, url_used = query_ror(query_str, debug=debug)

        # 3. Classify
        if status == "matched":
            fill_country = country_ror if (country_db is None and country_ror) else None
            resolved.append((editor_id, ror_id, score, country_ror, ror_display, fill_country))

            if debug:
                print(
                    f"  [MATCH]  {editor_id}  ror={ror_id}  score={score:.2f}  "
                    f"country={country_ror}  name={ror_display!r}"
                )

            if not dry_run:
                with conn.cursor() as cur:
                    cur.execute("""
                        UPDATE editors
                        SET ror = %s,
                            affiliation_ror = %s,
                            ror_lookup_status = %s,
                            ror_lookup_attempted_at = NOW(),
                            ror_lookup_score = %s,
                            ror_lookup_note = NULL,
                            ror_lookup_affiliation_clean = %s
                        WHERE editor_id = %s
                    """, (
                        ror_id,
                        ror_display,
                        "matched",
                        score,
                        query_str,
                        editor_id,
                    ))

                    if fill_country:
                        cur.execute("""
                            UPDATE editors
                            SET country = %s
                            WHERE editor_id = %s
                              AND country IS NULL
                        """, (fill_country, editor_id))

                written += 1

        elif status == "no_match":
            no_match.append((editor_id, affiliation, score))

            if debug:
                print(f"  [NOMATCH] {editor_id}  score={score:.2f}  query={query_str!r}")

            if not dry_run:
                with conn.cursor() as cur:
                    cur.execute("""
                        UPDATE editors
                        SET ror_lookup_status = %s,
                            ror_lookup_attempted_at = NOW(),
                            ror_lookup_score = %s,
                            ror_lookup_note = %s,
                            ror_lookup_affiliation_clean = %s
                        WHERE editor_id = %s
                    """, (
                        "no_match",
                        score,
                        f"No confident ROR match for query: {query_str}"[:255],
                        query_str[:250],
                        editor_id,
                    ))

        elif status == "api_error":
            if debug:
                print(f"  [ERROR]  {editor_id}  query={query_str!r}  error={error_msg}")

            if not dry_run:
                with conn.cursor() as cur:
                    cur.execute("""
                        UPDATE editors
                        SET ror_lookup_status = %s,
                            ror_lookup_attempted_at = NOW(),
                            ror_lookup_note = %s,
                            ror_lookup_affiliation_clean = %s
                        WHERE editor_id = %s
                    """, (
                        "api_error",
                        (error_msg or "Unknown API error")[:255],
                        query_str,
                        editor_id,
                    ))

        else:
            if debug:
                print(f"  [ERROR]  {editor_id}  unexpected status={status!r}")

    # ── Summary ───────────────────────────────────────────────────────────────
    print(f"\n{'─'*80}")
    print(f"{'EDITOR_ID':<12} {'SCORE':>6}  {'ROR ID':<14} {'COUNTRY':>24}  FILL?")
    print(f"{'─'*80}")
    for editor_id, ror_id, score, country_ror, ror_display, fill_country in resolved:
        fill = "← country" if fill_country else ""
        name_str = (ror_display or "")[:32]
        print(f"{editor_id:<12} {score:>6.2f}  {ror_id:<14} {name_str:<32}  {(country_ror or ''):>22}  {fill}")

    print(f"\n{'─'*80}")
    print(f"  Total candidates : {len(rows)}")
    print(f"  Junk / skipped   : {len(junk_skip)}")
    print(f"  Resolved (ROR)   : {len(resolved)}")
    print(f"    of which also fill country : {sum(1 for _, _, _, _, _, f in resolved if f)}")
    print(f"    of which have ror_display  : {sum(1 for _,_,_,_,d,_ in resolved if d)}")
    print(f"  No confident match           : {len(no_match)}")

    if no_match:
        print(f"\nSample unmatched affiliations (score shown):")
        for editor_id, affiliation, score in no_match[:20]:
            print(f"  [{score:.2f}] {affiliation!r}")
        if len(no_match) > 20:
            print(f"  … and {len(no_match) - 20} more")

    if dry_run:
        print("\nDry-run — nothing written.  Re-run with --write to apply.")
    else:
        print(f"\n✔  Written incrementally: {written} ROR IDs committed to DB")

    conn.close()


if __name__ == "__main__":
    main()