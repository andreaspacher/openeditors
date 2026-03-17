"""
harmonize_countries.py
──────────────────────
Reads editors.country_raw from MySQL, resolves every distinct value to a
standardised country name, previews the mapping, then (optionally) writes:
  • editors.country   – harmonised country name
  • editors.email     – email address extracted from the raw value (if any)
  • editors.orcid     – ORCID extracted from the raw value (if any)

Dependencies:
    pip install pymysql python-dotenv country_converter rapidfuzz
"""

import os
import re
import pymysql
from dotenv import load_dotenv
import country_converter as coco
from rapidfuzz import process, fuzz

load_dotenv()

# ── country_converter instance (loads once) ──────────────────────────────────
CC = coco.CountryConverter()

# ── Manual overrides ─────────────────────────────────────────────────────────
# Only needed for things country_converter can't handle:
#   • Sub-national regions / cities
#   • Politically disputed names you want to control explicitly
#
# Keys are lowercase; matched with word-boundary regex BEFORE any library call.
# Values are the final standardised strings you want stored.
OVERRIDES: dict[str, str] = {
    # ── Cities ───────────────────────────────────────────────────────────────
    "cairo":        "Egypt",
    "tehran":       "Iran",
    "karachi":      "Pakistan",
    "lahore":       "Pakistan",
    "riyadh":       "Saudi Arabia",
    "jeddah":       "Saudi Arabia",
    "dubai":        "United Arab Emirates",
    "abu dhabi":    "United Arab Emirates",
    "new delhi":    "India",
    "delhi":        "India",
    "mumbai":       "India",
    "kolkata":      "India",
    "montreal":     "Canada",
    "jerusalem":    "Israel",
    "florence":     "Italy",
    "prague":       "Czechia",
    "rio de janeiro":"Brazil",
    "srinagar":     "India",
    "tai po":       "Hong Kong",
    # "hyderabad" omitted intentionally — ambiguous (India vs Pakistan)

    # ── Sub-national regions ─────────────────────────────────────────────────
    "kpk":              "Pakistan",      # Khyber Pakhtunkhwa
    "punjab":           "India",         # change to "Pakistan" if your data skews that way
    "west bengal":      "India",
    "tamil nadu":       "India",
    "maharashtra":      "India",
    "kerala":           "India",
    "gujarat":          "India",
    "uttar pradesh":    "India",
    "ontario":          "Canada",
    "quebec":           "Canada",
    "british columbia": "Canada",
    "california":       "United States",
    "new york":         "United States",
    "texas":            "United States",
    "florida":          "United States",

    # ── Politically controlled spellings ─────────────────────────────────────
    "south korea":              "South Korea",
    "north korea":              "North Korea",
    "republic of korea":        "South Korea",
    "korea, republic of":       "South Korea",
    "democratic people's republic of korea": "North Korea",
    "taiwan":                   "Taiwan",
    "taiwan, province of china": "Taiwan",
    "roc":                      "Taiwan",     # Republic of China
    "hong kong":                "Hong Kong",
    "macao":                    "Macao",
    "macau":                    "Macao",
    "iran":                     "Iran",
    "islamic republic of iran": "Iran",
    "russia":                   "Russia",
    "russian federation":       "Russia",
    "syria":                    "Syria",
    "syrian arab republic":     "Syria",
    "vietnam":                  "Vietnam",
    "viet nam":                 "Vietnam",
    "laos":                     "Laos",
    "lao people's democratic republic": "Laos",
    "bolivia":                  "Bolivia",
    "venezuela":                "Venezuela",
    "moldova":                  "Moldova",
    "tanzania":                 "Tanzania",
    "libya":                    "Libya",
    "czechia":                  "Czechia",
    "czech republic":           "Czechia",
    "the netherlands":          "Netherlands",
    "holland":                  "Netherlands",
    "kingdom of the netherlands": "Netherlands",
    "netherlands, kingdom of":  "Netherlands",
    "great britain":            "United Kingdom",
    "england":                  "United Kingdom",
    "scotland":                 "United Kingdom",
    "wales":                    "United Kingdom",

    # ── Common abbreviations ─────────────────────────────────────────────────
    "u.s.a":      "United States",
    "u.s.":       "United States",
    "u.k.":       "United Kingdom",
    "uae":        "United Arab Emirates",
    "p.r. china": "China",
    "p.r.china":  "China",
    "prc":        "China",

    # ── Chinese cities ───────────────────────────────────────────────────────
    "beijing":    "China",
    "shanghai":   "China",
    "guangzhou":  "China",
    "shenzhen":   "China",
    "chengdu":    "China",
    "wuhan":      "China",
    "xi'an":      "China",
    "xian":       "China",
    "nanjing":    "China",
    "hangzhou":   "China",
    "tianjin":    "China",
    "chongqing":  "China",
    "wuxi":       "China",
    "suzhou":     "China",
    "xiamen":     "China",
    "zhengzhou":  "China",
    "zhenjiang":  "China",
    "zigong":     "China",
    "xiaozhai":   "China",
    "xinjiang":   "China",
    "zhejiang":   "China",
    "shenyang":   "China",
    "qingdao":    "China",
    "hefei":      "China",
    "kunming":    "China",
    "harbin":     "China",
    "changsha":   "China",
    "fuzhou":     "China",
    "jinan":      "China",
    "nanchang":   "China",
    "wenzhou":    "China",
    "dalian":     "China",

    # ── Other Asian cities ────────────────────────────────────────────────────
    "yerevan":      "Armenia",
    "zanjan":       "Iran",
    "seoul":        "South Korea",
    "busan":        "South Korea",
    "bangkok":      "Thailand",
    "istanbul":     "Turkey",
    "ankara":       "Turkey",
    "manila":       "Philippines",
    "jakarta":      "Indonesia",
    "kuala lumpur": "Malaysia",
    "colombo":      "Sri Lanka",
    "dhaka":        "Bangladesh",
    "kathmandu":    "Nepal",
    "yilan":        "Taiwan",
    "taipei":       "Taiwan",

    # ── African cities ────────────────────────────────────────────────────────
    "zaria":         "Nigeria",
    "lagos":         "Nigeria",
    "abuja":         "Nigeria",
    "nairobi":       "Kenya",
    "accra":         "Ghana",
    "addis ababa":   "Ethiopia",
    "dar es salaam": "Tanzania",

    # ── European cities ───────────────────────────────────────────────────────
    "milano":   "Italy",
    "messina":  "Italy",
    "perugia":  "Italy",
    "cordoba":  "Spain",
    "bornova":  "Turkey",
    "izmir":    "Turkey",

    # ── German/local language country names ──────────────────────────────────
    "österreich":   "Austria",
    "oesterreich":  "Austria",
    "schweiz":      "Switzerland",
    "suisse":       "Switzerland",
    "svizzera":     "Switzerland",
    "deutschland":  "Germany",
    "allemagne":    "Germany",
    "espana":       "Spain",
    "espana":       "Spain",
    "italia":       "Italy",
    "belgique":     "Belgium",
    "belgie":       "Belgium",
    "polska":       "Poland",
    "magyarorszag": "Hungary",
    "turkiye":      "Turkey",
}

# Pre-compile word-boundary patterns for each override key (done once at load).
_OVERRIDE_PATTERNS: list[tuple[re.Pattern, str]] = [
    (re.compile(rf'\b{re.escape(key)}\b', re.IGNORECASE), value)
    for key, value in OVERRIDES.items()
]


# ── Email / ORCID extractors ─────────────────────────────────────────────────

# Standard email address, also catches obfuscated forms with spaces around @
_EMAIL_RE = re.compile(
    r'[\w.+\-]+\s*@\s*[\w.\-]+\.[a-zA-Z]{2,}',
    re.IGNORECASE,
)

# ORCID bare ID: 0000-0000-0000-000X
_ORCID_RE = re.compile(r'\b(\d{4}-\d{4}-\d{4}-\d{3}[\dXx])\b')

# ORCID URL: orcid.org/0000-...
_ORCID_URL_RE = re.compile(
    r'orcid\.org/(\d{4}-\d{4}-\d{4}-\d{3}[\dXx])',
    re.IGNORECASE,
)


def extract_email(raw: str) -> str | None:
    """Return the first email address found in raw, or None."""
    m = _EMAIL_RE.search(raw)
    if m:
        return re.sub(r'\s', '', m.group(0))   # strip spaces around @
    return None


def extract_orcid(raw: str) -> str | None:
    """Return the first ORCID found in raw (bare ID, uppercased), or None."""
    m = _ORCID_URL_RE.search(raw)
    if m:
        return m.group(1).upper()
    m = _ORCID_RE.search(raw)
    if m:
        return m.group(1).upper()
    return None


# ── Noise-rejection patterns ──────────────────────────────────────────────────
# Values matching any of these return None for country; email/ORCID extraction
# still runs first so we don't lose those signals.
_NOISE_PATTERNS: list[re.Pattern] = [
    re.compile(r'@'),                               # email addresses
    re.compile(r'https?://', re.IGNORECASE),        # URLs
    re.compile(r'^www\.', re.IGNORECASE),           # bare www. URLs
    re.compile(r'\.(com|org|net|edu|de|ch|uk|cn)\b', re.IGNORECASE),
    re.compile(r'^\|?\s*\d{4,6}\b'),               # starts with postcode / number
    re.compile(r'^\d{4}[-\u2013]\d{2,4}$'),        # bare date range e.g. 2008-2013
    re.compile(r'\borcid\b', re.IGNORECASE),        # ORCID lines
    re.compile(r'\b(e-?mail|e mail)\s*:', re.IGNORECASE),
    re.compile(r'\b(h-index|citation|impact factor|published by'
               r'|springer|elsevier|wiley|frontiers in\b'
               r'|neuroscien|psychiatr|cardiol|oncol|genomic'
               r'|zebrafish|genetics|diabetes|editorial board'
               r'|self-citations|webpage|google scholar'
               r'|world economic)\b', re.IGNORECASE),
    re.compile(r'.{160,}'),                         # implausibly long bio snippets
    re.compile(r'\b(deceased|retired|emeritus|professor|phd|md|dr\.)\b', re.IGNORECASE),
    re.compile(r'^f\.y\.r\.', re.IGNORECASE),  # Former Yugoslav Republic
]


def _is_noise(raw: str) -> bool:
    return any(p.search(raw) for p in _NOISE_PATTERNS)


# ── String cleaning helpers ───────────────────────────────────────────────────

_NOISE_WORDS = re.compile(
    r'\b(province|state|region|district|department|prefecture'
    r'|governorate|oblast|city|municipality|republic'
    r'|postgraduate|institute|medical|education|research'
    r'|university|hospital|college|national|center|centre'
    r'|school|faculty|department)\b',
    re.IGNORECASE,
)


def _clean_token(text: str) -> str:
    """Strip parenthetical noise, institutional keywords, extra whitespace."""
    text = re.sub(r'\(.*?\)', '', text)
    text = _NOISE_WORDS.sub('', text)
    text = re.sub(r'\s{2,}', ' ', text)
    return text.strip(' ,.-|')


def _strip_extras(raw: str) -> str:
    """
    Remove known suffixes/prefixes that clutter country resolution:
      • leading pipe / whitespace     e.g. "|Spain"
      • trailing ORCID block          e.g. "France ORCID: 0000-..."
      • trailing / embedded email     e.g. "Germany user@uni.de"
      • trailing URL                  e.g. "Spain| https://..."
      • bracket blocks                e.g. "China(user@domain.com)"
      • anything after pipe           e.g. "Cyprus| https://..."
    """
    s = raw.strip().lstrip('|').strip()
    s = re.sub(r'\s*\borcid\b.*$', '', s, flags=re.IGNORECASE)
    s = re.sub(r'\s*https?://\S+', '', s, flags=re.IGNORECASE)
    s = re.sub(r'\s+[\w.+\-]+@\S+', '', s)          # trailing email
    s = re.sub(r'\([^)]*\)', '', s)                  # bracket block (email, code, etc.)
    s = re.sub(r'\s*\|.*$', '', s)                   # anything after |
    s = re.sub(r'\s+\S+@\S+', '', s)                 # any remaining email-like token
    return s.strip(' ,.-')


def extract_candidate(raw: str) -> str:
    """
    Pull the most likely country token from a noisy affiliation string.
    Walk comma-separated parts from the right; return the first clean token.
    """
    parts = [p.strip() for p in raw.split(',') if p.strip()]
    for part in reversed(parts):
        cleaned = _clean_token(part)
        if len(cleaned) >= 2:
            return cleaned
    return _clean_token(raw)


# ── All country names known to country_converter (for fuzzy matching) ─────────

def _build_coco_names() -> list[str]:
    df = CC.data
    names: set[str] = set()
    for col in ('name_short', 'name_official', 'ISO2', 'ISO3'):
        if col in df.columns:
            names.update(df[col].dropna().tolist())
    names.update(OVERRIDES.values())
    return list(names)


_COCO_NAMES: list[str] = _build_coco_names()


# ── Whitelist of all valid output values ─────────────────────────────────────
# Built from OVERRIDES values + every name_short coco can return.
# resolve() checks its result against this before returning — anything not on
# the list is returned as None, preventing false positives from reaching the DB.

def _build_whitelist() -> set[str]:
    df = CC.data
    valid: set[str] = set()
    if 'name_short' in df.columns:
        valid.update(df['name_short'].dropna().tolist())
    # Add all our custom override target values
    valid.update(OVERRIDES.values())
    return valid

_WHITELIST: set[str] = _build_whitelist()


def _coco_resolve(text: str) -> str | None:
    result = CC.convert(text, to='name_short', not_found=None)
    if isinstance(result, list):
        result = result[0] if result else None
    return result if result and result != 'not found' else None


# ── Core resolution function ──────────────────────────────────────────────────

def resolve(raw: str | None, fuzzy_threshold: int = 75) -> str | None:
    """
    Return a standardised country name, or None if unresolvable.

    Resolution order
    ----------------
    0.  Strip extras (ORCID, email, URL, bracket suffixes) first
    0b. Noise rejection on the stripped result -> None
    1.  Override match on stripped value
    2.  country_converter on stripped value
    3.  Extract candidate token -> override match
    4.  country_converter on candidate
    5.  Fuzzy match (rapidfuzz) on candidate
    6.  Whitelist gate: result must be a known country name or -> None
    """
    if not raw or not raw.strip():
        return None

    # 0. Strip extras FIRST so "UK ORCID:...", "USA email@...",
    #    "China(user@domain)", "UK| https://..." all reduce to just the country.
    clean = _strip_extras(raw.strip())

    if not clean:
        return None

    # 0b. Reject if what remains after stripping is still noise,
    #     or is implausibly long to be a country name (> 60 chars)
    if _is_noise(clean) or len(clean) > 60:
        return None

    # 1. Override match on stripped value
    for pattern, value in _OVERRIDE_PATTERNS:
        if pattern.search(clean):
            return value     # override values are always whitelisted by construction

    # 2. country_converter on stripped value
    result = _coco_resolve(clean)
    if result and result in _WHITELIST:
        return result

    # 3. Extract candidate token, try overrides
    candidate = extract_candidate(clean)

    for pattern, value in _OVERRIDE_PATTERNS:
        if pattern.search(candidate):
            return value

    # 4. country_converter on candidate
    result = _coco_resolve(candidate)
    if result and result in _WHITELIST:
        return result

    # 5. Fuzzy match on candidate — most likely to produce false positives,
    #    so whitelist gate is especially important here
    match = process.extractOne(
        candidate,
        _COCO_NAMES,
        scorer=fuzz.token_sort_ratio,
        score_cutoff=fuzzy_threshold,
    )
    if match:
        fuzzy_name = match[0]
        result = _coco_resolve(fuzzy_name) or fuzzy_name
        # 6. Whitelist gate: only return if it's a known valid country name
        return result if result in _WHITELIST else None

    return None


# ── Database helpers ──────────────────────────────────────────────────────────

def db_connect():
    return pymysql.connect(
        host=os.getenv('DB_HOST'),
        user=os.getenv('DB_USER'),
        password=os.getenv('DB_PASSWORD'),
        database=os.getenv('DB_NAME'),
        charset='utf8mb4',
        autocommit=True,
        cursorclass=pymysql.cursors.DictCursor,
    )


def fetch_distinct_countries(conn) -> list[str]:
    with conn.cursor() as cur:
        cur.execute(
            'SELECT DISTINCT country_raw FROM editors WHERE country_raw IS NOT NULL'
        )
        return [row['country_raw'] for row in cur.fetchall()]


def ensure_schema(conn):
    """
    Idempotently migrate the column layout:
      1. Rename  country  → country_raw  (preserves original values)
      2. Add     country  VARCHAR(100)   (harmonised name)
    Each step is skipped if the target state already exists.
    """
    with conn.cursor() as cur:
        cur.execute("SHOW COLUMNS FROM editors LIKE 'country_raw'")
        if not cur.fetchone():
            cur.execute('ALTER TABLE editors RENAME COLUMN country TO country_raw')
            print('  Renamed column: country -> country_raw')
        else:
            print('  Column country_raw already exists, skipping rename')

        cur.execute("SHOW COLUMNS FROM editors LIKE 'country'")
        if not cur.fetchone():
            cur.execute(
                'ALTER TABLE editors '
                'ADD COLUMN country VARCHAR(100) DEFAULT NULL AFTER country_raw'
            )
            print('  Added column: country (harmonised)')
        else:
            print('  Column country already exists, skipping add')


def write_mapping(conn, mapping: dict[str, str | None]):
    """Update editors.country for every distinct raw value."""
    MAX_LEN = 100  # VARCHAR(100)
    flagged = []
    with conn.cursor() as cur:
        for raw, clean in mapping.items():
            if clean is not None and len(clean) > MAX_LEN:
                # Shouldn't happen after noise filtering, but guard anyway
                flagged.append((raw, clean))
                clean = None   # write NULL rather than truncate silently
            cur.execute(
                'UPDATE editors SET country = %s WHERE country_raw = %s',
                (clean, raw),
            )
    if flagged:
        print(f'  WARNING: {len(flagged)} resolved value(s) exceeded {MAX_LEN} chars'
              f' and were written as NULL (add them to OVERRIDES or noise list):')
        for raw, clean in flagged:
            print(f'    raw={raw!r}  resolved={clean!r}')
    print(f'  Updated {len(mapping)} distinct values in editors.country')


def write_extras(conn, extras: dict[str, dict]):
    """
    Write extracted email / orcid values back to editors.
    extras = { raw_value: {'email': str|None, 'orcid': str|None} }
    Never overwrites an existing non-null / non-empty value.
    """
    email_count = orcid_count = 0
    with conn.cursor() as cur:
        for raw, found in extras.items():
            if found.get('email'):
                cur.execute(
                    'UPDATE editors SET email = %s '
                    'WHERE country_raw = %s AND (email IS NULL OR email = "")',
                    (found['email'], raw),
                )
                email_count += cur.rowcount
            if found.get('orcid'):
                cur.execute(
                    'UPDATE editors SET orcid = %s '
                    'WHERE country_raw = %s AND (orcid IS NULL OR orcid = "")',
                    (found['orcid'], raw),
                )
                orcid_count += cur.rowcount
    print(f'  Wrote {email_count} email(s) and {orcid_count} ORCID(s) to editors')


# ── Main ──────────────────────────────────────────────────────────────────────

def main():
    import argparse
    parser = argparse.ArgumentParser(
        description='Harmonize editors.country_raw -> editors.country '
                    'and extract emails/ORCIDs as a side-effect.'
    )
    parser.add_argument(
        '--write', action='store_true',
        help='Write results to the database (default: dry-run only)'
    )
    args = parser.parse_args()

    conn = db_connect()

    # Migrate columns first so country_raw exists before we query it
    ensure_schema(conn)
    print()

    print('Fetching distinct country_raw values ...')
    raw_values = fetch_distinct_countries(conn)
    print(f'  Found {len(raw_values)} distinct non-null values.\n')

    mapping:    dict[str, str | None] = {}
    extras:     dict[str, dict]       = {}
    unresolved: list[str]             = []

    for raw in sorted(raw_values, key=str.casefold):
        clean = resolve(raw)
        email = extract_email(raw)
        orcid = extract_orcid(raw)

        mapping[raw] = clean
        if email or orcid:
            extras[raw] = {'email': email, 'orcid': orcid}
        if clean is None:
            unresolved.append(raw)

    # ── Preview: country mapping ──────────────────────────────────────────────
    col_w = min(max(len(r) for r in raw_values), 60)
    print(f"{'RAW VALUE':<{col_w}}  ->  RESOLVED")
    print('-' * (col_w + 20))
    for raw, clean in sorted(mapping.items(), key=lambda x: x[0].casefold()):
        resolved_str = clean if clean else 'UNRESOLVED'
        print(f'{raw:<{col_w}}  ->  {resolved_str}')

    # ── Preview: extracted emails / ORCIDs ────────────────────────────────────
    if extras:
        print(f"\n{'RAW VALUE':<{col_w}}  ->  EXTRACTED")
        print('-' * (col_w + 40))
        for raw, found in sorted(extras.items(), key=lambda x: x[0].casefold()):
            parts = []
            if found.get('email'): parts.append(f"email={found['email']}")
            if found.get('orcid'): parts.append(f"orcid={found['orcid']}")
            print(f"{raw:<{col_w}}  ->  {', '.join(parts)}")

    # ── Summary ───────────────────────────────────────────────────────────────
    print(f"\n{'-' * 60}")
    print(f'  Total distinct values : {len(mapping)}')
    print(f'  Resolved (country)    : {len(mapping) - len(unresolved)}')
    print(f'  Unresolved            : {len(unresolved)}')
    print(f'  With email extracted  : {sum(1 for e in extras.values() if e.get("email"))}')
    print(f'  With ORCID extracted  : {sum(1 for e in extras.values() if e.get("orcid"))}')

    if unresolved:
        print('\nUnresolved values (add to OVERRIDES to fix):')
        for v in unresolved:
            print(f'    {v!r}')

    print()
    if args.write:
        write_mapping(conn, mapping)
        write_extras(conn, extras)
    else:
        print('Dry-run only -- nothing written.  Re-run with --write to apply.')

    conn.close()


if __name__ == '__main__':
    main()