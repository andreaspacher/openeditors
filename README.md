# OpenEditors

Scraping editorial board data from academic publisher websites.

---

## Overview

OpenEditors collects structured data about journal editors (names, roles, affiliations, countries; sometimes also ORCID IDs) from the websites of academic publishers. It matches raw affiliation data against the [ROR API](https://ror.org/) to obtain harmonized information about affiliations and country names. It also tracks changes over time, i.e., recording when editors are added or removed.

(Note that to avoid false positives caused by scraping failures or infinite-scroll timeouts, a confirmation window is used: an editor is only recorded as added or removed after the same outcome is observed across three consecutive scrape runs.)

The resulting dataset is publicly accessible via a **dashboard at [openeditors.ooir.org](https://openeditors.ooir.org/)**. Former version are archived on Zenodo: [https://doi.org/10.5281/zenodo.4619374](https://doi.org/10.5281/zenodo.4619374) (versions from 2021, 2022, and 2026).

---

## Citation

> Andreas Nishikawa-Pacher, Tamara Heck, Kerstin Schoch, **Open Editors: A dataset of scholarly journals' editorial board positions**, *Research Evaluation*, Volume 32, Issue 2, April 2023, Pages 228–243. [https://doi.org/10.1093/reseval/rvac037](https://doi.org/10.1093/reseval/rvac037)

---

## Repository Structure

```
.
├── 00_create_tables.py                   # Creates all required MySQL tables
├── 01_scrape_journals.py                 # Scrapes journal catalogues per publisher
├── 02_scrape_editors.py                  # Scrapes editorial boards & tracks changes
├── 03_postprocessing_relocatecountry.py  # Extracts country from affiliation string
├── 04_postprocessing_inferror.py         # Infers ROR IDs via the ROR affiliation API
├── 05_postprocessing_countrynames.py     # Harmonises country name variants
├── fetch_browser.py                      # Playwright-based browser fetcher (human-like)
├── parsers/
│   ├── __init__.py                       # Re-exports all strategies & run_* functions
│   ├── sage.py
│   ├── frontiers.py
│   ├── elife.py
│   └── ...                               # One file per publisher
└── publisher_configs/
    ├── sage.yml
    ├── frontiers.yml
    └── ...                               # One YAML file per publisher
```

---

## Pipeline Steps

### `00_create_tables.py`

Creates the MySQL schema, including:

- `publishers` — publisher names
- `journals` — journal catalogue (title, URL, publisher key)
- `editors` — editor records with timestamps (`first_seen_at`, `last_seen_at`, `removed_at`)
- `scrape_runs` — audit log of every scrape run
- `pending_changes` — staging table for the "confirmation window"

### `01_scrape_journals.py`

Scrapes the journal catalogue for a given publisher and populates the `journals` table. Pagination, field selectors, and URL patterns are all integrated in the publisher's YAML config.

```bash
python 01_scrape_journals.py --publisher sage
```

### `02_scrape_editors.py`

The main scraping script. For each journal, it fetches the editorial board page, parses the HTML, and reconciles the result against the current database state.

```bash
python 02_scrape_editors.py --publisher frontiers
python 02_scrape_editors.py --publisher sage --limit 50 --startingjournal 10
python 02_scrape_editors.py --publisher imedpub --debug
```

**Options:**

| Flag | Description |
|---|---|
| `--publisher` | Publisher key (required); must match a file in `publisher_configs/` |
| `--limit` | Maximum number of journals to process |
| `--startingjournal` | 1-based index to start from (useful for resuming) |
| `--debug` | Enables debug logging and saves screenshots when no editors are found |

#### Change tracking & confirmation window

Editors are not immediately marked as added or removed on a single scrape. Instead, candidate changes are written to a `pending_changes` staging table and only committed to `editors` after appearing consistently in three consecutive runs for the same journal. This prevents transient network failures or incomplete infinite-scroll loads from generating spurious churn in the data.

The threshold is configurable per publisher via the YAML:

```yaml
editors:
  confirmation_streak: 3   # default; override per publisher if needed
```

### `03_postprocessing_relocatecountry.py`

For editors where `country IS NULL`, extracts the substring after the last comma in the `affiliation` field and writes it to `country`. (Targets the following publishers: APA, CUP, iMedPub, Longdom, SAGE, SCIRP, SciTechnol.)

```bash
python 03_postprocessing_relocatecountry.py
```

### `04_postprocessing_inferror.py`

Calls the [ROR affiliation API](https://api.ror.org/v2/organizations?affiliation=Uni+Wien&single_search) on raw affiliation strings to infer ROR IDs and canonical institution names.

```bash
python 04_postprocessing_inferror.py
```

### `05_postprocessing_countrynames.py`

Harmonises country name variants (e.g. `"USA"` → `"United States"`, `"UK"` → `"United Kingdom"`) to a consistent controlled vocabulary.

```bash
python 05_postprocessing_countrynames.py
```

---

## Configuration

Each publisher has a YAML file in `publisher_configs/` and a Python parser in `parsers/`. The YAML controls journal catalogue scraping, editorial board URL patterns, HTML selectors, fetch mode, rate limiting, and pagination strategy. The parser contains the publisher-specific HTML parsing logic.

**Example — `publisher_configs/sage.yml` (excerpt):**

```yaml
publisher:
  key: sage
  name: SAGE Journals

editors:
  url_template: "https://journals.sagepub.com/editorial-board/{journal_slug}"
  parsing:
    strategy: sage

fetch:
  mode: browser
  rate_limit:
    min_seconds_per_request: 5
```

**Example — `parsers/sage.py` (excerpt):**

```python
def parse_sage(container_html: str) -> list[dict]:
    # returns a list of dicts with keys:
    # full_name, role, affiliation, country, raw_text
    ...
```

To add a new publisher, create both files and register the parser in `parsers/__init__.py`.

---

## Setup

### Requirements

- Python 3.11+
- MySQL 8+
- [Playwright](https://playwright.dev/python/) (for browser-mode fetching)

### Environment variables

Create a `.env` file in the project root:

```
DB_NAME=openeditors
DB_HOST=localhost
DB_PORT=3306
DB_USER=your_db_user
DB_PASSWORD=your_db_password
ROR_CLIENT_ID=your_ror_api_client_id
```

### Database initialisation

```bash
python 00_create_tables.py
```

---

## Supported Publishers

The following publishers are currently supported. Each has a corresponding YAML config and parser:

- Allied Academies (probably predatory)
- APA
- ASCE
- CUP (Cambridge University Press)
- eLife
- Emerald
- Frontiers
- iMedPub (probably predatory)
- Inderscience
- Karger
- Longdom (probably predatory)
- OMICS (probably predatory)
- PeerJ
- Pleiades
- PLOS
- RSC
- SAGE
- SCIRP (probably predatory)
- SciTechnol (probably predatory)
- Springer Nature

---

## License

[MIT](LICENSE)

---

## Disclosure

Claude (Sonnet 4.6) helped a lot in writing the code and in drafting this README file.

## Contact

Andreas Nishikawa-Pacher · [andreas.pacher@da-vienna.at](mailto:andreas.pacher@da-vienna.at)
