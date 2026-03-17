from dotenv import load_dotenv
import os
import mysql.connector

load_dotenv()

# ---- configure MySQL connection in `.env` ----
config = {
    "host":     os.getenv("DB_HOST"),
    "port":     int(os.getenv("DB_PORT", 3306)),
    "user":     os.getenv("DB_USER"),
    "password": os.getenv("DB_PASSWORD"),
    "database": os.getenv("DB_NAME"),
}

DDL = """
SET FOREIGN_KEY_CHECKS=0;

CREATE TABLE IF NOT EXISTS `scrape_runs` (
  `run_id` bigint(20) NOT NULL AUTO_INCREMENT,
  `stage` enum('journal_discovery','editor_extraction') NOT NULL,
  `publisher_key` varchar(64) DEFAULT NULL,
  `started_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `finished_at` datetime DEFAULT NULL,
  `status` enum('running','success','failed') NOT NULL DEFAULT 'running',
  `config_hash` char(64) DEFAULT NULL,
  `meta` json DEFAULT NULL,
  `records_updated` int(11) DEFAULT NULL,
  `records_removed` int(11) DEFAULT NULL,
  `records_inserted` int(11) DEFAULT '0',
  PRIMARY KEY (`run_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS `publishers` (
  `publisher_id` int(11) NOT NULL AUTO_INCREMENT,
  `publisher_key` varchar(64) NOT NULL,
  `name` varchar(255) NOT NULL,
  `predatory` int(11) DEFAULT NULL,
  `created_at` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`publisher_id`),
  UNIQUE KEY `publisher_key` (`publisher_key`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS `journals` (
  `journal_id` bigint(20) NOT NULL AUTO_INCREMENT,
  `publisher_key` varchar(64) NOT NULL,
  `journal` varchar(512) NOT NULL,
  `url` varchar(1024) NOT NULL,
  `issn_print` varchar(16) DEFAULT NULL,
  `issn_e` varchar(16) DEFAULT NULL,
  `issn_all` varchar(128) DEFAULT NULL,
  `discovered_run_id` bigint(20) DEFAULT NULL,
  `discovered_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`journal_id`),
  UNIQUE KEY `uq_journal_url` (`url`(255)),
  KEY `idx_pub` (`publisher_key`),
  KEY `idx_run` (`discovered_run_id`),
  CONSTRAINT `fk_journals_run` FOREIGN KEY (`discovered_run_id`) REFERENCES `scrape_runs` (`run_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS `editors` (
  `editor_id` bigint(20) NOT NULL AUTO_INCREMENT,
  `journal_id` bigint(20) NOT NULL,
  `role` varchar(255) DEFAULT NULL,
  `full_name` varchar(512) DEFAULT NULL,
  `affiliation_ror` varchar(255) DEFAULT NULL,
  `ror` varchar(64) DEFAULT NULL,
  `country` varchar(100) DEFAULT NULL,
  `scraped_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `raw_text` text,
  `affiliation` varchar(512) DEFAULT NULL,
  `country_raw` varchar(128) DEFAULT NULL,
  `email` varchar(320) DEFAULT NULL,
  `orcid` varchar(32) DEFAULT NULL,
  `first_seen_at` datetime DEFAULT NULL,
  `last_seen_at` datetime DEFAULT NULL,
  `removed_at` datetime DEFAULT NULL,
  `extracted_run_id` bigint(20) DEFAULT NULL,
  `first_run_id` bigint(20) DEFAULT NULL,
  `last_run_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`editor_id`),
  UNIQUE KEY `uq_journal_name` (`journal_id`,`full_name`(255)),
  KEY `idx_journal` (`journal_id`),
  KEY `idx_orcid` (`orcid`),
  KEY `idx_email` (`email`),
  KEY `fk_editors_run` (`extracted_run_id`),
  KEY `idx_removed` (`removed_at`),
  KEY `idx_last_seen` (`last_seen_at`),
  KEY `fk_editors_first_run` (`first_run_id`),
  KEY `fk_editors_last_run` (`last_run_id`),
  CONSTRAINT `fk_editors_first_run` FOREIGN KEY (`first_run_id`) REFERENCES `scrape_runs` (`run_id`),
  CONSTRAINT `fk_editors_journal` FOREIGN KEY (`journal_id`) REFERENCES `journals` (`journal_id`) ON DELETE CASCADE,
  CONSTRAINT `fk_editors_last_run` FOREIGN KEY (`last_run_id`) REFERENCES `scrape_runs` (`run_id`),
  CONSTRAINT `fk_editors_run` FOREIGN KEY (`extracted_run_id`) REFERENCES `scrape_runs` (`run_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE pending_changes (
    id            INT AUTO_INCREMENT PRIMARY KEY,
    journal_id    INT NOT NULL,
    full_name     VARCHAR(512) NOT NULL,
    change_type   ENUM('add', 'remove') NOT NULL,
    first_run_id  INT NOT NULL,
    last_run_id   INT NOT NULL,
    streak        TINYINT NOT NULL DEFAULT 1,
    created_at    DATETIME NOT NULL,
    updated_at    DATETIME NOT NULL,

    UNIQUE KEY uq_pending (journal_id, full_name, change_type)
);

ALTER TABLE editors
  ADD COLUMN ror_lookup_status VARCHAR(20) DEFAULT NULL,
  ADD COLUMN ror_lookup_attempted_at DATETIME NULL,
  ADD COLUMN ror_lookup_score DECIMAL(5,4) NULL,
  ADD COLUMN ror_lookup_note VARCHAR(255) NULL;

SET FOREIGN_KEY_CHECKS=1;
"""

# --- Seed data ---

# Paste the output of the mysqldump/SELECT query here as tuples: (publisher_key, name, predatory)
PUBLISHERS = [
    ('alliedacademies', 'Allied Academies', 1)
    ('apa', 'APA', 0)
    ('asce', 'ASCE', 0)
    ('cup', 'Cambridge University Press', 0)
    ('elife', 'eLife', 0)
    ('emerald', 'Emerald', 0)
    ('frontiers', 'Frontiers', 0)
    ('imedpub', 'iMedPub', 1)
    ('inderscience', 'Inderscience', 0)
    ('karger', 'Karger', 0)
    ('longdom', 'Longdom', 1)
    ('mdpi', 'MDPI', 0)
    ('elsevier', 'Elsevier', 0)
    ('brill', 'Brill', 0)
    ('omics', 'OMICS', 1)
    ('peerj', 'PeerJ', 0)
    ('pleiades', 'Pleiades', 0)
    ('plos', 'PLOS', 0)
    ('rsc', 'Royal Society of Chemistry', 0)
    ('sage', 'SAGE', 0)
    ('scirp', 'SCIRP', 1)
    ('scitechnol', 'SciTechnol', 1)
    ('springernature', 'Springer Nature', 0)
]

JOURNALS_SEED = [
    {
        "publisher_key": "elife",
        "journal":       "eLife",
        "url":           "https://elifesciences.org/",
        "issn_e":        "2050-084X",
    },
]


def run():
    conn = mysql.connector.connect(**config)
    cursor = conn.cursor()

    # Create tables
    for statement in DDL.strip().split(";"):
        stmt = statement.strip()
        if stmt:
            cursor.execute(stmt)
    print("Tables created.")

    # Seed publishers
    if PUBLISHERS:
        cursor.executemany(
            "INSERT IGNORE INTO publishers (publisher_key, name, predatory) VALUES (%s, %s, %s)",
            PUBLISHERS,
        )
        print(f"Inserted {cursor.rowcount} publisher(s).")

    # Seed journals
    for j in JOURNALS_SEED:
        cursor.execute(
            """INSERT IGNORE INTO journals (publisher_key, journal, url, issn_e)
               VALUES (%s, %s, %s, %s)""",
            (j["publisher_key"], j["journal"], j["url"], j.get("issn_e")),
        )
    print(f"Inserted {len(JOURNALS_SEED)} journal seed(s).")

    conn.commit()
    cursor.close()
    conn.close()
    print("Done.")


if __name__ == "__main__":
    run()