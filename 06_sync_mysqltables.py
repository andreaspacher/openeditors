import os
import pymysql
from dotenv import load_dotenv

load_dotenv()

def db_connect():
    return pymysql.connect(
        host=os.getenv("DB_HOST"),
        port=int(os.getenv("DB_PORT", 3306)),
        user=os.getenv("DB_USER"),
        password=os.getenv("DB_PASSWORD"),
        database=os.getenv("DB_NAME"),
        charset="utf8mb4",
        autocommit=True,
        cursorclass=pymysql.cursors.DictCursor,
    )

def sync():
    conn = db_connect()
    cur = conn.cursor()

    print("Syncing openeditors...")
    cur.execute("""
        INSERT INTO openeditors
        SELECT
            e.editor_id,
            e.editor,
            e.affiliation_ror,
            e.affiliation,
            e.ror,
            e.role,
            e.country,
            e.raw_text,
            LEFT(e.first_seen_at, 10) AS date,
            j.journal,
            j.url,
            p.name             AS publisher,
            p.predatory
        FROM editors e
        JOIN journals j ON j.journal_id = e.journal_id
        JOIN publishers p ON p.publisher_key = j.publisher_key
        ON DUPLICATE KEY UPDATE
            editor         = VALUES(editor),
            affiliation_ror = VALUES(affiliation_ror),
            affiliation    = VALUES(affiliation),
            ror            = VALUES(ror),
            role           = VALUES(role),
            country        = VALUES(country),
            raw_text       = VALUES(raw_text),
            date           = VALUES(date),
            journal        = VALUES(journal),
            url            = VALUES(url),
            publisher      = VALUES(publisher),
            predatory      = VALUES(predatory)
    """)
    print(f"Done. {cur.rowcount} rows inserted/updated.")

    # Also remove rows that no longer exist in editors
    cur.execute("""
        DELETE FROM openeditors
        WHERE editor_id NOT IN (SELECT editor_id FROM editors)
    """)
    print(f"Removed {cur.rowcount} stale rows.")

    cur.close()
    conn.close()

if __name__ == "__main__":
    sync()