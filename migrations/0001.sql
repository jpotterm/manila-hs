CREATE TABLE IF NOT EXISTS schema_migration (
    id INTEGER PRIMARY KEY,
    migration_number INTEGER NOT NULL
);

CREATE TABLE IF NOT EXISTS instance_uuid (
    id INTEGER PRIMARY KEY,
    instance_uuid CHAR(36)
);

CREATE TABLE IF NOT EXISTS category (
    id INTEGER PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    parent_id INTEGER,

    FOREIGN KEY(parent_id) REFERENCES category(id)
);

CREATE TABLE IF NOT EXISTS account (
    id INTEGER PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    balance INTEGER NOT NULL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS [transaction] (
    id INTEGER PRIMARY KEY,
    [date] DATE,
    description TEXT NOT NULL,
    original_description TEXT NOT NULL,
    amount INTEGER NOT NULL,
    category_id INTEGER,

    FOREIGN KEY(category_id) REFERENCES category(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS envelope (
    id INTEGER PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    amount INTEGER NOT NULL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS envelope_category (
    id INTEGER PRIMARY KEY,
    envelope_id INTEGER,
    category_id INTEGER,

    FOREIGN KEY(envelope_id) REFERENCES envelope(id),
    FOREIGN KEY(category_id) REFERENCES category(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS category_rule (
    id INTEGER PRIMARY KEY,
    envelope_id INTEGER,
    category_id INTEGER,
    amount INTEGER NOT NULL DEFAULT 0,

    FOREIGN KEY(envelope_id) REFERENCES envelope(id),
    FOREIGN KEY(category_id) REFERENCES category(id)
);

CREATE TABLE IF NOT EXISTS time_rule (
    id INTEGER PRIMARY KEY,
    envelope_id INTEGER,
    frequency VARCHAR(255) NOT NULL,
    amount INTEGER NOT NULL DEFAULT 0,
    [start] DATETIME NOT NULL,

    FOREIGN KEY(envelope_id) REFERENCES envelope(id)
);
