# calendar-seed

Generate seed data for the calendar database by fetching historical anime data from Mikan and BGM.tv.

## Usage

```bash
# Fetch all seasons (2013 to current)
cargo run -p calendar-seed

# Fetch only recent N seasons
cargo run -p calendar-seed -- --recent 2
```

## Workflow

```
1. Mikan: Get seasonal anime list
   └─> [mikan_id, name, air_week]

2. Mikan: Lookup BGM.tv ID for each anime
   └─> [mikan_id, bgmtv_id, air_week]

3. BGM.tv (via metadata): Fetch subject details
   └─> [title, year, season, platform, episodes, poster, air_date]

4. Save to assets/seed/{year}-{season}.json
```

## Output

Each season is saved as a separate JSON file:

```
assets/seed/
├── 2025-winter.json
├── 2024-fall.json
├── 2024-summer.json
└── ...
```

## Dependencies

- `mikan` - Seasonal anime list and BGM.tv ID lookup
- `metadata` - BGM.tv subject details via `BgmtvProvider`
