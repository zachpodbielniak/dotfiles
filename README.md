# Transcription Database

A PostgreSQL-based storage system for audio transcriptions, integrated with the dotfiles ecosystem.

## Overview

This implementation provides database-backed storage for audio transcriptions, enabling:
- Automatic storage of transcriptions in PostgreSQL
- Full-text search capabilities across all transcriptions
- Metadata tagging and categorization
- Integration with existing search infrastructure
- Efficient handling of large transcription texts through chunking

## Quick Start

1. **Initialize the database:**
   ```bash
   local_postgres setup
   ```

2. **Transcribe audio with database storage:**
   ```bash
   transcribe_audio --type meeting --tags "team,important" audio.mp3
   ```

3. **Search transcriptions:**
   ```bash
   semantic_search --transcriptions-only "action items"
   ```

## Components

### Scripts

#### `local_postgres`
Universal PostgreSQL management script for local dotfiles. Handles database setup, maintenance, and administration.

**Usage:**
```bash
# Initial setup
local_postgres setup

# Perform maintenance
local_postgres maintain

# Show statistics
local_postgres stats

# Export transcriptions
local_postgres export backup.json

# Archive old transcriptions
local_postgres archive 180  # Archive older than 180 days
```

#### `transcribe_audio`
Enhanced audio transcription script with PostgreSQL storage support.

**New Options:**
- `--no-store-db`: Skip database storage (legacy mode)
- `--db-only`: Store only in database, no file output
- `--update-db`: Update existing transcription metadata
- `--type TYPE`: Categorize transcription (e.g., meeting, interview)
- `--tags TAGS`: Add comma-separated tags

**Examples:**
```bash
# Basic transcription with auto DB storage
transcribe_audio audio.mp3

# Categorized transcription
transcribe_audio --type interview --tags "hiring,candidate-x" interview.mp3

# Update existing transcription
transcribe_audio --update-db --tags "reviewed,action-required" audio.mp3

# Database-only storage
transcribe_audio --db-only --type meeting standup.mp3
```

#### `semantic_search`
Enhanced search script with transcription database support.

**New Options:**
- `--include-transcriptions`: Include DB transcriptions in search
- `--transcriptions-only`: Search only transcriptions
- `--type TYPE`: Filter by transcription type
- `--tags TAGS`: Filter by tags

**Examples:**
```bash
# Search only transcriptions
semantic_search --transcriptions-only "budget discussion"

# Search with type filter
semantic_search --transcriptions-only --type meeting "action items"

# Search with tag filter
semantic_search --tags "client-x,important" "project deadline"

# Combined search (files + transcriptions)
semantic_search --include-transcriptions "neural networks"
```

## Database Schema

### Tables

1. **transcriptions**
   - Stores metadata about each transcription
   - Includes file info, timestamps, and JSONB metadata
   - Unique constraint on file hash prevents duplicates

2. **transcription_chunks**
   - Stores actual transcription text in chunks
   - Enables efficient handling of large transcriptions
   - Full-text search index on content

### Indexes
- GIN index on content for full-text search
- JSONB indexes for metadata queries
- Hash index for duplicate detection

## Configuration

### Configuration File
Create `~/.config/transcription_db/config.yaml`:

```yaml
database:
  host: localhost
  port: 5432
  name: transcriptions
  user: myuser
  password: mypass

transcription:
  chunk_size: 1048576  # 1MB
  default_model: ggml-base.en.bin

search:
  default_limit: 10
  fts_language: english
```

### Environment Variables
Override any setting with environment variables:
- `TRANSCRIPTION_DB_HOST`
- `TRANSCRIPTION_DB_PORT`
- `TRANSCRIPTION_DB_NAME`
- `TRANSCRIPTION_DB_USER`
- `TRANSCRIPTION_DB_PASSWORD`

## Features

### Automatic Chunking
Large transcriptions are automatically split into 1MB chunks for efficient storage and retrieval.

### Duplicate Detection
Files are hashed before processing. Duplicate transcriptions are automatically detected and skipped.

### Metadata Support
Store rich metadata with each transcription:
- Type categorization (meeting, interview, lecture, etc.)
- Tag arrays for flexible organization
- Source script tracking
- Custom JSON fields

### Full-Text Search
PostgreSQL's powerful full-text search with:
- Ranked results
- Phrase search
- Boolean operators
- Highlighted snippets
- Metadata filtering

## Testing

Run the test suite:
```bash
./tests/scripts/test-transcription-db.sh
```

The test suite covers:
- Database setup and schema creation
- Transcription storage and retrieval
- Search functionality
- Maintenance operations
- Error handling

## Maintenance

### Regular Maintenance
Run periodic maintenance to optimize performance:
```bash
# Vacuum, analyze, and reindex
local_postgres maintain
```

### Archiving
Archive old transcriptions to keep the database lean:
```bash
# Archive transcriptions older than 1 year
local_postgres archive 365
```

### Backup
Export transcriptions for backup:
```bash
local_postgres export transcriptions_backup.json
```

## Integration with sbi

The Second Brain Ingest (sbi) script automatically uses transcription features:
- Audio files are transcribed and stored in the database
- Metadata is preserved (PARA categorization as type)
- Tags can be added during ingestion

## Performance Considerations

- Transcriptions are indexed asynchronously
- Chunk size is configurable (default: 1MB)
- Full-text search uses GIN indexes for speed
- Connection pooling through psycopg

## Troubleshooting

### Database Connection Issues
1. Check PostgreSQL is running
2. Verify connection parameters in config
3. Test with: `psql -U $USER -d transcriptions`

### Search Not Finding Results
1. Ensure transcriptions are indexed: `local_postgres stats`
2. Check search syntax and filters
3. Verify full-text search configuration language

### Performance Issues
1. Run maintenance: `local_postgres maintain`
2. Check chunk sizes aren't too small
3. Monitor index bloat with stats command

## Future Enhancements

- Vector embeddings for semantic search
- Speaker diarization storage
- Real-time transcription updates
- Multi-language support
- Audio file linking and playback