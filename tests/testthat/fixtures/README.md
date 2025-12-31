# Test Fixtures

This directory contains test fixtures for the portR package integration tests.
Each fixture represents a different type of R project to test build functionality.

## Fixtures

### minimal-app
**Complexity:** Simple  
**Entry script:** `app.R`  
**Packages:** shiny only

The simplest possible Shiny app - a single `app.R` file with minimal dependencies.
Use this for quick smoke tests.

### standard-app
**Complexity:** Medium  
**Entry script:** `run.R`  
**Packages:** shiny, ggplot2, dplyr  
**Extra dirs:** models/, data/

A standard Shiny app structure with:
- Separate `ui.R` and `server.R` in `app/` folder
- Entry script that calls `shiny::runApp()`
- Additional model and data directories

### sourcing-app
**Complexity:** Medium-High  
**Entry script:** `app.R`  
**Packages:** shiny, yaml  
**Extra dirs:** R/, config/

Tests the edge case where the entry script uses `source()` to load other R files:
- `R/utils.R` - utility functions
- `R/data_processing.R` - data transformation functions
- `R/plot_helpers.R` - visualization functions
- `config/settings.yaml` - external configuration

### nested-entry-app
**Complexity:** Medium  
**Entry script:** `scripts/run_app.R`  
**Packages:** shiny  
**Extra dirs:** R/, scripts/

Tests when the entry script is not in the project root:
- Entry script in `scripts/` subdirectory
- Sources files from parent directories with relative paths

### local-package-app
**Complexity:** Medium  
**Entry script:** `app.R`  
**Packages:** shiny, mylocalpackage (Local)

Tests handling of local (non-CRAN) packages in `renv.lock`.
The local package won't be found during tests but validates the code path.

### heavy-deps-app
**Complexity:** High  
**Entry script:** `app.R`  
**Packages:** 8 packages including plotly, DT, shinydashboard

Tests with many CRAN dependencies. Use sparingly as downloads take time.
Includes interactive dashboard with multiple tab pages.

### script-only
**Complexity:** Simple  
**Entry script:** `main.R`  
**Packages:** dplyr, readr  
**Extra dirs:** input/, output/

A non-Shiny R script for batch processing. Tests that portR works
beyond Shiny apps for general R script distribution.

## Running Integration Tests

Integration tests are skipped by default because they:
- Require network access
- Download R installers (100+ MB)
- Download packages from CRAN
- Take several minutes to complete

### Enable Integration Tests

```bash
# Single run
PORTR_RUN_INTEGRATION_TESTS=true Rscript -e "devtools::test(filter='integration')"

# In R session
Sys.setenv(PORTR_RUN_INTEGRATION_TESTS = "true")
devtools::test(filter = "integration")
```

### Requirements for Windows Build Tests
- `innoextract` or `7z` must be installed
- Install on macOS: `brew install innoextract`

### Requirements for macOS Build Tests
- Must be running on macOS
- `pkgutil` and `hdiutil` (standard on macOS)

## Adding New Fixtures

1. Create a new directory under `fixtures/`
2. Add the R script(s) for your app
3. Create a minimal `renv.lock` with required packages
4. Create an empty `renv/` directory (required for validation)
5. Update this README with fixture documentation
6. Add corresponding tests in `test-integration.R`

### Fixture Requirements

Each fixture must have:
- `renv.lock` - Valid JSON with R version and Packages
- `renv/` - Directory (can be empty for testing)
- At least one `.R` file as entry point

### Helper Functions

The `helper-fixtures.R` file provides utilities:
- `get_fixture_path(name)` - Get absolute path to fixture
- `list_fixtures()` - List all available fixtures
- `is_valid_fixture(name)` - Check fixture has required files
- `get_fixture_info(name)` - Get metadata about fixture
- `create_fixture_copy(name)` - Create temp copy for testing
- `cleanup_fixture_copy(path)` - Clean up temp copy
