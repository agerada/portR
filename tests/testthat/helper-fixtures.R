# Helper functions for fixture-based tests

#' Get path to a test fixture
#'
#' @param fixture_name Name of the fixture folder
#' @return Absolute path to the fixture
get_fixture_path <- function(fixture_name) {
  # testthat::test_path() returns path relative to tests/testthat
  testthat::test_path("fixtures", fixture_name)
}

#' List all available fixtures
#'
#' @return Character vector of fixture names
list_fixtures <- function() {
  fixtures_dir <- testthat::test_path("fixtures")
  if (!dir.exists(fixtures_dir)) {
    return(character(0))
  }
  list.dirs(fixtures_dir, recursive = FALSE, full.names = FALSE)
}

#' Check if a fixture is valid (has renv.lock and renv folder)
#'
#' @param fixture_name Name of the fixture
#' @return TRUE if valid, FALSE otherwise
is_valid_fixture <- function(fixture_name) {
  fixture_path <- get_fixture_path(fixture_name)
  file.exists(file.path(fixture_path, "renv.lock")) &&
    dir.exists(file.path(fixture_path, "renv"))
}

#' Get fixture metadata
#'
#' @param fixture_name Name of the fixture
#' @return List with fixture information
get_fixture_info <- function(fixture_name) {
  fixture_path <- get_fixture_path(fixture_name)

  # Find entry script
  r_files <- list.files(fixture_path, pattern = "\\.R$", recursive = FALSE)
  entry_script <- if ("app.R" %in% r_files) {
    "app.R"
  } else if ("run.R" %in% r_files) {
    "run.R"
  } else if ("main.R" %in% r_files) {
    "main.R"
  } else if (length(r_files) > 0) {
    r_files[1]
  } else {
    # Check subdirectories
    sub_r_files <- list.files(fixture_path, pattern = "\\.R$", recursive = TRUE)
    if (length(sub_r_files) > 0) sub_r_files[1] else NA
  }

  # Read lockfile for package count
  lock_path <- file.path(fixture_path, "renv.lock")
  pkg_count <- 0
  if (file.exists(lock_path)) {
    lock <- jsonlite::read_json(lock_path)
    pkg_count <- length(lock$Packages)
  }

  # Check for extra directories
  all_dirs <- list.dirs(fixture_path, recursive = FALSE, full.names = FALSE)
  extra_dirs <- setdiff(all_dirs, c("renv", "app"))

  list(
    name = fixture_name,
    path = fixture_path,
    entry_script = entry_script,
    package_count = pkg_count,
    extra_dirs = extra_dirs,
    has_app_folder = dir.exists(file.path(fixture_path, "app")),
    has_r_folder = dir.exists(file.path(fixture_path, "R"))
  )
}

#' Create a temporary copy of a fixture for testing
#'
#' @param fixture_name Name of the fixture
#' @return Path to the temporary copy
create_fixture_copy <- function(fixture_name) {
  fixture_path <- get_fixture_path(fixture_name)
  temp_dir <- tempfile(pattern = paste0("portR_test_", fixture_name, "_"))
  dir.create(temp_dir)

  # Copy all files (including hidden files like .gitkeep)
  file.copy(
    list.files(fixture_path, full.names = TRUE, all.files = TRUE,
               no.. = TRUE),
    temp_dir,
    recursive = TRUE
  )

 # Ensure renv directory exists (may not copy if empty or only has hidden files)
  renv_dir <- file.path(temp_dir, "renv")
  if (!dir.exists(renv_dir)) {
    dir.create(renv_dir)
  }

  temp_dir
}

#' Clean up a temporary fixture copy
#'
#' @param temp_path Path to temporary fixture
cleanup_fixture_copy <- function(temp_path) {
  if (dir.exists(temp_path) && grepl("portR_test_", temp_path)) {
    unlink(temp_path, recursive = TRUE)
  }
}
