# Integration tests for build functions
#
# These tests actually run the build process and require:
# - Network access to download R and packages
# - innoextract or 7z for Windows builds
# - Several minutes to complete
#
# Skip these tests by default. Run with:
#   Sys.setenv(PORTR_RUN_INTEGRATION_TESTS = "true")
#   devtools::test(filter = "integration")
#
# Or from command line:
#   PORTR_RUN_INTEGRATION_TESTS=true Rscript -e "devtools::test(filter='integration')"

skip_if_no_integration <- function() {
  run_integration <- Sys.getenv("PORTR_RUN_INTEGRATION_TESTS", "false")
  if (!tolower(run_integration) %in% c("true", "1", "yes")) {
    testthat::skip("Integration tests skipped. Set PORTR_RUN_INTEGRATION_TESTS=true to run.")
  }
}

skip_if_no_extraction_tool <- function() {
  deps <- check_system_deps("windows")
  if (!deps$innoextract && !deps$seven_z) {
    testthat::skip("No extraction tool available (innoextract or 7z)")
  }
}

skip_if_not_macos <- function() {
  if (Sys.info()["sysname"] != "Darwin") {
    testthat::skip("macOS build tests only run on macOS")
  }
}

# =============================================================================
# Fixture validation tests (fast, always run)
# =============================================================================

test_that("all fixtures have valid structure", {
  fixtures <- list_fixtures()
  expect_true(length(fixtures) > 0, "No fixtures found")

  for (fixture_name in fixtures) {
    expect_true(
      is_valid_fixture(fixture_name),
      info = paste("Invalid fixture:", fixture_name)
    )
  }
})

test_that("fixture helper functions work correctly", {
  fixtures <- list_fixtures()
  expect_true("minimal-app" %in% fixtures)
  expect_true("standard-app" %in% fixtures)
  expect_true("sourcing-app" %in% fixtures)

  # Test get_fixture_info

  info <- get_fixture_info("minimal-app")
  expect_equal(info$name, "minimal-app")
  expect_equal(info$entry_script, "app.R")
  expect_true(info$package_count >= 1)

  info <- get_fixture_info("standard-app")
  expect_equal(info$entry_script, "run.R")
  expect_true("models" %in% info$extra_dirs || "data" %in% info$extra_dirs)
})

test_that("fixture copy and cleanup works", {
  temp_path <- create_fixture_copy("minimal-app")
  on.exit(cleanup_fixture_copy(temp_path))

  expect_true(dir.exists(temp_path))
  expect_true(file.exists(file.path(temp_path, "app.R")))
  expect_true(file.exists(file.path(temp_path, "renv.lock")))
})

# =============================================================================
# Windows Build Integration Tests
# =============================================================================

test_that("build_windows works with minimal-app fixture", {
  skip_if_no_integration()
  skip_if_no_extraction_tool()

  fixture_path <- create_fixture_copy("minimal-app")
  on.exit(cleanup_fixture_copy(fixture_path), add = TRUE)

  dist_dir <- file.path(fixture_path, "dist")
  on.exit(unlink(dist_dir, recursive = TRUE), add = TRUE)

  result <- build_windows(
    project_path = fixture_path,
    entry_script = "app.R",
    output_dir = dist_dir,
    clean_on_error = FALSE
  )

  expect_true(dir.exists(result))
  expect_true(file.exists(file.path(result, "run_app.bat")))
  expect_true(file.exists(file.path(result, "app.R")))
  expect_true(dir.exists(file.path(result, "library")))
  # R directory may or may not exist depending on extraction success
})

test_that("build_windows works with standard-app and extra_dirs", {
  skip_if_no_integration()
  skip_if_no_extraction_tool()

  fixture_path <- create_fixture_copy("standard-app")
  on.exit(cleanup_fixture_copy(fixture_path), add = TRUE)

  info <- get_fixture_info("standard-app")
  dist_dir <- file.path(fixture_path, "dist")
  on.exit(unlink(dist_dir, recursive = TRUE), add = TRUE)

  result <- build_windows(
    project_path = fixture_path,
    entry_script = info$entry_script,
    output_dir = dist_dir,
    extra_dirs = info$extra_dirs,
    clean_on_error = FALSE
  )

  expect_true(dir.exists(result))
  expect_true(dir.exists(file.path(result, "app")))
  expect_true(dir.exists(file.path(result, "models")) ||
                dir.exists(file.path(result, "data")))
})

test_that("build_windows works with sourcing-app fixture", {
  skip_if_no_integration()
  skip_if_no_extraction_tool()

  fixture_path <- create_fixture_copy("sourcing-app")
  on.exit(cleanup_fixture_copy(fixture_path), add = TRUE)

  info <- get_fixture_info("sourcing-app")
  dist_dir <- file.path(fixture_path, "dist")
  on.exit(unlink(dist_dir, recursive = TRUE), add = TRUE)

  result <- build_windows(
    project_path = fixture_path,
    entry_script = "app.R",
    output_dir = dist_dir,
    extra_dirs = c("R", "config"),
    clean_on_error = FALSE
  )

  expect_true(dir.exists(result))
  expect_true(dir.exists(file.path(result, "R")))
  expect_true(dir.exists(file.path(result, "config")))
  expect_true(file.exists(file.path(result, "R", "utils.R")))
})

test_that("build_windows works with nested-entry-app fixture", {
  skip_if_no_integration()
  skip_if_no_extraction_tool()

  fixture_path <- create_fixture_copy("nested-entry-app")
  on.exit(cleanup_fixture_copy(fixture_path), add = TRUE)

  dist_dir <- file.path(fixture_path, "dist")
  on.exit(unlink(dist_dir, recursive = TRUE), add = TRUE)

  result <- build_windows(
    project_path = fixture_path,
    entry_script = "scripts/run_app.R",
    output_dir = dist_dir,
    extra_dirs = c("R", "scripts"),
    clean_on_error = FALSE
  )

  expect_true(dir.exists(result))
  expect_true(file.exists(file.path(result, "scripts", "run_app.R")))
})

test_that("build_windows works with script-only fixture (non-Shiny)", {
  skip_if_no_integration()
  skip_if_no_extraction_tool()

  fixture_path <- create_fixture_copy("script-only")
  on.exit(cleanup_fixture_copy(fixture_path), add = TRUE)

  dist_dir <- file.path(fixture_path, "dist")
  on.exit(unlink(dist_dir, recursive = TRUE), add = TRUE)

  result <- build_windows(
    project_path = fixture_path,
    entry_script = "main.R",
    output_dir = dist_dir,
    extra_dirs = c("input", "output"),
    shiny_command = "source('main.R')",  # Not a Shiny app
    clean_on_error = FALSE
  )

  expect_true(dir.exists(result))
  expect_true(file.exists(file.path(result, "main.R")))
  expect_true(dir.exists(file.path(result, "input")))
})

test_that("build_windows creates zip archive when requested", {
  skip_if_no_integration()
  skip_if_no_extraction_tool()

  fixture_path <- create_fixture_copy("minimal-app")
  on.exit(cleanup_fixture_copy(fixture_path), add = TRUE)

  dist_dir <- file.path(fixture_path, "dist")
  zip_path <- file.path(fixture_path, "test_archive.zip")
  on.exit({
    unlink(dist_dir, recursive = TRUE)
    unlink(zip_path)
  }, add = TRUE)

  result <- build_windows(
    project_path = fixture_path,
    entry_script = "app.R",
    output_dir = dist_dir,
    create_zip = TRUE,
    zip_name = "test_archive.zip",
    clean_on_error = FALSE
  )

  expect_true(file.exists(zip_path))
})

# =============================================================================
# macOS Build Integration Tests
# =============================================================================

test_that("build_mac works with minimal-app fixture (make_app=FALSE legacy mode)", {
  skip_if_no_integration()
  skip_if_not_macos()

  fixture_path <- create_fixture_copy("minimal-app")
  on.exit(cleanup_fixture_copy(fixture_path), add = TRUE)

  dist_dir <- file.path(fixture_path, "dist")
  on.exit(unlink(dist_dir, recursive = TRUE), add = TRUE)

  result <- build_mac(
    project_path = fixture_path,
    entry_script = "app.R",
    output_dir = dist_dir,
    make_app = FALSE,  # Test legacy folder mode
    clean_on_error = FALSE
  )

  expect_true(dir.exists(result))
  expect_true(file.exists(file.path(result, "run_app.sh")))
  expect_true(file.exists(file.path(result, "Run Application.command")))
  expect_true(file.exists(file.path(result, "app.R")))
  expect_true(dir.exists(file.path(result, "library")))
})

test_that("build_mac works with standard-app and extra_dirs (make_app=FALSE legacy mode)", {
  skip_if_no_integration()
  skip_if_not_macos()

  fixture_path <- create_fixture_copy("standard-app")
  on.exit(cleanup_fixture_copy(fixture_path), add = TRUE)

  info <- get_fixture_info("standard-app")
  dist_dir <- file.path(fixture_path, "dist")
  on.exit(unlink(dist_dir, recursive = TRUE), add = TRUE)

  result <- build_mac(
    project_path = fixture_path,
    entry_script = info$entry_script,
    output_dir = dist_dir,
    extra_dirs = info$extra_dirs,
    make_app = FALSE,  # Test legacy folder mode
    clean_on_error = FALSE
  )

  expect_true(dir.exists(result))
  expect_true(dir.exists(file.path(result, "app")))
  
  # Check that extra_dirs were copied
  for (extra_dir in info$extra_dirs) {
    expect_true(
      dir.exists(file.path(result, extra_dir)),
      info = paste("Extra directory not copied:", extra_dir)
    )
  }
})

test_that("build_mac works with sourcing-app fixture (make_app=FALSE legacy mode)", {
  skip_if_no_integration()
  skip_if_not_macos()

  fixture_path <- create_fixture_copy("sourcing-app")
  on.exit(cleanup_fixture_copy(fixture_path), add = TRUE)

  dist_dir <- file.path(fixture_path, "dist")
  on.exit(unlink(dist_dir, recursive = TRUE), add = TRUE)

  result <- build_mac(
    project_path = fixture_path,
    entry_script = "app.R",
    output_dir = dist_dir,
    extra_dirs = c("R", "config"),
    make_app = FALSE,  # Test legacy folder mode
    clean_on_error = FALSE
  )

  expect_true(dir.exists(result))
  expect_true(dir.exists(file.path(result, "R")))
  expect_true(file.exists(file.path(result, "config", "settings.yaml")))
})

test_that("build_mac creates zip archive when requested", {
  skip_if_no_integration()
  skip_if_not_macos()

  fixture_path <- create_fixture_copy("minimal-app")
  on.exit(cleanup_fixture_copy(fixture_path), add = TRUE)

  dist_dir <- file.path(fixture_path, "dist")
  zip_path <- file.path(fixture_path, "test_mac_archive.zip")
  on.exit({
    unlink(dist_dir, recursive = TRUE)
    unlink(zip_path)
  }, add = TRUE)

  result <- build_mac(
    project_path = fixture_path,
    entry_script = "app.R",
    output_dir = dist_dir,
    create_zip = TRUE,
    zip_name = "test_mac_archive.zip",
    clean_on_error = FALSE
  )

  expect_true(file.exists(zip_path))
})

# =============================================================================
# macOS App Bundle (make_app = TRUE) Integration Tests
# =============================================================================

test_that("build_mac with make_app creates app bundle for minimal-app", {
  skip_if_no_integration()
  skip_if_not_macos()

  fixture_path <- create_fixture_copy("minimal-app")
  on.exit(cleanup_fixture_copy(fixture_path), add = TRUE)

  dist_dir <- file.path(fixture_path, "dist")
  on.exit(unlink(dist_dir, recursive = TRUE), add = TRUE)

  result <- build_mac(
    project_path = fixture_path,
    entry_script = "app.R",
    output_dir = dist_dir,
    make_app = TRUE,
    app_name = "MinimalApp",
    clean_on_error = FALSE
  )

  expect_true(dir.exists(result))
  expect_true(grepl("\\.app$", result))
  
  # Check app bundle structure
  expect_true(dir.exists(file.path(result, "Contents")))
  expect_true(dir.exists(file.path(result, "Contents", "MacOS")))
  expect_true(dir.exists(file.path(result, "Contents", "Resources")))
  expect_true(dir.exists(file.path(result, "Contents", "Frameworks")))
  
  # Check executable and launcher
  expect_true(file.exists(file.path(result, "Contents", "MacOS", "MinimalApp")))
  expect_true(file.exists(file.path(result, "Contents", "Resources", "launcher.sh")))
  
  # Check Info.plist
  expect_true(file.exists(file.path(result, "Contents", "Info.plist")))
  
  # Check app files were copied
  app_dir <- file.path(result, "Contents", "Resources", "app")
  expect_true(dir.exists(app_dir))
  expect_true(file.exists(file.path(app_dir, "app.R")))
  
  # Check library directory
  expect_true(dir.exists(file.path(result, "Contents", "Resources", "library")))
})

test_that("build_mac with make_app copies extra_dirs correctly", {
  skip_if_no_integration()
  skip_if_not_macos()

  fixture_path <- create_fixture_copy("standard-app")
  on.exit(cleanup_fixture_copy(fixture_path), add = TRUE)

  info <- get_fixture_info("standard-app")
  dist_dir <- file.path(fixture_path, "dist")
  on.exit(unlink(dist_dir, recursive = TRUE), add = TRUE)

  result <- build_mac(
    project_path = fixture_path,
    entry_script = info$entry_script,
    output_dir = dist_dir,
    extra_dirs = info$extra_dirs,
    make_app = TRUE,
    app_name = "StandardApp",
    clean_on_error = FALSE
  )

  expect_true(dir.exists(result))
  
  # Check app bundle structure
  app_dir <- file.path(result, "Contents", "Resources", "app")
  expect_true(dir.exists(app_dir))
  
  # Check entry script was copied
  expect_true(file.exists(file.path(app_dir, info$entry_script)))
  
  # Check that extra_dirs were copied into the app directory
  for (extra_dir in info$extra_dirs) {
    expect_true(
      dir.exists(file.path(app_dir, extra_dir)),
      info = paste("Extra directory not copied to app bundle:", extra_dir)
    )
  }
  
  # Check specific files from extra_dirs
  expect_true(file.exists(file.path(app_dir, "data", "sample.csv")))
  expect_true(file.exists(file.path(app_dir, "models", "model_config.R")))
})

test_that("build_mac with make_app handles sourcing-app fixture", {
  skip_if_no_integration()
  skip_if_not_macos()

  fixture_path <- create_fixture_copy("sourcing-app")
  on.exit(cleanup_fixture_copy(fixture_path), add = TRUE)

  dist_dir <- file.path(fixture_path, "dist")
  on.exit(unlink(dist_dir, recursive = TRUE), add = TRUE)

  result <- build_mac(
    project_path = fixture_path,
    entry_script = "app.R",
    output_dir = dist_dir,
    extra_dirs = c("R", "config"),
    make_app = TRUE,
    app_name = "SourcingApp",
    clean_on_error = FALSE
  )

  expect_true(dir.exists(result))
  
  app_dir <- file.path(result, "Contents", "Resources", "app")
  expect_true(dir.exists(file.path(app_dir, "R")))
  expect_true(dir.exists(file.path(app_dir, "config")))
  expect_true(file.exists(file.path(app_dir, "R", "utils.R")))
})

# =============================================================================
# Heavy Dependencies Test (very slow)
# =============================================================================

test_that("build_windows handles heavy-deps-app fixture",
  {
  skip_if_no_integration()
  skip_if_no_extraction_tool()

  fixture_path <- create_fixture_copy("heavy-deps-app")
  on.exit(cleanup_fixture_copy(fixture_path), add = TRUE)

  dist_dir <- file.path(fixture_path, "dist")
  on.exit(unlink(dist_dir, recursive = TRUE), add = TRUE)

  result <- build_windows(
    project_path = fixture_path,
    entry_script = "app.R",
    output_dir = dist_dir,
    clean_on_error = FALSE
  )

  expect_true(dir.exists(result))

  # Check that multiple packages were downloaded
  lib_dir <- file.path(result, "library")
  packages <- list.dirs(lib_dir, recursive = FALSE, full.names = FALSE)
  expect_true(length(packages) >= 5)
})
