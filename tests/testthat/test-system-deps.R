test_that("check_system_deps returns named list for windows", {
  deps <- check_system_deps("windows")

  expect_type(deps, "list")
  expect_true("curl" %in% names(deps))
  expect_true("innoextract" %in% names(deps))
  expect_true("seven_z" %in% names(deps))
  expect_true("zip" %in% names(deps))

  # All values should be logical
  for (name in names(deps)) {
    expect_type(deps[[name]], "logical")
  }
})

test_that("check_system_deps returns named list for mac", {
  deps <- check_system_deps("mac")

  expect_type(deps, "list")
  expect_true("curl" %in% names(deps))
  expect_true("hdiutil" %in% names(deps))
  expect_true("zip" %in% names(deps))
})

test_that("setup_dist_directory creates proper structure", {
  temp_dir <- file.path(tempdir(), "test_dist_setup")
  on.exit(unlink(temp_dir, recursive = TRUE))

  setup_dist_directory(temp_dir, "windows")

  expect_true(dir.exists(temp_dir))
  expect_true(dir.exists(file.path(temp_dir, "library")))
  expect_true(dir.exists(file.path(temp_dir, "app")))
})

test_that("setup_dist_directory removes existing directory", {
  temp_dir <- file.path(tempdir(), "test_dist_overwrite")
  dir.create(temp_dir, recursive = TRUE)
  # Create a file that should be removed
  writeLines("test", file.path(temp_dir, "old_file.txt"))
  on.exit(unlink(temp_dir, recursive = TRUE))

  setup_dist_directory(temp_dir, "windows")

  expect_false(file.exists(file.path(temp_dir, "old_file.txt")))
  expect_true(dir.exists(file.path(temp_dir, "library")))
})

test_that("copy_app_files copies entry script", {
  # Setup source project
  src_dir <- file.path(tempdir(), "test_src_project")
  dist_dir <- file.path(tempdir(), "test_dist_copy")
  dir.create(src_dir, recursive = TRUE)
  dir.create(dist_dir, recursive = TRUE)
  on.exit({
    unlink(src_dir, recursive = TRUE)
    unlink(dist_dir, recursive = TRUE)
  })

  # Create entry script
  writeLines("# App entry", file.path(src_dir, "app.R"))

  copy_app_files(src_dir, dist_dir, "app.R")

  expect_true(file.exists(file.path(dist_dir, "app.R")))
})

test_that("copy_app_files copies app directory if exists", {
  src_dir <- file.path(tempdir(), "test_src_app")
  dist_dir <- file.path(tempdir(), "test_dist_app")
  dir.create(file.path(src_dir, "app"), recursive = TRUE)
  dir.create(dist_dir, recursive = TRUE)
  on.exit({
    unlink(src_dir, recursive = TRUE)
    unlink(dist_dir, recursive = TRUE)
  })

  writeLines("# Entry", file.path(src_dir, "app.R"))
  writeLines("# UI", file.path(src_dir, "app", "ui.R"))

  copy_app_files(src_dir, dist_dir, "app.R")

  expect_true(dir.exists(file.path(dist_dir, "app")))
  expect_true(file.exists(file.path(dist_dir, "app", "ui.R")))
})

test_that("copy_app_files copies extra directories", {
  src_dir <- file.path(tempdir(), "test_src_extra")
  dist_dir <- file.path(tempdir(), "test_dist_extra")
  dir.create(file.path(src_dir, "models"), recursive = TRUE)
  dir.create(file.path(src_dir, "data"), recursive = TRUE)
  dir.create(dist_dir, recursive = TRUE)
  on.exit({
    unlink(src_dir, recursive = TRUE)
    unlink(dist_dir, recursive = TRUE)
  })

  writeLines("# Entry", file.path(src_dir, "app.R"))
  writeLines("model", file.path(src_dir, "models", "model.rds"))

  copy_app_files(src_dir, dist_dir, "app.R", extra_dirs = c("models", "data"))

  expect_true(dir.exists(file.path(dist_dir, "models")))
  expect_true(file.exists(file.path(dist_dir, "models", "model.rds")))
  expect_true(dir.exists(file.path(dist_dir, "data")))
})

test_that("copy_app_files errors on missing entry script", {
  src_dir <- file.path(tempdir(), "test_src_missing")
  dist_dir <- file.path(tempdir(), "test_dist_missing")
  dir.create(src_dir, recursive = TRUE)
  dir.create(dist_dir, recursive = TRUE)
  on.exit({
    unlink(src_dir, recursive = TRUE)
    unlink(dist_dir, recursive = TRUE)
  })

  expect_error(
    copy_app_files(src_dir, dist_dir, "nonexistent.R"),
    "Entry script not found"
  )
})
