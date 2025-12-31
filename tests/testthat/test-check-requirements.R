test_that("check_build_requirements returns proper structure", {
  temp_dir <- file.path(tempdir(), "test_check_req")
  dir.create(temp_dir, recursive = TRUE)
  dir.create(file.path(temp_dir, "renv"))
  writeLines("# app", file.path(temp_dir, "app.R"))

  lock_content <- jsonlite::toJSON(list(
    R = list(Version = "4.5.2"),
    Packages = list(
      testthat = list(Package = "testthat", Version = "3.0.0", Source = "CRAN")
    )
  ), auto_unbox = TRUE)
  writeLines(lock_content, file.path(temp_dir, "renv.lock"))
  on.exit(unlink(temp_dir, recursive = TRUE))

  result <- check_build_requirements(temp_dir, target = "windows", verbose = FALSE)

  expect_type(result, "list")
  expect_true("renv" %in% names(result))
  expect_true("system_deps" %in% names(result))
  expect_true("cran_access" %in% names(result))
  expect_true("errors" %in% names(result))
  expect_true("warnings" %in% names(result))
})

test_that("check_build_requirements detects valid renv", {
  temp_dir <- file.path(tempdir(), "test_check_valid_renv")
  dir.create(temp_dir, recursive = TRUE)
  dir.create(file.path(temp_dir, "renv"))

  lock_content <- jsonlite::toJSON(list(
    R = list(Version = "4.5.2"),
    Packages = list(
      testthat = list(Package = "testthat", Version = "3.0.0", Source = "CRAN")
    )
  ), auto_unbox = TRUE)
  writeLines(lock_content, file.path(temp_dir, "renv.lock"))
  on.exit(unlink(temp_dir, recursive = TRUE))

  result <- check_build_requirements(temp_dir, target = "windows", verbose = FALSE)

  expect_true(result$renv)
})

test_that("check_build_requirements detects missing renv", {
  temp_dir <- file.path(tempdir(), "test_check_no_renv")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))

  result <- check_build_requirements(temp_dir, target = "windows", verbose = FALSE)

  expect_false(result$renv)
  expect_true(length(result$errors) > 0)
})

test_that("check_build_requirements checks both platforms", {
  temp_dir <- file.path(tempdir(), "test_check_both")
  dir.create(temp_dir, recursive = TRUE)
  dir.create(file.path(temp_dir, "renv"))

  lock_content <- jsonlite::toJSON(list(
    R = list(Version = "4.5.2"),
    Packages = list()
  ), auto_unbox = TRUE)
  writeLines(lock_content, file.path(temp_dir, "renv.lock"))
  on.exit(unlink(temp_dir, recursive = TRUE))

  result <- check_build_requirements(temp_dir, target = "both", verbose = FALSE)

  expect_true("windows" %in% names(result$system_deps))
  expect_true("mac" %in% names(result$system_deps))
})

test_that("list_available_r_versions returns versions", {
  versions <- list_available_r_versions("windows")

  expect_type(versions, "character")
  expect_true(length(versions) > 0)
  expect_true(any(grepl("^4\\.", versions)))
})
