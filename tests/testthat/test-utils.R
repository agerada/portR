test_that("check_renv_status validates renv.lock exists", {
  # Create a temp directory without renv
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_no_renv")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE))

  expect_error(
    check_renv_status(test_dir),
    "renv.lock not found"
  )
})

test_that("check_renv_status validates renv folder exists", {
  # Create a temp directory with renv.lock but no renv folder
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_no_renv_folder")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create minimal renv.lock
  writeLines('{"R":{"Version":"4.5.2"},"Packages":{}}', 
             file.path(test_dir, "renv.lock"))

  expect_error(
    check_renv_status(test_dir),
    "renv folder not found"
  )
})

test_that("check_renv_status validates renv.lock is valid JSON", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_invalid_json")
  dir.create(test_dir, showWarnings = FALSE)
  dir.create(file.path(test_dir, "renv"), showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create invalid JSON
  writeLines("not valid json {", file.path(test_dir, "renv.lock"))

  expect_error(
    check_renv_status(test_dir),
    "not valid JSON"
  )
})

test_that("check_renv_status succeeds with valid renv setup", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_valid_renv")
  dir.create(test_dir, showWarnings = FALSE)
  dir.create(file.path(test_dir, "renv"), showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create valid renv.lock with packages
  lock_content <- jsonlite::toJSON(list(
    R = list(Version = "4.5.2"),
    Packages = list(
      rlang = list(Package = "rlang", Version = "1.0.0", Source = "CRAN")
    )
  ), auto_unbox = TRUE)
  writeLines(lock_content, file.path(test_dir, "renv.lock"))

  expect_true(check_renv_status(test_dir))
})

test_that("read_renv_lock returns lockfile contents", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_read_lock")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE))

  lock_content <- list(
    R = list(Version = "4.5.2"),
    Packages = list(
      rlang = list(Package = "rlang", Version = "1.0.0")
    )
  )
  jsonlite::write_json(lock_content, file.path(test_dir, "renv.lock"),
                       auto_unbox = TRUE)

  result <- read_renv_lock(test_dir)
  expect_equal(result$R$Version, "4.5.2")
  expect_true("rlang" %in% names(result$Packages))
})
