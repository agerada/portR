test_that("get_r_download_url returns correct Windows URL", {
  url <- get_r_download_url("4.5.2", "windows")
  expect_equal(url, "https://cran.r-project.org/bin/windows/base/R-4.5.2-win.exe")
})

test_that("get_cran_binary_repo returns correct Windows repo URL", {
  url <- get_cran_binary_repo("4.5", "windows")
  expect_equal(url, "https://cran.r-project.org/bin/windows/contrib/4.5")
})

test_that("check_r_extraction detects bin directory", {
  temp_dir <- file.path(tempdir(), "test_r_check")
  dir.create(file.path(temp_dir, "bin"), recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))

  expect_true(check_r_extraction(temp_dir))
})

test_that("check_r_extraction detects library directory", {
  temp_dir <- file.path(tempdir(), "test_r_check_lib")
  dir.create(file.path(temp_dir, "library"), recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))

  expect_true(check_r_extraction(temp_dir))
})

test_that("check_r_extraction returns FALSE for empty directory", {
  temp_dir <- file.path(tempdir(), "test_r_check_empty")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))

  expect_false(check_r_extraction(temp_dir))
})

test_that("create_windows_launcher creates batch file", {
  dist_dir <- file.path(tempdir(), "test_launcher")
  dir.create(dist_dir, recursive = TRUE)
  on.exit(unlink(dist_dir, recursive = TRUE))

  result <- create_windows_launcher(
    dist_dir,
    "app.R",
    "shiny::runApp('app')"
  )

  expect_true(file.exists(result))
  expect_true(grepl("\\.bat$", result))

  content <- readLines(result)
  expect_true(any(grepl("@echo off", content)))
  expect_true(any(grepl("R_HOME", content)))
  expect_true(any(grepl("R_LIBS_USER", content)))
  expect_true(any(grepl("shiny::runApp", content)))
})

test_that("build_windows validates project path", {
  expect_error(
    build_windows("/nonexistent/path", "app.R")
  )
})

test_that("build_windows validates renv", {
  temp_dir <- file.path(tempdir(), "test_build_no_renv")
  dir.create(temp_dir, recursive = TRUE)
  writeLines("# app", file.path(temp_dir, "app.R"))
  on.exit(unlink(temp_dir, recursive = TRUE))

  expect_error(
    build_windows(temp_dir, "app.R", clean_on_error = FALSE),
    "renv"
  )
})
