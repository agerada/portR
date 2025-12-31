test_that("get_r_download_url_mac returns correct arm64 URL", {
  url <- get_r_download_url_mac("4.5.2", "arm64")
  expect_equal(
    url,
    "https://cran.r-project.org/bin/macosx/big-sur-arm64/base/R-4.5.2-arm64.pkg"
  )
})

test_that("get_r_download_url_mac returns correct x86_64 URL", {
  url <- get_r_download_url_mac("4.5.2", "x86_64")
  expect_equal(
    url,
    "https://cran.r-project.org/bin/macosx/big-sur-x86_64/base/R-4.5.2-x86_64.pkg"
  )
})

test_that("get_cran_binary_repo_mac returns correct arm64 repo URL", {
  url <- get_cran_binary_repo_mac("4.5", "arm64")
  expect_equal(
    url,
    "https://cran.r-project.org/bin/macosx/big-sur-arm64/contrib/4.5"
  )
})

test_that("get_cran_binary_repo_mac returns correct x86_64 repo URL", {
  url <- get_cran_binary_repo_mac("4.5", "x86_64")
  expect_equal(
    url,
    "https://cran.r-project.org/bin/macosx/big-sur-x86_64/contrib/4.5"
  )
})

test_that("create_mac_launcher creates shell script", {
  dist_dir <- file.path(tempdir(), "test_mac_launcher")
  dir.create(dist_dir, recursive = TRUE)
  on.exit(unlink(dist_dir, recursive = TRUE))

  result <- create_mac_launcher(
    dist_dir,
    "app.R",
    "shiny::runApp('app')"
  )

  expect_true(file.exists(result))
  expect_true(grepl("run_app\\.sh$", result))

  content <- readLines(result)
  expect_true(any(grepl("#!/bin/bash", content)))
  expect_true(any(grepl("R_HOME", content)))
  expect_true(any(grepl("R_LIBS_USER", content)))
  expect_true(any(grepl("shiny::runApp", content)))

  # Check executable permissions
  file_info <- file.info(result)
  expect_true(file_info$mode >= as.octmode("0700"))
})

test_that("create_mac_launcher creates .command file", {
  dist_dir <- file.path(tempdir(), "test_mac_command")
  dir.create(dist_dir, recursive = TRUE)
  on.exit(unlink(dist_dir, recursive = TRUE))

  create_mac_launcher(dist_dir, "app.R", "shiny::runApp('app')")

  command_file <- file.path(dist_dir, "Run Application.command")
  expect_true(file.exists(command_file))

  # Check it references run_app.sh
  content <- readLines(command_file)
  expect_true(any(grepl("run_app.sh", content)))
})

test_that("build_mac validates project path", {
  expect_error(
    build_mac("/nonexistent/path", "app.R")
  )
})

test_that("build_mac validates renv", {
  temp_dir <- file.path(tempdir(), "test_build_mac_no_renv")
  dir.create(temp_dir, recursive = TRUE)
  writeLines("# app", file.path(temp_dir, "app.R"))
  on.exit(unlink(temp_dir, recursive = TRUE))

  expect_error(
    build_mac(temp_dir, "app.R", clean_on_error = FALSE),
    "renv"
  )
})

test_that("build_mac accepts architecture parameter", {
  # Test that the parameter matching works
  expect_error(
    build_mac(".", "app.R", arch = "invalid_arch"),
    regexp = "arg"
  )
})
