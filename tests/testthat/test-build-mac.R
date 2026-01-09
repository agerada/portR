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

test_that("create_mac_app_launcher creates self-contained launcher", {
  dist_dir <- file.path(tempdir(), "test_mac_app_launcher")
  app_name <- "TestApp"
  
  # Create the app bundle structure
  app_bundle_dir <- file.path(dist_dir, paste0(app_name, ".app"))
  contents_dir <- file.path(app_bundle_dir, "Contents")
  macos_dir <- file.path(contents_dir, "MacOS")
  resources_dir <- file.path(contents_dir, "Resources")
  frameworks_dir <- file.path(contents_dir, "Frameworks")
  
  dir.create(macos_dir, recursive = TRUE)
  dir.create(file.path(resources_dir, "app"), recursive = TRUE)
  dir.create(file.path(resources_dir, "library"), recursive = TRUE)
  dir.create(file.path(frameworks_dir, "R.framework", "Resources", "bin"), recursive = TRUE)
  
  on.exit(unlink(dist_dir, recursive = TRUE))
  
  result <- create_mac_app_launcher(
    dist_dir,
    "app.R",
    "shiny::runApp('app')",
    app_name
  )
  
  expect_true(file.exists(result))
  
  # Main executable is a wrapper that opens Terminal
  main_content <- readLines(result)
  expect_true(any(grepl("#!/bin/bash", main_content)))
  expect_true(any(grepl("osascript", main_content)))  # Uses AppleScript to open Terminal
  expect_true(any(grepl("Terminal", main_content)))
  expect_true(any(grepl("launcher.sh", main_content)))
  
  # Check the actual launcher script in Resources
  launcher_path <- file.path(contents_dir, "Resources", "launcher.sh")
  expect_true(file.exists(launcher_path))
  
  launcher_content <- readLines(launcher_path)
  
  # Check for self-contained path setup in launcher
  expect_true(any(grepl("#!/bin/bash", launcher_content)))
  expect_true(any(grepl("CONTENTS_DIR", launcher_content)))
  expect_true(any(grepl("RESOURCES_DIR", launcher_content)))
  expect_true(any(grepl("R_FRAMEWORK", launcher_content)))
  expect_true(any(grepl("R_SCRIPT=", launcher_content)))  # Uses R script, not Rscript binary
  
  # Check for environment isolation
  expect_true(any(grepl("R_LIBS=", launcher_content)))
  expect_true(any(grepl("R_LIBS_USER=", launcher_content)))
  
  # Check for debug/diagnostic output (always on now)
  expect_true(any(grepl("portR App Launcher", launcher_content)))
  expect_true(any(grepl("error_exit", launcher_content)))
  
  # Check for path verification
  expect_true(any(grepl("\\[OK\\]", launcher_content)) || any(grepl("\\[FAIL\\]", launcher_content)))
  
  # Check for architecture validation
  expect_true(any(grepl("uname -m", launcher_content)))
  expect_true(any(grepl("arm64", launcher_content)) && any(grepl("x86_64", launcher_content)))
  
  # Check it uses R with --vanilla and --slave for clean R session
  expect_true(any(grepl("--vanilla", launcher_content)))
  expect_true(any(grepl("--slave", launcher_content)))
  
  # Check executable permissions for both files
  file_info <- file.info(result)
  expect_true(file_info$mode >= as.octmode("0700"))
  
  launcher_info <- file.info(launcher_path)
  expect_true(launcher_info$mode >= as.octmode("0700"))
  
  # Check Info.plist was created
  info_plist <- file.path(contents_dir, "Info.plist")
  expect_true(file.exists(info_plist))
  
  plist_content <- readLines(info_plist)
  expect_true(any(grepl("CFBundleExecutable", plist_content)))
  expect_true(any(grepl(app_name, plist_content)))
})

test_that("launcher script has no external path dependencies", {
  dist_dir <- file.path(tempdir(), "test_mac_no_external_deps")
  app_name <- "TestApp"
  
  # Create the app bundle structure
  app_bundle_dir <- file.path(dist_dir, paste0(app_name, ".app"))
  contents_dir <- file.path(app_bundle_dir, "Contents")
  macos_dir <- file.path(contents_dir, "MacOS")
  resources_dir <- file.path(contents_dir, "Resources")
  frameworks_dir <- file.path(contents_dir, "Frameworks")
  
  dir.create(macos_dir, recursive = TRUE)
  dir.create(file.path(resources_dir, "app"), recursive = TRUE)
  dir.create(file.path(resources_dir, "library"), recursive = TRUE)
  dir.create(file.path(frameworks_dir, "R.framework", "Resources", "bin"), recursive = TRUE)
  
  on.exit(unlink(dist_dir, recursive = TRUE))
  
  result <- create_mac_app_launcher(
    dist_dir,
    "app.R",
    "shiny::runApp('app')",
    app_name
  )
  
  # Check the launcher.sh script (not the wrapper)
  launcher_path <- file.path(contents_dir, "Resources", "launcher.sh")
  expect_true(file.exists(launcher_path))
  
  content <- paste(readLines(launcher_path), collapse = "\n")
  
  # Must NOT contain any absolute paths to system locations
  expect_false(grepl("/Library/Frameworks", content))
  expect_false(grepl("/usr/local", content))
  expect_false(grepl("/opt/", content))
  expect_false(grepl("/Users/", content))
  expect_false(grepl("/home/", content))
  
  # All R paths should be relative to CONTENTS_DIR or derived variables
  # Check that R_HOME is set relative to the bundle
  expect_true(grepl("R_HOME.*R_HOME_DIR", content) || 
              grepl("R_HOME.*CONTENTS_DIR", content) ||
              grepl("R_HOME_DIR.*R_FRAMEWORK", content))
})

