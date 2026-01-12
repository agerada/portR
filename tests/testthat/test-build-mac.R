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

# ============================================================================
# Electron Wrapper Tests
# ============================================================================

test_that("use_electron requires make_app = TRUE", {
  temp_dir <- file.path(tempdir(), "test_electron_make_app")
  dir.create(temp_dir, recursive = TRUE)
  writeLines("# app", file.path(temp_dir, "app.R"))
  writeLines('{"R": {}, "Packages": {}}', file.path(temp_dir, "renv.lock"))
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  expect_error(
    build_mac(temp_dir, "app.R", use_electron = TRUE, make_app = FALSE),
    "use_electron = TRUE requires make_app = TRUE"
  )
})

test_that("get_latest_electron_version returns valid version string", {
  skip_on_cran()
  skip_if_offline()
  
  version <- get_latest_electron_version()
  
  # Should be a version string like "39.2.7" or similar
  expect_type(version, "character")
  expect_match(version, "^\\d+\\.\\d+\\.\\d+$")
})

test_that("create_electron_main_js creates valid main.js and package.json", {
  app_bundle_path <- file.path(tempdir(), "TestElectronApp.app")
  resources_dir <- file.path(app_bundle_path, "Contents", "Resources")
  dir.create(resources_dir, recursive = TRUE)
  on.exit(unlink(app_bundle_path, recursive = TRUE))
  
  create_electron_main_js(app_bundle_path, "TestElectronApp", 1984)
  
  app_dir <- file.path(resources_dir, "app")
  
  # Check package.json exists and is valid
  pkg_json_path <- file.path(app_dir, "package.json")
  expect_true(file.exists(pkg_json_path))
  
  pkg_content <- readLines(pkg_json_path)
  expect_true(any(grepl('"name"', pkg_content)))
  expect_true(any(grepl('"main": "main.js"', pkg_content)))
  
  # Check main.js exists and contains key elements
  main_js_path <- file.path(app_dir, "main.js")
  expect_true(file.exists(main_js_path))
  
  main_content <- paste(readLines(main_js_path), collapse = "\n")
  
  # Should have required Electron modules
  expect_true(grepl('require\\("electron"\\)', main_content))
  expect_true(grepl("BrowserWindow", main_content))
  
  # Should reference R components
  expect_true(grepl("R.framework", main_content))
  expect_true(grepl("R_HOME", main_content))
  
  # Should have Shiny URL
  expect_true(grepl("SHINY_PORT", main_content))
  expect_true(grepl("127.0.0.1", main_content))
  
  # Should handle app lifecycle
  expect_true(grepl("app.whenReady", main_content))
  expect_true(grepl("window-all-closed", main_content))
})

test_that("rebrand_electron_app updates Info.plist correctly", {
  app_bundle_path <- file.path(tempdir(), "TestRebrand.app")
  contents_dir <- file.path(app_bundle_path, "Contents")
  macos_dir <- file.path(contents_dir, "MacOS")
  dir.create(macos_dir, recursive = TRUE)
  on.exit(unlink(app_bundle_path, recursive = TRUE))
  
  # Create a mock Info.plist
  plist_content <- '<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleIdentifier</key>
  <string>com.electron.electron</string>
  <key>CFBundleName</key>
  <string>Electron</string>
  <key>CFBundleExecutable</key>
  <string>Electron</string>
</dict>
</plist>'
  writeLines(plist_content, file.path(contents_dir, "Info.plist"))
  
  # Create mock executable
  writeLines("#!/bin/bash\necho 'Hello'", file.path(macos_dir, "Electron"))
  
  rebrand_electron_app(app_bundle_path, "MyShinyApp")
  
  # Check Info.plist was updated
  new_plist <- paste(readLines(file.path(contents_dir, "Info.plist")), collapse = "\n")
  expect_true(grepl("com.portr.myshinyapp", new_plist))
  expect_true(grepl("MyShinyApp", new_plist))
  expect_false(grepl("com.electron.electron", new_plist))
  
  # Check executable was renamed
  expect_true(file.exists(file.path(macos_dir, "MyShinyApp")))
  expect_false(file.exists(file.path(macos_dir, "Electron")))
})

test_that("copy_app_files_electron copies files to correct location", {
  # Create source project
  project_path <- file.path(tempdir(), "test_electron_project")
  dir.create(project_path, recursive = TRUE)
  writeLines("library(shiny)\nshinyApp(ui, server)", file.path(project_path, "app.R"))
  dir.create(file.path(project_path, "www"))
  writeLines("body { color: red; }", file.path(project_path, "www", "style.css"))
  dir.create(file.path(project_path, "data"))
  writeLines("1,2,3", file.path(project_path, "data", "test.csv"))
  
  # Create app bundle structure
  app_bundle_path <- file.path(tempdir(), "TestCopy.app")
  resources_dir <- file.path(app_bundle_path, "Contents", "Resources")
  dir.create(resources_dir, recursive = TRUE)
  
  on.exit({
    unlink(project_path, recursive = TRUE)
    unlink(app_bundle_path, recursive = TRUE)
  })
  
  copy_app_files_electron(project_path, app_bundle_path, "app.R", c("data"))
  
  shiny_app_dir <- file.path(resources_dir, "shiny_app")
  
  # Check files were copied
  expect_true(file.exists(file.path(shiny_app_dir, "app.R")))
  expect_true(file.exists(file.path(shiny_app_dir, "www", "style.css")))
  expect_true(file.exists(file.path(shiny_app_dir, "data", "test.csv")))
})

test_that("fix_r_framework_paths_electron handles missing directories gracefully", {
  temp_dir <- file.path(tempdir(), "test_fix_paths")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # Should warn but not error when directories are missing
  expect_warning(
    result <- fix_r_framework_paths_electron(temp_dir),
    "Versions"
  )
  expect_false(result)
})

