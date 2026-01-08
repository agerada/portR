#' Build portable macOS distribution
#'
#' Creates a portable macOS distribution of an R Shiny application that
#' can run with minimal setup. The distribution includes R, all required
#' packages, and a shell script launcher.
#'
#' @param project_path Path to the project root directory. Must contain a
#'   valid renv.lock file.
#' @param entry_script Relative path to the entry script from project root
#'   (e.g., "app.R" or "run.R"). For multi-file Shiny apps, this can be NULL
#'   if ui.R and server.R are in the project root or app/ subdirectory.
#' @param output_dir Directory where the distribution will be created.
#'   Defaults to "dist" in the project root.
#' @param r_version Target R version for macOS (e.g., "4.5.2").
#' @param r_version_minor R major.minor version for package repository
#'   (e.g., "4.5"). If NULL, derived from r_version.
#' @param fallback_r_version Fallback R version if primary repository is
#'   unavailable.
#' @param arch Target architecture: "arm64" or "x86_64". Defaults to current
#'   system architecture.
#' @param extra_dirs Additional directories to include in the distribution
#'   (e.g., c("models", "data")).
#' @param create_zip Whether to create a zip archive of the distribution.
#' @param zip_name Name of the zip file (without path). Defaults to
#'   "portable_app_macos.zip".
#' @param shiny_command R command to run the Shiny app. Defaults to
#'   "shiny::runApp('app', launch.browser=TRUE)".
#' @param clean_on_error Whether to clean up on error.
#' @param app_name Name of the macOS application bundle (without .app extension).
#'   Only used when make_app = TRUE. Defaults to "ShinyApp".
#' @param make_app Whether to create a macOS application bundle (.app) that can be
#'   copied to /Applications/. If FALSE, creates a portable folder structure.
#'   Defaults to TRUE.
#'
#' @return Invisibly returns the path to the distribution directory.
#'
#' @details
#' This function creates a macOS application bundle (.app) that can be copied
#' to /Applications/ and run like any other macOS app. The bundle includes:
#' - R runtime embedded in the app
#' - All required R packages
#' - Application files
#' - A double-clickable launcher
#'
#' System requirements:
#' - macOS with `pkgutil` and `hdiutil` (standard on macOS)
#' - `zip` command for creating archives (if create_zip=TRUE)
#'
#' @export
#' @examples
#' \dontrun{
#' # Create macOS app bundle (default)
#' build_mac(
#'   project_path = "path/to/my/shiny/app",
#'   entry_script = "app.R",
#'   app_name = "MyShinyApp"
#' )
#'
#' # Create portable folder (like Windows version)
#' build_mac(
#'   project_path = "path/to/my/shiny/app",
#'   entry_script = "app.R",
#'   make_app = FALSE
#' )
#'
#' build_mac(
#'   project_path = ".",
#'   entry_script = "app.R",
#'   r_version = "4.4.2",
#'   arch = "arm64",
#'   extra_dirs = c("models", "data"),
#'   create_zip = TRUE,
#'   app_name = "MyShinyApp"
#' )
#'
#' # For multi-file Shiny apps with ui.R/server.R in project root
#' build_mac(
#'   project_path = "path/to/my/shiny/app"
#'   # entry_script can be NULL if ui.R and server.R exist
#' )
#' }
build_mac <- function(project_path,
                      entry_script = NULL,
                      output_dir = NULL,
                      r_version = "4.5.2",
                      r_version_minor = NULL,
                      fallback_r_version = "4.4",
                      arch = NULL,
                      extra_dirs = NULL,
                      create_zip = FALSE,
                      zip_name = "portable_app_macos.zip",
                      shiny_command = "shiny::runApp('app', launch.browser=TRUE)",
                      clean_on_error = TRUE,
                      app_name = "ShinyApp",
                      make_app = TRUE) {

  # Determine architecture
  if (is.null(arch)) {
    arch <- if (Sys.info()["machine"] == "arm64") "arm64" else "x86_64"
  }
  arch <- match.arg(arch, c("arm64", "x86_64"))

  # Normalize paths
  project_path <- normalizePath(project_path, mustWork = TRUE)

  if (is.null(output_dir)) {
    output_dir <- file.path(project_path, "dist")
  }
  dist_dir <- normalizePath(output_dir, mustWork = FALSE)

  # Derive r_version_minor if not provided
  if (is.null(r_version_minor)) {
    parts <- strsplit(r_version, "\\.")[[1]]
    r_version_minor <- paste(parts[1], parts[2], sep = ".")
  }

  # Setup cleanup on error
  if (clean_on_error) {
    on.exit({
      if (exists(".portR_error") && .portR_error) {
        message("Cleaning up after error...")
        unlink(dist_dir, recursive = TRUE)
      }
    })
  }
  .portR_error <- FALSE

  tryCatch({
    # Step 1: Check renv status
    message("=== Step 1: Checking renv status ===")
    check_renv_status(project_path)

    # Step 2: Check system dependencies
    message("\n=== Step 2: Checking system dependencies ===")
    deps <- check_system_deps("mac")

    # Step 3: Setup distribution directory
    if (make_app) {
      message("\n=== Step 3: Setting up macOS application bundle ===")
      setup_mac_app_bundle(dist_dir, app_name)
    } else {
      message("\n=== Step 3: Setting up portable folder ===")
      setup_dist_directory(dist_dir, "mac")
    }

    # Step 4: Copy app files
    message("\n=== Step 4: Copying app files ===")
    if (make_app) {
      copy_app_files_mac(project_path, dist_dir, entry_script, extra_dirs, app_name)
    } else {
      copy_app_files(project_path, dist_dir, entry_script, extra_dirs)
    }

    # Step 5: Download R for macOS
    message("\n=== Step 5: Downloading R for macOS ===")
    r_url <- get_r_download_url_mac(r_version, arch)
    r_pkg_name <- basename(r_url)
    r_pkg_path <- file.path(dist_dir, r_pkg_name)

    message("Downloading from: ", r_url)
    download.file(r_url, r_pkg_path, mode = "wb", quiet = FALSE)

    # Step 6: Extract R
    message("\n=== Step 6: Extracting R ===")
    if (make_app) {
      extract_r_mac_app(r_pkg_path, dist_dir, app_name)
    } else {
      extract_r_mac(r_pkg_path, dist_dir)
    }

    # Step 7: Download packages
    message("\n=== Step 7: Downloading macOS packages ===")
    if (make_app) {
      download_packages_mac_app(
        project_path = project_path,
        dist_dir = dist_dir,
        r_version_minor = r_version_minor,
        fallback_r_version = fallback_r_version,
        arch = arch,
        app_name = app_name
      )
    } else {
      download_packages_mac(
        project_path = project_path,
        dist_dir = dist_dir,
        r_version_minor = r_version_minor,
        fallback_r_version = fallback_r_version,
        arch = arch
      )
    }

    # Step 8: Create launcher
    if (make_app) {
      message("\n=== Step 8: Creating macOS app launcher ===")
      create_mac_app_launcher(dist_dir, entry_script, shiny_command, app_name)
    } else {
      message("\n=== Step 8: Creating launcher script ===")
      create_mac_launcher(dist_dir, entry_script, shiny_command)
    }

    # Step 9: Create zip if requested
    if (create_zip) {
      message("\n=== Step 9: Creating zip archive ===")
      create_zip_archive(dist_dir, zip_name)
    }

    message("\n=== Build complete! ===")
    if (make_app) {
      message("macOS application bundle created in: ", file.path(dist_dir, paste0(app_name, ".app")))
    } else {
      message("Portable folder created in: ", dist_dir)
    }

    .portR_error <- FALSE
    if (make_app) {
      invisible(file.path(dist_dir, paste0(app_name, ".app")))
    } else {
      invisible(dist_dir)
    }

  }, error = function(e) {
    .portR_error <<- TRUE
    stop("Build failed: ", e$message, call. = FALSE)
  })
}

.download_binary_with_retry <- function(url, dest, quiet = TRUE, retries = 3, retry_wait_sec = 2) {
  last_warnings <- character()
  last_status <- 1

  for (attempt in seq_len(retries)) {
    last_warnings <- character()
    last_status <- withCallingHandlers(
      suppressWarnings(
        download.file(url, dest, mode = "wb", quiet = quiet)
      ),
      warning = function(w) {
        last_warnings <<- c(last_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )

    if (identical(last_status, 0L) && file.exists(dest) && file.info(dest)$size > 0) {
      return(invisible(list(ok = TRUE, warnings = last_warnings, status = last_status)))
    }

    # Briefly wait and retry. CRAN can temporarily publish PACKAGES before binaries sync.
    if (attempt < retries) {
      Sys.sleep(retry_wait_sec)
    }
  }

  invisible(list(ok = FALSE, warnings = last_warnings, status = last_status))
}

#' Setup macOS application bundle structure
#'
#' @param dist_dir Distribution directory
#' @param app_name Name of the app
#' @return Invisible TRUE
#' @keywords internal
setup_mac_app_bundle <- function(dist_dir, app_name) {
  app_bundle_dir <- file.path(dist_dir, paste0(app_name, ".app"))
  
  if (dir.exists(app_bundle_dir)) {
    unlink(app_bundle_dir, recursive = TRUE)
  }

  # Create app bundle structure
  contents_dir <- file.path(app_bundle_dir, "Contents")
  dir.create(contents_dir, recursive = TRUE)
  
  # Create standard directories
  dir.create(file.path(contents_dir, "MacOS"))
  dir.create(file.path(contents_dir, "Resources"))
  dir.create(file.path(contents_dir, "Frameworks"))
  
  # Create app-specific directories
  dir.create(file.path(contents_dir, "Resources", "app"))
  dir.create(file.path(contents_dir, "Resources", "library"))

  invisible(TRUE)
}

#' Copy app files to macOS app bundle
#'
#' @param project_path Path to project root
#' @param dist_dir Path to distribution directory
#' @param entry_script Relative path to entry script from project root
#' @param extra_dirs Additional directories to copy
#' @param app_name Name of the app
#' @return Invisible TRUE
#' @keywords internal
copy_app_files_mac <- function(project_path, dist_dir, entry_script,
                               extra_dirs = NULL, app_name) {
  app_bundle_dir <- file.path(dist_dir, paste0(app_name, ".app"))
  contents_dir <- file.path(app_bundle_dir, "Contents")
  app_dir <- file.path(contents_dir, "Resources", "app")

  # Copy entry script if specified
  if (!is.null(entry_script) && entry_script != "") {
    entry_path <- file.path(project_path, entry_script)
    if (!file.exists(entry_path)) {
      stop("Entry script not found: ", entry_path)
    }
    file.copy(entry_path, app_dir)
  }

  # Copy typical Shiny app files from project root
  shiny_files <- c("ui.R", "server.R", "global.R", "app.R")
  for (file in shiny_files) {
    file_path <- file.path(project_path, file)
    if (file.exists(file_path)) {
      file.copy(file_path, app_dir)
    }
  }

  # Copy www directory if it exists
  www_dir <- file.path(project_path, "www")
  if (dir.exists(www_dir)) {
    file.copy(www_dir, app_dir, recursive = TRUE)
  }

  # Copy app folder if exists (for apps structured with ui.R, server.R in app/)
  app_src_dir <- file.path(project_path, "app")
  if (dir.exists(app_src_dir)) {
    # Copy contents of app/ to our app_dir
    app_files <- list.files(app_src_dir, full.names = TRUE)
    file.copy(app_files, app_dir, recursive = TRUE)
  }

  # Copy renv.lock if it exists
  renv_lock_path <- file.path(project_path, "renv.lock")
  if (file.exists(renv_lock_path)) {
    file.copy(renv_lock_path, app_dir)
  }

  # Copy extra directories
  if (!is.null(extra_dirs)) {
    for (dir_name in extra_dirs) {
      src_dir <- file.path(project_path, dir_name)
      if (dir.exists(src_dir)) {
        dest_dir <- file.path(app_dir, dir_name)
        dir.create(dest_dir, recursive = TRUE)
        file.copy(list.files(src_dir, full.names = TRUE), dest_dir, recursive = TRUE)
      } else {
        warning("Extra directory not found: ", src_dir)
      }
    }
  }

  invisible(TRUE)
}

#' Extract R for macOS app bundle from .pkg installer
#'
#' @param r_pkg_path Path to R .pkg file
#' @param dist_dir Distribution directory
#' @param app_name Name of the app
#' @return Invisible TRUE if successful
#' @keywords internal
extract_r_mac_app <- function(r_pkg_path, dist_dir, app_name) {
  app_bundle_dir <- file.path(dist_dir, paste0(app_name, ".app"))
  contents_dir <- file.path(app_bundle_dir, "Contents")
  frameworks_dir <- file.path(contents_dir, "Frameworks")
  
  extraction_done <- FALSE

  # pkgutil --expand requires the target directory to NOT exist
  # Use a unique temp directory name
  temp_dir <- file.path(dist_dir, paste0("R_temp_pkg_", format(Sys.time(), "%H%M%S")))
  if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)

  # Use pkgutil to expand the package
  message("Expanding .pkg with pkgutil...")
  cmd <- paste("pkgutil --expand", shQuote(r_pkg_path), shQuote(temp_dir))
  ret <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)

  if (ret != 0) {
    # pkgutil failed - try alternative approach with xar
    message("pkgutil failed, trying xar...")
    dir.create(temp_dir)
    cmd <- paste("cd", shQuote(temp_dir), "&& xar -xf", shQuote(r_pkg_path))
    ret <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)

    if (ret != 0) {
      unlink(temp_dir, recursive = TRUE)
      warning("Both pkgutil and xar failed to expand package. Manual extraction required.",
              "\nYou can extract R manually from: ", r_pkg_path)
      return(invisible(FALSE))
    }
  }

  # Find the R-fw.pkg component (which contains R.framework)
  # Important: R-fw.pkg has the framework, R-app.pkg just has the R.app GUI
  pkg_components <- list.dirs(temp_dir, recursive = FALSE)
  
  # Prefer R-fw.pkg specifically (contains R.framework)
  r_component <- pkg_components[grepl("R-fw", basename(pkg_components))]
  
  if (length(r_component) == 0) {
    # Fall back to R-app if no R-fw found
    r_component <- pkg_components[grepl("R-app", basename(pkg_components))]
  }

  if (length(r_component) == 0) {
    # Try looking for Payload directly
    r_component <- temp_dir
  } else {
    r_component <- r_component[1]
  }

  # Extract Payload
  payload_path <- file.path(r_component, "Payload")
  if (!file.exists(payload_path)) {
    # Maybe there's a Scripts directory and Payload is elsewhere
    payloads <- list.files(temp_dir, pattern = "Payload", recursive = TRUE,
                           full.names = TRUE)
    if (length(payloads) > 0) {
      payload_path <- payloads[1]
    }
  }

  if (file.exists(payload_path)) {
    r_extract_dir <- file.path(dist_dir, "R_extracted")
    if (dir.exists(r_extract_dir)) unlink(r_extract_dir, recursive = TRUE)
    dir.create(r_extract_dir)

    message("Extracting Payload...")
    # Payload is a cpio.gz or cpio.xz archive
    # Try gunzip first, then xz if that fails
    cmd <- paste("cd", shQuote(r_extract_dir), "&&",
                 "cat", shQuote(payload_path), "| gunzip -c 2>/dev/null | cpio -id 2>/dev/null",
                 "|| cat", shQuote(payload_path), "| xz -d 2>/dev/null | cpio -id 2>/dev/null",
                 "|| cat", shQuote(payload_path), "| cpio -id 2>/dev/null")
    ret <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)

    # R.framework can be in different locations depending on the pkg structure:
    # - Directly as R.framework (newer packages)
    # - Under Library/Frameworks/R.framework (older packages)
    r_framework <- NULL
    possible_locations <- c(
      file.path(r_extract_dir, "R.framework"),
      file.path(r_extract_dir, "Library", "Frameworks", "R.framework")
    )

    for (loc in possible_locations) {
      if (dir.exists(loc)) {
        r_framework <- loc
        break
      }
    }

    if (!is.null(r_framework)) {
      # Copy the framework to Frameworks directory
      file.copy(r_framework, frameworks_dir, recursive = TRUE)
      extraction_done <- TRUE

      # Verify the Resources directory exists
      resources <- file.path(frameworks_dir, "R.framework", "Resources")
      if (dir.exists(resources)) {
        message("R framework extracted successfully.")
      }
    }
    unlink(r_extract_dir, recursive = TRUE)
  }

  # Cleanup
  unlink(temp_dir, recursive = TRUE)

  if (extraction_done) {
    message("R extracted successfully.")
    unlink(r_pkg_path)
  } else {
    warning("Automatic extraction failed. Please extract R manually.",
            "\nThe .pkg file is at: ", r_pkg_path)
  }

  invisible(extraction_done)
}

#' Download macOS binary packages for app bundle
#'
#' @param project_path Path to project root
#' @param dist_dir Distribution directory
#' @param r_version_minor R major.minor version
#' @param fallback_r_version Fallback version
#' @param arch Architecture
#' @param app_name Name of the app
#' @return Invisible TRUE
#' @keywords internal
download_packages_mac_app <- function(project_path, dist_dir,
                                      r_version_minor, fallback_r_version,
                                      arch = "arm64", app_name) {
  app_bundle_dir <- file.path(dist_dir, paste0(app_name, ".app"))
  contents_dir <- file.path(app_bundle_dir, "Contents")
  lib_dir <- file.path(contents_dir, "Resources", "library")
  
  lock <- read_renv_lock(project_path)
  pkgs <- lock$Packages

  # Get repository
  repo_url <- get_cran_binary_repo_mac(r_version_minor, arch)
  message("Checking repository: ", repo_url)

  repo_info <- check_cran_repo(repo_url, fallback_r_version, "mac")
  avail <- repo_info$packages
  repo_url <- repo_info$repo_url

  failed_pkgs <- character()
  copied_local <- character()

  for (pkg_name in names(pkgs)) {
    pkg_info <- pkgs[[pkg_name]]

    # Handle local packages
    if (!is.null(pkg_info$Source) && pkg_info$Source == "Local") {
      message("Copying local package: ", pkg_name)
      local_path <- tryCatch(
        find.package(pkg_name, lib.loc = .libPaths(), quiet = TRUE),
        error = function(e) NULL
      )
      if (!is.null(local_path) && length(local_path) > 0) {
        file.copy(local_path[1], lib_dir, recursive = TRUE)
        copied_local <- c(copied_local, pkg_name)
      } else {
        warning("Could not find local package: ", pkg_name)
        failed_pkgs <- c(failed_pkgs, pkg_name)
      }
      next
    }

    # Handle CRAN packages
    if (pkg_info$Source %in% c("Repository", "CRAN")) {
      lock_ver <- if (!is.null(pkg_info$Version)) as.character(pkg_info$Version) else NULL
      repo_ver <- if (pkg_name %in% rownames(avail)) as.character(avail[pkg_name, "Version"]) else NULL
      try_versions <- unique(stats::na.omit(c(lock_ver, repo_ver)))

      if (length(try_versions) == 0) {
        warning("Package ", pkg_name, " not found in CRAN macOS binaries.")
        failed_pkgs <- c(failed_pkgs, pkg_name)
        next
      }

      if (!file.exists(file.path(lib_dir, pkg_name))) {
        downloaded <- FALSE
        lock_failed <- FALSE
        used_version <- NULL
        for (ver in try_versions) {
          url <- paste0(repo_url, "/", pkg_name, "_", ver, ".tgz")
          dest <- file.path(lib_dir, paste0(pkg_name, ".tgz"))

          message("Downloading ", pkg_name, " ", ver, "...")
          downloaded <- tryCatch({
            dl <- .download_binary_with_retry(url, dest, quiet = TRUE, retries = 3, retry_wait_sec = 2)
            if (!isTRUE(dl$ok)) {
              if (!is.null(lock_ver) && identical(ver, lock_ver)) lock_failed <- TRUE
              return(FALSE)
            }
            untar(dest, exdir = lib_dir)
            unlink(dest)
            TRUE
          }, error = function(e) {
            FALSE
          })

          if (isTRUE(downloaded)) {
            used_version <- ver
            break
          }
        }

        if (isTRUE(downloaded) && isTRUE(lock_failed) && !is.null(repo_ver) && identical(used_version, repo_ver)) {
          warning(
            "Downloaded ", pkg_name, " ", repo_ver,
            " because lockfile version ", lock_ver, " was not available as a macOS binary yet.",
            call. = FALSE
          )
        }

        if (!isTRUE(downloaded)) {
          warning(
            "Failed to download ", pkg_name, " (tried versions: ", paste(try_versions, collapse = ", "), "). ",
            "\nThis can happen during CRAN sync windows: PACKAGES metadata and the binary directory may be temporarily out of sync.",
            "\nWorkaround: set CRAN as your repos and update/snapshot your lockfile:",
            "\n  options(repos = c(CRAN = \"https://cran.r-project.org\"))",
            "\n  renv::update()",
            "\n  renv::snapshot()",
            call. = FALSE
          )
          failed_pkgs <- c(failed_pkgs, pkg_name)
        }
      }
    }
  }

  if (length(copied_local) > 0) {
    message("Copied ", length(copied_local), " local package(s)")
  }

  if (length(failed_pkgs) > 0) {
    warning("Failed to download ", length(failed_pkgs), " package(s): ",
            paste(failed_pkgs, collapse = ", "))
  }

  invisible(TRUE)
}

#' Create macOS app launcher and Info.plist
#'
#' @param dist_dir Distribution directory
#' @param entry_script Entry script name
#' @param shiny_command R command to run
#' @param app_name Name of the app
#' @return Invisible path to executable
#' @keywords internal
create_mac_app_launcher <- function(dist_dir, entry_script, shiny_command, app_name) {
  app_bundle_dir <- file.path(dist_dir, paste0(app_name, ".app"))
  contents_dir <- file.path(app_bundle_dir, "Contents")
  macos_dir <- file.path(contents_dir, "MacOS")
  
  # Create executable script
  script_content <- c(
    "#!/bin/bash",
    "",
    "# Get the directory where this script is located",
    "SCRIPT_DIR=\"$(cd \"$(dirname \"${BASH_SOURCE[0]}\")\" && pwd)\"",
    "CONTENTS_DIR=\"$(dirname \"$SCRIPT_DIR\")\"",
    "",
    "# Set up R environment",
    "export R_HOME=\"$CONTENTS_DIR/Frameworks/R.framework/Resources\"",
    "export R_LIBS_USER=\"$CONTENTS_DIR/Resources/library\"",
    "export PATH=\"$R_HOME/bin:$PATH\"",
    "",
    "# Change to the Resources directory where the app folder is located",
    "cd \"$CONTENTS_DIR/Resources\"",
    "",
    "echo \"Starting application...\"",
    paste0("\"$R_HOME/bin/Rscript\" -e \"", shiny_command, "\"")
  )

  executable_path <- file.path(macos_dir, app_name)
  writeLines(script_content, executable_path)

  # Make executable
  Sys.chmod(executable_path, mode = "0755")

  message("Created executable: ", app_name)

  # Create Info.plist
  info_plist_content <- c(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
    "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">",
    "<plist version=\"1.0\">",
    "<dict>",
    paste0("    <key>CFBundleName</key>"),
    paste0("    <string>", app_name, "</string>"),
    paste0("    <key>CFBundleDisplayName</key>"),
    paste0("    <string>", app_name, "</string>"),
    paste0("    <key>CFBundleIdentifier</key>"),
    paste0("    <string>com.portr.", tolower(gsub("[^a-zA-Z0-9]", "", app_name)), "</string>"),
    paste0("    <key>CFBundleVersion</key>"),
    "    <string>1.0</string>",
    paste0("    <key>CFBundleExecutable</key>"),
    paste0("    <string>", app_name, "</string>"),
    "    <key>CFBundleIconFile</key>",
    "    <string></string>",
    "    <key>CFBundlePackageType</key>",
    "    <string>APPL</string>",
    "    <key>LSMinimumSystemVersion</key>",
    "    <string>10.12</string>",
    "</dict>",
    "</plist>"
  )
  
  info_plist_path <- file.path(contents_dir, "Info.plist")
  writeLines(info_plist_content, info_plist_path)

  message("Created Info.plist")

  invisible(executable_path)
}

#' Get R download URL for macOS
#'
#' @param version R version string
#' @param arch Architecture (arm64 or x86_64)
#' @return URL string
#' @keywords internal
get_r_download_url_mac <- function(version, arch = "arm64") {
  # macOS R downloads are at:
  # https://cran.r-project.org/bin/macosx/big-sur-arm64/base/R-4.5.2-arm64.pkg
  # https://cran.r-project.org/bin/macosx/big-sur-x86_64/base/R-4.5.2-x86_64.pkg
  paste0("https://cran.r-project.org/bin/macosx/big-sur-", arch,
         "/base/R-", version, "-", arch, ".pkg")
}

#' Extract R for macOS from .pkg installer
#'
#' @param r_pkg_path Path to R .pkg file
#' @param dist_dir Distribution directory
#' @return Invisible TRUE if successful
#' @keywords internal
extract_r_mac <- function(r_pkg_path, dist_dir) {
  extraction_done <- FALSE

  # pkgutil --expand requires the target directory to NOT exist
  # Use a unique temp directory name
  temp_dir <- file.path(dist_dir, paste0("R_temp_pkg_", format(Sys.time(), "%H%M%S")))
  if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)

  # Use pkgutil to expand the package
  message("Expanding .pkg with pkgutil...")
  cmd <- paste("pkgutil --expand", shQuote(r_pkg_path), shQuote(temp_dir))
  ret <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)

  if (ret != 0) {
    # pkgutil failed - try alternative approach with xar
    message("pkgutil failed, trying xar...")
    dir.create(temp_dir)
    cmd <- paste("cd", shQuote(temp_dir), "&& xar -xf", shQuote(r_pkg_path))
    ret <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)

    if (ret != 0) {
      unlink(temp_dir, recursive = TRUE)
      warning("Both pkgutil and xar failed to expand package. Manual extraction required.",
              "\nYou can extract R manually from: ", r_pkg_path)
      return(invisible(FALSE))
    }
  }

  # Find the R-fw.pkg component (which contains R.framework)
  # Important: R-fw.pkg has the framework, R-app.pkg just has the R.app GUI
  pkg_components <- list.dirs(temp_dir, recursive = FALSE)
  
  # Prefer R-fw.pkg specifically (contains R.framework)
  r_component <- pkg_components[grepl("R-fw", basename(pkg_components))]
  
  if (length(r_component) == 0) {
    # Fall back to R-app if no R-fw found
    r_component <- pkg_components[grepl("R-app", basename(pkg_components))]
  }

  if (length(r_component) == 0) {
    # Try looking for Payload directly
    r_component <- temp_dir
  } else {
    r_component <- r_component[1]
  }

  # Extract Payload
  payload_path <- file.path(r_component, "Payload")
  if (!file.exists(payload_path)) {
    # Maybe there's a Scripts directory and Payload is elsewhere
    payloads <- list.files(temp_dir, pattern = "Payload", recursive = TRUE,
                           full.names = TRUE)
    if (length(payloads) > 0) {
      payload_path <- payloads[1]
    }
  }

  if (file.exists(payload_path)) {
    r_extract_dir <- file.path(dist_dir, "R_extracted")
    if (dir.exists(r_extract_dir)) unlink(r_extract_dir, recursive = TRUE)
    dir.create(r_extract_dir)

    message("Extracting Payload...")
    # Payload is a cpio.gz or cpio.xz archive
    # Try gunzip first, then xz if that fails
    cmd <- paste("cd", shQuote(r_extract_dir), "&&",
                 "cat", shQuote(payload_path), "| gunzip -c 2>/dev/null | cpio -id 2>/dev/null",
                 "|| cat", shQuote(payload_path), "| xz -d 2>/dev/null | cpio -id 2>/dev/null",
                 "|| cat", shQuote(payload_path), "| cpio -id 2>/dev/null")
    ret <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)

    # R.framework can be in different locations depending on the pkg structure:
    # - Directly as R.framework (newer packages)
    # - Under Library/Frameworks/R.framework (older packages)
    r_framework <- NULL
    possible_locations <- c(
      file.path(r_extract_dir, "R.framework"),
      file.path(r_extract_dir, "Library", "Frameworks", "R.framework")
    )

    for (loc in possible_locations) {
      if (dir.exists(loc)) {
        r_framework <- loc
        break
      }
    }

    if (!is.null(r_framework)) {
      # Create R directory and copy the framework
      r_dest <- file.path(dist_dir, "R")
      if (dir.exists(r_dest)) unlink(r_dest, recursive = TRUE)
      dir.create(r_dest)

      # Copy the framework
      file.copy(r_framework, r_dest, recursive = TRUE)
      extraction_done <- TRUE

      # Verify the Resources directory exists
      resources <- file.path(r_dest, "R.framework", "Resources")
      if (dir.exists(resources)) {
        message("R framework extracted successfully.")
      }
    }
    unlink(r_extract_dir, recursive = TRUE)
  }

  # Cleanup
  unlink(temp_dir, recursive = TRUE)

  if (extraction_done) {
    message("R extracted successfully.")
    unlink(r_pkg_path)
  } else {
    warning("Automatic extraction failed. Please extract R manually.",
            "\nThe .pkg file is at: ", r_pkg_path)
  }

  invisible(extraction_done)
}

#' Download macOS binary packages
#'
#' @param project_path Path to project root
#' @param dist_dir Distribution directory
#' @param r_version_minor R major.minor version
#' @param fallback_r_version Fallback version
#' @param arch Architecture
#' @return Invisible TRUE
#' @keywords internal
download_packages_mac <- function(project_path, dist_dir,
                                   r_version_minor, fallback_r_version,
                                   arch = "arm64") {
  lock <- read_renv_lock(project_path)
  pkgs <- lock$Packages

  # Get repository
  repo_url <- get_cran_binary_repo_mac(r_version_minor, arch)
  message("Checking repository: ", repo_url)

  repo_info <- check_cran_repo(repo_url, fallback_r_version, "mac")
  avail <- repo_info$packages
  repo_url <- repo_info$repo_url

  lib_dir <- file.path(dist_dir, "library")
  failed_pkgs <- character()
  copied_local <- character()

  for (pkg_name in names(pkgs)) {
    pkg_info <- pkgs[[pkg_name]]

    # Handle local packages
    if (!is.null(pkg_info$Source) && pkg_info$Source == "Local") {
      message("Copying local package: ", pkg_name)
      local_path <- tryCatch(
        find.package(pkg_name, lib.loc = .libPaths(), quiet = TRUE),
        error = function(e) NULL
      )
      if (!is.null(local_path) && length(local_path) > 0) {
        file.copy(local_path[1], lib_dir, recursive = TRUE)
        copied_local <- c(copied_local, pkg_name)
      } else {
        warning("Could not find local package: ", pkg_name)
        failed_pkgs <- c(failed_pkgs, pkg_name)
      }
      next
    }

    # Handle CRAN packages
    if (pkg_info$Source %in% c("Repository", "CRAN")) {
      lock_ver <- if (!is.null(pkg_info$Version)) as.character(pkg_info$Version) else NULL
      repo_ver <- if (pkg_name %in% rownames(avail)) as.character(avail[pkg_name, "Version"]) else NULL
      try_versions <- unique(stats::na.omit(c(lock_ver, repo_ver)))

      if (length(try_versions) == 0) {
        warning("Package ", pkg_name, " not found in CRAN macOS binaries.")
        failed_pkgs <- c(failed_pkgs, pkg_name)
        next
      }

      if (!file.exists(file.path(lib_dir, pkg_name))) {
        downloaded <- FALSE
        lock_failed <- FALSE
        used_version <- NULL
        for (ver in try_versions) {
          url <- paste0(repo_url, "/", pkg_name, "_", ver, ".tgz")
          dest <- file.path(lib_dir, paste0(pkg_name, ".tgz"))

          message("Downloading ", pkg_name, " ", ver, "...")
          downloaded <- tryCatch({
            dl <- .download_binary_with_retry(url, dest, quiet = TRUE, retries = 3, retry_wait_sec = 2)
            if (!isTRUE(dl$ok)) {
              if (!is.null(lock_ver) && identical(ver, lock_ver)) lock_failed <- TRUE
              return(FALSE)
            }
            untar(dest, exdir = lib_dir)
            unlink(dest)
            TRUE
          }, error = function(e) {
            FALSE
          })

          if (isTRUE(downloaded)) {
            used_version <- ver
            break
          }
        }

        if (isTRUE(downloaded) && isTRUE(lock_failed) && !is.null(repo_ver) && identical(used_version, repo_ver)) {
          warning(
            "Downloaded ", pkg_name, " ", repo_ver,
            " because lockfile version ", lock_ver, " was not available as a macOS binary yet.",
            call. = FALSE
          )
        }

        if (!isTRUE(downloaded)) {
          warning(
            "Failed to download ", pkg_name, " (tried versions: ", paste(try_versions, collapse = ", "), "). ",
            "\nThis can happen during CRAN sync windows: PACKAGES metadata and the binary directory may be temporarily out of sync.",
            "\nWorkaround: set CRAN as your repos and update/snapshot your lockfile:",
            "\n  options(repos = c(CRAN = \"https://cran.r-project.org\"))",
            "\n  renv::update()",
            "\n  renv::snapshot()",
            call. = FALSE
          )
          failed_pkgs <- c(failed_pkgs, pkg_name)
        }
      }
    }
  }

  if (length(copied_local) > 0) {
    message("Copied ", length(copied_local), " local package(s)")
  }

  if (length(failed_pkgs) > 0) {
    warning("Failed to download ", length(failed_pkgs), " package(s): ",
            paste(failed_pkgs, collapse = ", "))
  }

  invisible(TRUE)
}

#' Get CRAN binary repository URL for macOS
#'
#' @param r_version R major.minor version
#' @param arch Architecture
#' @return URL string
#' @keywords internal
get_cran_binary_repo_mac <- function(r_version, arch = "arm64") {
  paste0("https://cran.r-project.org/bin/macosx/big-sur-", arch,
         "/contrib/", r_version)
}

#' Create macOS launcher shell script
#'
#' @param dist_dir Distribution directory
#' @param entry_script Entry script name
#' @param shiny_command R command to run
#' @return Invisible path to script
#' @keywords internal
create_mac_launcher <- function(dist_dir, entry_script, shiny_command) {
  script_content <- c(
    "#!/bin/bash",
    "",
    "# Get the directory where this script is located",
    "SCRIPT_DIR=\"$(cd \"$(dirname \"${BASH_SOURCE[0]}\")\" && pwd)\"",
    "",
    "# Set up R environment",
    "export R_HOME=\"$SCRIPT_DIR/R/R.framework/Resources\"",
    "export R_LIBS_USER=\"$SCRIPT_DIR/library\"",
    "export PATH=\"$R_HOME/bin:$PATH\"",
    "",
    "echo \"Starting application...\"",
    paste0("\"$R_HOME/bin/Rscript\" -e \"", shiny_command, "\"")
  )

  script_path <- file.path(dist_dir, "run_app.sh")
  writeLines(script_content, script_path)

  # Make executable
  Sys.chmod(script_path, mode = "0755")

  message("Created launcher: run_app.sh")

  # Also create a .command file for double-click launching
  command_content <- c(
    "#!/bin/bash",
    "cd \"$(dirname \"$0\")\"",
    "./run_app.sh"
  )
  command_path <- file.path(dist_dir, "Run Application.command")
  writeLines(command_content, command_path)
  Sys.chmod(command_path, mode = "0755")

  message("Created double-click launcher: Run Application.command")

  invisible(script_path)
}
