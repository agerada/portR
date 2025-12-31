#' Build portable macOS distribution
#'
#' Creates a portable macOS distribution of an R Shiny application that
#' can run with minimal setup. The distribution includes R, all required
#' packages, and a shell script launcher.
#'
#' @param project_path Path to the project root directory. Must contain a
#'   valid renv.lock file.
#' @param entry_script Relative path to the entry script from project root
#'   (e.g., "app.R" or "run.R").
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
#'
#' @return Invisibly returns the path to the distribution directory.
#'
#' @details
#' This function:
#' 1. Validates renv status in the project
#' 2. Downloads R for macOS (as .pkg)
#' 3. Extracts R using pkgutil/hdiutil
#' 4. Downloads macOS binary packages from CRAN
#' 5. Copies app files and local packages
#' 6. Creates a launcher shell script
#' 7. Optionally creates a zip archive
#'
#' System requirements:
#' - macOS with `pkgutil` and `hdiutil` (standard on macOS)
#' - `zip` command for creating archives (if create_zip=TRUE)
#'
#' @export
#' @examples
#' \dontrun{
#' build_mac(
#'   project_path = "path/to/my/shiny/app",
#'   entry_script = "app.R"
#' )
#'
#' build_mac(
#'   project_path = ".",
#'   entry_script = "app.R",
#'   r_version = "4.4.2",
#'   arch = "arm64",
#'   extra_dirs = c("models", "data"),
#'   create_zip = TRUE
#' )
#' }
build_mac <- function(project_path,
                      entry_script,
                      output_dir = NULL,
                      r_version = "4.5.2",
                      r_version_minor = NULL,
                      fallback_r_version = "4.4",
                      arch = NULL,
                      extra_dirs = NULL,
                      create_zip = FALSE,
                      zip_name = "portable_app_macos.zip",
                      shiny_command = "shiny::runApp('app', launch.browser=TRUE)",
                      clean_on_error = TRUE) {

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
    message("\n=== Step 3: Setting up distribution directory ===")
    setup_dist_directory(dist_dir, "mac")

    # Step 4: Copy app files
    message("\n=== Step 4: Copying app files ===")
    copy_app_files(project_path, dist_dir, entry_script, extra_dirs)

    # Step 5: Download R for macOS
    message("\n=== Step 5: Downloading R for macOS ===")
    r_url <- get_r_download_url_mac(r_version, arch)
    r_pkg_name <- basename(r_url)
    r_pkg_path <- file.path(dist_dir, r_pkg_name)

    message("Downloading from: ", r_url)
    download.file(r_url, r_pkg_path, mode = "wb", quiet = FALSE)

    # Step 6: Extract R
    message("\n=== Step 6: Extracting R ===")
    extract_r_mac(r_pkg_path, dist_dir)

    # Step 7: Download packages
    message("\n=== Step 7: Downloading macOS packages ===")
    download_packages_mac(
      project_path = project_path,
      dist_dir = dist_dir,
      r_version_minor = r_version_minor,
      fallback_r_version = fallback_r_version,
      arch = arch
    )

    # Step 8: Create launcher
    message("\n=== Step 8: Creating launcher script ===")
    create_mac_launcher(dist_dir, entry_script, shiny_command)

    # Step 9: Create zip if requested
    if (create_zip) {
      message("\n=== Step 9: Creating zip archive ===")
      create_zip_archive(dist_dir, zip_name)
    }

    message("\n=== Build complete! ===")
    message("Distribution created in: ", dist_dir)

    .portR_error <- FALSE
    invisible(dist_dir)

  }, error = function(e) {
    .portR_error <<- TRUE
    stop("Build failed: ", e$message, call. = FALSE)
  })
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
      if (pkg_name %in% rownames(avail)) {
        ver <- avail[pkg_name, "Version"]
        # macOS uses .tgz files
        url <- paste0(repo_url, "/", pkg_name, "_", ver, ".tgz")
        dest <- file.path(lib_dir, paste0(pkg_name, ".tgz"))

        if (!file.exists(file.path(lib_dir, pkg_name))) {
          message("Downloading ", pkg_name, " ", ver, "...")
          result <- tryCatch({
            download.file(url, dest, mode = "wb", quiet = TRUE)
            # Extract .tgz
            untar(dest, exdir = lib_dir)
            unlink(dest)
            TRUE
          }, error = function(e) {
            warning("Failed to download ", pkg_name, ": ", e$message)
            FALSE
          })
          if (!result) failed_pkgs <- c(failed_pkgs, pkg_name)
        }
      } else {
        warning("Package ", pkg_name, " not found in CRAN macOS binaries.")
        failed_pkgs <- c(failed_pkgs, pkg_name)
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
