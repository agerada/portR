#' Build portable Windows distribution
#'
#' Creates a portable Windows distribution of an R Shiny application that
#' can run without installation. The distribution includes R, all required
#' packages, and a batch file launcher.
#'
#' @param project_path Path to the project root directory. Must contain a
#'   valid renv.lock file.
#' @param entry_script Relative path to the entry script from project root
#'   (e.g., "app.R" or "run.R").
#' @param output_dir Directory where the distribution will be created.
#'   Defaults to "dist" in the project root.
#' @param r_version Target R version for Windows (e.g., "4.5.2").
#' @param r_version_minor R major.minor version for package repository
#'   (e.g., "4.5"). If NULL, derived from r_version.
#' @param fallback_r_version Fallback R version if primary repository is
#'   unavailable.
#' @param extra_dirs Additional directories to include in the distribution
#'   (e.g., c("models", "data")).
#' @param create_zip Whether to create a zip archive of the distribution.
#' @param zip_name Name of the zip file (without path). Defaults to
#'   "portable_app_windows.zip".
#' @param shiny_command R command to run the Shiny app. Defaults to
#'   "shiny::runApp('app', launch.browser=TRUE)".
#' @param clean_on_error Whether to clean up on error.
#'
#' @return Invisibly returns the path to the distribution directory.
#'
#' @details
#' This function:
#' 1. Validates renv status in the project
#' 2. Downloads R for Windows
#' 3. Extracts R using innoextract or 7z

#' 4. Downloads Windows binary packages from CRAN
#' 5. Copies app files and local packages
#' 6. Creates a launcher batch file
#' 7. Optionally creates a zip archive
#'
#' System requirements:
#' - `innoextract` or `7z` for extracting R installer
#' - `zip` command for creating archives (if create_zip=TRUE)
#'
#' @export
#' @examples
#' \dontrun{
#' build_windows(
#'   project_path = "path/to/my/shiny/app",
#'   entry_script = "app.R"
#' )
#'
#' build_windows(
#'   project_path = ".",
#'   entry_script = "app.R",
#'   r_version = "4.4.2",
#'   extra_dirs = c("models", "data"),
#'   create_zip = TRUE
#' )
#' }
build_windows <- function(project_path,
                          entry_script,
                          output_dir = NULL,
                          r_version = "4.5.2",
                          r_version_minor = NULL,
                          fallback_r_version = "4.4",
                          extra_dirs = NULL,
                          create_zip = FALSE,
                          zip_name = "portable_app_windows.zip",
                          shiny_command = "shiny::runApp('app', launch.browser=TRUE)",
                          clean_on_error = TRUE) {

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
    deps <- check_system_deps("windows")
    if (!deps$innoextract && !deps$seven_z) {
      warning("R extraction may require manual intervention")
    }

    # Step 3: Setup distribution directory
    message("\n=== Step 3: Setting up distribution directory ===")
    setup_dist_directory(dist_dir, "windows")

    # Step 4: Copy app files
    message("\n=== Step 4: Copying app files ===")
    copy_app_files(project_path, dist_dir, entry_script, extra_dirs)

    # Step 5: Download R for Windows
    message("\n=== Step 5: Downloading R for Windows ===")
    r_url <- get_r_download_url(r_version, "windows")
    r_exe_name <- basename(r_url)
    r_exe_path <- file.path(dist_dir, r_exe_name)

    message("Downloading from: ", r_url)
    download.file(r_url, r_exe_path, mode = "wb", quiet = FALSE)

    # Step 6: Extract R
    message("\n=== Step 6: Extracting R ===")
    extract_r_windows(r_exe_path, dist_dir, deps)

    # Step 7: Download packages
    message("\n=== Step 7: Downloading Windows packages ===")
    download_packages_windows(
      project_path = project_path,
      dist_dir = dist_dir,
      r_version_minor = r_version_minor,
      fallback_r_version = fallback_r_version
    )

    # Step 8: Create launcher
    message("\n=== Step 8: Creating launcher script ===")
    create_windows_launcher(dist_dir, entry_script, shiny_command)

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

#' Extract R for Windows from installer
#'
#' @param r_exe_path Path to R installer
#' @param dist_dir Distribution directory
#' @param deps System dependencies list
#' @return Invisible TRUE if successful
#' @keywords internal
extract_r_windows <- function(r_exe_path, dist_dir, deps) {
  extraction_done <- FALSE

  # Try innoextract first
  if (deps$innoextract) {
    message("Using innoextract...")
    extract_dir <- file.path(dist_dir, "R_temp_inno")
    if (dir.exists(extract_dir)) unlink(extract_dir, recursive = TRUE)
    dir.create(extract_dir)

    innoextract_path <- Sys.which("innoextract")
    cmd <- paste(shQuote(innoextract_path), "-d", shQuote(extract_dir),
                 shQuote(r_exe_path))
    ret <- system(cmd)

    if (ret == 0) {
      # innoextract creates an 'app' folder
      app_dir <- file.path(extract_dir, "app")
      target_r_dir <- file.path(dist_dir, "R")
      
      # Determine source directory for R files
      source_dir <- if (dir.exists(app_dir)) app_dir else if (check_r_extraction(extract_dir)) extract_dir else NULL
      
      if (!is.null(source_dir)) {
        # If target R directory already exists (e.g., from extra_dirs), merge contents
        if (dir.exists(target_r_dir)) {
          # Copy extracted R files into existing directory, preserving user files
          all_files <- list.files(source_dir, full.names = TRUE)
          for (f in all_files) {
            file.copy(f, target_r_dir, recursive = TRUE, overwrite = TRUE)
          }
        } else {
          # No existing directory, just rename
          file.rename(source_dir, target_r_dir)
        }
        extraction_done <- TRUE
      }
    }
    if (dir.exists(extract_dir)) unlink(extract_dir, recursive = TRUE)
  }

  # Try 7z as fallback
  if (!extraction_done && deps$seven_z) {
    message("Using 7z...")
    seven_z_path <- Sys.which("7z")
    if (!nzchar(seven_z_path)) seven_z_path <- Sys.which("7za")

    temp_dir <- file.path(dist_dir, "R_temp")
    cmd <- paste(shQuote(seven_z_path), "x", shQuote(r_exe_path),
                 paste0("-o", shQuote(temp_dir)), "-y")
    ret <- system(cmd)

    if (ret == 0 && check_r_extraction(temp_dir)) {
      target_r_dir <- file.path(dist_dir, "R")
      
      # Determine the source R directory
      if (dir.exists(file.path(temp_dir, "bin"))) {
        source_dir <- temp_dir
      } else {
        subdirs <- list.dirs(temp_dir, recursive = FALSE)
        r_dir <- subdirs[grep("R-", basename(subdirs))]
        source_dir <- if (length(r_dir) > 0) r_dir[1] else NULL
      }
      
      if (!is.null(source_dir)) {
        # If target R directory already exists (e.g., from extra_dirs), merge contents
        if (dir.exists(target_r_dir)) {
          # Copy extracted R files into existing directory, preserving user files
          all_files <- list.files(source_dir, full.names = TRUE)
          for (f in all_files) {
            file.copy(f, target_r_dir, recursive = TRUE, overwrite = TRUE)
          }
        } else {
          # No existing directory, just rename
          file.rename(source_dir, target_r_dir)
        }
        extraction_done <- TRUE
      }
    }
    if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)
  }

  if (extraction_done) {
    message("R extracted successfully.")
    unlink(r_exe_path)
  } else {
    warning("Automatic extraction failed. Please extract '", basename(r_exe_path),
            "' to '", file.path(dist_dir, "R"), "' manually.",
            "\nInstall innoextract with: brew install innoextract")
  }

  invisible(extraction_done)
}

#' Check if R extraction was successful
#'
#' @param path Path to check
#' @return TRUE if R appears to be extracted
#' @keywords internal
check_r_extraction <- function(path) {
  if (dir.exists(file.path(path, "bin")) || dir.exists(file.path(path, "library"))) {
    return(TRUE)
  }
  subdirs <- list.dirs(path, recursive = FALSE)
  for (d in subdirs) {
    if (dir.exists(file.path(d, "bin"))) return(TRUE)
  }
  FALSE
}

#' Download Windows binary packages
#'
#' @param project_path Path to project root
#' @param dist_dir Distribution directory
#' @param r_version_minor R major.minor version
#' @param fallback_r_version Fallback version
#' @return Invisible TRUE
#' @keywords internal
download_packages_windows <- function(project_path, dist_dir,
                                       r_version_minor, fallback_r_version) {
  lock <- read_renv_lock(project_path)
  pkgs <- lock$Packages

  # Get repository
  repo_url <- get_cran_binary_repo(r_version_minor, "windows")
  message("Checking repository: ", repo_url)

  repo_info <- check_cran_repo(repo_url, fallback_r_version, "windows")
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
        url <- paste0(repo_url, "/", pkg_name, "_", ver, ".zip")
        dest <- file.path(lib_dir, paste0(pkg_name, ".zip"))

        if (!file.exists(file.path(lib_dir, pkg_name))) {
          message("Downloading ", pkg_name, " ", ver, "...")
          result <- tryCatch({
            download.file(url, dest, mode = "wb", quiet = TRUE)
            unzip(dest, exdir = lib_dir)
            unlink(dest)
            TRUE
          }, error = function(e) {
            warning("Failed to download ", pkg_name, ": ", e$message)
            FALSE
          })
          if (!result) failed_pkgs <- c(failed_pkgs, pkg_name)
        }
      } else {
        warning("Package ", pkg_name, " not found in CRAN Windows binaries.")
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

#' Create Windows launcher batch file
#'
#' @param dist_dir Distribution directory
#' @param entry_script Entry script name
#' @param shiny_command R command to run
#' @return Invisible path to batch file
#' @keywords internal
create_windows_launcher <- function(dist_dir, entry_script,
                                     shiny_command) {
  bat_content <- c(
    "@echo off",
    "SET R_HOME=%~dp0R",
    "SET PATH=%R_HOME%\\bin\\x64;%PATH%",
    "SET R_LIBS_USER=%~dp0library",
    "echo Starting application...",
    paste0("\"%R_HOME%\\bin\\x64\\Rscript.exe\" -e \"", shiny_command, "\""),
    "pause"
  )

  bat_path <- file.path(dist_dir, "run_app.bat")
  writeLines(bat_content, bat_path)
  message("Created launcher: run_app.bat")

  invisible(bat_path)
}
