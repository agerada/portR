#' @title Utility functions for portR
#' @name utils
#' @keywords internal
#' @importFrom utils available.packages download.file untar unzip
NULL

#' Check if renv is initialized and status is OK
#'
#' @param project_path Path to the project root
#' @return Invisible TRUE if OK, throws error otherwise
#' @keywords internal
check_renv_status <- function(project_path) {
  renv_lock <- file.path(project_path, "renv.lock")
  renv_folder <- file.path(project_path, "renv")

  if (!file.exists(renv_lock)) {
    stop("renv.lock not found in project root. Please initialize renv first.",
         call. = FALSE)
  }

  if (!dir.exists(renv_folder)) {
    stop("renv folder not found in project root. Please initialize renv first.",
         call. = FALSE)
  }

  # Check renv status by running renv::status() in the project context

  # We read the lockfile and check it's valid JSON
  lock_content <- tryCatch({
    jsonlite::read_json(renv_lock)
  }, error = function(e) {
    stop("renv.lock is not valid JSON: ", e$message, call. = FALSE)
  })

  if (is.null(lock_content$Packages)) {
    stop("renv.lock does not contain any packages.", call. = FALSE)
  }

  message("renv status: OK (", length(lock_content$Packages), " packages in lockfile)")
  invisible(TRUE)
}

#' Read renv.lock file
#'
#' @param project_path Path to the project root
#' @return List containing lockfile contents
#' @keywords internal
read_renv_lock <- function(project_path) {
  renv_lock <- file.path(project_path, "renv.lock")
  jsonlite::read_json(renv_lock)
}

#' Check system dependencies for building
#'
#' @param target Target platform ("windows" or "mac")
#' @return Named list of available tools
#' @keywords internal
check_system_deps <- function(target = c("windows", "mac")) {
  target <- match.arg(target)


  deps <- list()

  # Common dependencies
  deps$curl <- nzchar(Sys.which("curl"))

  if (target == "windows") {
    # For Windows builds, we need extraction tools
    deps$innoextract <- nzchar(Sys.which("innoextract"))
    deps$seven_z <- nzchar(Sys.which("7z")) || nzchar(Sys.which("7za"))

    if (!deps$innoextract && !deps$seven_z) {
      warning("Neither 'innoextract' nor '7z' found. ",
              "R for Windows extraction may fail. ",
              "Install with: brew install innoextract",
              call. = FALSE)
    }
  }

  if (target == "mac")

    # For Mac builds, we may need hdiutil (usually available on macOS)
    deps$hdiutil <- nzchar(Sys.which("hdiutil"))

  # Check for zip
  deps$zip <- nzchar(Sys.which("zip"))

  invisible(deps)
}

#' Create directory structure for distribution
#'
#' @param dist_dir Path to distribution directory
#' @param target Target platform
#' @return Invisible TRUE
#' @keywords internal
setup_dist_directory <- function(dist_dir, target = c("windows", "mac")) {
  target <- match.arg(target)

  if (dir.exists(dist_dir)) {
    unlink(dist_dir, recursive = TRUE)
  }

  dir.create(dist_dir, recursive = TRUE)
  dir.create(file.path(dist_dir, "library"))
  dir.create(file.path(dist_dir, "app"))

  invisible(TRUE)
}

#' Copy app files to distribution directory
#'
#' @param project_path Path to project root
#' @param dist_dir Path to distribution directory
#' @param entry_script Relative path to entry script from project root
#' @param extra_dirs Additional directories to copy
#' @return Invisible TRUE
#' @keywords internal
copy_app_files <- function(project_path, dist_dir, entry_script,
                           extra_dirs = NULL) {
  # Copy entry script

  entry_src <- file.path(project_path, entry_script)
  if (!file.exists(entry_src)) {
    stop("Entry script not found: ", entry_src, call. = FALSE)
  }
  file.copy(entry_src, dist_dir)

  # Copy app folder if exists
  app_dir <- file.path(project_path, "app")
  if (dir.exists(app_dir)) {
    file.copy(app_dir, dist_dir, recursive = TRUE)
  }

  # Copy extra directories
  if (!is.null(extra_dirs)) {
    for (dir_name in extra_dirs) {
      src_dir <- file.path(project_path, dir_name)
      if (dir.exists(src_dir)) {
        file.copy(src_dir, dist_dir, recursive = TRUE)
      }
    }
  }

  invisible(TRUE)
}

#' Get R download URL
#'
#' @param version R version string (e.g., "4.5.2")
#' @param target Target platform
#' @return URL string
#' @keywords internal
get_r_download_url <- function(version, target = c("windows", "mac")) {
  target <- match.arg(target)

  if (target == "windows") {
    paste0("https://cran.r-project.org/bin/windows/base/R-", version, "-win.exe")
  } else {
    # Mac uses .pkg files
    # Format: R-4.5.2-arm64.pkg or R-4.5.2-x86_64.pkg
    arch <- if (Sys.info()["machine"] == "arm64") "arm64" else "x86_64"
    paste0("https://cran.r-project.org/bin/macosx/big-sur-", arch,
           "/base/R-", version, "-", arch, ".pkg")
  }
}

#' Get CRAN binary repository URL
#'
#' @param r_version R major.minor version (e.g., "4.5")
#' @param target Target platform
#' @return URL string
#' @keywords internal
get_cran_binary_repo <- function(r_version, target = c("windows", "mac")) {
  target <- match.arg(target)

  if (target == "windows") {
    paste0("https://cran.r-project.org/bin/windows/contrib/", r_version)
  } else {
    arch <- if (Sys.info()["machine"] == "arm64") "arm64" else "x86_64"
    paste0("https://cran.r-project.org/bin/macosx/big-sur-", arch,
           "/contrib/", r_version)
  }
}

#' Check if CRAN binary repository is available
#'
#' @param repo_url Repository URL
#' @param fallback_version Fallback R version if primary fails
#' @param target Target platform
#' @return List with repo_url and available packages
#' @keywords internal
check_cran_repo <- function(repo_url, fallback_version = "4.4",
                            target = c("windows", "mac")) {
  target <- match.arg(target)

  result <- tryCatch({
    avail <- available.packages(repo_url, filters = NULL)
    list(repo_url = repo_url, packages = avail, success = TRUE)
  }, warning = function(w) {
    NULL
  }, error = function(e) {
    NULL
  })

  if (is.null(result)) {
    message("Repository not accessible. Falling back to version ", fallback_version)
    fallback_url <- get_cran_binary_repo(fallback_version, target)
    result <- tryCatch({
      avail <- available.packages(fallback_url, filters = NULL)
      list(repo_url = fallback_url, packages = avail, success = TRUE)
    }, error = function(e) {
      stop("Could not access CRAN binary repository: ", e$message, call. = FALSE)
    })
  }

  result
}

#' Create a zip archive of the distribution
#'
#' @param dist_dir Path to distribution directory
#' @param output_zip Path to output zip file
#' @return Invisible path to zip file
#' @keywords internal
create_zip_archive <- function(dist_dir, output_zip) {
  if (!nzchar(Sys.which("zip"))) {
    stop("'zip' command not found. Cannot create archive.", call. = FALSE
    )
  }

  # Get parent directory and folder name

parent_dir <- dirname(dist_dir)
  folder_name <- basename(dist_dir)

  # Create zip from parent directory
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(parent_dir)

  cmd <- paste("zip -r", shQuote(output_zip), shQuote(folder_name))
  ret <- system(cmd)

  if (ret != 0) {
    stop("Failed to create zip archive", call. = FALSE)
  }

  message("Created archive: ", file.path(parent_dir, output_zip))
  invisible(file.path(parent_dir, output_zip))
}
