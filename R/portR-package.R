#' @title portR: Build Portable R Application Distributions
#'
#' @description
#' portR provides tools to create portable distributions of R Shiny
#' applications that can run without requiring R installation on the
#' target machine. It supports building for Windows and macOS.
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{build_windows}}}{Build a portable Windows distribution}
#'   \item{\code{\link{build_mac}}}{Build a portable macOS distribution}
#'   \item{\code{\link{check_build_requirements}}}{Check system requirements}
#' }
#'
#' @section Requirements:
#' Projects must use renv for dependency management. The renv.lock file
#' is used to determine which packages to include in the distribution.
#'
#' @docType package
#' @name portR-package
#' @aliases portR
#' @keywords internal
"_PACKAGE"

#' Check build requirements
#'
#' Checks that all requirements are met for building a portable distribution.
#' This includes renv status, system dependencies, and network access.
#'
#' @param project_path Path to the project root directory.
#' @param target Target platform: "windows", "mac", or "both".
#' @param verbose Whether to print detailed messages.
#'
#' @return A list with check results, invisibly.
#'
#' @export
#' @examples
#' \dontrun{
#' check_build_requirements("path/to/project", target = "windows")
#' check_build_requirements(".", target = "both")
#' }
check_build_requirements <- function(project_path,
                                      target = c("windows", "mac", "both"),
                                      verbose = TRUE) {
  target <- match.arg(target)

  project_path <- normalizePath(project_path, mustWork = TRUE)

  results <- list(
    renv = FALSE,
    system_deps = list(),
    cran_access = FALSE,
    errors = character(),
    warnings = character()
  )

  # Check renv
  if (verbose) message("Checking renv status...")
  tryCatch({
    check_renv_status(project_path)
    results$renv <- TRUE
    if (verbose) message("  [OK] renv is properly configured")
  }, error = function(e) {
    results$errors <<- c(results$errors, paste("renv:", e$message))
    if (verbose) message("  [ERROR] ", e$message)
  })

  # Check system dependencies
  if (verbose) message("\nChecking system dependencies...")

  targets_to_check <- if (target == "both") c("windows", "mac") else target

  for (t in targets_to_check) {
    if (verbose) message("  Target: ", t)
    deps <- check_system_deps(t)
    results$system_deps[[t]] <- deps

    if (t == "windows") {
      if (deps$innoextract) {
        if (verbose) message("    [OK] innoextract available")
      } else if (deps$seven_z) {
        if (verbose) message("    [OK] 7z available (fallback)")
      } else {
        results$warnings <- c(results$warnings,
                              "Windows: No extraction tool available (innoextract or 7z)")
        if (verbose) message("    [WARN] No extraction tool (install innoextract)")
      }
    }

    if (t == "mac") {
      if (deps$hdiutil) {
        if (verbose) message("    [OK] hdiutil available")
      } else {
        results$warnings <- c(results$warnings, "macOS: hdiutil not available")
        if (verbose) message("    [WARN] hdiutil not available")
      }
    }

    if (deps$zip) {
      if (verbose) message("    [OK] zip available")
    } else {
      results$warnings <- c(results$warnings, paste(t, ": zip command not available"))
      if (verbose) message("    [WARN] zip not available (needed for archives)")
    }
  }

  # Check CRAN access
  if (verbose) message("\nChecking CRAN repository access...")
  tryCatch({
    # Try to access CRAN
    avail <- available.packages(
      repos = "https://cran.r-project.org",
      filters = NULL
    )
    if (nrow(avail) > 0) {
      results$cran_access <- TRUE
      if (verbose) message("  [OK] CRAN is accessible")
    }
  }, error = function(e) {
    results$errors <- c(results$errors, paste("CRAN access:", e$message))
    if (verbose) message("  [ERROR] Cannot access CRAN: ", e$message)
  })

  # Summary
  if (verbose) {
    message("\n=== Summary ===")
    all_ok <- results$renv && results$cran_access && length(results$errors) == 0

    if (all_ok && length(results$warnings) == 0) {
      message("All requirements met. Ready to build!")
    } else if (all_ok) {
      message("Requirements met with warnings:")
      for (w in results$warnings) {
        message("  - ", w)
      }
    } else {
      message("Requirements not met:")
      for (e in results$errors) {
        message("  [ERROR] ", e)
      }
      for (w in results$warnings) {
        message("  [WARN] ", w)
      }
    }
  }

  invisible(results)
}

#' List available R versions for download
#'
#' Queries CRAN to find available R versions for the specified platform.
#'
#' @param target Target platform: "windows" or "mac".
#' @param arch Architecture for macOS: "arm64" or "x86_64".
#'
#' @return Character vector of available R versions (may be partial).
#'
#' @export
#' @examples
#' \dontrun{
#' list_available_r_versions("windows")
#' list_available_r_versions("mac", arch = "arm64")
#' }
list_available_r_versions <- function(target = c("windows", "mac"),
                                       arch = c("arm64", "x86_64")) {
  target <- match.arg(target)
  arch <- match.arg(arch)

  # Note: This is a simplified version. Full implementation would

  # parse the CRAN directory listings.
  message("Commonly available R versions: 4.3.x, 4.4.x, 4.5.x")
  message("Check https://cran.r-project.org for current versions.")

  # Return commonly available versions
  c("4.5.2", "4.5.1", "4.5.0", "4.4.2", "4.4.1", "4.4.0", "4.3.3")
}
