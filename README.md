# portR

Build portable R application distributions that run without requiring R installation on the target machine.

## Overview

`portR` creates self-contained distributions of R Shiny applications for:

- **Windows**: Includes R runtime, packages, and a `.bat` launcher
- **macOS**: Includes R framework, packages, and shell script launchers

The distributions can be run from USB drives, network shares, or any location without administrative privileges.

## Installation

```r
devtools::install_github("username/portR")
```

## Requirements

### Project Requirements

Your R project must use **renv** for dependency management. If your project is
already set up to use **renv**, then this section can be skipped. Otherwise:

```r
# In your project
renv::init()
renv::snapshot()
```

### System Requirements

The build host and target machine determine which tools are required. Use the table below to quickly find the required tools for your build scenario.

| Host machine | Target machine | Requirements |
|---|---|---|
| macOS / Linux | Windows | [innoextract (recommended) or 7z for extracting the R installer](#windows-builds) |
| macOS | macOS | [pkgutil and hdiutil (included on macOS)](#macos-builds) |
| Any | Creating archives | [zip command (standard on most systems)](#creating-archives) |

#### <a name="windows-builds"></a>For Windows builds (on macOS/Linux)

`innoextract` (recommended) or `7z` for extracting the R installer.

Install notes:

```bash
# macOS
brew install innoextract

# Ubuntu/Debian
sudo apt install innoextract
```

#### <a name="macos-builds"></a>For macOS builds

`pkgutil` and `hdiutil` (standard on macOS)

#### <a name="creating-archives"></a>Creating archives

`zip` command (standard on most systems)

## Usage

### Check Requirements

```r
library(portR)

# Check if your project is ready for building
check_build_requirements("path/to/your/project", target = "both")
```

### Build for Windows

```r
build_windows(
  project_path = "path/to/your/project",
  entry_script = "app.R",
  r_version = "4.5.2",
  extra_dirs = c("models", "data"),
  create_zip = TRUE
)
```

### Build for macOS

```r
build_mac(
  project_path = "path/to/your/project",
  entry_script = "app.R",  # Optional for multi-file apps
  r_version = "4.5.2",
  arch = "arm64",  # or "x86_64"
  extra_dirs = c("models", "data"),
  create_zip = TRUE,
  app_name = "MyShinyApp"
)
```

## Function Arguments

### Common Arguments

| Argument | Description | Default |
|----------|-------------|---------|
| `project_path` | Path to project root with renv.lock | Required |
| `entry_script` | Relative path to entry script (e.g., "app.R") | `NULL` (auto-detect) |
| `output_dir` | Where to create distribution | `"dist"` in project |
| `r_version` | Target R version | `"4.5.2"` |
| `r_version_minor` | R major.minor for package repo | Derived from `r_version` |
| `fallback_r_version` | Fallback if primary repo unavailable | `"4.4"` |
| `extra_dirs` | Additional directories to include | `NULL` |
| `create_zip` | Create zip archive | `FALSE` |
| `zip_name` | Name of zip file | Platform-specific |
| `shiny_command` | R command to run app | `"shiny::runApp('app', launch.browser=TRUE)"` |
| `clean_on_error` | Clean up on build failure | `TRUE` |

### macOS-Specific

| Argument | Description | Default |
|----------|-------------|---------|
| `arch` | Target architecture: "arm64" or "x86_64" | System architecture |
| `app_name` | Name of the macOS application bundle (without .app) | `"ShinyApp"` |

## Output Structure

### Windows Distribution

```
dist/
├── R/                    # R runtime
│   └── bin/x64/
├── library/              # R packages
├── app/                  # Application files
├── app.R                 # Entry script
├── models/               # Extra directories
└── run_app.bat           # Launcher (double-click to run)
```

### macOS Distribution

Creates a macOS application bundle (.app) that can be copied to `/Applications/` and run like any other macOS app:

```
dist/
└── ShinyApp.app/         # macOS application bundle
    └── Contents/
        ├── Info.plist    # App metadata
        ├── MacOS/
        │   └── ShinyApp  # Executable launcher
        ├── Frameworks/
        │   └── R.framework/  # Embedded R runtime
        └── Resources/
            ├── app/      # Application files
            │   ├── app.R # Entry script
            │   └── models/  # Extra directories
            └── library/  # R packages
```

## Local Packages

If your project includes local packages (source = "Local" in renv.lock), they will be copied from your current R library. Make sure they are installed before building.

## Troubleshooting

### "renv.lock not found"

Initialize renv in your project:

```r
renv::init()
renv::snapshot()
```

### "No extraction tool available"

Install innoextract:

```bash
brew install innoextract  # macOS
```

### Package download failures

- Check internet connectivity
- Try a different R version with `fallback_r_version`
- Some packages may not have binary versions available

## License

MIT
