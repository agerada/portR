# portR

Build portable R application distributions that run without requiring R installation on the target machine.

## Overview

`portR` creates self-contained distributions of R Shiny applications for:

- **Windows**: Currently deployed as a portable folder with R framework, packages, and batch script launchers
- **macOS**: Can create either an installable macOS application bundle (.app) or a portable folder similar to the Windows version. Additionally, an Electron-based wrapper is available to create standalone shiny apps on macOS. 

Although other tools exist of local R deployement ([`RInno`](https://github.com/ficonsulting/RInno) for Windows, [`DesktopDeployR`](https://github.com/wleepang/DesktopDeployR), and [`electricShine`](https://github.com/chasemc/electricShine)), they are not actively maintained. `portR` has the following additional features:

- Cross-platform support (build Windows and macOS distributions),
- Uses **renv** for dependency management,
- Creation of macOS application bundles (.app) for native user experience and easier security permission management,
- Supports Electron-based app wrapper (currently experimental, macOS only).

## Installation

```r
remotes::install_github("agerada/portR")
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

Some general guidelines for setting up your project with renv:

- Ensure all required packages are installed and recorded in `renv.lock`
- Only record packages that are necessary for your application, i.e., avoid including packages used only for development or testing (to minimize distribution size)
- Carefully organise your project structure such that there is a single clear entry point script (e.g., `app.R`), 
  and any additional resources (data files, models, etc.) are located in subdirectories
- Use relative paths within your project for any data files or resources (e.g., `data/mydata.csv` instead of absolute paths like `/Users/username/project/data/mydata.csv`)

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
# Create macOS app bundle (recommended for end users)
build_mac(
  project_path = "path/to/your/project",
  entry_script = "app.R",  # Optional for multi-file apps
  r_version = "4.5.2",
  arch = "arm64",  # or "x86_64"
  extra_dirs = c("models", "data"),
  create_dmg = TRUE,  # Create DMG for distribution
  app_name = "MyShinyApp",
  make_app = TRUE  # Default
)

# Create portable folder (like Windows version)
build_mac(
  project_path = "path/to/your/project",
  entry_script = "app.R",
  make_app = FALSE,
  create_zip = TRUE  # Zip only for portable folder mode
)

### Experimental: Electron wrapper (macOS) ⚠️

The project includes an experimental Electron-based app wrapper for macOS that launches your Shiny app in a native window (no Terminal or browser required).

```r
# Create a macOS app bundle using the experimental Electron wrapper
build_mac(
  project_path = "path/to/your/project",
  entry_script = "app.R",
  app_name = "MyShinyApp",
  use_electron = TRUE  # Experimental
)
```
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
| `shiny_command` | R command to run app | `"shiny::runApp('app', launch.browser=TRUE)"` |
| `clean_on_error` | Clean up on build failure | `TRUE` |

### macOS-Specific

| Argument | Description | Default |
|----------|-------------|---------|
| `app_name` | Name of the macOS application bundle (without .app) | `"ShinyApp"` |
| `make_app` | Create macOS app bundle (TRUE) or portable folder (FALSE) | `TRUE` |
| `use_electron` | Use Electron wrapper (experimental) | `FALSE` |
| `create_dmg` | Create DMG disk image (app bundle mode only) | `FALSE` |
| `dmg_name` | Name of DMG file | `"{app_name}.dmg"` |
| `create_zip` | Create zip archive (portable folder mode only) | `FALSE` |
| `zip_name` | Name of zip file | `"portable_app_macos.zip"` |

## Output Structure

### macOS Application Bundle (make_app = TRUE)

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

### macOS Portable Folder (make_app = FALSE)

Creates a portable folder structure similar to the Windows version:

```
dist/
├── R/                    # R framework
│   └── R.framework/
├── library/              # R packages
├── app/                  # Application files
├── app.R                 # Entry script
├── models/               # Extra directories
├── run_app.sh            # Shell script launcher
└── Run Application.command  # Double-click launcher
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
\
**If you see missing macOS binaries or version mismatches:**

1. Ensure you're using the official CRAN repository and update your lockfile:

```r
options(repos = c(CRAN = "https://cran.r-project.org"))
renv::update()
renv::snapshot()
```

2. Then re-run the `build_mac()` command. If the problem persists, wait a short time (CRAN mirrors and PACKAGES metadata may be temporarily out of sync) or pin the package version in `renv.lock`.

### "App is damaged and can't be opened" (macOS)

When distributing DMG files via the web, macOS adds a quarantine attribute that triggers Gatekeeper warnings. Users may see "App is damaged" or "can't be opened because Apple cannot check it for malicious software."

**For users downloading the app:**

Option 1 - Right-click to open:
1. Right-click (or Control-click) the app
2. Select "Open" from the context menu
3. Click "Open" in the dialog

Option 2 - Remove quarantine attribute:
```bash
xattr -dr com.apple.quarantine ~/Downloads/YourApp.dmg
# Or after mounting:
xattr -dr com.apple.quarantine /Volumes/YourApp/YourApp.app
```

**For developers distributing the app:**

The proper solution is to code sign and notarize your app with an Apple Developer ID ($99/year). This is beyond the scope of portR, but you can sign the app after building:

```bash
# Sign the app (requires Apple Developer ID)
codesign --force --deep --sign "Developer ID Application: Your Name" YourApp.app

# Create a signed DMG
hdiutil create -volname "YourApp" -srcfolder YourApp.app -ov -format UDZO YourApp.dmg
codesign --sign "Developer ID Application: Your Name" YourApp.dmg

# Notarize (recommended for distribution)
xcrun notarytool submit YourApp.dmg --apple-id your@email.com --team-id TEAMID --wait
xcrun stapler staple YourApp.dmg
```

## License

MIT
