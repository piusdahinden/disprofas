cran-comments
================

<!-- cran-comments.md is generated from cran-comments.Rmd. -->

## New submission

This patch comes with the following changes:

- A few test cases checking function output were modified because errors
  occurred with the current R development environment.
- References to mztia list elements for the output by the print()
  function were corrected.
- The deprecated function get_hotellings() has been removed for good.

## Test environments

- Local:
  - Platform windows-x86_64-w64-mingw32/x64, R 4.4.3 (2025-02-28)
- win-builder:
  - Platform windows-x86_64-w64-mingw32/x64, R 4.4.3 (2025-02-28)
  - Platform windows-x86_64-w64-mingw32/x64, R 4.3.3 (2024-02-29)
  - Platform windows-x86_64-w64-mingw32/x64, R 4.5.0 alpha (2025-03-22)
  - Platform macosx aarch64-apple-darwin20, R 4.4.2 (2024-10-31)
- R-hub v2:
  - Platform Ubuntu 24.04.2 LTS \[x86_64-pc-linux-gnu\], R-devel
    (2025-03-21)
  - Platform macOS Sonoma 14.7.4 \[aarch64-apple-darwin20\], R-devel
    (2025-03-17)
  - Platform macOS Ventura 13.7.4 \[x86_64-apple-darwin20\], R-devel
    (2025-03-17)
  - Platform Windows Server 2022 x64 \[x86_64-w64-mingw32\], R-devel
    (2025-03-22)
  - Platform Fedora Linux 38 \[x86_64-pc-linux-gnu\], R-devel
    (2025-03-22)
  - Platform Fedora Linux 43 \[x86_64-pc-linux-gnu\], R-devel
    (2025-03-22)
  - Platform Ubuntu 22.04.5 LTS \[x86_64-pc-linux-gnu\], R-devel
    (2025-03-22)
  - Platform Ubuntu 22.04.5 LTS \[x86_64-pc-linux-gnu\], R 4.5.0
    (2025-03-21)
  - Platform Ubuntu 22.04.5 LTS \[x86_64-pc-linux-gnu\], R 4.4.3
    (2025-02-28)

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies

There are no downstream dependencies for this package.
