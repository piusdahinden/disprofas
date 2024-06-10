cran-comments
================

<!-- cran-comments.md is generated from cran-comments.Rmd. -->

## New submission

This minor release comes with the following changes:

- The deprecated ggplot2 function aes_string() has been replaced by
  aes(). Variables containing column names as a character vector
  (var_name) are now called via .data\[\[var_name\]\]. In addition,
  ‘size’ has been replaced by ‘linewidth’ when used in the geom_line()
  function. Visible binding for global variables was added. The
  parameter theme(legend.position = c(x, y)) has been replaced by
  theme(legend.position = “inside”, legend.position.inside = c(x, y)).
- Tests were complemented to reach 100% coverage. On the other hand,
  some redundant tests were removed. Comparable actions were applied to
  the examples.
- Cyclomatic complexity was reduced by the creation of the new function
  check_point_location().
- The new function get_T2_one() for the one-sample T2 test was added.
  The function get_hotellings() performing the two-sample T2 test was
  renamed to get_T2_two(). Both functions calculate corresponding
  confidence intervals, i.e. the get_T2_two() function was complemented
  accordingly.
- An additional parameter for the option to remove NA cases was added to
  the functions mztia(), get_T2_one() and get_T2_two(). The treatment of
  NA/NaN cases by the get_profile_portion() function was amended. The
  bootstrap_f2() and the mimcr() functions now check for the presence of
  NA/NaN values in the provided data frames and halt execution if any
  are found with the recommendation to impute the missing data. The
  functions get_f1() and get_f2(), and thus also the functions f1() and
  f2(), return a message if the data frame handed in contains any
  NAs/NaNs.
- The parameter ‘nsf’ (number of significant figures) was added to the
  functions with the ‘bounds’ parameter, i.e. bootstrap_f2(), mimcr(),
  f1(), f2() and get_profile_portion(). The ‘nsf’ parameter allows to
  control how the test values are rounded before they are compared with
  the limits set by the ‘bounds’ parameter.
- The complete documentation was reviewed and harmonised, e.g. by the
  sharing of specific text elements between the various function
  descriptions.

## Test environments

- Local:
  - Platform windows-x86_64-w64-mingw32/x64, R 4.4.0
- win-builder:
  - Platform windows-x86_64-w64-mingw32/x64, R 4.3.3
  - Platform windows-x86_64-w64-mingw32/x64, R 4.4.0
  - Platform windows-x86_64-w64-mingw32/x64, R-devel
- R-hub v2:
  - Platform Fedora Linux 38, x86_64-pc-linux-gnu, R-devel
  - Ubuntu 22.04.4 LTS, x86_64-pc-linux-gnu, R-devel
  - macOS Ventura 13.6.7, x86_64-apple-darwin20, R-devel
  - macOS Sonoma 14.5, aarch64-apple-darwin20, R-devel
  - Windows Server 2022 x64 (build 20348), x86_64-w64-mingw32, R-devel
  - Ubuntu 22.04.4 LTS, x86_64-pc-linux-gnu, R 4.4.1 RC
  - Ubuntu 22.04.4 LTS, x86_64-pc-linux-gnu, R 4.4.0

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies

There are no downstream dependencies for this package.
