cran-comments
================

<!-- cran-comments.md is generated from cran-comments.Rmd. -->

## First submission

This is the third iteration of the first submission. In this version I
have:

-   Changed the test parameters of function gep_by_nera(). It uses an
    iterative algorithm. It did not throw a warning on macOS 11.2.1
    arm64. I therefore changed the test parameters to be more
    restrictive.

## Test environments

-   Local windows-x86_64-w64-mingw32/x64 (R 4.1.2)
-   Win-builder windows-x86_64-w64-mingw32/x64 (R 4.0.5)
-   R-hub windows-x86_64-devel (r-devel)
-   R-hub ubuntu-gcc-release (r-release)
-   R-hub fedora-clang-devel (r-devel)
-   mac.R-project.org macosx-arm64 (r-release)

## R CMD check results

0 errors \| 0 warnings \| 1 note

There was one note because it is a new submission.

Possibly misspelled words in DESCRIPTION: the indicated words are names
or abbreviations. None of the words is misspelled.

## Downstream dependencies

There are no downstream dependencies for this package.
