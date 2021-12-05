cran-comments
================

<!-- cran-comments.md is generated from cran-comments.Rmd. -->

## First submission

This is the second iteration of the first submission. In this version I
have:

-   Removed the example from one of the unexported functions.
-   Unwrapped examples that were wrapped by \\dontrun{} that are
    executable in \< 5 sec.
-   Catching errors from examples that show possible errors by aid of
    tryCatch() instead of wrapping the code by \\dontrun{}.
-   Listing the package that is used only in an example in ‘Suggests’
    and wrapping the example by if(requireNamespace(“pkgname”)){}
    instead of by \\dontrun{}.

## Test environments

-   Local windows-x86_64-w64-mingw32/x64 (R 4.1.2)
-   Win-builder windows-x86_64-w64-mingw32/x64 (R 4.0.5)
-   R-hub windows-x86_64-devel (r-devel)
-   R-hub ubuntu-gcc-release (r-release)
-   R-hub fedora-clang-devel (r-devel)

## R CMD check results

0 errors \| 0 warnings \| 1 note

There was one note because it is a new submission.

Possibly misspelled words in DESCRIPTION: the indicated words are names
or abbreviations. None of the words is misspelled.

## Downstream dependencies

There are no downstream dependencies for this package.
