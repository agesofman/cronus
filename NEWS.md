# cronus 0.1.2

* Function `mutate()` was renamed into `modify()`.

# cronus 0.1.1

* Fixed bug in `new_Progress()`.

# cronus 0.1.0

* Variables now have their own S3 classes.
* Created `Progress` and `ProgressList` classes.
* Database functions `get_*()` are now exported.
* Function `tidy_progress()` was renamed to `tidy_Qs_progress()`.
* A `mutate()` function was added for `Progress`, `ProgressList`.
* Function `calc_percentage()` is now divided in `cal_perc()` and `calc_cumperc()`.
* Removed lifecycle badge from roi.
* Method `Database` for function `read()` was replaced by the `Progress` method.
* Function `combine()` was moved from `agesofman` to `cronus`.
* Function `get_combinations()` was moved from `persephone` to `cronus` and renamed to `combinations()`.

# cronus 0.0.10

* Deleted CRAN files.

# cronus 0.0.9

* Changed github R-CMD-check workflows.

# cronus 0.0.8

* Fixed bug in vignettes.

# cronus 0.0.7

* Fixed bug in `get_path_demeter()`.

# cronus 0.0.6

* Fixed bug in default `dir` slot of the `Database` class.

# cronus 0.0.5

* Removed `rprojroot` dependency.

# cronus 0.0.4

* Fixed a bug in `download()` that resulted in cdl rasters not recognized as categorical.
* Fixed a bug in `dtri()` caused by NA values.
* Added minimum required version in `terra` dependency.

# cronus 0.0.3

* Added a `NEWS.md` file to track changes to the package.
* Improved vignettes.
* Added tests with `testthat`.
* Added Codecov test coverage with `covr`.
* Added `cran-comments.md`.
* Performed checks (R CMD, Build windows binary package, CRAN).
