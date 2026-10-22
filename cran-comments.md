## Submission

This is a new submission.

## Test environments

* local: macOS (aarch64), R 4.5.2
* GitHub Actions: Ubuntu (R-devel, R-release and R 4.4.0, the minimum
  version), Windows (R-release), macOS (R-release)

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.

## Notes

* Possibly misspelled words in DESCRIPTION: FCA and SFCA are the method
  abbreviations used in the literature (floating catchment area); Delamater,
  Luo, Qi, Sternberg and Zou are authors of the cited methods.
* The example data in `data/` and `inst/extdata/` are openly licensed
  third-party data (Open Government Licence v3.0 and Creative Commons
  Attribution); `inst/COPYRIGHTS` lists each source and its attribution, as
  declared in the Copyright field of DESCRIPTION.
* To keep check time short, the slowest test blocks use `skip_on_cran()` and
  the bootstrap example is in `\donttest{}`; both run in our continuous
  integration.
