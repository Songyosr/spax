## Submission

This is a new submission.

## Test environments

* local: macOS 26 (aarch64), R 4.5.2
* GitHub Actions: Ubuntu (R-devel, R-release 4.6.1 and R 4.4.0, the minimum
  version), Windows (R-release 4.6.1), macOS (R-release 4.6.1)
* win-builder: R-release 4.6.1 and R-devel (2026-10-22 r90748); each 1 NOTE
  (new submission, possibly misspelled words), check time 539 s and 543 s

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.

## Notes

* Possibly misspelled words in DESCRIPTION: FCA and SFCA are the method
  abbreviations used in the literature (floating catchment area); Delamater,
  Luo, Qi, Sternberg and Zou are authors of the cited methods, and "et al." is
  part of a citation.
* The Cornwall example data in `data/` and `inst/extdata/` are openly licensed
  third-party data (Open Government Licence v3.0 and Creative Commons
  Attribution); `inst/COPYRIGHTS` lists each source and its attribution, as
  declared in the Copyright field of DESCRIPTION.
* To keep check time short, the slowest test blocks use `skip_on_cran()` and
  the bootstrap example is in `\donttest{}`; both run in our continuous
  integration.
