# Compare calibrated AE problem fits on an identical target and loss

Builds a tidy comparison table over fits of *different models calibrated
against the same data*: same output name, same observed values and
observation mask, same loss arguments (and the same loss function when
the fits carry one). Reports fit quality and stability diagnostics side
by side; it does not pick a winner. Canonical observation identities
must also agree. This helper does not reorder independently prepared
fits. Legacy fits without identity metadata can still be compared to
each other, but not to fits with declared identity.

## Usage

``` r
.compare_problem_fits(fits)
```
