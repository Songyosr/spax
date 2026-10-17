# Bind vector or matrix observations once to a declared output identity

Each named axis must contain the complete canonical ID set; unnamed axes
stay positional. Missing observations use NA values rather than omitted
IDs. Matrix orientation and dimensions are never guessed. A private
bound object can be passed by multistart/profile wrappers without
repeating ID matches; its identity must still agree with the receiving
problem and output.

## Usage

``` r
.bind_problem_target(problem, observed, output = "utilization")
```

## Details

Custom loss callbacks receive canonical observation order.
Observation-indexed auxiliary values in `loss_args` must already use
that order; arbitrary loss arguments are neither interpreted nor
silently rearranged here. Unannotated custom providers retain positional
numeric fitting for unnamed targets, but their fitted vectors are no
longer relabelled as facilities merely because lengths agree. Declare
axes and IDs for identity-safe names.
