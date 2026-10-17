# Evaluate the static Huff allocation and derived outputs at exogenous a

\`v0\` is the optional outside-option mass in the choice denominator
(\`v0 = 0\` is pure Huff; \`v0 \> 0\` is the aggregate conditional-logit
form with an outside option). \`allocation = "clm"\` returns the
normalized share; \`"huff_decay"\` multiplies that share by the decay
kernel once more, retaining the historical allocation. In both forms
\`outside_share\` describes the explicit choice outside option, not any
additional decay attenuation. A zero denominator (no opportunity and no
outside option) yields zero inside and outside shares. The returned
\`target\` is the exogenous attractiveness, so the map \`T(x) = a\` is
state-independent.

## Usage

``` r
.huff_state(a, plan, v0 = 0, allocation = c("clm", "huff_decay"))
```
