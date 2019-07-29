# DisturPloidy (v1.0.0)

**An Individual-Based Model (IBM) written in R.**

This model simulates plant populations over time in order to try and understand the prevalance of genome duplication (polyploidy) that we see in flowering plants (angiosperms). In particular it focuses on one specific question: *How does disturbance on a landscape affect the establishment of new polyploid plant species?*

## Installation

> This doesn't currently work

From your R console/script:

```R
install.packages("devtools")
library(devtools)
install_github("rozeykex/ploidy")
```

## Basic Usage

From your R console/script run `disturploidy()` to run a simulation. See `?disturploidy` for details on model parameters.

## Output

By default, model data is stored to an environment object (every generation) which can be loaded on completion by doing `data(dploidy)` from your R console. This object is overwritten everytime you run `disturploidy()` but you can specify a `filename` and/or `logfilename` to store information more permanently. Alternatively you can set `return = T` to output the data straight to console or assign it to your own objects.

An example dataset also comes with the package so you can look at our data for the null parameters with `data(dploidy_null)`. These data are gathered with stable parameter settings that allow populations to persist for many generations without tending towards extreme population growth/decline. Parameters also ensured no advantage/disadvantage to polyploids.

## Simulations

From a unix terminal:

```BASH
Rscript analysis/simulations.R
```

See this file for details of the parameters tested in our anlaysis, or run it for yourself.


---

This project was funded by [The Genetics Society](http://www.genetics.org.uk/) as a Summer Studentship in 2019.
