# DisturPloidy (v1.0.0)

**An Individual-Based Model (IBM) written in R.**

This model simulates plant populations over time in order to try and understand the prevalance of genome duplication (polyploidy) that we see in flowering plants (angiosperms). In particular it focuses on one specific question: *How does disturbance on a landscape affect the establishment of new polyploid plant species?*

## Installation

From your R console/script:

```R
install.packages("devtools")
library(devtools)
install_github("rozeykex/ploidy")
```

### If this repository is private

You'll need to setup a PAT token. See: `?install_github`.

## Basic Usage

From your R console/script run `disturploidy()` to run a simulation. See `?disturploidy` for details on model parameters.

## Output

By default, model data is stored to an environment object (every generation) which can be loaded on completion by doing `data(dploidy)` from your R console. This object is overwritten everytime you run `disturploidy()` but you can specify a `filename` and/or `logfilename` to store information more permanently. Alternatively you can set `return = T` to output the data straight to console or assign it to your own objects.

## Dependencies

The model requires Devtools, Tidyverse and Tictoc to run. Make sure you have them installed and loaded:

```
install.packages("devtools")
library(devtools)

install.packages("tidyverse")
library(tidyverse)

install.packages("tictoc")
library(tictoc)
```

## Analysis

Our analysis is not included in the Rbuild for the package, but it is available here in the GitHub repository. Feel free to clone the project and rummage through the `analysis` folder to look at the data we obtained.

### Running Simulations

Copy/edit the `analysis/simulations.R` file to set the model parameters for your replicates. Edit the `name` and `runs` objects at the top of the file to control the naming of the output data and log files.

From a unix terminal:

```BASH
Rscript analysis/simulations.R
```
Output will appear in `analysis/data/`, unless otherwise specified with `logfilepath`, see: `?disturploidy`.

---

This project was funded by [The Genetics Society](http://www.genetics.org.uk/) as a Summer Studentship in 2019.
