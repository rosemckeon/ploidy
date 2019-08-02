# DisturPloidy Simulations

The R scripts in this folder define the simulations that we have run to gather the data stored in `./data/`. Data files are RDS format and can be read into R using `readRDS()`. Logs, showing the message output from the model for each simulation are held in `./data/logs/`.

## Loading the data

```
sim <- readRDS("data/null-1.rds")
```

## Running these simulations for yourself

From RStudio you can simply open and run the script files, and in any R console you can use `source("path-to-file.R")`.

You can also set simulations running on a server/unix/mac machine without entering an R console. From terminal or a BASH script do `Rscript path-to-file.R`.

When you run simulations via `RScript` the directory the script is in acts as the working directory for R. Bear this in mind if you make edits or move data files etc. Output will appear in `./data/`, unless otherwise specified with `logfilepath`, see: `?disturploidy`.

## Data structure

Model output comes as a list containing the following items:

- `$call` Shows the funtion call used to run the simulations stored in the dataset.
- `$data` Is a nested data frame containing the individuals at the end of every generation (after reproduction and seed dispersal, but before winter survival). Each row represents a single individual with a nested genome. The structure of the data is as follows:

```
Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	999480 obs. of  11 variables:
 $ ID         : Factor w/ 465904 levels "0_1","0_10","0_100",..: 1 112 223 334 346 357 368 379 390 2 ...
 $ X          : num  24 7 30 13 13 9 0 7 26 26 ...
 $ Y          : num  33 37 37 34 0 33 22 34 0 22 ...
 $ life_stage : Factor w/ 3 levels "0","1","2": 1 1 1 1 1 1 1 1 1 1 ...
 $ size       : num  0 0 0 0 0 0 0 0 0 0 ...
 $ ploidy     : num  2 2 2 2 2 2 2 2 2 2 ...
 $ gen        : Factor w/ 101 levels "0","1","2","3",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ sim        : Factor w/ 1 level "1": 1 1 1 1 1 1 1 1 1 1 ...
 $ genome     :List of 999480
  ..$ :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	4 obs. of  3 variables:
  .. ..$ allele: num  1 1 2 2
  .. ..$ locus : int  1 2 1 2
  .. ..$ value : num  57.9 17.4 34.5 68.4
  .. [list output truncated]
 $ growth_rate: num  1.38 1.48 1.68 1.44 1.54 ...
 $ inbreeding : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
```

- `$time$start` System time at the beginning of the simulation.
- `$time$end` System time at the end of the simulation.
- `$software$R` R version used to run the simulation.
- `$software$DisturPloidy` DisturPloidy version used to run the simulation.
- `$logs` Log file paths associated with the simulations contained in `$data`.

