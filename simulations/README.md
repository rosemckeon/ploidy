# DisturPloidy Simulations

The R scripts in this folder define the simulations that we have run to gather the data stored in `./data/`. Data files are RDS format and can be read into R using `readRDS()`. Logs, showing the message output from the model for each simulation are held in `./data/logs/`.

## Loading the data

from R:

```
sim <- readRDS("simulations/data/quick-test-1.rds")
```

## Running these simulations for yourself

From RStudio you can simply open and run the script files, and in any R console you can use `source("path-to-file.R")`.

You can also set simulations running on a server/unix/mac machine without entering an R console. From terminal or a BASH script do `Rscript path-to-file.R`.

When you run simulations via `RScript` the directory the script is in acts as the working directory for R. Bear this in mind if you make edits or move data files etc. Output will appear in `./data/`, unless otherwise specified with `logfilepath`, see: `?disturploidy`.

## Data structure

Model output comes as a list. See an example dataset with `data(dploidy_demo)`. It looks like this:

```
$call
disturploidy(pop_size = 100, grid_size = 10, ploidy_growth_benefit = 0, 
    inbreeding_sensitivity = 0, germination_prob = 0.4, N_ovules = 25, 
    pollen_range = 10, fertilisation_prob = 0.5, uneven_matching_prob = 0.5, 
    selfing_polyploid_prob = 0, selfing_diploid_prob = 0, triploid_mum_prob = 0.5, 
    adult_survival_prob = 0.7, juvenile_selection_constant = 0.5, 
    seed_survival_prob = 0, disturbance_freq = 2, ploidy_prob = 0.5, 
    generations = generations, simulations = simulations, filepath = "simulations/data/", 
    filename = this_run, logfilepath = "simulations/data/logs/", 
    logfilename = this_run)

$time
$time$start
[1] "2019-08-03 20:46:22 UTC"

$time$end
[1] "2019-08-03 20:46:33 UTC"

$time$duration
Time difference of 10.91993 secs

$time$sim_duration
Time difference of 10.91993 secs


$R
[1] "R version 3.4.4 (2018-03-15)"

$disturploidy
[1] "DisturPloidy version 0.0.0006"

$notes
[1] "Dataframes contain all members of a generation that will face survival over the winter and go on to begin the next generation. Seedoutput and seedbank stored seperately so that fecundity can be calculated when dormancy exists. Seeds never have real genomes, instead they contain lineage details in $genome."

$data
$data$seedbank
# A tibble: 0 x 9
# … with 9 variables: ID <fct>, X <dbl>, Y <dbl>, life_stage <fct>, size <dbl>,
#   ploidy <dbl>, genome <list>, gen <int>, sim <fct>

$data$juveniles
# A tibble: 228 x 11
   ID        X     Y life_stage  size ploidy   gen sim   genome   growth_rate inbreeding
   <fct> <dbl> <dbl> <fct>      <dbl>  <dbl> <dbl> <fct> <list>         <dbl> <lgl>     
 1 0_1       1     0 1           1.46      2     1 1     <tibble…        1.46 FALSE     
 2 0_2       0     4 1           1.80      2     1 1     <tibble…        1.80 FALSE     
 3 0_3       0     5 1           1.41      2     1 1     <tibble…        1.41 FALSE     
 4 0_4       2     6 1           1.54      2     1 1     <tibble…        1.54 FALSE     
 5 0_5       9     3 1           1.68      2     1 1     <tibble…        1.68 FALSE     
 6 0_6       0     2 1           1.55      2     1 1     <tibble…        1.55 FALSE     
 7 0_7       6     8 1           1.78      2     1 1     <tibble…        1.78 FALSE     
 8 0_8       0     6 1           1.46      2     1 1     <tibble…        1.45 FALSE     
 9 0_9       3     3 1           1.39      2     1 1     <tibble…        1.39 FALSE     
10 0_10      6     7 1           1.45      2     1 1     <tibble…        1.45 FALSE     
# … with 218 more rows

$data$adults
# A tibble: 44 x 11
   ID        X     Y life_stage  size ploidy   gen sim   genome   growth_rate inbreeding
   <fct> <dbl> <dbl> <fct>      <dbl>  <dbl> <dbl> <fct> <list>         <dbl> <lgl>     
 1 0_20      0     6 2           2.57      2     2 1     <tibble…        1.60 FALSE     
 2 0_41      2     4 2           2.30      2     2 1     <tibble…        1.52 FALSE     
 3 0_6       0     2 2           2.4       2     2 1     <tibble…        1.55 FALSE     
 4 0_86      0     3 2           2.15      2     2 1     <tibble…        1.47 FALSE     
 5 0_91      0     7 2           2.45      2     2 1     <tibble…        1.57 FALSE     
 6 0_45      1     5 2           2.70      2     2 1     <tibble…        1.64 FALSE     
 7 0_51      1     8 2           2.03      2     2 1     <tibble…        1.43 FALSE     
 8 0_11      2     3 2           2.35      2     2 1     <tibble…        1.53 FALSE     
 9 0_32      3     0 2           2.65      2     2 1     <tibble…        1.63 FALSE     
10 0_60      3     2 2           2.22      2     2 1     <tibble…        1.49 FALSE     
# … with 34 more rows

$data$seedoutput
# A tibble: 279 x 9
   ID        X     Y life_stage  size ploidy genome              gen sim  
   <fct> <dbl> <dbl> <fct>      <int>  <dbl> <list>            <int> <fct>
 1 2_1      10     4 0              0      3 <tibble [1 × 13]>     2 1    
 2 2_2      10     4 0              0      4 <tibble [1 × 13]>     2 1    
 3 2_3      10     4 0              0      2 <tibble [1 × 13]>     2 1    
 4 2_4      10     4 0              0      4 <tibble [1 × 13]>     2 1    
 5 2_5      10     4 0              0      2 <tibble [1 × 13]>     2 1    
 6 2_6      10     7 0              0      2 <tibble [1 × 13]>     2 1    
 7 2_7      10     7 0              0      2 <tibble [1 × 13]>     2 1    
 8 2_8      10     7 0              0      2 <tibble [1 × 13]>     2 1    
 9 2_9      10     7 0              0      2 <tibble [1 × 13]>     2 1    
10 2_10     10     7 0              0      4 <tibble [1 × 13]>     2 1    
# … with 269 more rows

$data$disturbance
# A tibble: 4 x 3
    sim   gen occurred
  <int> <int> <lgl>   
1     1     2 FALSE   
2     1     3 TRUE    
3     1     4 TRUE    
4     1     5 FALSE   


$log
$log[[1]]
[1] "simulations/data/logs/quick-test-1-sim-1.txt"
```