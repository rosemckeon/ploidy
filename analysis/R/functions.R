# functions for automated analysis on all data
plot_sim_pop_sizes <- function(
  name = NULL,
  run = NULL,
  data = NULL,
  colour = NULL,
  colour.name = NULL,
  colour.labels = NULL,
  colour.h = NULL,
  colour.l = NULL,
  exportpath = "analysis/plots/population-size/"
){
  stopifnot(
    !is.null(
      c(name, run, data, colour)
     ),
    is.numeric(run),
    is.character(
      c(name, colour)
    ),
    is.data.frame(data),
    colour %in% colnames(data),
    "sim" %in% colnames(data),
    "gen" %in% colnames(data)
  )

  # get counts by colour col
  data$colour <- data %>% pull(!!colour)
  data <- data %>%
    group_by(sim, gen, colour) %>%
    tally()

  # setup export info and open pdf file for export
  filename <- paste0(name, "-", run, "-", colour, ".pdf")
  pdf(paste0(exportpath, filename))

  # build the base plot
  gg <- qplot(
    as.numeric(gen),
    n,
    data = data,
    geom = "line",
    colour = as.factor(colour),
    facets = ~sim
  ) + theme_classic() + theme(
    legend.position = "top",
    strip.background = element_rect(
      colour = "#d1d1d1",
      fill = "#d1d1d1"
    )
  ) + labs(
    tag = paste(toupper(name), run, sep = " "),
    caption = "Population size over time."
  ) + xlab("Generation")

  # set up customisations
  # for legend title
  if(!is.null(colour.name)){
    stopifnot(
      is.character(colour.name)
    )
    gg + scale_colour_discrete(
      name = colour.name
    )
  }
  # for legend labels
  colour.levels <- data %>% pull(colour) %>% nlevels()
  if(!is.null(colour.labels)){
    stopifnot(
      is.character(colour.labels),
      length(colour.labels) == colour.levels
    )
    gg + scale_colour_discrete(
      labels = colour.labels
    )
  }
  # for colour hue
  if(!is.null(colour.h)){
    stopifnot(
      is.numeric(colour.h),
      length(colour.h) == 2,
      between(colour.h, 0, 360),
      colour.h[1] <= colour.h[2]
    )
    gg + scale_colour_discrete(
      h = colour.h
    )
  }
  # for colour luminance
  if(!is.null(colour.l)){
    stopifnot(
      is.numeric(colour.l),
      length(colour.l) == 1,
      between(colour.l, 0, 100)
    )
    gg + scale_colour_discrete(
      l = colour.l
    )
  }
  # output to pdf and close pdf file
  gg
  dev.off()
}
