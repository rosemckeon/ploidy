# how to plot the landscape
qplot(
  X + .5,
  Y + .5,
  data = sim$data %>% filter(gen == 0, sim == 1),
  geom = "point"
) + scale_x_continuous(
  breaks = seq(0, 40, by = 10),
  limits = c(0, 40),
  minor_breaks = 0:40
) + scale_y_continuous(
  breaks = seq(0, 40, by = 10),
  limits = c(0, 40),
  minor_breaks = 0:40
) + theme_classic() + theme(
  panel.grid.major = element_line(
    size = .5,
    linetype = "solid",
    colour = "gray"
  ),
  panel.grid.minor = element_line(
    size = .25,
    linetype = "solid",
    colour = "gray"
  )
)

# or...
qplot(
  X, Y,
  data = sim$data %>% filter(gen == 0, sim == 1),
  geom = "count",
  facets = ~gen
) + theme_classic() + theme(
  legend.position = "top"
)