rm(list=ls())
library(tidyverse)
theme_colors <- list(
  white = "#fafafa",
  orange = "#ec8216",
  orange_faded = "#d7c7b9",
  grey = "#d4d8d9",
  black = "#000000"
)

all_counts <- readRDS("documents/presentation/data/selfing_counts_1.rds") %>%
  filter(dist_lvl != "0.04") %>%
  filter(selfing_prob == 1)

# Now make a useful facet plot
all_counts %>% ggplot(
  mapping = aes(
    x = gen,
    y = n,
    fill = ploidy,
    colour = ploidy
  )
) +
  theme_classic() +
  xlab("Time (generations)") +
  ylab("Population size (N)") +
  geom_area() +
  facet_wrap(~ dist_lvl) +
  scale_colour_manual(
    name = "",
    values = c(
      theme_colors$black,
      theme_colors$orange_faded,
      theme_colors$orange
      )
  )  +
  scale_fill_manual(
    name = "",
    values = c(
      theme_colors$black,
      theme_colors$orange_faded,
      theme_colors$orange
    )
  ) +
  scale_x_continuous(breaks = c(0, 100, 200)) +
  scale_y_continuous(breaks = c(0, 450, 900)) +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "#fafafa"),
    strip.background = element_rect(
      colour = "#d4d8d9",
      fill = "#d4d8d9"
    ),
    strip.text.x = element_text(
      face = "bold",
      size = 20
    ),
    plot.background = element_rect(
      fill = "#fafafa"
    ),
    legend.text = element_text(
      size = 30,
      margin = margin(r = 20, l = 2, unit = "pt")
    ),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.caption = element_text(size = 30)
  ) +
  labs(
    caption = "\nProbability of disturbance increases from left to right."
  )

# export plot as file
ggsave(
  paste0("selfing-ON-disturbance-1.pdf"),
  path = "documents/presentation/plots/", device = "pdf",
  width = 11.69, height = 8.27, units = "in",
  dpi = "retina",
  title = "Reversal of Selfing Inhibition and Disturbance"
)

null <- readRDS("documents/presentation/data/selfing_counts_1.rds") %>%
  filter(dist_lvl == 0) %>%
  filter(selfing_prob == 0)


null %>% ggplot(
  mapping = aes(
    x = gen,
    y = n,
    fill = ploidy,
    colour = ploidy
  )
) +
  theme_classic() +
  xlab("Time (generations)") +
  ylab("Population size (N)") +
  geom_area() +
  scale_colour_manual(
    name = "",
    values = c(
      theme_colors$black,
      theme_colors$orange_faded,
      theme_colors$orange
    )
  )  +
  scale_fill_manual(
    name = "",
    values = c(
      theme_colors$black,
      theme_colors$orange_faded,
      theme_colors$orange
    )
  ) +
  scale_x_continuous(breaks = c(0, 100, 200)) +
  scale_y_continuous(breaks = c(0, 450, 900)) +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "#fafafa"),
    strip.background = element_rect(
      colour = "#d4d8d9",
      fill = "#d4d8d9"
    ),
    strip.text.x = element_text(
      face = "bold",
      size = 20
    ),
    plot.background = element_rect(
      fill = "#fafafa"
    ),
    legend.text = element_text(
      size = 30,
      margin = margin(r = 20, l = 2, unit = "pt")
    ),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.caption = element_text(size = 25)
  ) +
  labs(
    caption = "\nWhole genome duplication occurs at a rate of 0.01\nThere is no cost/benefit to being polyploid.\nThere is no disturbance."
  )

# export plot as file
ggsave(
  paste0("selfing_null-1.pdf"),
  path = "documents/presentation/plots/", device = "pdf",
  width = 11.69, height = 8.27, units = "in",
  dpi = "retina",
  title = "Baseline - No Disturbance - No Costs/Benefits"
)


benefit <- readRDS("documents/presentation/data/selfing_counts_1.rds") %>%
  filter(dist_lvl == 0) %>%
  filter(selfing_prob == 1)

benefit %>% ggplot(
  mapping = aes(
    x = gen,
    y = n,
    fill = ploidy,
    colour = ploidy
  )
) +
  theme_classic() +
  xlab("Time (generations)") +
  ylab("Population size (N)") +
  geom_area() +
  scale_colour_manual(
    name = "",
    values = c(
      theme_colors$black,
      theme_colors$orange_faded,
      theme_colors$orange
    )
  )  +
  scale_fill_manual(
    name = "",
    values = c(
      theme_colors$black,
      theme_colors$orange_faded,
      theme_colors$orange
    )
  ) +
  scale_x_continuous(breaks = c(0, 100, 200)) +
  scale_y_continuous(breaks = c(0, 450, 900)) +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "#fafafa"),
    strip.background = element_rect(
      colour = "#d4d8d9",
      fill = "#d4d8d9"
    ),
    strip.text.x = element_text(
      face = "bold",
      size = 20
    ),
    plot.background = element_rect(
      fill = "#fafafa"
    ),
    legend.text = element_text(
      size = 30,
      margin = margin(r = 20, l = 2, unit = "pt")
    ),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.caption = element_text(size = 25)
  ) +
  labs(
    caption = "\n\nNow polyploids can self where diploids cannot.\nThere is still no disturbance."
  )

# export plot as file
ggsave(
  paste0("selfing_prob_ON-1.pdf"),
  path = "documents/presentation/plots", device = "pdf",
  width = 11.69, height = 8.27, units = "in",
  dpi = "retina",
  title = "Reversal of Selfing Inhibition"
)
