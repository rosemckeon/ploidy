
# quick plot selection
qplot(
  as.numeric(gen),
  growth_rate,
  data = plants,
  geom = "jitter",
  facets = ~sim
) + geom_smooth(
  method = "lm"
) + scale_y_continuous(
  breaks = c(1, 1.5, 2),
  limits = c(1, 2)
) + theme_classic()

# allele values
qplot(
  as.numeric(gen),
  value,
  data = plants %>% unnest(),
  geom = "jitter",
  facets = ~as.factor(locus)
)
