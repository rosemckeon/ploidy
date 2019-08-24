library(tidyverse)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

gvpath <- "documents/presentation/graphs/"

everything <- grViz(paste0(gvpath, "everything.gv")) %>%
  export_svg %>%
  charToRaw

everything %>% rsvg_pdf(paste0(gvpath, "everything.pdf"))
everything %>% rsvg_png(paste0(gvpath, "everything.png"))

simple <- grViz(paste0(gvpath, "not-so-much.gv")) %>%
  export_svg %>%
  charToRaw

simple %>% rsvg_pdf(paste0(gvpath, "not-so-much.pdf"))
simple %>% rsvg_png(paste0(gvpath, "not-so-much.png"))

simple1 <- grViz(paste0(gvpath, "not-so-much-1.gv")) %>%
  export_svg %>%
  charToRaw

simple1 %>% rsvg_pdf(paste0(gvpath, "not-so-much-1.pdf"))
simple1 %>% rsvg_png(paste0(gvpath, "not-so-much-1.png"))

simple2 <- grViz(paste0(gvpath, "not-so-much-2.gv")) %>%
  export_svg %>%
  charToRaw

simple2 %>% rsvg_pdf(paste0(gvpath, "not-so-much-2.pdf"))
simple2 %>% rsvg_png(paste0(gvpath, "not-so-much-2.png"))

simple3 <- grViz(paste0(gvpath, "not-so-much-3.gv")) %>%
  export_svg %>%
  charToRaw

simple3 %>% rsvg_pdf(paste0(gvpath, "not-so-much-3.pdf"))
simple3 %>% rsvg_png(paste0(gvpath, "not-so-much-3.png"))

simple4 <- grViz(paste0(gvpath, "not-so-much-4.gv")) %>%
  export_svg %>%
  charToRaw

simple4 %>% rsvg_pdf(paste0(gvpath, "not-so-much-4.pdf"))
simple4 %>% rsvg_png(paste0(gvpath, "not-so-much-4.png"))

simple5 <- grViz(paste0(gvpath, "not-so-much-5.gv")) %>%
  export_svg %>%
  charToRaw

simple5 %>% rsvg_pdf(paste0(gvpath, "not-so-much-5.pdf"))
simple5 %>% rsvg_png(paste0(gvpath, "not-so-much-5.png"))

simple6 <- grViz(paste0(gvpath, "not-so-much-complete.gv")) %>%
  export_svg %>%
  charToRaw

simple6 %>% rsvg_pdf(paste0(gvpath, "not-so-much-complete.pdf"))
simple6 %>% rsvg_png(paste0(gvpath, "not-so-much-complete.png"))

lifecycle <- grViz(paste0(gvpath, "life-cycle.gv")) %>%
  export_svg %>%
  charToRaw

lifecycle %>% rsvg_pdf(paste0(gvpath, "life-cycle.pdf"))
lifecycle %>% rsvg_png(
  paste0(gvpath, "life-cycle.png"),
  width = 1200
)

lifecycle <- grViz(paste0(gvpath, "life-cycle-simple.gv")) %>%
  export_svg %>%
  charToRaw

lifecycle %>% rsvg_pdf(paste0(gvpath, "life-cycle-simple.pdf"))
lifecycle %>% rsvg_png(
  paste0(gvpath, "life-cycle-simple.png"),
  width = 1200
)

