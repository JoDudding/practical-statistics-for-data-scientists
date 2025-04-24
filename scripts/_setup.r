#-------------------------------------------------------------------------------
#' _setup.r
#-------------------------------------------------------------------------------
#' jo dudding
#' April 2025
#' common setup
#-------------------------------------------------------------------------------

library(tidyverse)
library(glue)
library(scales)
library(cli)
library(tidymodels)
#library(janitor)


theme_set(
  theme_bw(base_size = 13) +
    theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.border = element_rect(fill = NA, colour = NA)
    )
)

# colour palette

jo <- function(n = 9, begin = 0.1, end = 0.85, ...) {

  viridisLite::plasma(n = n, begin = begin, end = end, ...)
}

jo_pal <- jo(9)

# show_col(jo_pal)

scale_fill_jo_d <- function(...) {
  scale_fill_viridis_d(option = 'plasma', begin = 0.3, end = 0.8, ...)
}

scale_fill_jo_c <- function(...) {
  scale_fill_viridis_c(option = 'plasma', begin = 0.3, end = 0.8, ...)
}

scale_colour_jo_d <- function(...) {
  scale_colour_viridis_d(option = 'plasma', begin = 0.3, end = 0.8, ...)
}

scale_colour_jo_c <- function(...) {
  scale_colour_viridis_c(option = 'plasma', begin = 0.3, end = 0.8, ...)
}

purple <- '#6800A8FF'
chocolate <- '#502A00FF'
orange <- '#F9973FFF'
pink <- '#C5407EFF'

purple_pal <- grDevices::colorRampPalette(c(purple, '#E1CEFFFF'))

#show_col(purple_pal(9))

orange_pal <- grDevices::colorRampPalette(c(chocolate, orange, '#FFD6C1FF'))

#show_col(orange_pal(9))

# change default colours etc for geoms ------------------------------------

base_size <- 13

update_geom_defaults("col", list(fill = purple, colour = NA))
update_geom_defaults("bar", list(fill = purple, colour = NA))
update_geom_defaults("rect", list(fill = purple, colour = NA))
update_geom_defaults("violin", list(fill = purple, colour = purple,
  alpha = 0.5))
update_geom_defaults("boxplot", list(fill = purple, colour = purple,
  alpha = 0.5))
update_geom_defaults("dotplot", list(fill = purple, colour = NA))
update_geom_defaults("point", list(colour = purple))
update_geom_defaults("line", list(colour = purple))
update_geom_defaults("step", list(colour = purple))
update_geom_defaults("path", list(colour = purple))
update_geom_defaults("text", list(lineheight = 0.85,
  size = base_size / .pt * 0.9, family = "Montserrat"))
update_geom_defaults("label", list(lineheight = 0.85,
  size = base_size / .pt * 0.9, family = "Montserrat"))
update_geom_defaults("vline", list(colour = jo_pal[9], linetype = 3))
update_geom_defaults("hline", list(colour = jo_pal[9], linetype = 3))
update_geom_defaults("abline", list(colour = jo_pal[9], linetype = 3))
update_geom_defaults("smooth", list(fill = jo_pal[9], colour = orange,
  alpha = 0.5))

