#library(ggplot2)
library(hexSticker)

dtf <- data.frame(
  x = c(1,2,3,4),
  y = c(4,4,3,3),
  f = c("A", "A", "B", "B"),
  g = c("a", "b", "a", "b"))
tbl_theme <- gridExtra::ttheme_default(
  core = list(padding = grid::unit(c(2.5, 2.5), "mm")))
tbl <- gridExtra::tableGrob(dtf, theme = tbl_theme, rows = NULL)


#di1 <- dist_chisq(df = 8)
#p <- chart(di1) +
#  geom_funfill(fun = dfun(di1), from = 0, to = 16, fill = "cornsilk") +
#  geom_funfill(fun = dfun(di1), from = 16, to = 30, fill = "red")
#p <- p + theme_void() + theme_transparent()
#p
outfile <- "inst/figures/data.trame.png"
sticker(tbl, package = "data.trame", filename = outfile,
  s_x = 1, s_y = 0.8, s_width = 1.4, s_height = 1,
  p_size = 24, h_fill = "gray70", h_color = "gray40")
