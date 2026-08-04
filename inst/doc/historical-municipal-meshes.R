## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center"
)

## ----packages-----------------------------------------------------------------
library(brazilmaps)

## ----inventory----------------------------------------------------------------
inventory <- brmap_editions()
inventory[c("year", "n_features", "md5")]

## ----editions-----------------------------------------------------------------
mesh_2000 <- get_brmap("municipality", year = 2000)
mesh_2010 <- get_brmap("municipality", year = 2010)
mesh_current <- get_brmap("municipality")

c(
  `2000` = nrow(mesh_2000),
  `2010` = nrow(mesh_2010),
  current = nrow(mesh_current)
)

## ----compare, fig.width=7, fig.height=3.5-------------------------------------
go_2000 <- get_brmap(
  "municipality", year = 2000, filters = list(state = 52)
)
go_current <- get_brmap("municipality", filters = list(state = 52))

plot_brmap(go_2000) +
  ggplot2::labs(title = "Municipal mesh of Goiás — 2000")

