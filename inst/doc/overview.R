## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center"
)

## ----packages-----------------------------------------------------------------
library(brazilmaps)

## ----current-map--------------------------------------------------------------
states <- get_brmap("state")
states

## ----current-municipal--------------------------------------------------------
latest_year <- max(brmap_editions()$year)
latest <- get_brmap("municipality")
stopifnot(unique(latest$year) == latest_year)

## ----filters------------------------------------------------------------------
ms <- get_brmap(
  "municipality",
  filters = list(region = 5, state = 50)
)
unique(ms[c("region_code", "state_code")])

## ----plot, fig.width=7, fig.height=4------------------------------------------
data("gini2015")

plot_brmap(
  states,
  data = gini2015,
  by = c("state_code" = "cod"),
  fill_by = "gini"
)

## ----hierarchy----------------------------------------------------------------
get_dtb(name = "Campo Grande")
get_dtb_levels(
  c("municipality", "immediate_region"),
  filters = list(state = 50)
)

