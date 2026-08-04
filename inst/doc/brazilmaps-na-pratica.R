## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.width = 7,
  fig.height = 4,
  dpi = 96,
  dev.args = list(type = "cairo-png")
)

old_ctype <- Sys.getlocale("LC_CTYPE")
if (
  .Platform$OS.type == "windows" &&
    old_ctype %in% c("C", "POSIX")
) {
  suppressWarnings(
    try(Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.utf8"), silent = TRUE)
  )
}

## ----packages-----------------------------------------------------------------
library(brazilmaps)
library(ggplot2)

## ----geographic-levels--------------------------------------------------------
levels <- c(
  "country", "region", "state",
  "intermediate_region", "immediate_region",
  "municipality", "mesoregion", "microregion",
  "state_hex", "state_region"
)

data.frame(level = levels)

## ----state-object-------------------------------------------------------------
states <- get_brmap("state")

class(states)
sf::st_crs(states)$input
names(states)

## ----regions-map--------------------------------------------------------------
regions <- get_brmap("region")

plot_brmap(
  regions,
  fill_by = "name",
  border_colour = "white",
  border_linewidth = 0.5
) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Grandes regiões do Brasil",
    fill = NULL
  )

## ----filters------------------------------------------------------------------
pernambuco <- get_brmap(
  "municipality",
  filters = list(region = 2, state = 26)
)

unique(
  sf::st_drop_geometry(pernambuco)[c("region_code", "state_code")]
)

## ----pernambuco-map, fig.height=3.8-------------------------------------------
pe_state <- get_brmap("state", filters = list(state = 26))

ggplot() +
  geom_sf(
    data = pernambuco,
    fill = "#d9ecf2",
    colour = "white",
    linewidth = 0.12
  ) +
  geom_sf(
    data = pe_state,
    fill = NA,
    colour = "#174a5b",
    linewidth = 0.65
  ) +
  labs(title = "Municípios de Pernambuco") +
  theme_brmap()

## ----immediate-filter---------------------------------------------------------
recife_hierarchy <- get_dtb(name = "Recife")
recife_hierarchy[
  recife_hierarchy$level == "municipality",
  c("municipality_name", "immediate_region_code", "immediate_region_name")
]

recife_immediate <- get_brmap(
  "municipality",
  filters = list(
    immediate_region =
      recife_hierarchy$immediate_region_code[
        recife_hierarchy$level == "municipality"
      ][1]
  )
)

## ----immediate-map, fig.height=3.8--------------------------------------------
plot_brmap(
  recife_immediate,
  fill = "#f4c95d",
  border_colour = "white",
  border_linewidth = 0.25
) +
  labs(title = "Região geográfica imediata do Recife")

## ----state-indicator----------------------------------------------------------
data("gini2015")

plot_brmap(
  states,
  data = gini2015,
  by = c("state_code" = "cod"),
  fill_by = "gini",
  border_colour = "white",
  border_linewidth = 0.3
) +
  scale_fill_viridis_c(
    option = "C",
    direction = -1,
    na.value = "grey90"
  ) +
  labs(
    title = "Índice de Gini por unidade da federação — 2015",
    fill = "Gini"
  )

## ----municipality-indicator---------------------------------------------------
data("pop2017")

pe_population <- join_brmap(
  get_brmap(
    "municipality",
    year = 2023,
    filters = list(state = 26)
  ),
  pop2017,
  by = c("municipality_code" = "mun")
)

inherits(pe_population, "sf")

## ----population-map, fig.height=2.8-------------------------------------------
plot_brmap(
  pe_population,
  fill_by = "pop2017",
  border_colour = "white",
  border_linewidth = 0.12
) +
  scale_fill_viridis_c(
    trans = "log10",
    labels = function(x) {
      format(
        x,
        big.mark = ".",
        decimal.mark = ",",
        scientific = FALSE,
        trim = TRUE
      )
    },
    na.value = "grey90"
  ) +
  labs(
    title = "População municipal de Pernambuco — 2017",
    subtitle = "Escala logarítmica",
    fill = "Habitantes"
  )

## ----hierarchy-count----------------------------------------------------------
municipality_state <- get_dtb_levels(c("municipality", "state"))

municipality_count <- aggregate(
  municipality_code ~ state_code,
  data = municipality_state,
  FUN = length
)
names(municipality_count)[2] <- "n_municipalities"

states_with_count <- join_brmap(
  states,
  municipality_count,
  by = "state_code"
)

## ----hierarchy-count-map------------------------------------------------------
plot_brmap(
  states_with_count,
  fill_by = "n_municipalities",
  border_colour = "white",
  border_linewidth = 0.3
) +
  scale_fill_viridis_c(option = "B", direction = -1) +
  labs(
    title = "Número de municípios por unidade da federação",
    fill = "Municípios"
  )

## ----cartograms-map, fig.width=8, fig.height=3.8------------------------------
state_attributes <- sf::st_drop_geometry(states)[
  c("state_code", "state_abbreviation")
]

state_hex <- join_brmap(
  get_brmap("state_hex"),
  state_attributes,
  by = "state_code"
)
state_hex$cartogram <- "Hexagonal"

state_region <- join_brmap(
  get_brmap("state_region"),
  state_attributes,
  by = "state_code"
)
state_region$cartogram <- "Agrupado por região"

state_cartograms <- rbind(state_hex, state_region)
state_cartograms <- join_brmap(
  state_cartograms,
  gini2015,
  by = c("state_code" = "cod")
)

cartogram_labels <- suppressWarnings(
  sf::st_point_on_surface(state_cartograms)
)
label_coordinates <- sf::st_coordinates(cartogram_labels)
cartogram_labels$x <- label_coordinates[, "X"]
cartogram_labels$y <- label_coordinates[, "Y"]
cartogram_labels <- sf::st_drop_geometry(cartogram_labels)

ggplot(state_cartograms) +
  geom_sf(
    aes(fill = gini),
    colour = "white",
    linewidth = 0.5
  ) +
  geom_text(
    data = cartogram_labels,
    aes(x = x, y = y, label = state_abbreviation),
    colour = "grey15",
    fontface = "bold",
    size = 2.1
  ) +
  facet_wrap(vars(cartogram), nrow = 1) +
  scale_fill_viridis_c(
    option = "C",
    direction = -1,
    na.value = "grey90"
  ) +
  labs(
    title = "Índice de Gini em dois cartogramas estaduais",
    fill = "Gini"
  ) +
  theme_brmap() +
  theme(
    strip.text = element_text(face = "bold"),
    panel.spacing = grid::unit(0.7, "lines")
  )

## ----customize----------------------------------------------------------------
map <- plot_brmap(
  get_brmap("state", filters = list(region = 4)),
  fill = "#92c5de",
  border_colour = "#1f4e5f",
  border_linewidth = 0.45
) +
  labs(
    title = "Região Sul",
    subtitle = "Malhas locais e simplificadas do brazilmaps",
    caption = "Sistema de referência: SIRGAS 2000"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(colour = "grey40")
  )

map

## ----export, eval=FALSE-------------------------------------------------------
# ggsave(
#   "regiao-sul.png",
#   plot = map,
#   width = 8,
#   height = 6,
#   dpi = 300
# )

## ----editions-----------------------------------------------------------------
brmap_editions()[c("year", "n_features")]

goias_2000 <- get_brmap(
  "municipality",
  year = 2000,
  filters = list(state = 52)
)

## ----restore-locale, include=FALSE--------------------------------------------
if (!identical(Sys.getlocale("LC_CTYPE"), old_ctype)) {
  suppressWarnings(Sys.setlocale("LC_CTYPE", old_ctype))
}

