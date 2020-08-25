library(gt)
library(tidyverse)
library(glue)
library(scales)
library(paletteer)
library(omsvg)

# Install `omsvg` from GitHub with
# remotes::install_github("rich-iannone/omsvg")

#
# Prep for the table
#

# Get a few SVGs. These are Line Awesome icons via {omsvg};
# Info on icons at: `omsvg::info_lineawesome()`
svg_sun <- omsvg::SVG_la("sun")
svg_globe <- omsvg::SVG_la("globe")
svg_clock <- omsvg::SVG_la("clock")

# Get a palette from {paletteer}! The one we want is
# {ggsci}'s `orange_material`
orange_material_palette <-
  paletteer::paletteer_d(palette = "ggsci::orange_material") %>%
  as.character()

# Define latitude north for table
# (acceptable values are 20, 30, 40, and 50)
lat_n <- 20

#
# Create a gt table based on the `sza` dataset
#

sza %>%
  dplyr::filter(latitude == lat_n) %>%  ####
  dplyr::select(-latitude) %>%       # All of this dplyr and
  dplyr::filter(!is.na(sza)) %>%     # tidyr work whips the
  tidyr::pivot_wider(                # input table into shape.
    names_from = tst,                ####
    names_sort = TRUE,
    values_from = sza
  ) %>%
  gt(rowname_col = "month") %>%                               # This defines the `month` column as the stub
  fmt_missing(columns = everything(), missing_text = "") %>%  # All NA values become empty strings/cells
  data_color(
    columns = everything(),                     #
    colors = scales::col_numeric(               # The `data_color()` function can flexibly
      palette = orange_material_palette,        # color text or the backgrounds of cells
      domain = c(0, 90),                        # with numeric data
      na.color = "#FFFFFF"                      #
    )
  ) %>%
  tab_stubhead(label = md(glue("month  \n({lat_n}&deg;N)"))) %>%  # This is the stubhead label
  tab_spanner("hour", everything()) %>%
  tab_header(
    title = md(glue("**SZA** {svg_sun} {svg_globe} {svg_clock}")),
    subtitle = glue("Approximate Solar Zenith Angles at {lat_n} Degrees North, Throughout the Year")
  ) %>%
  tab_source_note(
    md(
"
These calculations are made possible with the SZA formula.<br>
More details in <a href=\"https://en.wikipedia.org/wiki/Solar_zenith_angle\">
this Wikipedia article</a>.
")
  ) %>%
  opt_align_table_header("left") %>%    # This aligns the title and subtitle to the left
  opt_all_caps() %>%                    # This puts column, stub, and stubhead labels in bold, smallcaps
  opt_table_font(font = google_font("IBM Plex Condensed")) %>%
  tab_options(
    heading.title.font.size = px(24),          # The options inside of `tab_options()`
    heading.border.bottom.style = "none",      # allow you to radically modify
    column_labels.border.top.style = "none",   # the look of the table; there
    column_labels.font.size = "smaller",       # are dozens upon dozens
    table.font.size = "smaller",               # of useful options
    data_row.padding = px(4)                   # in there
  ) %>%
  cols_width(
    1 ~ px(50),
    everything() ~ px(45)
  )

