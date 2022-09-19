library(gt)
library(tidyverse)
library(glue)
library(scales)
library(paletteer)
library(omsvg)

# Let's make a table that uses the `sza` dataset (and let's throw
# everything we can at gt). First, get some SVGs for the table.
# We can use Line Awesome icons via the {omsvg} package. Info on
# icons at: `omsvg::info_lineawesome()`
svg_sun   <- omsvg::SVG_la("sun")
svg_globe <- omsvg::SVG_la("globe")
svg_clock <- omsvg::SVG_la("clock")

# Now let's get a color palette from the {paletteer} package.
# The one we want is {ggsci}'s `orange_material`.
orange_material_palette <-
  paletteer::paletteer_d(palette = "ggsci::orange_material") %>%
  as.character()

c(
  "#FFF3E0FF", "#FFE0B2FF", "#FFCC80FF", "#FFB74DFF", "#FFA726FF",
  "#FF9800FF", "#FB8C00FF", "#F57C00FF", "#EF6C00FF", "#E65100FF"
)

# The dataset we're using (sza) has data at multiple northern
# latitudes. We'll define one latitude to use for the table
# (acceptable values are `20`, `30`, `40`, and `50`)
lat_n <- 20

# Now, let's use a combination of dplyr/tidyr code
# to develop the input table and a lot of gt code to
# make a table of solar zenith angles
sza %>%
  dplyr::filter(latitude == lat_n) %>%  ####
  dplyr::select(-latitude) %>%          # All of this dplyr and
  dplyr::filter(!is.na(sza)) %>%        # tidyr work whips the
  tidyr::pivot_wider(                   # input table into shape.
    names_from = tst,                   ####
    names_sort = TRUE,
    values_from = sza
  ) %>%
  gt(rowname_col = "month") %>%   # This defines the `month` column as the stub
  sub_missing(
    columns = everything(),   # All NA values become
    missing_text = ""         # empty strings/cells
  ) %>%
  data_color(
    columns = everything(),               #
    colors = scales::col_numeric(         # The `data_color()` function can flexibly
      palette = orange_material_palette,  # color text or the backgrounds of cells
      domain = c(0, 90),                  # with numeric data
      na.color = "#FFFFFF"                #
    )
  ) %>%
  tab_stubhead(label = md(glue("month  \n({lat_n}&deg;N)"))) %>%  # Markdown used here
  tab_spanner(label = "hour", columns = everything()) %>%
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
  opt_align_table_header(align = "left") %>%   # Aligns the header to the left
  opt_all_caps() %>%                           # Cap-type styling for table labels
  opt_table_font(font = google_font("Karla")) %>%
  tab_options(
    heading.title.font.size = px(24),          # The options inside of `tab_options()`
    heading.subtitle.font.size = px(14),       # allow you to radically modify
    heading.border.bottom.style = "none",      # the look of the table; there
    column_labels.border.top.style = "none",   # are dozens upon dozens
    column_labels.font.size = "smaller",       # of useful options
    table.font.size = "smaller",               # in that
    data_row.padding = px(4),                  # fn
    stub.border.color = "lightblue",
    column_labels.border.bottom.color = "gray90",
    table.border.bottom.color = "gray90",
    table_body.border.bottom.color = "gray90",
    table.border.top.color = "gray90"
  ) %>%
  cols_width(
    1 ~ px(50),
    everything() ~ px(45)
  )
  