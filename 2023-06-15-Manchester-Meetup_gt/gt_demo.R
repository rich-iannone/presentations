library(gt)
library(tidyverse)
library(glue)
library(omsvg)
library(paletteer)


## The Basics


# Let's print out the `exibble` dataset and see what it looks like
exibble

# Now lets introduce `exibble` to the `gt()` function.
exibble |> gt()

# We can modify the layout and create a stub; a stub is a
# special column that contains row labels/names.
exibble |> gt(rowname_col = "row")

# We can do some more structuring and create row groups
exibble |>
  gt(rowname_col = "row", groupname_col = "group")

### Formatting Values

# We can do basic number formatting, like showing a
# fixed number of decimal places
exibble |>
  dplyr::select(num) |>
  gt() |>
  fmt_number(
    columns = num,
    decimals = 3
  )

# A locale can be chosen in a `fmt_*()` function or be
# declared in the `gt()` call (over 500 locales supported)
exibble |>
  dplyr::select(num) |>
  gt(locale = "fr") |>
  fmt_number(
    columns = num,
    decimals = 3
  )

# We can format values in scientific notation and use
# currencies as well; NA values can be replaced
exibble |>
  dplyr::select(num, currency) |>
  gt() |>
  fmt_number(
    columns = num,
    n_sigfig = 2
  ) |>
  fmt_currency(
    columns = currency,
    currency = "EUR"
  ) |>
  sub_missing(missing_text = "")

### Column merging

# Here we use the `cols_merge()` function to merge the
# `open` & `close` columns together, and, the `low` & `high` columns
sp500 |>
  dplyr::slice(50:55) |>
  dplyr::select(-volume, -adj_close) |>
  gt() |>
  cols_merge(
    columns = c(open, close),
    pattern = "{1}&mdash;{2}"
  ) |>
  cols_merge(
    columns = c(low, high),
    pattern = "{1}&mdash;{2}"
  ) |>
  cols_label(
    open = "open/close",
    low = "low/high"
  )

# This is an example that uses a summarized version of
# the `pizzaplace` dataset; the `cols_merge_n_pct()` function
# merges the `n` and `frac` columns together
pizzaplace |>
  dplyr::group_by(name, type, price) |>
  dplyr::summarize(
    n = dplyr::n(),
    frac = n/nrow(pizzaplace),
    .groups = "drop"
  ) |>
  dplyr::arrange(type, dplyr::desc(n)) |>
  dplyr::group_by(type) |>
  dplyr::slice_head(n = 3) |>
  gt(
    rowname_col = "name",
    groupname_col = "type"
  ) |>
  fmt_currency(price) |>
  fmt_percent(frac) |>
  cols_merge_n_pct(
    col_n = n,
    col_pct = frac
  ) |>
  cols_label(
    n = md("*N* (%)"),
    price = "Price"
  ) |>
  tab_style(
    style = cell_text(font = "monospace"),
    locations = cells_stub()
  ) |>
  tab_stubhead(md("Cat. and  \nPizza Code")) |>
  tab_header(title = "Top 3 Pizzas Sold by Category in 2015") |>
  tab_options(table.width = px(512))


### Header and Footer


# Let's use a small portion of the gtcars dataset; a
# header part can be added to the table with `tab_header()` 
gtcars |>
  dplyr::select(mfr, model, msrp) |>
  dplyr::slice(1:5) |>
  gt() |>
  tab_header(
    title = md("Data listing from **gtcars**"),
    subtitle = md("`gtcars` is an R dataset")
  )

# We can add source notes to the footer of the table
gtcars |>
  dplyr::select(mfr, model, msrp) |>
  dplyr::slice(1:5) |>
  gt() |>
  tab_source_note(source_note = "From edmunds.com")

# Footnotes can be added with multiple calls of
# `tab_footnote()`; here we target different column
# labels and a value in a body cell
sza |>
  dplyr::filter(
    latitude == 20 &
      month == "jan" &
      !is.na(sza)
  ) |>
  dplyr::select(-latitude, -month) |>
  gt() |>
  tab_footnote(
    footnote = md("SZA stands for *Solar Zenith Angle*."),
    locations = cells_column_labels(columns = sza)
  ) |>
  tab_footnote(
    footnote = "This is noon",
    locations = cells_body(columns = tst, rows = tst == "1200")
  ) |>
  tab_footnote(
    footnote = md("TST stands for *True Solar Time*."),
    locations = cells_column_labels(columns = tst)
  ) |>
  tab_footnote(
    footnote = "Higher Values indicate sun closer to horizon.",
    locations = cells_column_labels(columns = sza)
  )


###  Final example


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
  paletteer::paletteer_d(palette = "ggsci::orange_material") |>
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
sza |>
  dplyr::filter(latitude == lat_n) |>   ####
  dplyr::select(-latitude) |>           # These dplyr and tidyr statements
  dplyr::filter(!is.na(sza)) |>         # put the input table into the 
  tidyr::pivot_wider(                   # right shape for tabulation
    names_from = tst,                   ####
    names_sort = TRUE,
    values_from = sza
  ) |>
  gt(rowname_col = "month") |>      # This defines the `month` column as the stub
  sub_missing(missing_text = "") |> # All NA values become empty strings/cells
  data_color(               
    method = "numeric",                   # The `data_color()` function can flexibly
    palette = orange_material_palette,    # color text or the backgrounds of cells
    domain = c(0, 90),                    # with numeric data
    na_color = "#FFFFFF"
  ) |>
  tab_stubhead(label = md(glue("month  \n({lat_n}&deg;N)"))) |>
  tab_spanner(label = "hour", columns = everything()) |>
  tab_header(
    title = md(glue("**SZA** {svg_sun} {svg_globe} {svg_clock}")),
    subtitle = glue("Approximate Solar Zenith Angles at {lat_n} Degrees North, Throughout the Year")
  ) |>
  tab_source_note(
    md(
"
These calculations are made possible with the SZA formula.<br>
More details in <a href=\"https://en.wikipedia.org/wiki/Solar_zenith_angle\">
this Wikipedia article</a>.
")
  ) |>
  opt_align_table_header(align = "left") |>    # Aligns the header to the left
  opt_all_caps() |>                            # Cap-type styling for table labels
  opt_table_font(font = google_font("Karla")) |>
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
  ) |>
  cols_width(
    stub() ~ px(50),
    everything() ~ px(45)
  )
  