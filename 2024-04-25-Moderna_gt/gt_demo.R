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
exibble |> gt(rowname_col = "row", groupname_col = "group")

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
  gt(locale = "de") |>
  fmt_number(
    columns = num,
    decimals = 3
  )

# We can format values in scientific notation and use currencies as well;
# NA values can be replaced with `sub_missing()`
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


# Date and time formatting is fairly powerful, yet we can use an `easy mode`
# and supply presets for the date and time components; `text_transform()` allows
# for touch-ups if the formatting needs revisions
exibble |>
  dplyr::select(datetime) |>
  gt() |>
  fmt_datetime(
    date_style = "MMMEd",
    time_style = "Hm",
    locale = "fr-CA"
  ) |>
  text_transform(
    fn = function(x) gsub(" h ", "h", x)
  )



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
    frac = n / nrow(pizzaplace),
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




### Spanner Labels

towny |>
  dplyr::arrange(desc(population_2021)) |>
  dplyr::slice_head(n = 5) |>
  dplyr::select(
    name, latitude, longitude,
    ends_with("2016"), ends_with("2021")
  ) |>
  gt() |>
  tab_spanner(
    label = "Population",
    columns = starts_with("pop")
  ) |>
  tab_spanner(
    label = "Density",
    columns = starts_with("den")
  ) |>
  tab_spanner(
    label = md("*Location*"),
    columns = ends_with("itude"),
    id = "loc"
  ) |>
  cols_label_with(
    fn = function(x) gsub(".*?_", "", x)
  ) |>
  fmt_integer(columns = starts_with("population")) |>
  fmt_number(columns = starts_with("density")) |>
  tab_header(
    title = "Five Largest Municipalities in Ontario"
  ) |>
  opt_align_table_header(align = "left")

towny |>
  dplyr::select(
    name, ends_with("2001"), ends_with("2006"), matches("2001_2006")
  ) |>
  dplyr::filter(population_2001 > 100000) |>
  dplyr::arrange(desc(pop_change_2001_2006_pct)) |>
  dplyr::slice_head(n = 10) |>
  gt() |>
  fmt_integer() |>
  fmt_percent(columns = matches("change"), decimals = 1) |>
  tab_spanner(
    label = "Population",
    columns = starts_with("population")
  ) |>
  tab_spanner(
    label = "Density, {{*persons* km^-2}}",
    columns = starts_with("density")
  ) |>
  cols_label(
    ends_with("01") ~ "2001",
    ends_with("06") ~ "2006",
    matches("change") ~ md("Population Change,<br>2001 to 2006")
  ) |>
  cols_width(everything() ~ px(120))



### Units Notation

# "m/s" and "m / s" -> "m/s"
# 
# "m s^-1" will appear with the "-1" exponent intact
# 
# "m /s" gives the the same result, as "/<unit>" is equivalent to "<unit>^-1"
# 
# "E_h" will render an "E" with the "h" subscript
# 
# "t_i^2.5" provides a t with an "i" subscript and a "2.5" exponent
# 
# "m[_0^2]" will use overstriking to set both scripts vertically
# 
# "g/L %C6H12O6%" uses a chemical formula (enclosed in a pair of "%" characters)
#   as a unit partial, and the formula will render correctly with subscripted
#   numbers
# 
# Common units that are difficult to write using ASCII text may be implicitly
#   converted to the correct characters (e.g., the "u" in "ug", "um", "uL",
#   and "umol" will be converted to the Greek mu symbol; "degC" and "degF" will
#   render a degree sign before the temperature unit)
# 
# We can transform shorthand symbol/unit names enclosed in ":"
#   (e.g., ":angstrom:", ":ohm:", etc.) into proper symbols
# 
# Greek letters can added by enclosing the letter name in ":"; you can use
#   lowercase letters (e.g., ":beta:", ":sigma:", etc.) and uppercase letters
#   too (e.g., ":Alpha:", ":Zeta:", etc.)
#
# The components of a unit (unit name, subscript, and exponent) can be fully
#   or partially italicized/emboldened by surrounding text with "*" or "**"


# We can format a column of text that has measurement units with `fmt_units()`
illness |>
  gt() |>
  fmt_units(columns = units) |>
  sub_missing(columns = -starts_with("norm")) |>
  sub_missing(columns = c(starts_with("norm"), units), missing_text = "") |>
  sub_large_vals(rows = test == "MYO", threshold = 1200) |>
  fmt_number(
    decimals = 2,
    drop_trailing_zeros = TRUE
  ) |>
  tab_header(title = "Laboratory Findings for the YF Patient") |>
  tab_spanner(label = "Day", columns = starts_with("day")) |>
  cols_label_with(fn = ~ gsub("day_", "", .)) |>
  cols_merge_range(col_begin = norm_l, col_end = norm_u) |>
  cols_label(
    starts_with("norm") ~ "Normal Range",
    test ~ "Test",
    units ~ "Units"
  ) |>
  cols_width(
    starts_with("day") ~ px(80),
    everything() ~ px(120)
  ) |>
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = starts_with("day"))
  ) |>
  tab_style(
    style = cell_fill(color = "aliceblue"),
    locations = cells_body(columns = c(test, units))
  ) |>
  opt_vertical_padding(scale = 0.4) |>
  opt_align_table_header(align = "left") |>
  tab_options(heading.padding = px(10))

# We can append measurement units to column labels with `cols_units()` 
towny |>
  dplyr::select(
    name, land_area_km2,
    ends_with("2016"), ends_with("2021")
  ) |>
  dplyr::arrange(desc(population_2021)) |>
  dplyr::slice_head(n = 10) |>
  gt(rowname_col = "name") |>
  tab_stubhead(label = "City") |>
  fmt_integer() |>
  cols_label(
    land_area_km2 ~ "Area, {{km^2}}",
    starts_with("population") ~ "",
    starts_with("density") ~ ""
  ) |>
  cols_units(
    starts_with("population") ~ "*ppl*",
    starts_with("density") ~ "*ppl* km^-2",
    .units_pattern = "{2}"
  ) |>
  tab_spanner(
    label = "Population",
    columns = starts_with("population"),
    gather = FALSE
  ) |>
  tab_spanner(
    label = "Density",
    columns = starts_with("density"),
    gather = FALSE
  ) |>
  tab_spanner(
    label = "2016",
    columns = ends_with("2016"),
    gather = FALSE
  ) |>
  tab_spanner(
    label = "2021",
    columns = ends_with("2021"),
    gather = FALSE
  ) |>
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(
      c(starts_with("population"), starts_with("density"))
    )
  ) |>
  cols_width(everything() ~ px(120)) |>
  opt_horizontal_padding(scale = 3)




### Nanoplots

# Nanoplots are tiny plots you can use in your gt table; they are simple yet
# highly customizable

# Here we add a column of nanoplots with `cols_nanoplot()`
illness |>
  dplyr::slice_head(n = 10) |>
  gt(rowname_col = "test") |>
  tab_header("Partial summary of daily tests performed on YF patient") |>
  tab_stubhead(label = md("**Test**")) |>
  cols_hide(columns = c(starts_with("norm"), starts_with("day"))) |>
  fmt_units(columns = units) |>
  cols_nanoplot(
    columns = starts_with("day"),
    new_col_name = "nanoplots",
    new_col_label = md("*Progression*")
  ) |>
  cols_align(align = "center", columns = nanoplots) |>
  cols_merge(columns = c(test, units), pattern = "{1} ({2})") |>
  tab_footnote(
    footnote = "Measurements from Day 3 through to Day 8.",
    locations = cells_column_labels(columns = nanoplots)
  )

# In this example, the nanoplots are small bar plots
sza |>
  dplyr::filter(latitude == 20 & tst <= "1200") |>
  dplyr::select(-latitude) |>
  dplyr::filter(!is.na(sza)) |>
  dplyr::mutate(saa = 90 - sza) |>
  dplyr::select(-sza) |>
  tidyr::pivot_wider(
    names_from = tst,
    values_from = saa,
    names_sort = TRUE
  ) |>
  gt(rowname_col = "month") |>
  tab_header(
    title = "Solar Altitude Angles",
    subtitle = "Average values every half hour from 05:30 to 12:00"
  ) |>
  cols_nanoplot(
    columns = matches("0"),
    plot_type = "bar",
    missing_vals = "zero",
    new_col_name = "saa",
    plot_height = "2.5em",
    options = nanoplot_options(
      data_bar_stroke_color = "GoldenRod",
      data_bar_fill_color = "DarkOrange"
    )
  ) |>
  cols_hide(columns = matches("0")) |>
  tab_options(
    table.width = px(400),
    column_labels.hidden = TRUE
  ) |>
  cols_align(
    align = "center",
    columns = everything()
  ) |>
  tab_source_note(
    source_note = "The solar altitude angle is the complement to
    the solar zenith angle. TMYK."
  )

# We can even make scatterplots with `cols_nanoplot()`
pizzaplace |>
  dplyr::filter(date == "2015-01-01") |>
  dplyr::mutate(date_time = paste(date, time)) |>
  dplyr::select(type, date_time, price) |>
  dplyr::group_by(type) |>
  dplyr::summarize(
    date_time = paste(date_time, collapse = ","),
    sold = paste(price, collapse = ",")
  ) |>
  gt(rowname_col = "type") |>
  tab_header(
    title = md("Pizzas sold on **January 1, 2015**"),
    subtitle = "Between the opening hours of 11:30 to 22:30"
  ) |>
  cols_hide(columns = c(date_time, sold)) |>
  cols_nanoplot(
    columns = sold,
    columns_x_vals = date_time,
    expand_x = c("2015-01-01 11:30", "2015-01-01 22:30"),
    reference_line = "median",
    new_col_name = "pizzas_sold",
    new_col_label = "Pizzas Sold",
    options = nanoplot_options(
      show_data_line = FALSE,
      show_data_area = FALSE,
      currency = "USD"
    )
  ) |>
  cols_width(pizzas_sold ~ px(200)) |>
  cols_align(columns = pizzas_sold, align = "center") |>
  opt_all_caps()



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
  