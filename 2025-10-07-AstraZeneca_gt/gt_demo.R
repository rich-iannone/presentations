library(gt)
library(tidyverse)



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

# We can format numeric values using significant digits and also format
# using scientific notation; NA values can be replaced with `sub_missing()`
exibble |>
  dplyr::select(num, currency) |>
  gt() |>
  fmt_number(
    columns = num,
    n_sigfig = 2
  ) |>
  fmt_scientific(columns = currency) |>
  sub_missing(missing_text = "n.a.")


# Date and time formatting is fairly powerful, yet we can use an `easy mode`
# and supply presets for the date and time components; `text_transform()` allows
# for touch-ups if the formatting needs revisions
exibble |>
  dplyr::select(datetime) |>
  gt() |>
  fmt_datetime(
    date_style = "MMMEd",
    time_style = "Hm",
  ) |>
  text_transform(
    fn = function(x) gsub(" (\\d) ", " \\1 at ", x)
  )



### Column merging

# Here we use the `cols_merge()` function to merge the
# `open` & `close` columns together, and, the `low` & `high` columns
sp500 |>
  dplyr::slice(50:55) |>
  dplyr::select(-volume, -adj_close) |>
  gt() |>
  fmt_number(decimals = 1) |>
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

# Spanner labels can be used to group columns together

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

  