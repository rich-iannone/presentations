library(gt)
library(tidyverse)

# Define our preferred order `ctry_origin`
order_countries <- c("Germany", "Italy", "United States", "Japan")

# Use dplyr functions to get the car with the best city gas mileage;
# this will be used to target the correct cell for a footnote
best_gas_mileage_city <-
  gtcars %>%
  dplyr::arrange(desc(mpg_c)) %>%
  dplyr::slice(1) %>%
  dplyr::mutate(car = paste(mfr, model)) %>%
  dplyr::pull(car)

# Use dplyr functions to get the car with the highest horsepower
# this will be used to target the correct cell for a footnote
highest_horsepower <-
  gtcars %>%
  dplyr::arrange(desc(hp)) %>%
  dplyr::slice(1) %>%
  dplyr::mutate(car = paste(mfr, model)) %>%
  dplyr::pull(car)

# Create a display table with `gtcars`, using all of the previous
# statements piped together + additional `tab_footnote()` stmts
tab <-
  gtcars %>%
  dplyr::group_by(ctry_origin) %>%
  dplyr::top_n(2) %>%
  dplyr::ungroup() %>%
  dplyr::filter(ctry_origin != "United Kingdom") %>%
  dplyr::arrange(
    factor(ctry_origin, levels = order_countries),
    mfr, desc(msrp)
  ) %>%
  dplyr::mutate(car = paste(mfr, model)) %>%
  dplyr::select(-mfr, -model) %>%
  dplyr::group_by(ctry_origin) %>%
  gt(rowname_col = "car") %>%
  cols_hide(columns = vars(drivetrain, bdy_style)) %>%
  cols_move(
    columns = vars(trsmn, mpg_c, mpg_h),
    after = vars(trim)
  ) %>%
  tab_spanner(
    label = md("*Performance*"),
    columns = vars(mpg_c, mpg_h, hp, hp_rpm, trq, trq_rpm)
  ) %>%
  cols_merge(
    col_1 = vars(mpg_c),
    col_2 = vars(mpg_h),
    pattern = "{1}c<br>{2}h"
  ) %>%
  cols_merge(
    col_1 = vars(hp),
    col_2 = vars(hp_rpm),
    pattern = "{1}<br>@{2}rpm"
  ) %>%
  cols_merge(
    col_1 = vars(trq),
    col_2 = vars(trq_rpm),
    pattern = "{1}<br>@{2}rpm"
  ) %>%
  cols_label(
    mpg_c = "MPG",
    hp = "HP",
    trq = "Torque",
    year = "Year",
    trim = "Trim",
    trsmn = "Transmission",
    msrp = "MSRP"
  ) %>%
  fmt_currency(
    columns = vars(msrp),
    currency = "USD",
    decimals = 0
  ) %>%
  cols_align(
    align = "center",
    columns = vars(mpg_c, hp, trq)
  ) %>%
  tab_style(
    style = cells_styles(text_size = px(12)),
    locations = cells_data(columns = vars(trim, trsmn, mpg_c, hp, trq))
  ) %>%
  text_transform(
    locations = cells_data(columns = vars(trsmn)),
    fn = function(x) {

      speed <- substr(x, 1, 1)

      type <-
        dplyr::case_when(
          substr(x, 2, 3) == "am" ~ "Automatic/Manual",
          substr(x, 2, 2) == "m" ~ "Manual",
          substr(x, 2, 2) == "a" ~ "Automatic",
          substr(x, 2, 3) == "dd" ~ "Direct Drive"
        )

      paste(speed, " Speed<br><em>", type, "</em>")
    }
  ) %>%
  tab_header(
    title = md("The Cars of **gtcars**"),
    subtitle = "These are some fine automobiles"
  ) %>%
  tab_source_note(
    source_note = md(
      "Source: Various pages within [edmunds.com](https://www.edmunds.com).")
  ) %>%
  tab_footnote(
    footnote = md("Best gas mileage (city) of all the **gtcars**."),
    locations = cells_data(
      columns = vars(mpg_c),
      rows = mpg_c == max(mpg_c))
  ) %>%
  tab_footnote(
    footnote = md("The highest horsepower of all the **gtcars**."),
    locations = cells_data(
      columns = vars(hp),
      rows = hp == max(hp))
  ) %>%
  tab_footnote(
    footnote = "All prices in U.S. dollars (USD).",
    locations = cells_column_labels(
      columns = vars(msrp))
  )

