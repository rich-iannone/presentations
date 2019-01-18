library(gt)
library(tidyverse)

# Have a look at `exibble`
exibble

# View an `exibble` gt table
exibble %>% gt()

# Create a stub (with row labels), and,
# include row groups (using the `row`
# and `group` columns)
tab <-
  exibble %>%
  gt(
    rowname_col = "row",
    groupname_col = "group"
  )
tab

# Format the `num` column with gt's
# scientific notation
tab_2 <-
  tab %>%
  fmt_scientific(
    columns = vars(num),
    decimals = 3)
tab_2

# Format the dates in `date` (but
# only those rows that satisfy a
# condition) with `date_style` 6
tab_3 <-
  tab_2 %>%
  fmt_date(
    columns = vars(date),
    rows = grepl("^[a-d]", char),
    date_style = 6
  )
tab_3

# How do I get info on `date_style`s?
info_date_style()

# Are there other info tables? Yep:
info_currencies()
info_time_style()
info_paletteer()

# Anyway, let's get rid of some columns
tab_4 <-
  tab_3 %>%
  cols_hide(
    columns = vars(char, fctr, time, datetime))
tab_4

# Format the currency column to have
# values in pounds
tab_5 <-
  tab_4 %>%
  fmt_currency(
    columns = vars(currency),
    currency = "GBP"
  )
tab_5

# Add conditional footnotes to some of
# the `currency` values
tab_6 <-
  tab_5 %>%
  tab_footnote(
    footnote = "These are lower prices.",
    locations = cells_data(
      columns = vars(currency),
      rows = currency < 20
    )
  )
tab_6

# Add a footnote to the `currency`
# column label itself
tab_7 <-
  tab_6 %>%
  tab_footnote(
    footnote = "All values are in GBP.",
    locations = cells_column_labels(
      columns = vars(currency)
    )
  )
tab_7

# Add a header to the table (with a
# title and a subtitle)
tab_8 <-
  tab_7 %>%
  tab_header(
    title = "An Exibble Example",
    subtitle = md("Uses the `exibble` dataset from **gt**")
  )
tab_8

# Have the footnote glyphs be a
# sequence of lowercase letters
# (`tab_options()` has options galore)
tab_9 <-
  tab_8 %>%
  tab_options(footnote.glyph = letters)
tab_9

# Color the stub cells in blue; we just
# need to target `column_labels.background.color`
tab_10 <-
  tab_9 %>%
  tab_options(column_labels.background.color = "blue")
tab_10
