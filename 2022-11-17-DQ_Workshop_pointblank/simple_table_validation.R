library(pointblank)
library(tidyverse)
library(palmerpenguins)


# Create the `simple_table` that was seen in the
# presentation slides
simple_table <-
  tribble(
    ~a,      ~b,      ~c,
    "yko2",  1,       23.1,
    "lju7",  0,       16.3,
    "qib0",  1,       21.2,
    "sd33",  1,       24.9,
    NA,      2,       NA
  )

##
## Data Quality Analysis
##

agent <-
  create_agent(
    tbl = simple_table,
    actions = action_levels(warn_at = 1, stop_at = 2),
  ) %>%
  col_vals_gte(vars(c), 15) %>%
  col_vals_in_set(vars(b), c(0, 1)) %>%
  col_vals_regex(vars(a), "[a-z]{3}[0-9]") %>%
  col_vals_expr(~case_when(
    b == 1 ~ c >= 20,
    b == 0 ~ c < 20
  )) %>%
  col_vals_not_null(vars(a, b, c)) %>%
  interrogate()

# Printing the `agent` gives us the agent report
# in the Viewer
agent

# This is quite manual (which is fine) but the
# package can generate rules for you, based on the
# data table
draft_validation(tbl = dplyr::storms)

#
# Other things the package can do
#

# The package can also provide a summary of any
# dataset you provide (could be a dataframe or
# a database table)
scan_data(tbl = dplyr::storms)

# The package allows you to create data dictionaries
# that can be published with R Markdown, Quarto,
# or elsewhere
informant_pp <- 
  create_informant(
    tbl = palmerpenguins::penguins,
    tbl_name = "penguins",
    label = "The `penguins` dataset from the **palmerpenguins** 📦."
  ) %>% 
  info_columns(
    columns = "species",
    `ℹ️` = "A factor denoting penguin species (*Adélie*, *Chinstrap*, and *Gentoo*)."
  ) %>%
  info_columns(
    columns = "island",
    `ℹ️` = "A factor denoting island in Palmer Archipelago, Antarctica
    (*Biscoe*, *Dream*, or *Torgersen*)."
  ) %>%
  info_columns(
    columns = "bill_length_mm",
    `ℹ️` = "A number denoting bill length"
  ) %>%
  info_columns(
    columns = "bill_depth_mm",
    `ℹ️` = "A number denoting bill depth"
  ) %>%
  info_columns(
    columns = "flipper_length_mm",
    `ℹ️` = "An integer denoting flipper length"
  ) %>%
  info_columns(
    columns = ends_with("mm"),
    `ℹ️` = "(in units of millimeters)."
  ) %>%
  info_columns(
    columns = "body_mass_g",
    `ℹ️` = "An integer denoting body mass (grams)."
  ) %>%
  info_columns(
    columns = "sex",
    `ℹ️` = "A factor denoting penguin sex (`\"female\"`, `\"male\"`)."
  ) %>%
  info_columns(
    columns = "year",
    `ℹ️` = "The study year (e.g., `2007`, `2008`, `2009`)."
  )

informant_pp
