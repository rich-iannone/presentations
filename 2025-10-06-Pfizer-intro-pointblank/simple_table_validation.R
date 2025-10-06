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
  ) |>
  col_vals_gte(vars(c), 15) |>
  col_vals_in_set(vars(b), c(0, 1)) |>
  col_vals_regex(vars(a), "[a-z]{3}[0-9]") |>
  col_vals_expr(~case_when(
    b == 1 ~ c >= 20,
    b == 0 ~ c < 20
  )) |>
  col_vals_not_null(vars(a, b, c)) |>
  interrogate()

# Printing the `agent` to see the report in the Viewer
agent

# This is quite manual (which is fine) but the
# package can generate rules for you, based on the
# data table
draft_validation(tbl = dplyr::storms, overwrite = TRUE)

#
# Other things the package can do
#

# The package can also provide a summary of any
# dataset you provide (could be a dataframe or
# a database table)
scan_data(tbl = dplyr::storms, sections="OVS")


# We can perform validations over a period of time and
# get a unified report of all validation results

# First define thresholds for action levels
al <- 
  action_levels(
    warn_at = 0.05,
    stop_at = 0.10,
    notify_at = 0.20
  )

# Create the first agent
agent_1 <-
  create_agent(
    tbl = small_table,
    label = "An example.",
    actions = al
  ) |>
  col_vals_gt(
    columns = vars(date_time),
    value = vars(date),
    na_pass = TRUE
  ) |>
  col_vals_gt(
    columns = vars(b), 
    value = vars(g),
    na_pass = TRUE
  ) |>
  rows_distinct() |>
  col_vals_equal(
    columns = vars(d), 
    value = vars(d),
    na_pass = TRUE
  ) |>
  col_vals_between(
    columns = vars(c), 
    left = vars(a), right = vars(d)
  ) |>
  col_vals_not_between(
    columns = vars(c),
    left = 10, right = 20,
    na_pass = TRUE
  ) |>
  rows_distinct(columns = vars(d, e, f)) |>
  col_is_integer(columns = vars(a)) |>
  interrogate()

# The second agent...
agent_2 <- 
  agent_1 |>
  col_exists(columns = vars(date, date_time)) |>
  col_vals_regex(
    columns = vars(b), 
    regex = "[0-9]-[a-z]{3}-[0-9]{3}",
    active = FALSE
  ) |>
  interrogate()

# The third agent...
agent_3 <- 
  agent_2 |>
  col_vals_in_set(
    columns = vars(f),
    set = c("low", "mid", "high")
  ) |>
  remove_steps(i = 5) |>
  deactivate_steps(i = 1) |>
  interrogate()

# The fourth agent...
agent_4 <-
  agent_3 |>
  activate_steps(i = 1) |>
  activate_steps(i = 10) |>
  remove_steps(i = 6) |>
  interrogate()

# Create a `multiagent` object with the four agents
multiagent <-
  create_multiagent(
    agent_1, agent_2, agent_3, agent_4
  )

# Get a report of the `multiagent` in 'wide' format
report_wide <- 
  get_multiagent_report(
    multiagent,
    display_mode = "wide"
  )

report_wide
