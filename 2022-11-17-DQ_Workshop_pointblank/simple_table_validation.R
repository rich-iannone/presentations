library(pointblank)
library(tidyverse)


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
