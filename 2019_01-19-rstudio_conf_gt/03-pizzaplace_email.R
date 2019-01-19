library(gt)
library(emo)
library(tidyverse)
library(scales)
library(blastula)

# Create `sizes_order` and `types_order` to
# support the ordering of pizza sizes and types
sizes_order <- c("S", "M", "L", "XL", "XXL")
types_order <- c("Classic", "Chicken", "Supreme", "Veggie")

# Get the total pizzaplace sales in the
# 2015 year; format as a currency value
# (this will go into the email body)
total_sales_2015 <-
  pizzaplace %>% 
  dplyr::pull(price) %>%
  sum() %>%
  scales::comma_format(
    big.mark = ",",
    prefix = "$"
  )(.)

# Create a gt table that uses the `pizzaplace`
# dataset; ensure that `as_raw_html()` is used
# (that gets us an HTML fragment with inlined
# CSS styles)
pizza_tab_email <-
  pizzaplace %>%
  dplyr::mutate(type = stringr::str_to_title(type)) %>%
  dplyr::mutate(size = factor(size, levels = sizes_order)) %>%
  dplyr::mutate(type = factor(type, levels = types_order)) %>%
  dplyr::group_by(type, size) %>%
  dplyr::summarize(
    pies = n(),
    income = sum(price)
    ) %>%
  dplyr::arrange(type, size) %>%
  gt(rowname_col = "size") %>%
  fmt_currency(
    columns = vars(income),
    currency = "USD") %>%
  fmt_number(
    columns = vars(pies),
    use_seps = TRUE,
    decimals = 0
    ) %>%
  summary_rows(
    groups = TRUE,
    columns = "pies",
    fns = list(TOTAL = "sum"),
    formatter = fmt_number,
    use_seps = TRUE,
    decimals = 0
    ) %>%
  summary_rows(
    groups = TRUE,
    columns = "income",
    fns = list(TOTAL = "sum"),
    formatter = fmt_currency,
    currency = "USD"
    ) %>%
  tab_options(
    summary_row.background.color = "#FFFEEE",
    stub_group.background.color = "#E6EFFC",
    table.font.size = px(14),
    row.padding = "5px"
    ) %>%
  cols_label(
    pies = "Pizzas",
    income = "Income"
    ) %>%
  tab_header(
    title = paste0("My ", emo::ji("pizza"), " sales in 2015"),
    subtitle = "Split by the type of pizza and the size"
    ) %>%
  tab_footnote(
    footnote = md("Only **The Greek Pizza** comes in this size."),
    locations = cells_stub(rows = 4:5)
    ) %>%
  tab_footnote(
    footnote = "The small-sized classic pizzas sold the most.",
    locations = cells_data(columns = 1, rows = 1)
    ) %>%
  as_raw_html()


# Create an email message using the
# `compose_email()` function from the
# blastula package
email <-
  blastula::compose_email(
    body = "
  Hello,

  Just wanted to let you know that pizza \\
  sales were pretty strong in 2015. When \\
  I look back at the numbers, it's **{total_sales_2015}** \\
  in sales. Not too bad. All things considered.

  Here is a table that shows a breakdown \\
  of the 2015 results by pizza size, split into \\
  *Pizza Type* groups:

  {pizza_tab_email}

  I also put all the 2015 numbers into an R \\
  dataset called `pizzaplace`. Not sure why \\
  I did that, but I did. It's in the **gt** \\
  package.

  Talk to you later,

  Alfonso
  "
)

# Preview the email in the RStudio Viewer
email %>% blastula::preview_email()

# Create a credentials file for sending
# this message through Gmail
blastula::create_email_creds_file(
  user = "******@gmail.com",
  password = "**********",
  provider = "gmail",
  sender = "Sender Name",
  creds_file_name = "gmail_creds"
)

# Send the email message out with
# `send_email_out()`
send_email_out(
  message = email,
  from = "******@gmail.com",
  to = "******@email.net",
  subject = "A look back at pizzaplace's 2015 sales",
  creds_file = "gmail_creds"
)
