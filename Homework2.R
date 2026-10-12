
library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(stringr)
library(forcats)
library(purrr)
library(ggplot2)


# PROBLEM 1
# Part A

sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)

sales_clean <- sales_raw %>%
  mutate(
    item_clean = str_to_lower(str_squish(item_raw)),
    amount = parse_number(amount_raw),
    discount_rate = parse_number(discount_raw) / 100,
    net_amount = amount * (1 - discount_rate),
    size = case_when(
      is.na(net_amount) ~ "unrecorded",
      net_amount >= 20 ~ "large",
      TRUE ~ "small"
    )
  )

sales_clean %>%
  select(sale_id, item_clean, amount, discount_rate, net_amount, size)

# str_squish() removes extra spaces and str_to_lower() makes
# all item names lowercase.
# parse_number() converts the dollar amounts and discounts to numbers.
# Dividing by 100 converts discounts into proportions.
# net_amount calculates the sale amount after the discount.
# case_when() labels sales as large, small, or unrecorded.
# Missing amounts remain NA.


# Part B

sales_codes <- sales_clean %>%
  mutate(
    code_clean = str_to_upper(str_trim(code_raw)),
    valid_code = str_detect(code_clean, "^(ST|ON)-[0-9]{3}$")
  )

invalid_codes <- sales_codes %>%
  filter(!valid_code) %>%
  select(sale_id, code_raw, code_clean)

invalid_codes

# The invalid codes are sale 5 (BAD) and sale 7 (ON-107X).
# Both fail the required code format.

valid_code_parts <- sales_codes %>%
  filter(valid_code) %>%
  select(sale_id, code_raw, code_clean) %>%
  separate_wider_delim(
    code_clean,
    delim = "-",
    names = c("channel", "number")
  )

valid_code_parts

sale_tags <- sales_clean %>%
  select(sale_id, tags) %>%
  separate_longer_delim(tags, delim = ";")

sale_tags

# separate_wider_delim() splits one column into multiple columns,
# while separate_longer_delim() splits values into multiple rows.


# Part C

customer_summary <- sales_clean %>%
  summarise(
    sales_n = n(),
    known_n = sum(!is.na(net_amount)),
    large_n = sum(net_amount >= 20, na.rm = TRUE),
    large_share = if_else(
      known_n > 0,
      large_n / known_n,
      NA_real_
    ),
    .by = customer_id
  ) %>%
  arrange(customer_id)

customer_summary

# For C1, the numerator is 0 because none of its known sales
# have a net amount of at least 20.
# The denominator is 1 because only one sale has a known amount.
# Therefore, C1's large_share is 0 / 1 = 0.
# C5 has no known amounts, so large_share is NA.

sales_factors <- sales_clean %>%
  mutate(
    item_factor = fct_infreq(factor(item_clean)),
    item_group = fct_lump_n(
      factor(item_clean),
      n = 2,
      other_level = "Other"
    )
  )

levels(sales_factors$item_factor)
levels(sales_factors$item_group)

sales_factors %>%
  count(item_factor)

sales_factors %>%
  count(item_group)

# fct_infreq() changes the order of factor levels based on frequency.
# It does not remove or combine any categories.
# fct_lump_n() keeps the two most frequent categories and combines
# the remaining categories into Other.
# Pen appears 4 times, folder 2 times, and Other 2 times.


# PROBLEM 2
# Part A

reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)

customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)

reports_labeled <- reports_raw %>%
  arrange(form, line) %>%
  group_by(form) %>%
  fill(customer_id, .direction = "down") %>%
  ungroup()

reports_clean <- reports_labeled %>%
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount = coalesce(primary_clean, backup_amount)
  )

reports_clean %>%
  select(form, line, customer_id, round,
         primary_amount, backup_amount, amount)

# fill() uses the nearest customer ID above a missing ID
# within the same form.
# na_if() changes -999 into NA because it represents missing data.
# coalesce() uses the primary amount first and only uses the backup
# when the primary amount is missing.
# A recorded zero is kept because zero is a valid amount, not NA.

reports_complete <- reports_clean %>%
  select(customer_id, round, amount) %>%
  mutate(recorded_row = TRUE) %>%
  complete(customer_id, round = 1:3) %>%
  mutate(recorded_row = replace_na(recorded_row, FALSE))

reports_complete

original_rows <- nrow(reports_clean)
added_rows <- nrow(reports_complete) - original_rows

missing_before <- sum(is.na(reports_clean$amount))
missing_after <- sum(is.na(reports_complete$amount))

original_rows
added_rows
missing_before
missing_after

# There are 6 original rows and 6 added rows.
# Missing amounts increase from 1 before completion to 7 afterward.
# An explicit missing value is C3 in round 1, where amount is NA.
# An implicit missing value is C1 in round 2, which had no row.
# recorded_row identifies which rows originally existed.


# Part B

customers %>%
  count(customer_id) %>%
  filter(n > 1)

sum(is.na(customers$customer_id))

reports_clean %>%
  count(customer_id, round) %>%
  filter(n > 1)

# There are no duplicate customer IDs in customers.
# There are no missing customer IDs in customers.
# There are no repeated customer-round combinations in reports_clean.

customers_marked <- customers %>%
  mutate(customer_found = TRUE)

reports_joined <- reports_clean %>%
  left_join(
    customers_marked,
    by = join_by(customer_id),
    relationship = "many-to-one",
    na_matches = "never"
  ) %>%
  mutate(customer_found = replace_na(customer_found, FALSE))

reports_joined %>%
  select(customer_id, round, amount, region, customer_found)

only_reports <- reports_clean %>%
  distinct(customer_id) %>%
  anti_join(
    customers,
    by = join_by(customer_id),
    na_matches = "never"
  )

only_customers <- customers %>%
  distinct(customer_id) %>%
  anti_join(
    reports_clean,
    by = join_by(customer_id),
    na_matches = "never"
  )

only_reports
only_customers

# C4 appears only in reports_clean.
# C5 appears only in customers.
# C3 has customer_found = TRUE because its customer record exists,
# even though the region is missing.
# C4 has customer_found = FALSE because no customer record matches.
# A missing region alone cannot distinguish these two situations.


# Part C

# customers_bad was not defined in the provided assignment setup.
# This example duplicates C2 with conflicting region information.

customers_bad <- tibble(
  customer_id = c("C1", "C2", "C2", "C3", "C5"),
  region = c("North", "South", "East", NA, "West")
)

reports_duplicated <- reports_clean %>%
  left_join(
    customers_bad,
    by = join_by(customer_id),
    relationship = "many-to-many"
  )

nrow(reports_clean)
nrow(reports_duplicated)

sum(reports_clean$amount, na.rm = TRUE)
sum(reports_duplicated$amount, na.rm = TRUE)

reports_duplicated %>%
  count(customer_id, round) %>%
  filter(n > 1)

# Using the example customers_bad, rows increase from 6 to 7.
# The total amount increases from 75 to 95.
# C2 in round 1 is repeated because it matches two customer records.
# Its amount of 20 is counted twice, increasing the total by 20.
# Using relationship = "many-to-one" would produce an error
# because one report matches multiple customer records.
# Allowing many-to-many permits the join but does not resolve
# the conflicting customer regions.

reports_wide <- reports_complete %>%
  select(customer_id, round, amount) %>%
  pivot_wider(
    names_from = round,
    values_from = amount,
    names_prefix = "round_"
  )

reports_wide

reports_long <- reports_wide %>%
  pivot_longer(
    cols = starts_with("round_"),
    names_to = "round",
    names_prefix = "round_",
    values_to = "amount",
    values_drop_na = TRUE
  )

nrow(reports_long)

# The wide table contains one row per customer with three round columns.
# After pivoting back to long and dropping NA values, 5 rows remain.
# Dropping missing values removes unknown amounts from the data.
# It does not recover the values because they were never recorded.


# PROBLEM 3
# Part A

two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  nx <- length(x)
  ny <- length(y)
  
  mean_x <- mean(x)
  mean_y <- mean(y)
  
  var_x <- var(x)
  var_y <- var(y)
  
  if (equal_var) {
    pooled_var <- ((nx - 1) * var_x + (ny - 1) * var_y) /
      (nx + ny - 2)
    
    se <- sqrt(pooled_var * (1 / nx + 1 / ny))
    df <- nx + ny - 2
    method <- "Pooled"
  } else {
    se <- sqrt(var_x / nx + var_y / ny)
    
    df <- (var_x / nx + var_y / ny)^2 /
      ((var_x / nx)^2 / (nx - 1) +
         (var_y / ny)^2 / (ny - 1))
    
    method <- "Welch"
  }
  
  t_stat <- (mean_x - mean_y) / se
  p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  
  reject <- p_value < alpha
  
  return(list(
    method = method,
    t_stat = t_stat,
    df = df,
    p_value = p_value,
    reject = reject
  ))
}

x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)

welch_result <- two_sample_t(x_check, y_check)
pooled_result <- two_sample_t(x_check, y_check, equal_var = TRUE)

welch_result
pooled_result

welch_ref <- t.test(x_check, y_check, var.equal = FALSE)
pooled_ref <- t.test(x_check, y_check, var.equal = TRUE)

welch_check <- c(
  t_stat = abs(welch_result$t_stat - unname(welch_ref$statistic)) < 1e-8,
  df = abs(welch_result$df - unname(welch_ref$parameter)) < 1e-8,
  p_value = abs(welch_result$p_value - welch_ref$p.value) < 1e-8
)

pooled_check <- c(
  t_stat = abs(pooled_result$t_stat - unname(pooled_ref$statistic)) < 1e-8,
  df = abs(pooled_result$df - unname(pooled_ref$parameter)) < 1e-8,
  p_value = abs(pooled_result$p_value - pooled_ref$p.value) < 1e-8
)

welch_check
pooled_check

# The null hypothesis is that the two population means are equal.
# The alternative hypothesis is that the means are different.
# Welch's test allows unequal variances, while the pooled test
# assumes equal population variances.
# Both tests use their calculated standard error and degrees
# of freedom to calculate the t-statistic and two-sided p-value.
# The null hypothesis is rejected when the p-value is below 0.05.
# All three verification comparisons return TRUE for both methods.
# Therefore, the differences in t-statistic, degrees of freedom,
# and p-value are all smaller than 1e-8.


# Part B

B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05

scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null",
               "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)

set.seed(2026)

total_rows <- nrow(scenarios) * B * 2

simulation_results <- data.frame(
  scenario = character(total_rows),
  replicate = integer(total_rows),
  method = character(total_rows),
  t_stat = numeric(total_rows),
  df = numeric(total_rows),
  p_value = numeric(total_rows),
  reject = logical(total_rows)
)

row_index <- 1

for (i in 1:nrow(scenarios)) {
  for (j in 1:B) {
    
    x <- rnorm(
      n_x,
      mean = scenarios$mean_x[i],
      sd = scenarios$sd_x[i]
    )
    
    y <- rnorm(
      n_y,
      mean = scenarios$mean_y[i],
      sd = scenarios$sd_y[i]
    )
    
    welch <- two_sample_t(x, y, equal_var = FALSE, alpha = alpha)
    pooled <- two_sample_t(x, y, equal_var = TRUE, alpha = alpha)
    
    for (result in list(welch, pooled)) {
      
      simulation_results[row_index, ] <- list(
        scenarios$scenario[i],
        j,
        result$method,
        result$t_stat,
        result$df,
        result$p_value,
        result$reject
      )
      
      row_index <- row_index + 1
    }
  }
}

head(simulation_results, 6)
nrow(simulation_results)

# Four scenarios are simulated 1000 times for two methods.
# This produces 8000 total rows in simulation_results.
# The same independently generated x and y samples are used
# for Welch and pooled tests within each repetition.
# set.seed(2026) makes the simulation reproducible.

simulation_summary <- simulation_results %>%
  summarise(
    mean_t = mean(t_stat),
    sd_t = sd(t_stat),
    rejection_rate = mean(reject),
    .by = c(scenario, method)
  ) %>%
  arrange(scenario, method)

simulation_summary %>%
  mutate(
    mean_t = round(mean_t, 3),
    sd_t = round(sd_t, 3),
    rejection_rate = round(rejection_rate, 3)
  )

# The summary has eight rows, one for each scenario and method.
# mean_t is the average t-statistic.
# sd_t is the standard deviation of the simulated t-statistics.
# rejection_rate is the proportion of tests that reject H0.

ggplot(
  simulation_summary,
  aes(x = scenario, y = rejection_rate, fill = method)
) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  labs(
    title = "Rejection Rates by Scenario and Method",
    x = "Scenario",
    y = "Rejection Rate",
    fill = "Method"
  ) +
  theme_minimal()

# Short Report:
# With equal means and variances, both tests have rejection rates near 0.05, with pooled at 0.038 and Welch at 0.044.
# Under unequal variances, the pooled test has a higher Type I error rate of 0.139, compared with Welch at 0.061.
# When the means differ, rejection rates increase, indicating greater power, although Welch has lower power than the pooled test in the unequal-variance scenario.
# Higher power alone is not enough to judge a test because it must also control Type I errors, and the simulated rates can vary randomly.

