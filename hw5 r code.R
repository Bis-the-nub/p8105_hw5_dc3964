## Loading packages
library(tidyverse)
library(ggplot2)
library(broom)

## Problem 1

birthday_pool =
  function(n) {
  birthdays = sample(1:365, size = n, replace = TRUE)
  has_duplicate = length(birthdays) != length(unique(birthdays))
  return(has_duplicate)
}

# Simulation setting
set.seed(506)
n_simulations = 10000 
group_sizes = 2:50

probability_data =
  tibble(
  group_size = rep(group_sizes, each = n_simulations)
  ) |> 
  group_by(group_size) |> 
  mutate(
    match_result = map_lgl(group_size, birthday_pool)
  ) |> 
  summarise(
    probability = mean(match_result),
    .groups = 'drop'
  )

# Plot
probability_data |> 
  ggplot(aes(x = group_size, y = probability)) +
  geom_line(color = "deepskyblue3", linewidth = 1) +
  geom_point(color = "deepskyblue3", size = 2) +

  # mark the critical point when p exceeds 0.5
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 23, linetype = "dashed", color = "red") +
  
  # supplements
  labs(
    title = "Probability for duplicate birthdays in different groups",
    x = "Group size",
    y = "Probability"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_x_continuous(breaks = seq(0, 50, 5)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

## Problem 2

mu_values = c(0, 1, 2, 3, 4, 5, 6)

simulation_results =
  tibble(
  true_mu = rep(mu_values, each = 5000),
  sim_id = 1:length(true_mu)
  ) |>
  mutate(
    test_output = map(true_mu, ~ {
      data_set = rnorm(n = 30, mean = .x, sd = 5)
      t.test(data_set, mu = 0) |> 
        tidy()
    })
  ) |>
  unnest(
    test_output
  ) |>
  select(
    true_mu,
    mu_hat = estimate,
    p_value = p.value
  ) |>
  mutate(
    reject_H0 = (p_value < 0.05)
  )

summary_stats =
  simulation_results |>
  group_by(true_mu) |>
  summarise(
    mean_mu_hat = mean(mu_hat),
    power_or_typeI_error = mean(reject_H0),
    .groups = 'drop'
  )

# plot power vs true mean
summary_stats |>
  ggplot(aes(x = true_mu, y = power_or_typeI_error)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    x = "True mean (μ)",
    y = "Power",
    title = "Power of the t-test vs. True Mean"
  ) +
  theme_minimal(base_size = 14)

# plot 2
mean_estimates =
  simulation_results |>
  group_by(true_mu) |>
  summarise(
    mean_mu_hat_all = mean(mu_hat),
    mean_mu_hat_reject = mean(mu_hat[reject_H0]),
    .groups = "drop"
  )

mean_estimates |>
  ggplot(aes(x = true_mu)) +
  geom_line(aes(y = mean_mu_hat_all, color = "All samples"), linewidth = 1.2) +
  geom_point(aes(y = mean_mu_hat_all, color = "All samples"), size = 2) +
  geom_line(aes(y = mean_mu_hat_reject, color = "Rejected H0"), linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(y = mean_mu_hat_reject, color = "Rejected H0"), size = 2) +
  labs(
    x = "True mean (μ)",
    y = "Average estimated μ head",
    title = "Average estimate μ head vs. True μ",
    color = "Group"
  ) +
  theme_minimal(base_size = 14)
