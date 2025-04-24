#-------------------------------------------------------------------------------
## Practical Statistics for Data Scientists (R)
## Chapter 1. Exploratory Data Analysis
# > (c) 2019 Peter C. Bruce, Andrew Bruce, Peter Gedeck
#-------------------------------------------------------------------------------

# run setup ---------------------------------------------------------------

source('scripts/_setup.r')

# read datasets -----------------------------------------------------------

state <- read_csv('data/state.csv') |>
  janitor::clean_names()

dfw <- read_csv('data/dfw_airline.csv') |>
  janitor::clean_names()

sp500_px <- read_csv('data/sp500_data.csv.gz') |>
  rename(date = `...1`)

sp500_sym <- read_csv('data/sp500_sectors.csv')

kc_tax <- read_csv('data/kc_tax.csv.gz') |>
  janitor::clean_names()

lc_loans <- read_csv('data/lc_loans.csv')

airline_stats <- read_csv('data/airline_stats.csv') |>
  mutate(
    airline = factor(
      airline,
      levels = c('Alaska', 'American', 'Jet Blue', 'Delta', 'United', 'Southwest'),
      ordered = TRUE
    )
  )

# estimates of location ---------------------------------------------------

state |>
  arrange(state) |>
  slice(1:8) |>
  mutate(
    population = comma(population),
    murder_rate = comma(murder_rate, 0.1)
  ) |>
  print()

summaryx <- function(data, var) {
  data |>
    summarise(
      min = min({{ var }}, na.rm = TRUE),
      p1 = quantile({{ var }}, probs = 0.01, na.rm = TRUE, names = FALSE),
      p5 = quantile({{ var }}, probs = 0.05, na.rm = TRUE, names = FALSE),
      p10 = quantile({{ var }}, probs = 0.1, na.rm = TRUE, names = FALSE),
      p25 = quantile({{ var }}, probs = 0.25, na.rm = TRUE, names = FALSE),
      median = median({{ var }}, na.rm = TRUE),
      p75 = quantile({{ var }}, probs = 0.75, na.rm = TRUE, names = FALSE),
      p90 = quantile({{ var }}, probs = 0.9, na.rm = TRUE, names = FALSE),
      p95 = quantile({{ var }}, probs = 0.95, na.rm = TRUE, names = FALSE),
      p99 = quantile({{ var }}, probs = 0.99, na.rm = TRUE, names = FALSE),
      max = max({{ var }}, na.rm = TRUE),
      iqr = IQR({{ var }}, na.rm = TRUE),
      n_obs = n(),
      sum = sum({{ var }}, na.rm = TRUE),
      mean = mean({{ var }}, na.rm = TRUE),
      trim_mean = mean({{ var }}, na.rm = TRUE, trim = 0.1),
      mad = mad({{ var }}, na.rm = TRUE), # median absolute deviation
      sd = sd({{ var }}, na.rm = TRUE),
      lci_95 = mean({{ var }}, na.rm = TRUE) -
        qnorm(0.975) * sd({{ var }}, na.rm = TRUE),
      uci_95 = mean({{ var }}, na.rm = TRUE) +
        qnorm(0.975) * sd({{ var }}, na.rm = TRUE),
      n_miss = sum(is.na({{ var }})),
      n_zero = sum({{ var }} == 0, na.rm = TRUE),
      pct_miss = mean(is.na({{ var }})),
      .groups = "drop"
    ) |>
    mutate(
      pct_zero = n_zero / n_obs
    )
}

cli_h2('population stats')

state |>
  summaryx(population) |>
  mutate(
    across(starts_with('pct'), ~percent(.x, 0.1)),
    across(where(is.numeric), comma)
  ) |>
  glimpse()

cli_h2('murder rate stats')

state |>
  summaryx(murder_rate) |>
  mutate(
    across(starts_with('pct'), ~percent(.x, 0.1)),
    across(where(is.numeric), ~comma(.x, 0.01))
  ) |>
  glimpse()

cli_h2('murder rate weighted mean')

state |>
  summarise(
    median = median(murder_rate, na.rm = TRUE),
    mean = mean(murder_rate, na.rm = TRUE),
    #wgt_median = weighted.median(murder_rate, w = population, na.rm = TRUE),
    wgt_mean = weighted.mean(murder_rate, w = population, na.rm = TRUE)
  ) |>
  mutate(across(everything(), ~comma(.x, 0.01))) |>
  glimpse()

# boxplots ----------------------------------------------------------------

state |>
  ggplot(aes(y = population)) +
  geom_boxplot() +
  scale_y_continuous(label = comma_format(scale = 1/1000000, suffix = 'M')) +
  labs(
    x = NULL, y = NULL,
    title = 'Boxplot of population (in millions)'
  ) +
  guides(x = 'none')

# histogram ---------------------------------------------------------------

state |>
  ggplot(aes(population)) +
  geom_histogram(binwidth = 2500000, center = 1250000) +
  scale_x_continuous(label = comma_format(scale = 1/1000000, suffix = 'M')) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = NULL, y = NULL,
    title = 'Histogram of population (in millions)'
  )

state_breaks <- seq(0, max(state$population), by = 2500000)

state |>
  group_by(
    pop_band = cut(
      population,
      breaks = c(state_breaks, Inf),
      labels = (state_breaks + 1250000) |>
        comma(0.01, scale = 1/1000000, suffix = 'M')
    )
  ) |>
  summarise(
    n = n(),
    states = paste(abbreviation, collapse = ', ')
  ) |>
  ungroup() |>
  mutate(pct = percent(n / sum(n), 0.1)) |>
  relocate(states, .after = pct) |>
  print()

# density estimates -------------------------------------------------------

state |>
  ggplot(aes(murder_rate)) +
  #geom_histogram() +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, position = "identity",
    alpha = 0.8, center = 0.5) +
  geom_density() +
  scale_x_continuous(breaks = 0:11) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = NULL, y = NULL,
    title = 'Histogram and density of murder rate'
  )

# Exploring Binary and Categorical Data -----------------------------------

dfw |>
  gather() |>
  mutate(
    key = if_else(key == 'atc', 'ATC', str_to_sentence(key)) |>
      fct_reorder(value) |>
      fct_rev()
  ) |>
  ggplot(aes(key, value)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), label = comma) +
  labs(
    x = NULL, y = NULL,
    title = 'Cause of delay'
  )

# Correlation -------------------------------------------------------------

telecom <- sp500_px |>
  filter(date > ymd('2012-07-01')) |>
  select(
    sp500_sym |>
      filter(sector == 'telecommunications_services') |>
      pull(symbol)
  )

telecom |>
  ggcorrplot::cor_pmat() |>
  print()

telecom |>
  ggplot(aes(T, VZ)) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  geom_vline(xintercept = 0, alpha = 0.5) +
  geom_point(alpha = 0.5) +
  labs(
    x = 'ATT (T)',
    y = 'Verizon (VZ)',
    title = 'ATT vs Verizon'
  )

etfs <- sp500_px |>
  filter(date > ymd('2012-07-01')) |>
  select(
    sp500_sym |>
      filter(sector == 'etf') |>
      pull(symbol)
  )

etfs |>
  ggcorrplot::cor_pmat() |>
  print()

etfs |>
  ggcorrplot::cor_pmat() |>
  ggcorrplot::ggcorrplot(
    method = 'circle',
    type = 'upper',
    colors = c(orange, "white", purple),
    ggtheme = NULL
  ) +
  labs(title = 'Correlation for ETFs')

# Exploring Two or More Variables -----------------------------------------

kc_tax0 <- kc_tax |>
  filter(
    tax_assessed_value < 750000,
    sq_ft_tot_living > 100,
    sq_ft_tot_living < 3500
  )

cli_alert_info('kc_tax0 had {comma(nrow(kc_tax0))} rows')

kc_tax0 |>
  ggplot((aes(sq_ft_tot_living, tax_assessed_value))) +
  stat_bin_hex(color = 'white') +
  scale_fill_gradient(low = 'white', high = purple) +
  scale_y_continuous(labels = dollar) +
  labs(
    x = 'Finished Square Feet',
    y = 'Tax-Assessed Value',
    title = 'hex bin'
  )

ggplot(kc_tax0, aes(sq_ft_tot_living, tax_assessed_value)) +
  geom_point(alpha = 0.1) +
  geom_density2d(color = 'white') +
  scale_y_continuous(labels = dollar) +
  labs(
    x = 'Finished Square Feet',
    y = 'Tax-Assessed Value',
    title = '2d density'
  )

# Two Categorical Variables -----------------------------------------------

lc_loans |>
  tabyl(grade, status) |>
  adorn_totals() |>
  adorn_totals(where = 'col') |>
  adorn_percentages() |>
  adorn_pct_formatting() |>
  adorn_ns(position = 'front') |>
  as_tibble() |>
  print()

lc_loans |>
  tabyl(grade, status) |>
  adorn_totals() |>
  adorn_totals(where = 'col') |>
  gather(-grade, key = 'status', value = 'n') |>
  mutate(
    status = fct_reorder(status, n, .fun = sum) |>
      fct_rev() |>
      fct_relevel('Total', after = Inf)
  ) |>
  group_by(grade) |>
  mutate(
    row_pct = case_when(status != 'Total' ~ n / max(n))
  ) |>
  group_by(status) |>
  mutate(
    col_pct = case_when(grade != 'Total' ~ n / max(n)),
    label = glue('
      {comma(n)}
      {coalesce(percent(row_pct), "-")}
      {coalesce(percent(col_pct), "-")}
    ')
  ) |>
  ungroup() |>
  ggplot(aes(status, grade, fill = col_pct, label = label)) +
  geom_tile(colour = 'white') +
  geom_text(colour = 'white') +
  scale_x_discrete(
    expand = c(0, 0),
    position = 'top'
  ) +
  scale_y_discrete(expand = c(0, 0), limits = rev)  +
  scale_fill_jo_c(label = percent, na.value = chocolate) +
  labs(
    x = 'Status',
    y = 'Grade',
    fill = 'Column %',
    title = 'Status by grade heatmap'
  ) +
  theme(
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.text = element_text(size = rel(0.7)),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.key.size = unit(0.8, 'lines'),
    legend.key.width = unit(3.5, 'lines')
  )

# Categorical and Numeric Data --------------------------------------------

airline_stats |>
  ggplot(aes(airline, pct_carrier_delay / 100)) +
  geom_boxplot() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), label = percent) +
  labs(
    x = NULL,
    y = NULL,
    title = 'Daily % of Delayed Flights'
  ) +
  coord_cartesian(ylim = c(0, 0.5))

airline_stats |>
  ggplot(aes(airline, pct_carrier_delay / 100)) +
  geom_violin(draw_quantiles = c(.25, .5, .75)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), label = percent) +
  labs(
    x = NULL,
    y = NULL,
    title = 'Daily % of Delayed Flights'
  ) +
  coord_cartesian(ylim = c(0, 0.5))

# Visualizing Multiple Variables ------------------------------------------

kc_tax0 |>
  filter(zip_code %in% c(98188, 98105, 98108, 98126)) |>
  ggplot((aes(sq_ft_tot_living, tax_assessed_value))) +
  stat_bin_hex(color = 'white') +
  scale_fill_gradient(low = 'white', high = purple) +
  scale_y_continuous(labels = dollar) +
  labs(
    x = 'Finished Square Feet',
    y = 'Tax-Assessed Value',
    title = 'hex bin'
  ) +
  facet_wrap(~zip_code)

#-------------------------------------------------------------------------------
