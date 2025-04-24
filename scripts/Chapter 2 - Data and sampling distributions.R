#-------------------------------------------------------------------------------
## Practical Statistics for Data Scientists (R)
## Chapter 2. Data and Sampling Distributions
# > (c) 2019 Peter C. Bruce, Andrew Bruce, Peter Gedeck
#-------------------------------------------------------------------------------

# run setup ---------------------------------------------------------------

source('scripts/_setup.r')

# read datasets -----------------------------------------------------------

loans_income <- read_csv('data/loans_income.csv') |>
  pull(x)

sp500_px <- read_csv('data/sp500_data.csv.gz') |>
  rename(date = `...1`)

# distribution vs sample --------------------------------------------------

tibble(
  x = seq(from=-3, to=3, length=300),
  gauss = dnorm(x)
) |>
ggplot(aes(x, gauss)) +
  geom_area(fill = purple, alpha = 0.5, colour = purple) +
  labs(x = NULL, y = NULL, title = 'Population') +
  guides(x = 'none', y = 'none')

tibble(norm_samp = rnorm(100)) |>
  ggplot(aes(norm_samp)) +
  geom_histogram(fill = orange, alpha = 0.5, colour = orange, bins = 25) +
  labs(x = NULL, y = NULL, title = 'Sample') +
  guides(x = 'none', y = 'none')

# Sampling Distribution of a Statistic ------------------------------------

# function take a sample of means of n values to be repeated t times
samp_mean <- function(t, n) {
  tibble(
    income = sample(loans_income, n)
  ) |>
    summarise(income = mean(income)) |>
    mutate(type = glue('Mean of {n}'))
}

income_samps <- bind_rows(
  # take a simple random sample
  tibble(
    income = sample(loans_income, 1000),
    type = 'Data distribution'
  ),
  # take a sample of means of 5 values
  map_dfr(1:1000, ~samp_mean(.x, 5)),
  # take a sample of means of 20 values
  map_dfr(1:1000, ~samp_mean(.x, 20))
) |>
  mutate(type = fct_inorder(type))

income_samps |>
  ggplot(aes(income)) +
  geom_histogram(bins = 40) +
  facet_wrap(~ type, ncol = 1)


# The Bootstrap -----------------------------------------------------------
# As the calculation uses random samples, results will vary between runs

stat_fun <- function(x, idx) median(x[idx])
boot_obj <- boot::boot(loans_income, R=1000, statistic = stat_fun)

boot_obj


# Confidence Intervals ----------------------------------------------------

set.seed(7)

sample20 <- sample(loans_income, 20)
sample_mean <- mean(sample20)

stat_fun <- function(x, idx) mean(x[idx])
boot_obj <- boot::boot(sample20, R = 500, statistic = stat_fun)
boot_ci <- boot::boot.ci(boot_obj, conf = 0.9, type = 'basic')
boot_x <- tibble(mean = boot_obj$t)

ci <- tibble(ci = boot_ci$basic[4:5], y = c(9, 11))
ci

ggplot(boot_x, aes(mean)) +
    geom_histogram(bins = 40) +
    geom_vline(xintercept = sample_mean, linetype = 2) +
    geom_path(aes(ci, 10), data = ci, size = 2, colour = orange) +
    geom_path(aes(ci90[1], y), data = ci, size = 2, colour = orange) +
    geom_path(aes(ci90[2], y), data = ci, size = 2, colour = orange) +
    annotate('text', x=sample_mean, y = 20, label = 'Sample mean',
      size = 6, colour = orange) +
    annotate('text', x=sample_mean, y =  8, label = '90% interval',
      size = 6, colour = orange) +
    labs(x = NULL, y = 'Counts')


# Normal Distribution -----------------------------------------------------

tibble(x = rnorm(100)) |>
  ggplot(aes(sample = x)) +
  geom_qq() +
  geom_qq_line() +
  labs(
    x = 'Quantile of normal distribution',
    y = 'z-score',
    title = 'QQ plot')

set.seed(23598)

tibble(x = rnorm(100, 37, 6)) |>
  ggplot(aes(x)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 42) +
  geom_vline(xintercept = qnorm(0.9, 37, 6), colour = pink) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(
    x = 'x',
    y = 'Number of trials',
    title = 'Normal distribution with mean of 37 and sd of 6 with 100 trials (rnorm)',
    subtitle = glue('
    probability of 42 is {percent(dnorm(42, 37, 6))} (dnorm)
    probability of <= 42 is {percent(pnorm(42, 37, 6))} (pnorm)
    need {comma(qnorm(0.9, 37, 6), 0.1)} to be in at the 90% percentile (qnorm)
    ')
  )

# Long-Tailed Distributions -----------------------------------------------

sp500_px |>
  filter(NFLX > 0) |>
  ggplot(aes(sample = log(NFLX))) +
  geom_qq() +
  geom_qq_line() +
  labs(
    x = 'Quantile of normal distribution',
    y = 'z-score',
    title = 'QQ plot')


# Binomial Distribution ---------------------------------------------------

dbinom(x = 2, size = 5, p = 0.1)

pbinom(2, 5, 0.1)

dbinom(x = 0, size = 200, p = 0.02)

set.seed(23598)

tibble(binom = rbinom(100, 10, 0.5)) |>
  ggplot(aes(binom)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 6) +
  geom_vline(xintercept = qbinom(0.9, 10, 0.5), colour = pink) +
  scale_x_continuous(breaks = 0:10) +
  labs(
    x = 'Number of heads',
    y = 'Number of trials',
    title = 'Number of heads from 10 coin flips with 100 trials (binom)',
    subtitle = glue('
    probability of 6 heads is {percent(dbinom(6, 10, 0.5))} (dbinom)
    probability of <= 6 heads is {percent(pbinom(6, 10, 0.5))} (pbinom)
    need {qbinom(0.9, 10, 0.5)} heads to be in at the 90% percentile (qbinom)
    ')
  )

# Poisson and Related Distribution ----------------------------------------

# Poisson Distributions

rpois(100, lambda = 2)

set.seed(23598)

tibble(x = rpois(100, lambda = 3)) |>
  ggplot(aes(x)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 4) +
  geom_vline(xintercept = qpois(0.9, 3), colour = pink) +
  scale_x_continuous(breaks = 0:10) +
  labs(
    x = 'Number of sales in hour',
    y = 'Number of trials',
    title = 'Distribution of sales in hour from mean 3 per hour with 100 trials (rpois)',
    subtitle = glue('
    probability of 4 sales is {percent(dpois(4, 3))} (dpois)
    probability of <= 4 sales is {percent(ppois(4, 3))} (ppois)
    need {qpois(0.9, 3)} sales to be in at the 90% percentile (qpois)
    ')
  )


# Exponential Distribution

rexp(n = 100, rate = 0.2)

set.seed(23598)

tibble(x = rexp(100, rate = 0.2)) |>
  ggplot(aes(x)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 4) +
  geom_vline(xintercept = qexp(0.9, 0.2), colour = pink) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(
    x = 'Number of sales in hour',
    y = 'Number of trials',
    title = 'Distribution of exponential distribution with rate 20% and 100 trials (rexp)',
    subtitle = glue('
    probability of 4 is {percent(dexp(4, 0.2))} (dexp)
    probability of <= 4 is {percent(pexp(4, 0.2))} (pexp)
    need {comma(qexp(0.9, 0.2))} to be in at the 90% percentile (qexp)
    ')
  )


#  Weibull Distribution

rweibull(100, 1.5, 500)

set.seed(23598)

tibble(x = rweibull(100, 1.5, 500)) |>
  ggplot(aes(x)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 400) +
  geom_vline(xintercept = qweibull(0.9, 1.5, 500), colour = pink) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(
    x = 'x',
    y = 'Number of trials',
    title = 'Distribution of weibull distribution with xxx and 100 trials (rweibull)',
    subtitle = glue('
    probability of 400 is {percent(dweibull(400, 1.5, 500))} (dweibull)
    probability of <= 400 is {percent(pweibull(400, 1.5, 500))} (pweibull)
    need {comma(qweibull(0.9, 1.5, 500))} to be in at the 90% percentile (qweibull)
    ')
  )




# distribution function ---------------------------------------------------

dist_plot <- function(dist_name, n = 100, x, q, p, text_desc, ...) {

  d_val <- parse(text = paste0('d', dist_name, '(x, ...)')) |>
    eval()

  p_val <- parse(text = paste0('p', dist_name, '(q, ...)')) |>
    eval()

  q_val <- parse(text = paste0('q', dist_name, '(p, ...)')) |>
    eval()

  tibble(
    samp = parse(text = paste0('r', dist_name, '(n, ...)')) |>
      eval()
  ) |>
    ggplot(aes(samp)) +
    geom_histogram(binwidth = 1) +
    geom_vline(xintercept = x) +
    geom_vline(xintercept = q_val, colour = pink) +
    scale_x_continuous(breaks = pretty_breaks()) +
    labs(
      x = NULL,
      y = 'Number of trials',
      title = glue('{n} trials of {dist_name} with {text_desc}'),
      subtitle = glue('
    probability of {x} is {percent(d_val)} (d{dist_name})
    probability of <= {x} is {percent(p_val)} (p{dist_name})
    need {comma(q_val)} to be in at the {percent(p)} percentile (q{dist_name})
    ')
    )
}

dist_plot(
  'binom',
  x = 6, q = 6, p = 0.9,
  text_desc = 'size = 10, probability = 50%',
  size = 10, prob = 0.5
)

dist_plot(
  'norm',
  x = 42, q = 42, p = 0.9,
  text_desc = 'mean = 37, sd = 6',
  mean = 37, sd = 6
)

dist_plot(
  'pois',
  x = 4, q = 4, p = 0.9,
  text_desc = 'lambda = 3',
  lambda = 3
)

dist_plot(
  'exp',
  x = 10, q = 10, p = 0.9,
  text_desc = 'rate = 20%',
  rate = 0.2
)

dist_plot(
  'weibull',
  x = 7, q = 7, p = 0.9,
  text_desc = 'shape = 50%',
  shape = 0.5
)


#-------------------------------------------------------------------------------
