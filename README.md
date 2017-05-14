"Bayesian" Fisher Exact Test in Stan
================
TJ Mahr
May 14, 2017

This repository is an example I'm borrowing from [this talk by Bob Carpenter](https://www.youtube.com/watch?v=qQFF4tPgeWI).

Problem
-------

We observed the following data.

``` r
library(tibble)
library(dplyr, warn.conflicts = FALSE)

df <- tribble(
  ~ Sex, ~ Handedness, ~ n,
  "male", "left",  9,
  "male", "right", 43,
  "female", "left", 4,
  "female", "right", 44
)

knitr::kable(df)
```

| Sex    | Handedness |    n|
|:-------|:-----------|----:|
| male   | left       |    9|
| male   | right      |   43|
| female | left       |    4|
| female | right      |   44|

Is the rate of left-handedness different between men and women?

I'm not sure what this test is doing, but it's what one would do in classical statistics.

``` r
# Create a matrix for the test
m <- tidyr::spread(df, Handedness, n) %>% 
  as.data.frame()
rownames(m) <- m$Sex
m$Sex <- NULL
m
#>        left right
#> female    4    44
#> male      9    43

fisher.test(m)
#> 
#>  Fisher's Exact Test for Count Data
#> 
#> data:  m
#> p-value = 0.2392
#> alternative hypothesis: true odds ratio is not equal to 1
#> 95 percent confidence interval:
#>  0.09150811 1.71527769
#> sample estimates:
#> odds ratio 
#>  0.4378606
fisher.test(m, alternative = "less")
#> 
#>  Fisher's Exact Test for Count Data
#> 
#> data:  m
#> p-value = 0.1502
#> alternative hypothesis: true odds ratio is less than 1
#> 95 percent confidence interval:
#>  0.000000 1.427233
#> sample estimates:
#> odds ratio 
#>  0.4378606
```

Fit the model in Stan
---------------------

But I know Stan so I can compute a posterior probability of the difference between men and women.

I think that 10% of people are left handed. I don't know where I heard this number first, but it'll serve as my prior information. I messed around with `shape1` and `shape2` until I got the follow prior, which is peaked around .1-ish and keeps .5 and .15 as still plausible values.

``` r
plot(density(rbeta(100000, shape1 = 9, shape2 = 80)))
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

I write out a really simple model in Stan.

``` stan
# This knitr block also creates a `stan_model()` object in R called `stan_model`
data {
  int<lower=0> n_total_1;
  int<lower=0> n_total_2;
  int<lower=0> n_hits_1;
  int<lower=0> n_hits_2;
}
parameters { 
  real<lower=0, upper=1> theta_1;
  real<lower=0, upper=1> theta_2;
}
model {
  theta_1 ~ beta(9, 80);
  theta_2 ~ beta(9, 80);
  n_hits_1 ~ binomial(n_total_1, theta_1);
  n_hits_2 ~ binomial(n_total_2, theta_2);
}
generated quantities {
  real diff;
  diff = theta_1 - theta_2;
}
```

And fit it.

``` r
stan_data <- list(
  n_total_1 = 9 + 43,
  n_total_2 = 3 + 44,
  n_hits_1 = 9,
  n_hits_2 = 4
)

library(rstan)
#> Loading required package: ggplot2
#> Loading required package: StanHeaders
#> rstan (Version 2.15.1, packaged: 2017-04-19 05:03:57 UTC, GitRev: 2e1f913d3ca3)
#> For execution on a local, multicore CPU with excess RAM we recommend calling
#> rstan_options(auto_write = TRUE)
#> options(mc.cores = parallel::detectCores())
model <- sampling(stan_model, data = stan_data)
#> 
#> SAMPLING FOR MODEL '4cc80693f932c3135fe76eedf46fe324' NOW (CHAIN 1).
#> 
#> Gradient evaluation took 0 seconds
#> 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
#> Adjust your expectations accordingly!
#> 
#> 
#> Iteration:    1 / 2000 [  0%]  (Warmup)
#> Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Iteration: 2000 / 2000 [100%]  (Sampling)
#> 
#>  Elapsed Time: 0.025 seconds (Warm-up)
#>                0.016 seconds (Sampling)
#>                0.041 seconds (Total)
#> 
#> 
#> SAMPLING FOR MODEL '4cc80693f932c3135fe76eedf46fe324' NOW (CHAIN 2).
#> 
#> Gradient evaluation took 0 seconds
#> 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
#> Adjust your expectations accordingly!
#> 
#> 
#> Iteration:    1 / 2000 [  0%]  (Warmup)
#> Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Iteration: 2000 / 2000 [100%]  (Sampling)
#> 
#>  Elapsed Time: 0.02 seconds (Warm-up)
#>                0.027 seconds (Sampling)
#>                0.047 seconds (Total)
#> 
#> 
#> SAMPLING FOR MODEL '4cc80693f932c3135fe76eedf46fe324' NOW (CHAIN 3).
#> 
#> Gradient evaluation took 0 seconds
#> 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
#> Adjust your expectations accordingly!
#> 
#> 
#> Iteration:    1 / 2000 [  0%]  (Warmup)
#> Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Iteration: 2000 / 2000 [100%]  (Sampling)
#> 
#>  Elapsed Time: 0.004 seconds (Warm-up)
#>                0.045 seconds (Sampling)
#>                0.049 seconds (Total)
#> 
#> 
#> SAMPLING FOR MODEL '4cc80693f932c3135fe76eedf46fe324' NOW (CHAIN 4).
#> 
#> Gradient evaluation took 0 seconds
#> 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
#> Adjust your expectations accordingly!
#> 
#> 
#> Iteration:    1 / 2000 [  0%]  (Warmup)
#> Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Iteration: 2000 / 2000 [100%]  (Sampling)
#> 
#>  Elapsed Time: 0.04 seconds (Warm-up)
#>                0.022 seconds (Sampling)
#>                0.062 seconds (Total)
model
#> Inference for Stan model: 4cc80693f932c3135fe76eedf46fe324.
#> 4 chains, each with iter=2000; warmup=1000; thin=1; 
#> post-warmup draws per chain=1000, total post-warmup draws=4000.
#> 
#>           mean se_mean   sd    2.5%    25%    50%    75%  97.5% n_eff Rhat
#> theta_1   0.13    0.00 0.03    0.08   0.11   0.13   0.15   0.19  3626    1
#> theta_2   0.09    0.00 0.03    0.05   0.08   0.09   0.11   0.15  3015    1
#> diff      0.03    0.00 0.04   -0.04   0.01   0.03   0.06   0.11  3088    1
#> lp__    -97.76    0.02 1.01 -100.44 -98.16 -97.46 -97.02 -96.76  1761    1
#> 
#> Samples were drawn using NUTS(diag_e) at Sun May 14 13:32:54 2017.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
```

To get the Bayesian p-value (to compare the two procedures).

``` r
draws <- rstan::extract(model)

# p(men are more left handed than women)
mean(draws$diff > 0)
#> [1] 0.80975

# p("null" hypthosis)
mean(draws$diff <= 0)
#> [1] 0.19025
```

Use ggmcmc is to visualize the effects.

``` r
library(ggmcmc)
#> Loading required package: tidyr
#> 
#> Attaching package: 'tidyr'
#> The following object is masked from 'package:rstan':
#> 
#>     extract

# Get ggmcmc's tidy data of the model
ggs <- ggs(model)

ci(ggs, thick_ci = c(0.05, 0.95), thin_ci = c(0.025, 0.975)) %>% 
  knitr::kable(digits = 2)
```

| Parameter |    low|    Low|  median|  High|  high|
|:----------|------:|------:|-------:|-----:|-----:|
| diff      |  -0.04|  -0.03|    0.03|  0.10|  0.11|
| theta\_1  |   0.08|   0.08|    0.13|  0.18|  0.19|
| theta\_2  |   0.05|   0.06|    0.09|  0.14|  0.15|

``` r

ggs_density(ggs) + facet_grid(Parameter ~ .)
```

![](README_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r

ggs_caterpillar(ggs, line = 0)
```

![](README_files/figure-markdown_github/unnamed-chunk-7-2.png)

Try it out with a regression model
----------------------------------

``` r
library(rstanarm)
#> Loading required package: Rcpp
#> rstanarm (Version 2.15.3, packaged: 2017-04-29 06:18:44 UTC)
#> - Do not expect the default priors to remain the same in future rstanarm versions.
#> Thus, R scripts should specify priors explicitly, even if they are just the defaults.
#> - For execution on a local, multicore CPU with excess RAM we recommend calling
#> options(mc.cores = parallel::detectCores())
dfw <- tidyr::spread(df, Handedness, n)
reg <- stan_glm(
  cbind(left, right) ~ Sex, 
  family = binomial, 
  data = dfw)
#> 
#> SAMPLING FOR MODEL 'binomial' NOW (CHAIN 1).
#> 
#> Gradient evaluation took 0 seconds
#> 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
#> Adjust your expectations accordingly!
#> 
#> 
#> Iteration:    1 / 2000 [  0%]  (Warmup)
#> Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Iteration: 2000 / 2000 [100%]  (Sampling)
#> 
#>  Elapsed Time: 0.048 seconds (Warm-up)
#>                0.057 seconds (Sampling)
#>                0.105 seconds (Total)
#> 
#> 
#> SAMPLING FOR MODEL 'binomial' NOW (CHAIN 2).
#> 
#> Gradient evaluation took 0 seconds
#> 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
#> Adjust your expectations accordingly!
#> 
#> 
#> Iteration:    1 / 2000 [  0%]  (Warmup)
#> Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Iteration: 2000 / 2000 [100%]  (Sampling)
#> 
#>  Elapsed Time: 0.054 seconds (Warm-up)
#>                0.051 seconds (Sampling)
#>                0.105 seconds (Total)
#> 
#> 
#> SAMPLING FOR MODEL 'binomial' NOW (CHAIN 3).
#> 
#> Gradient evaluation took 0 seconds
#> 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
#> Adjust your expectations accordingly!
#> 
#> 
#> Iteration:    1 / 2000 [  0%]  (Warmup)
#> Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Iteration: 2000 / 2000 [100%]  (Sampling)
#> 
#>  Elapsed Time: 0.057 seconds (Warm-up)
#>                0.043 seconds (Sampling)
#>                0.1 seconds (Total)
#> 
#> 
#> SAMPLING FOR MODEL 'binomial' NOW (CHAIN 4).
#> 
#> Gradient evaluation took 0 seconds
#> 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
#> Adjust your expectations accordingly!
#> 
#> 
#> Iteration:    1 / 2000 [  0%]  (Warmup)
#> Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Iteration: 2000 / 2000 [100%]  (Sampling)
#> 
#>  Elapsed Time: 0.06 seconds (Warm-up)
#>                0.051 seconds (Sampling)
#>                0.111 seconds (Total)
summary(reg)
#> 
#> Model Info:
#> 
#>  function:  stan_glm
#>  family:    binomial [logit]
#>  formula:   cbind(left, right) ~ Sex
#>  algorithm: sampling
#>  priors:    see help('prior_summary')
#>  sample:    4000 (posterior sample size)
#>  num obs:   2
#> 
#> Estimates:
#>                 mean   sd    2.5%   25%   50%   75%   97.5%
#> (Intercept)    -2.5    0.5  -3.6   -2.8  -2.4  -2.1  -1.5  
#> Sexmale         0.9    0.6  -0.4    0.4   0.8   1.3   2.2  
#> mean_PPD        6.5    2.3   2.5    5.0   6.5   8.0  11.5  
#> log-posterior  -8.8    1.0 -11.4   -9.2  -8.4  -8.0  -7.8  
#> 
#> Diagnostics:
#>               mcse Rhat n_eff
#> (Intercept)   0.0  1.0  1906 
#> Sexmale       0.0  1.0  2161 
#> mean_PPD      0.0  1.0  3095 
#> log-posterior 0.0  1.0  1579 
#> 
#> For each parameter, mcse is Monte Carlo standard error, n_eff is a crude measure of effective sample size, and Rhat is the potential scale reduction factor on split chains (at convergence Rhat=1).

pp_reg <- posterior_linpred(reg) %>% 
  as.data.frame() %>% 
  setNames(c("female_left", "male_left")) %>% 
  mutate(female_rate = plogis(female_left),
         male_rate = plogis(male_left), 
         diff = male_rate - female_rate)

round(quantile(pp_reg$diff, c(.05, .1, .25, .5, .75, .9, .95)), 2)
#>    5%   10%   25%   50%   75%   90%   95% 
#> -0.02  0.01  0.04  0.08  0.13  0.17  0.19

pp_reg_long <- pp_reg %>% 
  select(male_rate, female_rate, diff) %>% 
  tidyr::gather(variable, value)

ggplot(pp_reg_long) + 
  aes(x = value) + 
  geom_density() + 
  facet_grid(variable ~ .)
```

![](README_files/figure-markdown_github/unnamed-chunk-8-1.png)
