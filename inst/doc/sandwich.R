## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>",
  eval = requireNamespace("sandwich", quietly = TRUE) &&
         requireNamespace("lmtest",   quietly = TRUE)
)

## ----setup, message = FALSE---------------------------------------------------
library(parglm)
library(sandwich)
library(lmtest)

## ----simulate-----------------------------------------------------------------
set.seed(1)
n          <- 200
cluster_id <- rep(1:20, each = 10)
x1         <- rnorm(n)
x2         <- rnorm(n)
u          <- rep(rnorm(20, sd = 0.5), each = 10)  # cluster random effect
y          <- rpois(n, exp(0.5 + 0.3 * x1 - 0.2 * x2 + u))
dat        <- data.frame(y = y, x1 = x1, x2 = x2, cluster = cluster_id)

## ----fit----------------------------------------------------------------------
fit <- parglm(y ~ x1 + x2, data = dat, family = poisson(),
              control = parglm.control(nthreads = 1L))

## ----model-based--------------------------------------------------------------
coeftest(fit)

## ----hc-----------------------------------------------------------------------
coeftest(fit, vcov = vcovHC)

## ----cluster------------------------------------------------------------------
coeftest(fit, vcov = vcovCL, cluster = ~cluster)

## ----vcov---------------------------------------------------------------------
vcovHC(fit, type = "HC3")
vcovCL(fit, cluster = ~cluster)

## ----gtsummary-setup, eval = requireNamespace("gtsummary", quietly = TRUE) && requireNamespace("broom", quietly = TRUE) && requireNamespace("broom.helpers", quietly = TRUE), message = FALSE----
library(gtsummary)

## ----gtsummary-logistic, eval = requireNamespace("gtsummary", quietly = TRUE) && requireNamespace("broom", quietly = TRUE) && requireNamespace("broom.helpers", quietly = TRUE)----
set.seed(2)
n2    <- 300
x1_b  <- rnorm(n2)
x2_b  <- rnorm(n2)
y_bin <- rbinom(n2, 1, plogis(0.4 + 0.6 * x1_b - 0.4 * x2_b))
dat_b <- data.frame(y = y_bin, x1 = x1_b, x2 = x2_b)

fit_logistic <- parglm(y ~ x1 + x2, data = dat_b, family = binomial(),
                       control = parglm.control(nthreads = 1L))

suppressWarnings(
  tbl_regression(fit_logistic, exponentiate = TRUE)
)

## ----gtsummary-robust, eval = requireNamespace("gtsummary", quietly = TRUE) && requireNamespace("broom", quietly = TRUE) && requireNamespace("broom.helpers", quietly = TRUE)----
tbl_regression(fit, tidy_fun = tidy_parglm_robust)

