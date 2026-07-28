## ----setup--------------------------------------------------------------------
#| code-fold: true
#| code-summary: "Show preliminaries"
library(stLMM)
library(ggplot2)

source(file.path(dirname(knitr::current_input(dir = TRUE)), "utils.R"))

set.seed(1)


## ----plot-theme, include=FALSE------------------------------------------------
theme_set(theme_bw(base_size = 12))


## ----fixed-data---------------------------------------------------------------
n <- 50
dat <- data.frame(x = seq(-1, 1, length.out = n))

beta <- c(0.5, -0.5)
tau_sq <- 0.2^2

mu <- beta[1] + beta[2] * dat$x
epsilon <- rnorm(n, sd = sqrt(tau_sq))
dat$y <- mu + epsilon


## ----fixed-data-plot, fig.width=6, fig.height=4-------------------------------
#| code-fold: true
#| code-summary: "Show plotting code"
ggplot(dat, aes(x, y)) +
  geom_point(
    color = stlmm_color("primary"),
    size = 2
  ) +
  labs(x = "x", y = "response")


## ----fixed-fit----------------------------------------------------------------
fit <- stLMM(
  y ~ x,
  data = dat,
  priors = list(resid = list(tau_sq = half_t(df = 3, scale = 0.5))),
  n_samples = 250,
  verbose = FALSE
)

summary(fit)


## ----fixed-fitted-------------------------------------------------------------
mu_hat <- fitted(fit)

head(mu_hat)


## ----fixed-fitted-plot, fig.width=6, fig.height=4-----------------------------
#| code-fold: true
#| code-summary: "Show plotting code"
plot_dat <- data.frame(
  x = dat$x,
  y = dat$y,
  fitted = as.numeric(mu_hat)
)

ggplot(plot_dat, aes(x, y)) +
  geom_point(
    color = stlmm_color("primary"),
    size = 2
  ) +
  geom_line(
    aes(y = fitted),
    color = stlmm_color("secondary"),
    linewidth = 0.8
  ) +
  labs(x = "x", y = "response")


## ----fixed-predict------------------------------------------------------------
new_dat <- data.frame(x = c(-0.75, 0, 0.75))

pred <- predict(fit, newdata = new_dat, y_samples = TRUE)

summary(pred)


## ----grouped-data-------------------------------------------------------------
n_group <- 5
n_per_group <- 10
group_dat <- data.frame(
  group = factor(rep(letters[1:n_group], each = n_per_group)),
  x = rep(seq(-1, 1, length.out = n_per_group), n_group)
)

sigma_sq_alpha <- 0.5^2
alpha_true <- rnorm(n_group, sd = sqrt(sigma_sq_alpha))
names(alpha_true) <- levels(group_dat$group)

beta <- c(0.5, -0.5)
mu <- beta[1] + beta[2] * group_dat$x +
  alpha_true[as.character(group_dat$group)]

tau_sq <- 0.25^2
epsilon <- rnorm(nrow(group_dat), sd = sqrt(tau_sq))
group_dat$y <- mu + epsilon


## ----grouped-fit--------------------------------------------------------------
group_fit <- stLMM(
  y ~ x + iid(group),
  data = group_dat,
  priors = list(
    resid = list(tau_sq = half_t(df = 3, scale = 0.5)),
    iid_1 = list(sigma_sq = ig(shape = 3, scale = 0.25))
  ),
  n_samples = 250,
  verbose = FALSE
)

summary(group_fit)


## ----grouped-predict----------------------------------------------------------
group_new <- data.frame(
  group = factor(c("a", "c", "e"), levels = levels(group_dat$group)),
  x = c(-0.5, 0, 0.5)
)

group_pred <- predict(group_fit, newdata = group_new)

summary(group_pred)

