## ----setup--------------------------------------------------------------------
#| code-fold: true
#| code-summary: "Show preliminaries"
library(stLMM)
library(ggplot2)

source(file.path(dirname(knitr::current_input(dir = TRUE)), "utils.R"))

set.seed(1)


## ----plot-theme, include=FALSE------------------------------------------------
theme_set(theme_bw(base_size = 12))


## ----simulate-data------------------------------------------------------------
n_train <- 90
n_holdout <- 10
n <- n_train + n_holdout

dat <- data.frame(
  lon = runif(n),
  lat = runif(n),
  x = runif(n, -1, 1)
)

beta <- c(0.5, 1)
sigma_sq <- 1
phi <- 4
tau_sq <- 0.1

C <- exp_cov(
  coords = dat[, c("lon", "lat")],
  sigma_sq = sigma_sq,
  phi = phi
)

dat$w_true <- rmvnorm(mean = rep(0, n), Sigma = C)

mu <- beta[1] + beta[2] * dat$x + dat$w_true
epsilon <- rnorm(n, sd = sqrt(tau_sq))
dat$y <- mu + epsilon

train <- dat[1:n_train, ]
holdout <- dat[(n_train + 1):n, ]


## ----spatial-data-plot, fig.width=6, fig.height=4.8---------------------------
#| code-fold: true
#| code-summary: "Show plotting code"
plot_dat <- rbind(
  data.frame(train, sample = "training"),
  data.frame(holdout, sample = "holdout")
)

ggplot(plot_dat, aes(lon, lat)) +
  geom_point(
    aes(color = y, shape = sample),
    size = 2.2
  ) +
  scale_color_gradientn(colors = stlmm_palette()) +
  coord_equal() +
  labs(
    x = "longitude",
    y = "latitude",
    color = "response",
    shape = NULL
  )


## ----fit-nngp-----------------------------------------------------------------
fit <- stLMM(
  y ~ x + nngp(lon, lat, m = 15, cov_model = "exp", ordering = "maxmin"),
  data = train,
  priors = list(
    resid = list(tau_sq = half_t(df = 3, scale = 0.5)),
    nngp_1 = list(
      sigma_sq = half_t(df = 3, scale = 1),
      phi = uniform(2, 30)
    )
  ),
  n_samples = 500,
  verbose = FALSE
)

summary(fit)


## ----recover-nngp-------------------------------------------------------------
rec <- recover(fit, sub_sample = list(start = 201, thin = 2))

rec


## ----recovery-plot, fig.width=5.5, fig.height=4.8-----------------------------
#| code-fold: true
#| code-summary: "Show plotting code"
rec_draws <- as_samples(rec, include_w = TRUE, metadata = FALSE)
w_cols <- paste0("w_nngp_1_", seq_len(nrow(train)))
w_hat <- colMeans(rec_draws[, w_cols, drop = FALSE])
beta_0_hat <- mean(rec_draws[["(Intercept)"]])

recovery_dat <- data.frame(
  truth = beta[1] + train$w_true,
  estimate = beta_0_hat + w_hat
)

ggplot(recovery_dat, aes(truth, estimate)) +
  geom_point(
    color = stlmm_color("primary"),
    size = 2
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = stlmm_color("secondary"),
    linewidth = 0.8
  ) +
  coord_equal() +
  labs(
    x = "true spatially varying intercept",
    y = "posterior mean estimate"
  )


## ----predict-holdout----------------------------------------------------------
holdout_new <- holdout[, c("lon", "lat", "x")]

pred <- predict(
  rec,
  newdata = holdout_new,
  y_samples = TRUE,
  joint = FALSE,
  return_w_samples = FALSE
)

summary(pred)


## ----prediction-plot, fig.width=5.5, fig.height=4.8---------------------------
#| code-fold: true
#| code-summary: "Show plotting code"
pred_draws <- as_samples(pred, sample = "all", metadata = FALSE)
mu_cols <- paste0("mu_", seq_len(nrow(holdout_new)))
y_cols <- paste0("y_", seq_len(nrow(holdout_new)))
mu_hat <- colMeans(pred_draws[, mu_cols, drop = FALSE])
y_hat <- colMeans(pred_draws[, y_cols, drop = FALSE])

pred_dat <- data.frame(
  y = holdout$y,
  mu_hat = mu_hat,
  y_hat = y_hat
)
pred_lim <- range(pred_dat$y, pred_dat$y_hat)

ggplot(pred_dat, aes(y, y_hat)) +
  geom_point(
    color = stlmm_color("primary"),
    size = 2
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = stlmm_color("secondary"),
    linewidth = 0.8
  ) +
  coord_equal(
    xlim = pred_lim,
    ylim = pred_lim
  ) +
  labs(
    x = "held-out response",
    y = "posterior predictive mean"
  )


## ----fit-gp-------------------------------------------------------------------
gp_fit <- stLMM(
  y ~ x + gp(lon, lat, cov_model = "exp"),
  data = train,
  priors = list(
    resid = list(tau_sq = half_t(df = 3, scale = 0.5)),
    gp_1 = list(
      sigma_sq = half_t(df = 3, scale = 1),
      phi = uniform(2, 30)
    )
  ),
  n_samples = 500,
  verbose = FALSE
)

summary(gp_fit)

