## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)

## ----setup--------------------------------------------------------------------
library(BiasedMatch)

## ----table1-------------------------------------------------------------------
d1 <- DISTM(N1 = 37, N2 = 45, N = 16, m1 = 12, m2 = 8, w = 1)
d2 <- DISTM(N1 = 37, N2 = 45, N = 16, m1 = 12, m2 = 8, w = 2)

round(
  data.frame(
    m = d1$mvec,
    `w = 1` = d1$fmvec,
    `w = 2` = d2$fmvec,
    check.names = FALSE
  ),
  4
)

## ----pmf-plot-----------------------------------------------------------------
plot(d1$mvec, d1$fmvec,
  type = "b", pch = 16, ylim = c(0, 0.45),
  xlab = "Number of matches (m)", ylab = "Probability"
)
lines(d2$mvec, d2$fmvec, type = "b", pch = 17, lty = 2)
legend("topright", c("w = 1", "w = 2"), pch = c(16, 17), lty = c(1, 2), bty = "n")

## ----vmf----------------------------------------------------------------------
c(closed_form = Vmf(N1 = 37, N2 = 45, N = 16, m1 = 12, m2 = 8), exact = d1$Vm)

## ----problem3-----------------------------------------------------------------
fit <- inferW(m = 13, N1 = 292, N2 = 350, N = 192, m1 = 90, m2 = 59)
fit

## ----problem3-curve-----------------------------------------------------------
curves <- likW(
  m = 13, N1 = 292, N2 = 350, N = 192, m1 = 90, m2 = 59,
  wgrid = seq(0.05, 8, by = 0.05)
)
plot(curves$w, curves$likelihood,
  type = "l",
  xlab = "w", ylab = "standardised value"
)
lines(curves$w, curves$posterior, lty = 2)
abline(v = c(fit$mle, fit$posterior_mode), col = "grey50", lty = 3)
legend("topright", c("likelihood", "posterior kernel"), lty = c(1, 2), bty = "n")

## ----problem2-----------------------------------------------------------------
inferW(m = 2, N1 = 52, N2 = 52, N = 22, m1 = 9, m2 = 6)

## ----problem2-table-----------------------------------------------------------
B_values <- 0:6
tab <- do.call(rbind, lapply(B_values, function(B) {
  r <- inferW(m = B, N1 = 52, N2 = 52, N = 22, m1 = 9, m2 = 6)
  data.frame(B = B, MLE = r$mle, posterior_mode = r$posterior_mode, p_value = r$p_value)
}))
round(tab, 4)

## ----mcmc, eval = FALSE-------------------------------------------------------
# set.seed(4172)
# res <- MHALG(
#   B = 500, J = 5000,
#   N1vec = 37, N2vec = 45, N0vec = 16,
#   M1vec = 12, M2vec = 8, mvec = 3,
#   Cvec = 1, del = 0.1
# )
# restable(res, Bn = 500)
# estprop(res, Bn = 500, Jn = 4000)

