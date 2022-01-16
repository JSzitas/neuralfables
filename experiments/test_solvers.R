# test solvers

remove(list=ls())

pkgload::load_all()
test <- tsibbledata::pelt$Lynx
library(magrittr)

warpbreaks %>%
  categoryEncodings::encoder(fact = c("wool", "tension"), method = "dummy") -> enc


poisson_ridge_wls(
  y = enc[["encoded"]]$breaks,
  X = cbind(1, as.matrix(enc[["encoded"]][, -c("breaks")])),
  reg_lambda = 1,
  tol = 1e-4
) -> pars


ridge_solver(
  y = enc[["encoded"]]$breaks,
  X = cbind(1, as.matrix(enc[["encoded"]][, -c("breaks")])),
  lambda = 1,
  family = "poisson",
  tol = 1e-4
) -> pars2
