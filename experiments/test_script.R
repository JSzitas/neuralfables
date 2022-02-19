
remove(list = ls())

pkgload::load_all()
library(magrittr)


# AirPassengers

test <- tsibbledata::pelt$Lynx
# x_reg <- tsibbledata::pelt$Hare

# test <- tsibbledata::aus_production$Beer

test_size <- 76
plot.ts(test)

### gbm
gbm <- train_gbm(test[1:test_size], lags = c(1:5), intercept = FALSE, index_dummy = FALSE, seas_dummy = TRUE)
fcst <- forecast.GBM(gbm, h = length(test) - test_size) # , xreg = as.matrix(x_reg[(test_size+1):length(x_reg)] ))
fcst %>%
  c(rep(NA, test_size), .) %>%
  lines(col = "red")



### gam
# gam <- train_gam(test[1:test_size], lags = c(1:5), intercept = FALSE, index_dummy = FALSE, seas_dummy = TRUE)
# fcst <- forecast.GAM(gam, h = length(test) - test_size) # , xreg = as.matrix(x_reg[(test_size+1):length(x_reg)] ))
# fcst %>%
#   c(rep(NA, test_size), .) %>%
#   lines(col = "red")

# ### ts GLM
# glm <- train_glm( test[1:test_size], index_dummy = FALSE, lags = c(1:12), lambda = NULL, family = "gaussian" )#, xreg = x_reg[1:test_size] )
# fcst <- forecast.tsGLM( glm, h = length(test)-test_size)#, xreg = as.matrix(x_reg[(test_size+1):length(x_reg)] ))
# fcst %>%
#   c(rep(NA,test_size),.) %>%
#   lines(col = "brown")

## MLP
# mlp <- train_mlp( test[1:test_size], index_dummy = FALSE, lags = c(1,2,4), reps = 20)#, xreg = x_reg[1:test_size] )
# fcst <- forecast.MLP( mlp, h = length(test)-test_size)#, xreg = as.matrix(x_reg[(test_size+1):length(x_reg)] ))
# fcst %>%
#   c(rep(NA,test_size),.) %>%
#   lines(col = "brown")

# ## MLP w xreg
# mlp <- train_mlp( test[1:test_size], index_dummy = FALSE, lags = c(1,2,4), reps = 20, xreg = x_reg[1:test_size] )
# fcst <- forecast.MLP( mlp, h = length(test)-test_size, xreg = as.matrix(x_reg[(test_size+1):length(x_reg)] ))
# fcst %>%
#   c(rep(NA,test_size),.) %>%
#   lines(col = "green")


### ELM
elm <- train_elm( test[1:test_size], lags = c(1,2,4), n_hidden = 25, reps = 45, lambda = NULL)#, n_diffs = 0)
fcst <- forecast.ELM(elm, h = length(test)-test_size )
plot.ts(test)
fcst %>%
  c(rep(NA,test_size),.) %>%
  lines(col = "blue")
#
# ## garbage
# sink <- train_garbage( test[1:test_size], index_dummy = FALSE, lags = c(1,2,4), n_garbage = 20)
# fcst <- forecast.garbage(sink, h = length(test)-test_size )
# # plot.ts(test)
# fcst %>%
#   c(rep(NA,test_size),.) %>%
#   lines(col = "red")
#
# # ESN
# # plot.ts(test)
# esn <- train_esn( test[1:test_size],lambda = 1e-2, alpha = 0.3, n_nodes = 500, spectral_radius = 0.75,
#                   # , scaler = scaler_identity, inv_scaler = scaler_inverse_identity, scaler_args = list()
#                   )
# fcst <- forecast.ESN( esn, h = length(test)-test_size )
# fcst %>%
#   c(rep(NA,test_size),.) %>%
#   lines(col = "grey")

### lstm ESN
# lstm_esn <- train_lstm_esn( test[1:test_size] )
