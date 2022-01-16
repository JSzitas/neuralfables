
remove(list=ls())

pkgload::load_all()
test <- tsibbledata::pelt$Lynx
library(magrittr)

test_size = 75
# plot.ts(test)

### MLP
mlp <- train_mlp( test[1:test_size], index_dummy = FALSE, lags = c(1,2,4), reps = 20 )
fcst <- forecast.MLP( mlp, h = length(test)-test_size )
fcst %>%
  c(rep(NA,test_size),.) %>%
  lines(col = "brown")

### ELM
elm <- train_elm( test[1:test_size], index_dummy = FALSE, lags = c(1,2,4), n_hidden = 20, lambda = 0.05, family = "gaussian")
fcst <- forecast.ELM(elm, h = length(test)-test_size )
# plot.ts(test)
fcst %>%
  c(rep(NA,test_size),.) %>%
  lines(col = "blue")

## garbage
sink <- train_garbage( test[1:test_size], index_dummy = FALSE, lags = c(1,2,4), n_garbage = 20)
fcst <- forecast.garbage(sink, h = length(test)-test_size )
# plot.ts(test)
fcst %>%
  c(rep(NA,test_size),.) %>%
  lines(col = "red")

# ESN
# plot.ts(test)
esn <- train_esn( test[1:test_size],lambda = 1e-2, alpha = 0.3, n_nodes = 500, spectral_radius = 0.75,
                  # , scaler = scaler_identity, inv_scaler = scaler_inverse_identity, scaler_args = list()
                  )
fcst <- forecast.ESN( esn, h = length(test)-test_size )
fcst %>%
  c(rep(NA,test_size),.) %>%
  lines(col = "grey")



