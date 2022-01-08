
remove(list=ls())

pkgload::load_all()
test <- tsibbledata::pelt$Lynx
library(magrittr)

test_size = 75

### MLP
# mlp <- MLP( test[1:test_size], index_dummy = FALSE, lags = c(1,2,4), reps = 20 )
# fcst <- forecast.MLP( mlp, h = length(test)-test_size )
# fcst %>%
#   c(rep(NA,test_size),.) %>%
#   lines(col = "red")
#
# benchmark <- nnfor::mlp( ts(test[1:test_size]) )
# benchmark_fcst <- benchmark %>%
#   nnfor:::forecast.mlp(h = length(test)-test_size)
#
# benchmark_fcst[["mean"]] %>%
#   c(rep(NA,test_size),.) %>%
#   lines(col = "blue")

### ELM
# elm <- ELM( test[1:test_size], index_dummy = FALSE, lags = c(1,2,4), n_hidden = 20)
# fcst <- forecast.ELM(elm, h = length(test)-test_size )
# plot.ts(test)
# fcst %>%
#   c(rep(NA,test_size),.) %>%
#   lines(col = "red")

### garbage
sink <- garbage( test[1:test_size], index_dummy = FALSE, lags = c(1,2,4), n_garbage = 20)
fcst <- forecast.garbage(sink, h = length(test)-test_size )
plot.ts(test)
fcst %>%
  c(rep(NA,test_size),.) %>%
  lines(col = "red")



### ESN
# esn <- ESN( test[1:test_size],lambda = 1, n_nodes = 10 )
#
#
# test %>% plot.ts
# forecast.ESN( esn, h = length(test)-test_size ) %>%
#   c(rep(NA,test_size),.) %>%
#   lines(col = "red")


