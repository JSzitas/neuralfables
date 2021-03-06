remove(list=ls())

pkgload::load_all()
test <- tsibbledata::pelt
library(magrittr)

train <- test %>%
  dplyr::filter( Year < 1920 )
train2 <- test %>%
  dplyr::filter( Year < 1925 )
test <- test %>%
  dplyr::filter(Year >=1920)

fit_models <- train %>%
  fabletools::model( mlp = neuralfables::neuralfable( Lynx ~ method("mlp") +
                                                        parameters( lags = c(1,2,4), n_hidden = 15 ))#,
                     # elm = neuralfables::neuralfable( Lynx ~ method("elm") +
                     #                                    parameters( lags = c(1,2,4), n_hidden = 20 )),
                     # gbm = neuralfables::neuralfable( Lynx ~ method("mboost"))
                     )

# acc <- fabletools::accuracy(fit_models)
# fcst <- fabletools::forecast( fit_models, h = 16 )
# acc_test <- fabletools::accuracy(fcst, test)
# library(fabletools)
# autoplot(fcst, dplyr::bind_rows(train,test))

refitted <- refit(fit_models, train2)
