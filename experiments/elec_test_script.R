remove(list=ls())

pkgload::load_all()
library(magrittr)

# hard test data (relatively speaking :)
electricity <- tsibbledata::vic_elec %>%
  dplyr::mutate( hour = lubridate::hour(Time),
                 weekday = lubridate::wday(Time))

train <- electricity %>%
  dplyr::filter( Time <= lubridate::dmy( "30-07-2014" ))
test <- electricity %>%
  dplyr::filter( Time > lubridate::dmy( "30-07-2014" ) & Time < lubridate::dmy( "05-08-2014" ))

test_week <- electricity %>%
  dplyr::filter( Time > lubridate::dmy( "25-07-2014" ) & Time < lubridate::dmy( "05-08-2014" ))


fit_models <- train %>%
  fabletools::model( mboost = neuralfables::neuralfable( Demand ~ method("mboost") + Temperature + Holiday +
                                                        hour + weekday +
                                                        parameters( lags = c(1:48),
                                                                    boost_control = mboost::boost_control( mstop = 250,
                                                                                                           nu = 0.1),
                                                                    tree_controls = partykit::ctree_control(
                                                                      teststat = "quad", testtype = "Teststatistic",
                                                                      mincriterion = 0, minsplit = 10, minbucket = 4,
                                                                      maxdepth = 5, saveinfo = FALSE),
                                                                    family = mboost::Huber(),
                                                                    seas_dummy = FALSE,
                                                                    n_diffs = 0) ),
                     xgboost = neuralfables::neuralfable( Demand ~ method("xgboost") + Temperature + Holiday +
                                                           hour + weekday +
                                                           parameters( lags = c(1:48),
                                                                       xgboost_control = list(
                                                                         max_depth = 5, eta = 0.3, nrounds = 200,
                                                                         nthread = 1, objective = "reg:tweedie", verbose = 0
                                                                       ),
                                                                       seas_dummy = FALSE,
                                                                       n_diffs = 0) )
                     )

fit_models2 <- train %>%
  fabletools::model( xgboost2 = neuralfables::neuralfable( Demand ~ method("xgboost") + Temperature + Holiday +
                                                            hour + weekday +
                                                            parameters( lags = c(1:48),
                                                                        xgboost_control = list(
                                                                          max_depth = 5, eta = 0.3, nrounds = 350,
                                                                          nthread = 5, objective = "reg:tweedie", verbose = 0
                                                                        ),
                                                                        seas_dummy = FALSE,
                                                                        n_diffs = 0) ),
                     xgboost3 = neuralfables::neuralfable( Demand ~ method("xgboost") + Temperature + Holiday +
                                                             hour + weekday +
                                                             parameters( lags = c(1:48),
                                                                         xgboost_control = list(
                                                                           max_depth = 5, eta = 0.3, nrounds = 350,
                                                                           nthread = 5, objective = "reg:pseudohubererror", verbose = 0
                                                                         ),
                                                                         seas_dummy = FALSE,
                                                                         n_diffs = 0) ),
                     xgboost4 = neuralfables::neuralfable( Demand ~ method("xgboost") + Temperature + Holiday +
                                                             hour + weekday +
                                                             parameters( lags = c(1:48),
                                                                         xgboost_control = list(
                                                                           max_depth = 5, eta = 0.3, nrounds = 350,
                                                                           nthread = 5, objective = "reg:squarederror", verbose = 0
                                                                         ),
                                                                         seas_dummy = FALSE,
                                                                         n_diffs = 0) )

  )

fit_models <- dplyr::bind_cols(fit_models, fit_models2)


acc <- fabletools::accuracy(fit_models)
fcst <- fabletools::forecast( fit_models, new_data = test )
acc_test <- fabletools::accuracy(fcst, test)
library(fabletools)
autoplot(fcst, test_week)

