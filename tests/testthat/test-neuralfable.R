
test_that( "Defining a model works", {

  model <- neuralfable( Lynx ~ method("mlp") )
  expect_equal( class(model), c("mdl_defn", "R6") )
})

test_that("Fitting a model works", {

  set.seed(1071)

  model_elm <- neuralfable( Lynx ~ method("elm") +
                            parameters( lags = c(1,2,4),
                                        n_hidden = 20 )
                            )

  fitted_model <- fabletools::model( tsibbledata::pelt, elm = model_elm )

  expect_equal( class(fitted_model[[1]][[1]][["fit"]]), "NEURALFABLE" )
  expect_equal( class( fitted_model[[1]][[1]][["fit"]][["fit"]] ),
                "ELM")
  expect_equal( sum( fitted_model[[1]][[1]][["fit"]][["fit"]]$fitted,
                     na.rm = TRUE),
                2413679,
                tolerance = 1
                )
})
