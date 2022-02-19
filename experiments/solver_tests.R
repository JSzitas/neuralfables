
data("CreditCard", package = "AER" )
CreditCard %>%
  dplyr::select( -c("card","owner","selfemp") ) -> test_data


dplyr::select( test_data, - active ) %>%
  as.matrix -> test_x

ridge_solver( y = test_data$active,
              X = test_x,
              family = "poisson",
              lambda = 0.3) -> pois_test


test_pred_reg <- predict.poisson_glm( pois_test, test_x )

rmse <- function( x,y  ){  sqrt( mean( (x-y)^2)) }


ridge_solver( y = test_data$active,
              X = test_x,
              family = "poisson",
              lambda = 0) -> pois_test

test_pred_unreg <- predict.poisson_glm( pois_test, test_x )

ridge_solver( y = test_data$active,
              X = test_x,
              family = "poisson",
              lambda = NULL) -> pois_test

test_pred_estim <- predict.poisson_glm( pois_test, test_x )


print( rmse( test_data$active, test_pred_reg))
print( rmse( test_data$active, test_pred_unreg))
print( rmse( test_data$active, test_pred_estim))

