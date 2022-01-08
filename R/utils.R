check_trend_season <- function( .data ) {
  target <- rlang::sym(tsibble::measured_vars(.data)[1])
  mdl <- fabletools::model( .data, fable::ETS( !!target ) )
  selected_model <- fabletools::model_sum(test[[1]][[1]][["fit"]])
  # this looks for the trend of the fitted ETS( error, TREND, season ) model -
  # where N means "no-trend" - in every other case, we assume there is a trend.
  trend <- !grepl( pattern = ",N,", x = selected_model )
  # ths looks for the seasonality of the fitted ETS(error, trend, SEASON ) model
  # where N means, again, "no-season"
  season <- !grepl( pattern = ",[A-za-z],N", x = selected_model )
  return( list(trend = trend, season = season) )
}

kdemode <- function(data){

  # Fix from/to
  from <- min(data)-0.1*diff(range(data))
  to <- max(data)+0.1*diff(range(data))

  # Calculate KDE
  ks <- density(data,bw="SJ",n=512,from=from,to=to)
  x <- ks$x
  f <- ks$y
  h <- ks$bw

  # Find mode
  mo <- x[which(f==max(f))][1] # mode

  return(list(mode=mo,xd=x,fd=f,h=h))
}

min_max <- function( x, a = -0.8, b = 0.8  ) {

  min_x = min(x, na.rm = TRUE)
  max_x = max(x, na.rm = TRUE)

  list( x = ( b - a ) * ( x - min_x )/( max_x - min_x ) + a,
        a = a,
        b = b,
        min_x = min_x,
        max_x = max_x )
}

inverse_min_max <- function( x, min_x = 0, max_x = 1, a = -1, b = 1  ) {
  # (max_x - min_x )*(x - min(x))/( max(x) - min(x)) + min_x
  ( max_x - min_x )*( x - a )/( b - a ) + min_x
}

es_run <- function( y, alpha = 0.3 ) {
  for( i in seq_len(length(y)-1) ) {
    y[i+1] <- alpha * y[i+1] + (1-alpha) * y[i]
  }
  return(y)
}


make_lag_matrix <- function( y, lags = 1:5 ) {
  lags <- purrr::map( lags, function(lag) {
    dplyr::lag( y, lag )
  })
  do.call(cbind,lags)
}


# init_w <- function( p, hd ) {
#   # Initialise layer weights
#   bb <- c(-1, 1) * (1 / sqrt(p))
#   w <- matrix(runif((p + 1) * hd, min = bb[1], max = bb[2]), nrow = (p + 1)) # p + 1 for bias
#   return(w)
# }
#
sigmoid <- function(x) {
  y <- x / (1 + abs(x))
  return(y)
}
#
#
# get.ff <- function(y){
#   # Get time series frequency
#   if (any(class(y) == "msts")){
#     ff <- attributes(y)$msts
#     ff.n <- length(ff)
#   } else {
#     ff <- frequency(y)
#     ff.n <- 1
#   }
#   return(list("ff"=ff,"ff.n"=ff.n))
# }
#
# def.lags <- function(lags,keep,ff,xreg.lags,xreg.keep,xreg){
#   # Default lagvector
#   if (is.null(lags)){
#     if (max(ff)>3){
#       lags <- 1:max(ff)
#     } else {
#       lags <- 1:4
#     }
#   }
#   if (!is.null(xreg) && is.null(xreg.lags)){
#     x.n <- dim(xreg)[2]
#     xreg.lags <- rep(list(lags),x.n)
#   }
#
#   # Check keep options
#   if (!is.null(keep)){
#     if (length(keep) != length(lags)){
#       stop("Argument `keep' must be a logical vector of length equal to lags.")
#     }
#   } else {
#     keep <- rep(FALSE,length(lags))
#   }
#   # If no univariate lags are requested, then make keep=NULL
#   if (length(lags)==1 & lags[1]==0){
#     keep <- NULL
#   }
#   if (!is.null(xreg.keep)){
#     if (all(unlist(lapply(xreg.lags,length)) == unlist(lapply(xreg.keep,length)))==FALSE){
#       stop("Argument `xreg.keep' must be a list of logical vectors of length equal to the length of the lags in xreg.lags.")
#     }
#   } else {
#     xreg.keep <- lapply(unlist(lapply(xreg.lags,length)),function(x){rep(FALSE,x)})
#   }
#
#   return(list("lags"=lags,"keep"=keep,"xreg.lags"=xreg.lags,"xreg.keep"=xreg.keep))
# }

#
# ndiffs.net <- function(difforder,y,ff,st){
#   # Find differencing for neural nets
#   # NULL is automatic
#   # 0 is no differencing
#
#   # Find differencing order
#   if (all(difforder != 0)){
#     cma <- st$cma
#     if (is.null(difforder)){
#       # Identify difforder automatically
#       difforder <- 0
#       if (trendcheck(cma) == TRUE){
#         difforder <- 1
#       }
#       if (frequency(y)>1){
#         if (st$season.exist == TRUE){
#           # difforder <- c(difforder,frequency(y))
#
#           # Remove trend appropriately
#           if (length(y)/max(ff) < 3){
#             m.seas <- TRUE
#           } else {
#             # Test can only run if there are at least three seasons
#             m.seas <- tsutils::mseastest(y,m=max(ff),cma=cma)$is.multiplicative
#           }
#           if (m.seas == TRUE){
#             y.dt <- y/cma
#           } else {
#             y.dt <- y-cma
#           }
#           d.order <- forecast::nsdiffs(ts(y.dt,frequency=max(ff)),test="ch")
#           if (d.order > 0){
#             difforder <- c(difforder,max(ff))
#           }
#         }
#       }
#     }
#   }
#
#   # To remove differencing from the remaining it should be set to NULL
#   if (any(difforder == 0)){
#     difforder <- NULL
#   }
#
#   return(difforder)
# }

n_diffs <- function( .data ) {
  target <- tsibble::measured_vars(.data)[1]
  target <- unlist(as.data.frame(.data)[,target])
  diffs <- unique( c( feasts::unitroot_ndiffs(target, differences = 0:3),
             feasts::unitroot_nsdiffs(target, differences = 0:3)
             )
  )
  diffs[ diffs > 0 ]
}

#
#
# preprocess <- function( .data,
#                         m,lags,keep,difforder,sel.lag,allow.det.season,det.type,
#                        # ff,
#                        ff.n,xreg,xreg.lags,xreg.keep){
#   # Pre-process data for MLP and ELM
#
#   # # Check seasonality & trend
#   # cma <- tsutils::cmav(y,ma=max(ff))
#   # st <- seasoncheck(y,m=max(ff),cma=cma)
#   # if (is.null(st$season.exist)){
#   #   st$season.exist <- FALSE
#   # }
#
#   # Specify differencing order
#   difforder <- n_diffs( .data )
#
#   # Apply differencing
#   d <- length(difforder)
#   y.d <- y.ud <- vector("list",d+1)
#   y.d[[1]] <- y
#   names(y.d)[1] <- "d0"
#   if (d>0){
#     for (i in 1:d){
#       y.d[[i+1]] <- diff(y.d[[i]],difforder[i])
#       names(y.d)[i+1] <- paste0(names(y.d)[i],"d",difforder[i])
#     }
#   }
#   names(y.ud) <- names(y.d)
#
#   # Scale target
#   sc <- linscale(tail(y.d,1)[[1]],minmax=list("mn"=-.8,"mx"=0.8))
#   y.sc <- sc$x
#   n <- length(y.sc)
#
#   # Scale xregs and trim initial values for differencing of y
#   if (!is.null(xreg)){
#     x.n <- dim(xreg)[2]
#     xreg.sc <- array(NA,c(dim(xreg)[1]-sum(difforder),x.n))
#     xreg.minmax <- vector("list",x.n)
#     dstart <- sum(difforder)
#     xreg <- xreg[(sum(difforder)+1):dim(xreg)[1],,drop=FALSE]
#     for (i in 1:x.n){
#       xreg.sc.temp <- linscale(xreg[,i],minmax=list("mn"=-.8,"mx"=0.8))
#       xreg.sc[,i] <- xreg.sc.temp$x
#       xreg.minmax[[i]] <- xreg.sc.temp$minmax
#     }
#   } else {
#     x.n <- 0
#     xreg.minmax <- NULL
#     xreg.sc <- xreg
#   }
#
#   net.inputs <- create.inputs(y.sc, xreg.sc, lags, xreg.lags, n)
#   Y <- net.inputs$Y
#   X <- net.inputs$X
#   Xreg <- net.inputs$Xreg
#   lag.max <- net.inputs$lag.max
#   rm("net.inputs")
#
#   # Create seasonal dummies
#   seas.dum <- seas.dum.net(st,difforder,det.type,ff,ff.n,Y,y,allow.det.season)
#   Xd <- seas.dum$Xd
#   det.type <- seas.dum$det.type
#   sdummy <- seas.dum$sdummy
#   rm("seas.dum")
#
#   # Select lags
#   if (sel.lag == TRUE){
#     if (x.n>0){
#       Xreg.all <- do.call(cbind,Xreg)
#       Xreg.all <- Xreg.all[1:length(Y),,drop=FALSE]
#     } else {
#       Xreg.all <- NULL
#     }
#     reg.isel <- as.data.frame(cbind(Y,X,Xreg.all))
#     # colnames(reg.isel) <- c("Y",paste0("X",lags),paste0("Xreg",))
#     if (sdummy == FALSE){
#       # Check if there are no inputs at all
#       if (all(colnames(reg.isel) == "Y")){
#         stop("Cannot build a network with no univariate or exogenous lags and no deterministic seasonality. Increase the maximum lags.")
#       } else {
#         fit <- lm(formula=Y~.,data=reg.isel)
#         if (sdummy == FALSE){
#           ff.det <- NULL
#         }
#       }
#     } else {
#       lm.frm <- as.formula(paste0("Y~.+",paste(paste0("Xd[[",1:ff.n,"]]"),collapse="+")))
#       fit <- lm(formula=lm.frm,data=reg.isel)
#     }
#     # Make selection of lags robust to sample size issues if all
#     # other checks fail by using lasso
#     cf.temp <- tryCatch({
#       keep.all <- c(keep,unlist(xreg.keep),rep(FALSE,ff.n))
#       if (all(!keep.all)){
#         scp <- NULL
#       } else {
#         scp <- list(lower = as.formula(paste("~",paste(attributes(fit$terms)$term.labels[keep.all],collapse=" + "))))
#       }
#       fit <- MASS::stepAIC(fit,trace=0,direction="backward",scope=scp)
#       # Get useful lags
#       cf.temp <- coef(fit)
#     }, error = function(e) {
#       lasso.Y <- reg.isel[,1]
#       lasso.X <- data.matrix(reg.isel[,2:dim(reg.isel)[2],drop=FALSE])
#       if (sdummy == TRUE){
#         for (i in 1:ff.n){
#           tempX <- Xd[[i]]
#           colnames(tempX) <- paste0(paste0("Xd[[",i,"]]"),colnames(tempX))
#           lasso.X <- cbind(lasso.X,tempX)
#         }
#       }
#       if (any(keep.all)){
#         warning("Cannot execute backwards variable selection, reverting to Lasso. Arguments `keep' and `xreg.keep' will be ignored.")
#       }
#       fit.lasso <- suppressWarnings(glmnet::cv.glmnet(x=lasso.X,y=lasso.Y))
#       cf.temp <- as.vector(coef(fit.lasso))
#       names(cf.temp) <- rownames(coef(fit.lasso))
#       cf.temp <- cf.temp[cf.temp!=0]
#     })
#     X.loc <- lags[which(colnames(X) %in% names(cf.temp))]
#
#     if (x.n>0){
#       Xreg.loc <- xreg.lags
#       for (i in 1:x.n){
#         Xreg.loc[[i]] <- xreg.lags[[i]][which(colnames(Xreg[[i]]) %in% names(cf.temp))]
#       }
#       xreg.lags <- Xreg.loc
#     }
#
#     # Check if deterministic seasonality has remained in the model
#     if (sdummy == TRUE){
#       still.det <- rep(TRUE,ff.n)
#       # Trigonometric dummies will not be retained by linear regression
#       # so do not allow rejection by stepwise!
#       if (det.type == "bin"){
#         for (i in 1:ff.n){
#           still.det[i] <- any(grepl(paste0("Xd[[",i,"]]"),names(cf.temp),fixed=TRUE))
#         }
#       }
#     }
#
#     # Although there is an error above to avoid having no inputs, it
#     # may still happen if regession rejects all lags. Give a warning!
#     # Check if there are any lags
#     if (x.n>0){
#       # If not univariate and exogenous
#       if (sum(c(length(X.loc),unlist(lapply(Xreg.loc,length))))==0){
#         # If no deterministic seasonal
#         if (sdummy == FALSE){
#           warning("No inputs left in the network after pre-selection, forcing AR(1).")
#           X.loc <- 1
#         }
#       }
#     } else {
#       # If no univariate lags
#       if (length(X.loc)==0){
#         # If no deterministic seasonal
#         if (sdummy == FALSE){
#           warning("No inputs left in the network after pre-selection, forcing AR(1).")
#           X.loc <- 1
#         }
#       }
#     }
#     if (length(X.loc)>0){
#       lags <- X.loc
#     }
#
#     # Recreate inputs
#     net.inputs <- create.inputs(y.sc, xreg.sc, lags, xreg.lags, n)
#     Y <- net.inputs$Y
#     X <- net.inputs$X
#     Xreg <- net.inputs$Xreg
#     lag.max <- net.inputs$lag.max
#     rm("net.inputs")
#
#     # Recreate seasonal dummies
#     if (sdummy == TRUE){
#       # Re-create seasonal dummies
#       if (sum(still.det)==0){
#         sdummy <- FALSE
#         ff.det <- NULL
#       } else {
#         ff.det <- ff[still.det]
#         ff.n.det <- length(ff.det)
#         Xd <- vector("list",ff.n.det)
#         for (s in 1:ff.n.det){
#           if (det.type=="trg"){
#             # There was a problem when the fractional seasonalities were < 3, so this is now separated
#             Xd[[s]] <- tsutils::seasdummy(length(Y),y=ts(Y,end=end(y),frequency=ff[s]),type="trg",full=TRUE)
#             Xd[[s]] <- Xd[[s]][,1:min(length(Xd[[s]][1,]),2)]
#           } else {
#             Xd[[s]] <- tsutils::seasdummy(length(Y),y=ts(Y,end=end(y),frequency=ff[s]),type="bin")
#           }
#           colnames(Xd[[s]]) <- paste0("D",s,".",1:length(Xd[[s]][1,]))
#         }
#       }
#     }
#
#   } else {
#     # If no selection is done, match frequencies of dummies with frequencies of time series
#     ff.det <- ff
#   }
#
#   # Merge lags and deterministic seasonality to create network inputs
#   if (x.n>0){
#     Xreg.all <- do.call(cbind,Xreg)
#   } else {
#     Xreg.all <- NULL
#   }
#   X.all <- cbind(X,Xreg.all[1:length(Y),,drop=FALSE])
#   if (sdummy == TRUE){
#     Xd <- do.call(cbind,Xd)
#     X.all <- cbind(X.all,Xd)
#   }
#
#   # Network formula
#   frm <- paste0(colnames(X.all),collapse="+")
#   frm <- as.formula(paste0("Y~",frm))
#
#   return(list("Y"=Y,"X"=X.all,"sdummy"=sdummy,"difforder"=difforder,"det.type"=det.type,"lags"=lags,"xreg.lags"=xreg.lags,"lag.max"=lag.max,"sc"=sc,"xreg.minmax"=xreg.minmax,"d"=d,"y.d"=y.d,"y.ud"=y.ud,"frm"=frm,"ff.det"=ff.det))
#
# }
#
# seas.dum.net <- function(st,difforder,det.type,ff,ff.n,Y,y,allow.det.season){
#   # Create seasonal dummies for networks
#
#   if ((if(ff.n > 1){TRUE}else{!any(difforder == max(ff))})
#       & frequency(y)>1 & st$season.exist==TRUE & allow.det.season==TRUE){
#     sdummy <- TRUE
#     # Set type of seasonal dummies
#     if (det.type == "auto"){
#       if (ff.n == 1 && ff[1] <= 12){
#         det.type <- "bin"
#       } else {
#         det.type <- "trg"
#       }
#     }
#     Xd <- vector("list",ff.n)
#     for (s in 1:ff.n){
#       if (det.type=="trg"){
#
#         # There was a problem when the fractional seasonalities were < 3, so this is now separated
#         Xd[[s]] <- tsutils::seasdummy(length(Y),y=ts(Y,end=end(y),frequency=ff[s]),type="trg",full=TRUE)
#         Xd[[s]] <- Xd[[s]][,1:min(length(Xd[[s]][1,]),2)]
#       } else {
#         Xd[[s]] <- tsutils::seasdummy(length(Y),y=ts(Y,end=end(y),frequency=ff[s]),type="bin")
#       }
#       colnames(Xd[[s]]) <- paste0("D",s,".",1:length(Xd[[s]][1,]))
#     }
#     # Xd <- do.call(cbind,Xd)
#     # X <- cbind(X,Xd)
#   } else {
#     sdummy <- FALSE
#     Xd <- NULL
#   }
#
#   return(list("Xd"=Xd,"det.type"=det.type,"sdummy"=sdummy))
#
# }
#
# create.inputs <- function(y.sc,xreg.sc,lags,xreg.lags,n){
#   # Prepare inputs & target
#   if (length(lags)>0){
#     ylags <- max(lags)
#   } else {
#     ylags <- 0
#   }
#   if (!is.null(xreg.sc)){
#     xlags <- unlist(lapply(xreg.lags,function(x){if(length(x)){max(x)}else{0}}))
#     lag.max <- max(c(ylags,xlags))
#   } else {
#     lag.max <- ylags
#   }
#   # Univariate
#   if (all(ylags != 0)){
#     y.sc.lag <- tsutils::lagmatrix(y.sc,unique(c(0,lags)))
#     Y <- y.sc.lag[(lag.max+1):n,1,drop=FALSE]
#     colnames(Y) <- "Y"
#     X <- y.sc.lag[(lag.max+1):n,2:(length(lags)+1),drop=FALSE]
#     colnames(X) <- paste0("X",lags)
#   } else {
#     Y <- matrix(y.sc[(lag.max+1):n],ncol=1)
#     colnames(Y) <- "Y"
#     X <- NULL
#   }
#   # Exogenous
#   if (!is.null(xreg.sc)){
#     x.p <- dim(xreg.sc)[2]
#     x.n <- dim(xreg.sc)[1]
#     Xreg <- vector("list",x.p)
#     for (i in 1:x.p){
#       if (length(xreg.lags[[i]]>0)){
#         Xreg[[i]] <- tsutils::lagmatrix(xreg.sc[,i],xreg.lags[[i]])[(lag.max+1):x.n,,drop=FALSE]
#         colnames(Xreg[[i]]) <- paste0("Xreg.",i,".",xreg.lags[[i]])
#       } else {
#         Xreg[[i]] <- NULL
#       }
#     }
#     x.n.all <- lapply(Xreg,function(x){length(x[,1])})
#     x.n.all <- x.n.all[x.n.all>0]
#     if (any(x.n.all < length(Y))){
#       stop("Length of xreg after construction of lags smaller than training sample.")
#     }
#   } else {
#     Xreg <- NULL
#   }
#
#   return(list("Y"=Y,"X"=X,"Xreg"=Xreg,"lag.max"=lag.max))
#
# }
#
# auto.hd.elm <- function(Y,X,frm){
#
#   # Use ELM to find hidden nodes
#   sz.elm <- max(min(dim(X)[2]+2,length(Y)-2))
#   reps.elm <- 20
#   # sz.elm <- min(40,max(1,length(Y)-2))
#
#   net <- neuralnet::neuralnet(frm,cbind(Y,X),hidden=sz.elm,threshold=10^10,rep=reps.elm,err.fct="sse",linear.output=FALSE)
#   hd.elm <- vector("numeric",reps.elm)
#   for (r in 1:reps.elm){
#     Z <- as.matrix(tail(neuralnet::compute(net,X,r)$neurons,1)[[1]][,2:(sz.elm+1)])
#
#     type <- "step"
#     # Calculate regression
#     switch(type,
#            "lasso" = {
#              fit <- suppressWarnings(glmnet::cv.glmnet(Z,cbind(Y)))
#              cf <- as.vector(coef(fit))
#              hd.elm[r] <- sum(cf != 0)-1 # -1 for intercept
#            },
#            {
#              reg.data <- as.data.frame(cbind(Y,Z))
#              colnames(reg.data) <- c("Y",paste0("X",1:sz.elm))
#              # Take care of linear dependency
#              alias.fit <- alias(Y~.,data=reg.data)
#              alias.x <- rownames(alias.fit$Complete)
#              frm.elm <- as.formula(paste0("Y~",paste0(setdiff(colnames(reg.data)[2:(sz.elm+1)],alias.x),collapse="+")))
#              fit <- suppressWarnings(lm(frm.elm,reg.data))
#              if (type == "step"){
#                fit <- suppressWarnings(MASS::stepAIC(fit,trace=0,direction="backward"))
#              }
#              hd.elm[r] <- sum(summary(fit)$coefficients[,4]<0.05,na.rm=TRUE)-(summary(fit)$coefficients[1,4]<0.05)
#            })
#
#   }
#   hd <- round(median(hd.elm))
#   if (hd<1){
#     hd <- 1
#   }
#
#   return(hd)
#
# }
#
# auto.hd.cv <- function(Y,X,frm,comb,reps,type=c("cv","valid"),hd.max=NULL){
#   # Find number of hidden nodes with CV
#
#   # Setup
#   type <- type[1]
#   K <- 5                                            # Number of folds
#   val.size <- 0.2                                   # Size of validation set
#   reps <- min(c(20,max(c(2,reps))))                 # Number of NN reps, maximum 20
#   if (is.null(hd.max)){
#     hd.max <- max(2,min(dim(X)[2]+2,length(Y)-2)) # Maximum number of hidden nodes
#   }
#
#   # Setup folds or validation set
#   n <- length(Y)
#   if (type == "cv"){
#     # Create folds
#     if (K >= n){
#       stop("Too few observations to perform cross-validation for specification of hidden nodes.")
#     }
#     # Create fold indices
#     idx.all <- sample(1:n)
#     cv.cut <- seq(0,n,length.out=K+1)
#     idx <- vector("list",K)
#     for (i in 1:K){
#       idx[[i]] <- which((idx.all <= cv.cut[i+1]) & (idx.all > cv.cut[i]))
#     }
#   } else {
#     # Validation set
#     K <- 1
#     idx <- list(sample(1:n,round(val.size*n)))
#   }
#
#   # Now run CV (1-step ahead)
#   err.h <- array(NA,c(hd.max,1),dimnames=list(paste0("H.",1:hd.max),"MSE"))
#   for (h in 1:hd.max){
#     # For each fold
#     err.cv <- vector("numeric",K)
#     for (i in 1:K){
#       Y.trn <- Y[setdiff(1:n,idx[[i]]),,drop=FALSE]
#       X.trn <- X[setdiff(1:n,idx[[i]]),,drop=FALSE]
#       Y.tst <- Y[idx[[i]],,drop=FALSE]
#       X.tst <- X[idx[[i]],,drop=FALSE]
#       net <- neuralnet::neuralnet(frm,cbind(Y.trn,X.trn),hidden=h,rep=reps,err.fct="sse",linear.output=TRUE)
#       reps <- length(net$weights) # In case some network is untrained
#
#       # For each training repetition
#       frc <- array(NA,c(length(Y.tst),reps))
#       for (r in 1:reps){
#         frc[,r] <- neuralnet::compute(net,X.tst,r)$net.result
#       }
#       frc <- frc.comb(frc,comb)
#       err.cv[i] <- mean((Y.tst - frc)^2)
#     }
#     err.h[h] <- mean(err.cv)
#   }
#   hd <- which(err.h == min(err.h))[1]
#
#   return(list("hd"=hd,"mseH"=err.h))
#
# }
#
# frc.comb <- function(Yhat,comb,na.rm=c(FALSE,TRUE)){
#   # Combine forecasts
#   na.rm <- na.rm[1]
#   r <- dim(Yhat)[2]
#
#   if (r>1){
#     switch(comb,
#            "median" = {yout <- apply(Yhat,1,median,na.rm=na.rm)},
#            "mean" = {yout <- apply(Yhat,1,mean,na.rm=na.rm)},
#            "mode" = {# Remove NAs
#              Ytemp <- Yhat
#              Ytemp <- Ytemp[, colSums(is.na(Ytemp))==0]
#              # Calculate only for non-constants
#              idx <- !apply(Ytemp,1,forecast::is.constant)
#              k <- sum(idx)
#              yout <- Yhat[,1]
#              if (k>0){
#                Ycomb <- Ytemp[idx,]
#                if (k>1){
#                  Ycomb <- sapply(apply(Ycomb,1,kdemode),function(x){x[[1]][1]})
#                } else {
#                  Ycomb <- kdemode(Ycomb)$mode
#                }
#                yout[idx] <- Ycomb
#              }
#            })
#   } else {
#     yout <- Yhat[,1]
#   }
#
#   return (yout)
# }
#
#
#
#
#
#
