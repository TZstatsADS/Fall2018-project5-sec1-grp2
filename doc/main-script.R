######################## BEGIN SCRIPT ######################

# Load Packages:
#install.packages(c("quantmod", "shiny", "shinythemes", "corrplot", "forecast", "xts", "dygraphs", "ggplot2", "reshape2", "gtools", "DT", "rnn", "plot3D", "plotly", "parcoords"))
library('quantmod')
library('shiny')
library('shinythemes')
library('corrplot')
library('forecast')
library('xts')
library('dygraphs')
library('ggplot2')
library('reshape2')
library('gtools')
library('DT')
library('rnn')
library("plot3D")
library("plotly")
library("parcoords")

# Download Data
# We download stock data using *quantmod* package:
data <- getSymbols(c(
  "MMM", "AXP", "AAPL", "BA", "CAT",
  "CVX", "CSCO", "KO", "DIS", "DWDP",
  "XOM", "GS", "HD", "IBM", "INTC",
  "JNJ", "JPM", "MCD", "MRK", "MSFT",
  "NKE", "PFE", "PG", "TRV", "UTX",
  "UNH", "VZ", "V", "WMT", "WBA"
))
data.list <- list(
  "MMM", "AXP", "AAPL", "BA", "CAT",
  "CVX", "CSCO", "KO", "DIS", "DWDP",
  "XOM", "GS", "HD", "IBM", "INTC",
  "JNJ", "JPM", "MCD", "MRK", "MSFT",
  "NKE", "PFE", "PG", "TRV", "UTX",
  "UNH", "VZ", "V", "WMT", "WBA"
)
data.data.matrix <- list(
  MMM, AXP, AAPL, BA, CAT,
  CVX, CSCO, KO, DIS, DWDP,
  XOM, GS, HD, IBM, INTC,
  JNJ, JPM, MCD, MRK, MSFT,
  NKE, PFE, PG, TRV, UTX,
  UNH, VZ, V, WMT, WBA
)

############################ PART I ############################

# Part I:
# Let us start with conventional time-series analysis.
# This part we use ARMA(p,q) model by applying arima() function
# in *forecast* packages.

# ARIMA Forecast + K-fold CV
ARMA_Fit_D_KFOLD_CV <- function(
  entry,
  ahead = 10,
  cutoff = 0.9,
  how.many.fold = 6) {
  
  # Define
  #entry = AAPL
  entry <- entry[,4]; entry.copy <- entry
  #entry <- entry/lag(entry) - 1; entry <- na.omit(entry)
  ahead <- ahead
  cutoff <- cutoff
  
  # K-fold CV
  all <- entry; all.copy <- all
  num.break <- how.many.fold
  train.results <- NULL
  test.results <- NULL
  folds <- cut(seq(1,nrow(all)),breaks=num.break,labels=FALSE)
  
  # CV
  sum.MSE <- NULL
  sum.Model <- list()
  for (i in c(1:(num.break-1))) {
    all <- all.copy
    train <- all[which(folds == i), ]
    all <- train
    
    # Model fitting
    fit <- auto.arima(
      all[1:round(cutoff*nrow(all)), ],
      stationary=FALSE,
      max.p = 30, max.q = 30, max.P = 10,
      max.Q = 10, max.order = 5, max.d = 2, max.D = 1, start.p = 2,
      start.q = 2, start.P = 1, start.Q = 1,
      ic=c("aicc","aic","bic"),
      test=c("kpss","adf","pp"),
      approximation=TRUE
    )
    Y_hat <- data.frame(forecast(fit, nrow(all[round(cutoff*nrow(all)):nrow(all), ])))
    Y_hat <- Y_hat$Point.Forecast
    Y <- all[round(cutoff*nrow(all)):nrow(all), ]
    MSE <- mean((Y_hat - Y)^2)
    
    # Store result
    sum.MSE <- c(sum.MSE, MSE)
    sum.Model[[i]] <- fit
  }
  
  # Choose the best param
  best.fit <- sum.Model[[which.min(sum.MSE)]]
  sum.MSE; best.fit
  
  # Test
  all <- all.copy
  train <- all[which(folds != num.break), ]
  test <- all[which(folds == num.break), ]
  fit <- auto.arima(
    train,
    stationary=FALSE,
    max.p = 30, max.q = 30, max.P = 10,
    max.Q = 10, max.order = 5, max.d = 2, max.D = 1, start.p = 2,
    start.q = 2, start.P = 1, start.Q = 1,
    ic=c("aicc","aic","bic"),
    test=c("kpss","adf","pp"),
    approximation=TRUE
  )
  Y_hat <- data.frame(forecast(fit, length(test)))
  Y_hat <- Y_hat$Point.Forecast
  Y <- test
  plot.data <- cbind(Y_hat, Y)
  colnames(plot.data)[1] <- c("Prediction")
  
  # Prediction
  fit <- auto.arima(
    entry,
    stationary=FALSE,
    max.p = 20, max.q = 20, max.P = 10,
    max.Q = 10, max.order = 5, max.d = 2, max.D = 1, start.p = 2,
    start.q = 2, start.P = 1, start.Q = 1,
    ic=c("aicc","aic","bic"),
    test=c("kpss","adf","pp"),
    approximation=TRUE
  )
  #summary(fit)
  results<-cbind(
    # fit$coef,
    fit$sigma^2,
    fit$log,
    fit$aic,
    fit$aicc,
    fit$bic
  )
  #results
  forecast<-data.frame(forecast(fit,ahead))
  forecast.days <- 1:ahead
  forecast <- data.frame(cbind(
    forecast.days, forecast
  ))
  colnames(forecast) <- c("Day + ?", "Point.Forecast", "Lo.80", "Hi.80", "Lo.95", "Hi.95")
  forecast
  
  # Return
  return(list(
    Summary.of.MSE = sum.MSE,
    Best.Model = best.fit,
    Test.MSE = mean(apply(plot.data, 1, mean)),
    Graph = dygraph(plot.data),
    Forecast = forecast
  ))
} # End of function

# Run a single stock, e.g. say the first one in the list
head(data.data.matrix[[1]])

# Run
# Source: http://indexarb.com/indexComponentWtsDJ.html
# Top three weights: BA, UNH, MMM
ARMA_Fit_D_KFOLD_CV(data.data.matrix[[26]],
                    ahead = 10,
                    cutoff = 0.9,
                    how.many.fold = 6)

# Store MSE
TS.Error <- NULL
for (i in 1:length(data.data.matrix)) {
  error <- ARMA_Fit_D_KFOLD_CV(data.data.matrix[[i]],
                      ahead = 10,
                      cutoff = 1/6,
                      how.many.fold = 6)$Test.MSE
  TS.Error <- c(TS.Error, error)
  print(c("Finished with round", i))
}

############################ PART II ############################

# Part II:
# To improve our results from time-series analysis, we propose
# to use influential measure as the statistics to select the 
# important covariates for us to do predict future stock
# price. The influential measure, e.g. I-score, is proposed 
# by Shaw-hwa Lo and Tian Zheng in a numerous of papers in 
# the past. 

# Begin Iscore
# Package:
require(gtools)

# Define Continuous Variable Selection Algorithm:
continuous.vs <- function(
  all = all,
  cut_off = 1,
  num.initial.set = 5,
  how.many.rounds = 3,
  i.want = 1,
  num.top.rows = 2,
  seed = 1,
  K.means.K = round(.5*ncol(all))
) {
  
  # Define data
  all <- data.frame(as.matrix(all))
  # all = df
  # cut_off = .9
  # num.initial.set = 7
  # how.many.rounds = 1
  # num.top.rows = 1
  # seed = 1
  # K.means.K = 2
  
  ################################ I-SCORE ##############################
  
  set.seed(seed)
  
  # Split:
  cutoff <- cut_off
  train <- all[1:(cutoff*nrow(all)),]; dim(train) # Training set
  test <- all[(cutoff*nrow(all)+1):nrow(all),]; dim(test) # Testing set
  
  # Identify Response and Explanatory:
  train.x <- train[,-1]; dim(train.x)
  train.y <- train[,1]; head(train.y)
  test.x <- test[,-1]; dim(test.x)
  test.y <- test[,1]; dim(data.frame(test.y))
  
  # Transpose:
  train.x <- t(train.x)
  test.x <- t(test.x)
  
  # I Score: 
  # Set up backups: 
  train.x.copy <- train.x
  
  #### Starting from here: all components of I score: 
  
  # Begin function:
  # Compute influence score, i.e., i-score: 
  iscore <- function(
    x = t(train.x), 
    y = train.y, 
    K = K.means.K) {
    
    # Define data frame:
    x = data.frame(x)
    y = data.frame(y)
    
    # Define modified I-score:
    # Standardize
    x.stand <- scale(x)
    k = K
    
    # K-means
    k.means.fit <- kmeans(x.stand, k)
    all <- data.frame(cbind(y, x))
    all$assigned.clusters <- k.means.fit$cluster
    
    # Compute I-score
    i.score.draft <- NULL
    y_bar <- sum(y)/nrow(x)
    for (i in 1:length(unique(k.means.fit$cluster))) {
      local.n <- length(all[all$assigned.clusters == i, 1]) #; local.n
      i.score <- local.n^2*(mean(all[all$assigned.clusters == i, 1]) - y_bar)^2
      i.score.draft <- c(i.score.draft, i.score)
    }
    i.score <- mean(i.score.draft)/nrow(all)
    #i.score
    
    # Return modified I-score:
    round(i.score, 2)
  } # End of function
  ## End function
  
  
  # The following is old function that we no longer use:
  # Begin function:
  # Compute influence score, i.e., i-score: 
  #iscore <- function(
  #  x = t(train.x), 
  #  y = train.y, 
  #  K = K.means.K) {
  
  # Define data frame:
  #  x = data.frame(x)
  #  y = data.frame(y)
  
  # Define modified I-score:
  #  cont_score <- function(i){
  #    dist.matrix <- abs(matrix(t(apply(x,1,'-',as.numeric(x[i,]))),nrow=nrow(x)))
  #    y_near <- mean(y[mixedorder(apply(dist.matrix,1,sum))[2:(2+K)],])
  #    y_bar <- colMeans(y)[[1]]
  #    (y_near - y_bar)^2
  #  } 
  
  # Compute:
  #  n = nrow(x)
  #  row.array <- matrix(1:n, nrow = n)
  
  # Return modified I-score:
  #  round(mean(apply(row.array, 1, cont_score)), 4)
  #} # End of function
  ## End function
  
  ################# BACKWARD DROPPING ALGORITHM #####################
  
  ### Backckward dropping algorithm: 
  # For each row, i.e., for each response variable,
  # we want run BDA for them individually.
  m <- num.initial.set
  # Algorithm starts from here: we need to repeat 1000 times:
  BDA.Algo <- function() {
    # Pick Initial Set (state of art) and call it X (capital X): 
    initial.set <- data.frame(t(train.x.copy)[,sample(nrow(train.x.copy),size=m,replace=FALSE)])
    # head(initial.set)
    # Records influence path:
    i.score.path <- matrix(0,nrow=2,ncol=m)
    i.score.path <- rbind(colnames(initial.set), i.score.path) #; i.score.path
    # Compute i score for initial set:
    i.score.col <- 1
    # Create iscore path: 
    i.score.path[2,i.score.col] <- iscore(x=initial.set, y=train.y); # i.score.path
    
    while(i.score.col < m) {
      # Each round: taking turns dropping one variable and computing I-score:
      i <- 0
      initial.set.copy <- initial.set 
      i.score.drop <- matrix(0,nrow=1,ncol=ncol(initial.set.copy))
      while (i < ncol(initial.set.copy)){
        i <- i + 1
        initial.set <- data.frame(initial.set.copy[,-i]); # head(initial.set.copy); dim(initial.set)
        i.score.drop[,i] <- iscore(x=initial.set, y=train.y); # i.score.drop; 
        #print("Done")
        # print(cbind("Drop Var.",i,"I score:", i.score.drop[,i]));
      }
      
      # This round:
      i.score.path[3,i.score.col] <- which(i.score.drop == max(i.score.drop))[[1]]
      variable.dropped <- which(i.score.drop == max(i.score.drop))[[1]]
      initial.set <- initial.set.copy[,-variable.dropped] # head(initial.set)
      i.score.col <- i.score.col + 1
      
      # Update I-score and ready for next round
      i.score.path[2,i.score.col] <- iscore(x=initial.set, y=train.y); # i.score.path
    }# End of loop
    
    # Record fianl table:
    i.score.path
    
    # Upload data: 
    # Indexed a, b, c, ... for different trials.
    final.i.score.path.mat <- i.score.path
    final.i.score.path.mat <- data.frame(as.matrix(final.i.score.path.mat))
    ##
    
    # We abstract three sets of matrices: 
    # 1. Variable Set
    # 2. Number of variables dropped
    # 3. I-score path
    
    ## How many variables in initial set? 
    # num.initial.set <- 10
    
    # 1. Variable Set:
    i.1 <- 1
    table.1 <- matrix(0,nrow=1,ncol=num.initial.set) #; dim(table.1)
    while (i.1 <= nrow(final.i.score.path.mat)){
      colnames(table.1) <- colnames(final.i.score.path.mat)
      ifelse((i.1 - 1) %% 3 == 0, 
             table.1 <- rbind(table.1, final.i.score.path.mat[i.1,]),
             i.1 <- i.1)
      i.1 <- i.1 + 1
    }
    table.1 <- table.1[-1,] #; head(table.1); dim(table.1)
    
    # 2. Number of variables dropped
    i.2 <- 1
    table.2 <- matrix(0,nrow=1,ncol=num.initial.set) #; dim(table.2)
    while (i.2 <= nrow(final.i.score.path.mat)){
      colnames(table.2) <- colnames(final.i.score.path.mat)
      ifelse((i.2) %% 3 == 0, 
             table.2 <- rbind(table.2, final.i.score.path.mat[i.2,]),
             i.2 <- i.2)
      i.2 <- i.2 + 1
    }
    table.2 <- table.2[-1,] #; head(table.2); dim(table.2)
    
    # 3. I-score path
    i.3 <- 1
    table.3 <- matrix(0,nrow=1,ncol=num.initial.set) #; dim(table.3)
    while (i.3 <= nrow(final.i.score.path.mat)){
      colnames(table.3) <- colnames(final.i.score.path.mat)
      ifelse((i.3 - 2) %% 3 == 0, 
             table.3 <- rbind(table.3, final.i.score.path.mat[i.3,]),
             i.3 <- i.3)
      i.3 <- i.3 + 1
    }
    table.3 <- table.3[-1,] #; head(table.3); dim(table.3)
    
    ## Finally:
    ## Delete variables one by one 
    # and generate final collection of variables:
    final.table.mat <- 1 # remove(final.table.mat)
    table.i <- 1
    
    # Start to get rid of variables:
    # loop starts here: 
    while (table.i <= nrow(table.1)){
      j <- 1
      final.table <- table.1[table.i,]
      while (
        j < which(table.3[table.i,] == as.numeric(as.matrix(table.3[table.i,ncol(table.3)])))[[1]]
      ) {
        if (j == 1){
          var.dropped <- as.numeric(table.2[table.i,j]);
          final.table <- final.table[,-var.dropped];
        } else if (
          #as.numeric(table.3[table.i,j]) > as.numeric(table.3[table.i,j-1])
          as.numeric(table.3[table.i,j]) < max(as.numeric(table.3[table.i,]))
        ) {
          var.dropped <- as.numeric(table.2[table.i,j])
          if (var.dropped > 0){final.table <- final.table[,-var.dropped]}
        }
        j <- j + 1; #print(final.table)
      }
      table.i <- table.i + 1
      if (length(final.table) == 1) {
        final.table.mat <- c(final.table.mat, as.character(final.table))
      } else {
        final.table.mat <- c(final.table.mat, as.character(unlist(final.table)))
      }
      #print(cbind("Done with round", table.i, "Dim =", dim(final.table.mat)))
    } # End of while loop
    
    # Report
    selected.variable <- rbind(final.table.mat)[, -1]
    #selected.variable <- final.table.mat[-1, ][!is.na(final.table.mat[-1, ])]
    report <- c(
      apply(rbind(selected.variable), 1, paste0, collapse = "_"), 
      max(as.numeric(table.3)))
    report
  }
  
  # Output
  # Compute iscore for each draw
  result <- t(replicate(how.many.rounds, BDA.Algo()))
  
  # Sort according to iscore
  all.variables <- result
  all.var <- NULL
  for (which.row in 1:nrow(all.variables)) {
    #which.row = 4
    selected.variable <- all.variables[which.row, ]
    selected.variable <- c(unlist(strsplit(as.character(selected.variable), split="_")))
    if (length(selected.variable) <= 2) {
      all.var <- rbind(all.var)
    } else {
      all.var <- rbind(all.var, all.variables[which.row, ])
    }
  } # End of loop
  all.var.new <- all.var
  colnames(all.var.new) <- c("Top Module", "Measure")
  all.result <- all.var.new[order(as.numeric(all.var.new[,2]), decreasing = TRUE),]
  num.top.rows <- ifelse(num.top.rows > nrow(all.result), 
                         nrow(all.result),
                         num.top.rows)
  top.result <- all.result[1:num.top.rows, ]
  
  # Print result:
  return(list(
    All.BDA.Modules = all.result,
    Top.BDA.Modules = top.result
  )
  ) # Output
} # End of function

# Continuous I-score to find weights
# Library
library(gtools)

# Create Cont Variable:
cont_var <- function(
  x = t(train.x), 
  y = train.y, 
  K = K.means.K,
  selected.variable = selected.variable) {
  
  # Define data frame:
  x = data.frame(x)
  y = data.frame(y)
  
  # Define modified I-score:
  cont_score <- function(i, x, y, K){
    dist.matrix <- abs(matrix(t(apply(x,1,'-',as.numeric(x[i,]))),nrow=nrow(x)))
    y_near <- mean(y[mixedorder(apply(dist.matrix,1,sum))[2:(2+K)],])
  } 
  
  # Define modified I-score:
  # Standardize
  x.stand <- scale(x)
  k = K
  
  # K-means
  k.means.fit <- kmeans(x.stand, k)
  all <- data.frame(cbind(y, x))
  all$assigned.clusters <- k.means.fit$cluster
  
  # Compute I-score
  i.score.draft <- NULL
  new.var <- matrix(0, nrow=nrow(x))
  for (i in 1:length(unique(k.means.fit$cluster))) {
    new.var[all$assigned.clusters == i, ] <- mean(all[all$assigned.clusters == i, 1])
  }
  
  # Return modified I-score:
  new.variable <- c(round(new.var,4))
  new.variable <- cbind(new.variable)
  colnames(new.variable) <- paste0(selected.variable, collapse = "___")
  
  # Return
  return(new.variable)
} # End of function
## End function

# DEFINE FINDING WEIGHTS FUNCTION FIRST;
# THEN WE ARE GOING TO CODE TRAINING/PREDICT FUNCTION

# Load package
library("quadprog")
library("pROC")
library("matrixcalc")
#library("matrixcalc", lib = "C:/Users/eagle/OneDrive/Documents/R/win-library/3.4")

# Define function
search.for.weights <- function(
  seed = 1,
  all = all, 
  cutoff.coefficient = 1
) {
  # Set up
  set.seed(seed)
  X <- as.matrix(all[,-1], nrow=nrow(all[,-1]))
  Y <- all[,1]
  num.col <- ncol(X)
  
  # Solve Optimization Problem
  Dmat = t(X) %*% X
  Amat = t(diag(num.col))
  bvec = rep(0, num.col); # bvec[num.col] <- 1 # Constraints: only positive weights
  dvec = t(Y) %*% X
  solution <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 0, factorized = T)
  
  # Output
  weights.normal.sol <- solution$solution
  weights.un.sol <- solution$unconstrained.solution
  weights.lagrange <- solution$Lagrangian
  # Here user can change to any one of the three types of weights
  weights.sol <- weights.normal.sol
  weights.sol <- weights.sol/sum(weights.sol) # Normalized; and sum to 1
  
  # Compute Response
  Y_hat <- matrix(0, nrow = nrow(X))
  for (i in 1:nrow(X)) {
    Y_hat[i, ] <- sum(X[i, ]*t(weights.sol))
  }
  compare_Y_Y_hat_original <- rbind(Y_hat = t(Y_hat), Truth = Y); 
  compare_Y_Y_hat_original = t(compare_Y_Y_hat_original)
  #Y_hat = ifelse(Y_hat > cutoff.coefficient*mean(Y_hat), 1, 0)
  
  # Output
  #compare_Y_Y_hat <- rbind(t(Y_hat), Y); compare_Y_Y_hat = t(compare_Y_Y_hat)
  #prediction.table <- table(Y_hat, Y)
  #type.I.II.error.table <- plyr::count(Y_hat - Y)
  #type.I.II.error.table$name <- ifelse(
  #  type.I.II.error.table[,1] == -1,
  #  "Type I Error",
  #  ifelse(
  #    type.I.II.error.table[,1] == 0, 
  #    "True Pos+Neg",
  #    "Type II Error"
  #  )
  #)
  #train.accuracy = type.I.II.error.table[type.I.II.error.table$x == 0, 2] / sum(type.I.II.error.table$freq)
  
  # ROC
  #actuals <- Y
  #scores <- Y_hat
  #roc_obj <- roc(response = actuals, predictor =  c(as.numeric(scores)))
  #auc <- auc(roc_obj); auc
  
  # Output
  return(list(
    Weights.for.Variables = cbind(Name = colnames(X), Weights = as.numeric(weights.sol)),
    Weights.Lagrange.for.Variables = weights.lagrange,
    Y.and.Y.hat.Table.Original = compare_Y_Y_hat_original#,
    #Y.and.Y.hat.Table.Binary = compare_Y_Y_hat,
    #I.II.Table = type.I.II.error.table,
    #Prediction.Truth.Table = prediction.table,
    #Accuracy = train.accuracy,
    #AUC = auc,
    #Gini.Co = 2*auc - 1
  ))
} # End of function

# Searching for weights train/test:
weights.train.test <- function(
  seed = 1,
  all = all, 
  cutoff.coefficient = 1,
  cutoff = .9
) {
  
  # Split data:
  train <- all[1:(cutoff*nrow(all)),]; dim(train) # Training set
  test <- all[(cutoff*nrow(all)+1):nrow(all),]; dim(test) # Testing set
  
  # Identify Response and Explanatory:
  train.x <- train[,-1]; dim(train.x)
  train.y <- train[,1]; head(train.y)
  test.x <- test[,-1]; dim(test.x)
  test.y <- test[,1]; dim(data.frame(test.y))
  
  # Train:
  all = data.frame(cbind(train.y, train.x))
  Result <- search.for.weights(1, all, 1)
  Result$Weights.for.Variables; 
  Train.List <- list(
    Weights.for.Variables = Result$Weights.for.Variables,
    Y.and.Y.hat.Table.Original = Result$Y.and.Y.hat.Table.Original#,
    #Y.and.Y.hat.Table.Binary = Result$Y.and.Y.hat.Table.Binary,
    #I.II.Table = Result$I.II.Table,
    #Prediction.Truth.Table = Result$Prediction.Truth.Table,
    #Accuracy = Result$Accuracy,
    #AUC = Result$AUC,
    #Gini.Co = 2*Result$AUC - 1
  )
  
  # Test
  Y_test_hat <- matrix(0, nrow = nrow(test.x))
  for (i in 1:nrow(test.x)) {
    Y_test_hat[i, ] <- sum(test.x[i, ]*t(as.numeric(Train.List$Weights.for.Variables[, 2])))
  }
  compare_Y_Y_hat_original <- rbind(Y_test_hat = t(Y_test_hat), Truth = test.y); 
  compare_Y_Y_hat_original = t(compare_Y_Y_hat_original)
  #Y_test_hat = ifelse(Y_test_hat > cutoff.coefficient*mean(Y_test_hat), 1, 0)
  #compare_Y_Y_hat <- rbind(t(Y_test_hat), test.y); compare_Y_Y_hat = t(compare_Y_Y_hat)
  #prediction.table <- table(Y_test_hat, test.y)
  #type.I.II.error.table <- plyr::count(Y_test_hat - test.y)
  #test.accuracy = type.I.II.error.table[type.I.II.error.table$x == 0, 2] / sum(type.I.II.error.table$freq)
  
  # ROC
  #roc_obj <- roc(response = test.y, predictor = c(as.numeric(Y_test_hat)))
  #auc.plot = plot(roc_obj)
  #auc <- auc(roc_obj); auc
  
  # Test Output
  Test.List <- list(
    Y.and.Y.hat.Table.Original = compare_Y_Y_hat_original#,
    #Y.and.Y.hat.Table.Binary = compare_Y_Y_hat,
    #Prediction.Truth.Table = prediction.table,
    #Prediction.Accuracy = test.accuracy,
    #AUC = auc,
    #Plot.AUC = auc.plot,
    #Gini.Co = 2*auc - 1
  )
  
  # Final Output
  return(list(
    Train.Result = Train.List,
    Test.Result = Test.List
  ))
} # End of function

# Define function:
iscore.buy.prediction <- function(
  x = x, 
  k.mean = 10,
  num.initial.set = 5,
  how.many.folds = 5,
  how.many.modules = 3
  #Buy.Frequency = -0.05,
  #daily.entry = daily.entry
) {
  # Define data
  #getSymbols("AAPL")
  #head(AAPL)
  
  # Set up data
  #all <- BS.Algo(x,.2,1,Buy.Frequency,.1,1,300,daily.entry)[, 3]
  all <- x[(0.2*nrow(x)):nrow(x),4]
  all <- data.frame(cbind(
    all,
    SMA(all, 2),SMA(all, 3),SMA(all, 4),SMA(all, 5),SMA(all, 6),
    SMA(all, 7),SMA(all, 8),SMA(all, 9),SMA(all, 10),
    SMA(all, 11),SMA(all, 12),SMA(all, 13),SMA(all, 14),
    SMA(all, 15),SMA(all, 16),SMA(all, 17),SMA(all,18),
    SMA(all, 19),SMA(all, 20),SMA(all, 21),SMA(all,22),
    SMA(all, 23),SMA(all, 24),SMA(all, 25)
  ))
  all <- na.omit(all)
  head(all); dim(all)
  
  # Copy for backup
  temp <- all
  
  # Let us cut the shuffled data into three folds
  cuts <- cut(seq(1,nrow(all)),breaks=how.many.folds,labels=FALSE)
  
  # Cross Validation
  Folds <- NULL
  Average.MSE <- NULL
  for (fold.i in 1:(how.many.folds-1)) {
    
    # Define data
    # fold.i = 1
    all <- temp
    all <- data.frame(all[cuts == fold.i, ])
    
    # Feature Selection
    # Run
    start_time <- Sys.time()
    Result <- continuous.vs(
      all = all,
      cut_off = 1/how.many.folds,
      num.initial.set = num.initial.set,
      how.many.rounds = 100,
      num.top.rows = how.many.modules,
      seed = 1,
      K.means.K = k.mean
    ); end_time <- Sys.time(); end_time - start_time
    
    # View
    #Result$All.BDA.Modules
    #Result$Top.BDA.Modules
    # Selected Variable
    which.row <- 1:1
    selected.variable <- Result$Top.BDA.Modules[which.row, 1]
    selected.variable <- c(unlist(strsplit(as.character(selected.variable), split="_")))
    selected.variable <- unique(selected.variable)
    selected.variable
    
    # Extract Variables by BDA Results:
    all.data <- all
    all <- all.data
    all <- data.frame(
      cbind(
        all[,1],
        all[,c(as.character(selected.variable))]
      )
    )
    names(all)[[1]] <- "label"
    length(selected.variable); dim(all) # Check!
    all.copy = all
    
    # Run function
    # Call this variable cont_var
    # output is a variable
    # with same length as the data set
    start_time <- Sys.time()
    k = k.mean
    new.var <- cont_var(all[,-1], all[,1], K=round(.5*k), selected.variable); end_time <- Sys.time()
    end_time - start_time
    
    # Add clusters back to data set:
    all.copy <- data.frame(cbind(all.copy,new.var)); dim(all.copy)
    all <- all.copy; colnames(all); 
    all <- all[, c(1, ncol(all))]; dim(all); all.copy = all; head(all)
    
    # Create clusters
    i = 1
    while (i <= (how.many.modules-1)) {
      # Selected Variable
      which.row <- i
      selected.variable <- Result$All.BDA.Modules[which.row, 1]
      selected.variable <- c(unlist(strsplit(as.character(selected.variable), split="_")))
      selected.variable <- unique(selected.variable)
      selected.variable
      all <- all.data
      all <- data.frame(
        cbind(
          all[,1],
          #all[,c(as.character(selected.variable))] # My Iscore Function
          all[,c(as.character(selected.variable))] # Tian Iscore Function
        )
      )
      names(all)[[1]] <- "label"
      length(selected.variable); dim(all) # Check!
      
      # Run function
      # Call this variable cont_var
      # output is a variable
      # with same length as the data set
      start_time <- Sys.time()
      k = k.mean
      new.var <- cont_var(all[,-1], all[,1], K=round(.5*k), selected.variable); end_time <- Sys.time()
      end_time - start_time
      
      # Add clusters back to data set:
      all.copy <- data.frame(cbind(all.copy,new.var)); dim(all.copy)
      all <- all.copy; colnames(all); dim(all)
      
      # i add 1
      i <- i + 1
    } # Finished updating clusters
    
    # Run regression
    Model <- weights.train.test(
      seed = 1, 
      all = all, 
      cutoff.coefficient = 1, 
      cutoff = .67)
    Weights <- Model$Train.Result$Weights.for.Variables
    Y.hat <- cbind(apply(all[,-1] * as.numeric(c((Weights[, 2]))), 1, sum)); 
    MSE <- sqrt(mean((c(Y.hat) - all$label)^2))
    
    # Report error
    Fold <- c(fold.i, MSE, c(Model$Train.Result$Weights.for.Variables[,2]))
    Folds <- rbind(Folds, Fold)
  } # End of for loop
  
  # Update
  Folds <- round(as.numeric(Folds), 2)
  Folds <- matrix(Folds, nrow = (how.many.folds-1))
  colnames(Folds) <- c("Fold", "MSE", c(rep("W", how.many.modules)))
  CV.Result <- Folds
  
  # Cross Validation
  Folds <- NULL
  Average.MSE <- NULL
  for (fold.i in how.many.folds:how.many.folds) {
    
    # Define data
    # fold.i = 1
    all <- temp
    all <- data.frame(all[cuts == fold.i, ])
    
    # Feature Selection
    # Run
    start_time <- Sys.time()
    Result <- continuous.vs(
      all = all,
      cut_off = .9,
      num.initial.set = num.initial.set,
      how.many.rounds = 50,
      num.top.rows = how.many.modules,
      seed = 1,
      K.means.K = k.mean
    ); end_time <- Sys.time(); end_time - start_time
    
    # View
    #Result$All.BDA.Modules
    #Result$Top.BDA.Modules
    # Selected Variable
    which.row <- 1:1
    selected.variable <- Result$Top.BDA.Modules[which.row, 1]
    selected.variable <- c(unlist(strsplit(as.character(selected.variable), split="_")))
    selected.variable <- unique(selected.variable)
    selected.variable
    
    # Extract Variables by BDA Results:
    all.data <- all
    all <- all.data
    all <- data.frame(
      cbind(
        all[,1],
        all[,c(as.character(selected.variable))]
      )
    )
    names(all)[[1]] <- "label"
    length(selected.variable); dim(all) # Check!
    all.copy = all
    
    # Run function
    # Call this variable cont_var
    # output is a variable
    # with same length as the data set
    start_time <- Sys.time()
    k = k.mean
    new.var <- cont_var(all[,-1], all[,1], K=round(.5*k), selected.variable); end_time <- Sys.time()
    end_time - start_time
    
    # Add clusters back to data set:
    all.copy <- data.frame(cbind(all.copy,new.var)); dim(all.copy)
    all <- all.copy; colnames(all); 
    all <- all[, c(1, ncol(all))]; dim(all); all.copy = all; head(all)
    
    # Create clusters
    i = 1
    while (i <= (how.many.modules-1)) {
      # Selected Variable
      which.row <- i
      selected.variable <- Result$All.BDA.Modules[which.row, 1]
      selected.variable <- c(unlist(strsplit(as.character(selected.variable), split="_")))
      selected.variable <- unique(selected.variable)
      selected.variable
      all <- all.data
      all <- data.frame(
        cbind(
          all[,1],
          #all[,c(as.character(selected.variable))] # My Iscore Function
          all[,c(as.character(selected.variable))] # Tian Iscore Function
        )
      )
      names(all)[[1]] <- "label"
      length(selected.variable); dim(all) # Check!
      
      # Run function
      # Call this variable cont_var
      # output is a variable
      # with same length as the data set
      start_time <- Sys.time()
      k = k.mean
      new.var <- cont_var(all[,-1], all[,1], K=round(.5*k), selected.variable); end_time <- Sys.time()
      end_time - start_time
      
      # Add clusters back to data set:
      all.copy <- data.frame(cbind(all.copy,new.var)); dim(all.copy)
      all <- all.copy; colnames(all); dim(all)
      
      # i add 1
      i <- i + 1
    } # Finished updating clusters
    
    # Run regression
    Model <- weights.train.test(
      seed = 1, 
      all = all, 
      cutoff.coefficient = 1, 
      cutoff = .67)
    Weights <- Model$Train.Result$Weights.for.Variables
    Y.hat <- cbind(apply(all[,-1] * as.numeric(c((Weights[, 2]))), 1, sum)); 
    MSE <- sqrt(mean((c(Y.hat) - all$label)^2))
    
    # Report error
    Fold <- c(fold.i, MSE, c(Model$Train.Result$Weights.for.Variables[,2]))
    Folds <- rbind(Folds, Fold)
  } # End of for loop
  
  # Plot data
  plot.data <- cbind(Y.hat, all$label)
  colnames(plot.data) <- c("Prediction", "Price")
  
  # Update
  Folds <- round(as.numeric(Folds), 2)
  Folds <- matrix(Folds, nrow = 1)
  colnames(Folds) <- c("Fold", "MSE", c(rep("W", how.many.modules)))
  Folds
  
  # Real Prediction
  Buy.Prediction <- sum(all[nrow(all), -1] * Folds[, -c(1,2)])
  
  return(list(
    Influential.Historical.Price = Result$Top.BDA.Modules,
    Training.by.Cross.Validating.Result = CV.Result,
    Set.Aside.Test.Result = Folds,
    Graph = dygraph(plot.data),
    Buy.Prediction = paste0(
      "Next trading day the price is ", Buy.Prediction, ". ",
      "Prediction less than Actual Price: overbought; ",
      "Prediction higher than Actual Price: oversold.")
  ))
} # End of function

# Run a single stock, e.g. say the first one in the list
head(data.data.matrix[[1]])

# Run a single stock
# Source: http://indexarb.com/indexComponentWtsDJ.html
# Top three weights: BA, UNH, MMM
# BA = 4, UNH = 26, MMM = 1
temp <- iscore.buy.prediction(
  x = data.data.matrix[[18]], 
  k.mean = 10,
  num.initial.set = 12,
  how.many.folds = 6,
  how.many.modules = 25
  #Buy.Frequency = -0.05,
  #daily.entry = daily.entry
); 

# Store MSE
Proposed.Error <- NULL
for (i in 1:length(data.data.matrix)) {
  error <- iscore.buy.prediction(
    x = data.data.matrix[[i]], 
    k.mean = 10,
    num.initial.set = 12,
    how.many.folds = 6,
    how.many.modules = 25
  )$Set.Aside.Test.Result[,2]
  Proposed.Error <- c(Proposed.Error, error)
  print(c("Finished with round", i))
}

######################### SUMMARY ###########################

# Summary:
DJI.Prediction.Summary <- cbind(
  TS.Error, Proposed.Error, Proposed.Error/TS.Error-1)
rownames(DJI.Prediction.Summary) <- unlist(data.list)
colnames(DJI.Prediction.Summary) <- c("TS.Error", "Proposed.Method.Error", "Error.Reduction")
DJI.Prediction.Summary <- data.frame(
  rbind(
    cbind(rownames(DJI.Prediction.Summary), round(DJI.Prediction.Summary[,1], 2), rep("TS",30)),
    cbind(rownames(DJI.Prediction.Summary), DJI.Prediction.Summary[,2], rep("Proposed", 30))
  )
)
colnames(DJI.Prediction.Summary) <- c("Ticker", "MSE", "Group")

# Plot
ggplot(data=DJI.Prediction.Summary, aes(x=DJI.Prediction.Summary$Ticker, 
                                        y=as.numeric(as.character(DJI.Prediction.Summary$MSE)),
                                        fill=Group)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  xlab("Ticker") + 
  ylab("Test Set MSE")

# Barplot
library(ggplot2)
df <- melt(DJI.Prediction.Summary[, c(1,2,3)])
colnames(df) <- c("Ticker", "MSE", "Methods")
p <- ggplot(df) +
  geom_boxplot(aes(x=Methods, y = MSE, color = Methods))
p

# Save
setwd("C:/Users/eagle/Desktop/Project 5/document")
save.image(file = "project-source.RData")
#write.csv(DJI.Prediction.Summary, "C:\Users\eagle\Desktop/Project 5/DIA-Prediction-Summary.csv")

######################### END OF SCRIPT #########################