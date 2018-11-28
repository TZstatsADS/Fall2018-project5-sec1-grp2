############################### BEGIN SCRIPT ##############################

#install.packages(c("NLP", "SnowballC", "slam", "tm", "wordcloud", "xml2"))
#install.packages(c("quantmod", "shiny", "shinythemes", "corrplot", "forecast",
#                   "xts", "dygraphs", "ggplot2", "reshape2", "DT"))
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
require('rnn')
library("plot3D")
library("plotly")
library("parcoords")


######################## DEFINE: FUNCTIONS ################################

# Download data for a stock if needed, and return the data
require_symbol <- function(symbol, envir = parent.frame()) {
  if (is.null(envir[[symbol]])) {
    envir[[symbol]] <- getSymbols(symbol,# src = "yahoo", 
                                  auto.assign = FALSE, from = "2010-01-01")
  }
  envir[[symbol]]
}

# Define basic plot function:
Basic.Plot <- function(x,r_day_plot,end_day_plot){
  #x <- AAPL; r_day_plot = .8; end_day_plot = 1
  daily_initial_time_plot <- r_day_plot*nrow(x)
  daily_ending_time_plot <- end_day_plot*nrow(x)
  data.all <- x[daily_initial_time_plot:daily_ending_time_plot,]
  data.all <- data.all[,-c(5:6)]
  #chartSeries(data,
  #            theme = chartTheme("black"),
  #            name = "Candlestick Chart for Entered Ticker",
  #            TA = c(addEMA(12, col = 'green'), addEMA(26, col = 'cyan'), 
  #                   addEMA(50, col = 'yellow'), 
  #                   addEMA(100, col = 'red'), addVo()))
  #addRSI(n = 28)
  
  # Advanced plot:
  dygraph(data.all) %>% dyCandlestick() %>%
    dyLegend(show = "onmouseover", hideOnMouseOut = FALSE)
} # End of function

# Define basic plot function:
Basic.Plot.Week <- function(x,r_day_plot,end_day_plot){
  #x <- AAPL; r_day_plot = .8; end_day_plot = 1
  name <- colnames(x)
  x <- to.weekly(x)
  colnames(x) <- name
  daily_initial_time_plot <- r_day_plot*nrow(x)
  daily_ending_time_plot <- end_day_plot*nrow(x)
  data.all <- x[daily_initial_time_plot:daily_ending_time_plot,]
  data.all <- data.all[,-c(5:6)]
  #chartSeries(data,
  #            theme = chartTheme("black"),
  #            name = "Candlestick Chart for Entered Ticker",
  #            TA = c(addEMA(12, col = 'green'), addEMA(26, col = 'cyan'), 
  #                   addEMA(50, col = 'yellow'), 
  #                   addEMA(100, col = 'red'), addVo()))
  #addRSI(n = 28)
  
  # Advanced plot:
  dygraph(data.all) %>% dyCandlestick() %>%
    dyLegend(show = "onmouseover", hideOnMouseOut = FALSE)
} # End of function

# Define basic plot function:
Basic.Plot.Month <- function(x,r_day_plot,end_day_plot){
  #x <- AAPL; r_day_plot = .8; end_day_plot = 1
  name <- colnames(x)
  x <- to.monthly(x)
  colnames(x) <- name
  daily_initial_time_plot <- r_day_plot*nrow(x)
  daily_ending_time_plot <- end_day_plot*nrow(x)
  data.all <- x[daily_initial_time_plot:daily_ending_time_plot,]
  data.all <- data.all[,-c(5:6)]
  #chartSeries(data,
  #            theme = chartTheme("black"),
  #            name = "Candlestick Chart for Entered Ticker",
  #            TA = c(addEMA(12, col = 'green'), addEMA(26, col = 'cyan'), 
  #                   addEMA(50, col = 'yellow'), 
  #                   addEMA(100, col = 'red'), addVo()))
  #addRSI(n = 28)
  
  # Advanced plot:
  dygraph(data.all) %>% dyCandlestick() %>%
    dyLegend(show = "onmouseover", hideOnMouseOut = FALSE)
} # End of function

# Buy signal function:
Buy<-function(x,r_day_plot,end_day_plot,c,height,test.new.price = 0){
  if (test.new.price == 0) {
    x = x
  } else {
    intra.day.test <- data.frame(matrix(c(0,0,0,test.new.price,0,0), nrow = 1))
    rownames(intra.day.test) <- as.character(Sys.Date())
    x = data.frame(rbind(x, intra.day.test))
  }
  Close<-x[,4] # Define Close as adjusted closing price
  # A new function needs redefine data from above:
  # Create SMA for multiple periods
  SMA10<-SMA(Close,n=10)
  SMA20<-SMA(Close,n=20)
  SMA30<-SMA(Close,n=30)
  SMA50<-SMA(Close,n=50)
  SMA200<-SMA(Close,n=200)
  SMA250<-SMA(Close,n=250)
  
  # Create RSI for multiple periods
  RSI10 <- (RSI(Close,n=10)-50)*height*5
  RSI20 <- (RSI(Close,n=20)-50)*height*5
  RSI30 <- (RSI(Close,n=30)-50)*height*5
  RSI50 <- (RSI(Close,n=50)-50)*height*5
  RSI200 <- (RSI(Close,n=200)-50)*height*5
  RSI250 <- (RSI(Close,n=250)-50)*height*5
  
  # Create computable dataset: Close/SMA_i-1
  ratio_10<-(Close/SMA10-1)
  ratio_20<-(Close/SMA20-1)
  ratio_30<-(Close/SMA30-1)
  ratio_50<-(Close/SMA50-1)
  ratio_200<-(Close/SMA200-1)
  ratio_250<-(Close/SMA250-1)
  all_data_ratio <- merge(
    ratio_10,
    ratio_20,
    ratio_30,
    ratio_50,
    ratio_200,
    ratio_250
  )
  # Here we want to create signal for each column
  # Then we add them all together
  all_data_ratio[is.na(all_data_ratio)] <- 0 # Get rid of NAs
  sd(all_data_ratio[,1])
  sd(all_data_ratio[,2])
  sd(all_data_ratio[,3])
  sd(all_data_ratio[,4])
  sd(all_data_ratio[,5])
  sd(all_data_ratio[,6])
  coef<-c
  m<-height*mean(Close)
  all_data_ratio$Sig1<-ifelse(
    all_data_ratio[,1] <= coef*sd(all_data_ratio[,1]), 
    m, "0") 
  all_data_ratio$Sig2<-ifelse(
    all_data_ratio[,2] <= coef*sd(all_data_ratio[,2]), 
    m, "0") 
  all_data_ratio$Sig3<-ifelse(
    all_data_ratio[,3] <= coef*sd(all_data_ratio[,3]), 
    m, "0") 
  all_data_ratio$Sig4<-ifelse(
    all_data_ratio[,4] <= coef*sd(all_data_ratio[,4]), 
    m, "0") 
  all_data_ratio$Sig5<-ifelse(
    all_data_ratio[,5] <= coef*sd(all_data_ratio[,5]), 
    m, "0") 
  all_data_ratio$Sig6<-ifelse(
    all_data_ratio[,6] <= coef*sd(all_data_ratio[,6]), 
    m, "0") 
  
  all_data_ratio$Signal<-(
    all_data_ratio[,7]
    +all_data_ratio[,8]
    +all_data_ratio[,9]
    +all_data_ratio[,10]
    +all_data_ratio[,11]
    +all_data_ratio[,12]
  )
  
  all_data_signal <- merge(Close, all_data_ratio$Signal) 
  # return(all_data_signal)
  # return(ts.plot(all_data_signal,gpars= list(col=c("black","green")),main="Closing Price and Buy Signal Plot"))
  
  daily_initial_time_plot <- r_day_plot*nrow(x)
  daily_ending_time_plot <- end_day_plot*nrow(x)
  plot(Close[daily_initial_time_plot:daily_ending_time_plot,],main="Closing Price with Buy Signal and SMA + RSI",
       ylim = c(-20,(max(Close)+3)))
  par(new=TRUE)
  lines(SMA10[daily_initial_time_plot:daily_ending_time_plot,], col = 5)
  lines(SMA20[daily_initial_time_plot:daily_ending_time_plot,], col = 6)
  lines(SMA30[daily_initial_time_plot:daily_ending_time_plot,], col = 7)
  lines(SMA50[daily_initial_time_plot:daily_ending_time_plot,], col = 8)
  lines(SMA200[daily_initial_time_plot:daily_ending_time_plot,], col = 2)
  lines(SMA250[daily_initial_time_plot:daily_ending_time_plot,], col = 3)
  lines(RSI10[daily_initial_time_plot:daily_ending_time_plot,], col = 4)
  lines(RSI20[daily_initial_time_plot:daily_ending_time_plot,], col = 4)
  lines(RSI30[daily_initial_time_plot:daily_ending_time_plot,], col = 4)
  lines(RSI50[daily_initial_time_plot:daily_ending_time_plot,], col = 4)
  lines(RSI200[daily_initial_time_plot:daily_ending_time_plot,], col = 4)
  lines(RSI250[daily_initial_time_plot:daily_ending_time_plot,], col = 4)
  lines(all_data_ratio$Signal[daily_initial_time_plot:daily_ending_time_plot,], 
        type = "h", col='green')
} # End of function # End of function # End of function
# End of function 

#Buy(SPY,.8,1,-1.2,.1,0)
#Buy(SPY,.8,1,-1.2,.1,100)


# Buy table;
Buy.table<-function(x,r_day_plot,end_day_plot,c,height,past.n.days,test.new.price = 0){
  if (test.new.price == 0) {
    x = x
  } else {
    intra.day.test <- matrix(c(0,0,0,test.new.price,0,0), nrow = 1)
    rownames(intra.day.test) <- as.character(Sys.Date())
    x = data.frame(rbind(x, intra.day.test))
  }
  Close<-x[,4] # Define Close as adjusted closing price
  # A new function needs redefine data from above:
  # Create SMA for multiple periods
  SMA10<-SMA(Close,n=10)
  SMA20<-SMA(Close,n=20)
  SMA30<-SMA(Close,n=30)
  SMA50<-SMA(Close,n=50)
  SMA200<-SMA(Close,n=200)
  SMA250<-SMA(Close,n=250)
  
  # Create RSI for multiple periods
  RSI10 <- (RSI(Close,n=10)-50)*height*5
  RSI20 <- (RSI(Close,n=20)-50)*height*5
  RSI30 <- (RSI(Close,n=30)-50)*height*5
  RSI50 <- (RSI(Close,n=50)-50)*height*5
  RSI200 <- (RSI(Close,n=200)-50)*height*5
  RSI250 <- (RSI(Close,n=250)-50)*height*5
  
  # Create computable dataset: Close/SMA_i-1
  ratio_10<-(Close/SMA10-1)
  ratio_20<-(Close/SMA20-1)
  ratio_30<-(Close/SMA30-1)
  ratio_50<-(Close/SMA50-1)
  ratio_200<-(Close/SMA200-1)
  ratio_250<-(Close/SMA250-1)
  all_data_ratio <- cbind.data.frame(
    ratio_10,
    ratio_20,
    ratio_30,
    ratio_50,
    ratio_200,
    ratio_250
  )
  # Here we want to create signal for each column
  # Then we add them all together
  all_data_ratio[is.na(all_data_ratio)] <- 0 # Get rid of NAs
  sd(all_data_ratio[,1])
  sd(all_data_ratio[,2])
  sd(all_data_ratio[,3])
  sd(all_data_ratio[,4])
  sd(all_data_ratio[,5])
  sd(all_data_ratio[,6])
  coef<-c
  m<-height*mean(Close)
  all_data_ratio$Sig1<-ifelse(
    all_data_ratio[,1] <= coef*sd(all_data_ratio[,1]), 
    m, "0") 
  all_data_ratio$Sig2<-ifelse(
    all_data_ratio[,2] <= coef*sd(all_data_ratio[,2]), 
    m, "0") 
  all_data_ratio$Sig3<-ifelse(
    all_data_ratio[,3] <= coef*sd(all_data_ratio[,3]), 
    m, "0") 
  all_data_ratio$Sig4<-ifelse(
    all_data_ratio[,4] <= coef*sd(all_data_ratio[,4]), 
    m, "0") 
  all_data_ratio$Sig5<-ifelse(
    all_data_ratio[,5] <= coef*sd(all_data_ratio[,5]), 
    m, "0") 
  all_data_ratio$Sig6<-ifelse(
    all_data_ratio[,6] <= coef*sd(all_data_ratio[,6]), 
    m, "0") 
  
  all_data_ratio$Signal <- (
    as.numeric(all_data_ratio[,7])
    + as.numeric(all_data_ratio[,8])
    + as.numeric(all_data_ratio[,9])
    + as.numeric(all_data_ratio[,10])
    + as.numeric(all_data_ratio[,11])
    + as.numeric(all_data_ratio[,12])
  )
  
  all_data_signal <- cbind.data.frame(Close, all_data_ratio$Signal) 

  return(
    #tail(all_data_signal)
    all_data_signal[(nrow(all_data_signal)-past.n.days):nrow(all_data_signal),]
  )
} # End of function # End of function # End of function
# End of function 

#x=SPY; r_day_plot=.8; end_day_plot=1; c=-1.2; height=.1; past.n.days=2; test.new.price=0
#Buy.table(SPY,.8,1,-1.2,.1,2,0)

# Sell table;
Sell.table<-function(x,r_day_plot,end_day_plot,c,height,past.n.days,test.new.price = 0){
  if (test.new.price == 0) {
    x = x
  } else {
    intra.day.test <- matrix(c(0,0,0,test.new.price,0,0), nrow = 1)
    rownames(intra.day.test) <- as.character(Sys.Date())
    x = data.frame(rbind(x, intra.day.test))
  }
  Close<-x[,4] # Define Close as adjusted closing price
  # A new function needs redefine data from above:
  # Create SMA for multiple periods
  SMA10<-SMA(Close,n=10)
  SMA20<-SMA(Close,n=20)
  SMA30<-SMA(Close,n=30)
  SMA50<-SMA(Close,n=50)
  SMA200<-SMA(Close,n=200)
  SMA250<-SMA(Close,n=250)
  
  # Create RSI for multiple periods
  RSI10 <- (RSI(Close,n=10)-50)*height*5
  RSI20 <- (RSI(Close,n=20)-50)*height*5
  RSI30 <- (RSI(Close,n=30)-50)*height*5
  RSI50 <- (RSI(Close,n=50)-50)*height*5
  RSI200 <- (RSI(Close,n=200)-50)*height*5
  RSI250 <- (RSI(Close,n=250)-50)*height*5
  
  # Create computable dataset: Close/SMA_i-1
  ratio_10<-(Close/SMA10-1)
  ratio_20<-(Close/SMA20-1)
  ratio_30<-(Close/SMA30-1)
  ratio_50<-(Close/SMA50-1)
  ratio_200<-(Close/SMA200-1)
  ratio_250<-(Close/SMA250-1)
  all_data_ratio <- cbind.data.frame(
    ratio_10,
    ratio_20,
    ratio_30,
    ratio_50,
    ratio_200,
    ratio_250
  )
  # Here we want to create signal for each column
  # Then we add them all together
  all_data_ratio[is.na(all_data_ratio)] <- 0 # Get rid of NAs
  sd(all_data_ratio[,1])
  sd(all_data_ratio[,2])
  sd(all_data_ratio[,3])
  sd(all_data_ratio[,4])
  sd(all_data_ratio[,5])
  sd(all_data_ratio[,6])
  coef<-c
  m<-height*mean(Close)
  all_data_ratio$Sig1<-ifelse(
    all_data_ratio[,1] >= coef*sd(all_data_ratio[,1]), 
    m, "0") 
  all_data_ratio$Sig2<-ifelse(
    all_data_ratio[,2] >= coef*sd(all_data_ratio[,2]), 
    m, "0") 
  all_data_ratio$Sig3<-ifelse(
    all_data_ratio[,3] >= coef*sd(all_data_ratio[,3]), 
    m, "0") 
  all_data_ratio$Sig4<-ifelse(
    all_data_ratio[,4] >= coef*sd(all_data_ratio[,4]), 
    m, "0") 
  all_data_ratio$Sig5<-ifelse(
    all_data_ratio[,5] >= coef*sd(all_data_ratio[,5]), 
    m, "0") 
  all_data_ratio$Sig6<-ifelse(
    all_data_ratio[,6] >= coef*sd(all_data_ratio[,6]), 
    m, "0") 
  
  all_data_ratio$Signal <- (
    as.numeric(all_data_ratio[,7])
    + as.numeric(all_data_ratio[,8])
    + as.numeric(all_data_ratio[,9])
    + as.numeric(all_data_ratio[,10])
    + as.numeric(all_data_ratio[,11])
    + as.numeric(all_data_ratio[,12])
  )
  
  all_data_signal <- cbind.data.frame(Close, all_data_ratio$Signal) 
  
  return(
    #tail(all_data_signal)
    all_data_signal[(nrow(all_data_signal)-past.n.days):nrow(all_data_signal),]
  )
} # End of function # End of function # End of function
# End of function 

# Buy/Sell Algorithm:
BS.Algo <- function(x,r_day_plot,end_day_plot,c.buy,c.sell,height,past.n.days,test.new.price=0) {
  buy.sell.table <- data.frame(cbind(
    rownames(Buy.table(x,r_day_plot,end_day_plot,c.buy,height,past.n.days)),
    Buy.table(x,r_day_plot,end_day_plot,c.buy,height,past.n.days,test.new.price)[,1:2],
    Sell.table(x,r_day_plot,end_day_plot,c.sell,height,past.n.days,test.new.price)[,2]
  ))
  colnames(buy.sell.table) <- c("Date", "Ticker", "Buy.Signal", "Sell.Signal")
  buy.sell.table
} # End of function

# Buy/Sell Signal Chart:
BS.Algo.Chart <- function(x,r_day_plot,end_day_plot,c.buy,c.sell,height,past.n.days,test.new.price=0) {
  daily_initial_time_plot <- r_day_plot*nrow(x)
  daily_ending_time_plot <- end_day_plot*nrow(x)
  past.n.days <- round(daily_ending_time_plot - daily_initial_time_plot)
  data <- BS.Algo(x,r_day_plot,end_day_plot,c.buy,c.sell,height,
                  past.n.days,test.new.price=0)[, c(3,4)]
  dygraph(data) %>% 
    dyBarChart() %>%
    dyLegend(show = "onmouseover", width = 700, hideOnMouseOut = FALSE)
}

#BS.Algo.Chart(AAPL, .5, 1, -1, 1.5, .1, 500, 0)

# Buy/sell Signal Distribution:
BS.Dist <- function(x,r_day_plot,end_day_plot,c.buy,c.sell,height,test.new.price=0) {
  # x <- SPY; r_day_plot = .8; end_day_plot = 1; c.buy = -.5; c.sell = .5; height = 1
  past.n.days <- nrow(x)
  buy.sell.table <- BS.Algo(x,
                            r_day_plot,end_day_plot,
                            c.buy,c.sell,height, 
                            past.n.days,test.new.price=0)
  
  bs.dist <- matrix(NA,nrow=3,ncol=2)
  bs.dist[1,1] <- round(mean(buy.sell.table[,3]),4)
  bs.dist[2,1] <- round(sd(buy.sell.table[,3]),4)
  bs.dist[3,1] <- round(max(buy.sell.table[,3]),4)
  bs.dist[1,2] <- round(mean(buy.sell.table[,4]),4)
  bs.dist[2,2] <- round(sd(buy.sell.table[,4]),4)
  bs.dist[3,2] <- round(max(buy.sell.table[,4]),4)
  bs.dist <- data.frame(cbind(
    c("Mean", "STD", "MAX"),bs.dist,
    c("Ave. buy/sell signals including zeros",
      "Alpha:>=1STD; Beta:>0; Exit:<80% D/W low",
      "Maximum buy/sell signal ever happened")
  ))
  colnames(bs.dist) <- c("Summary", "Buy.Sig.Dist", "Sell.Sig.Dist", "Indicated Game Plan")
  bs.dist
} # End of function

# Example:
#x,r_day_plot,end_day_plot,c.buy,c.sell,height,past.n.days
#BS.Algo(AAPL, 08, 1, -.5, +.5, 1, 2)
#BS.Dist(SPY, 08, 1, -.5, +.5, 1, 200)

#x <- getSymbols('AAPL')
#Close <- x[,4]
#Return <- Close/lag(Close)-1
#hist(Return)

# Benchmark with market
# Define function
price.to.market.algo <- function(x, #r_day_plot, end_day_plot, c, height, 
                                 past.n.buy,
                                 past.n.days){
  # Get data
  getSymbols("SPY")
  Close<-x[,4] # Define Close as adjusted closing price
  
  # Extract recent data
  #past.n.days = 500
  Close <- Close[(nrow(Close) - past.n.days):nrow(Close),]
  SPY <- SPY[(nrow(SPY) - past.n.days):nrow(SPY),]
  
  # Compute
  price.to.market <- Close/SPY[,4]
  minimum <- min(price.to.market); minimum <- round(minimum, 4)
  maximum <- max(price.to.market); maximum <- round(maximum, 4)
  average <- mean(price.to.market); average <- round(average, 4)
  SD <- sd(price.to.market); SD <- round(SD, 4)
  
  # Compute return beta
  Y = (Close/lag(Close)-1)*100; X = (SPY[,4]/lag(SPY[,4])-1)*100
  LM <- lm(Y ~ X)
  beta = LM$coefficients[2]; beta <- round(beta, 4)
  
  # Main Table Output
  Date <- rownames(data.frame(price.to.market))
  Result <- data.frame(cbind(
    data.frame(Date),
    data.frame(round(price.to.market, 4))
  )); colnames(Result) <- c("Date", "Price-to-Market")
  # Result
  
  # Stats
  statistics <- rbind(
    c("Minimum Price-to-Market = ", minimum),
    c("Average Price-to-Market = ", average),
    c("SD of Price-to-Market = ", SD),
    c("Maximum Price-to-Market = ", maximum),
    c("Beta (Ret ~ MKT) = ", beta))
  colnames(statistics) <- c("Name", "P/M Distribution")
  
  # Output
  print(list(
    Table = Result[(nrow(Result) - past.n.buy):nrow(Result),],
    Stats = statistics
  ))
} # End of function

# Correlation:
Buy.table.corr<-function(x,r_day_plot,end_day_plot,c,height,past.n.days){
  # x <- AAPL
  getSymbols(c('SPY','QQQ','DIA','IWM','GLD'))
  M <- data.frame(
    Buy.table(x,r_day_plot,end_day_plot,c,height,past.n.days)[,1],
    Buy.table(SPY,r_day_plot,end_day_plot,c,height,past.n.days)[,1],
    Buy.table(QQQ,r_day_plot,end_day_plot,c,height,past.n.days)[,1],
    Buy.table(DIA,r_day_plot,end_day_plot,c,height,past.n.days)[,1],
    Buy.table(IWM,r_day_plot,end_day_plot,c,height,past.n.days)[,1],
    Buy.table(GLD,r_day_plot,end_day_plot,c,height,past.n.days)[,1]
  )
  M.update <- cor(M)
  colnames(M.update) = c("Corr.","SPY","QQQ","DIA","IWM","GLD")
  rownames(M.update) = c("Corr.","SPY","QQQ","DIA","IWM","GLD")
  return(data.frame(M.update))
  #corrplot(M.update, method = "number", type = "upper")
} # End of function:

#Buy.table.corr(AAPL,.8, 1,-.5,1.2,10)

##################### WATCH LIST ################################
# Dow Jones Watch List
library('quantmod')
library('DT')
DIA.Watch.List <- function(
  buy.height = -1.96,
  past.days = 3
) {
  # Download Stock Pool:
  data <- getSymbols(c(
    # DJIA
    "SPY", "AXP", "AAPL", "BA", "CVX", "CSCO",
    "DIS", "XOM", "GE", "GS", "HD", "IBM", "INTC",
    "JNJ", "JPM", "MCD", "MRK", "MSFT", "NKE", 
    "PFE", "PG", "UTX", "UNH", 
    "V", "WMT"
  ))
  data.list <- list(
    # DJIA
    "SPY", "AXP", "AAPL", "BA", "CVX", "CSCO",
    "DIS", "XOM", "GE", "GS", "HD", "IBM", "INTC",
    "JNJ", "JPM", "MCD", "MRK", "MSFT", "NKE", 
    "PFE", "PG", "UTX", "UNH", 
    "V", "WMT"
  )
  data.data.matrix <- list(
    SPY, AXP, AAPL, BA, CVX, CSCO, 
    DIS, XOM, GE, GS, HD, IBM, INTC,
    JNJ, JPM, MCD, MRK, MSFT, NKE,
    PFE, PG, UTX, UNH, 
    V, WMT
  )
  
  # Watch List:
  #print("BRIDGEWATER PICKS")
  #buy.height <- -1.96
  #past.days <- 4
  buy.watch.list <- NULL
  # for (i in 1:length(data.data.matrix)){print(c(i, dim(data.data.matrix[[i]])))}
  for (i in 1:length(data.data.matrix)) {
    buy.watch.list <- data.frame(cbind(
      buy.watch.list,
      c(
        round(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, past.days)[,3],2),
        round(mean(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, nrow(data.data.matrix[[i]]))[,3]),2),
        round(sd(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, nrow(data.data.matrix[[i]]))[,3]),2)
      )
    ))
  }
  rownames(buy.watch.list) <- c(as.character(BS.Algo(SPY, .9, 1, buy.height, 2.9, 1, past.days)[,1]), "Mean", "SD")
  colnames(buy.watch.list) <- data.list
  buy.watch.list <- t(buy.watch.list)
  buy.watch.list <- data.frame(cbind(
    Name = rownames(buy.watch.list),
    buy.watch.list
  ))
  
  # Present table:
  return(buy.watch.list)
} # End of function

# Run
# Bridgewater.Watch.List(buy.height = -1.96)

# QQQ Watch List
library('quantmod')
library('DT')
QQQ.Watch.List <- function(
  buy.height = -1.96,
  past.days = 3
) {
  # Download Stock Pool:
  data <- getSymbols(c(
    # DJIA
    "SPY", "QQQ", "AAPL", "AMZN", "MSFT",
    "FB", "GOOG", "GOOGL", "INTC", "CSCO",
    "NFLX", "NVDA", "CMCSA", "AMGN", "ADBE",
    "TXN"
  ))
  data.list <- list(
    # DJIA
    "SPY", "QQQ", "AAPL", "AMZN", "MSFT",
    "FB", "GOOG", "GOOGL", "INTC", "CSCO",
    "NFLX", "NVDA", "CMCSA", "AMGN", "ADBE",
    "TXN"
  )
  data.data.matrix <- list(
    SPY, QQQ, AAPL, AMZN, MSFT, 
    FB, GOOG, GOOGL, INTC, CSCO, 
    NFLX, NVDA, CMCSA, AMGN, ADBE,
    TXN
  )
  
  # Watch List:
  #print("BRIDGEWATER PICKS")
  #buy.height <- -1.96
  #past.days <- 4
  buy.watch.list <- NULL
  # for (i in 1:length(data.data.matrix)){print(c(i, dim(data.data.matrix[[i]])))}
  for (i in 1:length(data.data.matrix)) {
    buy.watch.list <- data.frame(cbind(
      buy.watch.list,
      c(
        round(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, past.days)[,3],2),
        round(mean(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, nrow(data.data.matrix[[i]]))[,3]),2),
        round(sd(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, nrow(data.data.matrix[[i]]))[,3]),2)
      )
    ))
  }
  rownames(buy.watch.list) <- c(as.character(BS.Algo(SPY, .9, 1, buy.height, 2.9, 1, past.days)[,1]), "Mean", "SD")
  colnames(buy.watch.list) <- data.list
  buy.watch.list <- t(buy.watch.list)
  buy.watch.list <- data.frame(cbind(
    Name = rownames(buy.watch.list),
    buy.watch.list
  ))
  
  # Present table:
  return(buy.watch.list)
} # End of function

# XLF
XLF.Watch.List <- function(
  buy.height = -1.96,
  past.days = 3
) {
  # Download Stock Pool:
  data <- getSymbols(c(
    # DJIA
    "SPY", "XLF", "JPM", "BAC", "WFC",
    "USB", "GS", "AXP", "MS", "PNC",
    "BLK", "SCHW", "BK"
  ))
  data.list <- list(
    # DJIA
    "SPY", "XLF", "JPM", "BAC", "WFC",
    "USB", "GS", "AXP", "MS", "PNC",
    "BLK", "SCHW", "BK"
  )
  data.data.matrix <- list(
    SPY, XLF, JPM, BAC, WFC,
    USB, GS, AXP, MS, PNC,
    BLK, SCHW, BK
  )
  
  # Watch List:
  #print("BRIDGEWATER PICKS")
  #buy.height <- -1.96
  #past.days <- 4
  buy.watch.list <- NULL
  # for (i in 1:length(data.data.matrix)){print(c(i, dim(data.data.matrix[[i]])))}
  for (i in 1:length(data.data.matrix)) {
    buy.watch.list <- data.frame(cbind(
      buy.watch.list,
      c(
        round(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, past.days)[,3],2),
        round(mean(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, nrow(data.data.matrix[[i]]))[,3]),2),
        round(sd(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, nrow(data.data.matrix[[i]]))[,3]),2)
      )
    ))
  }
  rownames(buy.watch.list) <- c(as.character(BS.Algo(SPY, .9, 1, buy.height, 2.9, 1, past.days)[,1]), "Mean", "SD")
  colnames(buy.watch.list) <- data.list
  buy.watch.list <- t(buy.watch.list)
  buy.watch.list <- data.frame(cbind(
    Name = rownames(buy.watch.list),
    buy.watch.list
  ))
  
  # Present table:
  return(buy.watch.list)
} # End of function

# XLI
XLI.Watch.List <- function(
  buy.height = -1.96,
  past.days = 3
) {
  # Download Stock Pool:
  data <- getSymbols(c(
    # DJIA
    "SPY", "XLI", "BA", "GE", "MMM",
    "UNP", "HON", "UTX", "CAT", "LMT",
    "UPS", "FDX", "CSX", "RTN"
  ))
  data.list <- list(
    # DJIA
    "SPY", "XLI", "BA", "GE", "MMM",
    "UNP", "HON", "UTX", "CAT", "LMT",
    "UPS", "FDX", "CSX", "RTN"
  )
  data.data.matrix <- list(
    SPY, XLI, BA, GE, MMM,
    UNP, HON, UTX, CAT, LMT, 
    UPS, FDX, CSX, RTN
  )
  
  # Watch List:
  #print("BRIDGEWATER PICKS")
  #buy.height <- -1.96
  #past.days <- 4
  buy.watch.list <- NULL
  # for (i in 1:length(data.data.matrix)){print(c(i, dim(data.data.matrix[[i]])))}
  for (i in 1:length(data.data.matrix)) {
    buy.watch.list <- data.frame(cbind(
      buy.watch.list,
      c(
        round(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, past.days)[,3],2),
        round(mean(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, nrow(data.data.matrix[[i]]))[,3]),2),
        round(sd(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, nrow(data.data.matrix[[i]]))[,3]),2)
      )
    ))
  }
  rownames(buy.watch.list) <- c(as.character(BS.Algo(SPY, .9, 1, buy.height, 2.9, 1, past.days)[,1]), "Mean", "SD")
  colnames(buy.watch.list) <- data.list
  buy.watch.list <- t(buy.watch.list)
  buy.watch.list <- data.frame(cbind(
    Name = rownames(buy.watch.list),
    buy.watch.list
  ))
  
  # Present table:
  return(buy.watch.list)
} # End of function

# XLP
XLP.Watch.List <- function(
  buy.height = -1.96,
  past.days = 3
) {
  # Download Stock Pool:
  data <- getSymbols(c(
    # DJIA
    "SPY", "XLP", "PG", "KO", "PEP",
    "PM", "WMT", "COST", "MO", "MDLZ",
    "CL", "WBA", "KHC", "STZ"
  ))
  data.list <- list(
    # DJIA
    "SPY", "XLP", "PG", "KO", "PEP",
    "PM", "WMT", "COST", "MO", "MDLZ",
    "CL", "WBA", "KHC", "STZ"
  )
  data.data.matrix <- list(
    SPY, XLP, PG, KO, PEP,
    PM, WMT, COST, MO, MDLZ, 
    CL, WBA, KHC, STZ
  )
  
  # Watch List:
  #print("BRIDGEWATER PICKS")
  #buy.height <- -1.96
  #past.days <- 4
  buy.watch.list <- NULL
  # for (i in 1:length(data.data.matrix)){print(c(i, dim(data.data.matrix[[i]])))}
  for (i in 1:length(data.data.matrix)) {
    buy.watch.list <- data.frame(cbind(
      buy.watch.list,
      c(
        round(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, past.days)[,3],2),
        round(mean(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, nrow(data.data.matrix[[i]]))[,3]),2),
        round(sd(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, nrow(data.data.matrix[[i]]))[,3]),2)
      )
    ))
  }
  rownames(buy.watch.list) <- c(as.character(BS.Algo(SPY, .9, 1, buy.height, 2.9, 1, past.days)[,1]), "Mean", "SD")
  colnames(buy.watch.list) <- data.list
  buy.watch.list <- t(buy.watch.list)
  buy.watch.list <- data.frame(cbind(
    Name = rownames(buy.watch.list),
    buy.watch.list
  ))
  
  # Present table:
  return(buy.watch.list)
} # End of function

# XLU
XLU.Watch.List <- function(
  buy.height = -1.96,
  past.days = 3
) {
  # Download Stock Pool:
  data <- getSymbols(c(
    # DJIA
    "SPY", "XLU", "NEE", "DUK", 
    "SO", "D", "EXC", "AEP", 
    "SRE", "PEG", "ED", "XEL", 
    "PCG", "EIX"
  ))
  data.list <- list(
    # DJIA
    "SPY", "XLU", "NEE", "DUK", 
    "SO", "D", "EXC", "AEP", 
    "SRE", "PEG", "ED", "XEL", 
    "PCG", "EIX"
  )
  data.data.matrix <- list(
    SPY, XLU, NEE, DUK,
    SO, D, EXC, AEP, 
    SRE, PEG, ED, XEL,
    PCG, EIX
  )
  
  # Watch List:
  #print("BRIDGEWATER PICKS")
  #buy.height <- -1.96
  #past.days <- 4
  buy.watch.list <- NULL
  # for (i in 1:length(data.data.matrix)){print(c(i, dim(data.data.matrix[[i]])))}
  for (i in 1:length(data.data.matrix)) {
    buy.watch.list <- data.frame(cbind(
      buy.watch.list,
      c(
        round(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, past.days)[,3],2),
        round(mean(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, nrow(data.data.matrix[[i]]))[,3]),2),
        round(sd(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, nrow(data.data.matrix[[i]]))[,3]),2)
      )
    ))
  }
  rownames(buy.watch.list) <- c(as.character(BS.Algo(SPY, .9, 1, buy.height, 2.9, 1, past.days)[,1]), "Mean", "SD")
  colnames(buy.watch.list) <- data.list
  buy.watch.list <- t(buy.watch.list)
  buy.watch.list <- data.frame(cbind(
    Name = rownames(buy.watch.list),
    buy.watch.list
  ))
  
  # Present table:
  return(buy.watch.list)
} # End of function

# XLV
XLV.Watch.List <- function(
  buy.height = -1.96,
  past.days = 3
) {
  # Download Stock Pool:
  data <- getSymbols(c(
    # DJIA
    "SPY", "XLV", "JNJ", "UNH", "PFE", 
    "MRK", "ABBV", "AMGN", "MDT", "ABT", 
    "GILD", "BMY", "LLY",
    "BIIB", "CVS"
  ))
  data.list <- list(
    # DJIA
    "SPY", "XLV", "JNJ", "UNH", "PFE", 
    "MRK", "ABBV", "AMGN", "MDT", "ABT", 
    "GILD", "BMY", "LLY",
    "BIIB", "CVS"
  )
  data.data.matrix <- list(
    SPY, XLV, JNJ, UNH, PFE,
    MRK, ABBV, AMGN, MDT, ABT, 
    GILD, BMY, LLY,
    BIIB, CVS
  )
  
  # Watch List:
  #print("BRIDGEWATER PICKS")
  #buy.height <- -1.96
  #past.days <- 4
  buy.watch.list <- NULL
  # for (i in 1:length(data.data.matrix)){print(c(i, dim(data.data.matrix[[i]])))}
  for (i in 1:length(data.data.matrix)) {
    buy.watch.list <- data.frame(cbind(
      buy.watch.list,
      c(
        round(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, past.days)[,3],2),
        round(mean(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, nrow(data.data.matrix[[i]]))[,3]),2),
        round(sd(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, nrow(data.data.matrix[[i]]))[,3]),2)
      )
    ))
  }
  rownames(buy.watch.list) <- c(as.character(BS.Algo(XLV, .9, 1, buy.height, 2.9, 1, past.days)[,1]), "Mean", "SD")
  colnames(buy.watch.list) <- data.list
  buy.watch.list <- t(buy.watch.list)
  buy.watch.list <- data.frame(cbind(
    Name = rownames(buy.watch.list),
    buy.watch.list
  ))
  
  # Present table:
  return(buy.watch.list)
} # End of function

# Semi-Conductor
Semi.Conductor.Watch.List <- function(
  buy.height = -1.96,
  past.days = 3
) {
  # Download Stock Pool:
  data <- getSymbols(c(
    # DJIA
    "SPY", "NVDA", "AMD", "AVGO",
    "IDTI", "MCHP", "MXIM", "TXN", "ADI", 
    "XLNX", "MU"
  ))
  data.list <- list(
    # DJIA
    "SPY", "NVDA", "AMD", "AVGO",
    "IDTI", "MCHP", "MXIM", "TXN", "ADI", 
    "XLNX", "MU"
  )
  data.data.matrix <- list(
    SPY, NVDA, AMD, AVGO,
    IDTI, MCHP, MXIM, TXN, ADI,
    XLNX, MU
  )
  
  # Watch List:
  #print("BRIDGEWATER PICKS")
  #buy.height <- -1.96
  #past.days <- 4
  buy.watch.list <- NULL
  # for (i in 1:length(data.data.matrix)){print(c(i, dim(data.data.matrix[[i]])))}
  for (i in 1:length(data.data.matrix)) {
    buy.watch.list <- data.frame(cbind(
      buy.watch.list,
      c(
        round(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, past.days)[,3],2),
        round(mean(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, nrow(data.data.matrix[[i]]))[,3]),2),
        round(sd(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, nrow(data.data.matrix[[i]]))[,3]),2)
      )
    ))
  }
  rownames(buy.watch.list) <- c(as.character(BS.Algo(SPY, .9, 1, buy.height, 2.9, 1, past.days)[,1]), "Mean", "SD")
  colnames(buy.watch.list) <- data.list
  buy.watch.list <- t(buy.watch.list)
  buy.watch.list <- data.frame(cbind(
    Name = rownames(buy.watch.list),
    buy.watch.list
  ))
  
  # Present table:
  return(buy.watch.list)
} # End of function

# 3D Plot
All.Indice.3D <- function() {
  data <- getSymbols(c(
    "SPY", "DIA", "QQQ", "IWM", "GLD", "XLB", "XLE", "XLK", "XLU", "XLI", "XLP", "XLY",
    "EWC", "EWG", "EWJ", "EWZ", "FEZ", "FXI", "GDX", "GLD", "IBB", "INDA", "IVV", "SPXL", "TLT", "TQQQ",
    "XBI", "ITA", "IYZ", "HACK", "KRE", "MOO", "SOCL", "XHB", "IAK"
  )); data
  data.list <- list(
    SPY, DIA, QQQ, IWM, GLD, XLB, XLE, XLK, XLU, XLI, XLP, XLY,
    EWC, EWG, EWJ, EWZ, FEZ, FXI, GDX, GLD, IBB, INDA, IVV, SPXL, TLT, TQQQ,
    XBI, ITA, IYZ, HACK, KRE, MOO, SOCL, XHB, IAK
  )
  
  # Create data set:
  all <- matrix(NA,nrow=length(data),ncol=12)
  rownames(all) <- data
  
  # Update Price (Current, daily basis):
  for (i in c(1:nrow(all))){
    all[i,2] <- data.frame(data.list[i])[nrow(data.frame(data.list[i])),4]
  }
  
  # Update Momentum:
  for (i in c(1:nrow(all))){
    all[i,5] <- (data.frame(data.list[i])[nrow(data.frame(data.list[i])),4])/(data.frame(data.list[i])[(nrow(data.frame(data.list[i]))-5),4])-1
  }
  for (i in c(1:nrow(all))){
    all[i,6] <- (data.frame(data.list[i])[nrow(data.frame(data.list[i])),4])/(data.frame(data.list[i])[(nrow(data.frame(data.list[i]))-25),4])-1
  }
  for (i in c(1:nrow(all))){
    all[i,7] <- (data.frame(data.list[i])[nrow(data.frame(data.list[i])),4])/(data.frame(data.list[i])[(nrow(data.frame(data.list[i]))-25*3),4])-1
  }
  for (i in c(1:nrow(all))){
    all[i,8] <- (data.frame(data.list[i])[nrow(data.frame(data.list[i])),4])/(data.frame(data.list[i])[(nrow(data.frame(data.list[i]))-252),4])-1
  }
  
  # Clearn up
  all <- all[,-c(1,3,4,9,10,11,12)]
  
  # Update column names:
  colnames(all) <- c("Last Price",
                     "Pre 5-Days",
                     "Pre 30-Days", 
                     "Pre Quarter",
                     "Pre Year")
  
  # Quick vertical bar plot:
  counts <- all[,2]
  counts.std <- sd(all[,2])
  #barplot(counts, main="5-Day Return Bar Chart", #horiz=TRUE,
  #        names.arg=rownames(all), cex.names=0.35,
  #        col=ifelse(counts>counts.std,"green",ifelse(counts<(-1)*counts.std,"red","pink")))
  
  # Sortable table:
  library('DT')
  # Present table:
  d = data.frame(
    round(all[,c(2,3,4,5)],4),
    #round(all[,c(2,3,4,5)],4),
    stringsAsFactors = FALSE)
  #d <- data.frame(cbind(
  #  rownames(d),
  #  d
  #))
  #colnames(d)[1] <- "Name"
  
  # Output
  return(d)
} # End of function

# Slider Matplot
All.Indice.3D.Slide <- function() {
  d.parcoord <- All.Indice.3D()
  parcoords(d.parcoord,
            #All.Indice.3D(),
            rownames = T, 
            brushMode = "1d"#, 
            #reorderable = T, 
            #queue = F, 
            #color = list(colorBy = rownames(d.parcoord))
            )
}

# All.Indice.3D.Enter
All.Indice.3D.Enter <- function(
  a,b,c,d, e,f,g,h
) {
  # Data
  data.list <- list(
    a,b,c,d, e,f,g,h
  )
  all <- matrix(NA, nrow = 8, ncol = 4)
  
  # Update Momentum:
  for (i in c(1:nrow(all))){
    all[i,1] <- (data.frame(data.list[i])[nrow(data.frame(data.list[i])),4])/(data.frame(data.list[i])[(nrow(data.frame(data.list[i]))-5),4])-1
  }
  for (i in c(1:nrow(all))){
    all[i,2] <- (data.frame(data.list[i])[nrow(data.frame(data.list[i])),4])/(data.frame(data.list[i])[(nrow(data.frame(data.list[i]))-25),4])-1
  }
  for (i in c(1:nrow(all))){
    all[i,3] <- (data.frame(data.list[i])[nrow(data.frame(data.list[i])),4])/(data.frame(data.list[i])[(nrow(data.frame(data.list[i]))-25*3),4])-1
  }
  for (i in c(1:nrow(all))){
    all[i,4] <- (data.frame(data.list[i])[nrow(data.frame(data.list[i])),4])/(data.frame(data.list[i])[(nrow(data.frame(data.list[i]))-252),4])-1
  }
  
  # Update column names:
  colnames(all) <- c("Pre 5-Days",
                     "Pre 30-Days", 
                     "Pre Quarter",
                     "Pre Year")
  df <- data.frame(all)
  df
}

# For portfolio monitor
# Portfolio.Watch.List
Portfolio.Watch.List <- function(
  buy.height = -1.96,
  past.days = 3
) {
  # Download Stock Pool:
  data <- getSymbols(c(
    # Portfolio
    "SPY", "AAPL", "FB", "MSFT", "GOOG", "TSLA", "CRM", "BIIB", "BA"
  ))
  data.list <- list(
    # Portfolio
    "SPY", "AAPL", "FB", "MSFT", "GOOG", "TSLA", "CRM", "BIIB", "BA"
  )
  data.data.matrix <- list(
    SPY, AAPL, FB, MSFT, GOOG, TSLA, CRM, BIIB, BA
  )
  
  # Watch List:
  #print("BRIDGEWATER PICKS")
  #buy.height <- -1.96
  #past.days <- 4
  buy.watch.list <- NULL
  # for (i in 1:length(data.data.matrix)){print(c(i, dim(data.data.matrix[[i]])))}
  for (i in 1:length(data.data.matrix)) {
    buy.watch.list <- data.frame(cbind(
      buy.watch.list,
      c(
        round(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, past.days)[,3],2),
        round(mean(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, nrow(data.data.matrix[[i]]))[,3]),2),
        round(sd(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, 2.9, 0.01, nrow(data.data.matrix[[i]]))[,3]),2)
      )
    ))
  }
  rownames(buy.watch.list) <- c(as.character(BS.Algo(SPY, .9, 1, buy.height, 2.9, 1, past.days)[,1]), "Mean", "SD")
  colnames(buy.watch.list) <- data.list
  buy.watch.list <- t(buy.watch.list)
  buy.watch.list <- data.frame(cbind(
    Name = rownames(buy.watch.list),
    buy.watch.list
  ))
  
  # Present table:
  return(buy.watch.list)
} # End of function

######################### FEATURE SELECTION ################################

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

####################### SEARCHING FOR WEIGHTS: DEF FCT ######################

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

# I-SCORE Watch List
library('quantmod')
library('DT')
Mkt.Watch.List <- function(
  base.dollar = 1000, 
  buy.height = -1.96,
  top.how.many = 7,
  k = 12, # Size of Initial Draw
  rounds = 200, # How many rounds of BDA?
  n.neighbor = 3 # Size of nearest neighbor
) {
  # Download Stock Pool:
  data <- getSymbols(c(
    # Technology
    "QQQ", "GOOGL", "AMZN", "MSFT", "FB", "INTC", 
    "CSCO", "TSM", "ORCL", "NVDA", "SAP", "IBM", 
    "ADBE", "TXN", "AVGO", "ACN", "CRM", "QCOM", 
    "ASML", "BIDU", "MU", "VMW", "ADP", "ATVI", 
    "INTU", "MCHP",
    
    "AIG", "BAC", "WFC", "AXP", "BK", "DFS", "GS",
    
    "ABBV", "AMGN", "AZN", "BMY", "BAX", "CELG",
    "CVS", "JNJ", "LLY", "MDT", "MRK", "UNH", "GILD",
    "BIIB",
    
    "BA", "CAT", "LMT", "DE", "GD", "HON", "UTX"
  ), from = "2010-01-01")
  data.list <- list(
    # Technology
    "QQQ", "GOOGL", "AMZN", "MSFT", "FB", "INTC", 
    "CSCO", "TSM", "ORCL", "NVDA", "SAP", "IBM", 
    "ADBE", "TXN", "AVGO", "ACN", "CRM", "QCOM", 
    "ASML", "BIDU", "MU", "VMW", "ADP", "ATVI", 
    "INTU", "MCHP",
    
    "AIG", "BAC", "WFC", "AXP", "BK", "DFS", "GS",
    
    "ABBV", "AMGN", "AZN", "BMY", "BAX", "CELG",
    "CVS", "JNJ", "LLY", "MDT", "MRK", "UNH", "GILD",
    "BIIB",
    
    "BA", "CAT", "LMT", "DE", "GD", "HON", "UTX"
  )
  data.data.matrix <- list(
    QQQ, GOOGL, AMZN, MSFT, FB, INTC, CSCO, 
    TSM, ORCL, NVDA, SAP, IBM, ADBE, TXN, 
    AVGO, ACN, CRM, QCOM, ASML, BIDU, MU, 
    VMW, ADP, ATVI, INTU, MCHP,
    
    AIG, BAC, WFC, AXP, BK, DFS, GS,
    
    ABBV, AMGN, AZN, BMY, BAX, CELG,
    CVS, JNJ, LLY, MDT, MRK, UNH, GILD, BIIB,
    
    BA, CAT, LMT, DE, GD, HON, UTX
  )
  
  # Watch List:
  # Create Data
  #buy.height <- -1
  sell.height <- .9
  buy.watch.list <- NULL
  how.many.days.past <- 500
  for (i in 1:length(data.data.matrix)) {
    buy.watch.list <- data.frame(cbind(
      buy.watch.list,
      c(round(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, sell.height, 0.01, how.many.days.past)[,4],2))
    ))
  }
  rownames(buy.watch.list) <- c(as.character(BS.Algo(QQQ, .9, 1, buy.height, 2.9, 1, how.many.days.past)[,1]))
  colnames(buy.watch.list) <- data.list
  
  # Run
  all <- buy.watch.list
  #set.seed(1)
  #k = 15
  #start_time <- Sys.time()
  Result <- continuous.vs(
    all = all,
    cut_off = 1,
    num.initial.set = k,
    how.many.rounds = rounds,
    i.want = 1,
    num.top.rows = rounds,
    seed = 1,
    K.means.K = n.neighbor # round(.5*k)
  ); #end_time <- Sys.time(); end_time - start_time
  
  # Variables selected are from I+BDA function:
  Result$Top.BDA.Modules <- data.frame(cbind(
    c(1:nrow(Result$Top.BDA.Modules)),
      Result$Top.BDA.Modules)
  )
  colnames(Result$Top.BDA.Modules) <- c("No.", "Top Modules", "Influence Measure")
  top.how.many <- ifelse(
    top.how.many <= nrow(Result$Top.BDA.Modules),
    top.how.many,
    nrow(Result$Top.BDA.Modules)
  )
  
  which.row <- 1:1
  all.var.new <- Result$Top.BDA.Modules
  selected.variable <- all.var.new[which.row,  2]
  selected.variable <- c(unlist(strsplit(as.character(selected.variable), split="_")))
  selected.variable <- unique(selected.variable)
  selected.variable
  
  # Extract Variables by BDA Results:
  all.data <- all
  all <- all.data
  all <- data.frame(
    cbind(
      all[,1],
      #all[,c(as.character(selected.variable))] # My Iscore Function
      all[, -1][,c(selected.variable)] # Tian Iscore Function
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
  k = k
  new.var <- cont_var(all[,-1], all[,1], K=round(.5*k), selected.variable); end_time <- Sys.time()
  end_time - start_time
  
  # Add clusters back to data set:
  all.copy <- data.frame(cbind(all.copy,new.var)); dim(all.copy)
  all <- all.copy; colnames(all); 
  all <- all[, c(1, ncol(all))]; dim(all); all.copy = all
  
  # Create clusters
  i = 1
  while (i < top.how.many) {
    # Selected Variable
    which.row <- i + 1
    all.var.new <- Result$Top.BDA.Modules
    selected.variable <- all.var.new[which.row,  2]
    selected.variable <- c(unlist(strsplit(as.character(selected.variable), split="_")))
    selected.variable <- unique(selected.variable)
    selected.variable
    all <- all.data
    all <- data.frame(
      cbind(
        all[,1],
        #all[,c(as.character(selected.variable))] # My Iscore Function
        all[, -1][,c(selected.variable)] # Tian Iscore Function
      )
    )
    names(all)[[1]] <- "label"
    length(selected.variable); dim(all) # Check!
    
    # Run function
    # Call this variable cont_var
    # output is a variable
    # with same length as the data set
    new.var <- new.var <- cont_var(all[,-1], all[,1], K=round(.5*k), selected.variable)
    
    # Add clusters back to data set:
    all.copy <- data.frame(cbind(all.copy,new.var)); dim(all.copy)
    all <- all.copy; colnames(all); dim(all)
    
    # i add 1
    i <- i + 1
  } # Finished updating clusters
  
  # MODEL FITTING / MACHINE LEARNING: 
  # Tuning:
  # Tune
  # Solve Optimization Problem
  X <- as.matrix(all[,-1], nrow=nrow(all[,-1]))
  Y <- all[,1]
  num.col <- ncol(X)
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
  
  # Output
  Var.Result <- data.frame(cbind(Result$Top.BDA.Modules[1:(top.how.many), ], Weights = weights.sol))

  # Output: Portfolio in Detail
  Detail.Portfolio <- NULL
  for (which.row in 1:nrow(Var.Result)) {
    selected.variable <- Var.Result[which.row,2]
    selected.variable <- c(unlist(strsplit(as.character(selected.variable), split="_")))
    selected.variable <- unique(selected.variable)
    selected.stock <- matrix(c(selected.variable, 
                               rep(
                                 round(Var.Result[which.row,4]/length(selected.variable), 2), 
                                 length(selected.variable))),
                             nrow = length(selected.variable))
    Detail.Portfolio <- rbind(Detail.Portfolio, selected.stock)
  }; colnames(Detail.Portfolio) <- c("Holdings", "Weight")
  
  # Sort the Detail.Portfolio by Stocks
  Detail.Portfolio <- data.frame(cbind(
    Stocks = as.character(as.factor(plyr::count(Detail.Portfolio)[,1])),
    Weights = as.numeric(as.character(plyr::count(Detail.Portfolio)[,2]))*plyr::count(Detail.Portfolio)[,3]
  ))
  
  # Aggregate the same stocks
  Detail.Portfolio
  Pi <- plyr::count(Detail.Portfolio, "Stocks")
  Final.Portfolio <- NULL
  for (i in 1:nrow(Pi)) {
    Final.Portfolio <- rbind(Final.Portfolio, 
                             c(as.character(Pi[i, 1]), 
                               sum(as.numeric(as.character(Detail.Portfolio[1:Pi[i, 2], 2])
                               ))
                             )
    )
    i <- i + 1
  } # End of loop
  Final.Portfolio[, 2] <- round(
    as.numeric(as.character(Final.Portfolio[, 2]))/sum(as.numeric(as.character(Final.Portfolio[, 2]))),
    2
  ) # End of round
  colnames(Final.Portfolio) <- c("Stocks", "Weights")
  
  # Present table:
  return(list(
    Top.Modules.Result = data.frame(cbind(Result$Top.BDA.Modules[1:(top.how.many), ], Weights = weights.sol))[, -1],
    Detail.Portfolio.Weight = Final.Portfolio
    ))
} # End of function

# Simulation
Mkt.Watch.List.Plot <- function(
  base.dollar = 1000, 
  buy.height = -1.96,
  top.how.many = 3,
  date.cut = "2016-01-26",
  k = 7, # Size of Initial Draw
  rounds = 10, # How many rounds of BDA?
  n.neighbor = 3 # Size of nearest neighbor
) {
  # Download Stock Pool:
  data <- getSymbols(c(
    # Technology
    "QQQ", "GOOGL", "AMZN", "MSFT", "FB", "INTC", 
    "CSCO", "TSM", "ORCL", "NVDA", "SAP", "IBM", 
    "ADBE", "TXN", "AVGO", "ACN", "CRM", "QCOM", 
    "ASML", "BIDU", "MU", "VMW", "ADP", "ATVI", 
    "INTU", "MCHP",
    
    "AIG", "BAC", "WFC", "AXP", "BK", 
    
    "AMGN", "AZN", "BMY", "BAX", "CELG",
    "CVS", "JNJ", "LLY", "MDT", "MRK", "UNH", "GILD",
    "BIIB",
    
    "BA", "CAT", "LMT", "DE", "GD", "HON", "UTX"
  ), from = "2005-01-01", to = date.cut) # Here we should update date.cut
  data.list <- list(
    # Technology
    "QQQ", "GOOGL", "AMZN", "MSFT", "FB", "INTC", 
    "CSCO", "TSM", "ORCL", "NVDA", "SAP", "IBM", 
    "ADBE", "TXN", "AVGO", "ACN", "CRM", "QCOM", 
    "ASML", "BIDU", "MU", "VMW", "ADP", "ATVI", 
    "INTU", "MCHP",
    
    "AIG", "BAC", "WFC", "AXP", "BK",
    
    "AMGN", "AZN", "BMY", "BAX", "CELG",
    "CVS", "JNJ", "LLY", "MDT", "MRK", "UNH", "GILD",
    "BIIB",
    
    "BA", "CAT", "LMT", "DE", "GD", "HON", "UTX"
  )
  data.data.matrix <- list(
    QQQ, GOOGL, AMZN, MSFT, FB, INTC, CSCO, 
    TSM, ORCL, NVDA, SAP, IBM, ADBE, TXN, 
    AVGO, ACN, CRM, QCOM, ASML, BIDU, MU, 
    VMW, ADP, ATVI, INTU, MCHP,
    
    AIG, BAC, WFC, AXP, BK,
    
    AMGN, AZN, BMY, BAX, CELG,
    CVS, JNJ, LLY, MDT, MRK, UNH, GILD, BIIB,
    
    BA, CAT, LMT, DE, GD, HON, UTX
  )
  
  # Watch List:
  # Create Data
  #buy.height <- -1
  sell.height <- .5
  buy.watch.list <- NULL
  how.many.days.past <- 200
  for (i in 1:length(data.data.matrix)) {
    buy.watch.list <- data.frame(cbind(
      buy.watch.list,
      c(round(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, sell.height, 0.01, how.many.days.past)[,4],2))
    ))
  }
  rownames(buy.watch.list) <- c(as.character(BS.Algo(QQQ, .9, 1, buy.height, 2.9, 1, how.many.days.past)[,1]))
  colnames(buy.watch.list) <- data.list
  
  # Run
  all <- buy.watch.list
  #set.seed(1)
  #k = 15
  #start_time <- Sys.time()
  Result <- continuous.vs(
    all = all,
    cut_off = 1,
    num.initial.set = k,
    how.many.rounds = rounds,
    i.want = 1,
    num.top.rows = rounds,
    seed = 1,
    K.means.K = n.neighbor # round(.5*k)
  ); #end_time <- Sys.time(); end_time - start_time
  
  # Variables selected are from I+BDA function:
  all.var.new <- Result$Top.BDA.Modules

  # Select
  top.how.many <- ifelse(
    top.how.many <= nrow(Result$Top.BDA.Modules),
    top.how.many,
    nrow(Result$Top.BDA.Modules)
  )
  which.row <- 1:top.how.many
  selected.variable <- all.var.new[which.row,  1]
  selected.variable <- c(unlist(strsplit(as.character(selected.variable), split="_")))
  selected.variable <- unique(selected.variable)
  selected.variable

  # Download Stock Pool:
  data <- getSymbols(c(
    # Technology
    "QQQ", "GOOGL", "AMZN", "MSFT", "FB", "INTC", 
    "CSCO", "TSM", "ORCL", "NVDA", "SAP", "IBM", 
    "ADBE", "TXN", "AVGO", "ACN", "CRM", "QCOM", 
    "ASML", "BIDU", "MU", "VMW", "ADP", "ATVI", 
    "INTU", "MCHP",
    
    "AIG", "BAC", "WFC", "AXP", "BK",
    
    "AMGN", "AZN", "BMY", "BAX", "CELG",
    "CVS", "JNJ", "LLY", "MDT", "MRK", "UNH", "GILD",
    "BIIB",
    
    "BA", "CAT", "LMT", "DE", "GD", "HON", "UTX"
  ), from = date.cut) # Here we should update date.cut
  data.list <- list(
    # Technology
    "QQQ", "GOOGL", "AMZN", "MSFT", "FB", "INTC", 
    "CSCO", "TSM", "ORCL", "NVDA", "SAP", "IBM", 
    "ADBE", "TXN", "AVGO", "ACN", "CRM", "QCOM", 
    "ASML", "BIDU", "MU", "VMW", "ADP", "ATVI", 
    "INTU", "MCHP",
    
    "AIG", "BAC", "WFC", "AXP", "BK",
    
    "AMGN", "AZN", "BMY", "BAX", "CELG",
    "CVS", "JNJ", "LLY", "MDT", "MRK", "UNH", "GILD",
    "BIIB",
    
    "BA", "CAT", "LMT", "DE", "GD", "HON", "UTX"
  )
  data.data.matrix <- list(
    QQQ, GOOGL, AMZN, MSFT, FB, INTC, CSCO, 
    TSM, ORCL, NVDA, SAP, IBM, ADBE, TXN, 
    AVGO, ACN, CRM, QCOM, ASML, BIDU, MU, 
    VMW, ADP, ATVI, INTU, MCHP,
    
    AIG, BAC, WFC, AXP, BK,
    
    AMGN, AZN, BMY, BAX, CELG,
    CVS, JNJ, LLY, MDT, MRK, UNH, GILD, BIIB,
    
    BA, CAT, LMT, DE, GD, HON, UTX
  )
  
  # Plot
  base.plot.data <- NULL
  for (j in 1:length(selected.variable)) {
    #j = 1
    i = which((c(as.character(unlist(data.list))) == selected.variable[j]) == TRUE)
    ret.i <- (data.data.matrix[[i]][, 4]/lag(data.data.matrix[[i]][, 4]) - 1)[-1, ] + 1
    ret.i[1,] <- ret.i[1,] * base.dollar
    accumulated.ret.i <- cumprod(ret.i)
    base.plot.data <- cbind(base.plot.data, accumulated.ret.i)
  }
  getSymbols("SPY", from = date.cut) # Here we should update date.cut
  SPY.ret <- (SPY[, 4]/lag(SPY[,4]) - 1)[-1, ] + 1
  accumulated.ret.SPY <- cumprod(SPY.ret)*base.dollar
  comp.portfolio.market <- data.frame(cbind(
    Portfolio = rowMeans(base.plot.data), 
    Market = accumulated.ret.SPY
  ))

  # Multiple Plot
  dygraph(comp.portfolio.market) %>% dyRangeSelector() %>%
    dyLegend(show = "onmouseover", width = 700, hideOnMouseOut = FALSE) %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 2))
} # End of function

# Weighted Portfolio Plot
Weighted.Tech.Watch.List.Plot <- function(
  base.dollar = 1000, 
  buy.height = -1.96,
  top.how.many = 15,
  k = 9, # Size of Initial Draw
  rounds = 200, # How many rounds of BDA?
  n.neighbor = 3 # Size of nearest neighbor
) {
  # Download Stock Pool:
  data <- getSymbols(c(
    # Technology
    "QQQ", "GOOGL", "AMZN", "MSFT", "FB", "INTC", 
    "CSCO", "TSM", "ORCL", "NVDA", "SAP", "IBM", 
    "ADBE", "TXN", "AVGO", "ACN", "CRM", "QCOM", 
    "ASML", "BIDU", "MU", "VMW", "ADP", "ATVI", 
    "INTU", "MCHP",
    
    "AIG", "BAC", "WFC", "AXP", "BK",
    
    "AMGN", "AZN", "BMY", "BAX", "CELG",
    "CVS", "JNJ", "LLY", "MDT", "MRK", "UNH", "GILD",
    "BIIB",
    
    "BA", "CAT", "LMT", "DE", "GD", "HON", "UTX"
  ), from = "2005-01-01", to = "2013-06-24")
  data.list <- list(
    # Technology
    "QQQ", "GOOGL", "AMZN", "MSFT", "FB", "INTC", 
    "CSCO", "TSM", "ORCL", "NVDA", "SAP", "IBM", 
    "ADBE", "TXN", "AVGO", "ACN", "CRM", "QCOM", 
    "ASML", "BIDU", "MU", "VMW", "ADP", "ATVI", 
    "INTU", "MCHP",
    
    "AIG", "BAC", "WFC", "AXP", "BK",
    
    "AMGN", "AZN", "BMY", "BAX", "CELG",
    "CVS", "JNJ", "LLY", "MDT", "MRK", "UNH", "GILD",
    "BIIB",
    
    "BA", "CAT", "LMT", "DE", "GD", "HON", "UTX"
  )
  data.data.matrix <- list(
    QQQ, GOOGL, AMZN, MSFT, FB, INTC, CSCO, 
    TSM, ORCL, NVDA, SAP, IBM, ADBE, TXN, 
    AVGO, ACN, CRM, QCOM, ASML, BIDU, MU, 
    VMW, ADP, ATVI, INTU, MCHP,
    
    AIG, BAC, WFC, AXP, BK,
    
    AMGN, AZN, BMY, BAX, CELG,
    CVS, JNJ, LLY, MDT, MRK, UNH, GILD, BIIB,
    
    BA, CAT, LMT, DE, GD, HON, UTX
  )
  
  # Watch List:
  # Create Data
  #buy.height <- -1
  sell.height <- .9
  buy.watch.list <- NULL
  how.many.days.past <- 200
  for (i in 1:length(data.data.matrix)) {
    buy.watch.list <- data.frame(cbind(
      buy.watch.list,
      c(round(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, sell.height, 0.01, how.many.days.past)[,4],2))
    ))
  }
  rownames(buy.watch.list) <- c(as.character(BS.Algo(QQQ, .9, 1, buy.height, 2.9, 1, how.many.days.past)[,1]))
  colnames(buy.watch.list) <- data.list
  
  # Run
  all <- buy.watch.list
  #set.seed(1)
  #k = 15
  #start_time <- Sys.time()
  Result <- continuous.vs(
    all = all,
    cut_off = 1,
    num.initial.set = k,
    how.many.rounds = rounds,
    i.want = 1,
    num.top.rows = rounds,
    seed = 1,
    K.means.K = n.neighbor # round(.5*k)
  ); #end_time <- Sys.time(); end_time - start_time
  
  # Variables selected are from I+BDA function:
  all.var.new <- Result$Top.BDA.Modules
  
  # Select
  top.how.many <- ifelse(
    top.how.many <= nrow(Result$Top.BDA.Modules),
    top.how.many,
    nrow(Result$Top.BDA.Modules)
  )
  which.row <- 1:top.how.many
  selected.variable <- all.var.new[which.row,  1]
  selected.variable <- c(unlist(strsplit(as.character(selected.variable), split="_")))
  selected.variable <- unique(selected.variable)
  selected.variable
  sv.final <- selected.variable
  
  # Find the weight
  # Plot
  base.plot.data <- NULL
  for (j in 1:length(selected.variable)) {
    #j = 1
    i = which((c(as.character(unlist(data.list))) == selected.variable[j]) == TRUE)
    ret.i <- (data.data.matrix[[i]][, 4]/lag(data.data.matrix[[i]][, 4]) - 1)[-1, ] + 1
    ret.i[1,] <- ret.i[1,] * base.dollar
    accumulated.ret.i <- cumprod(ret.i)
    base.plot.data <- cbind(base.plot.data, accumulated.ret.i)
  }
  getSymbols("SPY", from = "2005-01-01", to = "2013-06-24")
  SPY.ret <- (SPY[, 4]/lag(SPY[,4]) - 1)[-1, ] + 1
  accumulated.ret.SPY <- cumprod(SPY.ret)*base.dollar
  comp.portfolio.market <- data.frame(cbind(
    Portfolio = rowMeans(base.plot.data), 
    Market = accumulated.ret.SPY
  ))
  
  # Create data with new Y and old X
  # Y (response) is the average return based on newly selected BDA top modules
  # X (explanatory) is the original processed data for all stocks
  all <- data.frame(cbind(
    comp.portfolio.market$Portfolio[(length(comp.portfolio.market$Portfolio) - 200):length(comp.portfolio.market$Portfolio)],
    buy.watch.list[, -1]
  ))
  colnames(all)[[1]] <- "New.Y"
  
  # Use continuous I-score and find weights:
  # To do this, we need to do BDA again
  #start_time <- Sys.time()
  Result <- continuous.vs(
    all = all,
    cut_off = 1,
    num.initial.set = k,
    how.many.rounds = rounds,
    i.want = 1,
    num.top.rows = rounds,
    seed = 1,
    K.means.K = n.neighbor # round(.5*k)
  ); #end_time <- Sys.time(); end_time - start_time
  
  # Select
  top.how.many <- ifelse(
    top.how.many <= nrow(Result$Top.BDA.Modules),
    top.how.many,
    nrow(Result$Top.BDA.Modules)
  )
  which.row <- 1:1
  all.var.new <- Result$Top.BDA.Modules
  selected.variable <- all.var.new[which.row,  1]
  selected.variable <- c(unlist(strsplit(as.character(selected.variable), split="_")))
  selected.variable <- unique(selected.variable)
  selected.variable
  
  # Extract Variables by BDA Results:
  all.data <- all
  all <- all.data
  all <- data.frame(
    cbind(
      all[,1],
      #all[,c(as.character(selected.variable))] # My Iscore Function
      all[, -1][,c(selected.variable)] # Tian Iscore Function
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
  k = k
  new.var <- cont_var(all[,-1], all[,1], K=round(.5*k)); end_time <- Sys.time()
  end_time - start_time
  
  # Add clusters back to data set:
  all.copy <- data.frame(cbind(all.copy,new.var)); dim(all.copy)
  all <- all.copy; colnames(all); 
  all <- all[, c(1, ncol(all))]; dim(all); all.copy = all
  
  # Create clusters
  i = 1
  while (i < top.how.many) {
    # Selected Variable
    which.row <- i + 1
    all.var.new <- Result$Top.BDA.Modules
    selected.variable <- all.var.new[which.row,  1]
    selected.variable <- c(unlist(strsplit(as.character(selected.variable), split="_")))
    selected.variable <- unique(selected.variable)
    selected.variable
    all <- all.data
    all <- data.frame(
      cbind(
        all[,1],
        #all[,c(as.character(selected.variable))] # My Iscore Function
        all[, -1][,c(selected.variable)] # Tian Iscore Function
      )
    )
    names(all)[[1]] <- "label"
    length(selected.variable); dim(all) # Check!
    
    # Run function
    # Call this variable cont_var
    # output is a variable
    # with same length as the data set
    new.var <- new.var <- cont_var(all[,-1], all[,1], K=round(.5*k))
    
    # Add clusters back to data set:
    all.copy <- data.frame(cbind(all.copy,new.var)); dim(all.copy)
    all <- all.copy; colnames(all); dim(all)
    
    # i add 1
    i <- i + 1
  } # Finished updating clusters
  
  # MODEL FITTING / MACHINE LEARNING: 
  # Tuning:
  # Tune
  # Solve Optimization Problem
  X <- as.matrix(all[,-1], nrow=nrow(all[,-1]))
  Y <- all[,1]
  num.col <- ncol(X)
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
  
  # Estimate of response
  Y.hat <- all[, -1] * weights.sol
  Y.hat <- apply(Y.hat, 1, sum)
  
  # MSE
  MSE <- mean(sum(all$label - Y.hat))^2
  
  # Download Stock Pool:
  data <- getSymbols(c(
    # Technology
    "QQQ", "GOOGL", "AMZN", "MSFT", "FB", "INTC", 
    "CSCO", "TSM", "ORCL", "NVDA", "SAP", "IBM", 
    "ADBE", "TXN", "AVGO", "ACN", "CRM", "QCOM", 
    "ASML", "BIDU", "MU", "VMW", "ADP", "ATVI", 
    "INTU", "MCHP",
    
    "AIG", "BAC", "WFC", "AXP", "BK",
    
    "AMGN", "AZN", "BMY", "BAX", "CELG",
    "CVS", "JNJ", "LLY", "MDT", "MRK", "UNH", "GILD",
    "BIIB",
    
    "BA", "CAT", "LMT", "DE", "GD", "HON", "UTX"
  ), from = "2013-01-01")
  data.list <- list(
    # Technology
    "QQQ", "GOOGL", "AMZN", "MSFT", "FB", "INTC", 
    "CSCO", "TSM", "ORCL", "NVDA", "SAP", "IBM", 
    "ADBE", "TXN", "AVGO", "ACN", "CRM", "QCOM", 
    "ASML", "BIDU", "MU", "VMW", "ADP", "ATVI", 
    "INTU", "MCHP",
    
    "AIG", "BAC", "WFC", "AXP", "BK",
    
    "AMGN", "AZN", "BMY", "BAX", "CELG",
    "CVS", "JNJ", "LLY", "MDT", "MRK", "UNH", "GILD",
    "BIIB",
    
    "BA", "CAT", "LMT", "DE", "GD", "HON", "UTX"
  )
  data.data.matrix <- list(
    QQQ, GOOGL, AMZN, MSFT, FB, INTC, CSCO, 
    TSM, ORCL, NVDA, SAP, IBM, ADBE, TXN, 
    AVGO, ACN, CRM, QCOM, ASML, BIDU, MU, 
    VMW, ADP, ATVI, INTU, MCHP,
    
    AIG, BAC, WFC, AXP, BK, DFS, GS,
    
    AMGN, AZN, BMY, BAX, CELG,
    CVS, JNJ, LLY, MDT, MRK, UNH, GILD, BIIB,
    
    BA, CAT, LMT, DE, GD, HON, UTX
  )
  
  # Plot
  base.plot.data <- NULL
  updated.w <- NULL
  for (i.i in 1:nrow(all.var.new[1:top.how.many, ])) {
    selected.variable <- all.var.new[i.i, 1]
    selected.variable <- c(unlist(strsplit(as.character(selected.variable), split="_")))
    selected.variable <- unique(selected.variable)
    selected.variable
    for (j in 1:length(selected.variable)) {
      #j = 1
      i = which((c(as.character(unlist(data.list))) == selected.variable[j]) == TRUE)
      ret.i <- (data.data.matrix[[i]][, 4]/lag(data.data.matrix[[i]][, 4]) - 1)[-1, ] + 1
      ret.i[1,] <- ret.i[1,] * base.dollar
      accumulated.ret.i <- cumprod(ret.i)
      base.plot.data <- cbind(base.plot.data, accumulated.ret.i)
      updated.w <- c(updated.w, weights.sol[[i.i]])
    }
  }
  getSymbols("SPY", from = "2013-01-01")
  SPY.ret <- (SPY[, 4]/lag(SPY[,4]) - 1)[-1, ] + 1
  accumulated.ret.SPY <- cumprod(SPY.ret)*base.dollar
  #Portfolio <- HoltWinters(
  #  rowSums(base.plot.data*c(t(matrix(c(weights.sol,weights.sol), ncol = 2))))/2,
    #cbind(ncol(base.plot.data)*rowMeans(base.plot.data*c(t(matrix(c(weights.sol,weights.sol), ncol = 2))))), 
  #  beta=FALSE, gamma=FALSE)
  
  
  
  Portfolio.I <- cbind(rowSums(base.plot.data * updated.w))
  #Portfolio.I <- cbind(rowMeans(rowSums(base.plot.data * updated.w)))
  #Portfolio.I <- cbind(rowMeans(rowSums(base.plot.data*c(t(matrix(c(weights.sol,weights.sol), ncol = 2))))/2))
  #Portfolio.I <- Portfolio.I - (Portfolio.I[1] - 1000)
  #Portfolio.I = c(base.dollar, Portfolio.I)
  length(Portfolio.I)
  length(accumulated.ret.SPY)
  comp.portfolio.market <- NULL
  comp.portfolio.market <- data.frame(cbind(
    Portfolio = Portfolio.I/2,
    #Portfolio.I = rowMeans(base.plot.data),
    #Portfolio.I = rowSums(base.plot.data*c(t(matrix(c(weights.sol,weights.sol), ncol = 2))))/2,
    #Portfolio.I = cbind(ncol(base.plot.data)*rowMeans(base.plot.data*c(t(matrix(c(weights.sol,weights.sol), ncol = 2))))),
    #Market = accumulated.ret.SPY[-c(1:(length(accumulated.ret.SPY)-length(Portfolio.I)))]
    Market = accumulated.ret.SPY
  ))
  comp.portfolio.market[1,] <- c(base.dollar, base.dollar)
  #head(comp.portfolio.market)
  
  # Multiple Plot
  dygraph(comp.portfolio.market) %>% dyRangeSelector() %>%
    dyLegend(show = "onmouseover", width = 700, hideOnMouseOut = FALSE) %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 2))
} # End of function

# I-SCORE Watch List
library('quantmod')
library('DT')
Tech.Watch.List <- function(
  base.dollar = 1000, 
  buy.height = -1.96,
  top.how.many = 3,
  k = 7, # Size of Initial Draw
  rounds = 10, # How many rounds of BDA?
  n.neighbor = 3 # Size of nearest neighbor
) {
  # Download Stock Pool:
  data <- getSymbols(c(
    # Technology
    "QQQ", "GOOGL", "AMZN", "MSFT", "FB", "INTC", 
    "CSCO", "TSM", "ORCL", "NVDA", "SAP", "IBM", 
    "ADBE", "TXN", "AVGO", "ACN", "CRM", "QCOM", 
    "ASML", "BIDU", "MU", "VMW", "ADP", "ATVI", 
    "INTU", "MCHP"
  ), from = "2010-01-01")
  data.list <- list(
    # Technology
    "QQQ", "GOOGL", "AMZN", "MSFT", "FB", "INTC", 
    "CSCO", "TSM", "ORCL", "NVDA", "SAP", "IBM", 
    "ADBE", "TXN", "AVGO", "ACN", "CRM", "QCOM", 
    "ASML", "BIDU", "MU", "VMW", "ADP", "ATVI", 
    "INTU", "MCHP"
  )
  data.data.matrix <- list(
    QQQ, GOOGL, AMZN, MSFT, FB, INTC, CSCO, 
    TSM, ORCL, NVDA, SAP, IBM, ADBE, TXN, 
    AVGO, ACN, CRM, QCOM, ASML, BIDU, MU, 
    VMW, ADP, ATVI, INTU, MCHP
  )
  
  # Watch List:
  # Create Data
  #buy.height <- -1
  sell.height <- .9
  buy.watch.list <- NULL
  how.many.days.past <- 500
  for (i in 1:length(data.data.matrix)) {
    buy.watch.list <- data.frame(cbind(
      buy.watch.list,
      c(round(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, sell.height, 0.01, how.many.days.past)[,4],2))
    ))
  }
  rownames(buy.watch.list) <- c(as.character(BS.Algo(QQQ, .9, 1, buy.height, 2.9, 1, how.many.days.past)[,1]))
  colnames(buy.watch.list) <- data.list
  
  # Run
  all <- buy.watch.list
  #set.seed(1)
  #k = 15
  #start_time <- Sys.time()
  Result <- continuous.vs(
    all = all,
    cut_off = 1,
    num.initial.set = k,
    how.many.rounds = rounds,
    i.want = 1,
    num.top.rows = rounds,
    seed = 1,
    K.means.K = n.neighbor # round(.5*k)
  ); #end_time <- Sys.time(); end_time - start_time
  
  # Variables selected are from I+BDA function:
  Result$Top.BDA.Modules <- data.frame(cbind(
    c(1:nrow(Result$Top.BDA.Modules)),
    Result$Top.BDA.Modules)
  )
  colnames(Result$Top.BDA.Modules) <- c("No.", "Top Modules", "Influence Measure")
  top.how.many <- ifelse(
    top.how.many <= nrow(Result$Top.BDA.Modules),
    top.how.many,
    nrow(Result$Top.BDA.Modules)
  )
  
  which.row <- 1:1
  all.var.new <- Result$Top.BDA.Modules
  selected.variable <- all.var.new[which.row,  2]
  selected.variable <- c(unlist(strsplit(as.character(selected.variable), split="_")))
  selected.variable <- unique(selected.variable)
  selected.variable
  
  # Extract Variables by BDA Results:
  all.data <- all
  all <- all.data
  all <- data.frame(
    cbind(
      all[,1],
      #all[,c(as.character(selected.variable))] # My Iscore Function
      all[, -1][,c(selected.variable)] # Tian Iscore Function
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
  k = k
  new.var <- cont_var(all[,-1], all[,1], K=round(.5*k), selected.variable); end_time <- Sys.time()
  end_time - start_time
  
  # Add clusters back to data set:
  all.copy <- data.frame(cbind(all.copy,new.var)); dim(all.copy)
  all <- all.copy; colnames(all); 
  all <- all[, c(1, ncol(all))]; dim(all); all.copy = all
  
  # Create clusters
  i = 1
  while (i < top.how.many) {
    # Selected Variable
    which.row <- i + 1
    all.var.new <- Result$Top.BDA.Modules
    selected.variable <- all.var.new[which.row,  2]
    selected.variable <- c(unlist(strsplit(as.character(selected.variable), split="_")))
    selected.variable <- unique(selected.variable)
    selected.variable
    all <- all.data
    all <- data.frame(
      cbind(
        all[,1],
        #all[,c(as.character(selected.variable))] # My Iscore Function
        all[, -1][,c(selected.variable)] # Tian Iscore Function
      )
    )
    names(all)[[1]] <- "label"
    length(selected.variable); dim(all) # Check!
    
    # Run function
    # Call this variable cont_var
    # output is a variable
    # with same length as the data set
    new.var <- new.var <- cont_var(all[,-1], all[,1], K=round(.5*k), selected.variable)
    
    # Add clusters back to data set:
    all.copy <- data.frame(cbind(all.copy,new.var)); dim(all.copy)
    all <- all.copy; colnames(all); dim(all)
    
    # i add 1
    i <- i + 1
  } # Finished updating clusters
  
  # MODEL FITTING / MACHINE LEARNING: 
  # Tuning:
  # Tune
  # Solve Optimization Problem
  X <- as.matrix(all[,-1], nrow=nrow(all[,-1]))
  Y <- all[,1]
  num.col <- ncol(X)
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
  
  # Output
  Var.Result <- data.frame(cbind(Result$Top.BDA.Modules[1:(top.how.many), ], Weights = weights.sol))
  
  # Output: Portfolio in Detail
  Detail.Portfolio <- NULL
  for (which.row in 1:nrow(Var.Result)) {
    selected.variable <- Var.Result[which.row,2]
    selected.variable <- c(unlist(strsplit(as.character(selected.variable), split="_")))
    selected.variable <- unique(selected.variable)
    selected.stock <- matrix(c(selected.variable, 
                               rep(
                                 round(Var.Result[which.row,4]/length(selected.variable), 2), 
                                 length(selected.variable))),
                             nrow = length(selected.variable))
    Detail.Portfolio <- rbind(Detail.Portfolio, selected.stock)
  }; colnames(Detail.Portfolio) <- c("Holdings", "Weight")
  
  # Sort the Detail.Portfolio by Stocks
  Detail.Portfolio <- data.frame(cbind(
    Stocks = as.character(as.factor(plyr::count(Detail.Portfolio)[,1])),
    Weights = as.numeric(as.character(plyr::count(Detail.Portfolio)[,2]))*plyr::count(Detail.Portfolio)[,3]
  ))
  
  # Aggregate the same stocks
  Detail.Portfolio
  Pi <- plyr::count(Detail.Portfolio, "Stocks")
  Final.Portfolio <- NULL
  for (i in 1:nrow(Pi)) {
    Final.Portfolio <- rbind(Final.Portfolio, 
                             c(as.character(Pi[i, 1]), 
                               sum(as.numeric(as.character(Detail.Portfolio[1:Pi[i, 2], 2])
                               ))
                             )
    )
    i <- i + 1
  } # End of loop
  Final.Portfolio[, 2] <- round(
    as.numeric(as.character(Final.Portfolio[, 2]))/sum(as.numeric(as.character(Final.Portfolio[, 2]))),
    2
  ) # End of round
  colnames(Final.Portfolio) <- c("Stocks", "Weights")
  
  # Present table:
  return(list(
    Top.Modules.Result = data.frame(cbind(Result$Top.BDA.Modules[1:(top.how.many), ], Weights = weights.sol))[, -1],
    Detail.Portfolio.Weight = Final.Portfolio
  ))
} # End of function

# Simulation
Tech.Watch.List.Plot <- function(
  base.dollar = 1000, 
  buy.height = -1.96,
  top.how.many = 3,
  date.cut = "2016-01-26",
  k = 7, # Size of Initial Draw
  rounds = 10, # How many rounds of BDA?
  n.neighbor = 3 # Size of nearest neighbor
) {
  # Download Stock Pool:
  data <- getSymbols(c(
    # Technology
    "QQQ", "GOOGL", "AMZN", "MSFT", "FB", "INTC", 
    "CSCO", "TSM", "ORCL", "NVDA", "SAP", "IBM", 
    "ADBE", "TXN", "AVGO", "ACN", "CRM", "QCOM", 
    "ASML", "BIDU", "MU", "VMW", "ADP", "ATVI", 
    "INTU", "MCHP"
  ), from = "2005-01-01", to = date.cut) # Here we should update date.cut
  data.list <- list(
    # Technology
    "QQQ", "GOOGL", "AMZN", "MSFT", "FB", "INTC", 
    "CSCO", "TSM", "ORCL", "NVDA", "SAP", "IBM", 
    "ADBE", "TXN", "AVGO", "ACN", "CRM", "QCOM", 
    "ASML", "BIDU", "MU", "VMW", "ADP", "ATVI", 
    "INTU", "MCHP"
  )
  data.data.matrix <- list(
    QQQ, GOOGL, AMZN, MSFT, FB, INTC, CSCO, 
    TSM, ORCL, NVDA, SAP, IBM, ADBE, TXN, 
    AVGO, ACN, CRM, QCOM, ASML, BIDU, MU, 
    VMW, ADP, ATVI, INTU, MCHP
  )
  
  # Watch List:
  # Create Data
  #buy.height <- -1
  sell.height <- .5
  buy.watch.list <- NULL
  how.many.days.past <- 200
  for (i in 1:length(data.data.matrix)) {
    buy.watch.list <- data.frame(cbind(
      buy.watch.list,
      c(round(BS.Algo(data.data.matrix[[i]], .01, 1, buy.height, sell.height, 0.01, how.many.days.past)[,4],2))
    ))
  }
  rownames(buy.watch.list) <- c(as.character(BS.Algo(QQQ, .9, 1, buy.height, 2.9, 1, how.many.days.past)[,1]))
  colnames(buy.watch.list) <- data.list
  
  # Run
  all <- buy.watch.list
  #set.seed(1)
  #k = 15
  #start_time <- Sys.time()
  Result <- continuous.vs(
    all = all,
    cut_off = 1,
    num.initial.set = k,
    how.many.rounds = rounds,
    i.want = 1,
    num.top.rows = rounds,
    seed = 1,
    K.means.K = n.neighbor # round(.5*k)
  ); #end_time <- Sys.time(); end_time - start_time
  
  # Variables selected are from I+BDA function:
  all.var.new <- Result$Top.BDA.Modules
  
  # Select
  top.how.many <- ifelse(
    top.how.many <= nrow(Result$Top.BDA.Modules),
    top.how.many,
    nrow(Result$Top.BDA.Modules)
  )
  which.row <- 1:top.how.many
  selected.variable <- all.var.new[which.row,  1]
  selected.variable <- c(unlist(strsplit(as.character(selected.variable), split="_")))
  selected.variable <- unique(selected.variable)
  selected.variable
  
  # Download Stock Pool:
  data <- getSymbols(c(
    # Technology
    "QQQ", "GOOGL", "AMZN", "MSFT", "FB", "INTC", 
    "CSCO", "TSM", "ORCL", "NVDA", "SAP", "IBM", 
    "ADBE", "TXN", "AVGO", "ACN", "CRM", "QCOM", 
    "ASML", "BIDU", "MU", "VMW", "ADP", "ATVI", 
    "INTU", "MCHP"
  ), from = date.cut) # Here we should update date.cut
  data.list <- list(
    # Technology
    "QQQ", "GOOGL", "AMZN", "MSFT", "FB", "INTC", 
    "CSCO", "TSM", "ORCL", "NVDA", "SAP", "IBM", 
    "ADBE", "TXN", "AVGO", "ACN", "CRM", "QCOM", 
    "ASML", "BIDU", "MU", "VMW", "ADP", "ATVI", 
    "INTU", "MCHP"
  )
  data.data.matrix <- list(
    QQQ, GOOGL, AMZN, MSFT, FB, INTC, CSCO, 
    TSM, ORCL, NVDA, SAP, IBM, ADBE, TXN, 
    AVGO, ACN, CRM, QCOM, ASML, BIDU, MU, 
    VMW, ADP, ATVI, INTU, MCHP
  )
  
  # Plot
  base.plot.data <- NULL
  for (j in 1:length(selected.variable)) {
    #j = 1
    i = which((c(as.character(unlist(data.list))) == selected.variable[j]) == TRUE)
    ret.i <- (data.data.matrix[[i]][, 4]/lag(data.data.matrix[[i]][, 4]) - 1)[-1, ] + 1
    ret.i[1,] <- ret.i[1,] * base.dollar
    accumulated.ret.i <- cumprod(ret.i)
    base.plot.data <- cbind(base.plot.data, accumulated.ret.i)
  }
  getSymbols("SPY", from = date.cut) # Here we should update date.cut
  SPY.ret <- (SPY[, 4]/lag(SPY[,4]) - 1)[-1, ] + 1
  accumulated.ret.SPY <- cumprod(SPY.ret)*base.dollar
  comp.portfolio.market <- data.frame(cbind(
    Portfolio = rowMeans(base.plot.data), 
    Market = accumulated.ret.SPY
  ))
  
  # Multiple Plot
  dygraph(comp.portfolio.market) %>% dyRangeSelector() %>%
    dyLegend(show = "onmouseover", width = 700, hideOnMouseOut = FALSE) %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 2))
} # End of function

####################### SEARCHING FOR WEIGHTS: DEF FCT ######################

# DEFINE FINDING WEIGHTS FUNCTION FIRST;
# THEN WE ARE GOING TO CODE TRAINING/PREDICT FUNCTION

# Load package
library("quadprog")
library("pROC")
library("matrixcalc", lib = "C:/Users/eagle/OneDrive/Documents/R/win-library/3.4")

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
    #Accuracy = Result$Accuracy
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
    #Y.and.Y.hat.Table.Binary = compare_Y_Y_hat#,
    #Prediction.Truth.Table = prediction.table,
    #Prediction.Accuracy = test.accuracy
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


########################### ISCORE TIME-SERIES PREDICTION ###########################

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
    #Graph = dygraph(plot.data),
    Buy.Prediction = paste0(
      "Next trading day the price is ", Buy.Prediction, ". ",
      "Prediction less than Actual Price: overbought; ",
      "Prediction higher than Actual Price: oversold.")
  ))
} # End of function

# Try
#getSymbols("AAPL")
#iscore.buy.prediction(x = AAPL, k.mean = 10, num.initial.set = 5, how.many.folds = 5, how.many.modules = 3)

# Define function:
iscore.buy.prediction.plot <- function(
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
    SMA(all, 2),
    SMA(all, 3),
    SMA(all, 4),
    SMA(all, 5),
    SMA(all, 6),
    SMA(all, 7),
    SMA(all, 8),
    SMA(all, 9),
    SMA(all, 10),
    SMA(all, 11),
    SMA(all, 12)
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
  
  return(Graph = dygraph(plot.data))
} # End of function

# Try
#getSymbols("BIIB")
#iscore.buy.prediction(x = BIIB, k.mean = 10, num.initial.set = 5, how.many.folds = 5, how.many.modules = 3)
#iscore.buy.prediction.plot(x = FB, k.mean = 20, num.initial.set = 5, how.many.folds = 7, how.many.modules = 3)


########################### TIME SERIES MODEL ###########################

# ARIMA Forecast:
ARMA_Fit_D<-function(entry,ahead){
  #entry <- SPY[,4]; ahead=10
  entry <- entry[,4]
  fit<-auto.arima(
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
  print(forecast, right = FALSE)
  #rownames(forecast) <- rownames('SPY')
  #plot(forecast,main="Forecast Daily Closing Price")
} # End function

# ARIMA Forecast + K-fold CV
ARMA_Fit_D_KFOLD_CV <- function(
  entry,
  ahead = 10,
  cutoff = 0.9,
  how.many.fold = 6) {
  
  # Define
  #entry = AAPL
  entry <- entry[,4]
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
  sum.MSE <- data.frame(sum.MSE)
  rownames(sum.MSE) <- c(1:(how.many.fold-1))
  colnames(sum.MSE) <- "Fold"
  
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
    #Best.Model = best.fit,
    Test.MSE = mean(apply(plot.data, 1, mean)),
    #Graph = dygraph(plot.data),
    Forecast = forecast
  ))
} # End of function

# ARIMA Forecast + K-fold CV
ARMA_Fit_D_KFOLD_CV_Plot_Only <- function(
  entry,
  ahead = 10,
  cutoff = 0.9,
  how.many.fold = 6) {
  
  # Define
  #entry = AAPL
  entry <- entry[,4]
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
  return(#list(
    #Summary.of.MSE = sum.MSE,
    #Best.Model = best.fit,
    #Test.MSE = mean(apply(plot.data, 1, mean)),
    Graph = dygraph(plot.data)#,
    #Forecast = forecast
  )#)
} # End of function

# Forecast with time series
ARMA_Fit_W<-function(entry,ahead){
  #entry <- SPY[,4]; ahead=10
  entry <-to.weekly(entry)[,4]
  fit<-auto.arima(
    entry,
    stationary=FALSE,
    max.p = 5, max.q = 5, max.P = 2,
    max.Q = 2, max.order = 5, max.d = 2, max.D = 1, start.p = 2,
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
  colnames(forecast) <- c("Week + ?", "Point.Forecast", "Lo.80", "Hi.80", "Lo.95", "Hi.95")
  print(forecast, right = FALSE)
  #rownames(forecast) <- rownames('SPY')
  #plot(forecast,main="Forecast Daily Closing Price")
} # End function

# Forecast with time series
ARMA_Fit_M<-function(entry,ahead){
  #entry <- SPY[,4]; ahead=10
  entry <-to.monthly(entry)[,4]
  fit<-auto.arima(
    entry,
    stationary=FALSE,
    max.p = 5, max.q = 5, max.P = 2,
    max.Q = 2, max.order = 5, max.d = 2, max.D = 1, start.p = 2,
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
  colnames(forecast) <- c("Month + ?", "Point.Forecast", "Lo.80", "Hi.80", "Lo.95", "Hi.95")
  print(forecast, right = FALSE)
  #rownames(forecast) <- rownames('SPY')
  #plot(forecast,main="Forecast Daily Closing Price")
} # End function

# Forecast with time series
ARMA_Fit_Q<-function(entry,ahead){
  #entry <- SPY[,4]; ahead=10
  entry <-to.quarterly(entry)[,4]
  fit<-auto.arima(
    entry,
    stationary=FALSE,
    max.p = 5, max.q = 5, max.P = 2,
    max.Q = 2, max.order = 5, max.d = 2, max.D = 1, start.p = 2,
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
  colnames(forecast) <- c("Quarter + ?", "Point.Forecast", "Lo.80", "Hi.80", "Lo.95", "Hi.95")
  print(forecast, right = FALSE)
  #rownames(forecast) <- rownames('SPY')
  #plot(forecast,main="Forecast Daily Closing Price")
} # End function

# Forecast with time series
ARMA_Fit_A<-function(entry,ahead){
  #entry <- SPY[,4]; ahead=10
  entry <-to.yearly(entry)[,4]
  fit<-auto.arima(
    entry,
    stationary=FALSE,
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
  colnames(forecast) <- c("Year + ?", "Point.Forecast", "Lo.80", "Hi.80", "Lo.95", "Hi.95")
  print(forecast, right = FALSE)
  #rownames(forecast) <- rownames('SPY')
  #plot(forecast,main="Forecast Daily Closing Price")
} # End function

# Quant Result:
Quant.Result <- function(x,past.n.days) {
  Close<-x[,4] # Define Close as adjusted closing price
  Return<-(Close-lag(Close))/Close
  DF<-data.frame(cbind(
    Close,
    Return
  ))
  return(
    DF[(nrow(DF)-past.n.days):nrow(DF),] 
  )
}

########################### RNN FORECAST ###########################

# Define an RNN model, Daily
RNN_Daily <- function(entry) {
  # Data
  getSymbols(c("SPY", "IVV"))
  x <- IVV[, 4][(nrow(IVV) - 800 + 1):nrow(IVV), ]
  y <- entry[, 4][(nrow(entry) - 800 + 1):nrow(entry), ]
  
  # Samples of 20 time series
  X <- matrix(x, nrow = 20)
  Y <- matrix(y, nrow = 20)
  
  # Plot noisy waves
  #plot(as.vector(X), col='blue', type='l', ylab = "X,Y", main = "Noisy waves")
  #lines(as.vector(Y), col = "red")
  #legend("topleft", c("X", "Y"), col = c("blue","red"), lty = c(1,1), lwd = c(1,1))
  
  # Standardize in the interval 0 - 1
  X <- (X - min(X)) / (max(X) - min(X))
  Y <- (Y - min(Y)) / (max(Y) - min(Y))
  
  # Transpose
  X <- t(X)
  Y <- t(Y)
  
  # Training-testing sets
  cutoff = .7
  train <- 1:(nrow(X)*cutoff)
  test <- (nrow(X)*cutoff + 1):nrow(X)
  
  # Train model. Keep out the last two sequences.
  model <- trainr(Y = Y[train,],
                  X = X[train,],
                  learningrate = 0.05,
                  hidden_dim = 16,
                  numepochs = 100)
  
  # Predicted values
  Yp <- predictr(model, X)
  
  # Plot predicted vs actual. Training set + testing set
  #plot(as.vector(t(Y)), col = 'red', type = 'l', main = "Actual vs predicted", ylab = "Y,Yp")
  #lines(as.vector(t(Yp)), type = 'l', col = 'blue')
  #legend("topleft", c("Predicted", "Real"), col = c("blue","red"), lty = c(1,1), lwd = c(1,1))
  
  # Plot predicted vs actual. Testing set only.
  #plot(as.vector(t(Y[test,])), col = 'red', type='l', main = "Actual vs predicted: testing set", ylab = "Y,Yp")
  #lines(as.vector(t(Yp[test,])), type = 'l', col = 'blue')
  #legend("topleft", c("Predicted", "Real"), col = c("blue","red"), lty = c(1,1), lwd = c(1,1))
  
  # Test MSE
  Test.MSE <- (sum(t(Y[test,]) - t(Yp[test,])))^2
  
  # Actual prediction
  Yp_actual <- predictr(model, X[(ncol(X) - 10):ncol(X), ])
  Yp_actual_vector <- as.vector(t(Yp_actual))
  result <- data.frame(
    Next_D_Change = Yp_actual_vector,
    CI_LB = Yp_actual_vector - 2*sd(Yp_actual_vector),
    CI_UB = Yp_actual_vector + 2*sd(Yp_actual_vector)
  )

  # Output
  return(tail(result))
}# End of function

# Define an RNN model, Weekly
RNN_Weekly <- function(entry) {
  # Weekly
  entry <- to.weekly(entry)
  
  # Data
  getSymbols(c("SPY", "IVV"))
  x <- IVV[, 4][(nrow(IVV) - 400 + 1):nrow(IVV), ]
  y <- entry[, 4][(nrow(entry) - 400 + 1):nrow(entry), ]
  
  # Samples of 20 time series
  X <- matrix(x, nrow = 20)
  Y <- matrix(y, nrow = 20)
  
  # Plot noisy waves
  #plot(as.vector(X), col='blue', type='l', ylab = "X,Y", main = "Noisy waves")
  #lines(as.vector(Y), col = "red")
  #legend("topleft", c("X", "Y"), col = c("blue","red"), lty = c(1,1), lwd = c(1,1))
  
  # Standardize in the interval 0 - 1
  X <- (X - min(X)) / (max(X) - min(X))
  Y <- (Y - min(Y)) / (max(Y) - min(Y))
  
  # Transpose
  X <- t(X)
  Y <- t(Y)
  
  # Training-testing sets
  cutoff = .7
  train <- 1:(nrow(X)*cutoff)
  test <- (nrow(X)*cutoff + 1):nrow(X)
  
  # Train model. Keep out the last two sequences.
  model <- trainr(Y = Y[train,],
                  X = X[train,],
                  learningrate = 0.05,
                  hidden_dim = 16,
                  numepochs = 100)
  
  # Predicted values
  Yp <- predictr(model, X)
  
  # Plot predicted vs actual. Training set + testing set
  #plot(as.vector(t(Y)), col = 'red', type = 'l', main = "Actual vs predicted", ylab = "Y,Yp")
  #lines(as.vector(t(Yp)), type = 'l', col = 'blue')
  #legend("topleft", c("Predicted", "Real"), col = c("blue","red"), lty = c(1,1), lwd = c(1,1))
  
  # Plot predicted vs actual. Testing set only.
  #plot(as.vector(t(Y[test,])), col = 'red', type='l', main = "Actual vs predicted: testing set", ylab = "Y,Yp")
  #lines(as.vector(t(Yp[test,])), type = 'l', col = 'blue')
  #legend("topleft", c("Predicted", "Real"), col = c("blue","red"), lty = c(1,1), lwd = c(1,1))
  
  # Test MSE
  Test.MSE <- (sum(t(Y[test,]) - t(Yp[test,])))^2
  
  # Actual prediction
  Yp_actual <- predictr(model, X[(ncol(X) - 10):ncol(X), ])
  Yp_actual_vector <- as.vector(t(Yp_actual))
  result <- data.frame(
    Next_W_Change = Yp_actual_vector,
    CI_LB = Yp_actual_vector - 2*sd(Yp_actual_vector),
    CI_UB = Yp_actual_vector + 2*sd(Yp_actual_vector)
  )
  
  # Output
  return(tail(result))
}# End of function

# Define an RNN model, Monthly
RNN_Monthly <- function(entry) {
  # Weekly
  entry <- to.monthly(entry)
  
  # Data
  getSymbols(c("SPY", "IVV"))
  x <- IVV[, 4][(nrow(IVV) - 40 + 1):nrow(IVV), ]
  y <- entry[, 4][(nrow(entry) - 40 + 1):nrow(entry), ]
  
  # Samples of 20 time series
  X <- matrix(x, nrow = 5)
  Y <- matrix(y, nrow = 5)
  
  # Plot noisy waves
  #plot(as.vector(X), col='blue', type='l', ylab = "X,Y", main = "Noisy waves")
  #lines(as.vector(Y), col = "red")
  #legend("topleft", c("X", "Y"), col = c("blue","red"), lty = c(1,1), lwd = c(1,1))
  
  # Standardize in the interval 0 - 1
  X <- (X - min(X)) / (max(X) - min(X))
  Y <- (Y - min(Y)) / (max(Y) - min(Y))
  
  # Transpose
  X <- t(X)
  Y <- t(Y)
  
  # Training-testing sets
  cutoff = .7
  train <- 1:(nrow(X)*cutoff)
  test <- (nrow(X)*cutoff + 1):nrow(X)
  
  # Train model. Keep out the last two sequences.
  model <- trainr(Y = Y[train,],
                  X = X[train,],
                  learningrate = 0.05,
                  hidden_dim = 16,
                  numepochs = 100)
  
  # Predicted values
  Yp <- predictr(model, X)
  
  # Plot predicted vs actual. Training set + testing set
  #plot(as.vector(t(Y)), col = 'red', type = 'l', main = "Actual vs predicted", ylab = "Y,Yp")
  #lines(as.vector(t(Yp)), type = 'l', col = 'blue')
  #legend("topleft", c("Predicted", "Real"), col = c("blue","red"), lty = c(1,1), lwd = c(1,1))
  
  # Plot predicted vs actual. Testing set only.
  #plot(as.vector(t(Y[test,])), col = 'red', type='l', main = "Actual vs predicted: testing set", ylab = "Y,Yp")
  #lines(as.vector(t(Yp[test,])), type = 'l', col = 'blue')
  #legend("topleft", c("Predicted", "Real"), col = c("blue","red"), lty = c(1,1), lwd = c(1,1))
  
  # Test MSE
  Test.MSE <- (sum(t(Y[test,]) - t(Yp[test,])))^2
  
  # Actual prediction
  Yp_actual <- predictr(model, X[(ncol(X) - 5):ncol(X), ])
  Yp_actual_vector <- as.vector(t(Yp_actual))
  result <- data.frame(
    Next_M_Change = Yp_actual_vector,
    CI_LB = Yp_actual_vector - 2*sd(Yp_actual_vector),
    CI_UB = Yp_actual_vector + 2*sd(Yp_actual_vector)
  )
  
  # Output
  return(tail(result))
}# End of function

# Overall RNN
RNN_All <- function(entry) {
  output <- data.frame(data.frame(cbind(
    RNN_Daily(entry), RNN_Weekly(entry), RNN_Monthly(entry)
  )))
  output <- output[nrow(output), ]
  colnames(output) <- c("Next Day", "Next D LB", "Next D UB",
                        "Next Week", "Next W LB", "Next W UB",
                        "Next Month", "Next M LB", "Next M UB")
  return(output)
}

# RNN Predict Buy Signal
RNN_Buy_Signal <- function(x,r_day_plot,end_day_plot,c,height,past.n.days = 1000){
  # x <- AAPL
  getSymbols(c('SPY','QQQ','DIA','IWM','GLD'))
  M <- data.frame(cbind(
    Buy.table(x,r_day_plot,end_day_plot,c,height,past.n.days)[,1],
    Buy.table(SPY,r_day_plot,end_day_plot,c,height,past.n.days)[,1],
    Buy.table(QQQ,r_day_plot,end_day_plot,c,height,past.n.days)[,1],
    Buy.table(DIA,r_day_plot,end_day_plot,c,height,past.n.days)[,1],
    Buy.table(IWM,r_day_plot,end_day_plot,c,height,past.n.days)[,1],
    Buy.table(GLD,r_day_plot,end_day_plot,c,height,past.n.days)[,1] )
  )
  #M.update <- cor(M)
  #colnames(M.update) = c("Corr.","SPY","QQQ","DIA","IWM","GLD")
  #rownames(M.update) = c("Corr.","SPY","QQQ","DIA","IWM","GLD")
  
  # RNN
  x <- M[(nrow(M) - 400 + 1):nrow(M), 2]
  y <- M[(nrow(M) - 400 + 1):nrow(M), 1]
  
  # Samples of 20 time series
  X <- matrix(x, nrow = 20)
  Y <- matrix(y, nrow = 20)
  
  # Plot noisy waves
  #plot(as.vector(X), col='blue', type='l', ylab = "X,Y", main = "Noisy waves")
  #lines(as.vector(Y), col = "red")
  #legend("topleft", c("X", "Y"), col = c("blue","red"), lty = c(1,1), lwd = c(1,1))
  
  # Standardize in the interval 0 - 1
  X <- (X - min(X)) / (max(X) - min(X))
  Y <- (Y - min(Y)) / (max(Y) - min(Y))
  
  # Transpose
  X <- t(X)
  Y <- t(Y)
  
  # Training-testing sets
  cutoff = .7
  train <- 1:(nrow(X)*cutoff)
  test <- (nrow(X)*cutoff + 1):nrow(X)
  
  # Train model. Keep out the last two sequences.
  model <- trainr(Y = Y[train,],
                  X = X[train,],
                  learningrate = 0.05,
                  hidden_dim = 16,
                  numepochs = 100)
  
  # Predicted values
  Yp <- predictr(model, X)
  
  # Plot predicted vs actual. Training set + testing set
  #plot(as.vector(t(Y)), col = 'red', type = 'l', main = "Actual vs predicted", ylab = "Y,Yp")
  #lines(as.vector(t(Yp)), type = 'l', col = 'blue')
  #legend("topleft", c("Predicted", "Real"), col = c("blue","red"), lty = c(1,1), lwd = c(1,1))
  
  # Plot predicted vs actual. Testing set only.
  #plot(as.vector(t(Y[test,])), col = 'red', type='l', main = "Actual vs predicted: testing set", ylab = "Y,Yp")
  #lines(as.vector(t(Yp[test,])), type = 'l', col = 'blue')
  #legend("topleft", c("Predicted", "Real"), col = c("blue","red"), lty = c(1,1), lwd = c(1,1))
  
  # Test MSE
  Test.MSE <- (sum(t(Y[test,]) - t(Yp[test,])))^2
  
  # Actual prediction
  Yp_actual <- predictr(model, X[(ncol(X) - 10):ncol(X), ])
  Yp_actual_vector <- as.vector(t(Yp_actual))
  result <- data.frame(
    Next_D_Change = Yp_actual_vector,
    CI_LB = Yp_actual_vector - 2*sd(Yp_actual_vector),
    CI_UB = Yp_actual_vector + 2*sd(Yp_actual_vector)
  )
  
  # Output
  return(tail(result))
  #corrplot(M.update, method = "number", type = "upper")
} # End of function:

# RNN_Buy_Signal(AAPL,.8, 1,-1.9,1.2)

###################################### FUNDAMENTALS ###############################
# Go to SEC.GOV
# Search for 10-K
# A [linked phrase][id].
SEC.gov <- function(x) {
  link.table <- rbind(
    "Copy link below to a web browser: ",
    paste0("https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=", x, "&type=10-K&dateb=&owner=exclude&count=40"))
  colnames(link.table) <- "SEC Filings"
  print(link.table)
}

# Get Financials
Oper.Income.Tot.Rev <- function(x) {
  #x <- 'AAPL'
  getFinancials(x,period="A")
  tickers <-  new.env()
  s <- c(x)
  lapply(s, getFinancials,env=tickers)
  result <- data.frame(sapply(
    ls(envir=tickers),
    function(x) {x <- get(x) ## get the varible name
    x$IS$A["Operating Income", ] / x$IS$A["Total Revenue",]}))
  colnames(result) = 'Op. Income / Total Rev.'
  return(result)
}

Income.After.Tax.Tot.Rev <- function(x) {
  #x <- 'AAPL'
  getFinancials(x,period="A")
  tickers <-  new.env()
  s <- c(x)
  lapply(s, getFinancials,env=tickers)
  result <- data.frame(sapply(
    ls(envir=tickers),
    function(x) {x <- get(x) ## get the varible name
    x$IS$A["Income After Tax", ] / x$IS$A["Total Revenue",]}))
  colnames(result) = 'Income Af. Tax / Total Rev.'
  rownames(result) = c("Year T", "T-1", "T-2", "T-3")
  return(result)
}

Ret.Earn.Tot.Ass <- function(x) {
  #x <- 'AAPL'
  getFinancials(x,period="A")
  tickers <-  new.env()
  s <- c(x)
  lapply(s, getFinancials,env=tickers)
  result <- data.frame(sapply(
    ls(envir=tickers),
    function(x) {x <- get(x) ## get the varible name
    x$BS$A["Retained Earnings (Accumulated Deficit)", ] / x$BS$A["Total Assets",]}))
  colnames(result) = 'Retained Earnings / Total Assets.'
  rownames(result) = c("Year T", "T-1", "T-2", "T-3")
  return(result)
}

Tot.Inv.Tot.Ass <- function(x) {
  #x <- 'AAPL'
  getFinancials(x,period="A")
  tickers <-  new.env()
  s <- c(x)
  lapply(s, getFinancials,env=tickers)
  result <- data.frame(sapply(
    ls(envir=tickers),
    function(x) {x <- get(x) ## get the varible name
    x$BS$A["Total Inventory", ] / x$BS$A["Total Assets",]}))
  colnames(result) = 'Total Inventory / Total Assets.'
  rownames(result) = c("Year T", "T-1", "T-2", "T-3")
  return(result)
}

Tot.Debt.Tot.Ass <- function(x) {
  #x <- 'AAPL'
  getFinancials(x,period="A")
  tickers <-  new.env()
  s <- c(x)
  lapply(s, getFinancials,env=tickers)
  result <- data.frame(sapply(
    ls(envir=tickers),
    function(x) {x <- get(x) ## get the varible name
    x$BS$A["Total Debt", ] / x$BS$A["Total Assets",]}))
  colnames(result) = 'Total Debt / Total Assets.'
  rownames(result) = c("T", "T-1", "T-2", "T-3")
  return(result)
}

Ch.WC.Net.Inc <- function(x) {
  #x <- 'AAPL'
  getFinancials(x,period="A")
  tickers <-  new.env()
  s <- c(x)
  lapply(s, getFinancials,env=tickers)
  result <- data.frame(sapply(
    ls(envir=tickers),
    function(x) {x <- get(x) ## get the varible name
    x$CF$A["Changes in Working Capital", ] / x$IS$A["Income After Tax",]}))
  colnames(result) = 'Ch. in WC / Income Af. Tax'
  rownames(result) = c("Year T", "T-1", "T-2", "T-3")
  return(result)
}

Cash.Opr.Act.Net.Inc <- function(x) {
  #x <- 'AAPL'
  getFinancials(x,period="A")
  tickers <-  new.env()
  s <- c(x)
  lapply(s, getFinancials,env=tickers)
  result <- data.frame(sapply(
    ls(envir=tickers),
    function(x) {x <- get(x) ## get the varible name
    x$CF$A["Cash from Operating Activities", ] / x$IS$A["Income After Tax",]}))
  colnames(result) = 'Ch. in WC / Income Af. Tax'
  rownames(result) = c("Year T", "T-1", "T-2", "T-3")
  return(result)
}

Financials.IS.Table <- function(x){
  final.table <- data.frame(cbind(
    Oper.Income.Tot.Rev(x),
    Income.After.Tax.Tot.Rev(x)
  )
  )
  final.table <- data.frame(
    cbind(
      c("T", "T-1", "T-2", "T-3"), final.table
    )
  )
  colnames(final.table) = c(
    "Year",
    "Oper.Income.to.Total.Revenue",
    "Income.Aft.Tax.to.Total.Revenue"
  )
  return(final.table)
}

Financials.BS.Table <- function(x){
  final.table <- data.frame(cbind(
    Ret.Earn.Tot.Ass(x),
    Tot.Inv.Tot.Ass(x),
    Tot.Debt.Tot.Ass(x)
  )
  )
  final.table <- data.frame(
    cbind(
      c("T", "T-1", "T-2", "T-3"), final.table
    )
  )
  colnames(final.table) = c(
    "Year",
    "Ret.Earn.to.Tot.Assets",
    "Tot.Inv.to.Tot.Assets",
    "Tot.Debt.to.Tot.Assets"
  )
  return(final.table)
}

Financials.CF.Table <- function(x){
  final.table <- data.frame(cbind(
    Ch.WC.Net.Inc(x),
    Cash.Opr.Act.Net.Inc(x)
  )
  )
  final.table <- data.frame(
    cbind(
      c("T", "T-1", "T-2", "T-3"), final.table
    )
  )
  colnames(final.table) = c(
    "Year",
    "Ch.WC.to.Net.Income",
    "Cash.Opr.Act.to.Net.Income"
  )
  return(final.table)
}

################################# TEXT MINING ##################################

# Text Mining:
text.mining.plot <-  function(
  link = "BRK-B"
){
  ############## INSTALL ###############
  # Install
  #install.packages("tm")  # for text mining
  #install.packages("SnowballC") # for text stemming
  #install.packages("wordcloud") # word-cloud generator 
  #install.packages("RColorBrewer") # color palettes
  
  
  # Load
  library("tm")
  library("SnowballC")
  library("wordcloud")
  library("RColorBrewer")
  
  ############## TEXTMINING ###############
  # Convert html
  # load packages
  library(RCurl)
  library(XML)
  
  # assign input (could be a html file, a URL, html text, or some combination of all three is the form of a vector)
  link <- paste0("https://finviz.com/quote.ashx?t=",link)
  input <- link
  
  htmlToText <- function(input, ...) {
    ###---PACKAGES ---###
    require(RCurl)
    require(XML)
    
    
    ###--- LOCAL FUNCTIONS ---###
    # Determine how to grab html for a single input element
    evaluate_input <- function(input) {    
      # if input is a .html file
      if(file.exists(input)) {
        char.vec <- readLines(input, warn = FALSE)
        return(paste(char.vec, collapse = ""))
      }
      
      # if input is html text
      if(grepl("</html>", input, fixed = TRUE)) return(input)
      
      # if input is a URL, probably should use a regex here instead?
      if(!grepl(" ", input)) {
        # downolad SSL certificate in case of https problem
        if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm")
        return(getURL(input, followlocation = TRUE, cainfo = "cacert.perm"))
      }
      
      # return NULL if none of the conditions above apply
      return(NULL)
    }
    
    # convert HTML to plain text
    convert_html_to_text <- function(html) {
      doc <- htmlParse(html, asText = TRUE)
      text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
      return(text)
    }
    
    # format text vector into one character string
    collapse_text <- function(txt) {
      return(paste(txt, collapse = " "))
    }
    
    ###--- MAIN ---###
    # STEP 1: Evaluate input
    html.list <- lapply(input, evaluate_input)
    
    # STEP 2: Extract text from HTML
    text.list <- lapply(html.list, convert_html_to_text)
    
    # STEP 3: Return text
    text.vector <- sapply(text.list, collapse_text)
    return(text.vector)
  }
  
  # evaluate input and convert to text
  txt <- htmlToText(input)
  #txt
  
  # Read the text file from internet
  #filePath <- "https://finviz.com/news.ashx"
  #text <- readLines(filePath)
  text <- txt
  
  # Load the data as a corpus
  docs <- Corpus(VectorSource(text))
  
  # Inspect the content of the document
  #inspect(docs)
  
  # Text Transformation
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  
  # Text Cleaning:
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("blabla1", "blabla2",
                                      "jan", "feb", "mar")) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  docs <- tm_map(docs, stemDocument)
  
  ############## BUILD TERM-DOC MATRIX ###############
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  #head(d, 10)
  
  ############## GENERATE WORD CLOUD ###############
  set.seed(1234)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  
  ############## FURTHER EXPLORE ###############
  #findFreqTerms(dtm, lowfreq = 4)
  #findAssocs(dtm, terms = "freedom", corlimit = 0.3)
  #head(d, 10)
  
  # Plot Frequency:
  #barplot(d[1:50,]$freq, las = 2, names.arg = d[1:50,]$word,
  #        col ="lightblue", main ="Most frequent words",
  #        ylab = "Word frequencies")
} # End of function

# Text Mining:
text.mining <-  function(
  link = "BRK-B"
){
  ############## INSTALL ###############
  # Install
  #install.packages("tm")  # for text mining
  #install.packages("SnowballC") # for text stemming
  #install.packages("wordcloud") # word-cloud generator 
  #install.packages("RColorBrewer") # color palettes
  
  # Load
  library("tm")
  library("SnowballC")
  library("wordcloud")
  library("RColorBrewer")
  
  ############## TEXTMINING ###############
  # Convert html
  # load packages
  library(RCurl)
  library(XML)
  
  # assign input (could be a html file, a URL, html text, or some combination of all three is the form of a vector)
  link <- paste0("https://finviz.com/quote.ashx?t=",link)
  input <- link
  
  htmlToText <- function(input, ...) {
    ###---PACKAGES ---###
    require(RCurl)
    require(XML)
    
    
    ###--- LOCAL FUNCTIONS ---###
    # Determine how to grab html for a single input element
    evaluate_input <- function(input) {    
      # if input is a .html file
      if(file.exists(input)) {
        char.vec <- readLines(input, warn = FALSE)
        return(paste(char.vec, collapse = ""))
      }
      
      # if input is html text
      if(grepl("</html>", input, fixed = TRUE)) return(input)
      
      # if input is a URL, probably should use a regex here instead?
      if(!grepl(" ", input)) {
        # downolad SSL certificate in case of https problem
        if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm")
        return(getURL(input, followlocation = TRUE, cainfo = "cacert.perm"))
      }
      
      # return NULL if none of the conditions above apply
      return(NULL)
    }
    
    # convert HTML to plain text
    convert_html_to_text <- function(html) {
      doc <- htmlParse(html, asText = TRUE)
      text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
      return(text)
    }
    
    # format text vector into one character string
    collapse_text <- function(txt) {
      return(paste(txt, collapse = " "))
    }
    
    ###--- MAIN ---###
    # STEP 1: Evaluate input
    html.list <- lapply(input, evaluate_input)
    
    # STEP 2: Extract text from HTML
    text.list <- lapply(html.list, convert_html_to_text)
    
    # STEP 3: Return text
    text.vector <- sapply(text.list, collapse_text)
    return(text.vector)
  }
  
  # evaluate input and convert to text
  txt <- htmlToText(input)
  #txt
  
  # Read the text file from internet
  #filePath <- "https://finviz.com/news.ashx"
  #text <- readLines(filePath)
  text <- txt
  
  # Load the data as a corpus
  docs <- Corpus(VectorSource(text))
  
  # Inspect the content of the document
  #inspect(docs)
  
  # Text Transformation
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  
  # Text Cleaning:
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("blabla1", "blabla2",
                                      "jan", "feb", "mar")) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  docs <- tm_map(docs, stemDocument)
  
  ############## BUILD TERM-DOC MATRIX ###############
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  #head(d, 10)
  
  ############## GENERATE WORD CLOUD ###############
  #set.seed(1234)
  #wordcloud(words = d$word, freq = d$freq, min.freq = 1,
  #          max.words=200, random.order=FALSE, rot.per=0.35, 
  #          colors=brewer.pal(8, "Dark2"))
  
  ############## FURTHER EXPLORE ###############
  #findFreqTerms(dtm, lowfreq = 4)
  #findAssocs(dtm, terms = "freedom", corlimit = 0.3)
  #head(d, 10)
  
  # Plot Frequency:
  barplot(d[1:50,]$freq, las = 2, names.arg = d[1:50,]$word,
          col ="lightblue", main ="Top 50 Most Frequent Words",
          ylab = "Word frequencies")
} # End of function

####################### DESIGN SHINY ###################################

shinyApp(
  ####################### DEFINE: UI ##############################
  ui = tagList(
    shinythemes::themeSelector(),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "CENTRAL INTELLIGENCE PLATFORM",
      fluid = TRUE,
      tabPanel("Navbar 1: ALPHA PROTOCAL",
               sidebarPanel(
                 h4("--------- MAIN ---------"),
                 textInput(inputId = "stock_enter_1", label = "Enter Ticker", value = "AAPL"),
                 sliderInput(inputId = "percentage", label = "Data: from the x% to y%", min = 0.05, max = 1, value = c(0.5, 1), step = 0.001),
                 #sliderInput(inputId = "percentage.end", label = "Data: to the the y%", min = 0.65, max = 1, value = 1, step = 0.001),
                 sliderInput(inputId = "constant", label = "Frequency (How often to buy/sell)", min = -4, max = 4, value = c(-1.96, +1.96), step = 0.01),
                 #sliderInput(inputId = "constant.sell", label = "Frequency (How often to sell)", min = 0.0000001, max = 4, value = +1.96, step = 0.01),
                 sliderInput(inputId = "past.n.days", label = "Past n-day Buy/Sell Signal; Forecast n-D/W/M/Q price range by auto ARIMA", min = 1, max = 30, value = 3, step = 1),
                 sliderInput(inputId = "past.n.days.corr", label = "Past n Days for Correlation", min = 100, max = 750, value = 500, step = 50),
                 textInput(inputId = "test.new.price", label = "Intraday: Test New Price", value = 0),
                 h4("--------- INTERACTION ---------"),
                 sliderInput(inputId = "size.n.neighbor.navbar.1", label = "BDA: Size of Nearest Neighbor (Sparsity in Data Engineering)", min = 2, max = 70, value = 20, step = 1),
                 sliderInput(inputId = "size.k.navbar.1", label = "BDA: Size of Initial Draw (Size of Strata at Detecting Interaction)", min = 4, max = 8, value = 5, step = 1),
                 sliderInput(inputId = "size.top.how.many.navbar.1", label = "BDA: Select Top Modules", min = 3, max = 20, value = 7, step = 1),
                 sliderInput(inputId = "size.folds.navbar.1", label = "CV: Number of Folds (Consistency of AI Leaner)", min = 4, max = 11, value = 6, step = 1),
                 submitButton("Run Script")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("BASIC",
                            h4("Stock Chart Daily Chart"),
                            dygraphOutput("dygraph"), 
                            h4("Stock Chart Buy/Sell Signal"),
                            dygraphOutput("dygraph.signal"),
                            #h4("Stock Chart Weekly Chart"),
                            #dygraphOutput("dygraph.week"),
                            #h4("Stock Chart Monthly Chart"),
                            #dygraphOutput("dygraph.month"),
                            h4("Buy Sell Action (in Table)"),
                            tableOutput(outputId = "table_enter_1"),
                            tableOutput(outputId = "table_enter_1_BS_Dist")#,
                            #tableOutput(outputId = "table_enter_1_price_to_market_table"),
                            #tableOutput(outputId = "table_enter_1_price_to_market_stats")
                   ),
                   #tabPanel("FORECAST", 
                   #         h4("Next Day/Week/Month TS Forecast with 80 and 95 Percentile Risk Profile (Predict Actual Price Per Share)"),
                   #         tableOutput(outputId = "table_enter_1_1"),
                   #         tableOutput(outputId = "table_enter_1_2"),
                   #         tableOutput(outputId = "table_enter_1_3"),
                   #         tableOutput(outputId = "table_enter_1_4"),
                            #verbatimTextOutput(outputId = "ts.prediction.k.fold.cv"),
                            #dygraphOutput("ts.prediction.k.fold.cv.plot"),
                            #h4("Next Day/Week/Month RNN Forecast with 95 Percentile Risk Profile (Predict Changes in Dollar Amount Per Share)"),
                            #tableOutput(outputId = "table_enter_1_5_RNN"),
                   #         h4("Correlation of Buy Signals with Indices"),
                   #         tableOutput(outputId = "table_enter_2")
                   #),
                   tabPanel("TIMES-SERIES CV",
                            h4("ARIMA Model: K-fold Cross Validation"),
                            verbatimTextOutput(outputId = "ts.prediction.k.fold.cv"),
                            h4("ARIMA Model: Plot of the Set-aside Test Set"),
                            dygraphOutput("ts.prediction.k.fold.cv.plot")
                            ),
                   tabPanel("INTERACTION-BASED FORECAST",
                            h4("Interaction-based AI Learner: Learning, Validating, and Test"),
                            verbatimTextOutput(outputId = "iscore.buy.prediction.Influential.Historical.Price"),
                            #h4("Interaction-based AI Learner: Validating"),
                            #tableOutput(outputId = "iscore.buy.prediction.Train.Result"),
                            #h4("Interaction-based AI Learner: Test"),
                            #tableOutput(outputId = "iscore.buy.prediction.Prediction"),
                            h4("Interaction-based AI Learner: Plot of the Set-aside Test Set"),
                            dygraphOutput("iscore.buy.prediction.plot")
                            #h4("Interaction-based AI Learner: Future"),
                            #tableOutput(outputId = "iscore.buy.prediction.Buy.Prediction")
                   ),
                   tabPanel("OTHER", 
                            h4("SEC Link for Entered Company"),
                            #tableOutput(outputId = "sec_link"),
                            uiOutput("tab"),
                            h4("High-Frequency Words Appeared Today Associated with the Stock"),
                            plotOutput(outputId = "text_plot_enter_1"),
                            plotOutput(outputId = "text_plot_enter_2")
                   )
                 ),
                 tags$head(
                   conditionalPanel(condition="input.goButton > 0 | $('html').hasClass('shiny-busy')",
                                    tags$div(
                                      c("Calculating... Please wait... Patience is the key to success.",
                                        "Calculating... Please wait... Patience is not simply the ability to wait - it's how we behave while we're waiting",
                                        "Calculating... Please wait... The two most powerful warriors are patience and time."
                                        )[sample(3,3)[1]]
                                      )))
                 )
      ),
      tabPanel("Navbar 2: HOUSE PARTY PROTOCOL", 
               sidebarPanel(
                 sliderInput(inputId = "Base.Dollar", label = "Initial Investment", min = 1, max = 1000000, value = 1000, step = 1000),
                 sliderInput(inputId = "size.k", label = "BDA: Size of Initial Draw", min = 5, max = 20, value = 12, step = 1),
                 sliderInput(inputId = "size.rounds", label = "BDA: Number of Rounds", min = 100, max = 2000, value = 200, step = 100),
                 sliderInput(inputId = "size.n.neighbor", label = "BDA: Size of Nearest Neighbor", min = 2, max = 7, value = 3, step = 1),
                 sliderInput(inputId = "top.how.many", label = "BDA: Select Top Modules", min = 2, max = 50, value = 7, step = 1),
                 submitButton("Run Script")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("iPortfolio - All Market",
                            h4("Influential Portfolio (iPortfolio) by Robust Stock Clusters 
                               (list out Top Modules and Portfolio Weights)"),
                            verbatimTextOutput(outputId = "B.W.WatchList.Robust"),
                            #h4("Influential Portfolio (iPortfolio) on Market: Stocks and Weight w/ Replicate"),
                            #DT::dataTableOutput(outputId = "B.W.WatchList.Robust.DP"),
                            h4("Simulation for Designated Initial Account Value"),
                            dygraphOutput(outputId = "B.W.WatchList.Plot"),
                            h5("Note: the script takes a long time. Please load the app and go grab a coffee.")),
                   #tabPanel("iPortfolio - MKT Portfolio",
                   #         h4("Influential Portfolio (iPortfolio) on Market: Stocks and Weight w/ Replicate"),
                   #         DT::dataTableOutput(outputId = "B.W.WatchList.Robust.DP")),
                   tabPanel("iPortfolio - TECH Sector",
                            h4("Influential Portfolio (iPortfolio) by Robust Stock Clusters 
                               (list out Top Modules and Portfolio Weights)"),
                            verbatimTextOutput(outputId = "B.W.Tech.WatchList.Robust"),
                            #h4("Influential Portfolio (iPortfolio) on Technology: Stocks and Weight w/ Replicate"),
                            #DT::dataTableOutput(outputId = "B.W.Tech.WatchList.Robust.DP"),
                            h4("Simulation for Designated Initial Account Value"),
                            dygraphOutput(outputId = "B.W.Tech.WatchList.Plot"),
                            h5("Note: the script takes a long time. Please load the app and go grab a coffee."))
                   #tabPanel("iPortfolio - TECH Portfolio",
                   #         h4("Influential Portfolio (iPortfolio) on Technology: Stocks and Weight w/ Replicate"),
                   #         DT::dataTableOutput(outputId = "B.W.Tech.WatchList.Robust.DP"))
                 )
               )
      )
    )
  ),
  ####################### DEFINE: SERVER ##############################
  server = function(input, output, session) {
    # NAVBAR 1
    # Create an environment for storing data
    symbol_env <- new.env()
    # Make a chart for a symbol, with the settings from the inputs
    make_chart <- function(symbol) {
      symbol_data <- require_symbol(symbol, symbol_env)
      
      chartSeries(symbol_data,
                  name      = symbol,
                  type      = input$chart_type,
                  subset    = paste(input$daterange, collapse = "::"),
                  log.scale = input$log_y,
                  theme     = "white")
    }
    output$dygraph <- renderDygraph({
      Basic.Plot(
        x = require_symbol(input$stock_enter_1),
        r_day_plot = input$percentage[1],
        end_day_plot = input$percentage[2]
      )
    })
    output$dygraph.signal <- renderDygraph({
      BS.Algo.Chart(
        x = require_symbol(input$stock_enter_1),
        r_day_plot = input$percentage[1],
        end_day_plot = input$percentage[2],
        c.buy = input$constant[1],
        c.sell = input$constant[2],
        height = 0.005, #input$signal.height,
        past.n.days = input$past.n.days+300,
        test.new.price = input$test.new.price)
    })
    output$table_enter_1 <- renderTable({
      BS.Algo(
        x = require_symbol(input$stock_enter_1),
        r_day_plot = input$percentage[1],
        end_day_plot = input$percentage[2],
        c.buy = input$constant[1],
        c.sell = input$constant[2],
        height = 0.005, #input$signal.height,
        past.n.days = input$past.n.days,
        test.new.price = input$test.new.price)
    })
    output$table_enter_1_BS_Dist <- renderTable({
      BS.Dist(
        x = require_symbol(input$stock_enter_1),
        r_day_plot = input$percentage[1],
        end_day_plot = input$percentage[2],
        c.buy = input$constant[1],
        c.sell = input$constant[2],
        height = 0.005, #input$signal.height,
        test.new.price = 0
      )
    })

    # FORECAST
    #output$table_enter_1_1 <- renderTable({
    #  ARMA_Fit_D(
    #    entry = require_symbol(input$stock_enter_1),
    #    ahead = input$past.n.days
    #  )
    #})
    #output$table_enter_1_2 <- renderTable({
    #  ARMA_Fit_W(
    #    entry = require_symbol(input$stock_enter_1),
    #    ahead = input$past.n.days
    #  )
    #})
    #output$table_enter_1_3 <- renderTable({
    #  ARMA_Fit_M(
    #    entry = require_symbol(input$stock_enter_1),
    #    ahead = input$past.n.days
    #  )
    #})
    #output$table_enter_1_4 <- renderTable({
    #  ARMA_Fit_Q(
    #    entry = require_symbol(input$stock_enter_1),
    #    ahead = input$past.n.days
    #  )
    #})
    output$ts.prediction.k.fold.cv <- renderPrint({
      ARMA_Fit_D_KFOLD_CV(
        entry = require_symbol(input$stock_enter_1),
        ahead = input$past.n.days,
        cutoff = 0.9,
        how.many.fold = input$size.folds.navbar.1)
    })
    output$ts.prediction.k.fold.cv.plot <- renderDygraph({
      ARMA_Fit_D_KFOLD_CV_Plot_Only(
        entry = require_symbol(input$stock_enter_1),
        ahead = input$past.n.days,
        cutoff = 0.9,
        how.many.fold = input$size.folds.navbar.1)
    })
    #output$table_enter_1_5_RNN <- renderTable({
    #  RNN_All(
    #    entry = require_symbol(input$stock_enter_1)
    #    #ahead = input$past.n.days
    #  )
    #})
    output$table_enter_2 <- renderTable({
      Buy.table.corr(
        x = require_symbol(input$stock_enter_1),
        r_day_plot = input$percentage[1],
        end_day_plot = input$percentage[2],
        c = input$constant,
        height = 0.005, #input$signal.height,
        past.n.days = input$past.n.days.corr)
    })
    
    # INTERACTION-BASED LEARNING
    output$iscore.buy.prediction.Influential.Historical.Price <- renderPrint({
      iscore.buy.prediction(
        x = require_symbol(input$stock_enter_1),
        k.mean = input$size.n.neighbor.navbar.1,
        num.initial.set = input$size.k.navbar.1,
        how.many.folds = input$size.folds.navbar.1,
        how.many.modules = input$size.top.how.many.navbar.1
        #Buy.Frequency = input$constant,
        #daily.entry = input$test.new.price
        )#$Influential.Historical.Price
    })
    #output$iscore.buy.prediction.Train.Result <- renderTable({
    #  iscore.buy.prediction(
    #    x = require_symbol(input$stock_enter_1),
    #    k.mean = input$size.n.neighbor.navbar.1,
    #    num.initial.set = input$size.k.navbar.1,
    #    how.many.folds = input$size.folds.navbar.1,
    #    how.many.modules = input$size.top.how.many.navbar.1
        #Buy.Frequency = input$constant,
        #daily.entry = input$test.new.price
    #    )$Train.Result
    #})
    #output$iscore.buy.prediction.Prediction <- renderTable({
    #  iscore.buy.prediction(
    #    x = require_symbol(input$stock_enter_1),
    #    k.mean = input$size.n.neighbor.navbar.1,
    #    num.initial.set = input$size.k.navbar.1,
    #    how.many.folds = input$size.folds.navbar.1,
    #    how.many.modules = input$size.top.how.many.navbar.1
        #Buy.Frequency = input$constant,
        #daily.entry = input$test.new.price
     #   )$Prediction
    #})
    output$iscore.buy.prediction.plot <- renderDygraph({
      iscore.buy.prediction.plot(
        x = require_symbol(input$stock_enter_1),
        k.mean = input$size.n.neighbor.navbar.1,
        num.initial.set = input$size.k.navbar.1,
        how.many.folds = input$size.folds.navbar.1,
        how.many.modules = input$size.top.how.many.navbar.1
        #Buy.Frequency = input$constant,
        #daily.entry = input$test.new.price
      )
    })
    #output$iscore.buy.prediction.Buy.Prediction <- renderText({
    #  iscore.buy.prediction(
    #    x = require_symbol(input$stock_enter_1),
    #    k.mean = input$size.n.neighbor.navbar.1,
    #    num.initial.set = input$size.k.navbar.1,
    #    how.many.folds = input$size.folds.navbar.1,
    #    how.many.modules = input$size.top.how.many.navbar.1
        #Buy.Frequency = input$constant,
        #daily.entry = input$test.new.price
    #    )$Buy.Prediction
    #})

    # OTHER
    #output$sec_link <- renderTable({SEC.gov(input$stock_enter_1)})
    #url <- a("here", href="SEC.gov(input$stock_enter_1)")
    url <- reactive({
      a("here", href = paste0(
        "https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=",
        #"IBM", 
        input$stock_enter_1,
        "&type=10-K&dateb=&owner=exclude&count=40"
      ))
    })
    output$tab <- renderUI({
      tagList("Please click", url())
    })
    #output$sec_link <- renderUI({
    #  withMathJax(helpText(  
    #    paste0(a("Here", href=SEC.gov(input$stock_enter_1)[2,]))))
    #})
    output$text_plot_enter_1 <- renderPlot({
      text.mining(link = input$stock_enter_1)
    })
    output$text_plot_enter_2 <- renderPlot({
      text.mining.plot(link = input$stock_enter_1)
    })

    
    # NAVBAR 3
    # TAB 1 ROBUST
    output$B.W.WatchList.Robust <- renderPrint({
      Mkt.Watch.List(
        k = input$size.k, # Size of Initial Draw
        rounds = input$size.rounds, # How many rounds of BDA?
        n.neighbor = input$size.n.neighbor, # Size of nearest neighbor
        top.how.many = input$top.how.many
      ) #$Var.Result
    })
    #output$B.W.WatchList.Robust.DP <- DT::renderDataTable({
    #  DT::datatable(Mkt.Watch.List(
    #    k = input$size.k, # Size of Initial Draw
    #    rounds = input$size.rounds, # How many rounds of BDA?
    #    n.neighbor = input$size.n.neighbor, # Size of nearest neighbor
    #    top.how.many = input$top.how.many
    #  )$Detail.Portfolio) %>% 
    #    formatStyle(
    #      colnames(Mkt.Watch.List(
    #        k = input$size.k, # Size of Initial Draw
    #        rounds = input$size.rounds, # How many rounds of BDA?
    #        n.neighbor = input$size.n.neighbor, # Size of nearest neighbor
    #        top.how.many = input$top.how.many
    #      )$Detail.Portfolio),
    #      color = styleInterval(c(0.05), c('yello','green')))
    #})
    
    # TAB 1 Plot
    output$B.W.WatchList.Plot <- renderDygraph({
      Mkt.Watch.List.Plot(
        base.dollar = input$Base.Dollar,
        k = input$size.k, # Size of Initial Draw
        rounds = input$size.rounds, # How many rounds of BDA?
        n.neighbor = input$size.n.neighbor, # Size of nearest neighbor
        top.how.many = input$top.how.many
      )
    })
    # TAB 2 ROBUST
    output$B.W.Tech.WatchList.Robust <- renderPrint({
      Tech.Watch.List(
        k = input$size.k, # Size of Initial Draw
        rounds = input$size.rounds, # How many rounds of BDA?
        n.neighbor = input$size.n.neighbor, # Size of nearest neighbor
        top.how.many = input$top.how.many
      ) #$Var.Result
    })
    #output$B.W.Tech.WatchList.Robust.DP <- DT::renderDataTable({
    #  DT::datatable(Tech.Watch.List(
    #    k = input$size.k, # Size of Initial Draw
    #    rounds = input$size.rounds, # How many rounds of BDA?
    #    n.neighbor = input$size.n.neighbor, # Size of nearest neighbor
    #    top.how.many = input$top.how.many
    #  )$Detail.Portfolio)  %>% 
    #    formatStyle(
    #      colnames(Tech.Watch.List(
    #        k = input$size.k, # Size of Initial Draw
    #        rounds = input$size.rounds, # How many rounds of BDA?
    #        n.neighbor = input$size.n.neighbor, # Size of nearest neighbor
    #        top.how.many = input$top.how.many
    #      )$Detail.Portfolio),
    #      color = styleInterval(c(0.05), c('yello','green')))
    #})
    
    # TAB 2 Plot
    output$B.W.Tech.WatchList.Plot <- renderDygraph({
      Tech.Watch.List.Plot(
        base.dollar = input$Base.Dollar,
        k = input$size.k, # Size of Initial Draw
        rounds = input$size.rounds, # How many rounds of BDA?
        n.neighbor = input$size.n.neighbor, # Size of nearest neighbor
        top.how.many = input$top.how.many
      )
    })
     
  }
)

############################## END SCRIPT ###############################