Sys.setenv(TZ = "UTC")
# AlphaVantage API Key: 4FS12H08TP68SJV2

#Download Daily Data for Components of the portfolio
BuyList <- read.csv("Buy_List.csv")
BuyList.tr <- t(BuyList)

ETFList <- read.csv("etfs.csv", header = TRUE)
etfs.tr <- t(ETFList)

symbol <- cbind(BuyList.tr, etfs.tr)

# read.csv("https://www.alphavantage.co/query?function=BATCH_STOCK_QUOTES&symbols=TPYP,XME,XOP&apikey=4FS12H08TP68SJV2&datatype=csv")

#### CREATE COMPONENTS FOR API CALL ####
apikey <- "&apikey=4FS12H08TP68SJV2&datatype=csv"
URLbase <- "https://www.alphavantage.co/query?function=BATCH_STOCK_QUOTES&symbols="

cu <-NULL
ru <-NULL

### Loop to download data for all symbols in list as it's own object ###

#### Set environment for quotes ####
quoteEnv<- new.env()

###Download Quotes###
for(i in 1:length(symbol)){
  cu[i] <- paste0(URLbase, symbol[i])
  ru[i] <- paste0(cu[i],apikey)
  Sys.sleep(1)
  assign(paste(symbol[i]), read.csv(ru[i], sep = ","), envir = quoteEnv)
}

Price <- NULL
### Extract Last Quote for each Symbol ###
for(i in 1:length(symbol)) {
  Price[i] <- cbind(get(symbol[i], envir = quoteEnv)$price)
}

LastQuote <- cbind(t(symbol), Price)

