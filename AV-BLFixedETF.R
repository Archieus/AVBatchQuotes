library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(fPortfolio)
library(quantmod)
library(BLCOP)
Sys.setenv(TZ = "EST5EDT")

#### reads/loads CSV into R ####
symblist <- t(read.csv("blfixed.csv", header = FALSE))

quotes <- new.env() # create new enviroment for data to be loaded to

#### CREATE COMPONENTS FOR API CALL ####
apikey <- "&outputsize=full&apikey=Y474&datatype=csv"
URLbase <- "http://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol="

cu <-NULL
ru <-NULL

### Loop to download data for all symbols in list as it's own object ###
dataEnv <- new.env()

for(i in 1:length(symblist)){
  cu[i] <- paste0(URLbase, symblist[i])
  ru[i] <- paste0(cu[i],apikey)
  assign(paste(symblist[i]), read.csv(ru[i]), env = dataEnv)
}

### IDENTIFY THE SYMBOL WITH THE LEAST NUMBER OF ROWS IN ENVIRONMENT ###
RowCount <- matrix(0, ncol = ncol(symblist), nrow = 1)
for(i in 1:length(symblist)) {
  RowCount[,i] <- cbind(nrow(get(symblist[,i], envir = dataEnv)))
}

RowMin <- min(RowCount)

#### SET NEW ENVIRONMENT FOR ADJUSTED CLOSE DATA ####
qmodEnv <- new.env()

for(i in 1:length(symblist)) {
  assign(symblist[i], head(cbind(get(symblist[,i], envir = dataEnv)$adjusted_close), RowMin), envir = qmodEnv)
}

#### Create a Matrix of Adjusted Close Values ####
AC <- matrix(0, ncol = ncol(symblist), nrow = RowMin)
for(i in 1:length(symblist)){
  AC[,i] <- cbind(get(symblist[,i], envir = qmodEnv))
}

Dates <- get(symblist[1], envir = dataEnv)$timestamp
df <- matrix(unlist(Dates))

AC.df <- as.data.frame(AC)
colnames(AC.df) <- symblist
row.names(AC.df) <- head(df,RowMin)

#### Create the XTS Object to be used for analysis ####
AC.zoo <- as.zoo(AC.df)
DailyClose <- as.xts(AC.zoo)
FIRet <- na.omit(Return.calculate(DailyClose))

###Convert to Monthly Data
monthend <- endpoints(FIRet, on = "months", k = 1)
Ret.mo <- FIRet[monthend]

BLroll <- rollingWindows(as.timeSeries(Ret.mo), "12m", "1m")

FIETFCAPMbeta <- CAPM.beta(Ret.mo[,c(1,3:13)]['last(BLroll$from):/last(BLroll$end)'],
                           Ret.mo$BNDX['last(BLroll$from):/last(BLroll$end)'], .03/12)
#Apply Weigths to Returns of Individual Assets
FIETFEqLib <- Return.portfolio(Ret.mo[,c(1,3:13)]['last(BLroll$from):/last(BLroll$end)'],
                               weights = as.numeric(FIETFCAPMbeta))
#Calculate Excess Returns over Eq Lib Port
FIETFExcRet <- Return.excess(Ret.mo[,c(1,3:13)]['last(BLroll$from):/last(BLroll$end)'], FIETFEqLib)
#Create Cov Matrix of Excess Returns
CovMat <- cov.mve(FIETFExcRet)$cov
nameList <- names(Ret.mo[,c(1,3:13)])
colnames(CovMat) <- nameList

##myPosterior Data##
priorMeans <- rep(0,12) #set means to 0 for the twelve assets

###Create a "pick" matrix.  Connects assets with a specific "view"
Pick <- matrix(0, ncol = ncol(CovMat), nrow = 6, dimnames = list(NULL,as.list(colnames(nameList))))

###Create a Vector Q which contains information on the Excess Return for the corresponding "view"
#QVect <- c(.02,.02,0,0,0,0)
QVect <- c(0,0,0,0,0,0)

##Fill Matrix with Picks on over and underperformance (Relative or Absolute)###
##Relative views in a row must net to zero (0).
##Absolute view in a row must add upt to one (1).
##Fill row1, col7 ==> pick[1,7]

#Pick[1,12] <- -1
#Pick[1,1] <- 1
#Pick[2,2] <- -1
#Pick[2,7] <- 1

# tau = scalar = set to as close to zero as possible (in practice) .025?
##Calculate the confidence of the Views (recipricols of the Variances of View Portfolios * tau)

#ViewConf <- c(70,70,.01,.01,.01,.01) #Between .01(No confidence) and 100(High confidence)
ViewConf <- c(.01,.01,.01,.01,.01,.01)
Views <- BLViews(Pick, QVect, confidences = ViewConf, assetNames = colnames(CovMat))

###Generate "posterior" estimates using "prior" inputs and Investors Views and confidences
CAPMPosterior <- posteriorEst(Views, mu = priorMeans, tau = 0.025, sigma = CovMat)

###Optimize with Constraints to Max Weight###
cvarSpec <- portfolioSpec(
  model = list(type = "CVaR", optimize = "minRisk",
               estimator = "covEstimator", tailRisk = list(),
               params = list(alpha = 0.05)),
  portfolio = list(weights = NULL,
                   targetReturn = NULL, targetRisk = NULL,
                   riskFreeRate = 0, nFrontierPoints = 50,
                   status = 0),
  optim = list(solver = "solveRglpk.CVAR", objective = NULL,
               params = list(), control = list(), trace = FALSE))

optimalPortfolios(CAPMPosterior)