# loan data analysis learning 

# define path
sampleDataDir <- rxGetOption("sampleDataDir")
sampleDataDir 

mortCsvDataName <- file.path(sampleDataDir, "mortDefaultSmall")
mortCsvDataName 

bigDataDir <- "C:/Users/Li/Desktop/machinelearning"
mortCsvDataName <- file.path(bigDataDir, "mortDefault", "mortDefault")

mortXdfFileName <- "mortDefaultSmall.xdf"

# import data
append <- "none"
for (i in 2000:2009)
{
  importFile <- paste(mortCsvDataName, i, ".csv", sep="")
  mortDS <- rxImport(importFile, mortXdfFileName, append=append)
  append <- "rows"
}

# get basic information of data
rxGetInfo(mortDS, numRows=5)

# computing summary statistics
rxSummary(~., data = mortDS,blocksPerRead = 2)

# logistic regression
logitObj <- rxLogit(default ~F(year)+creditScore + yearsEmploy + ccDebt,
                    data = mortDS, blocksPerRead = 2,
                    reportProgress = 1)
summary(logitObj)

# logistic regression with more paramters
system.time(
  logitObj <- rxLogit(default ~ F(houseAge) + F(year) + 
                         creditScore + yearsEmploy + ccDebt,
                       data = mortDS, blocksPerRead = 2, reportProgress = 1))
summary(logitObj)


# extract coeffcients from logitObj
cc <- coef(logitObj)
df <- data.frame(coefficients = cc[2:41], HouseAge = 0:39)
rxLinePlot(coefficients ~ HouseAge, data = df, type = "p")


# compute the probability of default
# input
creditScore <- c(300, 700)
yearsEmploy <- c( 2, 8)
ccDebt <- c(5000, 10000)
year <- c(2008, 2009)
houseAge <- c(5, 20)

# create a data frame
predictDF <- data.frame(
  creditScore = rep(creditScore, times = 16),
  yearsEmploy = rep(rep(yearsEmploy, each = 2), times = 8),
  ccDebt      = rep(rep(ccDebt, each = 4), times = 4),
  year        = rep(rep(year, each = 8), times = 2),
  houseAge    = rep(houseAge, each = 16))
  
# predict 
predictDF <- rxPredict(modelObject = logitObj, data = predictDF,
                       outData = predictDF)
predictDF[order(predictDF$default_Pred, decreasing = TRUE),]
  