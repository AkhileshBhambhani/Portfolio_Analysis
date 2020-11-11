#Portfolio Analysis

#Installing the Needed Packages
install.packages("quantmod")
install.packages("ggplot2")
install.packages("PerformanceAnalytics")
install.packages("PortfolioAnalytics")
install.packages("dplyr")
install.packages("stringr")
install.packages("ggthemes")
install.packages("ROI")
install.packages("ROI.plugin.quadprog")


#Loading Installed packages 

library(quantmod)
library(ggplot2)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(stringr)
library(dplyr)
library(ggthemes)
library(ROI)
library(ROI.plugin.quadprog)


##################################################### USER INPUT SECTION ####################################################################

#Specify the TICKER of ETF or Security in the below format, No less than 3 or no more than 5 to be tried at once
funds<- c("MSOPX", "TRBCX", "FDSCX", "FRVLX", "MACGX")

#Specifying the start Date for fund/Security Return, If a Fund or Security is incepted after User defined date, data for all other instruments will be omitted BEFORE the inception Date 
#Keep the Date in ISO Format

start_date<- as.Date("2015-01-01")

##################################################### End of Input Section ##################################################################

if (length(funds) > 5 | length(funds) <3) {
  print("Number of ETFs Chosen not Appropriate!")
  quit(save = "ask")
}

fund_env<- new.env()

#Extracting Data from Yahoo Finance

getSymbols(funds, from = start_date, env = fund_env)

loaded_funds<- funds[which(funds %in% ls(envir = fund_env))]

if (length(loaded_funds) != length(funds) ) {
  print(paste(c(funds[-which(funds %in% ls(envir = fund_env))]), "Was not an Appropriate TICKER Symbol, Please Check!!"))
  quit(save = "ask")
}


Adj_funds<- do.call(cbind, eapply(fund_env, Ad))

Adj_funds<- na.omit(Adj_funds)

Return_fund<-Return.calculate(Adj_funds)

Return_fund<- Return_fund[-1,]

df<- as.data.frame(Return_fund)

#Combinations for Portfolios

portfolios_1<- combn(df,3,simplify = F)
portfolios_2<-combn(df,4,simplify = F)
portfolios_3<- combn(df,5,simplify = F)

existing_objects<- ls()

if("portfolios_3" %in% existing_objects) {
  all_portfolios<- c(portfolios_1, portfolios_2, portfolios_3)
} else {
  if ("portfolios_2" %in% existing_objects) {
    all_portfolios<- c(portfolios_1, portfolios_2)
  } else {all_portfolios<- portfolios_1}
}

xts_portfolios<-lapply(all_portfolios, xts, order.by = index(Return_fund))


#Defining Constraints and Objectives
port_spec_list<- rep(list(NA), length(xts_portfolios))

for (i in 1:length(xts_portfolios)) {
  port_spec_list[[i]]<- portfolio.spec(assets = colnames(xts_portfolios[[i]]))
  port_spec_list[[i]]<- add.constraint(portfolio = port_spec_list[[i]], type = "full_investment")
  port_spec_list[[i]]<- add.constraint(portfolio = port_spec_list[[i]], type = "box", min = 0.05, max = 0.50)
  port_spec_list[[i]]<- add.objective(portfolio = port_spec_list[[i]], type = "return", name = "mean")
  port_spec_list[[i]]<- add.objective(portfolio = port_spec_list[[i]], type = "risk", name = "StdDev")
}

#Generating Returns and Optimum Weights

opt_portfolio_list<-rep(list(NA), length(port_spec_list))
return_portfolio_list<- rep(list(NA), length(port_spec_list))

for (i in 1: length(opt_portfolio_list)) {
  opt_portfolio_list[[i]]<- optimize.portfolio(R = xts_portfolios[[i]], portfolio = port_spec_list[[i]], optimize_method = "ROI")
  return_portfolio_list[[i]]<-Return.portfolio(xts_portfolios[[i]], weights = extractWeights(opt_portfolio_list[[i]]))
}

#Cleaning up Names and Creating Return Data Frame

Portfolio_Returns<- data.frame(rep(NA, length(return_portfolio_list)))
colnames(Portfolio_Returns)<- "Portfolio"
Portfolio_Returns$Portfolio<- sapply(xts_portfolios, colnames)
Portfolio_Returns$Portfolio_Names<- str_remove_all(Portfolio_Returns$Portfolio, "[^[:alnum:]]")
Portfolio_Returns$Portfolio<- str_remove(Portfolio_Returns$Portfolio_Names, "c")
Portfolio_Returns$Portfolio_Names<- str_replace_all(Portfolio_Returns$Portfolio, "Adjusted", ", ")
Portfolio_Returns$Portfolio_Names<-substr(Portfolio_Returns$Portfolio_Names,start = 1,stop = nchar(Portfolio_Returns$Portfolio_Names)-2)
Portfolio_Returns%>%select(-Portfolio)->Portfolio_Returns
names(return_portfolio_list)<- Portfolio_Returns$Portfolio_Names
names(opt_portfolio_list)<- names(return_portfolio_list)
names(port_spec_list)<-names(return_portfolio_list)

Portfolio_Returns$Return_Annualized<-sapply(return_portfolio_list, Return.annualized)
Portfolio_Returns$StdDev_Annualized<- sapply(return_portfolio_list, StdDev.annualized)
Portfolio_Returns$SharpeRatio_Annualized<- sapply(return_portfolio_list, SharpeRatio.annualized)

Portfolio_Returns%>%arrange(desc(Return_Annualized), desc(SharpeRatio_Annualized))->Portfolio_Returns

rm(portfolios_1,portfolios_2, portfolios_3, existing_objects, loaded_funds)

#Getting the top 5 Funds

Portfolio_Returns%>%top_n(Return_Annualized,n = 5)->Top5_Portfolios

#Adding Recommended Weights to Top5 Portfolios

top5_index<-which(names(opt_portfolio_list)%in% Top5_Portfolios$Portfolio_Names)

weights_list<-lapply(opt_portfolio_list[top5_index], extractWeights)

weights_df<-data.frame(names(weights_list))
weights_df%>%rename("Portfolios" = names.weights_list.)%>%mutate(Weights = NA)->weights_df

for(i in 1:nrow(weights_df)) {
  weights_df[i,2]<-paste(paste(names(weights_list[[i]]),round(as.vector(weights_list[[i]]),2)),collapse = ",")
}

weights_df$Weights<- str_replace_all(weights_df$Weights, ".Adjusted", ":")
weights_df$Weights<- str_replace_all(weights_df$Weights, ",", ", ")

Top5_Portfolios%>%inner_join(weights_df, by = c("Portfolio_Names" = "Portfolios"))->Top5_Portfolios


#Chart for $1 Return
charts.PerformanceSummary(return_portfolio_list[[Top5_Portfolios[1,1]]], main = Top5_Portfolios[1,1], plot.engine = "default", wealth.index = T, col = "lightgreen")

#Return xts for top5 Portfolios
top5_portfolio_returns<-do.call(cbind, return_portfolio_list[top5_index])
colnames(top5_portfolio_returns)<- names(return_portfolio_list[top5_index])

#top5 Portfolios Risk Measures
top5_portfolio_risk<-table.DownsideRisk(top5_portfolio_returns)
top5_portfolio_risk%>%mutate(Risk_Measure = rownames(top5_portfolio_risk))%>%select(Risk_Measure, everything())->top5_portfolio_risk

# Getting Benchmark Data

SPX<-getSymbols(Symbols = "^GSPC", auto.assign = F,from = min(index(Adj_funds)))
SPX_return<- Return.calculate(Ad(SPX))
SPX_return<- SPX_return[-1,]

RelativePerformance<-top5_portfolio_returns[,Top5_Portfolios[1,1]]
RelativePerformance$Sp500<-SPX_return$GSPC.Adjusted

#Charting Relative Performance DATA
charts.PerformanceSummary(RelativePerformance, main = "Relative Performance", wealth.index = T, col = c("lightgreen","red"))

View(Top5_Portfolios)
View(top5_portfolio_risk)

