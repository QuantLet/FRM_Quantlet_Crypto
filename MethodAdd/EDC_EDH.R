# EDC / EDH finish:0121
# load past 

rm(list = ls(all = TRUE))

# install.packages('deldir',type = "binary")
# install.packages('interp',type = "binary")
# install.packages('cvar')
# install.packages('PerformanceAnalytics')
libraries = c("cvar","classInt","interp","PerformanceAnalytics","SHAPforxgboost","shapr","xgboost","readxl","iml","ggplot2", "data.table", "igraph","timeDate", "stringr", 
              "graphics","magick", "scales", "tidyr", "zoo","xts", "foreach", "doParallel",
              "xgboost","shapr","randomForest", "rpart", "quantreg", "readxl","dplyr", "xlsx", "psych","qgraph", "gganimate","av",
              "gifski", "strex","matrixStats","tools","Hmisc","vars","aTSA","quantreg","rapport","sjmisc","haven","foreign","e1071")
# lapply(libraries, function(x) if (!(x %in% installed.packages())) {
#   install.packages(x)
# })
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

wdir = "/Users/ruting/Documents/Github/FRM/FRM_Quantlet/FRM_All/"
# wdir ="/Users/rutingwang/Library/Mobile Documents/com~apple~CloudDocs/Documents/Github/FRM/FRM_Quantlet/FRM_All"
setwd(wdir)
channel = "Crypto"
save_date = 20230514

return = read.csv(file = paste0(wdir, "/MethodAdd/Return_",save_date,".csv"), header = TRUE)%>% data.frame()
return = return[,-1]

nncol = ncol(return)
lengthfull = nrow(return)
winsize = 62
tau = 0.05


# EDC
subtract_mean <- function(x) {
  x - mean(x)
}


# EDC
EDC_Index_Simp = data.frame(date = return[winsize:nrow(return),1], EDC_simp = NA)
# nstart = 1
nstart = 1

# for (i in c(winsize : lengthfull)) {
for (i in c(winsize : lengthfull)) {
  
  temp_return = return[(i-winsize) : i,-1]
  mean_return = colMeans(temp_return)
  min_mean =  apply(temp_return, 2, subtract_mean)
  min_mean[min_mean>0] = 0
  
  Temp_EDC <- matrix(0, nrow = ncol(temp_return), ncol = ncol(temp_return))
  for (k in c(1:ncol(temp_return))) {
    for (j in c(1:ncol(temp_return))){
      if (k != j & (sqrt(sum(min_mean[,k]^2))*sqrt(sum(min_mean[,j]^2))) > 0 
          & !is.na(sum(min_mean[,k]*min_mean[,j]))){
        Temp_EDC[k,j] = sum(min_mean[,k]*min_mean[,j])/(sqrt(sum(min_mean[,k]^2))*sqrt(sum(min_mean[,j]^2)))
      }
    }
  }
  
  EDC_Index_Simp[nstart,c('EDC_simp')] = mean(Temp_EDC)
  
  print(paste0(nstart, '/', lengthfull))
  nstart = nstart + 1
}

write.csv(EDC_Index_Simp, paste0(wdir, "/MethodAdd/EDC_Index_Simple_", 
                       save_date, ".csv"), quote = FALSE) 


# EDH

CVaR_Matrix = return
CVaR_Matrix[, -1] = NA

fun_cvar <- function(a){
  ES = - cvar::ES(a, 0.05,dist.type = "cdf")
  return(ES)
}
# generate 
for (i in (winsize : lengthfull)) {
  a = return[(i-winsize) : i,]
  # VaR_norm = qnorm(tau,mean=mean(a),sd=sd(a))
  # lambda = pnorm(VaR_norm)
  # MC_CVaR_norm = mean(a[a <= VaR_norm]) 

  CVaR_norm <- apply(a[, -1], 2, fun_cvar)
  CVaR_Matrix[,-1] = CVaR_norm

}

write.csv(CVaR_Matrix, paste0(wdir, "/MethodAdd/CVaR_",save_date,".csv"), quote = FALSE)
DeltaCVaR_Matrix = CVaR_Matrix
DeltaCVaR_Matrix[,-1] = NA
DeltaCVaR_Matrix[-1,-1] =  sapply(CVaR_Matrix[,-1], diff)

# start the moving window VaR prediction, store the predict values
Beta = CVaR_Matrix[(winsize : nrow(CVaR_Matrix)),]
Beta[,-1] = NA

ticker = as.numeric(gsub("[^0-9]", "", return[,1]))

for (i in (200 + 2) : nrow(return)) {
  temp_return = return[(i-200):i, -1]
  temp_DeltaCVaR = DeltaCVaR_Matrix[(i-200):i,-1]
  temp_Beta  <- matrix(0, nrow = nrow(temp_return), ncol = ncol(temp_return))
  # standardize macro state variables
  for (k in c(1:ncol(temp_return))) {
    n_zero = length(which(temp_return[,k] == 0))
    if (n_zero < 1/3*nrow(temp_return)){
      fit = lm(temp_return[,k] ~ as.matrix(temp_DeltaCVaR[,-k]))
      temp_Beta[k, ] = fit$coefficient
    }else{
      temp_Beta[k, ] = 0
    }
  }
  
  write.csv(temp_Beta, paste0(wdir, "/MethodAdd/EDC_Beta/EDC_matix_", 
                                    ticker[i], ".csv"), quote = FALSE) 
 
}


# central index of EDC
input_path = paste0(wdir, "MethodAdd/EDC_Beta/")
file_list = list.files(input_path)
file_list = file_list[file_list!="Fixed"]
dates = as.character(str_first_number(file_list), format = "%Y%m%d")
# dates = as.Date(dates, format = "%Y%m%d")
N = length(file_list)

EDC = data.frame(date = dates, EDC_M = NA)
nstart = 1
for (i in 1 : N){
  data = as.matrix(read.csv(paste0(input_path, file_list[i]), row.names = 1))
  EDC[i,c('EDC_M')] = mean(data)
}

write.csv(EDC, paste0(wdir, "/MethodAdd/EDC_Index_", 
                      save_date, ".csv"), quote = FALSE) 

# BEKK GARCH
# BEKKs: An R Package for Estimation of Conditional Volatility of Multivariate Time Series

# install.packages('BEKKs',type = "binary")
library(BEKKs)

wdir_lambda = '/Users/ruting/Documents/Github/FRM-master_Rui_back/FRM_Quantlet/FRM_All/Output/Crypto'
FRM_5P  = data.frame(read.csv(paste0(wdir_lambda,'/Lambda/lambdas_wide.csv')))

FRM_Mean =colMeans(FRM_5P[,-1])
FRM_Mean = data.frame(FRM_Mean)

FRM_Mean = data.frame(coin = rownames(FRM_Mean), FRM = FRM_Mean[,1])
FRM_Mean = FRM_Mean[order(FRM_Mean$FRM, decreasing = TRUE),]
col = FRM_Mean$coin[1:4]

# top FRM mean
# col = c('BTC','ADA','ETH', 'BCH', 'XLM','XRP','BNB','XMR')
BEKK = data.frame(date = return[63:nrow(return),1], BEKK_A = NA, BEKK_G = NA)
nstart = 1
for (i in 63 : nrow(return)) {
  temp_return = as.matrix(return[(i-62):i, col])
  judge = temp_return
  judge[judge!=0] = 1
  
  select = colSums(judge)
  keep = temp_return[,which(select > 2/3 * nrow(judge))]

  obj_spec <- bekk_spec()
  x <- bekk_fit(obj_spec, keep, QML_t_ratios = TRUE, max_iter = 50)
  
  BEKK[nstart,c('BEKK_A','BEKK_G')] = c(mean(x$A), mean(x$G))
  
  print(paste0(i, '/', nrow(return)))
  nstart = nstart + 1

  
}
write.csv(BEKK, paste0(wdir, "/MethodAdd/BEKK_Index_", 
                            save_date, ".csv"), quote = FALSE) 


# matrix
# active data for each day, indicator for A, B, G
# 
# obj_spec <- bekk_spec(model = list(type = "bekk", asymmetric = TRUE))
# 
# x1 <- bekk_fit(obj_spec, StocksBonds, QML_t_ratios = FALSE, max_iter = 50, crit = 1e-9)
# data = as.matrix(return[500:1000,c('BTC','ETH',  'XLM','XRP','XMR')])

# obj_spec <- bekk_spec(model = list(type = "bekk", asymmetric = TRUE))
# obj_spec <- bekk_spec(model = list(type = "dbekk", asymmetric = FALSE))
# obj_spec <- bekk_spec(model = list(type = "sbekk", asymmetric = TRUE))


# backtesting

