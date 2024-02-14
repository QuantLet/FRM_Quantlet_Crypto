# Test FRM on var Sharpe ratio


rm(list = ls(all = TRUE))

libraries = c("iml","ggplot2", "data.table", "igraph","timeDate", "stringr", "graphics","magick", "scales", "tidyr", "zoo", "foreach", "doParallel",
              "xgboost","shapr","randomForest", "rpart", "quantreg", "readxl","dplyr", "xlsx", "psych","qgraph", "gganimate","av",
              "gifski", "strex","matrixStats","tools","Hmisc","vars","aTSA","quantreg","rapport","sjmisc","haven","foreign")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

wdir = "/Users/ruting/Documents/Github/FRM_Crypto_Github/FRM_All"

setwd(wdir)
channel = "Crypto"
date_start_source = 20141128
date_end_source =  20220511
save_date = 20230514


libraries = c("purrr","stringr","rfPermute","haven","sandwich","lmtest","nnet","rms","grf","varImp", "sandwich", "lmtest", "Hmisc", "ggplot2", "readxl", "plm", "psych", "sjmisc","xlsx","RODBC","stringr")
lapply(libraries, function(x) if((!x %in% installed.packages())){
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

input_path = paste0("Input/", channel, "/", date_start_source, "-", date_end_source)

# gen big data: frm, covar, var, financial report information
# FI characteristics
# leverage: asset/ equity
# maturity: book asset / (short term debt - short-term investments - cash)
# size: log(total market value / cross-sectional average of market value)
# boom:  decile of market-to-book ratio

fun_loc = function(x){
  loc = which(f$Stkcd == x)
  return(loc)
}

# volatility 63 days
mktcap = read.csv(file = paste0(input_path, "/", channel, "_Mktcap_", 
                                date_end_source, ".csv"), header = TRUE) %>% data.frame()
stock_prices = read.csv(file = paste0(input_path, "/", channel, "_Price_", 
                                      date_end_source, ".csv"), header = TRUE)%>% data.frame()
macro = read.csv(file = paste0(input_path, "/", channel, "_Macro_", 
                               date_end_source, ".csv"), header = TRUE)%>% data.frame()
crix = read.csv(file = paste0(input_path, "/vcrix.csv"), header = TRUE)%>% data.frame()
crix = crix[,-c(1,2)]


# read FRM
FRM_5P  = data.frame(read.csv(paste0(wdir,'/Output/Crypto/Lambda/FRM_Crypto_index.csv')))
colnames(FRM_5P)[2] = 'FRM_5P'

FRM_25P  = data.frame(read.csv(paste0(wdir,'/Output/Crypto/Lambda/Quantiles/q25_lambda.csv')))
colnames(FRM_25P)[2] = 'FRM_25P'

FRM_50P  = data.frame(read.csv(paste0(wdir,'/Output/Crypto/Lambda/Quantiles/q50_lambda.csv')))
colnames(FRM_50P)[2] = 'FRM_50P'

# calculate weighted index
all_prices = stock_prices
all_prices[, -1] = sapply(all_prices[, -1], as.numeric)
all_return = all_prices[-1, ]
all_return[,-1] = 0

all_return[, -1] = 
  as.data.frame(diff(log(as.matrix(all_prices[, -1])))) 
all_return[is.na(all_return)] = 0


for (i in c(2:ncol(mktcap))){
  Temp = mktcap[,c(1,i)]
  colnames(Temp)[2] = 'mktcap'
  Temp$Type = colnames(mktcap)[i]
  if (i == 2){
    Output_mktcap = Temp
  }else{
    Output_mktcap = rbind(Output_mktcap, Temp)
  }
}


# Sharpe Ratio
std_lag <- function(lag, all_return){
  all_std = all_return
  all_std[,-1] = NA
  for (i in c(lag:nrow(all_std))){
    all_std[i,-1] =  apply(all_return[(i-lag + 1):i,-1], 2, sd)
  }
  return(all_std)
}

all_mean_lag <- function(lag, all_return){
  all_mean = all_return
  all_mean[,-1] = NA
  for (i in c(lag:nrow(all_mean))){
    all_mean[i,-1] =  apply(all_return[(i-lag+1):i,-1], 2, mean)
  }
  return(all_mean)
}

all_Sharpe_lag <- function(lag, all_return){
  all_std <- std_lag(lag, all_return)
  all_mean <- all_mean_lag(lag, all_return)
  
  all_Sharpe = all_mean
  all_Sharpe[,-1] = all_mean[,-1] / all_std[,-1]
  all_Sharpe = all_Sharpe[-c(1:(lag-1)),]
  
  all_Sharpe_vol = all_Sharpe
  for (i in c(lag:nrow(all_Sharpe_vol))){
    all_Sharpe_vol[i,-1] =  apply(all_Sharpe[(i-lag+1):i,-1], 2, sd)
  }
  
  all_Sharpe_vol = all_Sharpe_vol[-c(1:(lag-1)),]
  
  for (i in c(2:ncol(all_Sharpe_vol))){
    Temp = all_Sharpe_vol[,c(1,i)]
    colnames(Temp)[2] = paste0('Sharpe_vol_',lag)
    Temp$Type = colnames(all_Sharpe_vol)[i]
    if (i == 2){
      Output = Temp
    }else{
      Output = rbind(Output, Temp)
    }
  }
  return(Output)
}

Sharpe_vol_10 = all_Sharpe_lag(10, all_return)
Sharpe_vol_25 = all_Sharpe_lag(25, all_return)
Sharpe_vol_63 <- all_Sharpe_lag(63, all_return)
Sharpe_vol_110<- all_Sharpe_lag(110, all_return)

# output = list(all_Sharpe_vol = all_Sharpe_vol, all_Sharpe = all_Sharpe) 
Output = merge(Sharpe_vol_10, Sharpe_vol_25, all.x = TRUE)
Output = merge(Output, Sharpe_vol_63, all.x = TRUE)
Output = merge(Output, Sharpe_vol_110, all.x = TRUE)

Output = merge(Output, Output_mktcap, all.x = TRUE, all.y = FALSE)
Output = merge(Output, macro, all.x = TRUE, all.y = FALSE)

Output = merge(Output, FRM_5P,  all.x = TRUE, all.y = FALSE)
Output = merge(Output, FRM_25P,  all.x = TRUE, all.y = FALSE)
Output = merge(Output, FRM_50P,  all.x = TRUE, all.y = FALSE)

colnames(Output)[((ncol(Output)) - 7):(ncol(Output)-3)] = c('BVIndex','CVIXIndex','DXYCurncy','SPXIndex','VIXIndex')

write_dta(Output,   paste0(wdir, "/Output/Crypto/Add_EfficiencyTest/Sharpe/Sharpe_volatility.dta"), version = 14)

