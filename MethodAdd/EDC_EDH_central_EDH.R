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

subtract_mean <- function(x) {
  x - mean(x)
}


# EDC
for (i in (winsize : lengthfull)) {
  temp_return = return[(i-winsize) : i,-1]
  mean_return = colMeans(temp_return)
  min_mean =  apply(temp_return, 2, subtract_mean)

  for (k in c(1:ncol(temp_return))) {
    a = temp_return[(1 : (i-1)),k]
    mean_a = mean(a)
    VaR_norm = qnorm(tau,mean=mean_a,sd=sd(a))
    x_u = 0
    if (temp_return[i,k] < VaR_norm){
      x_u = temp_return[i,k] - VaR_norm
    }
    # quantile_5
    

  }
  
  
  # VaR_norm = qnorm(tau,mean=mean(a),sd=sd(a))
  # lambda = pnorm(VaR_norm)
  # MC_CVaR_norm = mean(a[a <= VaR_norm]) 

}


# EDH part
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

col = c('BTC','ADA','ETH', 'BCH', 'XLM','XRP','BNB','XMR')
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
write.csv(temp_Beta, paste0(wdir, "/MethodAdd/BEKK_Index_", 
                            save_date, ".csv"), quote = FALSE) 

# Plot and correlation

cent_plot = function(cent_type, lambda) {
  cent_string = deparse(substitute(cent_type))
  png(paste0(output_path, "/Centrality/FRM_", cent_string,".png"), 
      width = 900, height = 600, bg = "transparent")
  par(mar = c(5, 4, 4, 4) + 0.3)
  # plot(lambda[-outliers], type = "l", col = "blue", xlab = "", 
  #      ylab = "FRM index", xaxt = "n", lwd = 2)
  plot(lambda, type = "l", col = "blue", xlab = "", 
       ylab = "FRM index", xaxt = "n", lwd = 2)
  par(new = TRUE)
  # plot(cent_type[-outliers], type = "l", col = "red", axes = FALSE, 
  #      xlab = "", ylab = "", xaxt = "n")
  plot(cent_type, type = "l", col = "red", axes = FALSE, 
       xlab = "", ylab = "", xaxt = "n")
  # axis(side = 4, at = pretty(range(cent_type[-outliers])))
  axis(side = 4, at = pretty(range(cent_type)))
  
  # ll = which(FRM_index$date[-outliers] %in% plot_labels)
  # ll = which(FRM_index$date %in% plot_labels)
  ll = seq(120, nrow(FRM_index), 120)
  axis(1, at = ll, labels = FRM_index$date[ll])
  mtext(paste0(gsub("_.*", "", cent_string) %>% toTitleCase, " centrality"), 
        side = 4, line = 3)
  dev.off()
}

cent_plot(outdegree_avg, FRM_index$frm)


# corr 
cormatrix = cbind(FRM_index$frm,outdegree_avg, closeness_avg, eigenvector_avg,betweenness_avg)
colnames(cormatrix)[1] = 'FRM'
corr = cor(cormatrix,  method = "pearson")
corr

p_value_matrix <- matrix(NA, nrow = ncol(cormatrix), ncol = ncol(cormatrix))

# Calculate p-values for each pair of variables
for (i in 1:(ncol(cormatrix) - 1)) {
  for (j in (i + 1):ncol(cormatrix)) {
    cor_test_result <- cor.test(cormatrix[, i], cormatrix[, j], method = "pearson")
    p_value_matrix[i, j] <- p_value_matrix[j, i] <- cor_test_result$p.value
  }
}

write.csv(corr,paste0(output_path,"/Centrality/Centralilty_FRM_Corr.csv"),quote = FALSE)
write.csv(p_value_matrix,paste0(output_path,"/Centrality/Centralilty_FRM_CorrP.csv"),quote = FALSE)


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

