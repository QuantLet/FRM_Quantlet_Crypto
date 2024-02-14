# 0211: overlapping problem 


rm(list = ls(all = TRUE))

libraries = c("SHAPforxgboost","shapr","xgboost","readxl","iml","ggplot2", "data.table", "igraph","timeDate", "stringr", "graphics","magick", "scales", "tidyr", "zoo","xts", "foreach", "doParallel",
              "xgboost","shapr","randomForest", "rpart", "quantreg", "readxl","dplyr", "xlsx", "psych","qgraph", "gganimate","av",
              "gifski", "strex","matrixStats","tools","Hmisc","vars","aTSA","quantreg","rapport","sjmisc","haven","foreign","e1071")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

wdir = "/Users/ruting/Documents/Github/FRM_Crypto_Github/FRM_All"
save =  paste0(wdir, "/Output/Crypto/Add_EfficiencyTest")

setwd(wdir)
channel = "Crypto"
date_start_source = 20141128
date_end_source =  20220511
save_date = 20230514

input_path = paste0("Input/", channel, "/", date_start_source, "-", date_end_source)

# data
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

# calculate weighted index
all_prices = stock_prices
all_prices[, -1] = sapply(all_prices[, -1], as.numeric)
# write.csv(all_prices, paste0(wdir, "/MethodAdd/Price_",save_date,".csv"), quote = FALSE)

all_return = all_prices[-1, ]
all_return[,-1] = 0

all_return[, -1] = 
  as.data.frame(diff(log(as.matrix(all_prices[, -1])))) 
all_return[is.na(all_return)] = 0
# write.csv(all_return, paste0(wdir, "/MethodAdd/Return_",save_date,".csv"), quote = FALSE)


mktcap[, -1] = sapply(mktcap[, -1], as.numeric)
mktcap_weight = mktcap
mktcap_weight[,-1] = 0
mktcap_weight[,-1] = mktcap[,-1]/rowSums(mktcap[, -1],na.rm = TRUE)
mktcap_weight = mktcap_weight[-1,]

all_return_temp = all_return
all_return_temp[,-1]= all_return_temp[,-1]*mktcap_weight[,-1]
all_return_temp[is.na(all_return_temp)] = 0
Fin_Index = data.frame(date = all_return_temp$date, Return = rowSums(all_return_temp[, -1]))


Fun_Vola = function (data){

  vola = NA
  for (i in c(63 : length(data)) ){
    vola = c(vola, sd(data[(i-62) : i]))
  }
  vola = vola[-1]
  
  return(vola)
}

# Fin_Index$MKVola = apply(Fin_Index$Return, width = 63, FUN = sd, fill = NA, align = "right") 
Fin_Index$MKVola = NA
Fin_Index$MKVola[63:nrow(Fin_Index)] <- Fun_Vola(Fin_Index$Return)
# Fin_Index = merge(Fin_Index,crix, all.x = TRUE)


Test_Fin_Index = Fin_Index
Test_Fin_Index$MKVola_L_10 = NA
Test_Fin_Index$MKVola_L_25 = NA
Test_Fin_Index$MKVola_L_110 = NA


Test_Fin_Index[11:nrow(Test_Fin_Index), 'MKVola_L_10'] = Test_Fin_Index[1:(nrow(Test_Fin_Index)-10), 'MKVola']
Test_Fin_Index[26:nrow(Test_Fin_Index), 'MKVola_L_25'] = Test_Fin_Index[1:(nrow(Test_Fin_Index)-25), 'MKVola']
Test_Fin_Index[64:nrow(Test_Fin_Index), 'MKVola_L_63'] = Test_Fin_Index[1:(nrow(Test_Fin_Index)-63), 'MKVola']
Test_Fin_Index[111:nrow(Test_Fin_Index), 'MKVola_L_110'] = Test_Fin_Index[1:(nrow(Test_Fin_Index)-110), 'MKVola']


# read FRM
FRM_5P  = data.frame(read.csv(paste0(wdir,'/Output/Crypto/Lambda/FRM_Crypto_index.csv')))
colnames(FRM_5P)[2] = 'FRM_5P'

FRM_25P  = data.frame(read.csv(paste0(wdir,'/Output/Crypto/Lambda/Quantiles/q25_lambda.csv')))
colnames(FRM_25P)[2] = 'FRM_25P'

FRM_50P  = data.frame(read.csv(paste0(wdir,'/Output/Crypto/Lambda/Quantiles/q50_lambda.csv')))
colnames(FRM_50P)[2] = 'FRM_50P'


# read BGVAR
BGVAR  = data.frame(read_excel(paste0(wdir,'/MethodAdd/BGVAR_20230514.xlsx')))
colnames(BGVAR)[1] = colnames(FRM_5P)[1]

# read GDC
GDC  = data.frame(read_excel(paste0(wdir,'/MethodAdd/rolling_GDC_total_20230514.xlsx')))
colnames(GDC)[1] = colnames(FRM_5P)[1]
GDC[,1] = as.character(GDC[,1])
colnames(GDC)[2] = 'GDC'
# GDC$date = paste0(substr(GDC$date,1,4), substr(GDC$date,6,7), substr(GDC$date,9,10))
# GDC$date = as.numeric(GDC$date)

# read PCA
PCA  = data.frame(read_excel(paste0(wdir,'/MethodAdd/PCA_20230514.xlsx')))
PCA = PCA[,-1]
PCA[,1] = as.character(PCA[,1])
colnames(PCA)[1] = colnames(FRM_5P)[1]
# PCA$date = paste0(substr(PCA$date,1,4), substr(PCA$date,6,7), substr(PCA$date,9,10))
# PCA$date = as.numeric(PCA$date)

# read DY
DY  = data.frame(read_excel(paste0(wdir,'/MethodAdd/DY_TCI.xlsx')))
DY = DY[,-1]
colnames(DY)[1] = colnames(FRM_5P)[1]

# read MV
MVWealth  = read.csv(paste0(wdir,'/MethodAdd/tables/PortWealth.csv'), header = TRUE)
MVWealth = MVWealth[,c('date','Return_MV')]
MVWealth$date = as.character(as.Date(as.character(MVWealth[,1]),  "%Y%m%d"))

Index = merge(FRM_5P, FRM_25P, by = intersect(names(FRM_5P), names(FRM_25P)),all = FALSE,sort = TRUE)
Index = merge(Index, FRM_50P, by = intersect(names(Index), names(FRM_50P)),all = FALSE,sort = TRUE)
Index = merge(Index, BGVAR, by = intersect(names(Index), names(BGVAR)),all = FALSE,sort = TRUE)
Index = merge(Index, GDC, by = intersect(names(Index), names(GDC)),all = FALSE,sort = TRUE)
Index = merge(Index, PCA, by = intersect(names(Index), names(PCA)),all = FALSE,sort = TRUE)
Index = merge(Index, DY, by = intersect(names(Index), names(DY)),all = FALSE,sort = TRUE)
Index = merge(Index, Fin_Index, by = intersect(names(Index), names(Fin_Index)),all = FALSE,sort = TRUE)

Index_MVWealth = merge(MVWealth, Index, by = intersect(names(MVWealth), names(Index)),all.x = TRUE,sort = TRUE)
Index_MVWealth = Index_MVWealth[63:nrow(MVWealth),]
# x_type = c('MKVola', 'Return')
select = c('FRM_5P','FRM_25P','FRM_50P','DAG','GDC','PCA_1','TCI')

# continuous: from 1day to 110day

Fun_Vola_dynamic = function (data,iLag){
  
  vola = NA
  for (i in c(iLag : length(data)) ){
    vola = c(vola, sd(data[(i-iLag+1) : i]))
  }
  vola = vola[-1]
  
  return(vola)
}



Fun_ROOS_fixed = function (Index, i_type, select, horizon){
  R_oos_out = matrix( nrow = length(horizon), ncol = length(select)+1)
  
  R_oos_out[,1] = horizon
  colnames(R_oos_out) = c('lagDay', paste0('Roos_', select))
  nstart = 1
  
  for (iLag in horizon){
    
    if (i_type == 'Return'){
      Testdata = Index
    }else if(i_type == 'MKVola'){
      Testdata = Index
      Testdata$MKVola = NA
      Testdata$MKVola[63:nrow(Testdata)] <- Fun_Vola_dynamic(Testdata$Return, 63)
      # Testdata$MKVola[iLag:nrow(Testdata)] <- Fun_Vola_dynamic(Testdata$Return, 21)
    }else{
      Testdata = Index
      Testdata$MVVola = NA
      Testdata$MVVola[63:nrow(Testdata)] <- Fun_Vola_dynamic(Testdata$Return_MV, 63)
    }
    
    for (j in select){
      temp_reg = cbind(Testdata[(iLag+1):nrow(Index), i_type], Testdata[1:(nrow(Index)-iLag), j])
      colnames(temp_reg) = c('y','x')
      
      temp_reg = data.frame(temp_reg)
      temp_reg = temp_reg[!is.na(temp_reg[,'x']),]
      
      for (i in c(63:(nrow(temp_reg)-1))){
        
        fit = lm(y ~ x, temp_reg[(i-62):(i-1), ])
        
        temp_reg[i, paste0('p_vol_',j)] = fit$coefficients[1] + fit$coefficients[2]*temp_reg$x[i]
        temp_reg[i, paste0('diff_s_',j)] = (temp_reg$y[i]-temp_reg[i,paste0('p_vol_',j)])^2
        # temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y))^2
        temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y[63:nrow(temp_reg)]))^2
      }
      
      R_oos_out[nstart, paste0('Roos_', j)] = 1 - sum(temp_reg[,paste0('diff_s_',j)], na.rm = TRUE)/sum(temp_reg[,paste0('diff_m_',j)], na.rm = TRUE)
      
    }
    
    nstart = nstart + 1
    print(paste0('finish ',iLag, ' Type ', i_type))
  }
  
  if ('MKVola_L_10' %in% select){
    save_csv =  paste0(save, "/Self_Prediction_Continuous",i_type,"_Fixed63_ROO_",save_date,".csv")
    save_csv_mean =  paste0(save, "/Self_Mean_Continuous",i_type,"_Fixed63_ROO_",save_date,".csv")
  }else{
    save_csv =  paste0(save, "/Prediction_Continuous",i_type,"_Fixed63_ROO_",save_date,".csv")
    save_csv_mean =  paste0(save, "/Mean_Continuous",i_type,"_Fixed63_ROO_",save_date,".csv")
  }
  write.csv(R_oos_out, save_csv, quote = FALSE)
  Mean_Roos = rbind(colMeans(R_oos_out[,-1]),colSds(R_oos_out[,-1]))
  write.csv(Mean_Roos, save_csv_mean, quote = FALSE)
  
}


Fun_R2_fixed = function (Index, i_type, select, horizon){
  R2 = matrix( nrow = length(horizon), ncol = length(select)+1)
  
  R2[,1] = horizon
  colnames(R2) = c('lagDay', paste0('R2_', select))
  nstart = 1
  
  for (iLag in horizon){
    
    if (i_type == 'Return'){
      Testdata = Index
    }else if(i_type == 'MKVola'){
      Testdata = Index
      Testdata$MKVola = NA
      Testdata$MKVola[63:nrow(Testdata)] <- Fun_Vola_dynamic(Testdata$Return, 63)
      # Testdata$MKVola[iLag:nrow(Testdata)] <- Fun_Vola_dynamic(Testdata$Return, 21)
    }else{
      Testdata = Index
      Testdata$MVVola = NA
      Testdata$MVVola[63:nrow(Testdata)] <- Fun_Vola_dynamic(Testdata$Return_MV, 63)
    }
    
    
    for (j in select){
      temp_reg = cbind(Testdata[(iLag+1):nrow(Index), i_type], Testdata[1:(nrow(Index)-iLag), j])
      colnames(temp_reg) = c('y','x')
      temp_reg = data.frame(temp_reg)
      temp_reg = temp_reg[!is.na(temp_reg[,'x']),]
      
      for (i in c(63:(nrow(temp_reg)-1))){
        
        fit = lm(y ~ x, temp_reg[(i-61):i, ])
        
        temp_reg[i, paste0('p_vol_',j)] = fit$coefficients[1] + fit$coefficients[2]*temp_reg$x[i]
        temp_reg[i, paste0('diff_s_',j)] = (temp_reg$y[i]-temp_reg[i,paste0('p_vol_',j)])^2
        # temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y))^2
        temp_reg[i, paste0('diff_m_',j)] = (temp_reg$y[i]-mean(temp_reg$y[63:nrow(temp_reg)]))^2
      }
      
      R2[nstart, paste0('R2_', j)] = 1 - sum(temp_reg[,paste0('diff_s_',j)], na.rm = TRUE)/sum(temp_reg[,paste0('diff_m_',j)], na.rm = TRUE)
      
    }
    
    nstart = nstart + 1
    print(paste0('finish ',iLag, ' Type ', i_type))
  }
  
  if ('MKVola_L_10' %in% select){
    save_csv =  paste0(save, "/Self_Prediction_Continuous",i_type,"_Fixed63_R2_",save_date,".csv")
    save_csv_mean =  paste0(save, "/Self_Mean_Continuous",i_type,"_Fixed63_R2_",save_date,".csv")
  }else{
    save_csv =  paste0(save, "/Prediction_Continuous",i_type,"_Fixed63_R2_",save_date,".csv")
    save_csv_mean =  paste0(save, "/Mean_Continuous",i_type,"_Fixed63_R2_",save_date,".csv")
  }
  write.csv(R2, save_csv, quote = FALSE)
  Mean_R2 = rbind(colMeans(R2[,-1]),colSds(R2[,-1]))
  write.csv(Mean_R2, save_csv_mean, quote = FALSE)
}


# horizon = c(10:110)
horizon = c(5:110)
Fun_ROOS(Index, 'MKVola', select, horizon)
Fun_R2(Index, 'MKVola', select, horizon)

Fun_ROOS_fixed(Index, 'MKVola', select, horizon)
Fun_R2_fixed(Index, 'MKVola', select, horizon)

Fun_ROOS(Index = Index_MVWealth, 'MVVola', select, horizon)
Fun_ROOS_fixed(Index = Index_MVWealth, 'MVVola', select, horizon)

Fun_ROOS(Index, 'Return', select, horizon = c(10,25,63,110))
Fun_R2(Index, 'Return', select, horizon = c(10,25,63,110))
Fun_ROOS_fixed(Index, 'Return', select, horizon = c(10,25,63,110))
Fun_R2_fixed(Index, 'Return', select, horizon = c(10,25,63,110))

Fun_ROOS_fixed(Index = Test_Fin_Index, 'MKVola', select = c('MKVola_L_10','MKVola_L_25', 'MKVola_L_63', 'MKVola_L_110'), horizon = c(0,1))
Fun_R2_fixed(Index = Test_Fin_Index, 'MKVola', select = c('MKVola_L_10','MKVola_L_25','MKVola_L_63','MKVola_L_110'), horizon = c(0,1))


# Output 
# Portfolio Prediction

Fun_ROOS_Performance <- function(i_type, fix){
  if (fix == FALSE){
    R_oos_out  = data.frame(read.csv(paste0(save, "/Prediction_Continuous",i_type,"_vol_ROO_",save_date,".csv")))
  }else{
    R_oos_out  = data.frame(read.csv(paste0(save, "/Prediction_Continuous",i_type,"_Fixed63_ROO_",save_date,".csv")))
  }
 
  
  # within 10 days 1 month 5weeks 3 months 5months
  Mean_horizon = c(10, 25, 63, 110)
  Roo_mean =  matrix(nrow = length(Mean_horizon), ncol = ncol(R_oos_out)-1)
  Roo_mean[,1] = Mean_horizon
  colnames(Roo_mean) = c('Horizon',colnames(R_oos_out)[3:ncol(R_oos_out)]) 
  # for (i in c(1:nrow(Roo_mean))){
  #   Roo_mean[i,-1] = colMeans(R_oos_out[2:which(R_oos_out$lagDay == Mean_horizon[i]),3:ncol(R_oos_out)])
  #   
  # }
  for (i in c(1:nrow(Roo_mean))){
    Roo_mean[i,-1] = as.matrix(R_oos_out[R_oos_out$lagDay == Mean_horizon[i],3:ncol(R_oos_out)])

  }
  
  if (fix == FALSE){
    write.csv(Roo_mean, paste0(save, "/R2/Table_Output_",i_type,"_vol_ROO_",save_date,".csv"), quote = FALSE)
  }else{
    write.csv(Roo_mean, paste0(save, "/R2/Table_Output_",i_type,"_Fixed63_ROO_",save_date,".csv"), quote = FALSE)
  }
  
  
}

Fun_R2_Performance <- function(i_type, fix){
  
  if (fix == FALSE){
    R_oos_out  =  data.frame(read.csv(paste0(save, "/Prediction_Continuous",i_type,"_R2_",save_date,".csv")))
  }else{
    R_oos_out  = data.frame(read.csv(paste0(save, "/Prediction_Continuous",i_type,"_Fixed63_R2_",save_date,".csv")))
  }

  # within 10 days 1 month 5weeks 3 months 5months
  Mean_horizon = c(10, 25, 63, 110)
  Roo_mean =  matrix(nrow = length(Mean_horizon), ncol = ncol(R_oos_out)-1)
  Roo_mean[,1] = Mean_horizon
  colnames(Roo_mean) = c('Horizon',colnames(R_oos_out)[3:ncol(R_oos_out)]) 
  # for (i in c(1:nrow(Roo_mean))){
  #   Roo_mean[i,-1] = colMeans(R_oos_out[2:which(R_oos_out$lagDay == Mean_horizon[i]),3:ncol(R_oos_out)])
  #   
  # }
  for (i in c(1:nrow(Roo_mean))){
    Roo_mean[i,-1] = as.matrix(R_oos_out[R_oos_out$lagDay == Mean_horizon[i],3:ncol(R_oos_out)])
    
  }
  
  if (fix == FALSE){
    write.csv(Roo_mean, paste0(save, "/R2/Table_Output_",i_type,"_R2_",save_date,".csv"), quote = FALSE)
  }else{
    write.csv(Roo_mean, paste0(save, "/R2/Table_Output_",i_type,"_Fixed63_R2_",save_date,".csv"), quote = FALSE)
  }
}


Fun_ROOS_Performance('MVVola',fix = TRUE)
Fun_ROOS_Performance('MKVola',fix = TRUE)

Fun_R2_Performance('MKVola',fix = TRUE)
Fun_ROOS_Performance('Return',fix = TRUE)
Fun_R2_Performance('Return',fix = TRUE)


# ML prediction SVR, rf 
# SHAP value 
# prediction error: MAE RMSE
# https://github.com/liuyanguu/SHAPforxgboost

Fun_ROOS_fixed_ML <-function(iLag, Index,i_type){
  R_oos = data.frame(matrix(0,1,4))
  colnames(R_oos) = c('Risk','R_oos_xgboost','MAE_xgboost','RMSE_xgboost')
  
  if (i_type == 'Return'){
    Testdata = Index
  }else if(i_type == 'MKVola'){
    Testdata = Index
    Testdata$MKVola = NA
    Testdata$MKVola[63:nrow(Testdata)] <- Fun_Vola_dynamic(Testdata$Return, 63)
    # Testdata$MKVola[iLag:nrow(Testdata)] <- Fun_Vola_dynamic(Testdata$Return, 21)
  }else{
    Testdata = Index
    Testdata$MVVola = NA
    Testdata$MVVola[63:nrow(Testdata)] <- Fun_Vola_dynamic(Testdata$Return_MV, 63)
  }
  
  
  temp_reg = cbind(Testdata[(iLag+1):nrow(Index), i_type], Testdata[1:(nrow(Index)-iLag), select])
  temp_reg = temp_reg[complete.cases(temp_reg),]
  
  colnames(temp_reg)[1:8] = c('y','x1','x2','x3','x4','x5','x6','x7')
  
  
  
  for (i in c(63:(nrow(temp_reg)-1))){
    
    trainval = temp_reg[(i-62):(i-1),c('x1','x2','x3','x4','x5','x6','x7')]
    trainval = sapply(trainval, as.numeric)
    trainval = as.matrix(trainval) 
    
    xgboost_model <- xgboost(data = trainval,
                             label = as.matrix(temp_reg[(i-63+1):(i-1), 'y']),
                             nround = 20,
                             verbose = FALSE)
    prediction_xgboost <- predict(xgboost_model, newdata = as.matrix(temp_reg[i, c('x1','x2','x3','x4','x5','x6','x7')]))
    
    temp_reg[i, 'PreXgboost'] = prediction_xgboost
    temp_reg[i, 'diffXgboost'] = temp_reg$y[i]-temp_reg[i,'PreXgboost']
    temp_reg[i, 'diffXgboost_s'] = (temp_reg[i, 'diffXgboost'])^2
    
    temp_reg[i, 'diff_m'] = (temp_reg$y[i]-mean(temp_reg$y[63:nrow(temp_reg)]))^2
  }
  
  R_oos[2] = 1 - sum(temp_reg[,'diffXgboost_s'], na.rm = TRUE)/sum(temp_reg[,'diff_m'], na.rm = TRUE)
  R_oos[3] = mean(abs(temp_reg[,'diffXgboost']),na.rm = TRUE)
  R_oos[4] = sqrt(mean(temp_reg[,'diffXgboost_s'],na.rm = TRUE))
  
  write.csv(R_oos, paste0(save, "/Shapley/MLPrediction_",i_type,"_lag_",iLag,"_Fixed63_ROO_",save_date,".csv"), quote = FALSE)
  
}

Fun_Shapley_fixed_ML <-function(iLag, Index,i_type){
  
  if (i_type == 'Return'){
    Testdata = Index
  }else if(i_type == 'MKVola'){
    Testdata = Index
    Testdata$MKVola = NA
    Testdata$MKVola[63:nrow(Testdata)] <- Fun_Vola_dynamic(Testdata$Return, 63)
    # Testdata$MKVola[iLag:nrow(Testdata)] <- Fun_Vola_dynamic(Testdata$Return, 21)
  }else{
    Testdata = Index
    Testdata$MVVola = NA
    Testdata$MVVola[63:nrow(Testdata)] <- Fun_Vola_dynamic(Testdata$Return_MV, 63)
  }
  
  
  temp_reg = cbind(Testdata[(iLag+1):nrow(Index), i_type], Testdata[1:(nrow(Index)-iLag), select])
  colnames(temp_reg)[1] = i_type
  temp_reg = temp_reg[complete.cases(temp_reg),]
  trainval = as.matrix(temp_reg[,-1])
  
  
  param_list <- list(objective = "reg:squarederror",  # For regression
                     eta = 0.02,
                     max_depth = 100,
                     gamma = 0.01,
                     subsample = 0.8,
                     colsample_bytree = 0.86)
  
  for (i in c(1:500)){
    mod <- xgboost::xgboost(data = trainval, 
                            label = as.matrix(temp_reg[, i_type]), 
                            params = param_list, nrounds = 200,
                            verbose = FALSE,
                            early_stopping_rounds = 8)
    shap_long <- shap.prep(xgb_model = mod, X_train = trainval)
    Shapley_Mean_Temp = unique(shap_long[,c('variable','mean_value')])
    colnames(Shapley_Mean_Temp)[2] = paste0('mean_value_',i)
    if (i == 1){
      Shapley_Mean = Shapley_Mean_Temp
    }else{
      Shapley_Mean = merge(Shapley_Mean, Shapley_Mean_Temp)
    }
  }
  Shapley_Mean$mean_value = rowMeans(Shapley_Mean[,-1])
  Shapley_Mean$variable = as.character(Shapley_Mean$variable)
  Shapley_Mean$variable[Shapley_Mean$variable == 'FRM_5P'] = 'FRM 5%'
  Shapley_Mean$variable[Shapley_Mean$variable == 'FRM_25P'] = 'FRM 25%'
  Shapley_Mean$variable[Shapley_Mean$variable == 'FRM_50P'] = 'FRM 50%'
  Shapley_Mean$variable[Shapley_Mean$variable == 'TCI'] = 'Total Connectedness'
  Shapley_Mean$variable[Shapley_Mean$variable == 'DAG'] = 'Bayesian Graphical VAR'
  Shapley_Mean$variable[Shapley_Mean$variable == 'PCA_1'] = 'Principal Components'
  Shapley_Mean$variable[Shapley_Mean$variable == 'GDC'] = 'Granger Causality'
  
  Shapley_Mean$Shapley_Mean = round(Shapley_Mean$mean_value,5)
  
  png(paste0(save, "/Shapley/SHAP_fixed63_",i_type,"_lag_",iLag,".png"), width = 900, height = 600, bg = "transparent")
  
  print(ggplot(Shapley_Mean,aes(x= reorder(variable, Shapley_Mean),y=Shapley_Mean, fill = variable))+geom_bar(stat="identity", width=0.5)+
          coord_flip()+
          geom_text(aes(label=Shapley_Mean), vjust=-2, size=5)+
          labs(y = "Shapley Value", size = 20)+
          labs(x = "Risk Measures", size = 20)+
          theme(axis.text.x = element_text(size =15, angle = 30, hjust = 1), 
                axis.text.y = element_text(size = 15), 
                axis.title =  element_text(size=,face = "bold"),
                panel.grid.major =element_blank(), 
                panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0.5,size = 35, face = "bold"),
                panel.background = element_rect(fill = "transparent",colour = NA),
                plot.background = element_rect(fill = "transparent",colour = NA),
                axis.line = element_line(colour = "black"),legend.position="none")
  )
  dev.off()
  
  write.csv(Shapley_Mean, paste0(save, "/Shapley/ShapleyCalData_fixed63_",i_type,"_lag_",iLag,"_",save_date,".csv"), quote = FALSE)
  
}

Fun_ROOS_fixed_ML(iLag = 10, Index, 'MKVola')
Fun_Shapley_fixed_ML(iLag = 10, Index, 'MKVola')

# Generate beta test part 
Index$MKVola_10[10:nrow(Index)] <- Fun_Vola_dynamic(Index$Return, iLag = 10)
Index$MKVola_25[25:nrow(Index)] <- Fun_Vola_dynamic(Index$Return, iLag = 25)
Index$MKVola_63[63:nrow(Index)] <- Fun_Vola_dynamic(Index$Return, iLag = 63)
Index$MKVola_110[110:nrow(Index)] <- Fun_Vola_dynamic(Index$Return, iLag = 110)

fun_lag <- function(x,lag){
  return(paste0(x,'_L',lag))
} 

pre_col_L10 = fun_lag(select, 10)
pre_col_L25 = fun_lag(select, 25)
pre_col_L63 = fun_lag(select, 63)
pre_col_L110 = fun_lag(select, 110)

pre_col = c(pre_col_L10, pre_col_L25, pre_col_L110)
Index[, pre_col] = NA

Index[11:nrow(Index), pre_col_L10] = Index[1:(nrow(Index)-10), select]
Index[26:nrow(Index), pre_col_L25] = Index[1:(nrow(Index)-25), select]
Index[64:nrow(Index), pre_col_L63] = Index[1:(nrow(Index)-63), select]
Index[111:nrow(Index), pre_col_L110] = Index[1:(nrow(Index)-110), select]


write_dta(Index, paste0(save, "/InsampleBetaTest_",save_date,".dta"))


# plot R2
save_csv =  paste0(save, "/Prediction_ContinuousMKVola_Fixed63_ROO_",save_date,".csv")
R_oos_out = data.frame(read.csv(save_csv))
png(paste0(save,  "/R2/Roos_Dynamic_MKVola.png"), width = 900, height = 600, bg = "transparent")
# Plot https://www.cnblogs.com/biostat-yu/p/13839621.html

# R_oos_out = data.frame(R_oos_out)
p=ggplot(R_oos_out, aes(x=lagDay)) +
  geom_line(aes(y = Roos_FRM_5P), color="#009933", linewidth = 0.75) +
  geom_line(aes(y = Roos_FRM_25P), color="#996600", linewidth = 0.75) + 
  geom_line(aes(y = Roos_FRM_50P), color="#33CCCC", linewidth = 0.75) +
  geom_line(aes(y = Roos_DAG), color="#FF6666", linewidth = 0.75) +
  geom_line(aes(y = Roos_GDC), color="#666666", linewidth = 0.75) +
  geom_line(aes(y = Roos_PCA_1), color="#9966CC", linewidth = 0.75) +
  geom_line(aes(y = Roos_TCI), color="magenta", linewidth = 0.75) +
  scale_x_continuous(limits = c(5, 110), breaks = seq(10,110,by=10))+
  scale_y_continuous(limits = c(0.5, 0.8), breaks = seq(-0.4,0.8,by=0.1))+
  labs(y = "Out-of-Sample R2", size = 20)+
  labs(x = "Prediction Horizon (day)", size = 20)+
  theme( panel.grid=element_blank(),
         legend.position = "none",
         axis.title =  element_text(size=,face = "bold"),
         panel.background = element_rect(fill = "transparent",colour = NA),
         plot.background = element_rect(fill = "transparent",colour = NA),
         legend.box.background = element_rect(fill = "transparent"),
         axis.line = element_line(colour = "black"),
         axis.text.x = element_text(size=14),
         axis.text.y = element_text(size=14 ) )
print(p)
dev.off()


# Plot beta's trend
Beta = read_excel( paste0(save, "/InsampleBeta/InsampleBeta_dynamic.xls")) %>% data.frame()
colnames(Beta) = c('Year','Beta','lb','ub')
Beta = Beta[Beta$Year<= 2021, ]
png(paste0(save, "/InsampleBeta/InsamplePredictFRM_5P.png"), 
    width = 900, height = 600, bg = "transparent")

ggplot(Beta, aes(x = Year)) +
  geom_segment(aes(xend = Year, y = lb, yend = ub), size = 0.75) +
  geom_point(aes(y = Beta), size = 1.5) +
  geom_line(aes(y = Beta), linewidth = 0.75) +
  scale_x_continuous(breaks = unique(Beta$Year), labels = unique(Beta$Year), expand = c(0.1, 0.11)) +
  theme(panel.background = element_rect(fill = "transparent",colour = NA),axis.line = element_line(colour = "black"),axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16))+
  labs(x = "Year",
       y = "Coefficient") 
dev.off()


