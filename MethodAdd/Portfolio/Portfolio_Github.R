# update: select the largest pool. rolling drop blank ones


rm(list = ls(all = TRUE))
graphics.off()

wdir = "/Users/ruting/Documents/Github/FRM_Crypto_Github/FRM_All/MethodAdd/"
wdir_ori = "/Users/ruting/Documents/Github/FRM_Crypto_Github/FRM_All/"
setwd(wdir)


# Path_load = paste0(root,"working_data/" ,date)
Path_save_xls = paste0(wdir_ori,'Output/Crypto/Add_PortfolioTest/')
Path_save_fig = paste0(wdir_ori,'Output/Crypto/Add_PortfolioTest/')  

# dir.create(Path_load)
dir.create(Path_save_xls)
dir.create(Path_save_fig)

library(quadprog)
library(lpSolve)
library(stringr)
library(ggplot2)
library(dplyr)
library(Matrix)

source("Portfolio/Portfolio_methods.R")

#----------------------------------------Data preparation ----------------------------------------

# data period
date_start_data = 20180201
date_end_data = 20220511

# porfolio construction 
date_start = 20190201
date_end = 20220511


#price
# price = read.csv("/Users/ruting/Documents/Github/FRM/FRM_Quantlet/FRM_All/MethodAdd/Portfolio/Crypto_Price_20200924.csv", header = TRUE)
# lambda = read.csv("/Users/ruting/Documents/Github/FRM/FRM_Quantlet/FRM_All/MethodAdd/Portfolio/lambdas_wide_20180701_20200924_90_0.05.csv", header = TRUE)

# update 20240112
price = read.csv("Price_20230514.csv", header = TRUE)
price = price[,-1]
price$date = as.numeric(gsub("[-]", "", price$date))


return = read.csv("Return_20230514.csv", header = TRUE)
return = return[,-1]
return$date = as.numeric(gsub("[-]", "", return$date))

lambda = read.csv("lambdas_wide_RuiBack_2022.csv", header = TRUE)
lambda$date = as.numeric(gsub("[-]", "", lambda$date))


tau=0.05

price[is.na(price)] = 0
return[is.na(return)] = 0

return=return[return$date>=date_start_data,]
price=price[price$date>=date_start_data,]
lambda=lambda[lambda$date>=date_start_data,]

# a = data.frame(sort(colMeans(lambda)))
# select_coin = c('date','BTC','ETH', 'XRP','BNB','BCH')
# 'LTC' lose data
select_coin = c('date','BTC','ADA','ETH', 'BCH', 'XLM','XRP','BNB','XMR')

lambdas=lambda[,select_coin] 
return=return[colnames(lambdas)]
price=price[colnames(lambdas)]


#----------------------------------------Application----------------------------------------

## 1. Comulatative wealth
ticker = return$date
N = nrow(return)       # number of total days
N0 = which( ticker == date_start )
N1 = which( ticker == date_end )
s = 120                #rolling window lengthn / days in the sample / construction period
reb = 1             #rebalancing period
N_upd = floor((N1-N0)/reb+1)  
Er =0.008             #expected return  mean(mu)
nss = 0               #Equals 1 if no short sales, for markowitz
ga=0.8               #qtec gamma 

pweight_m = matrix(0,N_upd,ncol(return)) #markowitz
wealth_m = matrix(1,N_upd,3)

pweight_q = matrix(0,N_upd,ncol(return)) #qtec
wealth_q = matrix(1,N_upd,3)

pweight_l = matrix(0,N_upd,ncol(return)) #ltec
wealth_l = matrix(1,N_upd,3)


## markowitz
for (t in seq(N0, N1, reb)){
  rdata = return[(t-s+1):t, ]
  ldata = lambdas[(t-s+1):t, ]
  #portfolio parameter
  mu =colMeans(rdata[,c(2:ncol(rdata))])    
  n = length(mu)                   
  cv = cov(rdata[,-1])            
  l_cv=cov(ldata[,-1])            
  l_mu=colMeans(ldata[,c(2:ncol(ldata) )])   
  # portfolio weight
  sol_m = markowitz_nss(n,mu,cv,Er,nss)  
  w_m = as.matrix(sol_m$solution)        
  pweight_m[(t-N0)/reb+1,1] = ticker[t]
  pweight_m[(t-N0)/reb+1,c(2:ncol(return))] = w_m
  #portfolio wealth
  if (t==N0){
    wealth_m[(t-N0)/reb+1,1] = ticker[t]     
  } else {
    ret = log(price[t,-1]) - log(price[t-reb,-1])   
    for (i in (1:length(ret))){
      if ( (ret[i]=="-Inf") || (ret[i]=="NaN") || (ret[i]=="Inf")){
        ret[i]=0
      }
    }
    wealth_m[(t-N0)/reb+1,1] = ticker[t]           
    wealth_m[(t-N0)/reb+1,2] = wealth_m[(t-N0)/reb,2] + as.numeric(ret) %*% pweight_m[(t-N0)/reb,-1]    #Wt=W(t-1)+w(t-1)Xt
  }
}
wealth_m[,3] = c(0,diff(wealth_m[,2])/wealth_m[1:(nrow(wealth_m)-1),2])

## for ltec
for (t in seq(N0, N1, reb)){
  rdata = return[(t-s+1):t, ]
  ldata = lambdas[(t-s+1):t, ]
  #squre root
  ldata[,2:ncol(lambdas)] = sqrt(lambdas[(t-s+1):t, 2:ncol(lambdas)] ) 
  
  #portfolio parameter
  mu =colMeans(rdata[,c(2:ncol(rdata))])    #mean vector
  n = length(mu)                            #number of assests
  cv = cov(rdata[,-1])                      #return covariance matrix
  l_cv=cov(ldata[,-1])                      #lambda covariance matrix
  l_mu=colMeans(ldata[,c(2:ncol(ldata) )])  #lambda mean
  sol_l = ltec(n,mu,l_mu,Er)             
  w_l = as.matrix(sol_l$solution)
  pweight_l[(t-N0)/reb+1,1] = ticker[t]
  pweight_l[(t-N0)/reb+1,c(2:ncol(return))] = w_l
  #portfolio wealth
  if (t==N0){
    wealth_l[(t-N0)/reb+1,1] = ticker[t]    
  } else {
    ret = log(price[t,-1]) - log(price[t-reb,-1])   
    for (i in (1:length(ret))){
      if ( (ret[i]=="-Inf") || (ret[i]=="NaN") || (ret[i]=="Inf")){
        ret[i]=0
      }
    }
    wealth_l[(t-N0)/reb+1,1] = ticker[t]           
    wealth_l[(t-N0)/reb+1,2] = wealth_l[(t-N0)/reb,2] + as.numeric(ret) %*% pweight_l[(t-N0)/reb,-1]
  }
}
wealth_l[,3] = c(0,diff(wealth_l[,2])/wealth_l[1:(nrow(wealth_l)-1),2])

#qtec
for (t in seq(N0, N1, reb)){
  rdata = return[(t-s+1):t, ]
  ldata = lambdas[(t-s+1):t, ]  
  #portfolio parameter
  mu =colMeans(rdata[,c(2:ncol(rdata))])    
  n = length(mu)                   
  cv = cov(rdata[,-1])             
  l_cv=cov(ldata[,-1])             
  l_mu=colMeans(ldata[,c(2:ncol(ldata) )])   
  
  w_q =qtec(n,mu,l_mu,l_cv,Er,nss,ga)
  pweight_q[(t-N0)/reb+1,1] = ticker[t]
  pweight_q[(t-N0)/reb+1,c(2:ncol(return))] = w_q
  if (t==N0){
    wealth_q[(t-N0)/reb+1,1] = ticker[t]    
  } else {
    ret = log(price[t,-1]) - log(price[t-reb,-1])   
    for (i in (1:length(ret))){
      if ( (ret[i]=="-Inf") || (ret[i]=="NaN") || (ret[i]=="Inf")){
        ret[i]=0
      }
    }
    wealth_q[(t-N0)/reb+1,1] = ticker[t]           
    wealth_q[(t-N0)/reb+1,2] = wealth_q[(t-N0)/reb,2] + as.numeric(ret) %*% pweight_q[(t-N0)/reb,-1]
  }
}
wealth_q[,3] = c(0,diff(wealth_q[,2])/wealth_q[1:(nrow(wealth_q)-1),2])

Out_PortWealth = cbind(wealth_q, wealth_m[,-1], wealth_l[,-1])
Out_PortWealth = data.frame(Out_PortWealth)
colnames(Out_PortWealth) = c('date', 'Wealth_QTEC','Return_QTEC', 'Wealth_MV', 'Return_MV', 'Wealth_LTEC','Return_LTEC')

write.csv(Out_PortWealth, paste0(Path_save_xls, "/PortWealth.csv"), quote = FALSE)

# #----------------------------------------Portfolio result----------------------------------------

# Performance
# Accumulated return, Average daily return, std of daily return, sharp ratio, effective n

Performance <- data.frame(Methodology = c('QTEC','MV','LTEC'), 
                          AccumulativeReturn = c(wealth_q[nrow(wealth_q),2]-1,wealth_m[nrow(wealth_m),2]-1,wealth_l[nrow(wealth_l),2]-1),
                          AverageDailyReturn = c(mean(wealth_q[,3],na.rm = TRUE), mean(wealth_m[,3],na.rm = TRUE), mean(wealth_l[,3],na.rm = TRUE)),
                          StdDailyReturn = c(sd(wealth_q[,3],na.rm = TRUE), sd(wealth_m[,3],na.rm = TRUE), sd(wealth_l[,3],na.rm = TRUE))
                          )
Performance$Sharp = Performance$AverageDailyReturn/Performance$StdDailyReturn
write.csv(Performance, paste0(Path_save_xls, "PortfolioPerformance.csv"), quote = FALSE)


# plot wealth for different strategies
df<- data.frame(
  x=as.Date(as.character(wealth_l[,1]),  "%Y%m%d"),
  y1=wealth_m [,2],
  y2=wealth_l [,2],
  y3=wealth_q [,2]
)

# Plot
p=ggplot(df, aes(x=x)) +
  scale_x_date(date_labels = "%Y%m%d",breaks='1 month',expand = c(0, 0)) +
  geom_line(aes(y = y1), color="dimgrey") +
  geom_line(aes(y = y2), color="steelblue") +
  geom_line(aes(y = y3), color="tomato2") +
  theme( panel.grid=element_blank(),
         panel.border = element_rect(color="black",fill = NA),
         legend.position = "none",
         axis.title = element_blank(),
         panel.background = element_rect(fill = "transparent",colour = NA),
         plot.background = element_rect(fill = "transparent",colour = NA),
         legend.box.background = element_rect(fill = "transparent"),
         #axis.line = element_line(colour = "black"),
         axis.text.x = element_text(angle = 90, hjust = 1,size=14, face= "bold"),
         axis.text.y = element_text(size=14, face= "bold" ) )

png(paste0(Path_save_fig,"fig_Wealth_",date_end,"_",s, "_",reb, "_",Er,"_",ga,"_",tau,".png"),width = 800, height = 500,
    bg = "transparent")  
p
dev.off()

