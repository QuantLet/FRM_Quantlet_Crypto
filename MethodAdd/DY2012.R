
rm(list = ls(all = TRUE))

libraries = c("iml","ggplot2", "data.table", "igraph","timeDate", "stringr", "graphics","magick", "scales", "tidyr", "zoo", "foreach", "doParallel",
              "xgboost","shapr","randomForest", "rpart", "quantreg", "readxl","dplyr", "xlsx", "psych","qgraph", "gganimate","av",
              "gifski", "strex","matrixStats","tools","usethis","Matrix", "ConnectednessApproach")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

wdir = "/Users/ruting/Documents/Github/FRM/FRM_Quantlet/FRM_All/"
Path_save_xls = paste0(wdir,'MethodAdd/')

# calculate dy2012
data_fixed = read.csv(file = paste0(wdir, "MethodAdd/Return_20230514.csv"), header = TRUE) %>% data.frame()
data_fixed = data_fixed[,-c(1,2)]
# data_fixed[data_fixed == 0] = NA
data_fixed[sapply(data_fixed, is.infinite)] <- 0
library(zoo)
a = data.frame(data_fixed)
a = (a^2*0.361*365)^0.5*100

time = as.Date('2014-11-29')+c(0:(nrow(data_fixed)-1))
data_test = as.zoo(a, time)

# dca = ConnectednessApproach(dy2012, 
#                             nlag=4, 
#                             nfore=10,
#                             model="VAR",
#                             connectedness="Time",
#                             Connectedness_config=list(TimeConnectedness=list(generalized=TRUE)))



# drop_sum = colSums(a[c(1 : 200),])
# temp = data_test[, -which(drop_sum==0)]  

for (i in c(200: nrow(data_fixed))){
  temp = data_test[(i-199):i,]
  # temp = temp[,colSums(is.na(temp)) ==0]
  temp = temp[,colSums(temp == 0) < 200/3]
  dca = ConnectednessApproach(temp, 
                              nlag=2, 
                              nfore=10,
                              window.size=200,
                              model="VAR",
                              connectedness="Time",
                              Connectedness_config=list(TimeConnectedness=list(generalized=TRUE)))
  keep = data.frame(TCI = dca$TCI[3])
  if (i == 200){
    total_dca = keep
  }else{
    total_dca = rbind(total_dca, keep)
  }
}

date_series = read.csv(file = paste0(wdir, "MethodAdd/Return_20230514.csv"), header = TRUE) %>% data.frame()
date_series = date_series$date
output = cbind(date_series[(length(date_series)-nrow(total_dca)+1):length(date_series)], total_dca)
colnames(output)[1] = 'Date'

save(dca, file = paste0(Path_save_xls, "/DYResult_",date_end_source,".rds"))

write.xlsx(output, paste0(Path_save_xls,"/DY_TCI.xlsx"), sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)
