rm(list = ls(all = TRUE))

# wdir = "//Users/ruting/Documents/Github/FRM/FRM_Quantlet/FRM_All"

wdir = "/Users/ruting/Documents/Github/FRM_Crypto_Github/FRM_All/"
input_path = paste0(wdir, 'Output/Crypto/')
output_path = paste0(input_path,'Add_Correlation/')
output_centrality = paste0(input_path,'Centrality/')
save_date = 20230514
channel = "Crypto"
tau = 0.05
s = 63

#Plot parameter defined based on the outliers
if (channel == "Crypto") {
  lambda_cutoff = 0.1359
  M_macro = 5} else 
if (channel == "EM") {
  lambda_cutoff = 10
  M_macro = 12}


#Check if package is installed, if not: install, either way: load 
libraries = c("iml","ggplot2", "data.table", "igraph","timeDate", "stringr", "graphics","magick", "scales", "tidyr", "zoo", "foreach", "doParallel",
              "xgboost","shapr","randomForest", "rpart", "quantreg", "readxl","dplyr", "xlsx", "psych","qgraph", "gganimate","av",
              "gifski", "strex","matrixStats","tools","qgraph")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


#Set ggplot2 theme
theme_set(theme_classic())

#List of centrality types and their numbers
centralitylist = list("OutDegree" = 1, "InDegree" = 2, "Closeness" = 3, 
                      "Betweenness" = 4, "InInfluence" = 5, "OutInfluence" = 6)

#Read historical FRM index
FRM_index = read.csv(paste0(input_path, "/Lambda/FRM_", channel, "_index.csv"))


## Calculate centralities

#Create a list of files in the folder and extract dates from the names
file_list = list.files(path = paste0(input_path, "/Adj_Matrices"))
file_list = file_list[file_list!="Fixed"]
dates = as.character(str_first_number(file_list), format = "%Y%m%d")
dates = as.Date(dates, format = "%Y%m%d")
N = length(file_list)

#Create a list of all network graphs
allgraphs = lapply(1:N, function(i) {
  data = read.csv(paste0(input_path, "/Adj_Matrices/", file_list[i]), row.names = 1)
  M_stock = ncol(data)-M_macro
  adj_matrix = data.matrix(data[1:M_stock, 1:M_stock])
  q = qgraph(adj_matrix, layout = "circle", details = TRUE, 
                                   vsize = c(5,15), DoNotPlot = TRUE)
  return(q)
})

allcentralities = centrality(allgraphs)
eigencentrality = lapply(1:N, function(i) eigen_centrality(as.igraph(allgraphs[[i]]))$vector)

#Calculate averages
outdegree_avg = sapply(1:N, function(i) mean(allcentralities[[i]]$OutDegree))
indegree_avg = sapply(1:N, function(i) mean(allcentralities[[i]]$InDegree))
closeness_avg = sapply(1:N, function(i) mean(allcentralities[[i]]$Closeness))
betweenness_avg = sapply(1:N, function(i) mean(allcentralities[[i]]$Betweenness))
eigenvector_avg = sapply(1:N, function(i) mean(eigencentrality[[i]]))

Degree_Index = cbind(FRM_index, outdegree_avg, indegree_avg, closeness_avg, eigenvector_avg)


# read index 
BEKK  = data.frame(read.csv(paste0(wdir, "MethodAdd/BEKK_Index_", 
                                   save_date, ".csv")))
BEKK = BEKK[,-1]

EDH = data.frame(read.csv(paste0(wdir, "MethodAdd/EDC_Index_", 
                                 save_date, ".csv"))) 
EDH = EDH[,-1]
colnames(EDH)[2] = 'EDH'
EDH$date = as.character(as.Date(as.character(EDH$date), format = "%Y%m%d")) 

EDC= data.frame(read.csv(paste0(wdir, "MethodAdd/EDC_Index_Simple_", 
                                save_date, ".csv"))) 
EDC = EDC[,-1]


Degree_Index = merge(Degree_Index, EDC, all.x = TRUE)
Degree_Index = merge(Degree_Index, EDH, all.x = TRUE)
Degree_Index = merge(Degree_Index, BEKK, all.x = TRUE)



cent_plot = function(cent_type, lambda) {
  # cent_string = deparse(substitute(cent_type))
  cent_string = cent_type
  lambda = Degree_Index$frm
  
  png(paste0(output_centrality, "FRM_", cent_string,".png"), 
      width = 900, height = 600, bg = "transparent")
  par(mar = c(5, 4, 4, 4) + 0.3)
  # plot(lambda[-outliers], type = "l", col = "blue", xlab = "", 
  #      ylab = "FRM index", xaxt = "n", lwd = 2)
  plot(lambda, type = "l", col = "blue", xlab = "", 
       ylab = "FRM index", xaxt = "n", lwd = 2)
  par(new = TRUE)
  # plot(cent_type[-outliers], type = "l", col = "red", axes = FALSE, 
  #      xlab = "", ylab = "", xaxt = "n")
  plot(Degree_Index[,cent_type], type = "l", col = "red", axes = FALSE, 
       xlab = "", ylab = "", xaxt = "n")
  # axis(side = 4, at = pretty(range(cent_type[-outliers])))
  axis(side = 4, at = pretty(range(Degree_Index[,cent_type])))
  
  # ll = which(FRM_index$date[-outliers] %in% plot_labels)
  # ll = which(FRM_index$date %in% plot_labels)
  ll = seq(120, nrow(FRM_index), 120)
  axis(1, at = ll, labels = FRM_index$date[ll])
  mtext(paste0(gsub("_.*", "", cent_string) %>% toTitleCase, " centrality"), 
        side = 4, line = 3)
  dev.off()
}

cent_plot('EDC_simp')
cent_plot('EDH')
cent_plot('BEKK_A')
cent_plot('BEKK_G')
cent_plot('outdegree_avg')
cent_plot('indegree_avg')
cent_plot('betweenness_avg')
cent_plot('closeness_avg')
cent_plot('eigenvector_avg')
# cent_plot(degree_avg, FRM_index$frm)

# corr 
# cormatrix = cbind(FRM_index$frm,outdegree_avg, closeness_avg, eigenvector_avg,betweenness_avg)

cormatrix = Degree_Index[,c('frm','closeness_avg','eigenvector_avg','EDC_simp',	'EDH','BEKK_G')]
cormatrix = cormatrix[!is.na(cormatrix$EDH),]
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

write.csv(corr,paste0(output_path, "Centralilty_FRM_Corr_add.csv"),quote = FALSE)
write.csv(p_value_matrix,paste0(output_path, "Centralilty_FRM_CorrP_add.csv"),quote = FALSE)


# compare with VCRIX and CRIX
VIX_file = paste0(wdir,"Input/", channel, "/VCRIX")
CRIX = read.csv(paste0(VIX_file, "/CRIX.csv")) 
loc = which(CRIX$price>200000) 
CRIX$price[loc] = CRIX$price[loc] /1000
CRIX$CRIX_return = c(0,diff(log(CRIX$price)))
CRIX$vol_CRIX = NA
for (i in c(63:nrow(CRIX))){
  CRIX$vol_CRIX[i] = (sd(CRIX$CRIX_return[(i-62):i]))^2
}

FRM_index = read.csv(paste0(input_path, "/Lambda/FRM_", channel, "_index.csv"))
FRM_index = merge(FRM_index, CRIX, all.x = TRUE, all.y = FALSE)
FRM_index = FRM_index[!is.na(FRM_index$vol_CRIX),]
FRM_index = FRM_index[FRM_index$date>'2018-11-01',]

png(paste0(output_path, "FRM_CRIX_Volatility.png"), 
    width = 900, height = 600, bg = "transparent")
par(mar = c(5, 4, 4, 4) + 0.3)
plot(FRM_index$frm, type = "l", col = "blue", xlab = "", 
     ylab = "FRM index", xaxt = "n", lwd = 2)
par(new = TRUE)
plot(FRM_index$vol_CRIX, type = "l", col = "red", axes = FALSE, 
     xlab = "", ylab = "", xaxt = "n")
axis(side = 4, at = pretty(range(FRM_index$vol_CRIX)))

ll = seq(120, nrow(FRM_index), 120)
axis(1, at = ll, labels = FRM_index$date[ll])
mtext("CRIX log return rolling variance", side = 4, line = 3)
dev.off()


# corr 
cormatrix = FRM_index[,c('frm','vol_CRIX')]
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
write.csv(corr,paste0(output_path,"FRM_CRIX_Volatility_Corr.csv"),quote = FALSE)
write.csv(p_value_matrix,paste0(output_path,"FRM_CRIX_Volatility_CorrP.csv"),quote = FALSE)


