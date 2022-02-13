################CLA############################
setwd("C:/Users/Lau/OneDrive/UCS2/UCS/unsupervised/CLAMI")
getwd()
data <- read.csv('CLA_result.csv')
res <- data.frame(matrix(nrow = nrow(data),ncol = 2))
colnames(res) <- c("Test","Cutoff")
res$Test <- data$Test
res$Cutoff <- data$tp + data$fp
write.csv(res,"cutoff_CLA.csv",row.names = FALSE,quote = FALSE)


# divide to dataset
datasets <- c("AEEEM", "ALLJURECZKO", "MDP", "NETGENE", "RELINK")
for (dataset in datasets){
  arr <- grep(dataset,data$Test,value = TRUE)
  div <- data[data$Test %in% arr,]
  write.csv(div,file=paste(dataset,'_cpdp_CLA.csv',sep=""),row.names = FALSE,quote = FALSE)
}


################SC############################
setwd("C:/Users/Lau/OneDrive/UCS2/UCS/unsupervised/SC")
getwd()
data <- read.csv('SC_result.csv')
res <- data.frame(matrix(nrow = nrow(data),ncol = 2))
colnames(res) <- c("Test","Cutoff")
res$Test <- data$Test
res$Cutoff <- data$tp + data$fp
write.csv(res,"cutoff_SC.csv",row.names = FALSE,quote = FALSE)


# divide to dataset
datasets <- c("AEEEM", "ALLJURECZKO", "MDP", "NETGENE", "RELINK")
for (dataset in datasets){
  arr <- grep(dataset,data$Test,value = TRUE)
  div <- data[data$Test %in% arr,]
  write.csv(div,file=paste(dataset,'_cpdp_SC.csv',sep=""),row.names = FALSE,quote = FALSE)
}
