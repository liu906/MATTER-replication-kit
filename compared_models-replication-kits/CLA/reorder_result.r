setwd("C:\\Users\\Lau\\OneDrive\\UCS2\\UCS\\unsupervised\\CLAMI\\result")
files <- list.files('.',full.names=TRUE)
for(file in files){
  data <- read.csv(file)
  data <- data[order(as.numeric(data$Index)), ]
  write.csv(data,file,quote=FALSE,row.names=FALSE)
}
