setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
files = list.files('./FCM/',full.names = TRUE)
for(file in files){
  data = read.csv(file)
  data[data$predictLabel=='T','predictLabel'] = 1
  data[data$predictLabel=='F','predictLabel'] = 0
  data$predictLabel <- 1 * data$predictLabel
  write.csv(data,file = file,row.names = FALSE)
}

files = list.files('./ManualDown/',full.names = TRUE)
for(file in files){
  data = read.csv(file)
  data[data$predictLabel=='T','predictLabel'] = 1
  data[data$predictLabel=='F','predictLabel'] = 0
  data$predictLabel <- 1 * data$predictLabel
  write.csv(data,file = file,row.names = FALSE)
}

files = list.files('./ManualUp/',full.names = TRUE)
for(file in files){
  data = read.csv(file)
  data[data$predictLabel=='T','predictLabel'] = 1
  data[data$predictLabel=='F','predictLabel'] = 0
  data$predictLabel <- 1 * data$predictLabel
  write.csv(data,file = file,row.names = FALSE)
}



files = list.files('./CLA/',full.names = TRUE)
for(file in files){
  data = read.csv(file,row.names = 1)
  colnames(data) <- c('predictedValue', 'predictLabel', 'sloc', 'actualBugLabel')
  write.csv(data,file = file,row.names = FALSE)
}
