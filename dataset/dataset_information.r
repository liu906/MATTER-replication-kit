
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
file_path <- "../data_Herbold_2017/"
# file_path <- "../model/classoverlap/data_classoverlap/"

folders <- list.files(file_path, full.names = TRUE)
table <- data.frame(matrix(nrow = 0,ncol = 6))
colnames(table) <- c("Dataset","Project","Versions","#Instances","%Defective","#Metrics")
for(folder in folders){
  dataset.name <- folder
  projects <- list.files(folder)
  
  for(project in projects){
    versions <- ""
    instances.min <- 1000000
    instances.max <- -1
    defectiveRatio.min <- 1
    defectiveRatio.max <- 0
    metricsNum <- -1
    versions.fullname <- list.files(paste(folder,project,sep = '/'))
    for (version.fullname in versions.fullname){
      versions.number <- as.numeric(unlist(regmatches(version.fullname,
                                                      gregexpr("-[[:digit:]]+\\.*[[:digit:]]*",version.fullname)) ))
      versions <- paste(versions,substr(versions.number[1],2,nchar(versions.number[1])),", ",sep = "")
      data <- read.csv(paste(folder,project,version.fullname,sep = '/'))
      instanceNum <- nrow(data)
      defectiveRatio <- nrow(data[data$bug>0,]) / nrow(data)
      if(instanceNum > instances.max){
        instances.max <- instanceNum
      }
      if(instanceNum < instances.min){
        instances.min <- instanceNum
      }
      if(defectiveRatio > defectiveRatio.max){
        defectiveRatio.max <- defectiveRatio
      }
      if(defectiveRatio < defectiveRatio.min){
        defectiveRatio.min <- defectiveRatio
      }
      if (metricsNum == -1){
        metricsNum <- ncol(data) - 1
      }
      else if(ncol(data)-1 != metricsNum){
        cat("dataset error: inconsistant number of metrics\n")
      }
    }
    if(length(versions.fullname)==1){
      table[nrow(table)+1,] = c(folder,project,"-",instances.min,paste(round(defectiveRatio.min*100),"%",sep = ""),metricsNum)
    }
    else{
      rangeOfDefective <- paste(round(defectiveRatio.min*100),"%~%",round(defectiveRatio.max*100),sep = "")
      rangeOfInstace <- paste(instances.min,"~",instances.max,sep = "")
      table[nrow(table)+1,] = c(folder,project,substr(versions,1,nchar(versions)-2),rangeOfInstace,rangeOfDefective,metricsNum)
    }
    
  }
}
table
write.csv2(table,file='./result/dataset_information.csv',quote = FALSE,row.names = FALSE)

