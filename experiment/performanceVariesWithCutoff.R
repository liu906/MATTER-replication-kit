setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
source("../summary_performance.r")
source('../One.r')
library(dplyr)

performanceVariesWithCutoff <- function(thresholds,mode,root_path){
  for (threshold in thresholds){
    
    res <- summaryPerformance2(root_path=root_path,threshold = threshold, mode=mode)
    res$cutoff <- threshold
    res$target <- list.files(root_path)
    if(threshold==thresholds[1]){
      total_res <- res
    }else{ 
      total_res <- rbind(total_res,res)
    }
  }
  return(total_res)
}
performanceVariesWithCutoff_one <- function(thresholds,mode,root_path,excluded_code_size_percentage=20){
  for (threshold in thresholds) {
    files <- list.files(root_path)
    res_root_path <- paste('../prediction_result/ONE/ALLJURECZKO/',
                           'cutoff',as.character(threshold),'_excludedCodeSizePercentage'
                           ,as.character(excluded_code_size_percentage),'/',sep = '')
    dir.create(res_root_path,showWarnings = FALSE)
    unlink(paste(res_root_path,'*',sep=''))
    for (file in files) {
      res <- one2(root = root_path,file = file,excluded_code_size_percentage = excluded_code_size_percentage,cutoff = threshold)  
      write.csv(res,file = paste(res_root_path,file,'_',threshold,'.csv',sep = ''))
    }
    
    one_df <- summaryPerformance2(res_root_path,threshold = -1,mode = 'default')
    one_df$cutoff <- threshold
    if(threshold == thresholds[1]){
      total_one_df <- one_df
    }else{
      total_one_df <- rbind(total_one_df,one_df)
    }
  }
  return(total_one_df)
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

detail_result_paths <- c('../prediction_result/SC/',
                         '../prediction_result/EASC_E/',
                         '../prediction_result/Bellwether/')
model_names <- c('SC','EASC_E','Bellwether')

for(idx in 1:length(model_names)){

  total_res <- performanceVariesWithCutoff(thresholds=seq(10,100,10),'SSC',detail_result_paths[idx])
  write.csv(total_res,paste('./result/performanceVariesWithCutoff_SSC_',model_names[idx],'.csv',sep = ''))

  total_res[, c(4:ncol(total_res))] <- sapply(total_res[, c(4:ncol(total_res))], as.numeric)
  total_res[is.na(total_res)] <- 0

  median_df  <- total_res %>%
    group_by(cutoff) %>%
    summarise_at(vars(recall,pii,pci,roi,ifap,roi3,ifap3), list(median = median))

  write.table(median_df , file = paste('./result/performanceVariesWithCutoff_SSC_',model_names[idx],'_median.csv',sep = ''),sep=',',row.names=FALSE)
  mean_df  <- total_res %>%
    group_by(cutoff) %>%
    summarise_at(vars(recall,pii,pci,roi3,ifap3), list(mean = mean))

  write.table(mean_df , file = paste('./result/performanceVariesWithCutoff_SSC_',model_names[idx],'_mean.csv',sep = ''),sep=',',row.names=FALSE)




  total_res <- performanceVariesWithCutoff(thresholds=seq(0.1,1,0.1),'SNM',detail_result_paths[idx])
  write.csv(total_res,paste('./result/performanceVariesWithCutoff_SNM_',model_names[idx],'.csv',sep = ''))

  total_res[, c(4:ncol(total_res))] <- sapply(total_res[, c(4:ncol(total_res))], as.numeric)
  total_res[is.na(total_res)] <- 0

  median_df  <- total_res %>%
    group_by(cutoff) %>%
    summarise_at(vars(recall,pii,pci,roi,ifap,roi3,ifap3), list(median = median))

  write.table(median_df , file = paste('./result/performanceVariesWithCutoff_SNM_',model_names[idx],'_median.csv',sep = ''),sep=',',row.names=FALSE)

  mean_df  <- total_res %>%
    group_by(cutoff) %>%
    summarise_at(vars(recall,pii,pci,roi,ifap,roi3,ifap3), list(mean = mean))

  write.table(mean_df , file = paste('./result/performanceVariesWithCutoff_SNM_',model_names[idx],'_mean.csv',sep = ''),sep=',',row.names=FALSE)

}

total_res <- performanceVariesWithCutoff_one(thresholds=seq(10,100,10),
                                             mode = 'SSC',root_path = "../prediction_result/Amasaki15-NB/",excluded_code_size_percentage = 20)
total_res[, c(4:ncol(total_res))] <- sapply(total_res[, c(4:ncol(total_res))], as.numeric)
total_res$roi <- total_res$recall / total_res$pii
total_res$roi3 <- total_res$recall / sqrt(total_res$pii * total_res$pci)
total_res[is.na(total_res)] <- 0
write.csv(total_res,paste('./result/performanceVariesWithCutoff_SSC_excludedCodeSizePercentage20_One.csv',sep = ''))
median_df  <- total_res %>%
  group_by(cutoff) %>%
  summarise_at(vars(recall,pii,pci,roi,ifap,roi3,ifap3), list(median = median))
write.table(median_df , file = paste('./result/performanceVariesWithCutoff_SSC_excludedCodeSizePercentage20_One_median.csv',sep = ''),sep=',',row.names=FALSE)


total_res <- performanceVariesWithCutoff_one(thresholds=seq(0.1,1,0.1),mode = 'SNM',root_path = "../prediction_result/Amasaki15-NB/")
total_res[, c(4:ncol(total_res))] <- sapply(total_res[, c(4:ncol(total_res))], as.numeric)

total_res$roi <- total_res$recall / total_res$pci
total_res$roi3 <- total_res$recall / sqrt(total_res$pii * total_res$pci)
total_res[is.na(total_res)] <- 0
write.csv(total_res,paste('./result/performanceVariesWithCutoff_SNM_excludedCodeSizePercentage20_One.csv',sep = ''))
median_df  <- total_res %>%
  group_by(cutoff) %>%
  summarise_at(vars(recall,pii,pci,roi,ifap,roi3,ifap3), list(median = median))

write.table(median_df , file = paste('./result/performanceVariesWithCutoff_SNM_excludedCodeSizePercentage20_One_median.csv',sep = ''),sep=',',row.names = FALSE)







