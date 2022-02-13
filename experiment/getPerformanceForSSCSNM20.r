setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
source("../summary_performance.r")

model_names <- c('MSMDA','KSETE','Bellwether','Kcore')
model_detail_prediction_results_roots <- c('../prediction_result/MSMDA_v1/',
                                           '../prediction_result/KSETE-master-master/',
                                           '../prediction_result/Bellwether/',
                                           '../prediction_result/Kcore/') 
effort_thresholds <- c(-1,0.2,20)
modes <- c('','SSC','SNM')

for(i in 1:length(model_names)){
  model_name <- model_names[i]
  root_path <- model_detail_prediction_results_roots[i]
  for(i_threshold in 1:length(effort_thresholds)){
    mode <- modes[i_threshold]
    threshold <- effort_thresholds[i_threshold]
    df <- summaryPerformance2(root_path,threshold, mode)
    write.csv(df,paste('./result/model_prediction_result_given_effort/',model_name,'_threshold',threshold,'.csv',sep=''))
  }
}

thresholds <- c(0.2,20)
modes <- c('SNM','SSC')
dataset_names <- c('KSETE','MSMDA','teraPROMISE','ALLJURECZKO')
for(dataset_name in dataset_names){
  one_threshold = -1
  excluded_code_size_percentage <- 20
  for(i in 1:length(thresholds)){
    threshold <- thresholds[i]
    mode <- modes[i]
    
    df <- summaryPerformance2(root_path=paste('../prediction_result/One/',dataset_name,'/cutoff',
                                              threshold,'_excludedCodeSizePercentage',
                                              excluded_code_size_percentage,'/',sep=''),one_threshold,mode)  
    write.csv(df,paste('./result/model_prediction_result_given_effort/ONE_sameDatasetWith',dataset_name,'_threshold',threshold,'.csv',sep=''))
  }
}













