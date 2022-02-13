setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

source("../summary_performance.r")

computePerformanceFromDetailResult <- function(path,threshold){
  files <- list.files(path=path,full.names = TRUE)
  # if(ncol(DF_pci)==0){
  #   rownames(DF_ifap3) <- rownames(DF_ifa_pii) <- rownames(DF_ifa_pci) <- rownames(DF_ifap2) <- rownames(DF_mdd) <- rownames(DF_ifap) <- rownames(DF_ifa) <- rownames(DF_mcc) <- rownames(DF_recall) <- rownames(DF_pf) <- rownames(DF_f1) <- rownames(DF_pii) <- rownames(DF_pci) <- files
  # }
  arr_f1 <- c()
  arr_g1 <- c()
  arr_pii <- c()
  arr_pf <- c()
  arr_pci <- c()
  arr_recall <- c()
  arr_mcc <- c()
  arr_ifa <- c()
  arr_ifap <- c()
  arr_mdd <- c()
  arr_ifap2 <- c()
  arr_ifa_pci <- c()
  arr_ifa_pii <- c()
  arr_ifap3 <- c()
  
  for(file in files){
    
    detail_result <- read.csv(file)
    
    res <- calculateIndicator(detail_result = detail_result,threshold = threshold)
    if(length(res)==1){
      cat(file)
      cat('\n')
    }
    
    tp <- res[1]
    fp <- res[2]
    tn <- res[3]
    fn <- res[4]
    f1 <- res[5]
    g1 <- res[6]
    pf <- res[7]
    pci <- res[8]
    pii <- res[9]
    recall <- res[10]
    mcc <- res[11]
    ifa <- res[12]
    ifap <- res[13]
    mdd <- res[14]
    ifap2 <- res[15]
    ifa_pii <- res[16]
    ifa_pci <- res[17]
    ifap3 <- res[18]
    
    arr_f1 <- append(arr_f1,f1)
    arr_g1 <- append(arr_g1,g1)
    arr_pf <- append(arr_pf,pf)
    arr_pci <- append(arr_pci,pci)
    arr_pii <- append(arr_pii,pii)
    arr_recall <- append(arr_recall,recall)
    arr_mcc <- append(arr_mcc,mcc)
    arr_ifa <- append(arr_ifa,ifa)
    arr_ifap <- append(arr_ifap,ifap)
    arr_mdd <- append(arr_mdd,mdd)
    arr_ifap2 <- append(arr_ifap2,ifap2)
    arr_ifa_pii <- append(arr_ifa_pii,ifa_pii)
    arr_ifa_pci <- append(arr_ifa_pci,ifa_pci)
    arr_ifap3 <- append(arr_ifap3,ifap3)
  }
  return(list('arr_f1' = arr_f1,'arr_g1' = arr_g1,'arr_pf' = arr_pf,'arr_pci' = arr_pci,
       'arr_pii' = arr_pii,'arr_recall' = arr_recall,'arr_mcc' = arr_mcc,
       'arr_ifa' = arr_ifa,'arr_ifap' = arr_ifap,'arr_mdd' = arr_mdd,
       'arr_ifap2' = arr_ifap2,'arr_ifa_pii' = arr_ifa_pii,
       'arr_ifa_pci' = arr_ifa_pci,'arr_ifap3' = arr_ifap3))
  # return(c(arr_f1,arr_g1,arr_pf,arr_pci,arr_pii,arr_recall,arr_mcc,arr_ifa,arr_ifap,arr_mdd,arr_ifap2,arr_ifa_pii,arr_ifa_pci,arr_ifap3))
}

compareUnderAlignedSQAeffort <- function(baselines,baseline_paths,threshold){
  # setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  # setwd('../CrossPare/')

  DF_pci <- data.frame(matrix(nrow = 86,ncol = 0))
  DF_pii <- data.frame(matrix(nrow = 86,ncol = 0))
  DF_f1 <- data.frame(matrix(nrow = 86,ncol = 0))
  DF_g1 <- data.frame(matrix(nrow = 86,ncol = 0))
  DF_pf <- data.frame(matrix(nrow = 86,ncol = 0))
  DF_recall <- data.frame(matrix(nrow = 86,ncol = 0))
  DF_mcc <- data.frame(matrix(nrow = 86,ncol = 0))
  DF_ifa <- data.frame(matrix(nrow = 86,ncol = 0))
  DF_ifap <- data.frame(matrix(nrow = 86,ncol = 0))
  DF_mdd <- data.frame(matrix(nrow = 86,ncol = 0))
  DF_ifap2 <- data.frame(matrix(nrow = 86,ncol = 0))
  DF_ifa_pii <- data.frame(matrix(nrow = 86,ncol = 0))
  DF_ifa_pci <- data.frame(matrix(nrow = 86,ncol = 0))
  DF_ifap3 <- data.frame(matrix(nrow = 86,ncol = 0))
  
  for(i in 1:length(baselines)){
    cat(baselines[i])
    cat('\n')
    
    temp_res_list <- computePerformanceFromDetailResult(baseline_paths[i],threshold)

    arr_f1 <- temp_res_list$arr_f1
    arr_g1 <- temp_res_list$arr_g1
    arr_pf <- temp_res_list$arr_pf
    arr_pci <- temp_res_list$arr_pci
    arr_pii <- temp_res_list$arr_pii
    arr_recall <- temp_res_list$arr_recall
    arr_mcc <- temp_res_list$arr_mcc
    arr_ifa <- temp_res_list$arr_ifa
    arr_ifap <- temp_res_list$arr_ifap
    arr_mdd <- temp_res_list$arr_mdd
    arr_ifap2 <- temp_res_list$arr_ifap2
    arr_ifa_pii <- temp_res_list$arr_ifa_pii
    arr_ifa_pci <- temp_res_list$arr_ifa_pci
    arr_ifap3 <- temp_res_list$arr_ifap3
    
    DF_pci[,baselines[i]] <- arr_pci
    DF_pii[,baselines[i]] <- arr_pii
    DF_f1[,baselines[i]] <- arr_f1
    DF_g1[,baselines[i]] <- arr_g1
    DF_pf[,baselines[i]] <- arr_pf
    DF_recall[,baselines[i]] <- arr_recall
    DF_mcc[,baselines[i]] <- arr_mcc
    DF_ifa[,baselines[i]] <- arr_ifa
    DF_ifap[,baselines[i]] <- arr_ifap
    DF_mdd[,baselines[i]] <- arr_mdd
    DF_ifap2[,baselines[i]] <- arr_ifap2
    DF_ifa_pii[,baselines[i]] <- arr_ifa_pii
    DF_ifa_pci[,baselines[i]] <- arr_ifa_pci
    DF_ifap3[,baselines[i]] <- arr_ifap3
    
  }
  
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  getwd()

  write.csv(DF_pci,file=paste('result/alignment_pci_comparison_',as.character(threshold),'.csv',sep=''))
  write.csv(DF_pii,file=paste('result/alignment_pii_comparison_',as.character(threshold),'.csv',sep=''))
  write.csv(DF_f1,file=paste('result/alignment_f1_comparison_',as.character(threshold),'.csv',sep=''))
  write.csv(DF_g1,file=paste('result/alignment_g1_comparison_',as.character(threshold),'.csv',sep=''))
  write.csv(DF_pf,file=paste('result/alignment_pf_comparison_',as.character(threshold),'.csv',sep=''))
  write.csv(DF_recall,file=paste('result/alignment_recall_comparison_',as.character(threshold),'.csv',sep=''))
  write.csv(DF_mcc,file=paste('result/alignment_mcc_comparison_',as.character(threshold),'.csv',sep=''))
  write.csv(DF_ifa,file=paste('result/alignment_ifa_comparison_',as.character(threshold),'.csv',sep=''))
  write.csv(DF_ifap,file=paste('result/alignment_ifap_comparison_',as.character(threshold),'.csv',sep=''))
  write.csv(DF_mdd,file=paste('result/alignment_mdd_comparison_',as.character(threshold),'.csv',sep=''))
  write.csv(DF_ifap2,file=paste('result/alignment_ifap2_comparison_',as.character(threshold),'.csv',sep=''))
  write.csv(DF_ifa_pii,file=paste('result/alignment_ifa_pii_comparison_',as.character(threshold),'.csv',sep=''))
  write.csv(DF_ifa_pci,file=paste('result/alignment_ifa_pci_comparison_',as.character(threshold),'.csv',sep=''))
  write.csv(DF_ifap3,file=paste('result/alignment_ifap3_comparison_',as.character(threshold),'.csv',sep=''))
}


baselines <- c("CamargoCruz09-NB","Amasaki15-NB","Peters15-NB", "EASC_E", "EASC_NE","SC","CLA","FCM",
               "ManualDown","ManualUp",
               "Bellwether")


baseline_paths <- c('../prediction_result/CamargoCruz09-NB/',
                    '../prediction_result/Amasaki15-NB/',
                    '../prediction_result/Peters15-NB/',
                    '../prediction_result/EASC_E/',
                    '../prediction_result/EASC_NE/',
                    '../prediction_result/SC',
                    '../prediction_result/CLA',
                    '../prediction_result/FCM',
                    '../prediction_result/ManualDown',
                    '../prediction_result/ManualUp',
                    '../prediction_result/Bellwether/')

compareUnderAlignedSQAeffort(baselines,baseline_paths,-1)
compareUnderAlignedSQAeffort(baselines,baseline_paths,20)
compareUnderAlignedSQAeffort(baselines,baseline_paths,0.2)





