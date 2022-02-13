rq2 <- function(threshold,one_path,postfix){
  modes = c('SSC','SNM')
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  getwd()
  for (mode in modes){
    if (mode=='SSC'){
      threshold = 20
    }else if(mode=='SNM'){
      threshold = 0.2
    }
    DF_pci = read.csv(paste('result/alignment_pci_comparison_',as.character(threshold),'.csv',sep=''),row.names = 1,)
    DF_pii = read.csv(paste('result/alignment_pii_comparison_',as.character(threshold),'.csv',sep=''),row.names = 1)
    DF_recall = read.csv(paste('result/alignment_recall_comparison_',as.character(threshold),'.csv',sep=''),row.names = 1)
    DF_ifap = read.csv(paste('result/alignment_ifap_comparison_',as.character(threshold),'.csv',sep=''),row.names = 1)
    DF_mdd = read.csv(paste('result/alignment_mdd_comparison_',as.character(threshold),'.csv',sep=''),row.names = 1)
    DF_ifap2 = read.csv(paste('result/alignment_ifap2_comparison_',as.character(threshold),'.csv',sep=''),row.names = 1)
    DF_ifa = read.csv(paste('result/alignment_ifa_comparison_',as.character(threshold),'.csv',sep=''),row.names = 1)
    DF_ifa_pii = read.csv(paste('result/alignment_ifa_pii_comparison_',as.character(threshold),'.csv',sep=''),row.names = 1)
    DF_ifa_pci = read.csv(paste('result/alignment_ifa_pci_comparison_',as.character(threshold),'.csv',sep=''),row.names = 1)
    DF_ifap3 = read.csv(paste('result/alignment_ifap3_comparison_',as.character(threshold),'.csv',sep=''),row.names = 1)
    if (mode=='SSC'){
      DF_roi = DF_recall/DF_pii
      
    }else if(mode=='SNM'){
      DF_roi = DF_recall/DF_pci
      
    }
    DF_roi2 = DF_recall/(0.5*DF_pii+0.5*DF_pci)
    DF_roi3 = DF_recall/sqrt(DF_pii*DF_pci)
    
    #todo: path 
    one_performance <- read.csv(one_path)
    
    
    DF <- data.frame(matrix(nrow = nrow(one_performance),ncol = 0))
    DF$Recall <- DF_recall
    DF$Recall_one <- one_performance$recall
    
    DF$IFAP <- DF_ifap
    DF$IFAP_one <- one_performance$ifap
    
    DF$ROI <- DF_roi
    DF$ROI_one <- one_performance$roi
    
    DF$MDD <- DF_mdd
    DF$MDD_one <- one_performance$mdd
    
    DF$IFAP3 <- DF_ifap3
    DF$IFAP3_one <- one_performance$ifap3
    
    DF$IFAP2 <- DF_ifap2
    DF$IFAP2_one <- one_performance$ifap2
    
    DF$ROI2 <- DF_roi2
    DF$ROI2_one <- one_performance$roi2
    
    DF$ROI3 <- DF_roi3
    DF$ROI3_one <- one_performance$roi3
    
    DF$IFA <- DF_ifa
    DF$IFA_one <- one_performance$ifa
    
    DF$IFA_PII <- DF_ifa_pii
    DF$IFA_PII_one <- one_performance$ifa_pii
    DF$IFA_PCI <- DF_ifa_pci
    DF$IFA_PCI_one <- one_performance$ifa_pci
    
    write.csv(DF_roi3,paste('result/alignment_ROI3_comparison_',as.character(threshold),'.csv',sep=''))
    write.csv(DF_ifap3,paste('result/alignment_IFAP3_comparison_',as.character(threshold),'.csv',sep=''))
    write.csv(DF_roi2,paste('result/alignment_ROI2_comparison_',as.character(threshold),'.csv',sep=''))
    write.csv(DF_ifap2,paste('result/alignment_IFAP2_comparison_',as.character(threshold),'.csv',sep=''))
    write.csv(DF_ifa,paste('result/alignment_IFA_comparison_',as.character(threshold),'.csv',sep=''))
    write.csv(DF_ifa_pii,paste('result/alignment_IFA_PII_comparison_',as.character(threshold),'.csv',sep=''))
    write.csv(DF_ifa_pci,paste('result/alignment_IFA_PCI_comparison_',as.character(threshold),'.csv',sep=''))
    

    write.csv(DF,paste('result/rq2_',as.character(threshold),postfix,'.csv',sep=''))
     
  }
}

one_path <- './result/model_prediction_result_given_effort/ONE_sameDatasetWithALLJURECZKO_threshold0.2.csv'
rq2(0.2,one_path,postfix = '')
one_path <- './result/model_prediction_result_given_effort/ONE_sameDatasetWithALLJURECZKO_threshold20.csv'
rq2(20,one_path,postfix = '')


rq2_append_one <- function(threshold,model_paths,one_paths,postfix){
  # 'KSETE','top-core','MSMDA',

  model_names <- c('KSETE','MSMDA','top-core')
  for(counter in 1:length(model_paths)){
    model_path <- model_paths[counter]
    model_performance <- read.csv(model_path)
    one_path <- one_paths[counter]
    if(threshold>1){
      mode = 'SSC'
    }else{
      mode = 'SNM'
    }
    one_performance <- read.csv(one_path)
    cols <- c('Recall','Recall_one','ROI','ROI_one','IFAP','IFAP_one','MDD','MDD_one','IFAP2','IFAP2_one','ROI2','ROI2_one','IFA','IFA_one','IFA_PII','IFA_PII_one','IFA_PCI','IFA_PCI_one')
    DF <- data.frame(matrix(nrow = nrow(model_performance),ncol = length(cols)))
    colnames(DF) <- cols
    DF$Recall <- model_performance$recall
    DF$Recall_one <- one_performance$recall
    DF$IFAP <- model_performance$ifap
    DF$IFAP_one <- one_performance$ifap
    DF$MDD <- model_performance$mdd
    DF$MDD_one <- one_performance$mdd
    
    DF$IFA <- model_performance$ifa
    DF$IFA_one <- one_performance$ifa
    
    DF$IFA_PII  <- model_performance$ifa_pii
    DF$IFA_PII_one <- one_performance$ifa_pii
    
    DF$IFA_PCI  <- model_performance$ifa_pci
    DF$IFA_PCI_one <- one_performance$ifa_pci
    
    DF$IFAP2 <- model_performance$ifap2
    DF$IFAP2_one <- one_performance$ifap2
    
    DF$IFAP3 <- model_performance$ifap3
    DF$IFAP3_one <- one_performance$ifap3
    
    
    if(threshold>1){
      DF$ROI <- model_performance$recall /model_performance$pii
      DF$ROI_one <- one_performance$recall /one_performance$pii
      
      
    }else{
      DF$ROI <- model_performance$recall /model_performance$pci
      DF$ROI_one <- one_performance$recall /one_performance$pci
      
    }
    DF$ROI2 <- model_performance$recall / (0.5*model_performance$pii + 0.5*model_performance$pci)
    DF$ROI2_one <- one_performance$recall /(0.5*one_performance$pii + 0.5*one_performance$pci)
    DF$ROI3 <- model_performance$recall / sqrt(model_performance$pii * model_performance$pci)
    DF$ROI3_one <- one_performance$recall / sqrt(one_performance$pii * one_performance$pci)
    
    result_root = './result/'
    write.csv(DF,paste(result_root,'rq2_',model_names[counter],'_',as.character(threshold),postfix,'.csv',sep=''),row.names = FALSE)
    
  }
}
model_paths_postfix <- c('./result/model_prediction_result_given_effort/KSETE_threshold',
                 './result/model_prediction_result_given_effort/MSMDA_threshold',
                 './result/model_prediction_result_given_effort/Kcore_threshold')
one_paths_postfix <- c('./result/model_prediction_result_given_effort/ONE_sameDatasetWithKSETE_threshold',
               './result/model_prediction_result_given_effort/ONE_sameDatasetWithMSMDA_threshold',
               './result/model_prediction_result_given_effort/ONE_sameDatasetWithteraPROMISE_threshold')
thresholds <- c(0.2,20)
for(threshold in thresholds){
  model_paths <- paste(model_paths_postfix,threshold,'.csv',sep = '')
  one_paths <- paste(one_paths_postfix,threshold,'.csv',sep = '')
  rq2_append_one(threshold,model_paths,one_paths,postfix = '')
}
