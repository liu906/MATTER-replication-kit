setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
source('../One.r')
root = '../prediction_result/Amasaki15-NB/'

# res_root = 'ALLJURECZKO/'
excluded_code_size_percentages = seq(0,100,5)
for (counter in 1:length(roots)){
  files = list.files(root)
  for(excluded_code_size_percentage in excluded_code_size_percentages){
    cat(excluded_code_size_percentage,'\n')
    cutoff=20
    res_root <- paste('../prediction_result/One/ALLJURECZKO/',
                      'cutoff20_excludedCodeSizePercentage',as.character(excluded_code_size_percentage),'/',
                      sep = '')
    dir.create(res_root,showWarnings = FALSE)
    unlink(paste(res_root,'*',sep = ''))
    for(file in files){
      
      res = one2(root,file,excluded_code_size_percentage,cutoff)
      write.csv(res,paste(res_root,file,'_',as.character(excluded_code_size_percentage),'_',as.character(cutoff),'.csv',sep=''),row.names = FALSE)
    }
    cutoff=0.2
    res_root <- paste('../prediction_result/One/ALLJURECZKO/',
                      'cutoff0.2_excludedCodeSizePercentage',as.character(excluded_code_size_percentage),'/',
                      sep = '')
    dir.create(res_root,showWarnings = FALSE)
    unlink(paste(res_root,'*',sep = ''))
    for(file in files){
      res = one2(root,file,excluded_code_size_percentage,cutoff)
      write.csv(res,paste(res_root,file,'_',as.character(excluded_code_size_percentage),'_',as.character(cutoff),'.csv',sep=''),row.names = FALSE)
    }  
  }
  
}



source("../summary_performance.r")
thresholds = c(20,0.2)
modes <- c('SSC','SNM')
for(excluded_code_size_percentage in excluded_code_size_percentages){
  for(idx in 1:length(thresholds)){
    threshold <- thresholds[idx]
    mode <- modes[idx]
    res <- summaryPerformance2(root_path=paste('../prediction_result/One/ALLJURECZKO/cutoff',
                               as.character(threshold),'_excludedCodeSizePercentage',
                               as.character(excluded_code_size_percentage),'/',sep = ''),
                               threshold = -1, mode)
    write.csv(res,paste(
              './result/model_prediction_result_given_effort/ONE_sameDatasetWithALLJURECZKO_cutoff',
              as.character(threshold),'_excludedCodeSizePercentage',
              as.character(excluded_code_size_percentage),'.csv',sep = ''),row.names = FALSE)
  }
}




################################################

for(threshold in thresholds){
  counter <- 1
  for(excluded_code_size_percentage in excluded_code_size_percentages){
    file_name <- paste(
      './result/model_prediction_result_given_effort/ONE_sameDatasetWithALLJURECZKO_cutoff',
      as.character(threshold),'_excludedCodeSizePercentage',
      as.character(excluded_code_size_percentage),'.csv',sep = '')  
    data <- read.csv(file_name)
    data[is.na(data)] = 0
    
    data.df <- sapply(data,FUN=median)
    if(counter==1){
      res <-  data.df 
    }else{
      res <- rbind(res,data.df)
    }
    counter <- counter + 1
  }
  res <- data.frame(res)
  rownames(res) <- seq(0,100,5)
  res$codeSizePercentage <- seq(0,100,5)
  # options(repr.plot.width = 5, repr.plot.height = 3.5)
  indicators <- c('recall','roi','ifap')
  df <- res
  write.csv(df,paste('./result/One_medianPerformance_variesWithExcludedCodeSizePercentage_(cutoff=',as.character(threshold),').csv',sep = ''))
}


##################draw graph################################3

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(ggrepel)

drawConnectedLineGrahp <- function(df,codeSizePercentage,indicator,threshold,mode){
  studiedModels <- read.csv(paste('./result/alignment_',indicator,'_comparison_',threshold,'.csv',sep = ''),row.names = 1)
  studiedModels[is.na(studiedModels)] = 0
  studiedModels.df <- sapply(studiedModels,FUN=median)
  studiedModels.df <- data.frame(studiedModels.df)
  studiedModels.df$modelName <- rownames(studiedModels.df)
  studiedModels.df <- studiedModels.df[studiedModels.df$modelName %in% c('EASC_E','SC','Bellwether'),]
  
  
  rownames(studiedModels.df)[rownames(studiedModels.df) == 'CamargoCruz09.NB'] = 'CamargoCruz09_NB'
  rownames(studiedModels.df)[rownames(studiedModels.df) == 'Amasaki15.NB'] = 'Amasaki15_NB'
  rownames(studiedModels.df)[rownames(studiedModels.df) == 'Peters15.NB'] = 'Peters15_NB'
  
  df <- read.csv(paste('./result/One_medianPerformance_variesWithExcludedCodeSizePercentage_(cutoff=',as.character(threshold),').csv',sep = ''))
  # df <- df[1:17,]
  df %>%
    ggplot( aes(x=codeSizePercentage, y=df[,indicator])) +
    xlab("Excluded code size percentage") + ylab(toupper(indicator))+
    # xlab("Excluded code size percentage") + ylab('recall')+
    scale_x_continuous(breaks = pretty(df$codeSizePercentage, n = 6)) +
    geom_line( color="grey",size=1) +
    geom_point(shape=21, color="#387063", fill="#69b3a2", size=6) +
    scale_y_continuous(breaks = seq(round(min(df[,indicator])-0.1,1),
                                    round(max(df[,indicator])+0.1,1), by = round((max(df[,indicator])-min(df[,indicator])) / 5,2)))+
    
    theme_ipsum() +
    geom_hline(yintercept = studiedModels.df[rownames(studiedModels.df)[1],'studiedModels.df'],colour='#263238',linetype=5,show.legend=TRUE) +
    geom_text(size=6,aes(6,studiedModels.df[rownames(studiedModels.df)[1],'studiedModels.df'],label = rownames(studiedModels.df)[1], vjust = -0.5)) +
    geom_hline(yintercept = studiedModels.df[rownames(studiedModels.df)[2],'studiedModels.df'],colour='#263238',linetype=5,show.legend=TRUE) +
    geom_text(size=6,aes(18,studiedModels.df[rownames(studiedModels.df)[2],'studiedModels.df'],label = rownames(studiedModels.df)[2], vjust = -0.5)) +
    geom_hline(yintercept = studiedModels.df[rownames(studiedModels.df)[3],'studiedModels.df'],colour='#263238',linetype=5,show.legend=TRUE) +
    geom_text(size=6,aes(27,studiedModels.df[rownames(studiedModels.df)[3],'studiedModels.df'],label = rownames(studiedModels.df)[3], vjust = -0.5)) +
    geom_hline(yintercept = studiedModels.df[rownames(studiedModels.df)[4],'studiedModels.df'],colour='#263238',linetype=5,show.legend=TRUE) +
    geom_text(size=6,aes(35,studiedModels.df[rownames(studiedModels.df)[4],'studiedModels.df'],label = rownames(studiedModels.df)[4], vjust = -0.5)) +
    geom_hline(yintercept = studiedModels.df[rownames(studiedModels.df)[5],'studiedModels.df'],colour='#263238',linetype=5,show.legend=TRUE) +
    geom_text(size=6,aes(42,studiedModels.df[rownames(studiedModels.df)[5],'studiedModels.df'],label = rownames(studiedModels.df)[5], vjust = -0.5)) +
    geom_hline(yintercept = studiedModels.df[rownames(studiedModels.df)[6],'studiedModels.df'],colour='#263238',linetype=5,show.legend=TRUE) +
    geom_text(size=6,aes(48,studiedModels.df[rownames(studiedModels.df)[6],'studiedModels.df'],label = rownames(studiedModels.df)[6], vjust = -0.5)) +
    geom_hline(yintercept = studiedModels.df[rownames(studiedModels.df)[7],'studiedModels.df'],colour='#263238',linetype=5,show.legend=TRUE) +
    geom_text(size=6,aes(55,studiedModels.df[rownames(studiedModels.df)[7],'studiedModels.df'],label = rownames(studiedModels.df)[7], vjust = -0.5)) +
    geom_hline(yintercept = studiedModels.df[rownames(studiedModels.df)[8],'studiedModels.df'],colour='#263238',linetype=5,show.legend=TRUE) +
    geom_text(size=6,aes(62,studiedModels.df[rownames(studiedModels.df)[8],'studiedModels.df'],label = rownames(studiedModels.df)[8], vjust = -0.5)) +
    geom_hline(yintercept = studiedModels.df[rownames(studiedModels.df)[9],'studiedModels.df'],colour='#263238',linetype=5,show.legend=TRUE) +
    geom_text(size=6,aes(70,studiedModels.df[rownames(studiedModels.df)[9],'studiedModels.df'],label = rownames(studiedModels.df)[9], vjust = -0.5)) +
    geom_hline(yintercept = studiedModels.df[rownames(studiedModels.df)[10],'studiedModels.df'],colour='#263238',linetype=5,show.legend=TRUE) +
    geom_text(size=6,aes(76,studiedModels.df[rownames(studiedModels.df)[10],'studiedModels.df'],label = rownames(studiedModels.df)[10], vjust = -0.5)) +# geom_text(size=6,aes(84,studiedModels.df[rownames(studiedModels.df)[11],],label = rownames(studiedModels.df)[11], vjust = -0.5)) +# ggtitle(paste(this_scenario,this_model,'pUCS'))+
    theme_bw() + 
    theme(axis.text = element_text( size = rel(2)),
          title = element_text( size = rel(2)))
}
indicators <- c('recall','roi','ifap2')
# indicators <- c('recall')
for (indicator in indicators){
  drawConnectedLineGrahp(df,codeSizePercentage,indicator,threshold = 20,mode = 'SSC')
  ggsave(paste('./result/excludedCodeSizePercentage/','One_',mode = 'SSC','_',indicator,'.svg',sep = ''))
  
}
for (indicator in indicators){
  drawConnectedLineGrahp(df,codeSizePercentage,indicator,threshold = 0.2,mode = 'SNM')
  ggsave(paste('./result/excludedCodeSizePercentage/','One_',mode = 'SNM','_',indicator,'.svg',sep = ''))
}






