setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
source('One.r') 
run_ONE <- function(target_sets_root,res_root,cutoff,excluded_code_size_percentage){
  root = target_sets_root
  res_root = res_root
  files = list.files(root)
  excluded_code_size_percentage = 20
  res_folder <- paste(res_root,'cutoff',cutoff,'_excludedCodeSizePercentage',
                      excluded_code_size_percentage,'/',sep = '')
  dir.create(res_folder,showWarnings = FALSE)
  unlink(paste(res_folder,'*',sep = ''))
  
  for(file in files){
    
    res = one2(root,file,excluded_code_size_percentage,cutoff)
    write.csv(res,paste(res_folder,'/',file,'.csv',
                        sep = ''),row.names = FALSE)
  }
}


roots = c('./prediction_result/Amasaki15-NB/',
          './prediction_result/KSETE-master-master/detail_result/',
          './prediction_result/MSMDA_v1/detail_result/',
          './prediction_result/Kcore/')

res_roots = c('./prediction_result/ONE/ALLJURECZKO/',
              './prediction_result/ONE/KSETE/',
              './prediction_result/ONE/MSMDA/',
              './prediction_result/ONE/teraPROMISE/')

for (counter in 1:length(roots)){
  target_sets_root = roots[counter]
  res_root = res_roots[counter]
  excluded_code_size_percentage = 20
  cutoff = 20
  run_ONE(target_sets_root,res_root,cutoff,excluded_code_size_percentage)
  cutoff = 0.2
  run_ONE(target_sets_root,res_root,cutoff,excluded_code_size_percentage)
}




