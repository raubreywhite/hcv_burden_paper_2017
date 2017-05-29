clean.output <- function(x){
  
  index <- which(names(x)=="IVDU_HCV_AI"|
                   names(x)=="IVDU_HCV_C"|
                   names(x)=="IVDU_HCV_CI"|
                   names(x)=="IVDU_HCC"|
                   names(x)=="IVDU_LT"|
                   names(x)=="IVDU_S"|
                   names(x)=="IVDU_S_C")
  x["IVDU.num"] <- sum(x[index])
  
  index <- which(names(x)=="IVDU_HCV_AI"|
                   names(x)=="IVDU_HCV_C"|
                   names(x)=="IVDU_HCV_CI")
  x["IVDU.num.hepc"] <- sum(x[index])
  
  index <- which(names(x)=="NONIVDU_HCV_AI"|
                   names(x)=="NONIVDU_HCV_C"|
                   names(x)=="NONIVDU_HCV_CI")
  x["NONIVDU.num.hepc"] <- sum(x[index])
  
  index <- which(names(x)=="new_IVDU_M"|
                   names(x)=="new_NONIVDU_M")
  x["new_M"] <- sum(x[index])
  
  index <- which(names(x)=="new_IVDU_LT"|
                   names(x)=="new_NONIVDU_LT")
  x["new_LT"] <- sum(x[index])
  
  index <- which(names(x)=="IVDU_HCV_C"|
                   names(x)=="NONIVDU_HCV_C")
  x["HCV_C"] <- sum(x[index])
  
  index <- which(names(x)=="IVDU_HCC"|
                   names(x)=="NONIVDU_HCC")
  x["HCC"] <- sum(x[index])
  
  index <- which(names(x)=="TRANS_IVDU_HCV_C_X_IVDU_HCC"|
                   names(x)=="TRANS_IVDU_S_C_X_IVDU_HCC"|
                   names(x)=="TRANS_NONIVDU_HCV_C_X_NONIVDU_HCC"|
                   names(x)=="TRANS_NONIVDU_S_C_X_NONIVDU_HCC")
  x["new_HCC"] <- sum(x[index])
  
  index <- which(names(x)=="IVDU_S_C"|
                   names(x)=="NONIVDU_S_C"|
                   names(x)=="IVDU_HCV_C"|
                   names(x)=="NONIVDU_HCV_C")
  #index <- which(names(x)=="NONIVDU_S_C")
  x["CIRR"] <- sum(x[index])
  
  index <- which(names(x)=="TRANS_IVDU_HCV_CI_X_IVDU_HCV_C"|
                   names(x)=="TRANS_NONIVDU_HCV_CI_X_NONIVDU_HCV_C")
  #index <- which(names(x)=="NONIVDU_S_C")
  x["TRANS_CI_X_CIRR"] <- sum(x[index])
  
  index <- which(names(x)=="TRANS_IVDU_HCV_C_X_IVDU_S_C"|
                   names(x)=="TRANS_NONIVDU_HCV_C_X_NONIVDU_S_C")
  x["TRANS_HCV_C_X_S_C"] <- sum(x[index])
  
  index <- which(names(x)=="TRANS_IVDU_HCV_CI_X_IVDU_S"|
                   names(x)=="TRANS_NONIVDU_HCV_CI_X_NONIVDU_S")
  x["TRANS_HCV_CI_X_S"] <- sum(x[index])
  
  index <- which(names(x)=="TRANS_IVDU_HCV_C_X_IVDU_M"|
                   names(x)=="TRANS_NONIVDU_HCV_C_X_NONIVDU_M" |
                   names(x)=="TRANS_IVDU_S_C_X_IVDU_M"|
                   names(x)=="TRANS_NONIVDU_S_C_X_NONIVDU_M")
  x["TRANS_C_X_M"] <- sum(x[index])
  
  index <- which(names(x)=="TRANS_IVDU_HCC_X_IVDU_M"|
                   names(x)=="TRANS_NONIVDU_HCC_X_NONIVDU_M")
  x["TRANS_HCC_X_M"] <- sum(x[index]) 
  
  index <- which(names(x) %in% 
                   c("IVDU_S","IVDU_HCV_AI","IVDU_HCV_CI","IVDU_HCV_C","IVDU_S_C","IVDU_HCC","IVDU_LT"))
  x["NUM_IVDU"] <- sum(x[index]) 
  
  index <- which(names(x) %in% 
                   c("NONIVDU_S","NONIVDU_HCV_AI","NONIVDU_HCV_CI","NONIVDU_HCV_C","NONIVDU_S_C","NONIVDU_HCC","NONIVDU_LT"))
  x["NUM_NONIVDU"] <- sum(x[index])
  
  # new ones for intervention
  index <- which(names(x) %in% 
                   c("IVDU_HCV_AI","IVDU_HCV_CI","IVDU_HCV_C","IVDU_HCC","IVDU_LT",
                     "NONIVDU_HCV_AI","NONIVDU_HCV_CI","NONIVDU_HCV_C","NONIVDU_HCC","NONIVDU_LT"))
  x["NUM_HCV"] <- sum(x[index])
  
  index <- which(names(x) %in% 
                   c("IVDU_HCV_C","IVDU_S_C",
                     "NONIVDU_HCV_C","NONIVDU_S_C"))
  x["NUM_CIRR"] <- sum(x[index])
  
  index <- which(names(x) %in% 
                   c("IVDU_HCC","NONIVDU_HCC"))
  x["NUM_HCC"] <- sum(x[index])
  
  index <- which(names(x) %in% 
                   c("IVDU_HCV_AI","IVDU_HCV_CI","IVDU_HCV_C","IVDU_HCC","IVDU_LT"))
  x["NUM_IVDU_HCV"] <- sum(x[index])
  
  index <- which(names(x)=="TRANS_IVDU_HCV_CI_X_IVDU_S"|
                   names(x)=="TRANS_NONIVDU_HCV_CI_X_NONIVDU_S")
  x["TREATED_HCV_CI"] <- sum(x[index])/treatmentSuccess
  
  index <- which(names(x)=="TREATED_HCV_CI")
  x["COSTTREATED_HCV_CI"] <- sum(x[index])*treatmentCost
  
  x["year"] <- x["time"]
  x <- x[-1]
  x <- x[which(names(x)!="time")]
  return(x)
}

