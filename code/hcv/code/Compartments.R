

#combinationsIVDU <- expand.grid(seq(0.01,0.41,0.05),seq(0.01,0.41,0.05))
#names(combinationsIVDU) <- c("prob.relapse","prob.cease")
#combinationsIVDU$est.ivdu <- 0
#combinationsIVDU$est.exivdu <- 0
#combinationsIVDU <- data.table::data.table(combinationsIVDU)


AddingIVDUCalculation <- function(probs,previous.ivdu,previous.exivdu,previous.curedivdu,previous.add.year){
  prob.relapse <- probs[1]
  prob.cease <- probs[2]
  prob.cure <- probs[3]
  
  est.ivdu <- (previous.ivdu+previous.add.year)*(1-prob.cease-prob.cure)+previous.exivdu*prob.relapse
  est.ivdu <- est.ivdu*(1-yearlyParams$deathProbExcessIVDU)
  est.exivdu <- (previous.ivdu+previous.add.year)*(prob.cease)+previous.exivdu*(1-prob.relapse)
  est.curedivdu <- (previous.ivdu+previous.add.year)*(prob.cure) + previous.curedivdu
  
  return(list(
    numPWID=est.ivdu,
    numEXPWID=est.exivdu,
    numCUREDPWID=est.curedivdu,
    added=previous.add.year,
    prob.relapse=prob.relapse,
    prob.cease=prob.cease,
    prob.cure=prob.cure))
}

internAddingIVDUs <- function(probs,previous.ivdu,previous.exivdu,previous.curedivdu,year){
  goodYear <- year
  if(year>=2012) goodYear <- 2012
  if(year<=1974) goodYear <- 1974
  yearBefore <- goodYear-1
  
  previous.add.year <- pwid[year==goodYear]$add.year

  if(year<=1995){
    previous.add.year <- previous.add.year*(1+probs[3])
  } else if(year<=2001){
    previous.add.year <- previous.add.year*(1+probs[4])
  }else {
    previous.add.year <- previous.add.year*(1+probs[5])
  }
  probs <- probs[1:2]
  
  return(AddingIVDUCalculation(c(probs,0.025),previous.ivdu,previous.exivdu,previous.curedivdu,previous.add.year))
}


IVDUcalc <- function(probs){
  vals <- data.table(year=1972:2012,
                     numPWID=as.numeric(numPWID1973), 
                     numEXPWID=as.numeric(numEXPWID1973),
                     numCUREDPWID=as.numeric(0.0))
  
  ivduParams <- data.table(year=1972:2012,
                           added=as.numeric(0.0),
                           prob.relapse=as.numeric(0.0),
                           prob.cease=as.numeric(0.0),
                           prob.cure=as.numeric(0.0))
  
  for(i in 2:nrow(vals)){
    temp <- internAddingIVDUs(probs, 
                              previous.ivdu=vals[i-1]$numPWID, 
                              previous.exivdu=vals[i-1]$numEXPWID, 
                              previous.curedivdu=vals[i-1]$numCUREDPWID,
                              year=vals[i]$year)
    vals[i,numPWID:=temp$numPWID]
    vals[i,numEXPWID:=temp$numEXPWID]
    vals[i,numCUREDPWID:=temp$numCUREDPWID]
    
    ivduParams[i,added:=temp$added]
    ivduParams[i,prob.relapse:=temp$prob.relapse]
    ivduParams[i,prob.cease:=temp$prob.cease]
    ivduParams[i,prob.cure:=temp$prob.cure]
  }
  vals <- melt.data.table(vals,id="year")
  obs <- merge(vals,pwidTargets,by=c("year","variable"))
  return(list(obs=obs,ivduParams=ivduParams))
}

IVDUcalcError <- function(probs){
  obs <- IVDUcalc(probs)[["obs"]]
  obs[,dif:=(value-obs)^2]
  obs[,dif:=dif/obs]
  return(obs[,.(dif=mean(dif))][[1]])
}



xxxxxxxxxxxxAddingIVDUsErrorFN <- function(probs,previous.ivdu,previous.exivdu,final.ivdu,final.exivdu,previous.add.year){
  prob.relapse <- probs[1]
  prob.cease <- probs[2]
  
  est.ivdu <- (previous.ivdu+previous.add.year)*(1-prob.cease)+previous.exivdu*prob.relapse
  est.exivdu <- (previous.ivdu+previous.add.year)*(prob.cease)+previous.exivdu*(1-prob.relapse)
  est.ivdu <- est.ivdu*(1-yearlyParams$deathProbExcessIVDU)

  error <- 10*abs(est.ivdu-final.ivdu)+abs(est.exivdu-final.exivdu)
  return(error)
}

xxxxxxxxxxxxxinternAddingIVDUs <- function(year,previous.ivdu=NULL,previous.exivdu=NULL){
  goodYear <- year
  if(year>=2012) goodYear <- 2012
  if(year<=1974) goodYear <- 1974
  yearBefore <- goodYear-1
  
  add.year <- pwid[year==goodYear]$add.year
  final.ivdu <- pwid[year==goodYear]$final.ivdu
  final.exivdu <- pwid[year==goodYear]$final.exivdu
  previous.add.year <- pwid[year==yearBefore]$add.year
  if(is.null(previous.ivdu)) previous.ivdu <- pwid[year==yearBefore]$final.ivdu
  if(is.null(previous.exivdu)) previous.exivdu <- pwid[year==yearBefore]$final.exivdu
  
  
  g <- optim(c(0.1,0.1), AddingIVDUsErrorFN, method="L-BFGS-B", lower=0.01, upper=0.4, control=list(
    maxit=10
    ),
    previous.ivdu=previous.ivdu,previous.exivdu=previous.exivdu,final.ivdu=final.ivdu,final.exivdu=final.exivdu,previous.add.year=previous.add.year
  )
  
  return(list(previous.add.year=previous.add.year,add.year=add.year,final.ivdu=final.ivdu,final.exivdu=final.exivdu,prob.relapse=g$par[1],prob.cease=g$par[2]))
  
  for(i in 1:nrow(combinationsIVDU)){
    combinationsIVDU[i,est.ivdu:= (previous.ivdu+previous.add.year)*(1-prob.cease)+previous.exivdu*prob.relapse]
    combinationsIVDU[i,est.exivdu:= (previous.ivdu+previous.add.year)*(prob.cease)+previous.exivdu*(1-prob.relapse)]
  }
  combinationsIVDU[,dif.ivdu := abs(final.ivdu-est.ivdu)]
  combinationsIVDU[,dif.exivdu := abs(final.exivdu-est.exivdu)]
  combinationsIVDU[,error := dif.ivdu] #*2+combinations$dif.exivdu
  retval <- combinationsIVDU[error==min(error)][1]

  
  if(year>=2016){
    #add.year <- add.year*(1-newIVDUReduction)^(year-2015)
    #final.ivdu <- final.ivdu*(1-totalIVDUReduction)^(year-2015)
  }
  return(list(previous.add.year=previous.add.year,add.year=add.year,final.ivdu=final.ivdu,final.exivdu=final.exivdu,prob.relapse=retval$prob.relapse,prob.cease=retval$prob.cease))
}


CleanParametersTreatSpecific <- function(param,treat1=0,treat2=0,yearlyParams){
  param <- copy(param)
  
  if(treat1==0){
    # everyone returns to HCV+  (remove t->s)
    param <- param[!intersect(grep("^T",State),grep("^S",Leaving))]
  } else {
    # everyone returns to S (remove t->hcv)
    param <- param[!intersect(grep("^T",State),grep("^H",Leaving))]
  }
  
  # duplicating informatino for missing rows
  inf <- param[param$State=="S" & param$Leaving=="HCV_AI",]
  inf$State <- "S_C"
  inf$Leaving <- "HCV_C"
  param <- rbind(param,inf)
  
  inf <- param[param$State=="HCV_C" & param$Leaving=="HCC",]
  inf$State <- "S_C"
  param <- rbind(param,inf)
  
  inf <- param[param$State=="HCV_C" & param$Leaving=="LT",]
  inf$State <- "S_C"
  param <- rbind(param,inf)
  
  inf <- param[param$State=="HCV_C" & param$Leaving=="M",]
  inf$State <- "S_C"
  inf$Prob <- inf$Prob/yearlyParams$SCMortalityReduction
  param <- rbind(param,inf)
  
  remaining.parameters <- param[,list(Prob=1-sum(Prob)),by=list(State)]
  remaining.parameters$Leaving <- "D"
  remaining.parameters$Prob <- yearlyParams$deathProbNormal+yearlyParams$deathProbExcessIVDU
  cleaned.parameters <- rbind(param,remaining.parameters)

  cleaned.parameters$State <- paste("IVDU_",cleaned.parameters$State,sep="")
  cleaned.parameters$Leaving <- paste("IVDU_",cleaned.parameters$Leaving,sep="")
  
  cleaned.ivdu <- cleaned.parameters#rbind(cleaned.parameters,cleaned.parameters.nonivud)
  cleaned.parameters$State <- gsub("IVDU_","NONIVDU_", cleaned.parameters$State)
  cleaned.parameters$Leaving <- gsub("IVDU_","NONIVDU_", cleaned.parameters$Leaving)
 
  cleaned.nonivdu <- cleaned.parameters#rbind(cleaned.parameters,cleaned.parameters.nonivud)
  cleaned.nonivdu$Prob[cleaned.nonivdu$Leaving=="NONIVDU_D"] <- yearlyParams$deathProbNormal
  
  # remove the ones that can't infect
  cleaned.nonivdu <- cleaned.nonivdu[!(cleaned.nonivdu$State=="NONIVDU_S" & cleaned.nonivdu$Leaving=="NONIVDU_HCV_AI" ),]
  cleaned.nonivdu <- cleaned.nonivdu[!(cleaned.nonivdu$State=="NONIVDU_S_C" & cleaned.nonivdu$Leaving=="NONIVDU_HCV_C" ),]
  
  # remove M->D
  cleaned.ivdu <- cleaned.ivdu[!(cleaned.ivdu$State=="IVDU_M" & cleaned.ivdu$Leaving=="IVDU_D" ),]
  cleaned.nonivdu <- cleaned.nonivdu[!(cleaned.nonivdu$State=="NONIVDU_M" & cleaned.nonivdu$Leaving=="NONIVDU_D" ),]
  
  # removing what is not possible
  ### to do, remove liver transplant in pwid
  cleaned.ivdu <- cleaned.ivdu[cleaned.ivdu$Leaving!="IVDU_LT",]
  
  # reducing likelihood of PWIDS to get treatment
  cleaned.ivdu$Prob[cleaned.ivdu$State=="IVDU_HCV_CI" & cleaned.ivdu$Leaving %in% c("IVDU_T_CI")] <- cleaned.ivdu$Prob[cleaned.ivdu$State=="IVDU_HCV_CI" & cleaned.ivdu$Leaving %in% c("IVDU_T_CI")]*yearlyParams$probTreatmentPWID
  cleaned.ivdu$Prob[cleaned.ivdu$State=="IVDU_HCV_C" & cleaned.ivdu$Leaving %in% c("IVDU_T_C")] <- cleaned.ivdu$Prob[cleaned.ivdu$State=="IVDU_HCV_C" & cleaned.ivdu$Leaving %in% c("IVDU_T_C")]*yearlyParams$probTreatmentPWID
  
  for(i in 1:2){
  	if(i==1){
  		cleaned.parameters <- cleaned.ivdu
  	} else cleaned.parameters <- cleaned.nonivdu
  	##
	  zero.prob <- cleaned.parameters[Prob==0,c("State","Leaving"),with=FALSE]
	  setnames(zero.prob,"Leaving","Leaving2")
	  remaining.parameters <- cleaned.parameters[,list(Prob=1-sum(Prob)),by=list(State)]
	  remaining.parameters[,Leaving:=State]
	  #remaining.parameters$Leaving[remaining.parameters$Leaving %in% zero.prob$State[j]] <- zero.prob$Leaving[j]
	  remaining.parameters <- merge(remaining.parameters,zero.prob,by="State",all.x=TRUE)
	  remaining.parameters[!is.na(Leaving2),Leaving:=Leaving2]
	  remaining.parameters[,Leaving2:=NULL]
	  #for(j in 1:nrow(zero.prob)) remaining.parameters$Leaving[remaining.parameters$Leaving==zero.prob$State[j]] <- zero.prob$Leaving[j]
	  ##
	  cleaned.parameters <- rbind(cleaned.parameters,remaining.parameters)
	  
  	cleaned.parameters <- data.frame(cleaned.parameters)
  	cleaned.parameters <- cleaned.parameters[order(cleaned.parameters$State,cleaned.parameters$Leaving),]
  	cleaned.parameters <- cleaned.parameters[cleaned.parameters$Prob>0,]
	  
	  if(i==1){
  		cleaned.ivdu <- cleaned.parameters
  	} else cleaned.nonivdu <- cleaned.parameters
  }
  cleaned.parameters <- rbind(cleaned.ivdu,cleaned.nonivdu)
  
  cleaned.parameters <- data.table(cleaned.parameters)
  prob.sum <- cleaned.parameters[,list(prob.sum=sum(Prob)),by=list(State)]
  cleaned.parameters <- merge(cleaned.parameters,prob.sum,by="State")
  cleaned.parameters$Prob <- cleaned.parameters$Prob/cleaned.parameters$prob.sum
  cleaned.parameters[,prob.sum:=NULL]
  cleaned.parameters <- data.frame(cleaned.parameters)
  
  cleanedParameters <- data.table(cleaned.parameters)
  cleanedParameters[,type:="PWID"]
  cleanedParameters[grep("^N",State),type:="Ex-PWID"]
  cleanedParameters[,typeLeaving:="PWID"]
  cleanedParameters[grep("^N",Leaving),typeLeaving:="Ex-PWID"]
  
  cleanedParameters[,State:=gsub("^IVDU_","",State)]
  cleanedParameters[,Leaving:=gsub("^IVDU_","",Leaving)]
  cleanedParameters[,State:=gsub("^NONIVDU_","",State)]
  cleanedParameters[,Leaving:=gsub("^NONIVDU_","",Leaving)]
  
  cleanedParameters[,treat1:=treat1]
  cleanedParameters[,treat2:=treat2]
  setnames(cleanedParameters,"State","state")
  setnames(cleanedParameters,"Leaving","leaving")
  setnames(cleanedParameters,"Prob","prob")
  ##
  MD <- data.table("leaving"=c("D","M"),type=c("PWID","PWID","Ex-PWID","Ex-PWID"),treat1=treat1,treat2=treat2,prob=1)
    #cleanedParameters[leaving %in% c("M","D"),.(prob=1),by=.(leaving,type,typeLeaving,treat1,treat2)]

  MD[,typeLeaving:=type]
  MD[,state:=leaving]
  cleanedParameters <- rbind(MD,cleanedParameters)
  x <- cleanedParameters[type=="Ex-PWID"]
  x[,type:="Cured PWID"]
  x[,typeLeaving:="Cured PWID"]
  cleanedParameters <- rbind(x,cleanedParameters)
  return(cleanedParameters)
}


CleanParametersYearSpecific <- function(param,year=1999,yearlyParams){

  #loop <- TRUE
  #while(loop){
  #  num.relapse <- num.nonivdu*prob.relapse
  #  net.gain.loss <- final.ivdu-num.ivdu-add.year
  #  num.cease <- num.ivdu+add.year+num.relapse-final.ivdu
  
  #  prob.cease <- num.cease/num.ivdu
  #  #print(prob.cease)
  #  #print(prob.relapse)
  #  if(prob.cease <0){
  #    prob.relapse <- prob.relapse+0.01
  #  } else loop <- FALSE
  #  if(prob.relapse > 0.4 & loop){
  #    prob.cease <- 0.001
  #    loop <- FALSE
  #  }
  #}
	
	goodYear <- year
	if(year>=2014) goodYear <- 2014
	p <- vector("list",100)
	
	for(age in 1:100){
  	yearlyParams$deathProbNormal <- mortalityRates[Year==goodYear & Age==age]$Total
  	
    CleanParamT00 <- CleanParametersTreatSpecific(param=param,yearlyParams=yearlyParams, treat1=0, treat2=0)
    CleanParamT01 <- copy(CleanParamT00)
    CleanParamT01[,treat2:=1]
    CleanParamT10 <- CleanParametersTreatSpecific(param=param,yearlyParams=yearlyParams, treat1=1, treat2=0)
    CleanParamT11 <- copy(CleanParamT10)
    CleanParamT11[,treat2:=1]
    CleanParam <- rbind(CleanParamT00,CleanParamT01,CleanParamT10,CleanParamT11)
    
    CleanParamG1 <- copy(CleanParam)
    CleanParamG1[,geno:=1]
    CleanParamG2 <- copy(CleanParam)
    CleanParamG2[,geno:=2]
    CleanParamG3 <- copy(CleanParam)
    CleanParamG3[,geno:=3]
    
    p[[age]] <- rbind(CleanParamG1,CleanParamG2,CleanParamG3)
    p[[age]][,age :=  age]
	}
	p <- rbindlist(p)
  return(p)
}

RunOneYear <- function(xstartDT,fullParam,yearlyParams,year=1999,yearlyIncreaseTreatment=0.03,infectiousnessShift1=(1-(1-0.58)/2),infectiousnessShift2=1,yearlyIncreaseLT=0.03,runDraw=FALSE){
  num.infectious <- sum(xstartDT[state %in% c("HCV_AI","HCV_CI","HCV_C") & type=="PWID"]$num)
  #num.pop <- sum(xstartDT[!state %in% c("M","D")]$num)
  num.pop <- sum(xstartDT[!state %in% c("M","D") & type=="PWID"]$num)
  
  prop.infectious <- num.infectious/num.pop
  num.infectious.community <- 10*prop.infectious*injectingGINI[year]
  
  fullParameters <- copy(fullParam)
  fullParameters[,old:=as.numeric(NA)]
  fullParameters[,new:=as.numeric(NA)]
  fullParameters[state=="S" & leaving=="HCV_AI" & type=="PWID",old:=prob]
  fullParameters[state=="S" & leaving=="HCV_AI" & type=="PWID",new:=1-(1-old)^num.infectious.community]
  # needle program
  coverage <- needleCoverage[year]
  
  fullParameters[state=="S" & leaving=="HCV_AI" & type=="PWID",new:=new*(1-infectiousnessShift1*coverage)]
  #print(mean(fullParameters[state=="S" & leaving=="HCV_AI" & type=="PWID"]$new))
  fullParameters[state=="S" & leaving=="HCV_AI" & type=="PWID",addition:=new-old]
  fullParameters[,addition:=mean(addition,na.rm=T),by=.(type)]
  
  # cant go past 100%
  fullParameters[state=="S" & leaving=="S" & type=="PWID",free:=prob]
  fullParameters[,free:=mean(free,na.rm=T),by=.(type)]
  fullParameters[free<addition,addition:=free]
  
  fullParameters[state=="S" & leaving=="HCV_AI" & type=="PWID",prob:=prob+addition]
  fullParameters[state=="S" & leaving=="S" & type=="PWID",prob:=prob-addition]
  fullParameters[,old:=NULL]
  fullParameters[,new:=NULL]
  fullParameters[,addition:=NULL]
  fullParameters[,free:=NULL]
  
  
  fullParameters[,old:=as.numeric(NA)]
  fullParameters[,new:=as.numeric(NA)]
  fullParameters[state=="S_C" & leaving=="HCV_C" & type=="PWID",old:=prob]
  fullParameters[state=="S_C" & leaving=="HCV_C" & type=="PWID",new:=1-(1-old)^num.infectious.community]
  # needle program
  fullParameters[state=="S_C" & leaving=="HCV_C" & type=="PWID",new:=new*(1-infectiousnessShift1*coverage)]
  
  fullParameters[state=="S_C" & leaving=="HCV_C" & type=="PWID",addition:=new-old]
  fullParameters[,addition:=mean(addition,na.rm=T),by=.(type)]
  
  # cant go past 100%
  fullParameters[state=="S_C" & leaving=="S_C" & type=="PWID",free:=prob]
  fullParameters[,free:=mean(free,na.rm=T),by=.(type)]
  fullParameters[free<addition,addition:=free]
  
  fullParameters[state=="S_C" & leaving=="HCV_C" & type=="PWID",prob:=prob+addition]
  fullParameters[state=="S_C" & leaving=="S_C" & type=="PWID",prob:=prob-addition]
  fullParameters[,old:=NULL]
  fullParameters[,new:=NULL]
  fullParameters[,addition:=NULL]
  fullParameters[,free:=NULL]
  #old <- fullParameters[state=="S" & leaving=="HCV_AI"]$prob
  #new <- 1-(1-old)^num.infectious.community
  #skeleton.parameters[State=="S" & Leaving=="HCV_AI",Prob:=new]
  
  #### INCREASED TREAMTNET OVER TIME
  if(year>=2004){
    
    fullParameters[,old:=as.numeric(NA)]
    fullParameters[,new:=as.numeric(NA)]
    fullParameters[state=="HCV_CI" & leaving=="T_CI",old:=prob]
    fullParameters[state=="HCV_CI" & leaving=="T_CI",new:=old*(1+yearlyIncreaseTreatment*(year-2003))]
    fullParameters[state=="HCV_CI" & leaving=="T_CI",addition:=new-old]
    fullParameters[,addition:=mean(addition,na.rm=T),by=.(type)]
    
    # cant go past 100%
    fullParameters[state=="HCV_CI" & leaving=="HCV_CI",free:=prob]
    fullParameters[,free:=mean(free,na.rm=T),by=.(type)]
    fullParameters[free<addition,addition:=free]
    
    fullParameters[state=="HCV_CI" & leaving=="T_CI",prob:=prob+addition]
    fullParameters[state=="HCV_CI" & leaving=="HCV_CI",prob:=prob-addition]
    fullParameters[,old:=NULL]
    fullParameters[,new:=NULL]
    fullParameters[,addition:=NULL]
    fullParameters[,free:=NULL]
    if(TRUE){
    
    fullParameters[,old:=as.numeric(NA)]
    fullParameters[,new:=as.numeric(NA)]
    fullParameters[state=="HCV_C" & leaving=="T_C",old:=prob]
    fullParameters[state=="HCV_C" & leaving=="T_C",new:=old*(1+yearlyIncreaseTreatment*(year-2003))]
    fullParameters[state=="HCV_C" & leaving=="T_C",addition:=new-old]
    fullParameters[,addition:=mean(addition,na.rm=T),by=.(type)]
    
    # cant go past 100%
    fullParameters[state=="HCV_C" & leaving=="HCV_C",free:=prob]
    fullParameters[,free:=mean(free,na.rm=T),by=.(type)]
    fullParameters[free<addition,addition:=free]
    
    fullParameters[state=="HCV_C" & leaving=="T_C",prob:=prob+addition]
    fullParameters[state=="HCV_C" & leaving=="HCV_C",prob:=prob-addition]
    fullParameters[,old:=NULL]
    fullParameters[,new:=NULL]
    fullParameters[,addition:=NULL]
    fullParameters[,free:=NULL]
    }
  }
  
  #### INCREASED LT OVER TIME
  if(year>=2000){
    
    fullParameters[,old:=as.numeric(NA)]
    fullParameters[,new:=as.numeric(NA)]
    fullParameters[type!="PWID" & state=="HCV_C" & leaving=="LT",old:=prob]
    fullParameters[type!="PWID" & state=="HCV_C" & leaving=="LT",new:=old*(1+yearlyIncreaseLT*(year-1999))]
    fullParameters[type!="PWID" & state=="HCV_C" & leaving=="LT",addition:=new-old]
    fullParameters[,addition:=mean(addition,na.rm=T),by=.(type)]
    
    # cant go past 100%
    fullParameters[type!="PWID" & state=="HCV_C" & leaving=="HCV_C",free:=prob]
    fullParameters[,free:=mean(free,na.rm=T),by=.(type)]
    fullParameters[free<addition,addition:=free]
    
    fullParameters[type!="PWID" & state=="HCV_C" & leaving=="LT",prob:=prob+addition]
    fullParameters[type!="PWID" & state=="HCV_C" & leaving=="HCV_C",prob:=prob-addition]
    fullParameters[,old:=NULL]
    fullParameters[,new:=NULL]
    fullParameters[,addition:=NULL]
    fullParameters[,free:=NULL]

    #
    fullParameters[,old:=as.numeric(NA)]
    fullParameters[,new:=as.numeric(NA)]
    fullParameters[type!="PWID" & state=="S_C" & leaving=="LT",old:=prob]
    fullParameters[type!="PWID" & state=="S_C" & leaving=="LT",new:=old*(1+yearlyIncreaseLT*(year-1999))]
    fullParameters[type!="PWID" & state=="S_C" & leaving=="LT",addition:=new-old]
    fullParameters[,addition:=mean(addition,na.rm=T),by=.(type)]
    
    # cant go past 100%
    fullParameters[type!="PWID" & state=="S_C" & leaving=="S_C",free:=prob]
    fullParameters[,free:=mean(free,na.rm=T),by=.(type)]
    fullParameters[free<addition,addition:=free]
    
    fullParameters[type!="PWID" & state=="S_C" & leaving=="LT",prob:=prob+addition]
    fullParameters[type!="PWID" & state=="S_C" & leaving=="S_C",prob:=prob-addition]
    fullParameters[,old:=NULL]
    fullParameters[,new:=NULL]
    fullParameters[,addition:=NULL]
    
    #
    fullParameters[,old:=as.numeric(NA)]
    fullParameters[,new:=as.numeric(NA)]
    fullParameters[type!="PWID" & state=="HCC" & leaving=="LT",old:=prob]
    fullParameters[type!="PWID" & state=="HCC" & leaving=="LT",new:=old*(1+yearlyIncreaseLT*(year-1999))]
    fullParameters[type!="PWID" & state=="HCC" & leaving=="LT",addition:=new-old]
    fullParameters[,addition:=mean(addition,na.rm=T),by=.(type)]
    
    # cant go past 100%
    fullParameters[type!="PWID" & state=="HCC" & leaving=="LT",free:=prob]
    fullParameters[,free:=mean(free,na.rm=T),by=.(type)]
    fullParameters[free<addition,addition:=free]
                   
    fullParameters[type!="PWID" & state=="HCC" & leaving=="LT",prob:=prob+addition]
    fullParameters[type!="PWID" & state=="HCC" & leaving=="HCC",prob:=prob-addition]
    fullParameters[,old:=NULL]
    fullParameters[,new:=NULL]
    fullParameters[,addition:=NULL]
  }
  
  numPWID=sum(xstartDT[type=="PWID" & state!="M" & state!="D"]$num)
  numEXPWID=sum(xstartDT[type=="Ex-PWID" & state!="M" & state!="D"]$num)
  #p <- AddingIVDUs(year,previous.ivdu=numPWID,previous.exivdu=numEXPWID)
  
  newPWID <- copy(ageByGenoDist[[year]])
  if(runDraw){
    val <- rbinom(nrow(newPWID),rep(round(AddingIVDUs[year]),nrow(newPWID)),newPWID$prop)
    newPWID[,newPWID:=val]
  } else {
    newPWID[,newPWID:=prop*AddingIVDUs[year]]
  }
  newPWID[,prop:=NULL]
  
  #11287.07
  #sum(xstartDT$num)
  #726
  #sum(newPWID$newPWID)
  together <- merge(xstartDT,newPWID,by=c("geno","treat1","treat2","age"))
  together[state=="S" & type=="PWID",num:=num+newPWID]
  together[,newPWID:=NULL]
  #12013.07
  #sum(together$num)
  #sum(together[state=="M" & type=="Ex-PWID"]$num)
  #sum(together[state=="D" & type=="Ex-PWID"]$num)
  
  together <- merge(together,fullParameters,by=c("state","type","geno","treat1","treat2","age"),allow.cartesian = TRUE,all.x=TRUE)
  
  togetherPWIDtoEX <- together[(!state %in% c("M","D")) & (!leaving %in% c("M","D","HCV_AI")) & type=="PWID"]
  togetherPWIDtoCURED <- copy(togetherPWIDtoEX)
  togetherEXtoPWID <- together[(!state %in% c("M","D")) & (!leaving %in% c("M","D","HCV_AI")) & type=="Ex-PWID"]
  togetherPWIDtoEX[,typeLeaving:="Ex-PWID"]
  togetherPWIDtoCURED[,typeLeaving:="Cured PWID"]
  togetherPWIDtoEX[,prob:=prob*yearlyParams$probCease]
  togetherPWIDtoCURED[,prob:=prob*yearlyParams$probCure]
  togetherEXtoPWID[,prob:=prob*yearlyParams$probRelapse]
  togetherEXtoPWID[,typeLeaving:="PWID"]
  together[(!state %in% c("M","D")) & (!leaving %in% c("M","D","HCV_AI")) & type=="PWID",prob:=prob*(1-yearlyParams$probCease-yearlyParams$probCure)]
  together[(!state %in% c("M","D")) & (!leaving %in% c("M","D","HCV_AI")) & type=="Ex-PWID",prob:=prob*(1-yearlyParams$probRelapse)]
  
  together <- rbind(together,togetherPWIDtoEX,togetherPWIDtoCURED,togetherEXtoPWID)
  
 # saveRDS(together,file=paste0("results/res_",year,".RDS"))
  together[prob<0,prob:=0]
  ## test
  if(runDraw){
    val <- rbinom(nrow(together),round(together$num),together$prob)
    together[,num:=val]
    transition <- together[state!=leaving,.(num=sum(num)),by=.(geno,treat1,treat2,age,state,leaving,typeLeaving)]
  } else {
    transition <- together[state!=leaving,.(num=sum(num*prob)),by=.(geno,treat1,treat2,age,state,leaving,typeLeaving)]
  }
  setnames(transition,"state","start")
  setnames(transition,"typeLeaving","type")
  setnames(transition,"leaving","end")
  transition[,age:=age+1]
  transition[age>100,age:=100]
  temp <- transition[age==min(age)]
  temp[,num:=0]
  transition <- rbind(temp,transition)
  transition[,year:=year]
  
  #########REMOVE IF DOESNT MAKE FASTER
  if(runDraw){
    together <- together[,.(num=sum(num)),by=.(geno,treat1,treat2,age,leaving,typeLeaving)]
  } else {
    together <- together[,.(num=sum(num*prob)),by=.(geno,treat1,treat2,age,leaving,typeLeaving)]
  }
  
  sum(together$num)
  setnames(together,"typeLeaving","type")
  setnames(together,"leaving","state")
  
  together[!state %in% c("M","D"),age:=age+1]
  together[age>100,age:=100]
  temp <- together[age==min(age)]
  temp[,num:=0]
  together <- rbind(temp,together)
  
  prevalence <- merge(prevalenceSkeleton, together, by=c("geno","treat1","treat2","age","state","type"),all=TRUE)
  prevalence <- prevalence[!duplicated(prevalence)]
  prevalence[is.na(num),num:=0]
  prevalence[,year:=year]
  
  return(list(prevalence=prevalence,transition=transition))
  
  sum(xstartDT$num)
  transition <- together[state!=leaving,.(num=sum(num*prob)),by=.(geno,treat1,treat2,age,state,leaving,typeLeaving)]
  setnames(transition,"state","start")
  setnames(transition,"typeLeaving","type")
  setnames(transition,"leaving","end")
  transition[,age:=age+1]
  transition[age>100,age:=100]
  temp <- transition[age==min(age)]
  temp[,num:=0]
  transition <- rbind(temp,transition)
  
  incidence <- together[state!=leaving,.(num=sum(num*prob)),by=.(geno,treat1,treat2,age,leaving,typeLeaving)]
  setnames(incidence,"typeLeaving","type")
  setnames(incidence,"leaving","state")
  incidence[,age:=age+1]
  incidence[age>100,age:=100]
  temp <- incidence[age==min(age)]
  temp[,num:=0]
  incidence <- rbind(temp,incidence)
  
  together <- together[,.(num=sum(num*prob)),by=.(geno,treat1,treat2,age,leaving,typeLeaving)]
  sum(together$num)
  setnames(together,"typeLeaving","type")
  setnames(together,"leaving","state")
  
  together[,age:=age+1]
  together[age>100,age:=100]
  temp <- together[age==min(age)]
  temp[,num:=0]
  together <- rbind(temp,together)
  
  
  return(list(prevalence=together,incidence=incidence, transition=transition))
  
}


VariablesFitting <- function(res){
  prevalence <- res[["prevalence"]]
  transition <- res[["transition"]]
  
  year <- res[["prevalence"]]$year[1]
  
  numPWIDHCV=sum(prevalence[type=="PWID" & state %in% c("HCV_AI","HCV_CI","HCV_C","HCC")]$num)
  numPWID=sum(prevalence[type=="PWID" & !state %in% c("M","D")]$num)
  percPWIDHCV=numPWIDHCV/numPWID*100
  #newLT
  #numCIRR=sum(prevalence[state %in% c("HCV_C","T_C","S_C")]$num)
  
  numHCC=sum(prevalence[state %in% c("HCC")]$num)
  
  #incidence
  newLT <- sum(transition[end %in% c("LT")]$num)
  newHCC <- sum(transition[end %in% c("HCC")]$num)
  treatCIRR=sum(prevalence[state %in% c("T_C")]$num)
  
  #transition
  newSuccessTreatedHCV <- sum(transition[end %in% c("S","S_C") & start!="HCV_AI"]$num)
  newMortalityHCC <- sum(transition[start %in% c("HCC") & end=="M"]$num)
  newMortalityCIRR <- sum(transition[start %in% c("HCV_C", "S_C") & end=="M"]$num)
  
  retVal <- data.table(percPWIDHCV,
                       treatCIRR,
                       numHCC,
                       newLT,
                       newSuccessTreatedHCV,
                       newHCC,
                       newMortalityHCC,
                       newMortalityCIRR)
  retVal <- melt(retVal)
  retVal[,year:=year]
  return(retVal)
}


RunModel <- function(xstartDT, modelProbs, yearlyParams, projections=TRUE, runDraw=FALSE){
  skeleton.parameters$Prob[skeleton.parameters.estimated] <- modelProbs[-c(1:3)]
  fullParam <- CleanParametersYearSpecific(param=skeleton.parameters,year=2000,yearlyParams=yearlyParams)
  
  if(projections){
    runLength <- 58
  } else runLength <- 41
  
  #skeleton.parameters <- readRDS(system.file("extdata","skeletonParameters.RDS",package="agedependent"))
  fitting <- vector("list",runLength)
  prevalence <- vector("list",runLength)
  transition <- vector("list",runLength)
  
  year <- 1973
  res <- RunOneYear(xstartDT, fullParam=fullParam, yearlyParams, year=1973,yearlyIncreaseTreatment=modelProbs[1],infectiousnessShift1=modelProbs[2],yearlyIncreaseLT=modelProbs[3],runDraw=runDraw)
  prevalence[[1]] <- res[["prevalence"]]
  transition[[1]] <- res[["transition"]]
  
  for(i in 2:runLength){
    #if(1973+i-1==2000) break
    #if(1973+i==1995) break
    res <- RunOneYear(prevalence[[i-1]], fullParam=fullParam, yearlyParams, year=1973+i-1,yearlyIncreaseTreatment=modelProbs[1],infectiousnessShift1=modelProbs[2],yearlyIncreaseLT=modelProbs[3],runDraw=runDraw)
    if(1973+i-1 >= 2000 & 1973+i-1 <= 2013) fitting[[i]] <- VariablesFitting(res)
    prevalence[[i]] <- res[["prevalence"]]
    transition[[i]] <- res[["transition"]]
  }
 # z <- 1
 # transition[[i-z]][end=="HCV_AI",.(num=sum(num)),by=.(year)]
 # (sum(prevalence[[i-z]][type=="PWID" & state %in% c("S")]$num))
 # (numPWIDHCV=sum(prevalence[[i-z]][type=="PWID" & state %in% c("HCV_CI","HCV_C","T_CI","T_C")]$num))
 # (numPWID=sum(prevalence[[i-z]][type=="PWID" & !state %in% c("M","D")]$num))
 # numPWIDHCV/numPWID
  
  return(list(fitting=rbindlist(fitting),prevalence=rbindlist(prevalence),transition=rbindlist(transition)))
}

Fitting <- function(initialModelProbs,yearlyParams){
  est <- RunModel(xstartDT, 
                  modelProbs=initialModelProbs, 
                  yearlyParams=yearlyParams,
                  projections=FALSE)
  
  modelFit <- merge(est[["fitting"]], targets, by=c("year","variable"))
  modelFit[,weight:=1/observed]
  #modelFit[variable=="percPWIDHCV",weight:=weight*20]
  mse <- mean(modelFit[,((value-observed)^2)*weight])
  return(mse)
}

Likelihood <- function(initialModelProbs,yearlyParams){
  est <- RunModel(xstartDT, 
                  modelProbs=initialModelProbs, 
                  yearlyParams=yearlyParams,
                  projections=FALSE)
  
  modelFit <- merge(est[["fitting"]], targets[,c("year","variable","observed"),with=F], by=c("year","variable"))
  modelFit <- merge(modelFit,mse,by="variable")
  
  nll <- sum(dnorm(x=modelFit$observed, modelFit$value, modelFit$mse,log=TRUE))
  #nll <- sum(dpois(x=modelFit$observed, modelFit$value,log=TRUE))
  
  return(nll)
}





