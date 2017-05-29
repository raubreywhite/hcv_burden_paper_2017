#' This function takes the previously calculated
#' transition probabilities and smoothes them
GetPreviousProbs <- function(){
print(data.table:::cedta())
  probs <- list()  
  for(i in 1:14){
    file <- system.file("extdata",paste("transition_prob_CONF_y",i+1999,".RDS",sep=""),package="hepcburden")
    x <- readRDS(file)
    x[is.na(L95),L95:=Prob]
    x[is.na(U95),U95:=Prob]
    x[,SE:=(U95-L95)/2/1.96]
    x[,year:= i+1999-1]
    x[,order:=1:nrow(x)]
    probs[[i]] <- x
  }
  probs <- rbindlist(probs)
  setkeyv(probs,c("order","year"))
  probs[,smoothed:=runmean(Prob,3),by=order]
  probs[,smoothed.sd:=runmean(SE,3),by=order]
  probs[,Prob:=smoothed]
  probs[,SE:=smoothed.sd]
  lastYr <- probs[year==max(probs$year)]
  for(i in 1:17){
    lastYr[,year:=year+1]
    probs <- rbind(probs,lastYr)
  }

  return(probs)
}

#' Alters the given probabilities to accommodate interventions regarding treatment success
#' @param interventionDuration Number of years the treatment will happen for
#' @param interventionSuccessProb: for CI and C, the increase in treatment success
#' //for AI, the decrease in progressing to chronic HCV
#' @param interventionStates The states that the treatment interventions will apply to
GenScenario <- function(probs, params=list(interventionDuration=1, interventionSuccessProb=0.2, interventionStates="")){
  scenario <- copy(probs)
  for(i in params$interventionStates){
    if(i=="CI"){
      inState <- "HCV_CI"
      outState <- "S"
      scenario[
        year>=2015 & 
        year<=2014+params$interventionDuration & 
        State==inState & 
        Leaving==outState, 
        Prob:=Prob*(treatmentSuccess/baselineTreatmentSuccess)*(1+params$interventionSuccessProb)
        ]
    } else if(i=="C"){
      inState <- "HCV_C"
      outState <- "S_C"
      scenario[
        year>=2015 & 
          year<=2014+params$interventionDuration & 
          State==inState & 
          Leaving==outState, 
        Prob:=Prob*(treatmentSuccess/baselineTreatmentSuccess)*(1+params$interventionSuccessProb)
        ]
    } else if(i=="AI"){
      inState <- "HCV_AI"
      outState <- "HCV_CI"
      scenario[
        year>=2015 & 
          year<=2014+params$interventionDuration & 
          State==inState & 
          Leaving==outState, 
        Prob:=Prob*(treatmentSuccess/baselineTreatmentSuccess)*(1-params$interventionSuccessProb)
        ]
    }
    
  }
  return(scenario)
}


RunScenario <- function(probs, params=list(interventionDuration=1, interventionSuccessProb=0.2, interventionStates="")){
  res <- RunAllYears(probs)
  
  full <- rbindlist(res)
  fullLong <- data.table(full %>% gather(var, est, -year))
  
  retval <- fullLong[, list(est=sum(est)), by=list(year,var)]
  retval[,interventionDuration:=params$interventionDuration]
  retval[,interventionSuccessProb:=params$interventionSuccessProb]
  retval[,interventionStates:=paste(params$interventionStates,collapse=", ")]
  retval[,relapse:=varying.parameters$relapse.prob]
  retval[,newIVDUReduction:=newIVDUReduction]
  retval[,totalIVDUReduction:=totalIVDUReduction]
  retval[,treatmentSuccess:=treatmentSuccess]
  
  return(retval)
}

RunAllScenarios <- function(){
  probs <- GetPreviousProbs()
  newIVDUReduction <<- 0
  totalIVDUReduction <<- 0
  
  res <- list()
  
  scenarioStates = list(c("CI"))
  scenarioDuration = c(1,5,15)
  scenarioProb = c(0.2, 1, 5, 10)
  scenarioReductions = list(c(0,0),c(0.1,0.1),c(0.2,0.2))
  
  for(s in scenarioStates) for(d in scenarioDuration) for(p in scenarioProb) for(r in scenarioReductions){
  #for(s in scenarioStates[[1]]) for(d in scenarioDuration[[1]]) for(p in scenarioProb[[1]]) for(r in scenarioReductions[[1]]){
    newIVDUReduction <<- r[1]
    totalIVDUReduction <<- r[2]
    
    treatmentSuccess <<- baselineTreatmentSuccess
    treatmentCost <<- treatmentCostBaseline
    paramsBaseline <- list(interventionDuration=d, interventionSuccessProb=0.0, interventionStates=s)
    prob <- GenScenario(probs, paramsBaseline)
    res[[length(res)+1]] <- RunScenario(prob, paramsBaseline)
    
    treatmentSuccess <<- 0.9
    treatmentCost <<- treatmentCostIntervention
    params <- list(interventionDuration=d, interventionSuccessProb=0.0, interventionStates=s)
    prob <- GenScenario(probs, params)
    res[[length(res)+1]] <- RunScenario(prob, params)
    
    treatmentSuccess <<- baselineTreatmentSuccess
    treatmentCost <<- treatmentCostBaseline
    params <- list(interventionDuration=d, interventionSuccessProb=p, interventionStates=s)
    prob <- GenScenario(probs, params)
    res[[length(res)+1]] <- RunScenario(prob, params)
    
    treatmentSuccess <<- 0.9
    treatmentCost <<- treatmentCostIntervention
    params <- list(interventionDuration=d, interventionSuccessProb=p, interventionStates=s)
    prob <- GenScenario(probs, params)
    res[[length(res)+1]] <- RunScenario(prob, params)
  }
  
  data <- rbindlist(res)
  data <- data[!duplicated(data)]
  return(data)
}



