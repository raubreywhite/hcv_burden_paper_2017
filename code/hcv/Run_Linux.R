library(data.table)
library(ggplot2)
library(pomp)
library(tidyr)

# This comes from github.com/raubreywhite/RAWmisc
# Install using: devtools::install_github("raubreywhite/RAWmisc")
RAWmisc::AllowFileManipulationFromInitialiseProject()

RAWmisc::InitialiseProject(
  HOME = "/git/hcv_burden_paper_2017/code/hcv/",
  RAW = "/git/hcv_burden_paper_2017/data_raw/hcv/",
  CLEAN = "/git/hcv_burden_paper_2017/data_clean/hcv",
  BAKED = "/git/hcv_burden_paper_2017/results_baked/hcv/",
  FINAL = "/git/hcv_burden_paper_2017/results_final/hcv/",
  SHARED = "/git/hcv_burden_paper_2017/results_shared/hcv/")

saveRDS(meanAge,file.path(RAWmisc::PROJ$BAKED,"AgeDebut_meanAge.RDS"))
saveRDS(sirusAges,file.path(RAWmisc::PROJ$BAKED,"AgeDebut_sirusAges.RDS"))

file <- file.path(RAWmisc::PROJ$RAW,"transitionRates.csv")
skeleton.parameters.raw <- fread(file)
skeleton.parameters.raw[,Prob:=(Prob_low+Prob_high)/2]
skeleton.parameters.estimated <- c(1,2,4,5,6,7,8,9,10,11,12,14)

#modelProbs <- readRDS(file.path(RAWmisc::PROJ$BAKED,"modelProbs.RDS"))
#saveRDS(modelProbs,file.path(RAWmisc::PROJ$HOME,"modelProbs.RDS"))
modelProbs <- readRDS(file.path(RAWmisc::PROJ$HOME,"modelProbs.RDS"))
# removing the three extra time sensitive terms
xyearlyIncreaseTreatment<-modelProbs[1]
xinfectiousnessShift1<-modelProbs[2]
xyearlyIncreaseLT<-modelProbs[3]
modelProbs <- modelProbs[-c(1:3)]

skeleton.parameters.raw[skeleton.parameters.estimated,Prob:=modelProbs]

saveRDS(skeleton.parameters.estimated,file=file.path(RAWmisc::PROJ$BAKED,"probTransitionEstimated.RDS"))
saveRDS(skeleton.parameters.raw,file=file.path(RAWmisc::PROJ$BAKED,"probTransition.RDS"))

skeleton.parameters <- skeleton.parameters.raw[,c("State","Leaving","Prob"),with=FALSE]
modelProbsL <- skeleton.parameters.raw$Prob_low[skeleton.parameters.estimated]
modelProbsH <- skeleton.parameters.raw$Prob_high[skeleton.parameters.estimated]

# adding on the two extra time sensitive terms
modelProbs <- c(xyearlyIncreaseTreatment,xinfectiousnessShift1,xyearlyIncreaseLT,modelProbs)
modelProbsL <- c(0.001,0.001,0.001,modelProbsL)
modelProbsH <- c(0.1,0.5,0.3,modelProbsH)

modelProbs[modelProbs==modelProbsL] <- modelProbs[modelProbs==modelProbsL] + 0.005
modelProbs[modelProbs==modelProbsH] <- modelProbs[modelProbs==modelProbsH] - 0.005

## FITTING PWID ENVELOPE
pomp::bake(file.path(RAWmisc::PROJ$BAKED,"pwid_envelope.RDS"),{
  optim(rep(0.1,5), IVDUcalcError, method="L-BFGS-B", 
           lower=c(rep(0.01,2),rep(-0.2,3)), 
           upper=c(rep(0.40,2),rep(0.2,3)), control=list(
             trace=3,
             maxit=30
           ))
}) -> g

temp <- IVDUcalc(g$par)
saveRDS(temp[["obs"]],file=file.path(RAWmisc::PROJ$BAKED,"pwid.RDS"))

AddingIVDUs <- temp[["ivduParams"]]
for(i in 2013:2030){
  x <- AddingIVDUs[year==2012]
  x[,year:=i]
  AddingIVDUs <- rbind(AddingIVDUs,x)
}
setkey(AddingIVDUs,year)
yearlyParams$probRelapse <- AddingIVDUs[2]$prob.relapse
yearlyParams$probCease <- AddingIVDUs[2]$prob.cease
yearlyParams$probCure <- AddingIVDUs[2]$prob.cure
saveRDS(yearlyParams,file=file.path(RAWmisc::PROJ$BAKED,"probParam.RDS"))

x <- numeric(2030)
for(i in AddingIVDUs$year) x[i] <- AddingIVDUs[year==i]$added
AddingIVDUs <- x
saveRDS(AddingIVDUs,file=file.path(RAWmisc::PROJ$BAKED,"AddingIVDUs.RDS"))

## FITTING THE MODEL
ts <- Sys.time()
print(ts)
pomp::bake(file.path(RAWmisc::PROJ$BAKED,"model.RDS"),{
  optim(modelProbs, Fitting, method="L-BFGS-B", lower=modelProbsL, upper=modelProbsH, control=list(
  trace=5,
  REPORT=1,
  maxit=4
  ),
  yearlyParams=yearlyParams
  )
}) -> g

te <- Sys.time()
difftime(te,ts,unit="secs")

## SAVING MODEL FIT
print(modelProbs)
(modelProbs <- g$par)
saveRDS(modelProbs, file=file.path(RAWmisc::PROJ$BAKED,"modelProbs.RDS"))
skeleton.parameters.raw[skeleton.parameters.estimated,Prob:=modelProbs[-c(1:3)]]
saveRDS(skeleton.parameters.raw,file=file.path(RAWmisc::PROJ$BAKED,"probTransition.RDS"))


## CALCULATING CONFIDENCE INTERVALS AROUND MODEL FIT
modelProbs <- readRDS(file=file.path(RAWmisc::PROJ$BAKED,"modelProbs.RDS"))
#modelProbs[1] <- 0
#modelProbs[2] <- (1-(1-0.58)/2)
# calculating model error
#modelProbs[3] <- 1

est <- RunModel(xstartDT, 
                modelProbs=modelProbs, 
                yearlyParams=yearlyParams)


FigureFittingCentre()

FigureIncidenceCentre()


modelFit <- merge(est[["fitting"]], targets[!variable %in% c("numPWID","numEXPWID")], by=c("year","variable"))
modelFit <- modelFit[,.(sumerror=sum((value-observed)^2),n=.N),by=variable]
p <- length(skeleton.parameters.estimated)/nrow(modelFit)
modelFit[variable!="treatCIRR",mse:=sqrt(sumerror/(n-p-1))]
modelFit[variable=="treatCIRR",mse:=sqrt(sumerror)]
mse <- modelFit[,c("variable","mse"),with=FALSE]

## calculating the CIs
l0 <- Likelihood(initialModelProbs=modelProbs, 
                 yearlyParams=yearlyParams)

pomp::bake(file.path(RAWmisc::PROJ$BAKED,"transitionProbsCIs.RDS"),{
  savedL <- modelProbs
  savedU <- modelProbs
  for(i in 1:length(modelProbs)){
    print(paste0(i,"/",length(modelProbs)))
    dist <- abs(modelProbsL[i]-modelProbsH[i])/100
    for(j in 1:100){
      print(j)
      testModelProbs <- modelProbs
      testModelProbs[i] <- testModelProbs[i]-dist*j*2^(j-1)
      if(testModelProbs[i] < modelProbsL[i]) testModelProbs[i] <- modelProbsL[i]-0.00001
      
      l1 <- Likelihood(initialModelProbs=testModelProbs, 
                       yearlyParams=yearlyParams)
      if(2*l0-2*l1 < 3.84 & testModelProbs[i] > modelProbsL[i]){
        savedL[i] <- testModelProbs[i]
      } else {
        savedL[i] <- (testModelProbs[i]+savedL[i])/2
        break
      }
    }
    
    for(j in 1:100){
      print(j)
      testModelProbs <- modelProbs
      testModelProbs[i] <- testModelProbs[i]+dist*j*2^(j-1)
      if(testModelProbs[i] > modelProbsH[i]) testModelProbs[i] <- modelProbsH[i]+0.00001
      
      l1 <- Likelihood(initialModelProbs=testModelProbs, 
                       yearlyParams=yearlyParams)
      if(2*l0-2*l1 < 3.84 & testModelProbs[i] < modelProbsH[i]){
        savedU[i] <- testModelProbs[i]
      } else  {
        savedU[i] <- (testModelProbs[i]+savedU[i])/2
        break
      }
    }
  }
  
  savedL[savedL<modelProbsL] <- modelProbsL[savedL<modelProbsL]
  savedU[savedU>modelProbsH] <- modelProbsH[savedU>modelProbsH]
  
  list(savedL=savedL,savedU=savedU)
}) -> saved

savedL <- saved$savedL
savedU <- saved$savedU

saveRDS(savedL,file=file.path(RAWmisc::PROJ$BAKED,"transitionProbsLower.RDS"))
saveRDS(savedU,file=file.path(RAWmisc::PROJ$BAKED,"transitionProbsUpper.RDS"))


###### RUNNING FINAL MODEL

ts <- Sys.time()
print(ts)
est <- RunModel(xstartDT, 
                modelProbs=modelProbs, 
                yearlyParams=yearlyParams)
te <- Sys.time()
difftime(te,ts,unit="secs")

saveRDS(est,file.path(RAWmisc::PROJ$BAKED,"est.RDS"))

savedL <- readRDS(file=file.path(RAWmisc::PROJ$BAKED,"transitionProbsLower.RDS"))
savedU <- readRDS(file=file.path(RAWmisc::PROJ$BAKED,"transitionProbsUpper.RDS"))
sd.est <- apply(cbind(abs(savedU-modelProbs),abs(savedL-modelProbs)),1,max)/1.96

### multiple draws
for(draw in 1:1000){
  file <- file.path(RAWmisc::PROJ$BAKED,paste0("draw_",draw,".RDS"))
  if(file.exists(file)) next
  print(draw)
  drawProbs <- rbeta.mom(modelProbs,sd.est)
  drawProbs[drawProbs<0] <- 0
  drawProbs[drawProbs>1] <- 1
  est <- RunModel(xstartDT, 
                  modelProbs=drawProbs, 
                  yearlyParams=yearlyParams,
                  runDraw=TRUE)
  
  saveRDS(est,file)
}

###### RUNNING TEST MODEL
if(FALSE){
  temp <- modelProbs
  temp[4] <- 0.12
  ts <- Sys.time()
  print(ts)
  est <- RunModel(xstartDT, 
                  modelProbs=temp, 
                  yearlyParams=yearlyParams)
  te <- Sys.time()
  difftime(te,ts,unit="secs")
  
  saveRDS(est,"results/est_test.RDS")
  
  FigureFittingCentre("results/est_test.RDS")
}

### making some graphs

FigureIncidence()
FigureArea()
FigureArea2()


TableProbs()
TableSummaryPerc2()
TableSummary()

FigurePWID()
FigureAgeDebut()
FigureFitting()
FigureAge()
FigureEnvelopePWIDPerc()

LargeSupplementalTables()
BurdenGraph()
DALYsPer100k()
DALYsPer100kForResultsAndDiscussion()




file <- system.file("extdata","report.Rmd",package="agedependent")
RmdToHTML(file,paste0("results/Report_",format(Sys.time(), "%Y_%m_%d"),".html"))


modelProbs <- readRDS(file=file.path(RAWmisc::PROJ$BAKED,"modelProbs.RDS"))
est <- RunModel(xstartDT, 
                modelProbs=modelProbs, 
                yearlyParams=yearlyParams)


baseline <- SensitivityOutcomes(est)
setnames(baseline,"value","baseline")

retval <- vector("list",100)
retvalIndex <- 1
for(i in skeleton.parameters.estimated){
  print(skeleton.parameters.estimated)
  print(i)
  from <- skeleton.parameters.raw$State[i]
  to <- skeleton.parameters.raw$Leaving[i]
  for(pos in c("Set to max","Set to min")){
    if(pos=="Set to max"){
      newProb <- skeleton.parameters.raw$Prob_high[i]
    } else {
      newProb <- skeleton.parameters.raw$Prob_low[i]
    }
    modelProbs <- readRDS(file=file.path(RAWmisc::PROJ$BAKED,"modelProbs.RDS"))
    modelProbs[3+i] <- newProb
    
    est <- RunModel(xstartDT, 
                    modelProbs=modelProbs, 
                    yearlyParams=yearlyParams)
    
    retval[[retvalIndex]] <- SensitivityOutcomes(est)
    retval[[retvalIndex]][,pos:=pos]
    retval[[retvalIndex]][,from:=from]
    retval[[retvalIndex]][,to:=to]
    
    retvalIndex <- retvalIndex+1
  }
}

retval <- rbindlist(retval)

retval <- merge(retval,baseline,by=c("TYPES","prettyName","variable"))
retval[,percDeviation:=100*(as.numeric(value)-as.numeric(baseline))/as.numeric(baseline)]
retval[,transProb:=sprintf("Transition prob: %s->%s",from,to)]

q <- ggplot(retval[variable=="Total" & TYPES %in% c("DISCRETE PREVALENCE")],
            aes(x=prettyName,y=as.numeric(value)-as.numeric(baseline),fill=pos))
q <- q + geom_bar(stat="identity",pos="dodge")
q <- q + facet_wrap(~transProb)
q <- q + scale_fill_brewer("Transition probability",palette="Set2")
q <- q + theme_SMAO_V3(base_size=24)
q <- q + scale_x_discrete("")
q <- q + scale_y_continuous("Absolute difference in 2015 prevalence from fitted model")
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
q <- q + theme(legend.position="bottom")
RAWmisc::SMAOpng(file.path(RAWmisc::PROJ$SHARED_TODAY,"Figure_Sensitivity_Analysis.png"),landscape=TRUE,w=1)
print(q)
dev.off()





