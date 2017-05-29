targets <- fread(file.path(RAWmisc::PROJ$RAW,"TARGETS.csv"))
targets[,observed:=as.numeric(gsub(",",".",observed))]
numPWID1973 <- targets[year==1973 & variable=="numPWID"]$observed
numEXPWID1973 <- targets[year==1973 & variable=="numEXPWID"]$observed
targets <- targets[!variable %in% c("numPWIDHCV","numPWID","numEXPWID","x")]

file <- file.path(RAWmisc::PROJ$RAW,"pwid.csv")
pwid <- data.table::fread(file)
pwid[,final.curedivdu:=cumsum(exivdu.cured)/2]
pwid[,final.exivdu:=exivdu.temporary-exivdu.dead]
pwid <- pwid[,c("year","add.year","final.ivdu","final.exivdu","final.curedivdu"),with=FALSE]
pwidTargets <- pwid[,c("year","final.ivdu","final.exivdu","final.curedivdu"),with=FALSE]
setnames(pwidTargets,c("year","numPWID","numEXPWID","numCUREDPWID"))
pwidTargets <- melt.data.table(pwidTargets,id="year")
setnames(pwidTargets,"value","obs")

#print(pwidTargets)

file <- file.path(RAWmisc::PROJ$RAW,"needles_oslo.txt")
needles <- data.table::fread(file)
needles[,source:=NULL]
needles[,needles:=as.numeric(gsub(" ","",needles))]
needles[,fylke:="Oslo"]

file <- file.path(RAWmisc::PROJ$RAW,"deaths_from_drugs.xlsx")
deathsFromDrugs <- readxl::read_excel(file)
deathsFromDrugs <- reshape2::melt(deathsFromDrugs,id="Fylke")
names(deathsFromDrugs) <- c("Fylke","Year","Deaths")
deathsFromDrugs <- data.table(deathsFromDrugs)
deathsFromDrugs[Fylke=="Totalt",Total:=Deaths]
deathsFromDrugs[,Total:=sum(Total,na.rm=T),by=Year]
deathsFromDrugs <- deathsFromDrugs[Fylke!="Totalt"]
deathsFromDrugs[,Proportion:=Deaths/Total]
deathsFromDrugs <- deathsFromDrugs[Fylke %in% c("Oslo","Sor-Trondelag","Hordaland","Rogaland")]
setorder(deathsFromDrugs,Fylke,Year)
deathsFromDrugs[,Year:=as.numeric(as.character(Year))]
deathsFromDrugs[Year==1996,Prop1996:=Proportion]
deathsFromDrugs[,Prop1996:=sum(Prop1996,na.rm=T),by=Fylke]
deathsFromDrugs[Fylke=="Oslo",Oslo:=Proportion-Prop1996]
deathsFromDrugs[,Oslo:=sum(Oslo,na.rm=T),by=Year]
deathsFromDrugs[is.na(Proportion),Proportion:=Prop1996-Oslo*Prop1996*2]
setnames(deathsFromDrugs,c("fylke","year","deaths","total","proportion","prop1996","oslo"))

deathsFromDrugs <- merge(deathsFromDrugs,needles,by=c("fylke","year"),all.x=TRUE)
deathsFromDrugs[fylke=="Oslo" & is.na(needles),needles:=0]
deathsFromDrugs[,needlesperPWID:=needles/deaths]
deathsFromDrugs[fylke=="Oslo" & year==2012,fullNeedles:=needlesperPWID]
deathsFromDrugs[fylke=="Oslo",fullNeedles:=sum(fullNeedles,na.rm=T),by=fylke]
deathsFromDrugs[,fylkeCoverage:=needlesperPWID/fullNeedles]
deathsFromDrugs[fylkeCoverage>1,fylkeCoverage:=1]
deathsFromDrugs[fylke=="Hordaland",fylkeCoverage:=approx(x=c(1992,2012),y=c(0,1),xout=seq(1987,2013),yleft=0,yright=1)$y]
deathsFromDrugs[fylke=="Rogaland",fylkeCoverage:=approx(x=c(1992,2012),y=c(0,1),xout=seq(1987,2013),yleft=0,yright=1)$y]
deathsFromDrugs[fylke=="Sor-Trondelag",fylkeCoverage:=approx(x=c(1992,2012),y=c(0,1),xout=seq(1987,2013),yleft=0,yright=1)$y]
deathsFromDrugs <- deathsFromDrugs[,c("fylke","year","proportion","fylkeCoverage"),with=F]
needleCoverageX <- deathsFromDrugs[,.(coverage=sum(proportion*fylkeCoverage)),by=.(year)]
needleCoverage <- rep(0,2040)
for(i in min(needleCoverageX$year):length(needleCoverage)){
  if(i<=max(needleCoverageX$year)){
    needleCoverage[i] <- needleCoverageX[year==i]$coverage
  } else {
    needleCoverage[i] <- needleCoverageX[year==max(needleCoverageX$year)]$coverage
  }
}

needleCoverageXOslo <- deathsFromDrugs[fylke=="Oslo",.(coverage=sum(fylkeCoverage)),by=.(year)]
needleCoverageOslo <- rep(0,2040)
for(i in min(needleCoverageXOslo$year):length(needleCoverageOslo)){
  if(i<=max(needleCoverageXOslo$year)){
    needleCoverageOslo[i] <- needleCoverageXOslo[year==i]$coverage
  } else {
    needleCoverageOslo[i] <- needleCoverageXOslo[year==max(needleCoverageXOslo$year)]$coverage
  }
}

attributableXOslo <- deathsFromDrugs[fylke=="Oslo",.(coverage=sum(proportion)),by=.(year)]
attributableOslo <- rep(0,2040)
for(i in min(attributableXOslo$year):length(attributableOslo)){
  if(i<=max(attributableXOslo$year)){
    attributableOslo[i] <- attributableXOslo[year==i]$coverage
  } else {
    attributableOslo[i] <- attributableXOslo[year==max(attributableXOslo$year)]$coverage
  }
}
attributableOslo[1973:1987] <- approx(x=c(1973,1987),y=c(1,attributableOslo[1987]),xout=c(1973:1987))$y

injectingGINIX <- deathsFromDrugs[,.(gini=ineq::ineq(proportion,type="Gini")),by=.(year)]
injectingGINI <- rep(0,2040)
for(i in min(injectingGINIX$year):length(injectingGINI)){
  if(i<=max(injectingGINIX$year)){
    injectingGINI[i] <- injectingGINIX[year==i]$gini
  } else {
    injectingGINI[i] <- injectingGINIX[year==max(injectingGINIX$year)]$gini
  }
}
injectingGINI[1973:1987] <- approx(x=c(1973,1987),y=c(1,injectingGINI[1987]),xout=c(1973:1987))$y

file <- file.path(RAWmisc::PROJ$RAW,"mortalityrates.txt")
mortalityRates <- fread(file)
mortalityRates[,Total:=as.numeric(Total)]
mortalityRates[,Age:=as.numeric(Age)]
mortalityRates[is.na(Total),Total:=0]
mortalityRates[Total==0,Total:=0.00001]
mortalityRates <- mortalityRates[,c("Year","Age","Total"),with=FALSE]


# calculating excess IVDU mortality
# http://www.ncbi.nlm.nih.gov/pmc/articles/PMC4047552/
maleCrudeProbDead <- 0.026
maleSMR <- 21
femaleCrudeProbDead <- 0.016
femaleSMR <- 39.4
maleExcess <- maleCrudeProbDead*(maleSMR-1)/maleSMR
femaleExcess <- femaleCrudeProbDead*(femaleSMR-1)/femaleSMR
excess <- (maleExcess*128 + femaleExcess*44)/(128+44)

yearlyParams <- list(
  deathProbNormal=0.00001,
  deathProbExcessIVDU=excess,
  ivduNew=0,
  ivduPrevalence=0,
  ivduInterventionReduction=0,
  ivduTreatmentSuccessG1=0.45,
  ivduTreatmentSuccessG2=0.80,
  ivduTreatmentSuccessG3=0.80,
  ivduTreatmentCostG1=500/1000,
  ivduTreatmentCostG2=500/1000,
  ivduTreatmentCostG3=500/1000,
  genoDist= c(0.35,0.15,0.50),
  probTreatmentPWID=1,
  SCMortalityReduction=5
)

treatment <- data.table(expand.grid(c(1,2,3),c(0,1),c(0,1)))
setnames(treatment,c("geno","treat1","treat2"))
treatment[,prop2:=0.9]
treatment[treat2==0,prop2:=1-prop2]

for(i in 1:3){
  if(i==1){
    val <- yearlyParams$ivduTreatmentSuccessG1
  } else if(i==2){
    val <- yearlyParams$ivduTreatmentSuccessG2
  } else if(i==3){
    val <- yearlyParams$ivduTreatmentSuccessG3
  } 
  treatment[geno==i,prop1:=val]
  treatment[geno==i & treat1==0,prop1:=1-prop1]
}
treatment

treatment[,prop:=prop1*prop2]
treatment[,prop1:=NULL]
treatment[,prop2:=NULL]
setorder(treatment,geno)

xstartPerm <- c(
  "S"=2786,
  "HCV_AI"=1385,
  "HCV_CI"=8718,
  "T_CI"=0,
  "HCV_C"=249,
  "T_C"=0,
  "S_C"=19,
  "HCC"=8,
  "LT"=1,
  "M"=0,
  "D"=0,
  "S"=6945,
  "HCV_AI"=0,
  "HCV_CI"=14961,
  "T_CI"=0,
  "HCV_C"=435,
  "T_C"=0,
  "S_C"=59,
  "HCC"=16,
  "LT"=1,
  "M"=0,
  "D"=0
)

xstartPerm <- c(
  "S"=25,
  "HCV_AI"=50,
  "HCV_CI"=25,
  "T_CI"=0,
  "HCV_C"=0,
  "T_C"=0,
  "S_C"=0,
  "HCC"=0,
  "LT"=0,
  "M"=0,
  "D"=0,
  "S"=25,
  "HCV_AI"=0,
  "HCV_CI"=0,
  "T_CI"=0,
  "HCV_C"=0,
  "T_C"=0,
  "S_C"=0,
  "HCC"=0,
  "LT"=0,
  "M"=0,
  "D"=0,
  "S"=0,
  "HCV_AI"=0,
  "HCV_CI"=0,
  "T_CI"=0,
  "HCV_C"=0,
  "T_C"=0,
  "S_C"=0,
  "HCC"=0,
  "LT"=0,
  "M"=0,
  "D"=0
)

xstartPerm <- xstartPerm*numPWID1973/sum(xstartPerm[1:(length(xstartPerm)/3)])

sirusAges <- data.frame(year=c(1975,1985,1995,
                               2003:2012),
                        y=c(15.5,17.7,20.9,
                            21.9,22.7,21.5,22.8,24.7,21.0,22.1,23.7,24.0,23.5))
#sirusAges$post2000 <- sirusAges$year>2005

meanAge <- data.frame(year=1973:2030)
meanAge$post2000 <- meanAge$year>2005
ageFit <- lm(y~year,data=sirusAges)
summary(ageFit)
meanAge$mean <- predict(ageFit,meanAge)
#plot(y ~year,data=sirusAges)
#points(meanAge$year,meanAge$mean,col="red")
#plot(meanAge$year,meanAge$mean,col="red")

GenAgeDist<- function(m,s=8){
  
  ageDist <- dnorm(1:100,mean=m,sd=s)
  ageDist[1:10] <- 0
  ageDist[60:100] <- 0
  ageDist <- ageDist/sum(ageDist)
  return(ageDist)
}

GenAgeByGenoDist <- function(m,s=8){
  ageDist <- GenAgeDist(m=m,s=s)
  
  ageByGenoDistTemp <- data.table::copy(treatment)
  ageByGenoDistTemp[geno==1,prop:=prop*yearlyParams$genoDist[1]]
  ageByGenoDistTemp[geno==2,prop:=prop*yearlyParams$genoDist[2]]
  ageByGenoDistTemp[geno==3,prop:=prop*yearlyParams$genoDist[3]]
  ageByGenoDistTemp <- copy(ageByGenoDistTemp)
  
  retval <- list()
  for(i in 1:length(ageDist)){
    retval[[i]] <- data.table::copy(ageByGenoDistTemp)
    retval[[i]][,age:=i]
    retval[[i]][,prop:=prop*ageDist[i]]
    
  }
  retval <- rbindlist(retval)
  return(retval)
}

ageDist <- GenAgeDist(meanAge$mean[meanAge$year==1973])

ageByGenoDist <- vector("list",2030)
for(i in 1:nrow(meanAge)) ageByGenoDist[[meanAge$year[i]]] <- GenAgeByGenoDist(meanAge$mean[i])

xstartSkeletonDT <- data.table(
  num=c(round(xstartPerm*yearlyParams$genoDist[1]),round(xstartPerm*yearlyParams$genoDist[2]),round(xstartPerm*yearlyParams$genoDist[3])),
  geno=rep(c(1,2,3),each=length(xstartPerm)),
  state=rep(names(xstartPerm),3),
  type=rep(c("PWID","Ex-PWID","Cured PWID"),each=length(xstartPerm)/3)
  )

xstartDT <- NULL
for(i in 1:length(ageDist)){
  xstartSkeletonTempDT <- copy(xstartSkeletonDT)
  xstartSkeletonTempDT[,num:=num*ageDist[i]]
  xstartSkeletonTempDT[,age:=i]
  if(is.null(xstartDT)){
    xstartDT <- xstartSkeletonTempDT
  } else xstartDT <- rbind(xstartDT,xstartSkeletonTempDT)
}
sum(xstartDT$num)

dim(xstartDT)
xstartDT <- merge(xstartDT,treatment,by="geno",allow.cartesian = TRUE)
dim(xstartDT)
xstartDT[,num:=num*prop]
xstartDT[,prop:=NULL]

sum(xstartDT$num)

prevalenceSkeleton <- copy(xstartDT)
prevalenceSkeleton[,num:=NULL]

