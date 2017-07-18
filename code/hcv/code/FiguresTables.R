
TableProbs <- function(){
  p <- readRDS(file=file.path(RAWmisc::PROJ$BAKED,"probParam.RDS"))
  sk <- data.frame(var="",est=0,l=0,u=0,estimated="",ref="")
  tab <- sk[-1,]
  
  tab <- rbind(tab,data.frame(
    var="Excess PWID mortality",
    est=p$deathProbExcessIVDU,
    lx=NA,
    ux=NA,
    l=NA,
    u=NA,
    estimated="No",
    ref="[@gjersing_gender_2014]"
  ))
  
  tab <- rbind(tab,data.frame(
    var="Yearly probability of ex-PWID relapse",
    est=p$probRelapse,
    lx=NA,
    ux=NA,
    l=NA,
    u=NA,
    estimated="Yes",
    ref="Internal SIRUS estimates"
  ))
  
  tab <- rbind(tab,data.frame(
    var="Yearly probability of PWID temporary cessation",
    est=p$probCease,
    lx=NA,
    ux=NA,
    l=NA,
    u=NA,
    estimated="Yes",
    ref="Internal SIRUS estimates"
  ))
  
  tab <- rbind(tab,data.frame(
    var="Yearly probability of PWID permanent cessation",
    est=p$probCure,
    lx=NA,
    ux=NA,
    l=NA,
    u=NA,
    estimated="No",
    ref="Internal SIRUS estimates"
  ))
  
  tab <- rbind(tab,data.frame(
    var="Genotype 1",
    est=p$genoDist[1],
    lx=NA,
    ux=NA,
    l=NA,
    u=NA,
    estimated="No",
    ref="MSIS"
  ))
  
  tab <- rbind(tab,data.frame(
    var="Genotype 2",
    est=p$genoDist[2],
    lx=NA,
    ux=NA,
    l=NA,
    u=NA,
    estimated="No",
    ref="MSIS"
  ))
  
  tab <- rbind(tab,data.frame(
    var="Genotype 3",
    est=p$genoDist[3],
    lx=NA,
    ux=NA,
    l=NA,
    u=NA,
    estimated="No",
    ref="MSIS"
  ))

  tab <- rbind(tab,data.frame(
    var="Genotype 1",
    est=p$ivduTreatmentSuccessG1,
    lx=NA,
    ux=NA,
    l=NA,
    u=NA,
    estimated="No",
    ref="[@martin_cost-effectiveness_2012]"
  ))
  
  tab <- rbind(tab,data.frame(
    var="Genotype 2",
    est=p$ivduTreatmentSuccessG2,
    lx=NA,
    ux=NA,
    l=NA,
    u=NA,
    estimated="No",
    ref="[@martin_cost-effectiveness_2012]"
  ))
  
  tab <- rbind(tab,data.frame(
    var="Genotype 3",
    est=p$ivduTreatmentSuccessG3,
    lx=NA,
    ux=NA,
    l=NA,
    u=NA,
    estimated="No",
    ref="[@martin_cost-effectiveness_2012]"
  ))
  
  modelProbs <- readRDS(file=file.path(RAWmisc::PROJ$BAKED,"modelProbs.RDS"))
  savedL <- readRDS(file=file.path(RAWmisc::PROJ$BAKED,"transitionProbsLower.RDS"))
  savedU <- readRDS(file=file.path(RAWmisc::PROJ$BAKED,"transitionProbsUpper.RDS"))
  
  tab <- rbind(tab,data.frame(
    var="Yearly multiplicative change in treatment from 2004",
    est=1+modelProbs[1],
    lx=1+savedL[1],
    ux=1+savedU[1],
    l=1.001,
    u=1.1,
    estimated="Yes",
    ref="Expert opinion"
  ))
  
  tab <- rbind(tab,data.frame(
    var="Proportion of infectiousness reduction with 100% needle exchange coverage&dagger;",
    est=modelProbs[2],
    lx=savedL[2],
    ux=savedU[2],
    l=0.001,
    u=0.5,
    estimated="Yes",
    ref="Expert opinion"
  ))
  
  tab <- rbind(tab,data.frame(
    var="Yearly multiplicative change in LT from 2000",
    est=1+modelProbs[3],
    lx=1+savedL[3],
    ux=1+savedU[3],
    l=1.001,
    u=1.3,
    estimated="Yes",
    ref="Expert opinion"
  ))
  savedL <- savedL[-c(1:3)]
  savedU <- savedU[-c(1:3)]
  
  trans <- readRDS(file=file.path(RAWmisc::PROJ$BAKED,"probTransition.RDS"))
  est <- readRDS(file=file.path(RAWmisc::PROJ$BAKED,"probTransitionEstimated.RDS"))
  trans[,estimated:="No"]
  trans[est,estimated:="Yes"]
  trans[,var:=paste0(State," -> ",Leaving)]
  trans[est,lx:=savedL]
  trans[est,ux:=savedU]
  
  setnames(trans,c("a","b","l","u","ref","est","estimated","var","lx","ux"))
  trans <- trans[,names(tab),with=FALSE]
  trans <- trans[est>0]
  trans$ref[trans$ref=="Martin 2012"] <- "[@martin_cost-effectiveness_2012]"
  trans$ref[trans$ref=="Seeff 2009"] <- "[@seeff_history_2009]"
  trans$ref[trans$ref=="Dalgard 2014"] <- "[@kielland_hcv_2014]"
  trans$ref[trans$ref=="Martin 2012 / Sangiovanni 2006 / Hutchinson"] <- "[@martin_cost-effectiveness_2012; @sangiovanni_natural_2006; @hutchinson_modeling_2005]"
  trans$ref[trans$ref=="Hutchinson 2005 (decompensated!), sangiovanni"] <- "[@sangiovanni_natural_2006; @hutchinson_modeling_2005]"
  trans$ref[trans$ref=="Hutchinson 2005"] <- "[@hutchinson_modeling_2005]"
  trans$ref[trans$ref=="Hitchinson 2005"] <- "[@hutchinson_modeling_2005]"
  tab <- rbind(tab,trans)
  tab[,est:=format(round(est,3),nsmall=3)]
  tab[,lx:=format(round(lx,3),nsmall=3)]
  tab[,ux:=format(round(ux,3),nsmall=3)]
  tab[lx=="   NA",lx:="-"]
  tab[ux=="   NA",ux:="-"]
  tab[,l:=format(round(l,3),nsmall=3)]
  tab[,u:=format(round(u,3),nsmall=3)]
  tab[l=="   NA",l:="-"]
  tab[u=="   NA",u:="-"]
  tab[,var:=as.character(var)]
  tab[,estimated:=as.character(estimated)]
  tab[,ref:=as.character(ref)]
  tab <- htmlTable(tab,
            rnames=FALSE,
            align="rccccl",
            header=c("Variable","Estimate","Lower","Upper","Lower","Upper","Estimated","Data source"),
            cgroup=c("","Estimate","From literature",""),
            n.cgroup=c(1,3,2,2),
            rgroup=c("Injecting drug parameters",
                     "Genotype prevalence",
                     "Probability of successful treatment",
                     "Time-dependent changes to yearly transition probabilities",
                     "Yearly transition probabilities"),
            n.rgroup=c(4,3,3,3,12),
            tfoot="&dagger; Infectiousness=Infectiousness&#42;(1-**InfectiousnessReduction**&#42;NeedleCoverage)")
  saveRDS(tab,file=file.path(RAWmisc::PROJ$BAKED,"Table_Parameters.RDS"))
  
}

TableSummaryInt <- function(est, yearsOfInterest=c(2000,2015,2030)){       
  unique(est$prevalence$state)
  
  ## envelope1
  p <- est$prevalence
  
  p[,prettyName := ""]
  p[!state %in% c("M", "D"), prettyName := "Alive"]
  p <- p[prettyName != ""]
  p[,prettyName:=factor(prettyName,levels=c(
    "Alive"
  ))]
  
  pt <- p[year %in% yearsOfInterest,.(num=round(sum(num))),by=.(year,prettyName)]
  pt[,type:="Total"]
  p <- p[year %in% yearsOfInterest,.(num=round(sum(num))),by=.(year,type,prettyName)]
  p <- rbind(p,pt)
  p[,type:=factor(type,levels=c("Total","PWID","Ex-PWID","Cured PWID"))]
  #p[,year:=-year]
  pe1 <- dcast.data.table(p, prettyName~type+year,value.var="num")
  pe1[,prettyName:=as.character(prettyName)]
  pe1
  
  ## envelope2
  p <- est$prevalence
  
  p[,prettyName := ""]
  p[state %in% c("HCV_AI", "HCV_CI", "HCV_C","HCC","T_CI","T_C"), prettyName := "HCV+ (incl. treatment)"]
  p <- p[prettyName != ""]
  p[,prettyName:=factor(prettyName,levels=c(
    "HCV+ (incl. treatment)"
  ))]
  
  pt <- p[year %in% yearsOfInterest,.(num=round(sum(num))),by=.(year,prettyName)]
  pt[,type:="Total"]
  p <- p[year %in% yearsOfInterest,.(num=round(sum(num))),by=.(year,type,prettyName)]
  p <- rbind(p,pt)
  p[,type:=factor(type,levels=c("Total","PWID","Ex-PWID","Cured PWID"))]
  #p[,year:=-year]
  pe2 <- dcast.data.table(p, prettyName~type+year,value.var="num")
  pe2[,prettyName:=as.character(prettyName)]
  pe2
  
  ## envelope3
  p <- est$prevalence
  
  p[,prettyName := ""]
  p[state %in% c("HCV_AI", "HCV_CI", "HCV_C","HCC"), prettyName := "HCV+ (excl. treatment)"]
  p <- p[prettyName != ""]
  p[,prettyName:=factor(prettyName,levels=c(
    "HCV+ (excl. treatment)"
  ))]
  
  pt <- p[year %in% yearsOfInterest,.(num=round(sum(num))),by=.(year,prettyName)]
  pt[,type:="Total"]
  p <- p[year %in% yearsOfInterest,.(num=round(sum(num))),by=.(year,type,prettyName)]
  p <- rbind(p,pt)
  p[,type:=factor(type,levels=c("Total","PWID","Ex-PWID","Cured PWID"))]
  #p[,year:=-year]
  pe3 <- dcast.data.table(p, prettyName~type+year,value.var="num")
  pe3[,prettyName:=as.character(prettyName)]
  pe3
  
  ## envelope4
  p <- est$prevalence
  
  p[,prettyName := ""]
  p[state %in% c("T_CI","T_C"), prettyName := "HCV treatment (excl. transplant)"]
  p <- p[prettyName != ""]
  p[,prettyName:=factor(prettyName,levels=c(
    "HCV treatment (excl. transplant)"
  ))]
  
  pt <- p[year %in% yearsOfInterest,.(num=round(sum(num))),by=.(year,prettyName)]
  pt[,type:="Total"]
  p <- p[year %in% yearsOfInterest,.(num=round(sum(num))),by=.(year,type,prettyName)]
  p <- rbind(p,pt)
  p[,type:=factor(type,levels=c("Total","PWID","Ex-PWID","Cured PWID"))]
  #p[,year:=-year]
  pe4 <- dcast.data.table(p, prettyName~type+year,value.var="num")
  pe4[,prettyName:=as.character(prettyName)]
  pe4
  
  # PREVALENCE
  p <- est$prevalence
  
  p[,prettyName := ""]
  p[state %in% c("HCV_CI", "T_CI"), prettyName := "HCV+ chronic"]
  p[state %in% c("HCV_C","T_C","S_C"), prettyName := "Cirrhosis"]
  p[state %in% c("HCC"), prettyName := "HCC"]
  p[state %in% c("LT"), prettyName := "Transplant"]
  p <- p[prettyName != ""]
  p[,prettyName:=factor(prettyName,levels=c(
    "HCV+ chronic",
    "Cirrhosis",
    "HCC",
    "Transplant"
  ))]
  
  pt <- p[year %in% yearsOfInterest,.(num=round(sum(num))),by=.(year,prettyName)]
  pt[,type:="Total"]
  p <- p[year %in% yearsOfInterest,.(num=round(sum(num))),by=.(year,type,prettyName)]
  p <- rbind(p,pt)
  p[,type:=factor(type,levels=c("Total","PWID","Ex-PWID","Cured PWID"))]
  #p[,year:=-year]
  pp <- dcast.data.table(p, prettyName~type+year,value.var="num")
  pp[,prettyName:=as.character(prettyName)]
  pp
  
  # INCIDENCE
  p <- est$transition
  p[,prettyName := ""]
  p[end=="HCV_AI",prettyName := "HCV+ acute"]
  p[end=="HCV_CI" & start=="HCV_AI",prettyName := "HCV+ chronic"]
  p[end=="HCV_C" & start=="HCV_CI",prettyName := "Cirrhosis"]
  p[end=="HCC",prettyName := "HCC"]
  p[end=="LT",prettyName := "Transplant"]
  p <- p[prettyName != ""]
  p[,prettyName:=factor(prettyName,levels=c(
    "HCV+ acute",
    "HCV+ chronic",
    "Cirrhosis",
    "HCC",
    "Transplant"
  ))]
  
  pt <- p[year %in% yearsOfInterest,.(num=round(sum(num))),by=.(year,prettyName)]
  pt[,type:="Total"]
  p <- p[year %in% yearsOfInterest,.(num=round(sum(num))),by=.(year,type,prettyName)]
  #p[prettyName=="HCV+ acute",num:=sum(num),by=year]
  #p[prettyName=="HCV+ acute" & type=="Ex-PWID",num:=0]
  p <- rbind(p,pt)
  p[,type:=factor(type,levels=c("Total","PWID","Ex-PWID","Cured PWID"))]
  #p[,year:=-year]
  pi <- dcast.data.table(p, prettyName~type+year,value.var="num")
  pi[,prettyName:=as.character(prettyName)]
  pi
  
  
  # SPOT MORTALITY
  
  p <- est$transition
  p[,prettyName := ""]
  p[end=="M" & start %in% c("HCV_C","T_C"), prettyName := "Cirrhosis"]
  p[end=="M" & start %in% c("S_C"), prettyName := "Cirrhosis"]
  p[end=="M" & start %in% c("HCC"), prettyName := "HCC"]
  p[end=="M" & start %in% c("LT"), prettyName := "Transplant"]
  p <- p[prettyName != ""]
  p[,prettyName:=factor(prettyName,levels=c(
    "Cirrhosis",
    "HCC",
    "Transplant"
  ))]
  
  pt <- p[year %in% yearsOfInterest,.(num=round(sum(num))),by=.(year,prettyName)]
  pt[,type:="Total"]
  p <- p[year %in% yearsOfInterest,.(num=round(sum(num))),by=.(year,type,prettyName)]
  p <- rbind(p,pt)
  p[,type:=factor(type,levels=c("Total","PWID","Ex-PWID","Cured PWID"))]
  #p[,year:=-year]
  psmSPECIFIC <- dcast.data.table(p, prettyName~type+year,value.var="num")
  psmSPECIFIC[,prettyName:=as.character(prettyName)]
  psmSPECIFIC
  
  
  p <- est$transition
  p[,prettyName := ""]
  p[end=="M",prettyName := "HCV related"]
  p[end=="D",prettyName := "Not related to HCV"]
  p <- p[prettyName != ""]
  p[,prettyName:=factor(prettyName,levels=c(
    "HCV related",
    "Not related to HCV"
  ))]
  
  pt <- p[year %in% yearsOfInterest,.(num=round(sum(num))),by=.(year,prettyName)]
  pt[,type:="Total"]
  p <- p[year %in% yearsOfInterest,.(num=round(sum(num))),by=.(year,type,prettyName)]
  p <- rbind(p,pt)
  p[,type:=factor(type,levels=c("Total","PWID","Ex-PWID","Cured PWID"))]
  #p[,year:=-year]
  psm <- dcast.data.table(p, prettyName~type+year,value.var="num")
  psm[,prettyName:=as.character(prettyName)]
  psm
  
  # CUMULATIVE MORTALITY
  p <- est$prevalence
  
  p[,prettyName := ""]
  p[state %in% c("M"), prettyName := "HCV related"]
  p[state %in% c("D"), prettyName := "Not related to HCV"]
  p <- p[prettyName != ""]
  p[,prettyName:=factor(prettyName,levels=c(
    "HCV related",
    "Not related to HCV"
  ))]
  
  pt <- p[year %in% yearsOfInterest,.(num=round(sum(num))),by=.(year,prettyName)]
  pt[,type:="Total"]
  p <- p[year %in% yearsOfInterest,.(num=round(sum(num))),by=.(year,type,prettyName)]
  p <- rbind(p,pt)
  p[,type:=factor(type,levels=c("Total","PWID","Ex-PWID","Cured PWID"))]
  #p[,year:=-year]
  pcm <- dcast.data.table(p, prettyName~type+year,value.var="num")
  pcm[,prettyName:=as.character(prettyName)]
  pcm
  
  # YLL
  # http://ssb.no/natur-og-miljo/barekraft/forventet-levealder-ved-fodselen
  # gutt 79,7 år og en jente 83,6 år.
  lifeExpectancy <- (79.7*128 + 83.6*44)/(128+44)
  
  p <- est$transition
  p[,prettyName := ""]
  p[end=="M",prettyName := "HCV related"]
  p[end=="D",prettyName := "Not related to HCV"]
  p <- p[prettyName != ""]
  p[,prettyName:=factor(prettyName,levels=c(
    "HCV related",
    "Not related to HCV"
  ))]
  p[,yll:=lifeExpectancy-age]
  p[yll<0, yll:=0]
  
  pt <- p[year %in% yearsOfInterest,.(num=round(sum(num*yll))),by=.(year,prettyName)]
  pt[,type:="Total"]
  p <- p[year %in% yearsOfInterest,.(num=round(sum(num*yll))),by=.(year,type,prettyName)]
  p <- rbind(p,pt)
  p[,type:=factor(type,levels=c("Total","PWID","Ex-PWID","Cured PWID"))]
  #p[,year:=-year]
  psyll <- dcast.data.table(p, prettyName~type+year,value.var="num")
  psyll[,prettyName:=as.character(prettyName)]
  psyll
  
  p <- est$transition
  p[,prettyName := ""]
  p[end=="M" & start %in% c("HCV_C","T_C"), prettyName := "Cirrhosis"]
  p[end=="M" & start %in% c("S_C"), prettyName := "Cirrhosis"]
  p[end=="M" & start %in% c("HCC"), prettyName := "HCC"]
  p[end=="M" & start %in% c("LT"), prettyName := "Transplant"]
  p <- p[prettyName != ""]
  p[,prettyName:=factor(prettyName,levels=c(
    "Cirrhosis",
    "HCC",
    "Transplant"
  ))]
  p[,yll:=lifeExpectancy-age]
  p[yll<0, yll:=0]
  
  pt <- p[year %in% yearsOfInterest,.(num=round(sum(num*yll))),by=.(year,prettyName)]
  pt[,type:="Total"]
  p <- p[year %in% yearsOfInterest,.(num=round(sum(num*yll))),by=.(year,type,prettyName)]
  p <- rbind(p,pt)
  p[,type:=factor(type,levels=c("Total","PWID","Ex-PWID","Cured PWID"))]
  #p[,year:=-year]
  psyllSPECIFIC <- dcast.data.table(p, prettyName~type+year,value.var="num")
  psyllSPECIFIC[,prettyName:=as.character(prettyName)]
  psyllSPECIFIC
  
  # YLD
  p <- est$prevalence
  
  p[, prettyName:=""]
  p[state %in% c("HCV_AI"), prettyName := "HCV+ acute"]
  p[state %in% c("HCV_CI", "T_CI"), prettyName := "HCV+ chronic"]
  p[state %in% c("HCV_C","T_C"), prettyName := "Cirrhosis"]
  p[state %in% c("S_C"), prettyName := "Cirrhosis"]
  p[state %in% c("HCC"), prettyName := "HCC"]
  p[state %in% c("LT"), prettyName := "Transplant"]
  
  p[,dw := 0]
  #Infectious disease: post-acute consequences (fatigue, emotional lability, insomnia)
  p[state %in% c("HCV_AI"), dw := 0.254]
  p[state %in% c("HCV_CI", "T_CI"), dw := 0.254]
  #Infectious disease: post-acute consequences (fatigue, emotional lability, insomnia)
  #Decompensated cirrhosis of the liver
  p[state %in% c("HCV_C","T_C"), dw := 1-(1-0.254)*(1-0.194)]
  #Decompensated cirrhosis of the liver
  p[state %in% c("S_C"), dw := 0.194]
  #Infectious disease: post-acute consequences (fatigue, emotional lability, insomnia)
  #Terminal phase: with medication (for cancers, end-stage kidney or liver disease)
  p[state %in% c("HCC"), dw := 1-(1-0.254)*(1-0.508)]
  p[state %in% c("LT"), dw := 1-(1-0.254)*(1-0.508)]
  
  unique(p[dw==0]$state)
  p <- p[prettyName != ""]
  px <- copy(p)
  px[,prettyName:="Total"]
  p <- rbind(p,px)
  p[,prettyName:=factor(prettyName,levels=c(
    "HCV+ acute",
    "HCV+ chronic",
    "Cirrhosis",
    "HCC",
    "Transplant",
    "Total"
  ))]
  
  
  pt <- p[year %in% yearsOfInterest,.(num=round(sum(num*dw))),by=.(year,prettyName)]
  pt[,type:="Total"]
  p <- p[year %in% yearsOfInterest,.(num=round(sum(num*dw))),by=.(year,type,prettyName)]
  p <- rbind(p,pt)
  p[,type:=factor(type,levels=c("Total","PWID","Ex-PWID","Cured PWID"))]
  #p[,year:=-year]
  psyld <- dcast.data.table(p, prettyName~type+year,value.var="num")
  psyld[,prettyName:=as.character(prettyName)]
  psyld
  
  temp <- data.frame(rbind(psyll[prettyName=="HCV related"],  psyld[prettyName=="Total"]))
  temp <- data.table(t(data.frame(c("HCV related",apply(temp[,-1],2,sum)))))
  setnames(temp,names(psyld))
  pdalyALL <- temp
  
  pdalyACUTE <- psyld[prettyName=="HCV+ acute"]
  pdalyCHRONIC <- psyld[prettyName=="HCV+ chronic"]
  
  temp <- data.frame(rbind(psyllSPECIFIC[prettyName=="Cirrhosis"],  psyld[prettyName=="Cirrhosis"]))
  temp <- data.table(t(data.frame(c("Cirrhosis",apply(temp[,-1],2,sum)))))
  setnames(temp,names(psyld))
  pdalyCIRRHOSIS <- temp
  
  temp <- data.frame(rbind(psyllSPECIFIC[prettyName=="HCC"],  psyld[prettyName=="HCC"]))
  temp <- data.table(t(data.frame(c("HCC",apply(temp[,-1],2,sum)))))
  setnames(temp,names(psyld))
  pdalyHCC <- temp
  
  temp <- data.frame(rbind(psyllSPECIFIC[prettyName=="Transplant"],  psyld[prettyName=="Transplant"]))
  temp <- data.table(t(data.frame(c("Transplant",apply(temp[,-1],2,sum)))))
  setnames(temp,names(psyld))
  pdalyTRANSPLANT <- temp
  
  return(rbind(pe1,pe2,pe3,pe4,pp,pi,psmSPECIFIC,psm,pcm,psyllSPECIFIC,psyll,psyld,
               pdalyACUTE,pdalyCHRONIC,pdalyCIRRHOSIS,pdalyHCC,pdalyTRANSPLANT,pdalyALL))
}

TableSummary <- function(yearsOfInterest=(c(2000,2015,2030))){       
  tab <- htmlTable(TableSummaryInt(readRDS(file.path(RAWmisc::PROJ$BAKED,"est.RDS")),yearsOfInterest=yearsOfInterest),
                   rnames=FALSE,
                   align="rc",
                   rgroup=c("Overlapping prevalence","Discrete prevalence","Incidence","Yearly mortality","Cumulative mortality",
                            "YLLs","YLDs","DALYs"),
                   n.rgroup=c(4,4,5,5,2,5,6,6),
                   header = c("",rep(yearsOfInterest,4)),
                   cgroup=c("","Total","PWID","Former PWID (will relapse)","Former PWID (won't relapse)"),
                   n.cgroup=c(1,rep(length(yearsOfInterest),4)))
  saveRDS(tab,file=file.path(RAWmisc::PROJ$BAKED,"Table_Summary.RDS"))
}

TableSummaryInt2 <- function(est,yearsOfInterest=(c(2000,2015,2030))){   
  tab <- TableSummaryInt(est,yearsOfInterest=yearsOfInterest)
  tab <- data.frame(tab)
  for(i in 1:length(yearsOfInterest)){
      reference <- 1+length(yearsOfInterest)*2+i
      changing <- reference+length(yearsOfInterest)
      tab[,reference] <- round(as.numeric(tab[,changing])+as.numeric(tab[,reference]))
    }
  remove <- (ncol(tab)-length(yearsOfInterest)+1):ncol(tab)
  tab <- tab[,-remove]
  return(tab)
}

TableSummaryPerc2Int <- function(est,yearsOfInterest=(c(2000,2015,2030))){
  tab <- TableSummaryInt2(est,yearsOfInterest=yearsOfInterest)
  tab <- data.frame(tab)
  for(i in 1:length(yearsOfInterest)){
      reference <- 1+i
      changing <- 1+i+length(yearsOfInterest)
      tab[,changing] <- as.numeric(tab[,changing])/as.numeric(tab[,reference])*100
    }
  
  #for(i in 1:nrow(tab)) for(j in 1:ncol(tab)) if(tab[i,j]=="NA%") tab[i,j] <- NA
  remove <- (ncol(tab)-length(yearsOfInterest)+1):ncol(tab)
  tab <- tab[,-remove]
  for(i in 2:ncol(tab)) tab[,i] <- as.numeric(tab[,i])
  return(tab)
  tab <- htmlTable(tab,
                   rnames=FALSE,
                   align="rc",
                   rgroup=c("Overlapping prevalence","Discrete prevalence","Incidence","Yearly mortality","Cumulative mortality",
                            "YLLs","YLDs","DALYs"),
                   n.rgroup=c(4,4,5,5,2,5,6,6),
                   header = c("",rep(yearsOfInterest,2)),
                   cgroup=c("","Absolute numbers","PWID % attribution&dagger;"),
                   n.cgroup=c(1,rep(length(yearsOfInterest),2)),
                   tfoot="&dagger;Percent absolute numbers attributable to active PWIDs")
  saveRDS(tab,file=file.path(RAWmisc::PROJ$BAKED,"Table_SummaryPerc2.RDS"))
}

TableSummaryPerc2 <- function(yearsOfInterest=(c(2000,2015,2030))){
  tableCentre <- TableSummaryPerc2Int(readRDS(file.path(RAWmisc::PROJ$BAKED,"est.RDS")),yearsOfInterest=yearsOfInterest)
  tableSummary <- vector("list",1000)
  for(draw in 1:length(tableSummary)){
    print(draw)
    est <- readRDS(file.path(RAWmisc::PROJ$BAKED,paste0("draw_",draw,".RDS")))
    tableSummary[[draw]] <- TableSummaryPerc2Int(est,yearsOfInterest=yearsOfInterest)
  }
  
  x <- array(unlist(tableSummary), dim = c(nrow(tableSummary[[1]]),ncol(tableSummary[[1]]),length(tableSummary)))
  l <- apply(x,1:2,function(x){round(quantile(as.numeric(x),probs=0.025,na.rm=T))})
  u <- apply(x,1:2,function(x){round(quantile(as.numeric(x),probs=0.975,na.rm=T))})
  
  mx <- u <- apply(x,1:2,function(x){round(quantile(as.numeric(x),probs=0.5,na.rm=T))})
  m <- as.data.frame(tableCentre)
  
  retval <- m[,1]
  for(i in 2:4){
    retval <- cbind(retval,round(m[,i]),paste(l[,i],u[,i],sep=" to "))
  }
  for(i in 5:ncol(m)){
    retval <- cbind(retval,paste0(round(m[,i]),"%"))
  }
  for(j in 2:ncol(retval)){
    retval[retval[,j]=="NA to NA",j] <- NA
    retval[retval[,j]=="NaN%",j] <- NA
  }
  
  tab <- htmlTable(retval,
                   rnames=FALSE,
                   align="rc",
                   rgroup=c("Overlapping prevalence","Discrete prevalence","Incidence","Yearly mortality","Cumulative mortality",
                            "YLLs","YLDs","DALYs"),
                   n.rgroup=c(4,4,5,5,2,5,6,6),
                   header = c("",rep(yearsOfInterest,each=2),yearsOfInterest),
                   cgroup=c("","Absolute numbers with 95% CI","PWID % attribution&dagger;"),
                   n.cgroup=c(1,6,3),
                   tfoot="&dagger;Percent absolute numbers attributable to active PWIDs")
  saveRDS(tab,file=file.path(RAWmisc::PROJ$BAKED,"Table_SummaryPerc2.RDS"))
}


FigureIncidenceInt <- function(est){
  p <- est$transition
  p <- p[end=="HCV_AI",.(num=sum(num)),by=.(year)]
  return(p)
}

FigureNumPWIDInt <- function(est){
  p <- est$prevalence
  p <- p[type=="PWID" & !state%in%c("D","M"),.(num=sum(num)),by=.(year)]
  return(p)
}

FigureIncidence <- function(){
  tableCentre <- FigureIncidenceInt(readRDS(file.path(RAWmisc::PROJ$BAKED,"est.RDS")))
  tableDenom <- FigureNumPWIDInt(readRDS(file.path(RAWmisc::PROJ$BAKED,"est.RDS")))
  
  pd <- merge(tableCentre, tableDenom, by="year")
  setnames(pd,c("year","num","denom"))
  pd[,rateper100k:=num/denom*100000]
  pd[,denom:=NULL]
  tableCentre <- pd
  
  tableSummary <- vector("list",1000)
  for(draw in 1:1000){
    print(draw)
    est <- readRDS(file.path(RAWmisc::PROJ$BAKED,paste0("draw_",draw,".RDS")))
    num <- FigureIncidenceInt(est)
    denom <- FigureNumPWIDInt(est)
    
    pd <- merge(num, denom, by="year")
    setnames(pd,c("year","num","denom"))
    pd[,rateper100k:=num/denom*100000]
    pd[,denom:=NULL]

    tableSummary[[draw]] <- pd
  }
  
  x <- array(unlist(tableSummary), dim = c(nrow(tableSummary[[1]]),ncol(tableSummary[[1]]),1000))
  l <- apply(x,1:2,function(x){quantile(as.numeric(x),probs=0.025,na.rm=T)})
  u <- apply(x,1:2,function(x){quantile(as.numeric(x),probs=0.975,na.rm=T)})
  
  m <- as.data.frame(tableCentre)
  
  retval <- m[,1]
  for(i in 2:ncol(m)){
    retval <- cbind(retval,m[,i],l[,i],u[,i])
  }
  retval <- data.frame(retval)
  num <- retval[,1:4]
  denom <- retval[,c(1,5:7)]
  num$variable <- "Number of newly infected HCV per year"
  denom$variable <- "Rate of newly infected HCV/100,000 PWIDs per year"
  names(num) <- c("year","m","l","u","variable")
  names(denom) <- c("year","m","l","u","variable")
  retval <- rbind(num,denom)
  
  q <- ggplot(retval,aes(x=year,y=m,ymin=l,ymax=u))
  q <- q + geom_rect(aes(xmin=2014,xmax=2030,ymin=-Inf,ymax=Inf),alpha=0.002,fill="yellow")
  q <- q + geom_ribbon(alpha=0.7)
  q <- q + geom_line(lwd=3)
  q <- q + facet_wrap(~variable,scales="free",ncol=1)
  
  q <- q + scale_x_continuous("")
  q <- q + scale_y_continuous("")
  q <- q + ggplot2::expand_limits(y=0)
  q <- q + theme_gray(base_size=22)
  RAWmisc::png_a4(file.path(RAWmisc::PROJ$SHARED_TODAY,"Figure_Incidence.png"),landscape=TRUE,w=0.5,h=0.5)
  print(q)
  RAWmisc::MakeFootnote("Yellow area denotes projections")
  dev.off()
}


FigureIncidenceCentre <- function(){
  tableCentre <- FigureIncidenceInt(readRDS(file.path(RAWmisc::PROJ$BAKED,"est.RDS")))
  tableDenom <- FigureNumPWIDInt(readRDS(file.path(RAWmisc::PROJ$BAKED,"est.RDS")))
  
  pd <- merge(tableCentre, tableDenom, by="year")
  setnames(pd,c("year","num","denom"))
  pd[,rateper100k:=num/denom*100000]
  pd[,denom:=NULL]
  setnames(pd,c("year","Number of newly infected HCV per year","Rate of newly infected HCV/100,000 PWIDs per year"))
  pd <- reshape2::melt(pd,id="year")
  
  q <- ggplot(pd,aes(x=year,y=value))
  q <- q + geom_rect(aes(xmin=2014,xmax=2030,ymin=-Inf,ymax=Inf),alpha=0.002,fill="yellow")
  q <- q + geom_line(lwd=3)
  q <- q + facet_wrap(~variable,scales="free",ncol=1)
  
  q <- q + scale_x_continuous("")
  q <- q + scale_y_continuous("")
  q <- q + ggplot2::expand_limits(y=0)
  q <- q + theme_gray(base_size=22)
  RAWmisc::png_a4(file.path(RAWmisc::PROJ$SHARED_TODAY,"Figure_Incidence.png"),landscape=TRUE,w=0.5,h=0.5)
  print(q)
  RAWmisc::MakeFootnote("Yellow area denotes projections")
  dev.off()
}


FigureArea <- function(){
  p <- readRDS(file.path(RAWmisc::PROJ$BAKED,"est.RDS"))$prevalence
  unique(p$state)
  
  p[,prettyName := ""]
  p[state %in% c("S"), prettyName := "HCV-"]
  p[state %in% c("HCV_AI"), prettyName := "HCV+ acute"]
  p[state %in% c("HCV_CI", "T_CI"), prettyName := "HCV+ chronic"]
  p[state %in% c("HCV_C","T_C","S_C"), prettyName := "Cirrhosis"]
  p[state %in% c("HCC"), prettyName := "HCC"]
  p[state %in% c("LT"), prettyName := "Transplant"]
  p[state %in% c("M"), prettyName := "Cumulative HCV mortality"]
  p <- p[prettyName != ""]
  p[,prettyName:=factor(prettyName,levels=c(
    "HCV-",
    "HCV+ acute",
    "HCV+ chronic",
    "Cirrhosis",
    "HCC",
    "Transplant",
    "Cumulative HCV mortality"
  ))]
  p[,type:=factor(type,levels=c("PWID","Ex-PWID","Cured PWID"))]
  levels(p$type) <- c("PWID","Former PWID (will relapse)","Former PWID (won't relapse)")
  sum(p[prettyName=="HCV mortality" & year==2030]$num)
  ppgraph <- p[,.(num=round(sum(num))),by=.(year,type,prettyName)]
  
  setorder(ppgraph,-prettyName)
  q <- ggplot(ppgraph,aes(x=year,y=num))
  q <- q + geom_rect(aes(xmin=2014,xmax=2030,ymin=-Inf,ymax=Inf),alpha=0.002,fill="yellow")
  q <- q + geom_area(aes(fill=prettyName),alpha=0.8)
  q <- q + facet_wrap(~type,scales="free")
  q <- q + scale_fill_brewer("",palette="Set2")
  
  q <- q + scale_x_continuous("")
  q <- q + scale_y_continuous("",lim=c(0,12500))
  q <- q + theme_gray(base_size=22)
  RAWmisc::png_a4(file.path(RAWmisc::PROJ$SHARED_TODAY,"Figure_Area.png"),landscape=TRUE,w=0.5,h=0.25)
  print(q)
  RAWmisc::MakeFootnote("Yellow area denotes projections")
  dev.off()
}

FigureArea2 <- function(){
  p <- readRDS(file.path(RAWmisc::PROJ$BAKED,"est.RDS"))$prevalence
  unique(p$state)
  
  p[,prettyName := ""]
  p[state %in% c("S"), prettyName := "HCV-"]
  p[state %in% c("HCV_AI"), prettyName := "HCV+ acute"]
  p[state %in% c("HCV_CI", "T_CI"), prettyName := "HCV+ chronic"]
  p[state %in% c("HCV_C","T_C","S_C"), prettyName := "Cirrhosis"]
  p[state %in% c("HCC"), prettyName := "HCC"]
  p[state %in% c("LT"), prettyName := "Transplant"]
  p[state %in% c("M"), prettyName := "Cumulative HCV mortality"]
  p <- p[prettyName != ""]
  p[,prettyName:=factor(prettyName,levels=c(
    "HCV-",
    "HCV+ acute",
    "HCV+ chronic",
    "Cirrhosis",
    "HCC",
    "Transplant",
    "Cumulative HCV mortality"
  ))]
  p[,newType:=type]
  p[type=="Ex-PWID",newType:="Former PWID"]
  p[type=="Cured PWID",newType:="Former PWID"]
  p[,type:=newType]
  p[,newType:=NULL]
  p[,type:=factor(type,levels=c("PWID","Former PWID"))]
  levels(p$type) <- c("PWID","Former PWID")
  sum(p[prettyName=="HCV mortality" & year==2030]$num)
  ppgraph <- p[,.(num=round(sum(num))),by=.(year,type,prettyName)]
  
  setorder(ppgraph,-prettyName)
  q <- ggplot(ppgraph,aes(x=year,y=num))
  q <- q + geom_rect(aes(xmin=2014,xmax=2030,ymin=-Inf,ymax=Inf),alpha=0.002,fill="yellow")
  q <- q + geom_area(aes(fill=prettyName),alpha=0.8)
  q <- q + facet_wrap(~type,scales="free")
  q <- q + scale_fill_brewer("",palette="Set2")
  
  q <- q + scale_x_continuous("")
  q <- q + scale_y_continuous("",lim=c(0,20000))
  q <- q + theme_gray(base_size=22)
  RAWmisc::png_a4(file.path(RAWmisc::PROJ$SHARED_TODAY,"Figure_Area2.png"),landscape=TRUE,w=0.5,h=0.375)
  print(q)
  RAWmisc::MakeFootnote("Yellow area denotes projections")
  dev.off()
}

FigureESCAIDEArea <- function(){
  p <- readRDS(file.path(RAWmisc::PROJ$BAKED,"est.RDS"))$prevalence
  unique(p$state)
  
  p[,prettyName := ""]
  p[state %in% c("S"), prettyName := "HCV-"]
  p[state %in% c("HCV_AI"), prettyName := "HCV+ acute"]
  p[state %in% c("HCV_CI", "T_CI"), prettyName := "HCV+ chronic"]
  p[state %in% c("HCV_C","T_C","S_C"), prettyName := "Cirrhosis"]
  p[state %in% c("HCC"), prettyName := "HCC"]
  p[state %in% c("LT"), prettyName := "Transplant"]
  p[state %in% c("M"), prettyName := "Cumulative HCV mortality"]
  p <- p[prettyName != ""]
  p[,prettyName:=factor(prettyName,levels=c(
    "HCV-",
    "HCV+ acute",
    "HCV+ chronic",
    "Cirrhosis",
    "HCC",
    "Transplant",
    "Cumulative HCV mortality"
  ))]
  p[type=="Ex-PWID",type:="Former PWID"]
  p[type=="Cured PWID",type:="Former PWID"]
  p[,type:=factor(type,levels=c("PWID","Former PWID"))]
  sum(p[prettyName=="HCV mortality" & year==2030]$num)
  ppgraph <- p[,.(num=round(sum(num))),by=.(year,type,prettyName)]
  
  setorder(ppgraph,-prettyName)
  q <- ggplot(ppgraph,aes(x=year,y=num))
  q <- q + geom_rect(aes(xmin=2014,xmax=2030,ymin=-Inf,ymax=Inf),alpha=0.002,fill="yellow")
  q <- q + geom_area(aes(fill=prettyName),alpha=0.8)
  q <- q + facet_wrap(~type,scales="free")
  q <- q + scale_fill_brewer("",palette="Set2")
  
  q <- q + scale_x_continuous("")
  q <- q + scale_y_continuous("",lim=c(0,20000))
  q <- q + theme_gray(base_size=22)
  RAWmisc::png_a4(file.path(RAWmisc::PROJ$SHARED_TODAY,"Figure_ESCAIDEArea.png"),landscape=TRUE,w=0.5,h=0.375)
  print(q)
  RAWmisc::MakeFootnote("Yellow area denotes projections")
  dev.off()
}

FigureAge <- function(){
  est <- readRDS(file.path(RAWmisc::PROJ$BAKED,"est.RDS"))
  p <- est$prevalence
  unique(p$state)
  
  pt <- est$prevalence
  pt[,prettyName := ""]
  pt[!state %in% c("M","D"), prettyName := "Alive cohort"]
  pt <- pt[prettyName != ""]
  pt <- pt[,.(meanAge=sum(num*age)/sum(num)),by=.(year,prettyName)]
  
  p[,prettyName := ""]
  p[state %in% c("S"), prettyName := "HCV-"]
  p[state %in% c("HCV_AI"), prettyName := "HCV+ acute"]
  p[state %in% c("HCV_CI", "T_CI"), prettyName := "HCV+ chronic"]
  p[state %in% c("HCV_C","T_C","S_C"), prettyName := "Cirrhosis"]
  p[state %in% c("HCC"), prettyName := "HCC"]
  p[state %in% c("LT"), prettyName := "Transplant"]
  p <- p[prettyName != ""]
  p[,type:=factor(type,levels=c("PWID","Ex-PWID"))]
  sum(p[prettyName=="HCV mortality" & year==2030]$num)
  pp <- rbind(pt,p[,.(meanAge=sum(num*age)/sum(num)),by=.(year,prettyName)])
  pp[,measure:="Prevalence"]
  
  # INCIDENCE
  p <- est$transition
  p[,prettyName := ""]
  p[end=="HCV_AI",prettyName := "HCV+ acute"]
  p[end=="HCV_CI" & start=="HCV_AI",prettyName := "HCV+ chronic"]
  p[end=="HCV_C" & start=="HCV_CI",prettyName := "Cirrhosis"]
  p[end=="HCC",prettyName := "HCC"]
  p[end=="LT",prettyName := "Transplant"]
  p[end=="M",prettyName := "HCV mortality"]
  p <- p[prettyName != ""]
  pi <- p[,.(meanAge=sum(num*age)/sum(num)),by=.(year,prettyName)]
  pi[,measure:="Incidence"]
  
  ppgraph <- rbind(pp,pi)
  ppgraph[,prettyName:=factor(prettyName,levels=c(
    "Alive cohort",
    "HCV-",
    "HCV+ acute",
    "HCV+ chronic",
    "Cirrhosis",
    "HCC",
    "Transplant",
    "HCV mortality"
  ))]
  
  q <- ggplot(ppgraph,aes(x=year,y=meanAge))
  q <- q + geom_rect(aes(xmin=2014,xmax=2030,ymin=-Inf,ymax=Inf),alpha=0.002,fill="yellow")
  q <- q + geom_line(aes(colour=prettyName),lwd=4)
  q <- q + scale_colour_brewer("",palette="Set2")
  q <- q + facet_wrap(~measure, scales="free")
  
  q <- q + scale_x_continuous("")
  q <- q + scale_y_continuous("Mean age\n",lim=c(10,65))
  q <- q + theme_gray(base_size=22)
  RAWmisc::png_a4(file.path(RAWmisc::PROJ$SHARED_TODAY,"Figure_Age.png"),landscape=TRUE,w=0.5,h=0.375)
  print(q)
  RAWmisc::MakeFootnote("Yellow area denotes projections")
  dev.off()
}

FigureFittingInt <- function(est){
  modelFit <- merge(est[["fitting"]], targets, by=c("year","variable"))
  modelFit[,prettyName:=""]
  modelFit[variable=="newHCC",prettyName:="New HCC"]
  modelFit[variable=="newLT",prettyName:="New LT"]
  modelFit[variable=="newMortalityCIRR",prettyName:="New cirrhosis mortality"]
  modelFit[variable=="newMortalityHCC",prettyName:="New HCC mortality"]
  modelFit[variable=="percPWIDHCV",prettyName:="Percentage of PWIDs with HCV"]
  modelFit[variable=="newSuccessTreatedHCV",prettyName:="New Successfully treated HCV (not cirrhosis)"]
  modelFit[variable=="treatCIRR",prettyName:="Cirrhotics entering treatment"]
  modelFit[variable=="numHCC",prettyName:="Number of HCC"]
  return(modelFit)
  
}

FigureFitting <- function(){
  tableCentre <- FigureFittingInt(readRDS(file.path(RAWmisc::PROJ$BAKED,"est.RDS")))
  tableSummary <- vector("list",1000)
  for(draw in 1:length(tableSummary)){
    print(draw)
    est <- readRDS(file.path(RAWmisc::PROJ$BAKED,paste0("draw_",draw,".RDS")))
    tableSummary[[draw]] <- FigureFittingInt(est)
  }
  ts <- rbindlist(tableSummary)
  ts <- ts[,.(l=quantile(value,probs=0.025),u=quantile(value,probs=0.975)),by=.(year,prettyName)]
  
  q <- ggplot(tableCentre, aes(x=year))
  q <- q + geom_hline(yintercept=0,alpha=0)
  q <- q + geom_ribbon(data=ts,mapping=aes(ymin=l,ymax=u),alpha=0.5)
  q <- q + geom_line(aes(y=value),lwd=2)
  q <- q + geom_point(aes(y=value), col="black", alpha=0.7, size=6, shape=12)
  q <- q + geom_point(aes(y=observed), col="red", alpha=0.7, size=6)
  q <- q + facet_wrap(~prettyName, scales="free")
  
  q <- q + scale_x_continuous("",lim=c(2000,2013))
  q <- q + scale_y_continuous("")
  q <- q + theme_gray(base_size=22)
  RAWmisc::png_a4(file.path(RAWmisc::PROJ$SHARED_TODAY,"Figure_Fitting.png"),landscape=TRUE,w=0.5,h=0.5)
  print(q)
  dev.off()
}

FigureFittingCentre <- function(file=NULL){
  if(is.null(file)) file <- file.path(RAWmisc::PROJ$BAKED,"est.RDS")
  tableCentre <- FigureFittingInt(readRDS(file))
  tableSummary <- vector("list",1000)
  
  q <- ggplot(tableCentre, aes(x=year))
  q <- q + geom_hline(yintercept=0,alpha=0)
  q <- q + geom_line(aes(y=value),lwd=2)
  q <- q + geom_point(aes(y=value), col="black", alpha=0.7, size=6, shape=12)
  q <- q + geom_point(aes(y=observed), col="red", alpha=0.7, size=6)
  q <- q + facet_wrap(~variable, scales="free")
  q <- q + scale_x_continuous("",lim=c(2000,2013))
  q <- q + scale_y_continuous("")
  q <- q + theme_gray(base_size=22)
  RAWmisc::png_a4(file.path(RAWmisc::PROJ$SHARED_TODAY,"Figure_Fitting.png"),landscape=TRUE,w=0.5,h=0.5)
  print(q)
  dev.off()
}

FigureAgeDebut <- function(){
  meanAge <- readRDS(file=file.path(RAWmisc::PROJ$BAKED,"AgeDebut_meanAge.RDS"))
  sirusAges <- readRDS(file=file.path(RAWmisc::PROJ$BAKED,"AgeDebut_sirusAges.RDS"))
  q <- ggplot2::ggplot(meanAge,ggplot2::aes(x=year))
  q <- q + ggplot2::geom_line(ggplot2::aes(y=mean),lwd=2)
  q <- q + ggplot2::geom_point(data=sirusAges,mapping=ggplot2::aes(y=y),colour="red",size=6, alpha=0.7)
  
  q <- q + ggplot2::scale_x_continuous("Year")
  q <- q + ggplot2::scale_y_continuous("Mean age of debut",lim=c(0,30))
  q <- q + theme_gray(base_size=22)
  RAWmisc::png_a4(file.path(RAWmisc::PROJ$SHARED_TODAY,"Figure_AgeDebut.png"),landscape=TRUE,w=0.5,h=0.5)
  print(q)
  dev.off()
  
}

FigurePWID <- function(){
  pwidGraph <- readRDS(file=file.path(RAWmisc::PROJ$BAKED,"pwid.RDS"))
  pwidGraph[,prettyName:=""]
  pwidGraph[variable=="numPWID",prettyName:="PWID"]
  pwidGraph[variable=="numEXPWID",prettyName:="Former PWID (will relapse)"]
  pwidGraph[variable=="numCUREDPWID",prettyName:="Former PWID (will not relapse)"]
  pwidGraph[,prettyName:=factor(prettyName,levels=c(
    "PWID","Former PWID (will relapse)","Former PWID (will not relapse)"
  ))]

  q <- ggplot(pwidGraph,aes(x=year))
  q <- q + geom_line(aes(y=value),lwd=2)
  q <- q + geom_point(aes(y=obs),col="red",size=6, alpha=0.7)
  q <- q + facet_wrap(~prettyName,scales="free")
  
  q <- q + ggplot2::scale_x_continuous("Year")
  q <- q + ggplot2::scale_y_continuous("Number of people",lim=c(0,15000))
  q <- q + theme_gray(base_size=22)
  RAWmisc::png_a4(file.path(RAWmisc::PROJ$SHARED_TODAY,"Figure_PWID.png"),landscape=TRUE,w=0.5,h=0.375)
  print(q)
  dev.off()

}



FigureESCAIDENumberHCVInfected <- function(){
  p <- readRDS(file.path(RAWmisc::PROJ$BAKED,"est.RDS"))$prevalence
  unique(p$state)
  
  p[,prettyName := ""]
  p[state %in% c("S","S_C"), prettyName := "HCV-"]
  p[state %in% c("HCV_AI"), prettyName := "HCV+"]
  p[state %in% c("HCV_CI", "T_CI"), prettyName := "HCV+"]
  p[state %in% c("HCV_C","T_C"), prettyName := "HCV+"]
  p[state %in% c("HCC"), prettyName := "HCV+"]
  p[state %in% c("LT"), prettyName := "HCV+"]
  p <- p[prettyName != ""]
  p[,prettyName:=factor(prettyName,levels=c(
    "HCV-",
    "HCV+"
  ))]
  p[type=="Ex-PWID",type:="Former PWID"]
  p[type=="Cured PWID",type:="Former PWID"]
  p[,type:=factor(type,levels=c("PWID","Former PWID"))]
  sum(p[prettyName=="HCV mortality" & year==2030]$num)
  ppgraph <- p[year==2013,.(num=round(sum(num))),by=.(year,type,prettyName)]
  ppgraph[,denom:=sum(num),by=type]
  ppgraph[prettyName=="HCV+",perc:=paste0(round(num/denom*100),"%")]
  
  setorder(ppgraph,-prettyName)
  
  q <- ggplot(ppgraph,aes(x=type,y=num,fill=prettyName,label=perc))
  q <- q + geom_bar(stat="identity")
  q <- q + geom_text(aes(y=3000),size=20)
  q <- q + scale_fill_brewer("",palette="Set2")
  
  q <- q + scale_x_discrete("")
  q <- q + scale_y_continuous("",lim=c(0,14000),breaks=seq(0,14000,2000))
  q
  q <- q + theme_gray(base_size=22)
  RAWmisc::png_a4(file.path(RAWmisc::PROJ$SHARED_TODAY,"Figure_ESCAIDEBar.png"),landscape=TRUE,w=0.25,h=0.375)
  print(q)
  dev.off()
}

FigureEnvelopePWIDPerc <- function(){
  AddingIVDUs <- readRDS(file=file.path(RAWmisc::PROJ$BAKED,"AddingIVDUs.RDS"))
  est <- readRDS(file=file.path(RAWmisc::PROJ$BAKED,"est.RDS"))
  targ <- targets[variable=="percPWIDHCV",c("year","observed"),with=F]
  
  numPWIDHCV=est$prevalence[type=="PWID" & state %in% c("HCV_AI","HCV_CI","HCV_C","HCC"),.(hcv=sum(num)),by=year]
  numPWID=est$prevalence[type=="PWID" & !state %in% c("M","D"),.(pwid=sum(num)),by=year]
  numtot <- merge(numPWIDHCV,numPWID,by=c("year"))
  for(i in unique(numtot$year)){
    numtot[year==i,newPWID1:=AddingIVDUs[i]]
    numtot[year==i,newPWID2:=AddingIVDUs[i-1]]
  }
  numtot[,perc:=hcv/pwid*100]
  numtot[,newperc1:=hcv/(pwid-newPWID1)*100]
  numtot[,newperc2:=hcv/(pwid-newPWID1-newPWID2)*100]
  numtot <- numtot[,c("year","perc","newperc1","newperc2"),with=F]
  numtot <- melt(numtot,id=c("year"))
  levels(numtot$variable) <- c("No removal","1 year","1-2 years")
  
  q <- ggplot(numtot[year>=1970])
  q <- q + geom_line(aes(x=year,y=value,colour=variable),lwd=3)
  q <- q + geom_point(data=targ, mapping=aes(x=year,y=observed*100),col="red",size=10)
  q <- q + scale_colour_brewer("Within X years",palette="Set2")
  q <- q + scale_y_continuous(lim=c(0,100))
  q <- q + labs(title="Prevalence of HCV within PWIDs\nRemoving subjects from denominator based on # years injecting")
  
  q <- q + scale_x_continuous("")
  q <- q + scale_y_continuous("Prevalence of HCV within PWIDs",lim=c(0,100))
  q <- q + theme_gray(base_size=22)
  RAWmisc::png_a4(file.path(RAWmisc::PROJ$SHARED_TODAY,"Figure_PWIDPerc.png"),landscape=TRUE,w=0.5,h=0.5)
  print(q)
  dev.off()
}

SubstrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

SubstrRemoveRight <- function(x, n){
  substr(x, 1,nchar(x)-n)
}

LargeSupplementalTables <- function(){
  TYPES <- c(rep("OVERLAPPING PREVALENCE",4),
             rep("DISCRETE PREVALENCE",4),
             rep("INCIDENCE",5),
             rep("YEARLY MORTALITY",5),
             rep("CUMULATIVE MORTALITY",2),
             rep("YLLs",5),
             rep("YLDs",6),
             rep("DALYs",6))
 
  tableCentre <- TableSummaryInt(readRDS(file.path(RAWmisc::PROJ$BAKED,"est.RDS")),yearsOfInterest=1973:2030)
  tableSummary <- vector("list",1000)
  for(draw in 1:length(tableSummary)){
    print(draw)
    est <- readRDS(file.path(RAWmisc::PROJ$BAKED,paste0("draw_",draw,".RDS")))
    tableSummary[[draw]] <- TableSummaryInt(est,yearsOfInterest=1973:2030)
  }
  
  x <- array(unlist(tableSummary), dim = c(nrow(tableSummary[[1]]),ncol(tableSummary[[1]]),length(tableSummary)))
  l <- apply(x,1:2,function(x){round(quantile(as.numeric(x),probs=0.025,na.rm=T))})
  u <- apply(x,1:2,function(x){round(quantile(as.numeric(x),probs=0.975,na.rm=T))})
  
  m <- as.data.frame(tableCentre)
  names(m) <- gsub("Ex-PWID","Former-PWID-Will-Relapse",names(m))
  names(m) <- gsub("Cured PWID","Former-PWID-Wont-Relapse",names(m))
  l <- as.data.frame(l)
  u <- as.data.frame(u)
  names(l) <- names(m)
  names(u) <- names(m)
  l[,1] <- m[,1]
  u[,1] <- m[,1]
  
  temp <- gather(data.frame(type=TYPES,l),key,value,-prettyName,-type)
  temp$year <- as.numeric(SubstrRight(as.character(temp$key),4))
  temp$variable <- SubstrRemoveRight(as.character(temp$key),5)
  temp <- temp[,c("type","variable","prettyName","year","value")]
  temp$value <- as.numeric(temp$value)
  machinel <- temp
  
  temp <- gather(data.frame(type=TYPES,u),key,value,-prettyName,-type)
  temp$year <- as.numeric(SubstrRight(as.character(temp$key),4))
  temp$variable <- SubstrRemoveRight(as.character(temp$key),5)
  temp <- temp[,c("type","variable","prettyName","year","value")]
  temp$value <- as.numeric(temp$value)
  machineu <- temp
  
  temp <- gather(data.frame(type=TYPES,m),key,value,-prettyName,-type)
  temp$year <- as.numeric(SubstrRight(as.character(temp$key),4))
  temp$variable <- SubstrRemoveRight(as.character(temp$key),5)
  temp <- temp[,c("type","variable","prettyName","year","value")]
  temp$value <- as.numeric(temp$value)
  machinem <- temp
  
  v2 <- data.frame(rbind(TYPES,
                         t(l)))
  try(write.table(v2,file=file.path(RAWmisc::PROJ$SHARED_TODAY,"All_Summary_Lower.csv"),row.names=TRUE,col.names=FALSE,sep=";"),TRUE)
  
  v2 <- data.frame(rbind(TYPES,
                         t(u)))
  try(write.table(v2,file=file.path(RAWmisc::PROJ$SHARED_TODAY,"All_Summary_Upper.csv"),row.names=TRUE,col.names=FALSE,sep=";"),TRUE)
  
  v2 <- data.frame(rbind(TYPES,
                         t(m)))
  try(write.table(v2,file=file.path(RAWmisc::PROJ$SHARED_TODAY,"All_Summary_Estimate.csv"),row.names=TRUE,col.names=FALSE,sep=";"),TRUE)
  
  try(write.table(machinel,file=file.path(RAWmisc::PROJ$SHARED_TODAY,"All_Summary_Lower_machinefriendly.csv"),row.names=FALSE,col.names=TRUE,sep=";"),TRUE)
  try(write.table(machineu,file=file.path(RAWmisc::PROJ$SHARED_TODAY,"All_Summary_Upper_machinefriendly.csv"),row.names=FALSE,col.names=TRUE,sep=";"),TRUE)
  try(write.table(machinem,file=file.path(RAWmisc::PROJ$SHARED_TODAY,"All_Summary_Estimate_machinefriendly.csv"),row.names=FALSE,col.names=TRUE,sep=";"),TRUE)
  
}

BurdenGraph <- function(){
  x <- fread(file.path(RAWmisc::PROJ$SHARED_TODAY,"All_Summary_Estimate_machinefriendly.csv"))
  x <- x[type %in% c("YLLs","YLDs","DALYs") & variable=="Total" & !prettyName %in% c("HCV related","Not related to HCV","Total")]
  x[,total:=sum(value),by=.(type,year)]
  x[,proportion:=value/total]
  x$prettyName <- factor(x$prettyName,levels=c("HCV+ acute","HCV+ chronic","Cirrhosis","HCC","Transplant"))
  x$type <- factor(x$type,levels=c("YLDs","YLLs","DALYs"))
  q <- ggplot(x,aes(x=year,y=proportion,fill=prettyName))
  q <- q + geom_rect(aes(xmin=2014,xmax=2030,ymin=-Inf,ymax=Inf),alpha=0.002,fill="yellow")
  q <- q + geom_area(alpha=0.8)
  q <- q + facet_wrap(~type,scales="free")
  q <- q + scale_fill_brewer("",palette="Set2")
  
  q <- q + scale_x_continuous("")
  q <- q + scale_y_continuous("Proportion of burden\n",lim=c(0,1))
  q <- q + theme_gray(base_size=22)
  RAWmisc::png_a4(file.path(RAWmisc::PROJ$SHARED_TODAY,"Figure_BurdenProportion.png"),landscape=TRUE,w=0.5,h=0.3)
  print(q)
  RAWmisc::MakeFootnote("Yellow area denotes projections")
  dev.off()
}

DALYsPer100k <- function(){
  population <- fread("http://data.ssb.no/api/v0/dataset/59322.csv?lang=en")
  setnames(population,c("age","sex","year","content","pop"))
  population[,year:=as.numeric(year)]
  population <- population[sex=="0 Both sexes",.(pop=sum(pop)),by=.(year)]
  
  population2 <- fread("http://data.ssb.no/api/v0/dataset/85456.csv?lang=en")
  population2 <- population2[contents=="Medium national growth (Variant MMMM)"]
  setnames(population2,c("region","sex","age","contents","year","pop"))
  population2[,year:=as.numeric(year)]
  population2 <- population2[!year %in% population$year,.(pop=sum(pop)),by=.(year)]
  population <- rbind(population,population2)
  
  x <- fread(file.path(RAWmisc::PROJ$SHARED_TODAY,"All_Summary_Estimate_machinefriendly.csv"))
  x <- x[type %in% c("YLLs","YLDs","DALYs") & variable=="Total" & !prettyName %in% c("HCV related","Not related to HCV","Total")]
  x <- x[,.(total=sum(value)),by=.(type,year)]
  
  plotData <- merge(x,population,by="year")
  plotData[,per100k:=total/pop*100000]
 
  q <- ggplot(plotData,aes(x=year,y=per100k,colour=type))
  q <- q + geom_rect(aes(xmin=2014,xmax=2030,ymin=-Inf,ymax=Inf),alpha=0.002,fill="yellow",colour=NA)
  q <- q + geom_line(lwd=3)
  q <- q + scale_colour_brewer("",palette="Set2")
  
  q <- q + scale_x_continuous("")
  q <- q + scale_y_continuous("Burden per 100,000 population\n")
  q <- q + theme_gray(base_size=22)
  RAWmisc::png_a4(file.path(RAWmisc::PROJ$SHARED_TODAY,"Figure_BurdenPer100k.png"),landscape=TRUE,w=0.5,h=0.5)
  print(q)
  RAWmisc::MakeFootnote("Yellow area denotes projections")
  dev.off()
}

DALYsPer100kForResultsAndDiscussion <- function(){
  population <- fread("http://data.ssb.no/api/v0/dataset/59322.csv?lang=en")
  setnames(population,c("age","sex","year","content","pop"))
  population[,year:=as.numeric(year)]
  population <- population[sex=="0 Both sexes",.(pop=sum(pop)),by=.(year)]
  
  population2 <- fread("http://data.ssb.no/api/v0/dataset/85456.csv?lang=en")
  population2 <- population2[contents=="Medium national growth (Variant MMMM)"]
  setnames(population2,c("region","sex","age","contents","year","pop"))
  population2[,year:=as.numeric(year)]
  population2 <- population2[!year %in% population$year,.(pop=sum(pop)),by=.(year)]
  population <- rbind(population,population2)
  
  x <- fread(file.path(RAWmisc::PROJ$SHARED_TODAY,"All_Summary_Estimate_machinefriendly.csv"))
  x <- x[type %in% c("DALYs") & variable=="Total"]
  
  plotData <- merge(x,population,by="year")
  plotData[,per100k:=gsub(" ","",format(round(value/pop*100000,2),nsmall=2))]
  
  DALYs2010HCV <- plotData[year==2010 & prettyName=="HCV related"]$per100k
  DALYs2013HCV <- plotData[year==2013 & prettyName=="HCV related"]$per100k
  DALYs2015HCV <- plotData[year==2015 & prettyName=="HCV related"]$per100k
  DALYs2004HCV <- plotData[year==2004 & prettyName=="HCV related"]$per100k
  
  DALYs2010 <- list()
  DALYs2010[["HCV related"]] <- plotData[year==2010 & prettyName=="HCV related"]$per100k
  DALYs2010[["HCV+ acute"]] <- plotData[year==2010 & prettyName=="HCV+ acute"]$per100k
  DALYs2010[["Cirrhosis"]] <- plotData[year==2010 & prettyName=="Cirrhosis"]$per100k
  DALYs2010[["HCC"]] <- plotData[year==2010 & prettyName=="HCC"]$per100k
  
  saveRDS(DALYs2015HCV,file=file.path(RAWmisc::PROJ$BAKED,"DALYs2015HCV.RDS"))
  saveRDS(DALYs2013HCV,file=file.path(RAWmisc::PROJ$BAKED,"DALYs2013HCV.RDS"))
  saveRDS(DALYs2004HCV,file=file.path(RAWmisc::PROJ$BAKED,"DALYs2004HCV.RDS"))
  saveRDS(DALYs2010,file=file.path(RAWmisc::PROJ$BAKED,"DALYs2010.RDS"))
  
  maxDALYS <- plotData[prettyName=="HCV related"]
  z <- max(as.numeric(maxDALYS$per100k))
  maxDALYS <- maxDALYS[as.numeric(per100k)==z]
  saveRDS(maxDALYS,file=file.path(RAWmisc::PROJ$BAKED,"maxDALYS.RDS"))
}

GraphNeedleAndGINI <- function(){
  needles <- data.frame(coverage=needleCoverage,gini=injectingGINI,year=1:length(needleCoverage))
  needles <- needles[needles$year>=1973 & needles$year<=2030,]
  names(needles) <- c("NSP coverage","GINI(county drug deaths)","year")
  needles <- reshape2::melt(needles,id="year")
  
  q <- ggplot(needles,aes(x=year,y=value,colour=variable))
  q <- q + geom_rect(aes(xmin=2014,xmax=2030,ymin=-Inf,ymax=Inf),alpha=0.002,fill="yellow",colour=NA)
  q <- q + geom_line(lwd=3)
  q <- q + scale_colour_brewer("",palette="Set2")
  q <- q + scale_x_continuous("")
  q <- q + scale_y_continuous("")
  q <- q + theme_gray(base_size=22)
  RAWmisc::png_a4(file.path(RAWmisc::PROJ$SHARED_TODAY,"Figure_NeedleAndGINI.png"),landscape=TRUE,w=0.5,h=0.5)
  print(q)
  RAWmisc::MakeFootnote("Yellow area denotes projections")
  dev.off()
}

SensitivityOutcomes <- function(est){
  res <- TableSummaryInt(est, 2015)
  TYPES <- c(rep("OVERLAPPING PREVALENCE",4),
             rep("DISCRETE PREVALENCE",4),
             rep("INCIDENCE",5),
             rep("YEARLY MORTALITY",5),
             rep("CUMULATIVE MORTALITY",2),
             rep("YLLs",5),
             rep("YLDs",6),
             rep("DALYs",6))
  res <- cbind(TYPES,res)
  res <- res[TYPES %in% c("OVERLAPPING PREVALENCE","DISCRETE PREVALENCE","INCIDENCE","YEARLY MORTALITY")]
  res <- melt.data.table(res,id=c("TYPES","prettyName"))
  res[,variable:=stringr::str_replace_all(variable,"_2015","")]
  res <- res[variable %in% c("Total","PWID")]
  return(res)
}

theme_SMAO_V3 <- function(base_size=24, base_family=""){
  half_line <- base_size / 2
  theme_gray(base_size) +
    theme(
      # Elements in this first block aren't used directly, but are inherited
      # by others
      legend.key.size =    unit(0.11*base_size, "lines"),
      panel.grid.major =   element_line(colour = "white", size = base_size*0.045),
      panel.grid.minor =   element_line(colour = "white", size = half_line*0.045)
    )
  
}


  