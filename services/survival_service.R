############################### Analyses on effects on nest survival rates ################################################
library(randomForest)
library(RMark)
library(tidyverse)
rm(list=ls(all=TRUE))
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
SurvivalService <- function(df,output){
  sink(paste(output,"res.txt",sep = "/"))


  final2 <- df
  final2$Fate <- as.factor(final2$Fate)
  rfData <- final2[,c(10,22:53)]


  rfVars<-colnames(rfData[,c(3:33)])
  fmla <- as.formula(paste(" ~ ", paste(rfVars, collapse= "+")))
  fmla


  #the program MARK needs to be installed separately

  rftest.getImportance <- function(data){
    rftest <-  randomForest(Fate~., data=data, importance=TRUE,
                            proximity=TRUE)
    rfImportance <- data.frame(row.names = rownames(importance(rftest)))
    rfImportance$MeanDecreaseGini <- importance(rftest)[,c=("MeanDecreaseGini")]
    for(index in 1:200){
      index
      rftest <-  randomForest(Fate~., data=data, importance=TRUE,
                              proximity=TRUE)
      rfImportance$MeanDecreaseGini <- importance(rftest)[,c=("MeanDecreaseGini")]+rfImportance$MeanDecreaseGini
    }
    meanImportance <-rfImportance/201
    meanImportance <- meanImportance[order(-meanImportance$MeanDecreaseGini),,drop=FALSE]
    return(meanImportance)
  }

  rftest.getImportance(rfData)

  meanImportance<-rftest.getImportance(rfData)

  importantVariables<- row.names(meanImportance[meanImportance$MeanDecreaseGini>=0.5,,drop=FALSE])

  res <- lapply(0:length(importantVariables), function(x) combn(length(importantVariables),x))
  formulas <- list()
  for (subsets in res){
    for(subset in as.list(as.data.frame(subsets))){
      if(length(subset)>0){
        formulas <- append(formulas,as.formula(paste(" ~ ", paste(importantVariables[subset], collapse= "+"))))
      }}
  }
  for (param in importantVariables){
    param
  }
  print(formulas)

  markrun.nestall=function()
  {
    res <- list()
    tables <- data.frame()
    for(formula in formulas){
      mark <- mark(final2,nocc=162,model = "Nest", model.parameters=list(S=list(formula=formula)),output = FALSE)
      table <- collect.models()
      tables[nrow(tables)+1,colnames(table$model.table)] <- table$model.table
      res <- cbind(res,list(mark,table))
    }
    return (list(res,tables))
  }

  nest.full_results=markrun.nestall()
  nest.table <- nest.full_results[[2]][order(nest.full_results[[2]]$AICc),,drop=FALSE]
  nest.mark_results <- nest.full_results[[1]][,order(nest.full_results[[2]]$AICc),drop=FALSE]
  print(nest.table)

  print(nest.mark_results)
  ####plot Figure 2 - Daily nest survival in relation to Rain and Temperature

  #rain
  #watch out as par.index changes when fed => unfed
  sink()
  #
  #dev.off()
  #drawParamPlot <- function(param,netl_par_index=1,sequence_,name_){
  #  df <- data.frame(sequence_)
  #  names(df) <- param
  #  Phi.rain.predictions <- covariate.predictions(result,data=df,indices=c(netl_par_index))$estimate #use nest.results[[8]]$design.data$S to see what the par indices are
  #  with(Phi.rain.predictions,{
  #    my_plot <- plot(x=sequence_, y=seq(0.9,1, length=100),xlab=name_, ylab="DSR", las=1, cex.lab=1.5, cex.axis=1.5, type="n",  bty="L")
  #    polygon(c(covdata[par.index==netl_par_index], rev(covdata[par.index==netl_par_index])), c(lcl[par.index==netl_par_index], rev(ucl[par.index==netl_par_index])), border=NA, col=adjustcolor("grey", alpha=0.5))
  #    lines(covdata[par.index==netl_par_index],estimate[par.index==netl_par_index], lwd=2, lty="dashed")
  #    par(new)
  #    axis(1, at=c(6,10,14,18,22), labels=c(6,10,14,18,22), cex.axis=1.5)
  #  })
  #  mtext(text="(a)",side=3, line=-0.5, at=-12, cex=1.5)
  #  mtext(text="(b)",side=3, line=-0.5, at=6.3, cex=1.5)
  #}
  #drawParamPlot(param="EVI082022",sequence_ = seq(0.09,0.18,length=100),name_="EVI08")
  #drawParamPlot(param="dist.Achr",sequence_ = seq(0,30,length=100),name_="dist.Achr")
  #survival <- result$results$real$estimate^120
  #survival
  #####different DSR
  #y <- nest.results[[1]]$design.data$S
  #
  #g <- group_by(final2, stage)%>%summarise(meant = mean(temp), meanr = mean(mean_rain))
  ##10.9, 15.5
  #
  ##nest survival for unfed phase 1 and 2 of env model, predicted for different rain scenarios
  #
  ##get estimates from model
  #DSR1=covariate.predictions(nest.results[[1]],data=data.frame(mean_rain=0, temp=10.9),indices=c(121))$estimate #here agian par indices
  #DSR2=covariate.predictions(nest.results[[1]],data=data.frame(mean_rain=0, temp=15.5),indices=c(361))$estimate
  #DSR3=covariate.predictions(nest.results[[1]],data=data.frame(mean_rain=1.5, temp=10.9),indices=c(121))$estimate
  #DSR4=covariate.predictions(nest.results[[1]],data=data.frame(mean_rain=1.5, temp=15.5),indices=c(361))$estimate
  #DSR5=covariate.predictions(nest.results[[1]],data=data.frame(mean_rain=2, temp=10.9),indices=c(121))$estimate
  #DSR6=covariate.predictions(nest.results[[1]],data=data.frame(mean_rain=2, temp=15.5),indices=c(361))$estimate
  #
  #DSR_rain <- bind_rows(DSR1, DSR2, DSR3, DSR4, DSR5, DSR6)
  #
  #DSR_rain <- dplyr::select(DSR_rain, -vcv.index, -model.index, -fixed)
  #
  ##calculate survival
  #DSR_rain <- mutate(DSR_rain, survival=ifelse(par.index==121, estimate^35, estimate^37))
  #
  ##calculate standard deviation
  #DSR_rain <- mutate(DSR_rain, se_survival=ifelse(par.index==121, (35*estimate^34)*se, (37*estimate^36)*se))
  #DSR_rain
  #
  ##calculate confidence interval
  #DSR_rain <- mutate(DSR_rain, lcl_survival=survival-1.96*se_survival)
  #DSR_rain <- mutate(DSR_rain, ucl_survival=survival+1.96*se_survival)
  #DSR_rain <- dplyr::select(DSR_rain, par.index, mean_rain, survival, lcl_survival, ucl_survival)
  #
  #####year####
  #
  ##best year model
  #year.results[2]
  #
  #####different DSR
  #
  ##data set for DSR calculation
  #y <- year.results$Full1$results$real #estimates from model
  #y$phase <- (c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2)) #add factor names manually
  #y$treatment <- c("fed", "undfed")
  #y$year <- c(2015, 2015, 2016, 2016, 2017, 2017, 2018, 2018)
  #
  #y <- mutate(y, survival=ifelse(phase==1, estimate^35, estimate^37)) #calculate survival for phase 1 -> 35 days long, phase 2 -> 37 days long
  #y <- mutate(y, se_survival=ifelse(phase==1, (35*estimate^34)*se, (37*estimate^36)*se)) #calculate se for phase with delta formula
  #y <- mutate(y, lcl_survival=survival-1.96*se_survival) #calculate confidence intervals
  #y <- mutate(y, ucl_survival=survival+1.96*se_survival) #calculate confidence intervals
  #y <- dplyr::select(y, phase, treatment, year, survival, lcl_survival, ucl_survival)
  #
  #year <- y[c(2,10,1,9,4,12,3,11,6,14,5,13,8,16,7,15),] #change order
  #
  #####season
  #rr=get.real(year.results[[2]],"S",se=TRUE,vcv=TRUE)#get covariance matrix from model
  #covvar <- rr$vcv.real
  ###survival
  ##2015 fed
  #x <- year.results$Full1$results$real[1,1] #DSR stage1 from model
  #y <- year.results$Full1$results$real[9,1] #DSR stage2 from model
  #px <- (35*(x^34)*(y^37)) #to insert in delta formula (stage1)
  #py <- (37*(x^35)*(y^36)) #to insert in delta formula (stage2)
  #z <- covvar[1,9] #covariance between x and y
  #varx <- covvar[1,1] #variance of x
  #vary <- covvar[9,9] #variance of y
  #
  #var <- varx*((px)^2)+vary*((py)^2)+2*(z*px*py) #delta formula for multiplying phase 1 with phase 2
  #se <- var^(1/2)
  #mean15fed <- year.results$Full1$results$real$estimat[9]^37*year.results$Full1$results$real$estimat[1]^35 #nest survival estimate
  #lcl15fed <- mean15fed-1.96*se #confidence interval
  #ucl15fed <- mean15fed+1.96*se #confidence interval
  #
  ##2015 unfed
  #x <- year.results$Full1$results$real[2,1]
  #y <- year.results$Full1$results$real[10,1]
  #px <- (35*(x^34)*(y^37))
  #py <- (37*(x^35)*(y^36))
  #z <- covvar[2,10]
  #varx <- covvar[2,2]
  #vary <- covvar[10,10]
  #
  #var <- varx*((px)^2)+vary*((py)^2)+2*(z*px*py)
  #se <- var^(1/2)
  #mean15unfed <- year.results$Full1$results$real$estimat[10]^37*year.results$Full1$results$real$estimat[2]^35
  #lcl15unfed <- mean15unfed-1.96*se
  #ucl15unfed <- mean15unfed+1.96*se
  #
  ##2016 fed
  #x <- year.results$Full1$results$real[3,1]
  #y <- year.results$Full1$results$real[11,1]
  #px <- (35*(x^34)*(y^37))
  #py <- (37*(x^35)*(y^36))
  #z <- covvar[3,11]
  #varx <- covvar[3,3]
  #vary <- covvar[11,11]
  #
  #var <- varx*((px)^2)+vary*((py)^2)+2*(z*px*py)
  #se <- var^(1/2)
  #mean16fed <- year.results$Full1$results$real$estimat[11]^37*year.results$Full1$results$real$estimat[3]^35
  #lcl16fed <- mean16fed-1.96*se
  #ucl16fed <- mean16fed+1.96*se
  #
  ##2016 unfed
  #x <- year.results$Full1$results$real[4,1]
  #y <- year.results$Full1$results$real[12,1]
  #px <- (35*(x^34)*(y^37))
  #py <- (37*(x^35)*(y^36))
  #z <- covvar[4,12]
  #varx <- covvar[4,4]
  #vary <- covvar[12,12]
  #
  #var <- varx*((px)^2)+vary*((py)^2)+2*(z*px*py)
  #se <- var^(1/2)
  #mean16unfed <- year.results$Full1$results$real$estimat[12]^37*year.results$Full1$results$real$estimat[4]^35
  #lcl16unfed <- mean16unfed-1.96*se
  #ucl16unfed <- mean16unfed+1.96*se
  #
  ##2017 fed
  #x <- year.results$Full1$results$real[5,1]
  #y <- year.results$Full1$results$real[13,1]
  #px <- (35*(x^34)*(y^37))
  #py <- (37*(x^35)*(y^36))
  #z <- covvar[5,13]
  #varx <- covvar[5,5]
  #vary <- covvar[13,13]
  #
  #var <- varx*((px)^2)+vary*((py)^2)+2*(z*px*py)
  #se <- var^(1/2)
  #mean17fed <- year.results$Full1$results$real$estimat[13]^37*year.results$Full1$results$real$estimat[5]^35
  #lcl17fed <- mean17fed-1.96*se
  #ucl17fed <- mean17fed+1.96*se
  #
  ##2017 unfed
  #x <- year.results$Full1$results$real[6,1]
  #y <- year.results$Full1$results$real[14,1]
  #px <- (35*(x^34)*(y^37))
  #py <- (37*(x^35)*(y^36))
  #z <- covvar[6,14]
  #varx <- covvar[6,6]
  #vary <- covvar[14,14]
  #
  #var <- varx*((px)^2)+vary*((py)^2)+2*(z*px*py)
  #se <- var^(1/2)
  #mean17unfed <- year.results$Full1$results$real$estimat[14]^37*year.results$Full1$results$real$estimat[6]^35
  #lcl17unfed <- mean17unfed-1.96*se
  #ucl17unfed <- mean17unfed+1.96*se
  #
  ##2018 fed
  #x <- year.results$Full1$results$real[7,1]
  #y <- year.results$Full1$results$real[15,1]
  #px <- (35*(x^34)*(y^37))
  #py <- (37*(x^35)*(y^36))
  #z <- covvar[7,15]
  #varx <- covvar[7,7]
  #vary <- covvar[15,15]
  #
  #var <- varx*((px)^2)+vary*((py)^2)+2*(z*px*py)
  #se <- var^(1/2)
  #mean18fed <- year.results$Full1$results$real$estimat[15]^37*year.results$Full1$results$real$estimat[7]^35
  #lcl18fed <- mean18fed-1.96*se
  #ucl18fed <- mean18fed+1.96*se
  #
  ##2018 unfed
  #x <- year.results$Full1$results$real[8,1]
  #y <- year.results$Full1$results$real[16,1]
  #px <- (35*(x^34)*(y^37))
  #py <- (37*(x^35)*(y^36))
  #z <- covvar[8,16]
  #varx <- covvar[8,8]
  #vary <- covvar[16,16]
  #
  #var <- varx*((px)^2)+vary*((py)^2)+2*(z*px*py)
  #se <- var^(1/2)
  #mean18unfed <- year.results$Full1$results$real$estimat[16]^37*year.results$Full1$results$real$estimat[8]^35
  #lcl18unfed <- mean18unfed-1.96*se
  #ucl18unfed <- mean18unfed+1.96*se
  #
  #####plot
  #
  ### Figure 1 - Nest survival by year in relation to Feeding treatment and phase
  #
  #tiff(filename = "Fig1_Nestsurvival_by_PhaseYearandTreatment.tiff",width = 9, height = 5, units = "in", res=800,compression = "lzw")
  #par(oma=c(3,0,0,0), mfrow=c(2,2))
  #par(mar=c(2,5,1,0))
  #plot(x=c(1, 1.5, 2.5, 3), year$survival[year$year==2015], xlim=range(1,3.5), ylim=range(0.25,1), pch=16, cex.lab=1.5, col=c("grey","grey","aquamarine4","aquamarine4"), xaxt="n", las=1, cex.axis=1.5, ylab="", yaxt="n", bty="L")
  #segments(c(1, 1.5, 2.5, 3),year$lcl_survival[year$year==2015], c(1, 1.5, 2.5, 3), year$ucl_survival[year$year==2015], lwd=1.5, col=c("grey","grey","aquamarine4","aquamarine4"),lty=c("dashed","dotted"), xaxt="n")
  #segments(c(2,3.5),c(lcl15unfed,lcl15fed), c(2, 3.5),c(ucl15unfed,ucl15fed), lwd=1.5, col=c("grey","aquamarine4"),lty="solid", xaxt="n")
  #points(x=c(2, 3.5), y=c(mean15unfed, mean15fed), pch=16, col=c("grey","aquamarine4"))
  #axis(2, at=c(0.25, 0.5, 0.75, 1), cex.axis=1.5, labels=c(0.25, 0.5, 0.75, 1.0), las=1)
  #legend(x = 0.83, y = 0.43, bty="n",col="black", legend="Incubation",cex=1.2)
  #legend(x = 1.30, y = 0.88, bty="n",col="black", legend="Nestling",cex=1.2)
  #legend(x = 1.83, y = 0.35, bty="n",col="black", legend="Overall",cex=1.2)
  #
  #par(mar=c(2,1,1,3.8))
  #plot(c(1, 1.5, 2.5, 3), year$survival[year$year==2016], xlim=range(1,3.5), ylim=range(0.25,1), pch=16, cex.lab=1.5, col=c("grey","grey","aquamarine4","aquamarine4"), xaxt="n", las=1, cex.axis=1.5, ylab="", yaxt="n", bty="L")
  #segments(c(1, 1.5, 2.5, 3),year$lcl_survival[year$year==2016], c(1, 1.5, 2.5, 3), year$ucl_survival[year$year==2016], lwd=1.5, col=c("grey","grey","aquamarine4","aquamarine4"),lty=c("dashed","dotted"), xaxt="n")
  #segments(c(2,3.5),c(lcl16unfed,lcl16fed), c(2, 3.5),c(ucl16unfed,ucl16fed), lwd=1.5, col=c("grey","aquamarine4"),lty="solid", xaxt="n")
  #points(x=c(2, 3.5), y=c(mean16unfed, mean16fed), pch=16, col=c("grey","aquamarine4"))
  #
  #par(mar=c(3,5,0,0))
  #plot(c(1, 1.5, 2.5, 3), year$survival[year$year==2017], xlim=range(1,3.5), ylim=range(0.25,1), xlab="", pch=16, cex.lab=1.5, col=c("grey","grey","aquamarine4","aquamarine4"), xaxt="n", las=1, cex.axis=1.5, ylab="", yaxt="n", bty="L")
  #segments(c(1, 1.5, 2.5, 3),year$lcl_survival[year$year==2017], c(1, 1.5, 2.5, 3), year$ucl_survival[year$year==2017], lwd=1.5, col=c("grey","grey","aquamarine4","aquamarine4"),lty=c("dashed","dotted"), xaxt="n")
  #segments(c(2, 3.5),c(lcl17unfed,lcl17fed), c(2, 3.5),c(ucl17unfed,ucl17fed), lwd=1.5, col=c("grey","aquamarine4"),lty="solid", xaxt="n")
  #points(x=c(2, 3.5), y=c(mean17unfed, mean17fed), pch=16, col=c("grey","aquamarine4"))
  #axis(2, at=c(0.25, 0.5, 0.75, 1), cex.axis=1.5, labels=c(0.25, 0.5, 0.75, 1.0), las=1)
  #axis(1, at=c(1.5, 3), labels=c("Control", "Supplemented"), cex.axis=1.5)
  #mtext(text="Nest survival",side=2, line=3.5, at=1.15, cex=1.5)
  #
  #par(mar=c(3,1,0,3.8))
  #plot(c(1, 1.5, 2.5, 3), year$survival[year$year==2018], xlim=range(1,3.5), ylim=range(0.25,1), pch=16, xlab="", cex.lab=1.5, col=c("grey","grey","aquamarine4","aquamarine4"), xaxt="n", las=1, cex.axis=1.5, ylab="", yaxt="n", bty="L")
  #segments(c(1, 1.5, 2.5, 3),year$lcl_survival[year$year==2018], c(1, 1.5, 2.5, 3), year$ucl_survival[year$year==2018], lwd=1.5, col=c("grey","grey","aquamarine4","aquamarine4"),lty=c("dashed","dotted"), xaxt="n")
  #segments(c(2,3.5),c(lcl18unfed,lcl18fed), c(2, 3.5),c(ucl18unfed,ucl18fed), lwd=1.5, col=c("grey","aquamarine4"),lty="solid", xaxt="n")
  #points(x=c(2, 3.5), y=c(mean18unfed, mean18fed), pch=16, col=c("grey","aquamarine4"))
  #axis(1, at=c(1.5, 3), labels=c("Control", "Supplemented"), cex.axis=1.5, cex.lab=1.5)
  #mtext(text="2017",side=3, line=-0.8, at=-1.65, cex=1.2)
  #mtext(text="2018",side=3, line=-0.8, at=1.15, cex=1.2)
  #mtext(text="2015",side=3, line=11.3, at=-1.65, cex=1.2)
  #mtext(text="2016",side=3, line=11.3, at=1.15, cex=1.2)
  #mtext(text="Food supplementation", side=1,line=0,outer=TRUE, cex=1.5)
  #dev.off()
  #
  #
  #####average values for text in publication
  #
  ##from year model
  #mean_fed <- (mean18fed+mean17fed+mean16fed+mean15fed)/4 #mean per treatment and year
  #mean_unfed <- (mean18unfed+mean17unfed+mean16unfed+mean15unfed)/4
  #
  #year <- group_by(year, phase, treatment)%>%mutate(mean=mean(survival))#mean per phase and treatment
  #
  #0.733-0.514 #survival phase 1 unfed 2016 - phase 1 fed 2016 (from year data set)
  #
  #mean16fed-mean16unfed #difference of unfed vs fed in 2016
  #
  #number <- group_by(final2, year, stage, fed)%>%count()
  #
  #se_unfed1 <- ((10-1)*(0.13454461^2)+(36-1)*(0.08382417^2)+(81-1)*(0.04806924^2)+(92-1)*(0.04073495^2))/(10+36+81+92-4) #variance for mean survival unfed phase 1 (values from y data set from coding line 490), sample sizes for each group from number data set line above
  #
  #se_unfed_1 <- se_unfed1^(1/2)
  #
  #se_unfed2 <- ((13-1)*(0.04697636^2)+(45-1)*(0.04130556^2)+(69-1)*(0.01996744^2)+(86-1)*(0.01545519^2))/(13+45+69+86-4) #variance for mean survival unfed phase 2 (values from y data set from coding line 490)
  #
  #se_unfed_2 <- se_unfed2^(1/2)
  #
  ###values from best environmental model
  #p <- nest.results$Final2$results$real
  #p$phase <- (c(1,1,2,2))
  #p$treatment <- c("fed", "undfed")
  #
  #p <- mutate(p, survival=ifelse(phase==1, estimate^35, estimate^37)) #nest survival fed incubation phase, nest survival unfed incubation phase
  #p <- mutate(p, se_survival=ifelse(phase==1, (35*estimate^34)*se, (37*estimate^36)*se)) #calculate se for phase with delta formula
  #
  #

}
