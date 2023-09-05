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

  sorted <- nest.full_results[[1]][,order(nest.full_results[[2]]$AICc),drop=FALSE]


  result <- nest.mark_results[1,][[1]]
  print(result)



  survival <- result$results$real$estimate^120
  print(survival)

  importantVariablesForPlots <- c()
  for(index in 1:min(length(sorted[1,]),3)){
    importantVariablesForPlots <- unique(append(importantVariablesForPlots,sorted[1,][[index]]$covariates))
  }

  importantVariablesForPlots <- intersect(importantVariables,importantVariablesForPlots)
  drawParamPlot <- function(param,netl_par_index=1,sequence_,name_,result){
    df <- data.frame(sequence_)
    names(df) <- param
    predictions <- covariate.predictions(result,data=df,indices=c(netl_par_index))$estimate #use nest.results[[8]]$design.data$S to see what the par indices are
    with(predictions,{
      my_plot <- plot(x=sequence_, y=seq(0.9,1, length=100),xlab=name_, ylab="DSR", las=1, cex.lab=1.5, cex.axis=1.5, type="n",  bty="L")
      polygon(c(covdata[par.index==netl_par_index], rev(covdata[par.index==netl_par_index])), c(lcl[par.index==netl_par_index], rev(ucl[par.index==netl_par_index])), border=NA, col=adjustcolor("grey", alpha=0.5))
      lines(covdata[par.index==netl_par_index],estimate[par.index==netl_par_index], lwd=2, lty="dashed")
      par(new)
      axis(1, at=c(6,10,14,18,22), labels=c(6,10,14,18,22), cex.axis=1.5)
    })
    mtext(text="(a)",side=3, line=-0.5, at=-12, cex=1.5)
    mtext(text="(b)",side=3, line=-0.5, at=6.3, cex=1.5)
  }
  for(param in importantVariablesForPlots){
    maxParamVal <- max(rfData[param])
    minParamVal <- min(rfData[param])
    jpeg(paste(output,paste(param,"jpg",sep="."),sep = "/"))
    drawParamPlot(param=param,sequence_ = seq(minParamVal,maxParamVal,length=100),name_=param,result=result)
    dev.off()
  }
  #rain
  #watch out as par.index changes when fed => unfed
  sink()
  #



}
