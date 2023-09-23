############################### Analyses on effects on nest survival rates ################################################
library(randomForest)
library(RMark)
library(tidyverse)
rm(list = ls(all = TRUE))
getwd()

SurvivalService <- function(df, nocc, output) {
  options(width=10000)

  final2 <- df
  final2$Fate <- as.factor(final2$Fate)
  rfData <- subset(final2, select = -c(FirstFound, LastChecked, LastPresent))

  rftest.getImportance <- function(data) {
    rftest <- randomForest(Fate ~ ., data = data, importance = TRUE,
                           proximity = TRUE)
    rfImportance <- data.frame(row.names = rownames(importance(rftest)))
    rfImportance$MeanDecreaseGini <- importance(rftest)[, c = ("MeanDecreaseGini")]
    for (index in 1:200) {
      index
      rftest <- randomForest(Fate ~ ., data = data, importance = TRUE,
                             proximity = TRUE)
      rfImportance$MeanDecreaseGini <- importance(rftest)[, c = ("MeanDecreaseGini")] + rfImportance$MeanDecreaseGini
    }
    meanImportance <- rfImportance / 201
    meanImportance <- meanImportance[order(-meanImportance$MeanDecreaseGini), , drop = FALSE]
    return(meanImportance)
  }

  rftest.getImportance(rfData)

  meanImportance <- rftest.getImportance(rfData)
  importantVariables <- row.names(meanImportance[meanImportance$MeanDecreaseGini >= 0.5, , drop = FALSE])

  res <- lapply(0:length(importantVariables), function(x) combn(length(importantVariables), x))
  formulas <- list()
  for (subsets in res) {
    for (subset in as.list(as.data.frame(subsets))) {
      if (length(subset) > 0) {
        formulas <- append(formulas, as.formula(paste(" ~ ", paste(importantVariables[subset], collapse = "+"))))
      } }
  }

  markrun.nestall <- function()
    {
    res <- list()
    tables <- data.frame()
    for (formula in formulas) {
      mark <- mark(final2, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = formula)), prefix = paste(output, "", sep = "/"))
      table <- collect.models()
      tables[nrow(tables) + 1, colnames(table$model.table)] <- table$model.table
      res <- cbind(res, list(mark, table))
   }
    return(list(res, tables))
  }

  nest.full_results <- markrun.nestall()

  nest.table <- nest.full_results[[2]][order(nest.full_results[[2]]$AICc), , drop = FALSE]


  sorted <- nest.full_results[[1]][, order(nest.full_results[[2]]$AICc), drop = FALSE]


  drawParamPlot <- function(param, netl_par_index = 1, sequence_, name_, result) {
    df <- data.frame(sequence_)
    names(df) <- param
    predictions <- covariate.predictions(result, data = df, indices = c(netl_par_index))$estimate #use nest.results[[8]]$design.data$S to see what the par indices are
    with(predictions, {
      plot(x = sequence_, y = seq(0.9, 1, length = 100), xlab = name_, ylab = "DSR", las = 1, cex.lab = 1.5, cex.axis = 1.5, type = "n", bty = "L")
      polygon(c(covdata[par.index == netl_par_index], rev(covdata[par.index == netl_par_index])), c(lcl[par.index == netl_par_index], rev(ucl[par.index == netl_par_index])), border = NA, col = adjustcolor("grey", alpha = 0.5))
      lines(covdata[par.index == netl_par_index], estimate[par.index == netl_par_index], lwd = 2, lty = "dashed")
      par(new)
      axis(1, at = c(6, 10, 14, 18, 22), labels = c(6, 10, 14, 18, 22), cex.axis = 1.5)
    })
    mtext(text = "(a)", side = 3, line = -0.5, at = -12, cex = 1.5)
    mtext(text = "(b)", side = 3, line = -0.5, at = 6.3, cex = 1.5)
  }

  for (index in 1:min(length(sorted[1,]), 3)) {
    covariates <- sorted[1,][[index]]$covariates
    for (param in covariates) {
      maxParamVal <- max(rfData[param])
      minParamVal <- min(rfData[param])
      jpeg(paste(output, paste(paste(covariates, collapse = "+"), paste(param, "jpg", sep = "."), sep = "_"), sep = "/"))
      drawParamPlot(param = param, sequence_ = seq(minParamVal, maxParamVal, length = 100), name_ = param, result = sorted[1,][[index]])
      dev.off()
    }
  }

  sink(paste(output, "res.txt", sep = "/"))
  print("Mean variable importance according to Random forest:")
  print(meanImportance)
  print("Tested models:")
  print(nest.table)
  print("Best model result:")
  print(sorted[1,][[1]]$results)
  sink()
}
