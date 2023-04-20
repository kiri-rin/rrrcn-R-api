library("Distance")
data("minke")
head(minke)
minke
DistanceService <-  function (df,density_function="hn",output) {

  sink(paste(output,"res.txt",sep = "/"))
  minke_hn <- ds(df, key=density_function)
  jpeg(paste(output,"QQPlot.jpg",sep = "/"))
  kgs <- ddf.gof(minke_hn,ks = TRUE,chisq=TRUE,nboot = ifelse(density_function=="hn",50,0))
  dev.off()
  jpeg(paste(output,"Distance.jpg",sep = "/"))
  plot(minke_hn)
  dev.off()
  print(AIC(minke_hn))
  print(summary(minke_hn))
  print(kgs)
  sink()

  list(AIC=AIC(minke_hn),minke_hn$dht$individuals)

}
