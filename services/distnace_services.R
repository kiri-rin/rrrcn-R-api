library("Distance")
data("minke")
head(minke)
minke
DistanceService <-  function (df,totalArea) {
  minke_hn <- ds(df, truncation = 1.5, key="hr")
  res <-  dht2(minke_hn, flatfile=df, stratification="effort_sum",total_area=totalArea,
                     strat_formula=~Region.Label)
  list(AIC=AIC(minke_hn),data=res)
}
