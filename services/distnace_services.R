library("Distance")
data("minke")
head(minke)
minke
DistanceService <-  function (df) {
  minke_hn <- ds(df, truncation = 1.5)
  summary(minke_hn)
}
