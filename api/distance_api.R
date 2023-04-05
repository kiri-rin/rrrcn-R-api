distance_services <- source("../services/distnace_services.R")
#* @serializer  json
#* @parser multi
#* @parser octet
#* @param totalArea:[dbl]
#* @post /distance
function(req,totalArea){
  body <- readr::read_csv(req$body$data$value)
  print(req$body)
  print(totalArea)
  totalArea <- as.numeric(totalArea)
  body$distance <-body$Dist 
 DistanceService(body,totalArea)
}

