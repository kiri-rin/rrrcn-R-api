distance_services <- source("../services/distnace_services.R")
#* @serializer  print
#* @parser multi
#* @parser csv
#* @post /distance
function(req){
  body <- req$body$data$parsed
  body$distance <-body$Dist 
 DistanceService(body)
}

