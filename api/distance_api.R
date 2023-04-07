library(readr)
distance_services <- source("../services/distnace_services.R")
#* @serializer unboxedJSON
#* @parser multi
#* @parser octet
#* @param totalArea:[dbl]
#* @post /distance
function(req,totalArea){
 body <- readr::read_csv(req$body$data$value)
 body$object <- body$ID
 totalArea <- as.numeric(totalArea)
 DistanceService(body,totalArea)
}

