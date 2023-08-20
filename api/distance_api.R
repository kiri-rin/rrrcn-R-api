library(readr)
distance_services <- source("../services/distnace_services.R")
#* @serializer unboxedJSON
#* @parser multi
#* @parser octet
#* @param density_function:[str]
#* @param output:[str]
#* @post /distance
function(req,density_function="hn",output){
 body <- readr::read_csv(req$body$data$value)
 body$object <- body$ID
 totalArea <- as.character(density_function)
 output <- as.character(output)

 DistanceService(body,totalArea,output)
}

#* @serializer unboxedJSON
#* @parser multi
#* @parser octet
#* @post /survival
function(req,density_function="hn",output){
 body <- readr::read_csv(req$body$data$value)
 body$object <- body$ID
 totalArea <- as.character(density_function)
 output <- as.character(output)

 DistanceService(body,totalArea,output)
}

