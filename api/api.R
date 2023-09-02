library(readr)
source("../services/survival_service.R");

source("../services/distnace_services.R");

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
#* @param output:[str]
#* @post /survival
function(req,output){
 body <- readr::read_csv(req$body$data$value)
 output <- as.character(output)

 SurvivalService(body,output)
}
