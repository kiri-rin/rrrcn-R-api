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
 density <- as.character(density_function)
 output <- as.character(output)

 DistanceService(body,density,output)
}

#* @serializer unboxedJSON
#* @parser multi
#* @parser octet
#* @param nocc:[str]
#* @param output:[str]
#* @post /survival
function(req,nocc,output){
 body <- readr::read_csv(req$body$data$value)
 output <- as.character(output)
 numberOfOccasions <- as.numeric(as.character(nocc))

 SurvivalService(body,numberOfOccasions,output)
}
