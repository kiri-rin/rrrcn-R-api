library(readr)
survival_services <- source("../servicsurvival_service.R")

#* @serializer unboxedJSON
#* @parser multi
#* @parser octet
#* @param output:[str]
#* @post /survival
function(req,output){
 body <- readr::read_csv(req$body$data$value)
 body$object <- body$ID
 output <- as.character(output)

 SirvivalService(body,output)
}

