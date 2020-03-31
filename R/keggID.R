#' Obtains the KEGG ID corresponding to a HMDB coded metabolite
#'
#' The function looks at the HMDB entry and retrieves the KEGG ID codification specific for the metabolite of interest, if available.
#'
#' @param x is the HMDB coded metabolite of which we want the KEGG ID.
#' @return returns the KEGG IDs of the metabolite of interest.
#' @import XML
#' @import httr
#' @export
hkeggid<-function(x){
  temp<-xmlToList(xmlParse(GET(paste0("http://www.hmdb.ca/metabolites/", x,  ".xml"))))
  keggid<-temp$kegg_id
  return(keggid)
}
