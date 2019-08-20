#' Translates OWL code to KEGG and HMDB ones
#'
#' The function looks at Owl file, which includes OWL code, metabolite name and family to obtain both the corresponding KEGG and HMDB IDs.
#' #'
#' @param x is the Owl-coded metabolite of interest
#' @return returns both KEGG and HMDB IDs.
#' @export
owl_code<-function(x){
  data.frame(
    Name=owl[grep(paste0('^', x, '$'), owl$OWL.Code), 3],
    Alternative=owl[grep(paste0('^', x, '$'), owl$OWL.Code), 4],
    HMDB=owl[grep(paste0('^', x, '$'), owl$OWL.Code), 6],
    KEGG=owl[grep(paste0('^', x, '$'), owl$OWL.Code), 7]
  , stringsAsFactors = F)
}
