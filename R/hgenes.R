#' Retrieves genes from HMDB
#'
#' The function looks at HMDB entry and retrieves the genes related to specific metabolite.
#' #'
#' @param x is the metabolite of interest
#' @return returns a list of the gene names related to a specific metabolite.
#' @import XML
#' @import httr
#' @export
hgenes<-function(x){
  temp<-xmlToList(xmlParse(GET(paste0("http://www.hmdb.ca/metabolites/", x,  ".xml"))))
  Genes<-vector('list', length = length(temp$protein_associations))
  for (i in 1:length(temp$protein_associations)){
  Genes[i]<-temp$protein_associations[i]$protein$gene_name
  }
  return(unlist(Genes))
}

