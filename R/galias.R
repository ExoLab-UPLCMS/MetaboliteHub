#' Retrieves the potential gene aliases from NCBI Gene Database
#'
#' The function looks at NCBI Gene Database entry for the specified gene and returns the list of aliases (if available).
#' The function is designed to retrieve data only for Homo sapies specie.
#'
#' @param x is the gene of interest
#' @return returns a list containing the aliases of each one of the genes passed to it.
#' @import rentrez
#' @export
galias<-function(x){
  p1<-entrez_search(db='gene', term=paste0(x, '[Gene Name] AND Homo sapiens[Organism]'))$ids
  if (length(p1)==0)
  {}
  else if (length(p1)==1){
    p2<-entrez_summary(db='gene', id=p1)$otheraliases
  }
  else {
    p2<-sapply(p1, function(z){
      entrez_summary(db='gene', id=z)$otheraliases
    })
  }
  if (isTRUE(p2=="")==TRUE)
  {}
  else {
    return(unlist(strsplit(as.character(p2), ', ')))
  }
}
