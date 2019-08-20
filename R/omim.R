#' Retrieves diseases and metabolic pathways alttered present in OMIM database.
#'
#' For a gene, the functions searches on OMIM database and retrieves what is publicated there, the diseases and metabolic alterations reported.
#'
#' @param x is the gene of interest.
#' @return returns a list of the different alterations described for the gene.
#' @import rentrez
#' @export
omim<-function(x){
  omim_id<-entrez_search("omim", x)$ids
  if (length(omim_id)==0){
    summary_omim<-c('No entries found for this gene')
  }
  else {
    summary_omim<-sapply(omim_id, FUN=function(x){
      entrez_summary("omim", x)$title
    })
  }
  return(unname(summary_omim))
}

