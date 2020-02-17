#' Retrieves accession numbers of nucleotide transcripts from NCBI
#'
#' Using a gene name, it retrieves the transcripts available on NCBI database. The function was developed to look for specifically Homo sapiens sequences.
#'
#' @param x is the gene of interest
#' @return returns a list of nucleotide accession numbers.
#' @import rentrez
#' @export
search_nucl<-function(x){
  ids<-entrez_search("nucleotide", term=paste0(x, " AND Homo sapiens[porgn]"))$ids
  accession<-sapply(ids, function(x){
    entrez_summary("nucleotide", x)$accessionversion
  })
  return(unname(accession))
}
