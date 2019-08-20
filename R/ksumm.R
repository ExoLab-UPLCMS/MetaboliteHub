#' Retrieve all available information of interest from KEGG entry
#'
#' Starting from KEGG ID, it retrieves the Enzymes, Genes and Pathways related to the specified metabolite
#'
#'
#' @param x is the metabolite codified in KEGG ID
#' @return returns a data.frame of different data entries: Enzymes, Genes and Pathways
#' @import rentrez
#' @export
ksumm<-function(x){
  df1<-list(
    list(
      Enzymes=kenz(x),
      Genes=kgenes(x),
      Paths=kpaths(x)))
  return(df1)
}

