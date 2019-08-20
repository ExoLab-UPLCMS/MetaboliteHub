#' Retrieve pathways previously described in KEGG database
#'
#' Using the metabolite KEGG ID, it looks for the pathways already published in KEGG and returns them.
#' @param x is the metabolite we want information from
#' @return returns the pathways in which the metabolite is involved
#' @export
kpaths<-function(x){
  temp<-readLines(paste0('http://rest.kegg.jp/link/pathway/', x))
  temp_paths<-sapply(temp, function(x){
    strsplit(x, 'path:')[[1]][2]
  })
  paths<-sapply(temp_paths, function(y){readLines(paste0('http://rest.kegg.jp/find/pathway/', y))})
  paths<-sapply(paths, function(z){strsplit(z, '\t')[[1]][2]})
  return(unname(paths))
}
