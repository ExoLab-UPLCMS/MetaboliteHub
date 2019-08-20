#' Retrieve enzymes related to specific metabolite
#'
#' @param x is the metabolite we want to obtain information from
#' @return returns EC code of enzymes related to the metabolite present in KEGG database
#' @export
kenz<-function(x){
  KEGG_LINK_BASE <- "http://rest.kegg.jp/link/enzyme/"
  link_REST_url <- paste(KEGG_LINK_BASE, x, sep="")
  link <- readLines(link_REST_url)
  enzs<-sapply(link, function(x){
    strsplit(x, "\t")[[1]][2]
  })
  enzs<-unname(enzs)
  enzs<-sapply(enzs, function(x){
    strsplit(x, "ec:")[[1]][2]
  })
  enzs<-unname(enzs)
  enzs<-enzs[!is.na(enzs)]
}
