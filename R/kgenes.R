#' Retrieve genes related to a specific metabolite
#'
#' Starting from a metabolite KEGG ID, it searches for the related enzymes and, then, the genes that regulate these enzymes.
#' @param met is the metabolite we want to obtain information from
#' @return returns gene names related to the metabolite
#' @export
kgenes<-function(met){
  KEGG_LINK_BASE <- "http://rest.kegg.jp/link/enzyme/"
  link_REST_url <- paste(KEGG_LINK_BASE, met, sep="")
  link <- readLines(link_REST_url)
  enzs<-sapply(link, function(x){
    strsplit(x, "\t")[[1]][2]
  })
  enzs<-unname(enzs)
  enzs<-sapply(enzs, function(y){
    strsplit(y, "ec:")[[1]][2]
  })
  enzs<-unname(enzs)
  enzs<-enzs[!is.na(enzs)]

  kegg_link_gene<-"http://rest.kegg.jp/link/hsa/enzyme"
  total_genes<-readLines(kegg_link_gene)
  genes<-sapply(enzs, function(c){
    total_genes[grep(c, total_genes)]})
  genes<-unlist(sapply(genes, function(z){
    unname(unlist(sapply(z, function(d){
      sapply(d, function(f){strsplit(f, "\t")[[1]][2]})})))}))
  genes<-unname(unlist(sapply(genes, function(z){
    unname(unlist(sapply(z, function(d){
      sapply(d, function(f){strsplit(f, "hsa:")[[1]][2]})})))})))
  require(rentrez)
  gene_names<-sapply(genes, function(m){
    sapply(m, function(n){
      entrez_summary('gene', n)
    })
  })
  gene_names<-as.data.frame(gene_names)
  gene_names<-(unname(unlist(gene_names[2,])))
  return(gene_names)
}


