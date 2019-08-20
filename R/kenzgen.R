#' Retrieve genes related to a specific enzyme
#'
#' It takes the EC code of the enzyme and searches on KEGG database for related genes
#' @param x is the enzyme that we want information from
#' @return names of the genes related to the enzyme
#' @import rentrez
#' @export
kenzgen<-function(x){
  kegg_link_gene<-"http://rest.kegg.jp/link/hsa/enzyme"
  total_genes<-readLines(kegg_link_gene)
  genes<-sapply(x, function(c){
    total_genes[grep(c, total_genes)]})
  genes<-unlist(sapply(genes, function(z){
    unname(unlist(sapply(z, function(d){
      sapply(d, function(f){strsplit(f, "\t")[[1]][2]})})))}))
  genes<-unname(unlist(sapply(genes, function(z){
    unname(unlist(sapply(z, function(d){
      sapply(d, function(f){strsplit(f, "hsa:")[[1]][2]})})))})))
  gene_names<-sapply(genes, function(m){
    sapply(m, function(n){
      entrez_summary('gene', n)
    })
  })
  gene_names<-as.data.frame(gene_names)
  gene_names<-(unname(unlist(gene_names[2,])))
}
