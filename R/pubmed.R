#' Obtains information from PubMed Database
#'
#' Using a series of keywords, this function accesses the PubMed database and look for publications related to specific gene, filtering the results depending on the keywords used.
#' It is generated in order to filter for publications related to Homo sapiens specie.
#'
#'
#'
#'
#' keywords_pubmed Depending on what we want, we have different options:
#' - to filter for words in Title/Abstract add [Title/Abtract] after the word.
#' - to filter for words in the general text, add [Text Word] after the word.
#' - to filter por publication type, add [ptyp] after the word. For example, for a review: Review[ptyp].
#' keywords_pubmed object is generated as follows: keywords_pubmed<-c("[Title/Abtract] whatever text we want")
#' @param x is the gene we want to look for information
#' @param keywords_pubmed is a character string that limits your search
#' @return returns a list of ID of GEO entries, the description, title, pubmed_id and citation details of each GEO entry.
#' @import RISmed
#' @export
pubmed<-function(x, keywords_pubmed){
  query_search<-EUtilsSummary(paste0(x, '[Gene Name] ', keywords_pubmed),type="esearch", db="pubmed")
  summary_pubmed<-as.data.frame(cbind(
    Title=ArticleTitle(EUtilsGet(query_search)),
    Authors=Author(EUtilsGet(query_search)),
    Abstract=AbstractText(EUtilsGet(query_search)),
    Journal=ISOAbbreviation(EUtilsGet(query_search)),
    Volume=Volume(EUtilsGet(query_search)),
    Year=YearPubmed(EUtilsGet(query_search)),
    Article_PubMed_ID=ArticleId(EUtilsGet(query_search)),
    DOI=ELocationID(EUtilsGet(query_search)),
    Citation=RefSource(EUtilsGet(query_search))))
  return(summary_pubmed)}
