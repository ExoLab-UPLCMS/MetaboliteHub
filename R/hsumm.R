#' Retrieve all available information of interest from HMDB entry
#'
#' Starting from HMDB ID, it retrieves the Accession number of the metabolite, the name and chemical and SMILE formula,
#' the diseases/pathways described in which it is involved, the KEGG ID to connect with KEGG database and the genes that are
#' related to it.
#'
#'
#'
#'
#' @param x is the metabolite codified in HMDB ID
#' @return returns a data.frame of different data entries: Acces for accession nubmer, Name for the metabolite name, Formula for its formula, SMILES for
#' the formula in SMILES form, Genes for the list of genes related to the metabolite and Paths for the metabolic pathways related to the metabolite.
#' @import XML
#' @import httr
#' @export
hsumm<-function(x){
  temp <- xmlToList(xmlParse(GET(paste0("https://hmdb.ca/metabolites/", x, ".xml"))))
  Accession = temp$accession
  Name = temp$name
  Formula = temp$chemical_formul
  Smiles = temp$smile
  Genes <- vector("list", length = length(temp$protein_associations))
  for (i in 1:length(temp$protein_associations)) {
    Genes[i] <- temp$protein_associations[i]$protein$gene_name
  }
  KEGG_ID = temp$kegg_id
  Paths <- vector("list", length = length(temp$biological_properties$pathways))
  if (length(temp$biological_properties$pathways) == 1) {
    Paths = temp$biological_properties$pathways$pathway$name
  }
  else {
    for (a in 1:length(temp$biological_properties$pathways)) {
      Paths[a] <- temp$biological_properties$pathways[a]$pathway$name
    }
  }
  hmdb_data <- list(Acces = Accession, Name = Name, Formula = Formula, 
                    SMILES = Smiles, Genes = unlist(Genes), KEGG = KEGG_ID, Paths = unlist(Paths))
}
