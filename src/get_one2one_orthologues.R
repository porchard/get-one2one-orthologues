library(RMySQL)
library(glue)
library(dplyr)


get_one2one_orthologues <- function(species_1, species_2) {
  # connect to database
  host <- 'ensembldb.ensembl.org'
  user <- 'anonymous'
  port <- 3306
  database <- 'ensembl_compara_75'
  
  con <- dbConnect(RMySQL::MySQL(), dbname = database, host=host, user=user, port=port)
  
  
  # determine the genome IDs for our species
  species_genome_db_ids <- dbGetQuery(con, glue('SELECT * FROM genome_db WHERE name = "{species_1}" OR name = "{species_2}"'))
  stopifnot(nrow(species_genome_db_ids)==2)
  taxon_id_1 <- species_genome_db_ids$taxon_id[species_genome_db_ids$name==species_1]
  taxon_id_2 <- species_genome_db_ids$taxon_id[species_genome_db_ids$name==species_2]
  
  # determine the species set/species set ID that corresponds to this relationship
  # get all the species sets
  species_sets <- dbGetQuery(con, 'SELECT * FROM species_set')
  
  # now narrow down to the one containing our species of interest
  tmp <- species_sets %>% dplyr::mutate(is_one_of_our_species = genome_db_id %in% species_genome_db_ids$genome_db_id) %>%
    dplyr::group_by(species_set_id) %>% 
    dplyr::summarize(number_total_species=n(), number_our_species=sum(is_one_of_our_species)) %>%
    dplyr::filter(number_total_species == 2 & number_our_species == 2)
  
  # verify that we found one and only one such set
  stopifnot(nrow(tmp)==1)
  
  our_species_set <- tmp$species_set_id
  
  
  # we'll want the Ensembl orthologues
  tmp <- dbGetQuery(con, 'SELECT * FROM method_link WHERE type="ENSEMBL_ORTHOLOGUES"')
  stopifnot(nrow(tmp)==1)
  method_link_id <- tmp$method_link_id
  
  # now get the method link species set id
  cmd <- glue("SELECT * FROM method_link_species_set WHERE method_link_id = {method_link_id} AND species_set_id = {our_species_set}")
  tmp <- dbGetQuery(con, cmd)
  stopifnot(nrow(tmp)==1)
  method_link_species_id <- tmp$method_link_species_set_id
  
  
  # now grab the homologies
  cmd <- glue("SELECT * FROM homology INNER JOIN homology_member USING (homology_id) WHERE homology.method_link_species_set_id = {method_link_species_id} AND description = 'ortholog_one2one'")
  tmp <- dbGetQuery(con, cmd)
  tmp <- tmp %>% dplyr::select(homology_id, member_id, peptide_member_id)
  
  # link the homologies to genes
  cmd <- glue("SELECT * FROM member WHERE source_name = 'ENSEMBLGENE' AND (taxon_id = {taxon_id_1} OR taxon_id = {taxon_id_2})")
  genes <- dbGetQuery(con, cmd)
  x <- left_join(tmp, genes) %>% dplyr::select(homology_id, stable_id, display_label, taxon_id)
  x$species <- ifelse(x$taxon_id==taxon_id_1, species_1, species_2)
  x <- x %>% dplyr::select(-taxon_id)
  colnames(x) <- c('homology_id', 'ensembl_gene_id', 'gene_name', 'species')
  
  
  dbDisconnect(con)
  
  return(x)
}
