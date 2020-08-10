
# Name: Niki Kyriakopoulou
# Master Research Project: Repair what is broken
# Date:2020

# Use case 1: Paper with title "Bat coronavirus phylogeography in the Western Indian Ocean" (Joffrin et al., 2020)

rm(list = ls(all=T))
setwd("D:/Research project_DISSCO/DISSCO R")
getwd()

install.packages(c("rentrez", "tidyverse"))

citation("rentrez")

# To cite rentrez in publications use:
  
# Winter, D. J. (2017) rentrez: an R package for the NCBI eUtils API The R Journal 9(2):520-526

# A BibTeX entry for LaTeX users is

# @Article{,
#   title = {{rentrez}: an R package for the NCBI eUtils API},
#   author = {David J. Winter},
#   journal = {The R Journal},
#   year = {2017},
#   volume = {9},
#   issue = {2},
#   pages = {520--526},
# }

#  Query GenBank for studied sequences  ---------------------------------

# The DNA sequences used in this paper are stored in NCBI (GenBank) database "nuccore" with accession numbers: MN183146 to MN183273 

library(rentrez)
library(tidyverse)

bats.accessions <- read.csv(file = 'bats.accessions.csv') 
names(bats.accessions)[1] <- "accessions"

# create accession vector
accessions <- unique(bats.accessions$accessions)
class(accessions) 
# character vector
length(accessions) 
# 128 accessions
accessions

# "MN183146" "MN183147" "MN183148" "MN183149" "MN183150" "MN183151" "MN183152" "MN183153" "MN183154" "MN183155" "MN183156" "MN183157" "MN183158"
# "MN183159" "MN183160" "MN183161" "MN183162" "MN183163" "MN183164" "MN183165" "MN183166" "MN183167" "MN183168" "MN183169" "MN183170" "MN183171"
# ...

# First we will check the type of format used for the specimen vouchers.

# Create a comma separated vector of accessions ready for input in R.
dput(as.character(accessions))

# c("MN183146", "MN183147", "MN183148", "MN183149", "MN183150", 
#   "MN183151", "MN183152", "MN183153", "MN183154", "MN183155", "MN183156", ...

# We will use rentrez package that provides functions to use the NCBI REST API Entrez Utilities (Eutils).

entrez_dbs()

# "pubmed"          "protein"         "nuccore"         "ipg"             "nucleotide"      "structure"      
# "sparcle"         "genome"          "annotinfo"       "assembly"        "bioproject"      "biosample"      
# "blastdbinfo"     "books"           "cdd"             "clinvar"         "gap"             "gapplus"        
# "grasp"           "dbvar"           "gene"            "gds"             "geoprofiles"     "homologene"     
# "medgen"          "mesh"            "ncbisearch"      "nlmcatalog"      "omim"            "orgtrack"       
# "pmc"             "popset"          "proteinclusters" "pcassay"         "biosystems"      "pccompound"     
# "pcsubstance"     "seqannot"        "snp"             "sra"             "taxonomy"        "biocollections" 
# "gtr"

entrez_db_summary("nuccore")

# DbName: nuccore
# MenuName: Nucleotide
# Description: Core Nucleotide db
# DbBuild: Build200705-0300m.1
# Count: 426578552
# LastUpdate: 2020/07/06 23:16 

entrez_db_searchable("nuccore")

# Searchable fields for database 'nuccore'
# ALL 	 All terms from all searchable fields 
# UID 	 Unique number assigned to each sequence 
# FILT 	 Limits the records 
# WORD 	 Free text associated with record 
# TITL 	 Words in definition line 
# KYWD 	 Nonstandardized terms provided by submitter 
# AUTH 	 Author(s) of publication 
# JOUR 	 Journal abbreviation of publication 
# VOL 	 Volume number of publication 
# ISS 	 Issue number of publication 
# PAGE 	 Page number(s) of publication 
# ORGN 	 Scientific and common names of organism, and all higher levels of taxonomy 
# ACCN 	 Accession number of sequence 
# PACC 	 Does not include retired secondary accessions 
# GENE 	 Name of gene associated with sequence 
# PROT 	 Name of protein associated with sequence 
# ECNO 	 EC number for enzyme or CAS registry number 
# PDAT 	 Date sequence added to GenBank 
# MDAT 	 Date of last update 
# SUBS 	 CAS chemical name or MEDLINE Substance Name 
# PROP 	 Classification by source qualifiers and molecule type 
# SQID 	 String identifier for sequence 
# GPRJ 	 BioProject 
# SLEN 	 Length of sequence 
# FKEY 	 Feature annotated on sequence 
# PORG 	 Scientific and common names of primary organism, and all higher levels of taxonomy 
# COMP 	 Component accessions for an assembly 
# ASSM 	 Assembly 
# DIV 	 Division 
# STRN 	 Strain 
# ISOL 	 Isolate 
# CULT 	 Cultivar 
# BRD 	 Breed 
# BIOS 	 BioSample 

# Search the NCBI database "nuccore" for the 128 studied accessions.

results <- sapply(dput(as.character(accessions)), entrez_search, db = "nuccore", simplify = FALSE) 

ids <- lapply(results, "[[", "ids") # list of 128 IDs


# Create a data frame with the accessions queried and the GenBank IDs that correspond to the records associated with them.
ids.df <- map(ids, paste0, collapse = " ") %>% bind_rows() %>% gather(accessions, GenBank.ids)

#  Get summaries of GenBank IDs ---------------------------------

summaries <- entrez_summary(db="nuccore", id=ids.df$GenBank.ids)

class(summaries)
# "esummary_list" "list"

# apply over the list "summaries" to extract specific fields
accessions.data.list <- sapply(summaries, function(x) extract_from_esummary(x, elements = c("uid", "caption", "title", "subtype", "subname", "organism")))

# Convert list to data frame.
accessions.data.df <- data.frame(matrix(unlist(accessions.data.list), nrow=128, byrow=T), stringsAsFactors=FALSE)

accessions.data.df <- accessions.data.df[, c(2, 6, 1, 3, 4, 5)]

colnames(accessions.data.df) <- c("accession", "organism", "uid", "definition", "subtype", "subname")

write.csv(accessions.data.df, file = 'bats.summariesGenBank.csv', row.names = F)

# Each accession has one GenBank ID and all of them are linked to the same organism (bat coronavirus). GenBank IDs (uids) are 
# not included in the paper but accessions can be easily traced in nuccore so the IDs are not essential in this case.

accessions.data.df <- read.csv(file = 'bats.summariesGenBank.csv')

# According to the summaries downloaded from GenBank, the field "subname" contains source identifiers "isolate" and "host" 
# which indicate viral source material and not material that has been stored in a museum or a collection 
# (https://www.ncbi.nlm.nih.gov/books/NBK53701/#gbankquickstart.what_kind_of_source_info_3). In this paper, some of the virus 
# material studied was extracted from vouchered specimens, so it is relevant to examine the source identifiers available.

#  Extract and process the isolates and hosts found in the column "subname" ---------------------------------

accessions.data.df <- accessions.data.df %>% add_column(isolate = NA, .after = "subname") # add a new column

accessions.data.df$isolate[is.na(accessions.data.df$isolate)] <- as.character(accessions.data.df$subname[is.na(accessions.data.df$isolate)])

accessions.data.df$isolate = sub("\\|.*$", "", as.character(accessions.data.df$isolate)) # remove part of string after first "|"

accessions.data.df <- accessions.data.df %>% add_column(host = NA, .after = "isolate") # add a new column

accessions.data.df$host[is.na(accessions.data.df$host)] <- as.character(accessions.data.df$subname[is.na(accessions.data.df$host)])

accessions.data.df$host = sub("\\|[^|]*$", "", as.character(accessions.data.df$host))

accessions.data.df$host = sub("\\|[^|]*$", "", as.character(accessions.data.df$host))

accessions.data.df$host = sub("\\|[^|]*$", "", as.character(accessions.data.df$host))

accessions.data.df$host = sub("^[^|]*|", "", as.character(accessions.data.df$host))

accessions.data.df$host <- gsub("^\\|", "", accessions.data.df$host)

accessions.data.df$isolate

# "19020"  "19037"  "19028"  "19031"  "19032"  "19030"  "19026"  "19027"  "18990"  "18991"  "18992"  "19013"  "19004"  "19024"  "19015"  "19006" 
# "18958"  "18996"  "19154"  "19174"  "19183"  "18997"  "18993"  "19185"  "19002"  "18951"  "18987"  "015"    "051"    "025"    "027"    "004"   
# ...

# No conclusions can be drawn from the isolates regarding any institutions and/or collections.

accessions.data.df <- accessions.data.df %>% add_column(host.species = NA, .after = "host") # add a new column

accessions.data.df$host.species[is.na(accessions.data.df$host.species)] <- as.character(accessions.data.df$host[is.na(accessions.data.df$host.species)])

accessions.data.df$host.species = sub("\\;[^;]*$", "", as.character(accessions.data.df$host.species))

accessions.data.df$host.species = sub("\\;[^;]*$", "", as.character(accessions.data.df$host.species))

accessions.data.df <- accessions.data.df %>% add_column(host.identifier = NA, .after = "host.species") # add a new column

accessions.data.df$host.identifier[is.na(accessions.data.df$host.identifier)] <- as.character(accessions.data.df$host[is.na(accessions.data.df$host.identifier)])

# replace values that do not contain "text;text;text" with empty cells.

accessions.data.df <- accessions.data.df %>%
  mutate(host.identifier = case_when(
    !str_detect(host.identifier, ".*;(.*)\\;.*") ~ "",
    TRUE ~ host.identifier
  )
)

accessions.data.df$host.identifier = sub("^[^;]*;", "", as.character(accessions.data.df$host.identifier))

accessions.data.df$host.identifier = sub("^[^;]*;", "", as.character(accessions.data.df$host.identifier))

write.csv(accessions.data.df, file = 'bats.dataGenBank.csv', row.names = F)


# Process bats.dataGenBank csv file.

#  Examine the format of host identifiers ---------------------------------

accessions.data <- read.csv(file = 'bats.dataGenBankNEW.csv')

length(accessions.data$host.identifier[accessions.data$host.identifier==""])
# 14 sequence accessions do not have a host identifier, probably they come from non-vouchered specimens

(length(accessions.data$host.identifier[accessions.data$host.identifier==""])/length(accessions))*100
# 10.9375%
  
length(accessions.data$host.identifier[accessions.data$host.identifier!=""])
# 114 sequence accessions have a host identifier

(length(accessions.data$host.identifier[accessions.data$host.identifier!=""])/length(accessions))*100
# 89.0625%

# These identifiers have the GenBank specimen voucher format =specimen_id and since some of the samples were taken
# from vouchered specimens, the identifiers might actually represent actual specimen vouchers.


