
# Name: Niki Kyriakopoulou
# Master Research Project: Repair what is broken
# Date:2020

# Use case 3: Paper with title "A complete time-calibrated multi-gene phylogeny of the European butterflies" (Wiemers et al., 2020)

rm(list = ls(all=T))
setwd("D:/Research project_DISSCO/DISSCO R")
getwd()

install.packages(c("tidyverse", "rentrez",  "urltools", "httr", "jsonlite"))

#  Import dataset listing European butterfly species with higher taxonomy, voucher codes and accession numbers of the studied sequences  ---------------------------------

# The vouchers are stored in NCBI (GenBank) database "nuccore" and the original dataset is available here: 
# https://zookeys.pensoft.net/article/50878/download/suppl/31/.

library(tidyverse)
library(rentrez)
library(httr)
library(jsonlite)

butterflies.vouchers <- read.csv(file = "zookeys-938-097-s001.csv") 
names(butterflies.vouchers)[1] <- "Species.name"

# create voucher vector
voucher <- unique(butterflies.vouchers$Voucher)
class(voucher) 
# character vector
length(voucher) 
# 496 vouchers
voucher

# "RV12O525"              "NW63-16"               "NW63-3"                "NHMO-06179"            "AD00P259"              "VL05Z994"              "VL01B424"             
# "AD03B064"              "MW99018"               "RV09V415"              "NW151-2"               "WMB1125-13"            "NW151-1"               "BC_SB_Lep_0030"    
# ...

# Examine the format used for the specimen vouchers.

# Detect the presence or absence of ":".

presence <- str_detect(butterflies.vouchers$Voucher, ":")

length(presence[presence == "FALSE"])
# 496 vouchers do not contain ":".

# 100% of the vouchers have the GenBank format /specimen_voucher=specimen_id.


#  Query GenBank for studied vouchers  ---------------------------------

# Create a comma separated vector of vouchers ready for input in R.
dput(as.character(voucher))

# c("RV12O525", "NW63-16", "NW63-3", "NHMO-06179", "AD00P259", 
#   "VL05Z994", "VL01B424", "AD03B064", "MW99018", "RV09V415", "NW151-2",... 

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
# DbBuild: Build200623-1240m.1
# Count: 425580984
# LastUpdate: 2020/06/25 19:20 

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

# Search the NCBI database "nuccore" for the 496 studied vouchers.

results <- sapply(dput(as.character(voucher)), entrez_search, db = "nuccore", simplify = FALSE)

ids <- lapply(results, "[[", "ids") # list of 496

# Create a data frame with the vouchers queried and the GenBank IDs that correspond to the records associated with them.
ids.df <- map(ids, paste0, collapse = " ") %>% bind_rows() %>% gather(Voucher, GenBank.ids)

# The column GenBank.ids can have 0, 1 or multiple IDs. We want to separate it into multiple rows without losing those
# vouchers with 0 ID.

ids.dfA <- subset(ids.df, ids.df$GenBank.ids == "") # vouchers with 0 IDs.

length(unique(ids.dfA$Voucher))
# 90 unique vouchers with 0 ID, these vouchers were not found in GenBank

(length(unique(ids.dfA$Voucher))/length(unique(ids.df$Voucher)))*100
# 18.14516% of the 496 vouchers were not found in GenBank (0 IDs)

ids.dfB <- ids.df %>%
  mutate(GenBank.ids=strsplit(GenBank.ids, " ")) %>%
  unnest(GenBank.ids)

length(unique(ids.dfB$Voucher))
# 406 unique vouchers with at least 1 ID

(length(unique(ids.dfB$Voucher))/length(unique(ids.df$Voucher)))*100
# 81.85484% of the 496 vouchers were found in GenBank and are associated to one or more IDs

length(unique(ids.dfB$GenBank.ids))
# 2123 unique IDs that are associated with 406 unique vouchers

# Subset ids.dfB
ids.dfB = ids.dfB[!duplicated(ids.dfB$GenBank.ids),]


#  Get summaries of GenBank IDs ---------------------------------

# Get summaries of the 2123 unique GenBank.ids.
summaries <- entrez_summary(db="nuccore", id=unique(ids.dfB$GenBank.ids))

# Error in entrez_check(response) : 
#   HTTP failure 414, the request is too large. For large requests, try using web history as described in the rentrez tutorial

# create an index that splits
bigqueryindex <- split(seq(1, length(unique(ids.dfB$GenBank.ids))), ceiling(seq_along(seq(1, length(unique(ids.dfB$GenBank.ids))))/200))

# create an empty list to hold summary contents
summaries <- list()

# loop through the list of ids 200 at a time and pause for 5 seconds in between queries
for (i in bigqueryindex) {
  summaries[unlist(i)] <- entrez_summary("nuccore", unique(ids.dfB$GenBank.ids)[unlist(i)])
  Sys.sleep(5)
}

class(summaries)
# "list" of 2123 elements

# apply over the list "summaries" to extract specific fields
voucher.data.list <- sapply(summaries, function(x) extract_from_esummary(x, elements = c("uid", "caption", "title", "subtype", "subname")))

# convert list to data frame with 2123 observations.
voucher.data.df <- data.frame(matrix(unlist(voucher.data.list), nrow=2123, byrow=T), stringsAsFactors=FALSE)

voucher.data.df <- cbind(ids.dfB$Voucher, voucher.data.df)

voucher.data.df <- voucher.data.df[, c(3, 2, 1, 6, 5, 4)]

colnames(voucher.data.df) <- c("accession", "uid", "voucher", "subname", "subtype", "definition")

# Run through the 'definition' column, checking if the value fully or partially matches to one of the strings in the vector (butterflies.vouchers$Genus), 
# and filter out the ones that aren't (delete organisms that weren't studied in the paper).

voucher.dataNEW <- filter(voucher.data.df, grepl(paste(dput(as.character(butterflies.vouchers$Genus)), collapse = "|"), definition))

length(unique(voucher.dataNEW$voucher))
# after the query in GenBank, we have 360 unique vouchers from the paper linked to 1464 unique IDs (accessions).

write.csv(voucher.dataNEW, file = "butterflies.summariesGenBank.csv", row.names = F)

# One voucher can be linked to many sequences and in some cases to different organisms. 


#  Extract and process the specimen vouchers and isolates found in the column "subname" ---------------------------------

# Modify butterflies.summariesGenBank.csv file, take out sub-species from columns "subname" and "subtype" and keep specimen vouchers
# and isolates first in these columns.

voucher.dataNEW <- read.csv(file = "butterflies.summariesGenBankNEW.csv")

voucher.dataNEW <- voucher.dataNEW %>% add_column(source_identifier = NA, .after = "subname") # add a new column

voucher.dataNEW$source_identifier[is.na(voucher.dataNEW$source_identifier)] <- as.character(voucher.dataNEW$subname[is.na(voucher.dataNEW$source_identifier)])

voucher.dataNEW$source_identifier = sub("\\|.*$", "", as.character(voucher.dataNEW$source_identifier)) # remove part of string after first "|"


# We have a new data frame with the column "voucher" that contains the vouchers obtained from the paper and the column
# "source_identifier" that includes the vouchers and isolates from the GenBank query.


# Some accessions seem to have more than one source identifiers separated by ;.

source_identifiers <- voucher.dataNEW %>% filter(str_detect(voucher.dataNEW$source_identifier, ";"))
length(source_pointers$accession)
# 15 accessions have more than one source identifier

voucher.dataNEW <- voucher.dataNEW %>% add_column(specimen_id = NA, .after = "source_identifier") # add a new column

voucher.dataNEW$specimen_id[is.na(voucher.dataNEW$specimen_id)] <- as.character(voucher.dataNEW$source_identifier[is.na(voucher.dataNEW$specimen_id)]) # replace NA values in column "specimen_id" with the values in the adjacent column "source_identifier"

voucher.dataNEW$specimen_id = sub(".*:", "", as.character(voucher.dataNEW$specimen_id)) # remove part of string before last ":"

voucher.dataNEW <- voucher.dataNEW %>%
  mutate(specimen_id = case_when(
    str_detect(specimen_id, ";") ~ "",
    TRUE ~ specimen_id
  )
)

# specimen_id column ready


voucher.dataNEW <- voucher.dataNEW %>% add_column(institution_code = NA, .after = "source_identifier") # create a new column

voucher.dataNEW$institution_code[is.na(voucher.dataNEW$institution_code)] <- as.character(voucher.dataNEW$source_identifier[is.na(voucher.dataNEW$institution_code)]) # replace NA values in column "institution_code" with the values in the column "source_identifier"

# replace values that do not contain ":" with empty cells.

voucher.dataNEW <- voucher.dataNEW %>%
  mutate(institution_code = case_when(
    !str_detect(institution_code, ":") ~ "",
    TRUE ~ institution_code
  )
)

voucher.dataNEW$institution_code = sub("\\:[^:]*", "", as.character(voucher.dataNEW$institution_code)) # remove part of string after first ":"

# institution_code column ready


voucher.dataNEW <- voucher.dataNEW %>% add_column(specimen_idA = NA, .after = "specimen_id") # create a new column

voucher.dataNEW$specimen_idA[is.na(voucher.dataNEW$specimen_idA)] <- as.character(voucher.dataNEW$source_identifier[is.na(voucher.dataNEW$specimen_idA)])

voucher.dataNEW <- voucher.dataNEW %>%
  mutate(specimen_idA = case_when(
    !str_detect(specimen_idA, ";") ~ "",
    TRUE ~ specimen_idA
  )
)

voucher.dataNEW <- voucher.dataNEW %>% add_column(specimen_idB = NA, .after = "specimen_idA") # create a new column

voucher.dataNEW$specimen_idB[is.na(voucher.dataNEW$specimen_idB)] <- as.character(voucher.dataNEW$specimen_idA[is.na(voucher.dataNEW$specimen_idB)])

voucher.dataNEW$specimen_idA = sub("\\;.*$", "", as.character(voucher.dataNEW$specimen_idA)) # remove part of string after first ";"

voucher.dataNEW <- voucher.dataNEW[, c(1, 2, 3, 4, 10, 5, 6, 7, 8, 9, 11)]

voucher.dataNEW$specimen_idB = sub("^[^;]*;", "", as.character(voucher.dataNEW$specimen_idB)) # remove part of string before first ";"

# specimen_idA and specimen_idB columns ready


write.csv(voucher.dataNEW, file = "butterflies.dataGenBank.csv", row.names = F)


DwCD <- voucher.dataNEW %>%
  filter(institution_code!="" & specimen_id!="")

length(DwCD$accession)
# 115 accessions have institution_code and specimen_id

specimen_ID <- voucher.dataNEW %>%
  filter(institution_code=="" & specimen_id!="")

length(specimen_ID$accession)
# 1333 accessions have one specimen_id

specimen_IDs <- voucher.dataNEW %>%
  filter(specimen_idA!="" & specimen_idB!="")

length(specimen_IDs$accession)
# 15 accessions have two specimen_id

no_identifier <- voucher.dataNEW %>%
  filter(institution_code=="" & specimen_id=="" & specimen_idA=="" & specimen_idB=="")

length(no_identifier$accession)
# 1 accession doesn't have any identifier

# 0 accessions have institution_code, collection_code and specimen_id (Darwin Core Triplet).


# Table and pie chart showing the types of source identifier formats associated with the 1,464 sequences linked to the vouchers studied.

x <- c("No source identifier", "specimen_id", "institution_code:specimen_id")
y <- c(1, 1348, 115)
x_name <- "identifier"
y_name <- "accessions count"
df1 <- data.frame(x,y)
names(df1) <- c(x_name,y_name)
df1$percentage <- (df1$`accessions count`/1464)*100

print(df1) # dataframe that includes the number of accessions that have no identifier, one or two parts of the 
           # Darwin Core Triplet as well as their percentages in a total of 1,464 accessions (same number as IDs)

#                       identifier  accessions count  percentage
# 1           No source identifier                 1  0.06830601
# 2                   specimen_id               1348 92.07650273
# 3  institution_code:specimen_id                115  7.85519126


slices <- c(1, 1348, 115)
lbls <- c("No source identifier", "specimen_id", "institution_code:specimen_id")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # add % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Source identifier formats associated with the 1,464 sequences linked to the vouchers studied")


# Conclusions: 496 unique vouchers analysed in the paper. They have the GenBank format /specimen_voucher=specimen_id 
# and they cannot be traced back to the institutions and/or collections where they are stored. They were queried in
# GenBank and they were associated with GenBank IDs. 90 vouchers (18.14516%) had 0 IDs (0 sequences) and 406 (81.85484%)
# had one or more IDs (one or more sequence accessions). Theses 406 vouchers are linked to 2123 unique IDs in total, which 
# were queried again in order to get their summaries. The summaries of 1464 unique IDs actually linked to the butterfly 
# species studied. These IDs are associated with 360 unique specimen vouchers from the paper (we queried 406 vouchers 
# with entrez_summary but 46 didn't result in summaries).


# According to Martin Wiemers, 14 vouchers have specific Tissue IDs assigned to them by the museum they are stored in.

#  Import dataset listing vouchers and Tissue IDs from Wiemer's personal collection  ---------------------------------

butterflies.Wiemers <- read.csv(file = "butterflies.Wiemers.csv")

names(butterflies.Wiemers)[1] <- "Voucher" 
names(butterflies.Wiemers)[2] <- "Tissue ID"

tissue_ID <- butterflies.Wiemers$`Tissue ID`

tissue_ID

# "ZFMK-TIS-31171" "ZFMK-TIS-32408" "ZFMK-TIS-32393" "ZFMK-TIS-32115" "ZFMK-TIS-30226" "ZFMK-TIS-30681"
# "ZFMK-TIS-32013" "ZFMK-TIS-32403" "ZFMK-TIS-32399" "ZFMK-TIS-30491" "ZFMK-TIS-31288" "ZFMK-TIS-30496"
# "ZFMK-TIS-31643" "ZFMK-TIS-30549"

# These identifiers seem to conform to the INSDC format /specimen_voucher=institution_code:collection_code:specimen_id 
# but instead of a colon (":") delimiter, they have dash ("-") delimiter.

butterflies.Wiemers.codes <- separate(data = butterflies.Wiemers, col = 'Tissue ID', into = c("institution_code", "collection_code", "specimen_id"), sep = "-", remove = FALSE)


#  Search institution codes against ROR API using /organizations?query= (https://github.com/ror-community/ror-api) ---------------------------------

# institution codes from Wiemers and GenBank

institution_code <- unique(butterflies.Wiemers.codes$institution_code)

institution_code

# "ZFMK"

voucher.dataNEW$institution_code[voucher.dataNEW$institution_code==""] <- NA

code <- unique(voucher.dataNEW$institution_code)

code <- na.omit(code) # character vector with unique institution codes without NAs

code

# "MCZ"

institution_codes <- c(institution_code, code)

base_ROR = "https://api.ror.org/organizations?query=%s"

institution_code_RORurl <- sprintf(base_ROR, institution_codes)

institution_code_RORurl[1]

# "https://api.ror.org/organizations?query=ZFMK"

URL <- lapply(institution_code_RORurl, GET)

URL[1]

# [[1]]
# Response [https://api.ror.org/organizations?query=ZFMK]
# Date: 2020-07-09 16:00
# Status: 200
# Content-Type: application/json
# Size: 672 B

content <- lapply(URL, content, "text")

list <- lapply(content, fromJSON) # list of lists

list[[1]]$number_of_results
# 1

list[[2]]$number_of_results
# 1

list[[1]]$items$name
# "Zoological Research Museum Alexander Koenig"

list[[2]]$items$name
# "Methodist Church in Zimbabwe"


#  Search institution codes against GRID database ---------------------------------

# Filter GRID.json data frame. Select rows based on the values in "acronyms" variable matching to ZMFK and MCZ.

GRID.json <- read_json("grid-2020-03-15/grid.json", simplifyVector = TRUE)

class(GRID.json)
# "list"

class(GRID.json$institutes)
# "data.frame"

GRID.json.match <- filter(GRID.json$institutes, acronyms %in% institution_codes)

GRID.json.match$name
# "Zoological Research Museum Alexander Koenig" "Methodist Church in Zimbabwe" 


#  Search institution codes against GBIF Registry of Scientific Collections API (https://www.gbif.org/developer/registry) ---------------------------------
  
base_GBIF = "https://api.gbif.org/v1/grscicoll/institution?q=%s"

institution_code_GBIFurl <- sprintf(base_GBIF, institution_codes)

institution_code_GBIFurl[1]

# "https://api.gbif.org/v1/grscicoll/institution?q=ZFMK"

URL1 <- lapply(institution_code_GBIFurl, GET)

URL1[1]

# [[1]]
# Response [https://api.gbif.org/v1/grscicoll/institution?q=ZFMK]
# Date: 2020-07-09 16:52
# Status: 200
# Content-Type: application/json
# Size: 2.24 kB

content1 <- lapply(URL1, content, "text")

list1 <- lapply(content1, fromJSON) # list of lists

list1[[1]]$count
# 1

list1[[2]]$count
# 2

list1[[1]]$results[["name"]]
# "Zoologisches Forschungsmuseum Alexander Koenig"

list1[[2]]$results[["name"]]
# "Museum of Comparative Zoology" "Museo Civico di Zoologia"


#  Search collection codes against GBIF Registry of Scientific Collections API (https://www.gbif.org/developer/registry) ---------------------------------
  
collection_code <- unique(butterflies.Wiemers.codes$collection_code)

collection_code

# "TIS"

base_GBIF1 = "https://api.gbif.org/v1/grscicoll/collection?q=%s"

collection_code_GBIFurl <- sprintf(base_GBIF1, collection_code)

collection_code_GBIFurl

# "https://api.gbif.org/v1/grscicoll/collection?q=TIS"

URL2 <- GET(url = collection_code_GBIFurl)

URL2

# Response [https://api.gbif.org/v1/grscicoll/collection?q=TIS]
# Date: 2020-06-29 20:00
# Status: 200
# Content-Type: application/json
# Size: 56.2 kB

content2 <- content(URL2, as = "text")

list2 <- fromJSON(content2)

list2$count
# 105 results

list2$results[["name"]]

# [1] "The NIST Biorepository"                                          
# [2] "Great Lakes Human Health Fish Fillet Tissue Study"               
# [3] "Great Lakes Human Health Fish Tissue Study"                      
# [4] "ZFMK Tissue Bank (part of ZFMK Biobank)"                         
# [5] "ZFMK Tissue Bank (part of ZFMK Biobank)"                         
# [6] "Tissues" 
# ...

# The correct result comes up 4th and 5th. This is also confirmed by the following table that shows the countries where the collections are held.

list2$results[["address"]]

#    key      country      address
# 1  25558    <NA>         <NA>
# 2  25499    <NA>         <NA>
# 3  25495    <NA>         <NA>
# 4  11787     DE          <NA>
# 5  11786     DE          <NA>
# 6  NA       <NA>         <NA>



