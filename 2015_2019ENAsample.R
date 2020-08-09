#Name: Niki Kyriakopoulou
#Master Research Project: Repair what is broken
#Date:2020

#Programmatic access to sequence records held with the European Nucleotide Archive (ENA).
#We will retrieve public data regarding these sequences held within ENA utilizing ENA portal API in JSON format.

#ENA provides two different APIS; Portal API available in this link: https://www.ebi.ac.uk/ena/portal/api and
#Browser API found here: https://www.ebi.ac.uk/ena/browser/api. More information regarding the documentation 
#can be found in https://www.ebi.ac.uk/ena/portal/api/doc and https://www.ebi.ac.uk/ena/browser/api/doc 
#respectively.

rm(list = ls(all=T))
setwd("D:/Research project_DISSCO/DISSCO R")
getwd()

citation()
citation("base")

# To cite R in publications use:
  
# R Core Team (2020). R: A language and environment for statistical computing. R Foundation for
# Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

# A BibTeX entry for LaTeX users is

# @Manual{,
#   title = {R: A Language and Environment for Statistical Computing},
#   author = {{R Core Team}},
#   organization = {R Foundation for Statistical Computing},
#   address = {Vienna, Austria},
#   year = {2020},
#   url = {https://www.R-project.org/},
# }

install.packages(c("httr", "urltools", "jsonlite", "tidyverse"))

citation("httr")

# To cite package ‘httr’ in publications use:

# Hadley Wickham (2019). httr: Tools for Working with URLs and HTTP. R package version 1.4.1.
# https://CRAN.R-project.org/package=httr

# A BibTeX entry for LaTeX users is

# @Manual{,
#   title = {httr: Tools for Working with URLs and HTTP},
#   author = {Hadley Wickham},
#   year = {2019},
#   note = {R package version 1.4.1},
#   url = {https://CRAN.R-project.org/package=httr},
# }

citation("urltools")

# To cite package ‘urltools’ in publications use:
  
# Os Keyes, Jay Jacobs, Drew Schmidt, Mark Greenaway, Bob Rudis, Alex Pinto, Maryam Khezrzadeh, Peter
# Meilstrup, Adam M. Costello, Jeff Bezanson, Peter Meilstrup and Xueyuan Jiang (2019). urltools:
# Vectorised Tools for URL Handling and Parsing. R package version 1.7.3.
# https://CRAN.R-project.org/package=urltools

# A BibTeX entry for LaTeX users is

# @Manual{,
#   title = {urltools: Vectorised Tools for URL Handling and Parsing},
#   author = {Os Keyes and Jay Jacobs and Drew Schmidt and Mark Greenaway and Bob Rudis and Alex Pinto and Maryam Khezrzadeh and Peter Meilstrup and Adam M. Costello and Jeff Bezanson and Peter Meilstrup and Xueyuan Jiang},
#   year = {2019},
#   note = {R package version 1.7.3},
#   url = {https://CRAN.R-project.org/package=urltools},
# }

citation("jsonlite")

# To cite jsonlite in publications use:
  
# Jeroen Ooms (2014). The jsonlite Package: A Practical and Consistent Mapping Between JSON Data and R
# Objects. arXiv:1403.2805 [stat.CO] URL https://arxiv.org/abs/1403.2805.

# A BibTeX entry for LaTeX users is

# @Article{,
#   title = {The jsonlite Package: A Practical and Consistent Mapping Between JSON Data and R Objects},
#   author = {Jeroen Ooms},
#   journal = {arXiv:1403.2805 [stat.CO]},
#   year = {2014},
#   url = {https://arxiv.org/abs/1403.2805},
# }

citation("tidyverse")

# Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686,
# https://doi.org/10.21105/joss.01686

# A BibTeX entry for LaTeX users is

# @Article{,
#   title = {Welcome to the {tidyverse}},
#   author = {Hadley Wickham and Mara Averick and Jennifer Bryan and Winston Chang and Lucy D'Agostino McGowan and Romain Francois and Garrett Grolemund and Alex Hayes and Lionel Henry and Jim Hester and Max Kuhn and Thomas Lin Pedersen and Evan Miller and Stephan Milton Bache and Kirill Muller and Jeroen Ooms and David Robinson and Dana Paige Seidel and Vitalie Spinu and Kohske Takahashi and Davis Vaughan and Claus Wilke and Kara Woo and Hiroaki Yutani},
#   year = {2019},
#   journal = {Journal of Open Source Software},
#   volume = {4},
#   number = {43},
#   pages = {1686},
#   doi = {10.21105/joss.01686},
#  }


#The main API endpoint is /search. "It performs a search against a single data group (result) of the data available in ENA".
#The parameters of the /search endpoint are: 
#result --> the result type (data set) to search against,
#query --> a set of search conditions joined by logical operators (AND, OR, NOT) and bound by "",
#fields --> a list of fields to be returned in the result,
#etc (more info can be found in ENA portal API documentation).

#Which fields to return? In order to be able to link a sequence with its physical object (held by an institute, 
#a collection or a lab), we need to choose fields that represent pointers to physical material. For sequence_release
#(and sequence_update) type of result the following fields can be added to the query in order to obtain information 
#regarding these pointers (ENA portal API documentation): accession, bio_material (=Identifier for biological material 
#including institution and collection code), culture_collection (=Identifier for the sample culture including institution 
#and collection code) and specimen_voucher (=Identifier for the sample culture including institution and collection code).


#  Query the ENA PORTAL API ---------------------------------

library(httr)
library(jsonlite)
library(tidyverse)

base_url = "https://www.ebi.ac.uk/ena/portal/api/search?"

full_url = paste0(base_url, "result=sequence_release",
                  "&query=collection_date>=2015-01-01 AND collection_date<=2019-12-31",
                  "&fields=accession,country,location,description,scientific_name,bio_material,culture_collection,specimen_voucher,sample_accession,study_accession",
                  "&limit=0&format=json")

#view full_url
full_url

#[1] "https://www.ebi.ac.uk/ena/portal/api/search?result=sequence_release&query=collection_date>=2015-01-01 AND collection_date<=2019-12-31&fields=accession,country,location,description,scientific_name,bio_material,culture_collection,specimen_voucher,sample_accession,study_accession&format=json"

#encode the URL adding characters for each space
full_url <- URLencode(full_url)

search <- GET(url=full_url)

search$status_code
#[1] 200

search.txt <- content(search, as = "text", encoding = "UTF-8")

#convert the response into a data frame
search.df <- fromJSON(search.txt, flatten = TRUE)

#view data structure
str(search.df)

#'data.frame':	1404517 obs. of  10 variables:
#$ accession         : chr  "AB641402" "AB641409" "AB795663" "AB795664" ...
#$ country           : chr  "Taiwan:Kaohsiung" "Taiwan:Kaohsiung" "Japan:Hyogo, Moroyose" "Japan:Hyogo, Moroyose" ...
#$ location          : chr  "" "" "" "" ...
#$ description       : chr  "Coxsackievirus B5 VP1 gene for capsid protein,..

search.df <- as_tibble(search.df)

search.df

# A tibble: 1,404,517 x 10
# accession  country    location  description           scientific_name bio_material culture_collect~ specimen_voucher sample_accession study_accession
#  <chr>     <chr>      <chr>     <chr>                 <chr>           <chr>        <chr>            <chr>            <chr>            <chr>          
#1 AB641402  Taiwan:Ka~ ""        Coxsackievirus B5 VP~ Coxsackievirus~ ""           ""               ""               ""               ""             
#2 AB641409  Taiwan:Ka~ ""        Coxsackievirus B5 VP~ Coxsackievirus~ ""           ""               ""               ""               ""             
#3 AB795663  Japan:Hyo~ ""        Cyclopteropsis bergi~ Cyclopteropsis~ ""           ""               ""               ""               ""             
#4 AB795664  Japan:Hyo~ ""        Cyclopteropsis bergi~ Cyclopteropsis~ ""           ""               ""               ""               ""             
#5 AB795671  Japan:Hyo~ ""        Eumicrotremus asperr~ Eumicrotremus ~ ""           ""               ""               ""               ""             
#6 AB795700  Japan:Hyo~ ""        Eumicrotremus asperr~ Eumicrotremus ~ ""           ""               ""               ""               ""             
#7 AB795701  Japan:Hyo~ ""        Eumicrotremus asperr~ Eumicrotremus ~ ""           ""               ""               ""               ""             
#8 AB795702  Japan:Hyo~ ""        Eumicrotremus asperr~ Eumicrotremus ~ ""           ""               ""               ""               ""             
#9 AB795703  Japan: Hy~ ""        Eumicrotremus asperr~ Eumicrotremus ~ ""           ""               ""               ""               ""             
#10 AP014562  Japan:Myo~ "32.1 N ~ Bathymodiolus septem~ Bathymodiolus ~ ""           ""               ""               ""               ""             
# ... with 1,404,507 more rows

write.csv(search.df, file = "D:/Research project_DISSCO/DISSCO R/ENA_sequences_2015_2019.csv", row.names = F)

length(search.df$accession)
#[1] 1,404,517 accessions


#  How many accessions have information in at least one of the pointers to physical material? ---------------------------------

search.df <- read.csv(file = "D:/Research project_DISSCO/DISSCO R/ENA_sequences_2015_2019.csv")

#Create a new data frame with sequence accessions that have source pointers.
search.df.source <- search.df %>% filter(!(bio_material == "" & specimen_voucher == "" & culture_collection == ""))

length(search.df.source$accession)
#[1] 461,712 accessions

(length(search.df.source$accession)/length(search.df$accession))*100
#[1] 32.87337% of the 1.4 million sequence accessions have source pointers and will be used for further analysis

write.csv(search.df.source, file = "D:/Research project_DISSCO/DISSCO R/ENAsource.analysis.csv", row.names = F)


#  How many accessions do not have information in one of the pointers to physical material? ---------------------------------

search.df.nosource <- search.df %>% filter(bio_material == "" & specimen_voucher == "" & culture_collection == "")

length(search.df.nosource$accession)
#[1]  942,803 accessions do not have source pointer info

(length(search.df.nosource$accession)/length(search.df$accession))*100
#[1] 67.12649% of the 1.4 million sequence accessions do not have source pointer info

#2 accessions are missing so they will be excluded from the analysis

slices <- c(461712, 942803)
lbls <- c("one source modifier", "no source modifier")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # add % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Source pointer information in 1.4 million sequence accessions")

write.csv(search.df.nosource, file = "D:/Research project_DISSCO/DISSCO R/ENAnosource.analysis.csv", row.names = F)


#  How many accessions from the remaining 67% have a sample accession? ---------------------------------

sample <- search.df.nosource %>% filter(sample_accession != "")
  
length(sample$accession)
#[1] 63,557 sequence accessions have sample accession

(length(sample$accession)/length(search.df.nosource$accession))*100
#[1] 6.741281%

write.csv(sample, file = "D:/Research project_DISSCO/DISSCO R/ENAsample.analysis.csv", row.names = F)


#  How many accessions from the remaining 67% do not have sample accession? ---------------------------------

no.sample <- search.df.nosource %>% filter(sample_accession == "")

length(no.sample$accession)
#[1] 879,246 sequence accessions do not have sample accession

(length(no.sample$accession)/length(search.df.nosource$accession))*100
#[1] 93.25872%

slices1 <- c(63557, 879246)
lbls1 <- c("sample accession", "no sample accession")
pct1 <- round(slices1/sum(slices1)*100)
lbls1 <- paste(lbls1, pct1) # add percents to labels
lbls1 <- paste(lbls1,"%",sep="") # add % to labels
pie(slices1,labels = lbls1, col=rainbow(length(lbls1)),
    main="Sample accessions found in the remaining 67% of sequence accessions")


#  Examine the accessions that do not have a source pointer and assess if they are taken from viruses and bacteria ---------------------------------
  
search.df.nosource <- read.csv(file = "D:/Research project_DISSCO/DISSCO R/ENAnosource.analysis.csv")

search.df.nosource[is.na(search.df.nosource)] <- "" 

#add a new column which will receive the value "TRUE" if the description has the term "virus" in it
search.df.nosource$virus <- grepl("virus", search.df.nosource$description)

length(search.df.nosource$virus[search.df.nosource$virus == TRUE])
# 484027 accessions have the term "virus" in the column "description"

#add a new column which will receive the value "TRUE" if the description has the term "bacter" in it
search.df.nosource$bacter <- grepl("bacter", search.df.nosource$description)

length(search.df.nosource$bacter[search.df.nosource$bacter == TRUE])
# 74033 accessions have the term "bacter" in the column "description"

#add a new column which will receive the value "TRUE" if the description has the term "coccus" in it
search.df.nosource$coccus <- grepl("coccus", search.df.nosource$description)

length(search.df.nosource$coccus[search.df.nosource$coccus == TRUE])
# 16284 accessions have the term "coccus" in the column "description"

#add a new column which will receive the value "TRUE" if the description has the term "cocci" in it
search.df.nosource$cocci <- grepl("cocci", search.df.nosource$description)

length(search.df.nosource$cocci[search.df.nosource$cocci == TRUE])
# 66 accessions have the term "cocci" in the column "description"

#add a new column which will receive the value "TRUE" if the description has the term "strain" in it
search.df.nosource$strain <- grepl("strain", search.df.nosource$description)

length(search.df.nosource$strain[search.df.nosource$strain == TRUE])
# 158567 accessions have the term "strain" in the column "description"

#add a new column which will receive the value "TRUE" if the description has the term "plasmid" in it
search.df.nosource$plasmid <- grepl("plasmid", search.df.nosource$description)

length(search.df.nosource$plasmid[search.df.nosource$plasmid == TRUE])
# 7247 accessions have the term "plasmid" in the column "description"

#How many accessions have the term "strain" in the column "description" but not the other terms (virus, bacterium etc.)?
strain <- search.df.nosource %>% filter(strain == TRUE & virus == FALSE & bacter == FALSE & coccus == FALSE & cocci == FALSE & plasmid == FALSE)

length(strain$accession)
# 84325 accessions have the term "strain" but not the rest of the terms

#How many accessions have the term "plasmid" in the column "description" but not the other terms (bacterium etc.)?
plasmid <- search.df.nosource %>% filter(plasmid == TRUE & strain == FALSE & virus == FALSE & bacter == FALSE & coccus == FALSE & cocci == FALSE)

length(plasmid$accession)
# 549 accessions have the term "plasmid" but not the rest of the terms


sum(484027, 74033, 16284, 66, 84325, 549)
# 659284 accessions have one of these terms in the column "description"

(659284/length(search.df.nosource$accession))*100
# at least 69.92808% of the accessions with no source modifiers refer to sequences coming from viruses, bacteria and in general, microorganisms

write.csv(search.df.nosource, file = "D:/Research project_DISSCO/DISSCO R/ENAnosource.analysis.csv", row.names = F)


#  Examine the data frame that contains sequences with pointers to the physical material and split into smaller data frames ---------------------------------
  
rm(list = ls(all=T))
setwd("D:/Research project_DISSCO/DISSCO R")
getwd()

library(tidyverse)

search.df.source <- read.csv(file = "D:/Research project_DISSCO/DISSCO R/ENAsource.analysis.csv")

#Some accessions seem to have more than one culture_collection or specimen_voucher qualifiers separated by ;.
#We will create three different data frames and analyse them separately.

#Check bio_material column first.
bio <- search.df.source %>% filter(str_detect(search.df.source$bio_material, ";"))
#bio_material column has no more than one qualifier in each cell

sourceB <- search.df.source %>% filter(str_detect(search.df.source$culture_collection, ";"))
length(sourceB$accession)
#[1] 46 accessions have more than one culture_collection qualifiers

sourceC <- search.df.source %>% filter(str_detect(search.df.source$specimen_voucher, ";"))
length(sourceC$accession)
#[1] 123 accessions have more than one specimen_voucher qualifiers

sourceA <- search.df.source %>%
  filter(
    !str_detect(culture_collection, ";")
)

sourceA <- sourceA %>%
  filter(
    !str_detect(specimen_voucher, ";")
)

length(sourceA$accession)
#[1] 461,543 accessions have one qualifier in each source pointer column


#We will take a look at sourceA data frame.

sourceA %>%
  filter(bio_material!="" & culture_collection!="" & specimen_voucher!="")
#0 obs.

sourceA %>%
  filter(bio_material!="" & culture_collection!="")
#0 obs.

sourceA.A <- sourceA %>%
  filter(bio_material!="" & specimen_voucher!="")
#450 obs. with bio_material and specimen_voucher

sourceA.B <- sourceA %>%
  filter(culture_collection!="" & specimen_voucher!="")
#634 obs. with culture_collection and specimen_voucher

sourceA.C <- sourceA %>%
  filter(!(bio_material!="" & specimen_voucher!=""))

sourceA.C <- sourceA.C %>%
  filter(!(culture_collection!="" & specimen_voucher!=""))
#460,459 obs. with either bio_material, culture_collection or specimen_voucher

#sourceA data frame was split into three data frames, sourceA.A, sourceA.B and sourceA.C.   


#  First, we will work with sourceA data frames. Split columns with source pointers to three columns; institution_code, collection_code and material/culture/specimen_id ---------------------------------
#  sourceA.A data frame =================================

ENA.dfA.A.codes <- sourceA.A %>% add_column(material_id = NA, .after = "bio_material") #add a new column 

ENA.dfA.A.codes$material_id[is.na(ENA.dfA.A.codes$material_id)] <- as.character(ENA.dfA.A.codes$bio_material[is.na(ENA.dfA.A.codes$material_id)]) #replace NA values in column "material_id" with the values in the adjacent column "bio_material"

ENA.dfA.A.codes$material_id = sub(".*:", "", as.character(ENA.dfA.A.codes$material_id)) #remove part of string before last ":"

#material_id column ready


ENA.dfA.A.codes <- ENA.dfA.A.codes %>% add_column(specimen_id = NA, .after = "specimen_voucher") #add a new column 

ENA.dfA.A.codes$specimen_id[is.na(ENA.dfA.A.codes$specimen_id)] <- as.character(ENA.dfA.A.codes$specimen_voucher[is.na(ENA.dfA.A.codes$specimen_id)]) #replace NA values in column "specimen_id" with the values in the adjacent column "specimen_voucher"

ENA.dfA.A.codes$specimen_id = sub(".*:", "", as.character(ENA.dfA.A.codes$specimen_id)) #remove part of string before last ":"

#specimen_id column ready


ENA.dfA.A.codes <- ENA.dfA.A.codes %>% add_column(collection_codeA = NA, .after = "bio_material") #create a new column

ENA.dfA.A.codes$collection_codeA[is.na(ENA.dfA.A.codes$collection_codeA)] <- as.character(ENA.dfA.A.codes$bio_material[is.na(ENA.dfA.A.codes$collection_codeA)]) #replace NA values in column "collection_codeA" with the values in the column "bio_material"

#replace values that do not contain "text:text:text" with empty cells.

ENA.dfA.A.codes <- ENA.dfA.A.codes %>%
  mutate(collection_codeA = case_when(
    !str_detect(collection_codeA, ".*:(.*)\\:.*") ~ "",
    TRUE ~ collection_codeA
  )
)

ENA.dfA.A.codes$collection_codeA = sub("^[^:]*:", "", as.character(ENA.dfA.A.codes$collection_codeA)) #remove part of string before first ":"

ENA.dfA.A.codes$collection_codeA = sub("\\:[^:]*$", "", as.character(ENA.dfA.A.codes$collection_codeA)) #remove part of string after last ":"

#collection_codeA column ready


ENA.dfA.A.codes <- ENA.dfA.A.codes %>% add_column(collection_codeB = NA, .after = "specimen_voucher") #create a new column

ENA.dfA.A.codes$collection_codeB[is.na(ENA.dfA.A.codes$collection_codeB)] <- as.character(ENA.dfA.A.codes$specimen_voucher[is.na(ENA.dfA.A.codes$collection_codeB)]) #replace NA values in column "collection_codeB" with the values in the column "specimen_voucher"

#replace values that do not contain "text:text:text" with empty cells.

ENA.dfA.A.codes <- ENA.dfA.A.codes %>%
  mutate(collection_codeB = case_when(
    !str_detect(collection_codeB, ".*:(.*)\\:.*") ~ "",
    TRUE ~ collection_codeB
  )
)

ENA.dfA.A.codes$collection_codeB = sub("^[^:]*:", "", as.character(ENA.dfA.A.codes$collection_codeB)) #remove part of string before first ":"

ENA.dfA.A.codes$collection_codeB = sub("\\:[^:]*$", "", as.character(ENA.dfA.A.codes$collection_codeB)) #remove part of string after last ":"

#collection_codeB column ready
  

ENA.dfA.A.codes <- ENA.dfA.A.codes %>% add_column(institution_codeA = NA, .after = "bio_material") #create a new column

ENA.dfA.A.codes$institution_codeA[is.na(ENA.dfA.A.codes$institution_codeA)] <- as.character(ENA.dfA.A.codes$bio_material[is.na(ENA.dfA.A.codes$institution_codeA)]) #replace NA values in column "institution_codeA" with the values in the column "bio_material"

#replace values that do not contain ":" with empty cells.

ENA.dfA.A.codes <- ENA.dfA.A.codes %>%
  mutate(institution_codeA = case_when(
    !str_detect(institution_codeA, ":") ~ "",
    TRUE ~ institution_codeA
  )
)

ENA.dfA.A.codes$institution_codeA = sub("\\:[^:]*", "", as.character(ENA.dfA.A.codes$institution_codeA)) #remove part of string after ":"
ENA.dfA.A.codes$institution_codeA = sub("\\:[^:]*", "", as.character(ENA.dfA.A.codes$institution_codeA)) #remove part of string after ":"

#institution_codeA column ready


ENA.dfA.A.codes <- ENA.dfA.A.codes %>% add_column(institution_codeB = NA, .after = "specimen_voucher") #create a new column

ENA.dfA.A.codes$institution_codeB[is.na(ENA.dfA.A.codes$institution_codeB)] <- as.character(ENA.dfA.A.codes$specimen_voucher[is.na(ENA.dfA.A.codes$institution_codeB)]) #replace NA values in column "institution_codeB" with the values in the column "specimen_voucher"

#replace values that do not contain ":" with empty cells.

ENA.dfA.A.codes <- ENA.dfA.A.codes %>%
  mutate(institution_codeB = case_when(
    !str_detect(institution_codeB, ":") ~ "",
    TRUE ~ institution_codeB
  )
)

ENA.dfA.A.codes$institution_codeB = sub("\\:[^:]*", "", as.character(ENA.dfA.A.codes$institution_codeB)) #remove part of string after ":"
ENA.dfA.A.codes$institution_codeB = sub("\\:[^:]*", "", as.character(ENA.dfA.A.codes$institution_codeB)) #remove part of string after ":"

#institution_codeB column ready

#rearrange columns in ENA.dfA.C.codes

ENA.dfA.A.codes <- ENA.dfA.A.codes[, c(1, 2, 3, 4, 5, 6, 10, 11, 7, 8, 9, 12, 13, 14, 15, 16)]

write.csv(ENA.dfA.A.codes, file = "D:/Research project_DISSCO/DISSCO R/ENA.dfA.A.codes.csv", row.names = F)


#  sourceA.B data frame =================================

ENA.dfA.B.codes <- sourceA.B %>% add_column(culture_id = NA, .after = "culture_collection") #add a new column 

ENA.dfA.B.codes$culture_id[is.na(ENA.dfA.B.codes$culture_id)] <- as.character(ENA.dfA.B.codes$culture_collection[is.na(ENA.dfA.B.codes$culture_id)]) #replace NA values in column "culture_id" with the values in the adjacent column "culture_collection"

ENA.dfA.B.codes$culture_id = sub(".*:", "", as.character(ENA.dfA.B.codes$culture_id)) #remove part of string before last ":"

#culture_id column ready


ENA.dfA.B.codes <- ENA.dfA.B.codes %>% add_column(specimen_id = NA, .after = "specimen_voucher") #add a new column 

ENA.dfA.B.codes$specimen_id[is.na(ENA.dfA.B.codes$specimen_id)] <- as.character(ENA.dfA.B.codes$specimen_voucher[is.na(ENA.dfA.B.codes$specimen_id)]) #replace NA values in column "specimen_id" with the values in the adjacent column "specimen_voucher"

ENA.dfA.B.codes$specimen_id = sub(".*:", "", as.character(ENA.dfA.B.codes$specimen_id)) #remove part of string before last ":"

#specimen_id column ready


ENA.dfA.B.codes <- ENA.dfA.B.codes %>% add_column(institution_codeA = NA, .after = "culture_collection") #create a new column

ENA.dfA.B.codes$institution_codeA[is.na(ENA.dfA.B.codes$institution_codeA)] <- as.character(ENA.dfA.B.codes$culture_collection[is.na(ENA.dfA.B.codes$institution_codeA)]) #replace NA values in column "institution_codeA" with the values in the column "culture_collection"

#replace values that do not contain ":" with empty cells.

ENA.dfA.B.codes <- ENA.dfA.B.codes %>%
  mutate(institution_codeA = case_when(
    !str_detect(institution_codeA, ":") ~ "",
    TRUE ~ institution_codeA
  )
)

ENA.dfA.B.codes$institution_codeA = sub("\\:[^:]*", "", as.character(ENA.dfA.B.codes$institution_codeA)) #remove part of string after ":"

#institution_codeA column ready


ENA.dfA.B.codes <- ENA.dfA.B.codes %>% add_column(institution_codeB = NA, .after = "specimen_voucher") #create a new column

ENA.dfA.B.codes$institution_codeB[is.na(ENA.dfA.B.codes$institution_codeB)] <- as.character(ENA.dfA.B.codes$specimen_voucher[is.na(ENA.dfA.B.codes$institution_codeB)]) #replace NA values in column "institution_codeB" with the values in the column "specimen_voucher"

#replace values that do not contain ":" with empty cells.

ENA.dfA.B.codes <- ENA.dfA.B.codes %>%
  mutate(institution_codeB = case_when(
    !str_detect(institution_codeB, ":") ~ "",
    TRUE ~ institution_codeB
  )
)

ENA.dfA.B.codes$institution_codeB = sub("\\:[^:]*", "", as.character(ENA.dfA.B.codes$institution_codeB)) #remove part of string after ":"

#institution_codeB column ready


#rearrange columns in ENA.dfA.B.codes

ENA.dfA.B.codes <- ENA.dfA.B.codes[, c(1, 2, 3, 4, 5, 6, 7, 10, 8, 9, 11, 12, 13, 14)]

write.csv(ENA.dfA.B.codes, file = "D:/Research project_DISSCO/DISSCO R/ENA.dfA.B.codes.csv", row.names = F)


#  sourceA.C data frame =================================
  
ENA.dfA.C <- unite(sourceA.C, source_identifier, c("bio_material", "culture_collection", "specimen_voucher"), sep = "", remove = FALSE)

ENA.dfA.C.codes <- ENA.dfA.C %>% add_column(material_culture_or_specimen_id  = NA, .after = "source_identifier") #create a new data frame by adding a new column 

ENA.dfA.C.codes$material_culture_or_specimen_id[is.na(ENA.dfA.C.codes$material_culture_or_specimen_id)] <- as.character(ENA.dfA.C.codes$source_identifier[is.na(ENA.dfA.C.codes$material_culture_or_specimen_id)]) #replace NA values in column "material_culture_or_specimen_id" with the values in the adjacent column "source_pointer"

ENA.dfA.C.codes$material_culture_or_specimen_id = sub(".*:", "", as.character(ENA.dfA.C.codes$material_culture_or_specimen_id)) #remove part of string before last ":"

#material_culture_or_specimen_id column ready


ENA.dfA.C.codes <- ENA.dfA.C.codes %>% add_column(collection_code = NA, .after = "source_identifier") #create a new column

ENA.dfA.C.codes$collection_code[is.na(ENA.dfA.C.codes$collection_code)] <- as.character(ENA.dfA.C.codes$source_identifier[is.na(ENA.dfA.C.codes$collection_code)]) #replace NA values in column "collection_code" with the values in the column "source_pointer"

#replace values that do not contain "text:text:text" with empty cells.

ENA.dfA.C.codes <- ENA.dfA.C.codes %>%
  mutate(collection_code = case_when(
    !str_detect(collection_code, ".*:(.*)\\:.*") ~ "",
    TRUE ~ collection_code
  )
)

ENA.dfA.C.codes$collection_code = sub("^[^:]*:", "", as.character(ENA.dfA.C.codes$collection_code)) #remove part of string before first ":"

ENA.dfA.C.codes$collection_code = sub("\\:[^:]*$", "", as.character(ENA.dfA.C.codes$collection_code)) #remove part of string after last ":"

#collection_code column ready


ENA.dfA.C.codes <- ENA.dfA.C.codes %>% add_column(institution_code = NA, .after = "source_identifier") #create a new column

ENA.dfA.C.codes$institution_code[is.na(ENA.dfA.C.codes$institution_code)] <- as.character(ENA.dfA.C.codes$source_identifier[is.na(ENA.dfA.C.codes$institution_code)]) #replace NA values in column "institution_code" with the values in the column "source_pointer"

#replace values that do not contain ":" with empty cells. If the text that doesn't contain ":" is found, it will return TRUE, or else it will return FALSE.

ENA.dfA.C.codes <- ENA.dfA.C.codes %>%
  mutate(institution_code = case_when(
    !str_detect(institution_code, ":") ~ "",
    TRUE ~ institution_code
  )
)

ENA.dfA.C.codes$institution_code = sub("\\:[^:]*", "", as.character(ENA.dfA.C.codes$institution_code)) #remove part of string after first ":"

ENA.dfA.C.codes$institution_code = sub("\\:[^:]*$", "", as.character(ENA.dfA.C.codes$institution_code)) #remove part of string after last ":"

#institution_code column ready


#rearrange columns in ENA.dfA.C.codes

ENA.dfA.C.codes <- ENA.dfA.C.codes[, c(1, 2, 3, 4, 5, 10, 11, 12, 6, 7, 8, 9, 13, 14)]

write.csv(ENA.dfA.C.codes, file = "D:/Research project_DISSCO/DISSCO R/ENA.dfA.C.codes.csv", row.names = F)


#  Next we will work on sourceB and sourceC data frames ---------------------------------
#  sourceB data frame =================================
  
ENA.dfB <- unite(sourceB, source_identifier, c("bio_material", "culture_collection", "specimen_voucher"), sep = "", remove = FALSE)

#rearrange columns in ENA.dfB

ENA.dfB <- ENA.dfB[, c(1, 2, 3, 4, 5, 7, 8, 9, 6, 10, 11)]

ENA.dfB.codes <- separate(data = ENA.dfB, col = source_identifier, into = c("culture_collectionA", "culture_collectionB"), sep = ";", remove = FALSE)
#In this case we can clearly say that both culture_collection qualifiers of the 46 accessions follow the INSDC format /culture_collection=institution-code:specimen_id.   

ENA.dfB.codes <- separate(data = ENA.dfB.codes, col = culture_collectionA, into = c("institution_codeA", "culture_idA"), sep = ":", remove = FALSE)

ENA.dfB.codes <- separate(data = ENA.dfB.codes, col = culture_collectionB, into = c("institution_codeB", "culture_idB"), sep = ":", remove = FALSE)

ENA.dfB.codes[is.na(ENA.dfB.codes)] <- "" 

write.csv(ENA.dfB.codes, file = "D:/Research project_DISSCO/DISSCO R/ENA.dfB.codes.csv", row.names = F)


#  sourceC data frame =================================
  
ENA.dfC <- unite(sourceC, source_identifier, c("bio_material", "culture_collection", "specimen_voucher"), sep = "", remove = FALSE)

#rearrange columns in ENA.dfC

ENA.dfC <- ENA.dfC[, c(1, 2, 3, 4, 5, 7, 8, 9, 6, 10, 11)]

ENA.dfC.codes <- separate(data = ENA.dfC, col = specimen_voucher, into = c("specimen_voucherA", "specimen_voucherB"), sep = ";", remove = FALSE)
#Warning message:
#  Expected 2 pieces. Additional pieces discarded in 2 rows [5, 106]. 

#Examine these 2 cells [5, 106]. They have 3 different specimen_vouchers.
ENA.dfC[5,8]
#[1] "NH GF01;NH GF02;NH GF03"

ENA.dfC[106,8]
#[1] "Adavi2016;AJ;Li-Shaanxi20150628"

#rearrange columns in ENA.dfC.codes

ENA.dfC.codes <- ENA.dfC.codes[, c(1, 2, 3, 4, 5, 6, 7, 8, 11, 9, 10, 12, 13)]

ENA.dfC.codes <- ENA.dfC.codes %>% add_column(specimen_voucherC = NA, .after = "specimen_voucherB") #create a new column 

ENA.dfC.codes[5,12] <- "NH GF03"

ENA.dfC.codes[106,12] <- "Li-Shaanxi20150628"

ENA.dfC.codes[is.na(ENA.dfC.codes)] <- ""  

ENA.dfC.codes <- ENA.dfC.codes %>% add_column(specimen_idA = NA, .after = "specimen_voucherA") #add a new column 

ENA.dfC.codes$specimen_idA[is.na(ENA.dfC.codes$specimen_idA)] <- as.character(ENA.dfC.codes$specimen_voucherA[is.na(ENA.dfC.codes$specimen_idA)]) #replace NA values in column "specimen_idA" with the values in the adjacent column "specimen_voucherA"

ENA.dfC.codes$specimen_idA = sub(".*:", "", as.character(ENA.dfC.codes$specimen_idA)) #remove part of string before last ":"

#specimen_idA column ready


ENA.dfC.codes <- ENA.dfC.codes %>% add_column(institution_codeA = NA, .after = "specimen_voucherA") #create a new column

ENA.dfC.codes$institution_codeA[is.na(ENA.dfC.codes$institution_codeA)] <- as.character(ENA.dfC.codes$specimen_voucherA[is.na(ENA.dfC.codes$institution_codeA)]) #replace NA values in column "institution_codeA" with the values in the column "specimen_voucherA"

#replace values that do not contain ":" with empty cells.

ENA.dfC.codes <- ENA.dfC.codes %>%
  mutate(institution_codeA = case_when(
    !str_detect(institution_codeA, ":") ~ "",
    TRUE ~ institution_codeA
  )
)

ENA.dfC.codes$institution_codeA = sub("\\:[^:]*", "", as.character(ENA.dfC.codes$institution_codeA)) #remove part of string after ":"

#institution_codeA column ready


ENA.dfC.codes <- ENA.dfC.codes %>% add_column(specimen_idB = NA, .after = "specimen_voucherB") #add a new column 

ENA.dfC.codes$specimen_idB[is.na(ENA.dfC.codes$specimen_idB)] <- as.character(ENA.dfC.codes$specimen_voucherB[is.na(ENA.dfC.codes$specimen_idB)]) #replace NA values in column "specimen_idB" with the values in the adjacent column "specimen_voucherB"

ENA.dfC.codes$specimen_idB = sub(".*:", "", as.character(ENA.dfC.codes$specimen_idB)) #remove part of string before ":"

#specimen_idB column ready


ENA.dfC.codes <- ENA.dfC.codes %>% add_column(institution_codeB = NA, .after = "specimen_voucherB") #create a new column

ENA.dfC.codes$institution_codeB[is.na(ENA.dfC.codes$institution_codeB)] <- as.character(ENA.dfC.codes$specimen_voucherB[is.na(ENA.dfC.codes$institution_codeB)]) #replace NA values in column "institution_codeB" with the values in the column "specimen_voucherB"

#replace values that do not contain ":" with empty cells.

ENA.dfC.codes <- ENA.dfC.codes %>%
  mutate(institution_codeB = case_when(
    !str_detect(institution_codeB, ":") ~ "",
    TRUE ~ institution_codeB
  )
)

ENA.dfC.codes$institution_codeB = sub("\\:[^:]*", "", as.character(ENA.dfC.codes$institution_codeB)) #remove part of string after ":"

#institution_codeB column ready


ENA.dfC.codes <- ENA.dfC.codes %>% add_column(specimen_idC = NA, .after = "specimen_voucherC") #add a new column 

ENA.dfC.codes$specimen_idC[is.na(ENA.dfC.codes$specimen_idC)] <- as.character(ENA.dfC.codes$specimen_voucherC[is.na(ENA.dfC.codes$specimen_idC)]) #replace NA values in column "specimen_idC" with the values in the adjacent column "specimen_voucherC"

#specimen_idC column ready

write.csv(ENA.dfC.codes, file = "D:/Research project_DISSCO/DISSCO R/ENA.dfC.codes.csv", row.names = F)


#  Calculate percentages of sequence accessions with source identifiers that have one, two or three parts of the Darwin Core Triplet ---------------------------------

rm(list = ls(all=T))
setwd("D:/Research project_DISSCO/DISSCO R")
getwd()

library(tidyverse)

ENA.dfB.codes <- read.csv(file = "D:/Research project_DISSCO/DISSCO R/ENA.dfB.codes.csv")
ENA.dfC.codes <- read.csv(file = "D:/Research project_DISSCO/DISSCO R/ENA.dfC.codes.csv")

#ENA.dfB.codes: 46 accessions have two culture_collection identifiers each. Both have the format /culture_collection=institution-code:culture_id.
#ENA.dfC.codes: 123 accessions have two or three specimen_vouchers with formats /specimen_voucher=institution-code:specimen_id and 
#/specimen_voucher=specimen_id.

a <- ENA.dfC.codes %>%
        filter(specimen_voucherA!="" & specimen_voucherB!="" & specimen_voucherC!="")

length(a$accession)
#2 accessions have three specimen_vouchers, all have the format /specimen_voucher=specimen_id

# 1 accession have "-" delimiter in "specimen_idC" 

b <- ENA.dfC.codes %>%
        filter(specimen_voucherA!="" & specimen_voucherB!="" & specimen_voucherC=="")

length(b$accession)
#121 accessions have two specimen_vouchers

c <- b %>%
       filter(institution_codeA!="" & specimen_idA!="" & institution_codeB!="" & specimen_idB!="")

length(c$accession)
#19 accessions have two specimen_vouchers, both have the format /specimen_voucher=institution-code:specimen_id

d <- b %>%
       filter(institution_codeA!="" & specimen_idA!="" & institution_codeB=="" & specimen_idB!="")

length(d$accession)
#22 accessions have two specimen_vouchers, one with the format /specimen_voucher=institution-code:specimen_id 
#and one with the format /specimen_voucher=specimen_id

# Detect the presence of "-" delimiter in "specimen_id".

dash <- str_detect(d$specimen_idB, "-")

length(dash[dash == "TRUE"])
# 0 accessions have "-" delimiter in "specimen_id"


e <- b %>%
  filter(institution_codeA=="" & specimen_idA!="" & institution_codeB=="" & specimen_idB!="")

length(e$accession)
#80 accessions have two specimen_vouchers, both have the format /specimen_voucher=specimen_id

# Detect the presence of "-" delimiter in "specimen_idA" or "specimen_idB".

dash <- str_detect(e$specimen_idA, "-")

length(dash[dash == "TRUE"])
# 5 accessions have "-" delimiter in "specimen_idA"

# OR

dash <- str_detect(e$specimen_idB, "-")

length(dash[dash == "TRUE"])
# 6 accessions have "-" delimiter in "specimen_idB" 

# CONCLUSIONS: Data frame ENA.dfB.codes has 46 accessions with source identifier format = institution-code:specimen_id. 
# Data frame ENA.dfC.codes has 82 accessions with source identifier format = specimen_id (7 have the dash delimiter), 
# 19 accessions with source identifier format = institution-code:specimen_id and 22 accessions with source identifier 
# formats = institution-code:specimen_id and = specimen_id.



ENA.dfA.A.codes <- read.csv(file = "D:/Research project_DISSCO/DISSCO R/ENA.dfA.A.codes.csv")

df <- ENA.dfA.A.codes %>%
  filter(institution_codeA!="" & collection_codeA!="" & material_id!="" & institution_codeB!="" & collection_codeB!="" & specimen_id!="")

length(df$accession)
#38 accessions have bio_material and specimen_voucher qualifiers with the format = institution-code:collection_code:material_id/specimen_id 

df1 <- ENA.dfA.A.codes %>%
  filter(institution_codeA!="" & collection_codeA=="" & material_id!="" & institution_codeB!="" & collection_codeB=="" & specimen_id!="")

length(df1$accession)
#8 accessions have bio_material and specimen_voucher qualifiers with the format = institution-code:material_id/specimen_id

df2 <- ENA.dfA.A.codes %>%
  filter(institution_codeA=="" & collection_codeA=="" & material_id!="" & institution_codeB=="" & collection_codeB=="" & specimen_id!="")

length(df2$accession)
#383 accessions have bio_material and specimen_voucher qualifiers with the format = material_id/specimen_id

# Detect the presence of "-" delimiter in either "material_id" or "specimen_id".

dash <- str_detect(df2$material_id, "-")

length(dash[dash == "TRUE"])
# 258 accessions have "-" delimiter in "material_id" 

# OR

dash <- str_detect(df2$specimen_id, "-")

length(dash[dash == "TRUE"])
# 157 accessions have "-" delimiter in "specimen_id" 


df3 <- ENA.dfA.A.codes %>%
  filter(institution_codeA!="" & collection_codeA!="" & material_id!="" & institution_codeB!="" & collection_codeB=="" & specimen_id!="")

length(df3$accession)
#10 accessions have bio_material and specimen_voucher qualifiers with the formats = institution-code:collection_code:material_id/specimen_id and
# = institution-code:material_id/specimen_id

df4 <- ENA.dfA.A.codes %>%
  filter(institution_codeA!="" & collection_codeA=="" & material_id!="" & institution_codeB=="" & collection_codeB=="" & specimen_id!="")

length(df4$accession)
#10 accessions have bio_material and specimen_voucher qualifiers with the formats = institution-code:material_id/specimen_id and = material_id/specimen_id

# Detect the presence of "-" delimiter in "specimen_id".

dash <- str_detect(df4$specimen_id, "-")

length(dash[dash == "TRUE"])
# 4 accessions have "-" delimiter in "specimen_id" 


df5 <- ENA.dfA.A.codes %>%
  filter(institution_codeA=="" & collection_codeA=="" & material_id!="" & institution_codeB!="" & collection_codeB=="" & specimen_id!="")

length(df5$accession)
#1 accession has bio_material and specimen_voucher qualifiers with the formats = institution-code:material_id/specimen_id and = material_id/specimen_id 
#(11 in total)

# CONCLUSIONS: Data frame ENA.dfA.A.codes has 38 accessions with source identifier format = institution-code:collection-code:material_id/specimen_id, 
# 8 accessions with  = institution-code:material_id/specimen_id, 383 accessions with = material_id/specimen_id (258 have the dash delimiter), 10 accessions with 
# = institution-code:collection-code:material_id/specimen_id and = institution-code:material_id/specimen_id and 11 accessions with 
# = institution-code:material_id/specimen_id and = material_id/specimen_id (4 have the dash delimiter).


ENA.dfA.B.codes <- read.csv(file = "D:/Research project_DISSCO/DISSCO R/ENA.dfA.B.codes.csv")

df <- ENA.dfA.B.codes %>%
  filter(institution_codeA!="" & culture_id!="" & institution_codeB!="" & specimen_id!="")

length(df$accession)
# 411 accessions have culture_collection and specimen_voucher qualifiers with the format = institution-code:culture_id/specimen_id

df1 <- ENA.dfA.B.codes %>%
  filter(institution_codeA!="" & culture_id!="" & institution_codeB=="" & specimen_id!="")

length(df1$accession)
# 223 accessions have culture_collection and specimen_voucher qualifiers with the formats = institution-code:culture_id
#and = specimen_id

# Detect the presence of "-" delimiter in "specimen_id".

dash <- str_detect(df1$specimen_id, "-")

length(dash[dash == "TRUE"])
# 58 accessions have "-" delimiter in "specimen_id" 


# CONCLUSIONS: Data frame ENA.dfA.B.codes has 411 accessions with source identifier format = institution-code:culture_id/specimen_id  
# and 223 accessions with source identifier formats = institution-code:culture_id and = specimen_id (58 have the dash delimiter). 


ENA.dfA.C.codes <- read.csv(file = "D:/Research project_DISSCO/DISSCO R/ENA.dfA.C.codes.csv")

df <- ENA.dfA.C.codes %>%
  filter(institution_code!="" & collection_code!="" & material_culture_or_specimen_id!="")

length(df$accession)
# 4476 accessions have source identifiers with the format = institution-code:collection-code:material_culture_or_specimen_id

df1 <- ENA.dfA.C.codes %>%
  filter(institution_code!="" & collection_code=="" & material_culture_or_specimen_id!="")

length(df1$accession)
# 17953 accessions have source identifiers with the format = institution-code:material_culture_or_specimen_id

df2 <- ENA.dfA.C.codes %>%
  filter(institution_code=="" & collection_code=="" & material_culture_or_specimen_id!="")

length(df2$accession)
# 438030 accessions have source identifiers with the format = material_culture_or_specimen_id

# Detect the presence of "-" delimiter in "material_culture_or_specimen_id".

dash <- str_detect(df2$material_culture_or_specimen_id, "-")

length(dash[dash == "TRUE"])
# 176245 accessions have "-" delimiter in "material_culture_or_specimen_id" 


# CONCLUSIONS: Data frame ENA.dfA.C.codes has 4476 accessions with source identifier format = institution-code:collection-code:material_culture_or_specimen_id, 
# 17953 accessions with source identifier format = institution-code:material_culture_or_specimen_id and 438030 accessions with source identifier format = material_culture_or_specimen_id
# (176245 have the dash delimiter). 


#How many accessions coming from all data frames have source identifiers that correspond to the Darwin Core Triplet (institution_code, 
#collection_code and material/culture/specimen_id (=catalogNumber))? How many accessions have two or one part of the triplet?

search.df.source <- read.csv(file = "D:/Research project_DISSCO/DISSCO R/ENAsource.analysis.csv")

#Number of accessions with source identifier format that corresponds to the Darwin Core Triplet:
sum(38,4476)
#4514


#Number of accessions with source identifier format =institution_code:catalogNumber:
sum(46,19,8,411,17953)
#18437


#Number of accessions with source identifier format = catalogNumber:
sum(2,80,383,438030)
#438495

sum(4514,18437,438495)
# 461446 accessions studied have source identifier(s) with either Darwin Core Triplet, institution_code:catalogNumber or catalogNumber format.

461712-461446
# 266 accessions have more than one source identifier formats

sum(262,58,176245,7)
# 176572 accessions studied have source identifier format =catalogNumber that contains dash ("-") delimiter 

(176572/461712)*100
# 38.24289% of the accessions associated with source identifiers have the format =catalogNumber that contains dash ("-") delimiter  

#  Create tables with percentages and numbers of accessions =================================

x <- c("catalogNumber", "institution_code:catalogNumber", "DwCT", ">1 source identifier format")
y <- c(438495, 18437, 4514, 266)
x_name <- "identifier"
y_name <- "accessions.count"
df <- data.frame(x,y)
names(df) <- c(x_name,y_name)
df$percentage <- (df$accessions.count/461712)*100

print(df) #dataframe that includes the number of accessions that have one, two or three parts of the Darwin Core Triplet
           #as well as their percentages in the 461,712 sequence accessions studied

#                      identifier accessions.count    percentage
#1                  catalogNumber           438495   94.97154070
#2 institution_code:catalogNumber            18437    3.99318190
#3                           DwCT             4514    0.97766573
#4   >1 source identifier formats              266    0.05761167


x1 <- c("No source identifier", "catalogNumber", "institution_code:catalogNumber", "DwCT", ">1 source identifier formats")
y1 <- c(942803, 438495, 18437, 4514, 266)
x1_name <- "identifier"
y1_name <- "accessions.count"
df1 <- data.frame(x1,y1)
names(df1) <- c(x1_name,y1_name)
df1$percentage <- (df1$accessions.count/1404515)*100

print(df1) #dataframe that includes the number of accessions that have one, two or three parts of the Darwin Core Triplet
           #as well as their percentages in the 1,404,515 sequence accessions downloaded from the ENA database.

#                      identifier  accessions.count      percentage
#1           No source identifier            942803     67.12658818
#2                  catalogNumber            438495     31.22038568
#3 institution_code:catalogNumber             18437      1.31269513
#4                           DwCT              4514      0.32139208
#5   >1 source identifier formats               266      0.01893892



#  Search institution codes against ROR API using /organizations?query= (https://github.com/ror-community/ror-api) ---------------------------------
  
rm(list = ls(all=T))
setwd("D:/Research project_DISSCO/DISSCO R")
getwd()

library(httr)
library(jsonlite)
library(tidyverse)

ENA.dfA.A.codes <- read.csv(file = "D:/Research project_DISSCO/DISSCO R/ENA.dfA.A.codes.csv")
ENA.dfA.B.codes <- read.csv(file = "D:/Research project_DISSCO/DISSCO R/ENA.dfA.B.codes.csv")
ENA.dfA.C.codes <- read.csv(file = "D:/Research project_DISSCO/DISSCO R/ENA.dfA.C.codes.csv")
ENA.dfB.codes <- read.csv(file = "D:/Research project_DISSCO/DISSCO R/ENA.dfB.codes.csv")
ENA.dfC.codes <- read.csv(file = "D:/Research project_DISSCO/DISSCO R/ENA.dfC.codes.csv")


#Create a character vector of institution codes from all data frames.

ENA.dfA.A.codes$institution_codeA[ENA.dfA.A.codes$institution_codeA==""] <- NA
ENA.dfA.A.codes$institution_codeB[ENA.dfA.A.codes$institution_codeB==""] <- NA
ENA.dfA.B.codes$institution_codeA[ENA.dfA.B.codes$institution_codeA==""] <- NA
ENA.dfA.B.codes$institution_codeB[ENA.dfA.B.codes$institution_codeB==""] <- NA
ENA.dfA.C.codes$institution_code[ENA.dfA.C.codes$institution_code==""] <- NA
ENA.dfC.codes$institution_codeA[ENA.dfC.codes$institution_codeA==""] <- NA
ENA.dfC.codes$institution_codeB[ENA.dfC.codes$institution_codeB==""] <- NA

institution_codeALL <- c(ENA.dfA.A.codes$institution_codeA, ENA.dfA.A.codes$institution_codeB, ENA.dfA.B.codes$institution_codeA, ENA.dfA.B.codes$institution_codeB, ENA.dfA.C.codes$institution_code, ENA.dfB.codes$institution_codeA, ENA.dfB.codes$institution_codeB, ENA.dfC.codes$institution_codeA, ENA.dfC.codes$institution_codeB)

institution_codeALL <- unique(institution_codeALL)

institution_codeALL <- na.omit(institution_codeALL) #character vector with unique institution codes without NAs

length(institution_codeALL)
#481 unique institution codes

institution_codeALL

#[1] "NRM"           "B"             "ZFMK"          "RMRIMS"        "DB"            "USNM"          "IBIW"          "MFLU"          "RCC"          
#[10] "MFLUCC"        "CBS"           "ICMP"          "DAOMC"         "NIES"          "CCF"           "KZP"           "NK"            "NFCCI"        
#...

institution_codeTOT <- as.data.frame(institution_codeALL)

write.csv(institution_codeTOT, file = "D:/Research project_DISSCO/DISSCO R/institution_codeTOT.csv", row.names = F)

base_ROR = "https://api.ror.org/organizations?query=%s"

institution_code_RORurls <- sprintf(base_ROR, institution_codeALL)

institution_code_RORurls[1] #list of URLS

#[1] "https://api.ror.org/organizations?query=NRM"

URLS <- lapply(institution_code_RORurls, GET) #list of responses

URLS[1]

#[[1]]
#Response [https://api.ror.org/organizations?query=NRM]
#Date: 2020-07-12 15:35
#Status: 200
#Content-Type: application/json
#Size: 84 B

contents <- lapply(URLS, content, "text")

lol <- lapply(contents, fromJSON)

#Error: lexical error: invalid char in json text.
#<h1>Server Error (500)</h1>
#  (right here) ------^

length(contents[str_detect(contents, "Server Error")])
#4 institution code queries resulted in an error

grep("Server Error", contents)
# 21 397 408 445 positions

institution_codeALL[c(21, 397, 408, 445)]
# "CPC"           "BCCM/MUCL"     "BCCM/LMG"      "UOA/HCPF<GRC>"

#delete these elements
contents <- purrr::discard(contents,.p = ~stringr::str_detect(.x,"Server Error")) #list of 477 character vectors

lol <- lapply(contents, fromJSON)

#Error: lexical error: invalid char in json text.
#<!DOCTYPE HTML PUBLIC "-//W3C//
#                     (right here) ------^

length(contents[str_detect(contents, "-//W3C//")])
#1 institution code query resulted in an error

grep("-//W3C//", contents)
# 221 position 

institution_codeALL[221]
# "ZRC"

#delete these elements
contents <- purrr::discard(contents,.p = ~stringr::str_detect(.x,"-//W3C//")) #list of 476 character vectors

lol <- lapply(contents, fromJSON)

number_of_results <- sapply(lol, "[[", "number_of_results")

#5 institution codes, when queried, resulted in error
institution_code_error <- institution_codeALL[c(21, 221, 397, 408, 445)]

institution_code <- institution_codeALL[-c(21, 221, 397, 408, 445)]

institution_code_nores <- data.frame(institution_code, number_of_results) 
#data frame with institution codes queried and number of results per code

institution_code_nores[nrow(institution_code_nores) + 5,] <- NA

institution_code_nores$institution_code[is.na(institution_code_nores$institution_code)] <- institution_code_error

names(institution_code_nores)[1] <- "institution codes"
names(institution_code_nores)[2] <- "number of results"

institution_code_nores$`number of results`[is.na(institution_code_nores$`number of results`)] <- "" 

write.csv(institution_code_nores, file = "D:/Research project_DISSCO/DISSCO R/institution_code_nores.csv", row.names = F)

#5 institution codes had error

length(number_of_results[number_of_results==0])
#[1] 186 institution codes had 0 results

length(number_of_results[number_of_results==1])
#[1] 79 institution codes had 1 result

length(number_of_results[number_of_results==2])
#[1] 36 institution codes had 2 results

length(number_of_results[number_of_results>2])
#[1] 175 institution codes had more than 2 results


#Pie chart with percentages of institution codes that have 0, 1, 2 or more than 2 results in ROR API

slices <- c(5, 186, 79, 36, 175)
lbls <- c("Error", "0 results", "1 result", "2 results", ">2 results")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # add % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie chart of institution codes queried in ROR API")

institution_code_ROR <- sapply(lol, "[[", "items")

institution_code_ROR_df <- lapply(institution_code_ROR, as.data.frame) 
#list of data frames with results per institution code query


#  Search GRID for the institution codes of the studied accessions ---------------------------------

#Filter GRID.json data frame. Select rows based on the values in "acronyms" variable matching to the values in
#the "institution_codeALL" character vector.

GRID.json <- read_json("D:/Research project_DISSCO/grid-2020-03-15/grid.json", simplifyVector = TRUE)

class(GRID.json)
#[1] "list"

class(GRID.json$institutes)
#[1] "data.frame"

GRID.json.match <- filter(GRID.json$institutes, acronyms %in% institution_codeALL) #585 obs.

class(GRID.json.match$acronyms)
#[1] "list"

GRID.json.match$acronyms <- unlist(GRID.json.match$acronyms) #unlist column "acronyms"

class(GRID.json.match$acronyms)
#[1] "character"

nores_GRID_json <- GRID.json.match %>% count(acronyms)
#data frame with institution codes queried in GRID.json and number of results per code

length(nores_GRID_json$acronyms)
#183 from a total of 481 institution codes that were queried in GRID.json gave back results

(length(nores_GRID_json$acronyms)/length(institution_codeALL))*100
#[1] 38.04574% of the 481 institution codes queried in GRID.json gave back results


#298 institution codes had 0 results

length(nores_GRID_json$n[nores_GRID_json$n==1])
#[1] 80 institution codes had 1 result

length(nores_GRID_json$n[nores_GRID_json$n==2])
#[1] 34 institution codes had 2 results

length(nores_GRID_json$n[nores_GRID_json$n>2])
#[1] 69 institution codes had more than 2 results

#Pie chart with percentages of institution codes that have 0, 1, 2 or more than 2 matches in GRID.acronyms data frame

slices1 <- c(298, 80, 34, 69)
lbls1 <- c("0 results", "1 result", "2 results", ">2 results")
pct1 <- round(slices1/sum(slices1)*100)
lbls1 <- paste(lbls1, pct1) # add percents to labels
lbls1 <- paste(lbls1,"%",sep="") # add % to labels
pie(slices1,labels = lbls1, col=rainbow(length(lbls1)),
    main="Pie chart of institution codes queried in GRID database")

#create a character vector of institution codes with 0 results
institution_codes_zeroresults <- setdiff(institution_codeALL, nores_GRID_json$acronyms)

nores_GRID_json[nrow(nores_GRID_json) + 298,] <- NA

nores_GRID_json$acronyms[is.na(nores_GRID_json$acronyms)] <- institution_codes_zeroresults

nores_GRID_json$n[is.na(nores_GRID_json$n)] <- "" 

names(nores_GRID_json)[1] <- "institution codes" 
names(nores_GRID_json)[2] <- "number of results" 

write.csv(nores_GRID_json, file = "D:/Research project_DISSCO/DISSCO R/nores_GRID_json.csv", row.names = F)


#  Search against GBIF API (https://www.gbif.org/developer/registry) ---------------------------------

#  Institution codes =================================
  
base_GBIF = "https://api.gbif.org/v1/grscicoll/institution?q=%s"

institution_code_GBIFurls <- sprintf(base_GBIF, institution_codeALL)

institution_code_GBIFurls[[1]] #list of URLS

#[1] "https://api.gbif.org/v1/grscicoll/institution?q=NRM"

URLS1 <- lapply(institution_code_GBIFurls, GET) #list of responses

URLS1[[1]]

#Response [https://api.gbif.org/v1/grscicoll/institution?q=NRM]
#Date: 2020-07-12 16:43
#Status: 200
#Content-Type: application/json
#Size: 3.03 kB

contents1 <- lapply(URLS1, content, "text") #list of 56 character vectors

lol1 <- lapply(contents1, fromJSON)
#Error: lexical error: invalid char in json text.
#<!doctype html><html lang="en">
#  (right here) ------^


length(contents1[str_detect(contents1, "html lang")])
#57 institution code queries resulted in an error

grep("html lang", contents1)
#  30  58  70  72  73  84  88  93  99 105 111 119 131 132 146 148 157 158 170 171 180 191 193 195 196 197 199 200
# 202 209 212 216 239 253 269 290 308 317 318 322 335 337 339 352 354 359 415 431 442 445 450 454 470 476 479 480
# 481 positions

#delete this element
contents1 <- purrr::discard(contents1,.p = ~stringr::str_detect(.x,"html lang")) #list of 424 character vectors

lol1 <- lapply(contents1, fromJSON)

#Error: parse error: premature EOF

#(right here) ------^

contents1[190]
#[1] ""
#1 institution code query resulted in blank response

contents1 <- contents1[-190]

lol1 <- lapply(contents1, fromJSON) #list of 423 character vectors

number_of_results1 <- sapply(lol1, "[[", "count")

institution_code_nores1 <- data.frame(institution_codeALL[-c(30,  58, 70, 72, 73, 84, 88, 93, 99, 105, 111, 119, 131, 132, 146, 148, 157, 158, 170, 171, 180, 190, 191, 193, 195, 196, 197, 199, 200, 202, 209, 212, 216, 239, 253, 269, 290, 308, 317, 318, 322, 335, 337, 339, 352, 354, 359, 415, 431, 442, 445, 450, 454, 470, 476, 479, 480, 481)], number_of_results1) 
#data frame with institution codes queried and number of results per code

#58 institution codes, when queried, resulted in error
institution_code_error1 <- institution_codeALL[c(30,  58, 70, 72, 73, 84, 88, 93, 99, 105, 111, 119, 131, 132, 146, 148, 157, 158, 170, 171, 180, 190, 191, 193, 195, 196, 197, 199, 200, 202, 209, 212, 216, 239, 253, 269, 290, 308, 317, 318, 322, 335, 337, 339, 352, 354, 359, 415, 431, 442, 445, 450, 454, 470, 476, 479, 480, 481)]

institution_code_nores1[nrow(institution_code_nores1) + 58,] <- NA

names(institution_code_nores1)[1] <- "institution_codes"

institution_code_nores1$institution_codes[is.na(institution_code_nores1$institution_codes)] <- institution_code_error1

institution_code_nores1$number_of_results1[is.na(institution_code_nores1$number_of_results1)] <- "" 

names(institution_code_nores1)[1] <- "institution codes"
names(institution_code_nores1)[2] <- "number of results"

write.csv(institution_code_nores1, file = "D:/Research project_DISSCO/DISSCO R/institution_code_nores1.csv", row.names = F)


#58 institution codes had error

length(number_of_results1[number_of_results1==0])
#[1] 48 institution codes had 0 results

length(number_of_results1[number_of_results1==1])
#[1] 152 institution codes had 1 result

length(number_of_results1[number_of_results1==2])
#[1] 54 institution codes had 2 results

length(number_of_results1[number_of_results1>2])
#[1] 169 institution codes had more than 2 results


#Pie chart with percentages of institution codes that have 0, 1, 2 or more than 2 results in GBIF API

slices2 <- c(58, 48, 152, 54, 169)
lbls2 <- c("Error", "0 results", "1 result", "2 results", ">2 results")
pct2 <- round(slices2/sum(slices2)*100)
lbls2 <- paste(lbls2, pct2) # add percents to labels
lbls2 <- paste(lbls2,"%",sep="") # add % to labels
pie(slices2,labels = lbls2, col=rainbow(length(lbls2)),
    main="Pie chart of institution codes queried in GRSciColl database")

institution_code_GBIF <- sapply(lol1, "[[", "results")

institution_code_GBIF_df <- lapply(institution_code_GBIF, as.data.frame) 
#list of data frames with results per institution code query


#  Collection codes =================================
  
#Create a character vector of collection codes from all data frames.
  
ENA.dfA.A.codes$collection_codeA[ENA.dfA.A.codes$collection_codeA==""] <- NA
ENA.dfA.A.codes$collection_codeB[ENA.dfA.A.codes$collection_codeB==""] <- NA
ENA.dfA.C.codes$collection_code[ENA.dfA.C.codes$collection_code==""] <- NA

collection_code <- c(ENA.dfA.A.codes$collection_codeA, ENA.dfA.A.codes$collection_codeB, ENA.dfA.C.codes$collection_code)

collection_code <- unique(collection_code)

collection_code <- na.omit(collection_code) #character vector with unique collection codes without NAs

length(collection_code)
#104 unique collection codes

collection_codeTOT <- as.data.frame(collection_code)

write.csv(collection_codeTOT, file = "D:/Research project_DISSCO/DISSCO R/collection_codeTOT.csv", row.names = F)

base_GBIF1 = "https://api.gbif.org/v1/grscicoll/collection?q=%s"

collection_code_GBIFurls <- sprintf(base_GBIF1, collection_code)

collection_code_GBIFurls[[1]] #list of URLS

#[1] "https://api.gbif.org/v1/grscicoll/collection?q=DNA"

URLS2 <- lapply(collection_code_GBIFurls, GET) #list of responses

URLS2[[1]]

#Response [https://api.gbif.org/v1/grscicoll/collection?q=DNA]
#Date: 2020-07-12 18:24
#Status: 200
#Content-Type: application/json
#Size: 55.3 kB

contents2 <- lapply(URLS2, content, "text") #list of 104 character vectors

lol2 <- lapply(contents2, fromJSON)
#Error: parse error: premature EOF

#(right here) ------^


#extract elements with error

contents2 <- contents2[-c(2, 3, 8, 12, 13, 14, 16, 24, 26, 27, 31, 32, 33, 34, 42, 43, 44, 45, 46, 47, 49, 51, 53, 54, 56, 57, 60, 62, 64, 65, 70, 71, 72, 74, 77, 78, 79, 80, 84, 85, 87, 88, 90, 91, 92, 100, 102, 103, 104)]
#list of 55 character vectors

lol2 <- lapply(contents2, fromJSON)

number_of_results2 <- sapply(lol2, "[[", "count")

collection_code_nores <- data.frame(collection_code[-c(2, 3, 8, 12, 13, 14, 16, 24, 26, 27, 31, 32, 33, 34, 42, 43, 44, 45, 46, 47, 49, 51, 53, 54, 56, 57, 60, 62, 64, 65, 70, 71, 72, 74, 77, 78, 79, 80, 84, 85, 87, 88, 90, 91, 92, 100, 102, 103, 104)], number_of_results2) 
#data frame with collection codes queried and number of results per code

collection_code_nores[nrow(collection_code_nores) + 49,] <- NA

collection_code_error <- collection_code[c(2, 3, 8, 12, 13, 14, 16, 24, 26, 27, 31, 32, 33, 34, 42, 43, 44, 45, 46, 47, 49, 51, 53, 54, 56, 57, 60, 62, 64, 65, 70, 71, 72, 74, 77, 78, 79, 80, 84, 85, 87, 88, 90, 91, 92, 100, 102, 103, 104)]

names(collection_code_nores)[1] <- "collection_codes"

collection_code_nores$collection_codes[is.na(collection_code_nores$collection_codes)] <- collection_code_error

collection_code_nores$number_of_results2[is.na(collection_code_nores$number_of_results2)] <- "" 

names(collection_code_nores)[1] <- "collection codes"
names(collection_code_nores)[2] <- "number of results"

write.csv(collection_code_nores, file = "D:/Research project_DISSCO/DISSCO R/collection_code_nores.csv", row.names = F)


#49 institution codes had Error

length(number_of_results2[number_of_results2==0])
#[1] 18 collection codes had 0 results

length(number_of_results2[number_of_results2==1])
#[1] 6 collection codes had 1 result

length(number_of_results2[number_of_results2==2])
#[1] 2 collection codes had 2 results

length(number_of_results2[number_of_results2>2])
#[1] 29 institution codes had more than 2 results


#Pie chart with percentages of institution codes that have 0, 1, 2 or more than 2 results in GBIF API

slices3 <- c(49, 18, 6, 2, 29)
lbls3 <- c("Error", "0 results", "1 result", "2 results", ">2 results")
pct3 <- round(slices3/sum(slices3)*100)
lbls3 <- paste(lbls3, pct3) # add percents to labels
lbls3 <- paste(lbls3,"%",sep="") # add % to labels
pie(slices3,labels = lbls3, col=rainbow(length(lbls3)),
    main="Pie chart of collection codes queried in GRSciColl database")

collection_code_GBIF <- sapply(lol2, "[[", "results")

collection_code_GBIF_df <- lapply(collection_code_GBIF, as.data.frame) 
#list of data frames with results per collection code query

