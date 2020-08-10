
# Name: Niki Kyriakopoulou
# Master Research Project: Repair what is broken
# Date:2020

# Use case 2: Paper with title "Museomics for reconstructing historical floristic exchanges: Divergence of stone oaks across Wallacea" (Strijk et al., 2020)

rm(list = ls(all=T))
setwd("D:/Research project_DISSCO/DISSCO R")
getwd()

install.packages(c("httr", "urltools", "jsonlite", "tidyverse"))

#  Import datasets listing voucher info and collection data for Lithocarpus and Quercus species  ---------------------------------

# According to the authors, raw reads were deposited to the ENA database under project number PRJEB34850, 
# which is at the moment private and not accessible to the public. On the other hand, data on Quercus is available
# in ENA. The original dataset is available here: https://doi.org/10.1371/journal.pone.0232936.s001

library(httr)
library(jsonlite)
library(tidyverse)

# Accession list of Lithocarpus species
Lithocarpus.vouchers <- read.csv(file = 'Supplementary Table_Lithocarpus.csv')
names(Lithocarpus.vouchers)[1] <- "Species"

Lithocarpus.vouchers$Voucher.info

# "Chamchunroon 5181 (BKF), Chiang Mai, Thailand (2011)"   "Sirimongkol 424 (BKF), Khamphaengphet, Thailand (2012)"           
# "Price & Hernaez 745 (L), Samar, Philippines (1975)"     "Jacobs 8000 (L), Luzon, Philippines (1968)"  
# ...

length(Lithocarpus.vouchers$Voucher.info)
# 55 accessions

# After a preliminary examination of the dataset, we conclude that the Lithocarpus sequence accessions 
# (55 in total) have the INSDC /specimen voucher format =institution-code:specimen_id. 

# Quercus dataset
Quercus.vouchers <- read.csv(file = 'Supplementary Table_Quercus.csv')
names(Quercus.vouchers)[1] <- "Species"

# After examining the Quercus.vouchers dataset, the specimen vouchers also seem to conform to the ENA format
# /specimen_voucher=institution-code:specimen_id.


#  Query the ENA Portal API for the Quercus accessions  ---------------------------------

# create accession vector
accession <- Quercus.vouchers$Accession
class(accession) # character vector
accession

# "LC318796" "LC318516" "MF770291" "LC318778" "LC318498" "MF770277" "LC318777" "LC318497" "MF770276" "LC318789" "LC318509" "MF770284" "LC318800" "LC318520" "MF770294"
# "LC318801" "LC318521" "MF770295"   ""         "LC318774" "LC318494" "MF770273"

length(accession)
# 22 accessions - 1 since Quercus sp. is not linked to an accession

print(accession, quote=FALSE)

# LC318796 LC318516 MF770291 LC318778 LC318498 MF770277 LC318777 LC318497 MF770276 LC318789 LC318509 MF770284 LC318800
# LC318520 MF770294 LC318801 LC318521 MF770295 LC318774 LC318494 MF770273

# turn the character vector of values (accessions) into a comma separated vector

accession <- paste(accession, collapse=",")

print(accession, quote = FALSE)

#[1] LC318796,LC318516,MF770291,LC318778,LC318498,MF770277,LC318777,LC318497,MF770276,LC318789,LC318509,MF770284,LC318800,LC318520,MF770294,LC318801,LC318521,MF770295,,LC318774,LC318494,MF770273


base_portalurl = "https://www.ebi.ac.uk/ena/portal/api/search?"

full_portalurl = paste0(base_portalurl, "result=sequence_release",
                        "&fields=accession,scientific_name,bio_material,specimen_voucher",
                        "&includeAccessionType=sequence&includeAccessions=", accession,
                        "&format=json")

full_portalurl

# "https://www.ebi.ac.uk/ena/portal/api/search?result=sequence_release&fields=accession,scientific_name,bio_material,specimen_voucher&includeAccessionType=sequence&includeAccessions=LC318796,LC318516,MF770291,LC318778,LC318498,MF770277,LC318777,LC318497,MF770276,LC318789,LC318509,MF770284,LC318800,LC318520,MF770294,LC318801,LC318521,MF770295,,LC318774,LC318494,MF770273&format=json"

data <- GET(full_portalurl)

headers(data)$"content-type"

# "application/json;charset=UTF-8"

data_text <- content(data, as="text")

df <- fromJSON(data_text)


#  Split columns with source identifiers into two columns; institution_code and specimen_id  ---------------------------------

df <- df %>% add_column(specimen_id = NA, .after = "specimen_voucher") # add a new column 

df$specimen_id[is.na(df$specimen_id)] <- as.character(df$specimen_voucher[is.na(df$specimen_id)]) # replace NA values in column "specimen_id" with the values in the adjacent column "specimen_voucher"

df$specimen_id = sub(".*:", "", as.character(df$specimen_id)) # remove part of string before last ":"

# specimen_id column ready


df <- df %>% add_column(institution_code = NA, .after = "specimen_voucher") # create a new column

df$institution_code[is.na(df$institution_code)] <- as.character(df$specimen_voucher[is.na(df$institution_code)]) # replace NA values in column "institution_code" with the values in the column "specimen_voucher"

# replace values that do not contain ":" with empty cells.

df <- df %>%
  mutate(institution_code = case_when(
    !str_detect(institution_code, ":") ~ "",
    TRUE ~ institution_code
  )
)

df$institution_code = sub("\\:[^:]*", "", as.character(df$institution_code)) # remove part of string after ":"

# institution_code column ready


df[nrow(df)+1,] <- ""

Quercus.vouchersENA <- df[c(22, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21),]

Quercus.vouchersENA$scientific_name[1] <- "Quercus sp."

# remove bio_material column since its empty

Quercus.vouchersENA <- subset(Quercus.vouchersENA, select = -bio_material)

write.csv(Quercus.vouchersENA, file = 'Quercus.vouchersENA.csv', row.names = F)


Quercus.vouchers <- Quercus.vouchers[, c(5, 1, 2, 3, 4)]

Quercus.vouchers <- Quercus.vouchers[order(Quercus.vouchers$Accession),]

write.csv(Quercus.vouchers, file = 'Quercus.vouchers.csv', row.names = F)

# Comparing the two data frames Quercus.vouchers (from paper) and Quercus.vouchersENA we conclude that 
# the institution codes for the same accessions are different. According to the paper it's "KYO" while
# according to ENA it's	FU<JPN>. Also some accessions from ENA share the same specimen ID but not the
# institution code. Also, for Quercus sp. there is not a known accession that could be searched against
# ENA.

# CONCLUSIONS: All Lithocarpus and Quercus specimen vouchers from the suppementary file of the paper can 
# be resolved via their institution codes (=institution-code:specimen_id format). Quercus specimen vouchers from
# the ENA database have either the format =institution-code:specimen_id or just specimen_id. 


#  How many accessions have none, one, two, or three parts of the Darwin Core Triplet?  ---------------------------------

specimen_ID <- Quercus.vouchersENA %>%
  filter(institution_code=="" & specimen_id!="")

length(specimen_ID$accession)
# 7 sequence accession have only specimen_id.

DwCD <- Quercus.vouchersENA %>%
  filter(institution_code!="" & specimen_id!="")

length(DwCD$accession)
# 14 sequence accessions have institution_code and specimen_id.

# There are 0% sequence sequences with collection code ("Darwin Core Triplet").


slices <- c(7, 14)
lbls <- c("specimen_id", "institution_code:specimen_id")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # add % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Source identifier formats in the 21 Quercus sequence accessions studied")


#  Search institution codes against ROR API using /organizations?query= (https://github.com/ror-community/ror-api) ---------------------------------

rm(list = ls(all=T))
setwd("D:/Research project_DISSCO/DISSCO R")
getwd()

library(httr)
library(jsonlite)
library(tidyverse)

Quercus.vouchersENA <- read.csv(file = 'Quercus.vouchersENA.csv')

Quercus.vouchers <- read.csv(file = 'Quercus.vouchers.csv')
  
Lithocarpus.vouchers <- read.csv(file = 'Supplementary Table_Lithocarpus.csv')

  
l <- Lithocarpus.vouchers$Institution.codes
q <- Quercus.vouchers$Institution.code
q.ena <- Quercus.vouchersENA$institution_code
  
institution.codes <- c(l, q, q.ena)

institution.codes <- unique(institution.codes)

length(institution.codes)
# 8 unique institution codes

institution.codes
# "BKF"     "L"       "KYO"     "BGT"     "KAG"     "DLU, FU" ""        "FU<JPN>"

institution.codes <- institution.codes[!institution.codes == ""]

institution.codes <- unlist(strsplit(institution.codes, ","))

institution.codes
# "BKF"     "L"       "KYO"     "BGT"     "KAG"     "DLU"     " FU"     "FU<JPN>"

base.ROR = "https://api.ror.org/organizations?query=%s"

institution.code.urls <- sprintf(base.ROR, institution.codes) # list of URLS

institution.code.urls[1] 

# "https://api.ror.org/organizations?query=BKF"

URLS <- lapply(institution.code.urls, GET) # list of responses

URLS[1]

# [[1]]
# Response [https://api.ror.org/organizations?query=BKF]
# Date: 2020-07-01 16:19
# Status: 200
# Content-Type: application/json
# Size: 1.32 kB

contents <- lapply(URLS, content, "text")

lol <- lapply(contents, fromJSON) # list of lists

class(lol[[1]])
# "list"

lol[[1]]

# $number_of_results
#  2

# $time_taken
# 12

# ...

number.of.results <- sapply(lol, "[[", "number_of_results")

number.of.results

# 2 79  0  2  1  1 10 10

institution.nores <- data.frame(institution.codes, number.of.results) 
# data frame with institution codes queried and number of results per code

length(number.of.results[number.of.results==0])
# 1 institution code had 0 results

length(number.of.results[number.of.results==1])
# 2 institution codes had 1 result

length(number.of.results[number.of.results==2])
# 2 institution codes had 2 results

length(number.of.results[number.of.results>2])
# 3 institution codes had more than 2 results

# Pie chart with percentages of institution codes that have 0, 1, 2 or more than 2 results in ROR API

slices <- c(1, 2, 2, 3)
lbls <- c("0 results", "1 result", "2 results", ">2 results")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # add % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie chart of institution codes queried in ROR API")

institution.code.items <- sapply(lol, "[[", "items")

institution.code.items.df <- lapply(institution.code.items, as.data.frame) 
# list of data frames with results per institution code query


#  Search institution codes against GRID database ---------------------------------

GRID.json <- read_json('grid-2020-03-15/grid.json', simplifyVector = TRUE)

class(GRID.json)
# "list"

class(GRID.json$institutes)
# "data.frame"


# Filter GRID.json data frame. Select rows based on the values in "acronyms" variable matching to the values in
# the "institution_code" character vector.

GRID.json.match <- filter(GRID.json$institutes, acronyms %in% institution.codes)

class(GRID.json.match$acronyms)
# "list"

GRID.json.match$acronyms <- unlist(GRID.json.match$acronyms) # unlist column "acronyms"

class(GRID.json.match$acronyms)
# "character"

GRID.nores <- GRID.json.match %>% count(acronyms)
# data frame with institution codes queried in GRID.json and number of results per code

length(GRID.nores$acronyms)
# 3 from a total of 8 institution codes that were queried in GRID.json gave back results

(length(GRID.nores$acronyms)/length(institution.codes))*100
# 37.5% of the 8 institution codes queried in GRID.json gave back results


# 5 institution codes had 0 results

length(GRID.nores$n[GRID.nores$n==1])
# 3 institution codes had 1 result

length(GRID.nores$n[GRID.nores$n==2])
# 0 institution codes had 2 results

length(GRID.nores$n[GRID.nores$n>2])
# 0 institution codes had more than 2 results

# Pie chart with percentages of institution codes that have 0 or 1 match in GRID database

slices1 <- c(5, 3)
lbls1 <- c("0 results", "1 result")
pct1 <- round(slices1/sum(slices1)*100)
lbls1 <- paste(lbls1, pct1) # add percents to labels
lbls1 <- paste(lbls1,"%",sep="") # add % to labels
pie(slices1,labels = lbls1, col=rainbow(length(lbls1)),
    main="Pie chart of institution codes queried in GRID database")


#  Search institution codes against GBIF Registry of Scientific Collections API (https://www.gbif.org/developer/registry) ---------------------------------

base.GBIF = "https://api.gbif.org/v1/grscicoll/institution?q=%s"

institution.code.urls1 <- sprintf(base.GBIF, institution.codes) # list of URLS

institution.code.urls1[[1]] 

# "https://api.gbif.org/v1/grscicoll/institution?q=BKF"

URLS1 <- lapply(institution.code.urls1, GET) # list of responses

URLS1[[1]]

# Response [https://api.gbif.org/v1/grscicoll/institution?q=BKF]
# Date: 2020-07-07 08:07
# Status: 200
# Content-Type: application/json
# Size: 13.5 kB

contents1 <- lapply(URLS1, content, "text")

lol1 <- lapply(contents1, fromJSON)

# Error: parse error: premature EOF

#                      (right here) ------^

contents1[[7]]
# ""

contents1[[8]]
# "<!doctype html><html lang=\"en\"><head><title>HTTP Status 400  Bad Request</title><style type=\"text/css\">body {font-family:Tahoma,Arial,sans-serif;} h1, h2, h3, b {color:white;background-color:#525D76;} h1 {font-size:22px;} h2 {font-size:16px;} h3 {font-size:14px;} p {font-size:12px;} a {color:black;} .line {height:1px;background-color:#525D76;border:none;}</style></head><body><h1>HTTP Status 400  Bad Request</h1></body></html>


length(contents1[str_detect(contents1, "Status 400")])
# 1 institution code query resulted in response status code 400

(length(contents1[str_detect(contents1, "Status 400")])/length(institution.codes))*100
# 12.5% of the 8 institution code queries had status code 400

# delete elements from the list that contain "Status 400"
contents1 <- purrr::discard(contents1,.p = ~stringr::str_detect(.x,"Status 400")) #list of 50 character vectors

# delete 7th element since it is blank
contents1 <- contents1[-7]

# 1 institution code query resulted in a blank response

lol1 <- lapply(contents1, fromJSON)

class(lol1[[1]])
# "list"

lol1[[1]]

# $offset
# 0

# $limit
# 20

# $endOfRecords
# TRUE

# $count
# 1

# $results
# ...

number.of.results1 <- sapply(lol1, "[[", "count")

number.of.results1

# 1 2846   20    1    9    1

institution.codes1 <- institution.codes[-c(7, 8)]

institution.nores2 <- data.frame(institution.codes1, number.of.results1) 
# data frame with institution codes queried and number of results per code


# 1 institution code had Status Code 400
# 1 institution code had a blank response

length(number.of.results1[number.of.results1==0])
# 0 institution code had 0 results (BUT 1 HAD BLANK RESPONSE)

length(number.of.results1[number.of.results1==1])
# 3 institution codes had 1 result

length(number.of.results1[number.of.results1==2])
# 0 institution codes had 2 results

length(number.of.results1[number.of.results1>2])
# 3 institution codes had more than 2 results


# Pie chart with percentages of institution codes that have Status code 400 response, 0, 1 or more than 2 results in GBIF Registry of Scientific Collections

slices2 <- c(1, 1, 3, 3)
lbls2 <- c("Status code 400", "0 results", "1 result", ">2 results")
pct2 <- round(slices2/sum(slices2)*100)
lbls2 <- paste(lbls2, pct2) # add percents to labels
lbls2 <- paste(lbls2,"%",sep="") # add % to labels
pie(slices2,labels = lbls2, col=rainbow(length(lbls2)),
    main="Pie chart of institution codes queried in GRSciColl database")

institution.code.items1 <- sapply(lol1, "[[", "results")

institution.code.items1.df <- lapply(institution.code.items1, as.data.frame) 
# list of data frames with results per institution code query


#  Search institution codes against Index Herbariorum API (https://github.com/nybgvh/IH-API/wiki/GET-Institutions) ---------------------------------

base.INDEX = "http://sweetgum.nybg.org/science/api/v1/institutions/%s"

INDEX.urls <- sprintf(base.INDEX, institution.codes) # list of URLS

INDEX.urls[[1]] 

# "http://sweetgum.nybg.org/science/api/v1/institutions/BKF"

INDEX.response <- lapply(INDEX.urls, GET) # list of responses

INDEX.response[[1]]

# Response [http://sweetgum.nybg.org/science/api/v1/institutions/BKF]
# Date: 2020-07-08 17:35
# Status: 200
# Content-Type: application/json
# Size: 1.33 kB
  
INDEX.content <- lapply(INDEX.response, content, "text")

INDEX.content[7]

# "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>400 Bad Request</title>\n</head><body>\n<h1>Bad Request</h1>\n<p>Your browser sent a request that this server could not understand.<br />\n</p>\n</body></html>\n"

INDEX.content[8]

# "{\"hits\":0,\"code\":200,\"message\":\"No matching request found.\"}"


(length(INDEX.content[str_detect(INDEX.content, "400 Bad Request")])/length(institution.codes))*100
# 1 out of the 8 institution codes queries (12.5%) had "400 Bad Request" (institution code "FU")

(length(INDEX.content[str_detect(INDEX.content, "No matching request found.")])/length(institution.codes))*100
# 1 out of the 8 institution codes queries (12.5%) had no results (institution code "FU<JPN>")

# delete elements from the list that contain "400 Bad Request" and "No matching request found."
INDEX.content <- purrr::discard(INDEX.content,.p = ~stringr::str_detect(.x,"400 Bad Request"))
INDEX.content <- purrr::discard(INDEX.content,.p = ~stringr::str_detect(.x,"No matching request found.")) # list of 6 character vectors

INDEX.list <- lapply(INDEX.content, fromJSON) # list of 6 lists

organization <- unlist(sapply(INDEX.list, "[[", "organization"))

code <- sapply(INDEX.list, "[[", "code")

codes <- institution.codes[-c(7, 8)]

INDEX.df <- data.frame(codes, organization, code)

names(INDEX.df)[1] <- "query"
names(INDEX.df)[2] <- "organization"
names(INDEX.df)[3] <- "code"

INDEX.df[nrow(INDEX.df) + 1,] = c("FU","", "")
INDEX.df[nrow(INDEX.df) + 1,] = c("FU<JPN>","", "")

# 1 institution code had 400 Bad Request"
# 1 institution code had no results
# 6 institution codes had 1 result

# Pie chart with percentages of institution codes that have 400 Bad Request Response, 0 or 1 result in INDEX HERBARIORUM

slices3 <- c(1, 1, 6)
lbls3 <- c("400 Bad Request", "0 results", "1 result")
pct3 <- round(slices3/sum(slices3)*100)
lbls3 <- paste(lbls3, pct3) # add percents to labels
lbls3 <- paste(lbls3,"%",sep="") # add % to labels
pie(slices3,labels = lbls3, col=rainbow(length(lbls3)),
    main="Pie chart of institution codes queried in INDEX HERBARIORUM")



