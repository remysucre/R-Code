## authors: Professor Montgomery, Clara Wang and Regan Plekenpol
## Professor Brendan Nyhan & Professor Montgomery's Research Project
## program to download and merge files from http://www.policyfile.com/
## June 2015


## --------------------------------------------< PREPARE WORKSPACE >--------------------------------------------

rm(list = ls())

## Set my working directory so that this only needs to be changed once when we switch computers.
#myDir<- "~/Dropbox/Think tanks/PolicyFile/"  # Professor Montgomery's directory
#myDir <- "C:/Users/Clara/Dropbox/Think tanks/PolicyFile/"  # Clara's directory

wd <- "files"  # wd set to where the CSV files are
#wd  <- "TEST DATA"  # wd set to test dataset
setwd(paste0(myDir, wd))

##install.packages("plyr", "dplyr", "reshape2", "splitstackshape", "stringr")
library(plyr)
library(dplyr)
library(reshape2)
library(splitstackshape)
library(stringr)


## ------------------------------------------< LOAD AND MERGE DATA >-------------------------------------------

files <- dir()                                   # assign csv files in working directory to "files" object
bigdataset <- data.frame(coltitles = c("none"))  # create data frame for initial merge

## I am re-writing this to make it go faster.  R hates this code.
size <- length(files)
size

## A function that reads in one file and turns the output into a list
myReader <- function(i){
  temp <- try(read.csv(files[i], row.names = NULL, stringsAsFactors = FALSE, check.names = FALSE, header = TRUE))
  if(class(temp) == "try-error"){
    return(NULL)
  }
  num_docURL <- length(grep("Document", temp[,1], 1))  # number of Document URL variables
  # if there's more than one Document URL variable, rename them 
  if(num_docURL > 1){
    temp[grep("Document", temp[,1]), 1] <- c(rep(paste0("Document URL", 1:num_docURL), 1))
  }
  rownames(temp) <- temp[,1]  # set rownames
  temp <- temp[2]
  colnames(temp) <- files[i]  # set column names
  temp2 <- as.list(list(unlist(temp[,1]))[[1]])
  names(temp2) <- gsub(":", "", rownames(temp))
  print(i)
  return(temp2)
}

myReader(2)
myReader(1331)  # file with two Document URL variables

bigList <- llply(1:size, myReader) # make a big list where each element contains what we want

length(bigList)
allNames <- unique(unlist(llply(bigList, names))) # All possible field names
allNames

myOrganizer <- function(i){
  outDat <- rep(NA, length(allNames)) 
  names(outDat) <- allNames
  outDat[names(bigList[[i]])] <- unlist(bigList[[i]])
  return(outDat)
}

bigdataset <- ldply(1:size, myOrganizer)
rownames(bigdataset) <- gsub(".csv", "", files)


## save the new data frame as a CSV so in the future we can just load the CSV and not run the merge every time
write.csv(bigdataset, 
          file = paste0(myDir, "bigdataset.csv"),
          row.names = TRUE)


## -----------------------------------------------< CLEAN DATA >------------------------------------------------

bigdataset <- read.csv(file = paste0(myDir, "bigdataset.csv"))

## Merge duplicate columns, remove unecessary columns
## This code only works on the bigdataset that's read in as a .csv, doesn't work if use bigdataset
## created after simply running the R code
bigdataset <- bigdataset %>%
  mutate("publication_date" = paste0(Publication.date, Publication.date.1)) %>%
  mutate("publication_num" = paste0(Publication.number, Publication.number.1)) %>%
  mutate("publication_lang" = paste0(Publication.language, Publication.language.1)) %>%
  mutate("policyfile_record_date" = paste0(Policy.file.record.date, Policy.file.record.date.1, Policy.file.record.date.2)) %>%
  mutate("organization_type" = paste0(Organization.type.s., Organization.type.s..1)) %>%
  mutate("document_URL" = paste0(Document.URL, Document.URL.1, Document.URL1)) %>%
  mutate("publishing_body" = paste0(Publishing.body, Publishing.body.1)) %>%
  mutate("political_leaning" = paste0(Political.leaning, Political.leaning.1)) %>%
  select("policyfile_record_num" = X, 
         "title" = Title, 
         "organization" = Organization, 
         "telephone" = Telephone,
         "email" = Email,
         "homepage" = Homepage,
         "author.s" = Author.s.,
         "status" = Status,
         "subject.s" = Subject.s.,
         "abstract" = Abstract,
         document_URL,
         "document_URL2" = Document.URL2,
         "document_URL3" = Document.URL3,
         "document_URL4" = Document.URL4,
         "document_URL5" = Document.URL5,
         "document_URL6" = Document.URL6,
         "document_URL7" = Document.URL7,
         publishing_body,
         publication_date,
         publication_num,
         publication_lang,
         policyfile_record_date,
         organization_type, 
         political_leaning
         )

# Remove NAs
bigdataset <- as.data.frame(lapply(bigdataset, function(x) if(is.character(x)|is.factor(x)) gsub("NA", "", x) else x))

### NOTE: The rows here with nine missing values are the cases where there were duplicate field names in the original CSV.
#bigdataset$policyfile_record_num[(rowSums(is.na(bigdataset)))==9]


## save the new data frame as a .csv
write.csv(bigdataset, 
          file = paste0(myDir, "bigdataset_clean.csv"),
          row.names = TRUE)


## -----------------------------------------< CREATE MENU OF SUBJECTS >------------------------------------------

# use cleaned bigdataset.csv file 
bigdataset <- read.csv(file = paste0(myDir, "bigdataset_clean.csv"), stringsAsFactors = FALSE)

# menu of subjects
menu <- bigdataset %>%
  select(subject.s) %>%
  cSplit("subject.s", sep = ";", direction = "long")

menu <- as.data.frame(lapply(menu, function(x) if(is.character(x)|is.factor(x)) gsub("\n", " ", x) else x))  # remove extra lines

menu <- unique.data.frame(menu, incomparables = FALSE) %>%  # remove duplicate subjects
  arrange(subject.s)

## save menu as .csv file in working directory
#write.csv(menu, file = paste0(myDir, "subject_menu.csv"), row.names = FALSE)


# menu of organization_type or think tanks
menu2 <- bigdataset %>%
  select(organization_type, organization) %>%
  cSplit("organization", sep = ";", direction = "long")

menu2 <- as.data.frame(lapply(menu2, function(x) if(is.character(x)|is.factor(x)) gsub("\n", " ", x) else x))  # remove extra lines

menu2 <- unique.data.frame(menu2, incomparables = FALSE) %>%  # remove duplicate subjects
  arrange(organization)

## save menu2 as .csv file in working directory
#write.csv(menu2, file = paste0(myDir, "organization_menu.csv"), row.names = FALSE)


## -------------------------------------< SUBSET DATA TO AREAS OF INTEREST >-------------------------------------

## THIS CODE HAS NOT BEEN RUN YET, THIS IS FOR SUBSETTING THE DESIRED THINK TANKS AND SUBJECTS

# use cleaned bigdataset.csv file 
bigdataset <- read.csv(file = paste0(myDir, "bigdataset_clean.csv"), stringsAsFactors = FALSE)

## subset specific rows from data frame
subsetSubjects <- bigdataset %>%
  select(policyfile_record_num, subject.s, abstract) %>%
  cSplit("subject.s", sep = ";", direction = "long")
# need code here to filter for the desired subjects


## subset specific rows from data frame
subsetThinkTank <- bigdataset %>%
  select(policyfile_record_num, organization_type, organization, abstract) %>%
  cSplit("organization", sep = ";", direction = "long")
# need code here to filter for the desired think tanks/organizations

#----------------------------------------------< EXTRACT URLs >------------------------------------------------

## code to extract URLs
## FOLLOWING CODE ASSUMES THAT DATA HAS ALREADY BEEN SUBSET INTO AREAS OF INTEREST
## HOWEVER, HAVE NOT SUBSET FOR THINK TANKS OR SUBJECTS YET
## After subsetting bigdataset for the desired subjects/think tanks, should use the subsetted data set rather
## than bigdataset to run this code

# use cleaned bigdataset.csv file 
bigdataset <- read.csv(file = paste0(myDir, "bigdataset_clean.csv"), stringsAsFactors = FALSE)

extractFile <- bigdataset %>%
  select(document_URL, document_URL2, document_URL3, document_URL4, document_URL5,  # select columns with URLs and subject column
         document_URL6, document_URL7, policyfile_record_num, subject.s) %>%
  filter(document_URL != "") %>%  # remove files with no URLs (if document_URL is blank, all the other doc_URL columns will be blank too)
  cSplit("subject.s", sep = ";", direction = "long") %>%  # separate URLs by subject (creates rows with duplicate URLs), this was done because data had not been subset yet, can be removed
  select(-subject.s) %>%  # remove subject column
  unique.data.frame(incomparables = FALSE)  # remove rows with duplicate URLs

# create a list of URLs from document_URL column, add ".1" to end of policy file record num
extractDocOne <- sprintf("%08d.1..%s", extractFile$policyfile_record_num, extractFile$document_URL)  # creates string headed by policy file record num, followed by ".." followed by uncleaned URL
extractDocOne <- extractDocOne[unlist(!grepl("..NA", extractDocOne, fixed = TRUE))]                  # remove records with no URL
extractDocOne <- as.list(lapply(extractDocOne, function(x) if(is.character(x)) gsub("\n", "", x) else x))  # remove extra lines
extractDocOne <- as.list(lapply(extractDocOne, function(x) if(is.character(x)) gsub(" ", "", x) else x))   # remove extra spaces

# create a list of URLs from document_URL2 column, add ".2" to end of policy file record num
extractDocTwo <- sprintf("%08d.2..%s", extractFile$policyfile_record_num, extractFile$document_URL2)
extractDocTwo <- extractDocTwo[unlist(!grepl("..NA", extractDocTwo, fixed = TRUE))]
extractDocTwo <- as.list(lapply(extractDocTwo, function(x) if(is.character(x)) gsub("\n", "", x) else x))  # remove extra lines
extractDocTwo <- as.list(lapply(extractDocTwo, function(x) if(is.character(x)) gsub(" ", "", x) else x))   # remove extra spaces

# create a list of URLs from document_URL3 column, add ".3" to end of policy file record num
extractDocThree <- sprintf("%08d.3..%s", extractFile$policyfile_record_num, extractFile$document_URL3)
extractDocThree <- extractDocThree[unlist(!grepl("..NA", extractDocThree, fixed = TRUE))]
extractDocThree <- as.list(lapply(extractDocThree, function(x) if(is.character(x)) gsub("\n", "", x) else x))  # remove extra lines
extractDocThree <- as.list(lapply(extractDocThree, function(x) if(is.character(x)) gsub(" ", "", x) else x))   # remove extra spaces

# create a list of URLs from document_URL4 column, add ".4" to end of policy file record num
extractDocFour <- sprintf("%08d.4..%s", extractFile$policyfile_record_num, extractFile$document_URL4)
extractDocFour <- extractDocFour[unlist(!grepl("..NA", extractDocFour, fixed = TRUE))]
extractDocFour <- as.list(lapply(extractDocFour, function(x) if(is.character(x)) gsub("\n", "", x) else x))  # remove extra lines
extractDocFour <- as.list(lapply(extractDocFour, function(x) if(is.character(x)) gsub(" ", "", x) else x))   # remove extra spaces

# create a list of URLs from document_URL5 column, add ".5" to end of policy file record num
extractDocFive <- sprintf("%08d.5..%s", extractFile$policyfile_record_num, extractFile$document_URL5)
extractDocFive <- extractDocFive[unlist(!grepl("..NA", extractDocFive, fixed = TRUE))]
extractDocFive <- as.list(lapply(extractDocFive, function(x) if(is.character(x)) gsub("\n", "", x) else x))  # remove extra lines
extractDocFive <- as.list(lapply(extractDocFive, function(x) if(is.character(x)) gsub(" ", "", x) else x))   # remove extra spaces

# create a list of URLs from document_URL6 column, add ".6" to end of policy file record num
extractDocSix <- sprintf("%08d.6..%s", extractFile$policyfile_record_num, extractFile$document_URL6)
extractDocSix <- extractDocSix[unlist(!grepl("..NA", extractDocSix, fixed = TRUE))]
extractDocSix <- as.list(lapply(extractDocSix, function(x) if(is.character(x)) gsub("\n", "", x) else x))  # remove extra lines
extractDocSix <- as.list(lapply(extractDocSix, function(x) if(is.character(x)) gsub(" ", "", x) else x))   # remove extra spaces

# create a list of URLs from document_URL7 column, add ".7" to end of policy file record num
extractDocSeven <- sprintf("%08d.7..%s", extractFile$policyfile_record_num, extractFile$document_URL7)
extractDocSeven <- extractDocSeven[unlist(!grepl("..NA", extractDocSeven, fixed = TRUE))]
extractDocSeven <- as.list(lapply(extractDocSeven, function(x) if(is.character(x)) gsub("\n", "", x) else x))  # remove extra lines
extractDocSeven <- as.list(lapply(extractDocSeven, function(x) if(is.character(x)) gsub(" ", "", x) else x))   # remove extra spaces


# function to download files
downloadFiles <- function(i){
  recNum <- substr(i, 1, 10)            # identify the policyfile record number and which document URL column, set as recNum
  if(grepl("[PDF]", i, fixed = TRUE)){
    URL <- substr(i, 13, nchar(i) - 5)  # remove the [PDF] from the URL link, identify the URL link
  } else{
    URL <- substr(i, 13, nchar(i))
  } 
  # download PDFs
  if(grepl("pdf", URL, fixed = TRUE)){
    dir <- paste0(myDir, "PDFs and HTMLs/", recNum, ".pdf")  # set directory to save file in
    tempFile <- try(download.file(url = URL, destfile = dir, mode = "wb", quiet = TRUE))  # download file
    if(class(tempFile) == "try-error"){
      print(paste("ERROR", recNum, URL, i, sep = " | "))  # if download error, print all info
      return(NULL)
    } else{
      print(paste("SUCCESS", recNum, URL, sep = " | "))   # if download success, print record number and URL used
    }
  }
  # download HTMLs
  else{
    dir <- paste0(myDir, "PDFs and HTMLs/", recNum, ".html")  # set directory to save file in
    tempFile <- try(download.file(url = URL, destfile = dir, mode = "wb", quiet = TRUE))
    if(class(tempFile) == "try-error"){
      print(paste("ERROR", recNum, URL, i, sep = " | "))
      return(NULL)
    } else{
      print(paste("SUCCESS", recNum, URL, sep = " | "))
    }
  }
}


# run the downloadFiles function on all of the URL links
l_ply(extractDocOne, downloadFiles)    # download files using URLs from document_URL
l_ply(extractDocTwo, downloadFiles)    # download files using URLs from document_URL2
l_ply(extractDocThree, downloadFiles)  # download files using URLs from document_URL3
l_ply(extractDocFour, downloadFiles)   # download files using URLs from document_URL4
l_ply(extractDocFive, downloadFiles)   # download files using URLs from document_URL5
l_ply(extractDocSix, downloadFiles)    # download files using URLs from document_URL6
l_ply(extractDocSeven, downloadFiles)  # download files using URLs from document_URL7
