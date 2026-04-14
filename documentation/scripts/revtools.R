#see more on revtools.net or paper (westgate 2019)#

library (revtools)
library(openxlsx)

#SET WORKING DIRECTORY#

setwd("~/Documents/Cursos/Research_Synthesis/Challenge/")


#IMPORT: if you have data in different folders you can use these commands, if you only have one file you can import directly using read_bibliography#

path1 <- "./scopus/"
file_names1 <- paste0(path1, list.files(path = path1))
data1 <- read_bibliography(file_names1)

path2 <- "./WoS/"
file_names2 <- paste0(path2, list.files(path = path2))
data2 <- read_bibliography(file_names2)                         
                           
data <- merge_columns(data1, data2)

# REMOVE DUPLICATES BASED ON DOI: automatically#

doi_match <- find_duplicates(data,
                             match_variable = "doi",
                             group_variables = NA,
                             match_function = "exact")


data_unique <- extract_unique_references(data, doi_match)
write.csv(data_unique, "unique_references.csv")

revtools::write_bibliography(data_unique, 
                             "unique_references.bib", 
                             format = "bib")

# Check unique searches with Google
path3 <- "./PROQUEST/"
file_names3 <- paste0(path3, list.files(path = path3))
data3 <- read_bibliography(file_names3)

# Remove http from doi
data3$doi <- gsub("http://dx.doi.org/","",data3$doi)

# Repeat screen for unique DOI
data_screen <- merge_columns(data_unique, data3)
doi_match2 <- find_duplicates(data_screen,
                             match_variable = "doi",
                             group_variables = NA,
                             match_function = "exact")


data_screen <- extract_unique_references(data_screen, doi_match2)

data_screen[!data_screen$doi %in% data_unique$doi,]$title

### Score articles by category

# Define categories
category <- list(
  urodele = "salamander|urodele|newt",
  theory = "isolation|ibd|distance|barrier|ibr|resistance",
  population = "gene flow|fst",
  urban = "city|urban|metropo*",
  forest = "forest|conifer|wood|deciduous",
  agriculture = "farm|agro*|pasture"
)

# Define where to add new columns

index <- length(colnames(data_unique))

# Score for each category
scoreddata = data_unique
for (i in seq_along(category)){
  scoreddata[index+i] <- grepl(category[[i]],
                               scoreddata$abstract,ignore.case = T)
}
colnames(scoreddata)[(index+1):length(colnames(scoreddata))] <- 
  names(category)

# Proportion of articles with each category
colSums(scoreddata[(index+1):length(colnames(scoreddata))])/
  dim(scoreddata)[1] * 100

# Number of positive category matches for each article
cutoff = 3 
sum(
  rowSums(scoreddata[(index+1):length(colnames(scoreddata))]) >= cutoff
  )

catagorizeddata <- scoreddata[
  rowSums(scoreddata[(index+1):length(colnames(scoreddata))]) >= cutoff,]

#### VOS Export
# Export for Vos
colnames(Vosdata)
# Export list of authors
write.csv(catagorizeddata[,("author")], "vos_author.csv")
# Export list of titles
write.csv(catagorizeddata[,("title")], "vos_title.csv")

names(categoryV)
vos <- tibble::tibble(Vosdata[,names(categoryV)])
for (i in 1:length(vos)) {
  vos[[i]] <- stringr::str_replace(vos[[i]], "TRUE", colnames(vos)[i])
}
vos

write.csv(vos, "vos_keywords.csv",
          row.names = F)

write.csv(apply(vos, 1, paste, collapse=","), "vos_keywords.csv",
          row.names = F)

## Review articles

# Randonly select some articles to review
catagorizeddata[sample(1:dim(catagorizeddata), 9, replace = T),"doi"]


####FLEISS'S KAPPA FOR M RATERS####
#Computes Fleiss’ Kappa as an index of interrater agreement between m raters on categorical data 
#Additionally, category-wise Kappas could be computed
library(irr)

matrix<-readxl::read_xlsx(path = "MatrixIRR.xlsx", 
                          sheet="Review",
                          col_names = TRUE)

#The null hypothesis Kappa=0
kappam.fleiss(matrix)
kappam.fleiss(matrix, exact = TRUE)
kappam.fleiss(matrix, detail = TRUE)


# Export to bib



#OTHER FUNCTIONS#

# SCREEN ABSTRACTS  - opens app#

abstracts <- screen_abstracts(catagorizeddata[,1:(index-2)])

revtools::write_bibliography(abstracts[abstracts$screened_abstracts == "selected",],
                             "catagorized.bib", format = "bib")


# SCREEN TOPICS #

topics <- screen_topics(abstracts)


#Assign proportions of articles to members of a team#

reviewers <- allocate_effort(
  reviewers = c("rev 1", "rev 2", "rev 3"),
  effort = c(0.6, 0.2, 0.2),
  proportion_checked = 0.6,
  max_reviewers = 2,
  quiet = FALSE)


#Use results from allocate_effort to split a data.frame, returning a .csv file for each reviewer#

distribute_tasks(finaldata,
                 reviewers = reviewers,
                 write_csv = TRUE)


#Combined screened references back into a single data.frame#

final <- aggregate_tasks(list.files(), 
                         match_column = "label", 
                         selection_column = "screened_abstracts")


