###set up
library(here)
library(stringr)
library(dplyr)
library(rstudioapi)


#working directory for creating GOLD lists, last update June 2021
setwd(file.path(dirname(getActiveDocumentContext()$path),"../.."))
med_aurum<- read.delim("./codelists/data_dictionary/CPRDAurumMedical.txt", sep= "\t",  colClasses="character")
prod_aurum <- read.delim("./codelists/data_dictionary/CPRDAurumProduct.txt", sep= "\t",  colClasses="character")




###---filtering addintional terms in the aurum data set which might have been missed
#function for looking up the terms
term_lookup <- function(terms){
  #result list for all the searched terms
  list_terms <- list()
  
  for(i in 1:length(terms)){
    
    index <- grep(terms[i], med_aurum$Term, ignore.case=T)
    list_terms[[i]]<- med_aurum[index,]
  }
  
  #generate one data frame deduplicating entries
  df <- list_terms[[1]]
  for(i in 2:length(list_terms)){
    df <- unique(rbind(df, list_terms[[i]]))
  }
  
  df<-df%>%
    arrange(OriginalReadCode)
  return(df)
}


###---code to find the right products
prod_lookup <- function(terms){
  #result list for all the searched terms
  list_terms <- list()
  
  for(i in 1:length(terms)){
    
    index_1 <- grep(terms[i], prod_aurum$Term.from.EMIS, ignore.case=T)
    index_2 <- grep(terms[i], prod_aurum$DrugSubstanceName, ignore.case=T)
    index <- c(index_1, index_2)
    list_terms[[i]]<- prod_aurum[index,]
    
  }
  
  #generate one data frame deduplicating entries
  df <- list_terms[[1]]
  for(i in 2:length(list_terms)){
    df <- unique(rbind(df, list_terms[[i]]))
  }
  
  df<-df%>%
    arrange(Term.from.EMIS)
  return(df)
}


###---looking up the codes

terms1 <- c("DT", "DTP", "diphteria", "tetanus", "pertussis",
            "diphth", "dipht", "trivax", "boostrix", "DTAP",
            "whooping", "infanrix", "tetavax")
terms2 <- c("vac", "imm", "consent", "injec", "declin", "invit", "boost", 
            "not given", "call", "message", "dose", "syringe", "amp", "attenuated")


#vaccine terms
df1 <- term_lookup(terms = terms1)
df2 <- term_lookup(terms = terms2)

df_terms <- intersect(df1, df2)

#vaccine product codes
df1a <- prod_lookup(terms = terms1)
df2a <- prod_lookup(terms = terms2)

df_prods <- intersect(df1a, df2a)

###safing the preliminary lists
write.csv2(df_terms, file = "./codelists/vaccines/outputs/DTP_terms_raw.csv")
write.csv2(df_prods, file = "./codelists/vaccines/outputs/DTP_prod_raw.csv")


###--- second step search, expanding for SNOMED concept IDs which weren't detected 

# function to look up snomed concept IDs
concept_lookup <- function(terms){
  #result list for all the searched terms
  list_terms <- list()
  
  for(i in 1:length(terms)){
    
    index <- grep(terms[i], med_aurum$SnomedCTConceptId, ignore.case=T)
    list_terms[[i]]<- med_aurum[index,]
  }
  
  #generate one data frame deduplicating entries
  df <- list_terms[[1]]
  for(i in 2:length(list_terms)){
    df <- unique(rbind(df, list_terms[[i]]))
  }
  
  df<-df%>%
    arrange(OriginalReadCode)
  return(df)
}


###related snomed concept IDs from pervious code list screening
snomed <- c("3934008", "48028008", "61602008", "76668005", "127786006", "170399005", "281040003", 
            "281040003", "3128700000", "412373007", "41400400516")

concepts <- concept_lookup(terms = snomed)

#checking mismatch between already identified terms
missed <- setdiff(concepts, df_terms)

View(missed)
missed[1,]
