###set up
library(here)
library(stringr)
library(dplyr)
library(rstudioapi)


#working directory for creating CPRD Aurum lists, last update June 2021
setwd(file.path(dirname(getActiveDocumentContext()$path),"../.."))
med_aurum<- read.delim("./codelists/data_dictionary/CPRDAurumMedical.txt", sep= "\t",  colClasses="character")
prod_aurum <- read.delim("./codelists/data_dictionary/CPRDAurumProduct.txt", sep= "\t",  colClasses="character")

###---functions to look up different terms and IDs in the aurum data dictionaties
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


###---looking up the codes

#defining the search terms for DTP antigen containing vaccines

###---looking up the codes
terms1 <- c("pneumococc", "pneumovax", "pnu-imune", "PPV", "PCV", "prevenar", "synflorix")
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
write.csv2(df_terms, file = "J:/EHR-Working/Anne/Codelists/Vaccines/Pneu/Pneu_terms_raw.csv")
write.csv2(df_prods, file = "J:/EHR-Working/Anne/Codelists/Vaccines/Pneu/Pneu_prod_raw.csv")




###--- second step search
###after screening the raw code lists, it was searched for smnomed concept IDs
###of key terms 

snomed <- c("310578008", "12866006", "10100410000006110", "16660311000000100",
            "714821000000108")

concepts <- concept_lookup(terms = snomed)

#checking mismatch between already identified terms
missed <- setdiff(concepts, df_terms)
#print out the list of missed terms
write.csv2(missed, file = "./codelists/vaccines/outputs/PNEU_terms_raw_snomedconcept.csv")

###--- third step search, searching for codes of the enhanced services codelists
# https://digital.nhs.uk/data-and-information/data-collections-and-data-sets/data-collections/quality-and-outcomes-framework-qof/quality-and-outcome-framework-qof-business-rules/enhanced-services-es-vaccination-and-immunisation-vi-and-core-contract-components-2021-2022
snomed_2<- as.character(c(10231211000001106,
                          10245211000001108,
                          16649411000001104,
                          16660211000001102,
                          27396511000001105,
                          3005011000001103,
                          3017311000001106,
                          3018111000001105,
                          3021111000001108,
                          3439111000001102,
                          3439211000001108,
                          3439311000001100,
                          34783011000001102,
                          35111211000001108,
                          36461211000001108))
concepts_2 <- concept_lookup(terms = snomed_2)
missed_2 <- setdiff(concepts_2, df_terms)
#print out the list of missed terms
write.csv2(missed_2, file = "./codelists/vaccines/outputs/PNEU_terms_raw_add.csv")
