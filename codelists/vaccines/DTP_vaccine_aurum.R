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
terms1 <- c("DT", "DTP", "diphteria", "tetanus", "pertussis",
            "diphth", "dipht", "trivax", "boostrix", "DTAP",
            "whooping", "infanrix", "tetavax")
terms2 <- c("vac", "imm", "consent", "injec", "declin", "invit", "boost", 
            "not given", "call", "message", "dose", "syringe", "amp", "attenuated")


#looking up the vaccine terms
df1 <- term_lookup(terms = terms1)
df2 <- term_lookup(terms = terms2)

df_terms <- intersect(df1, df2)

#looking up the vaccine product codes
df1a <- prod_lookup(terms = terms1)
df2a <- prod_lookup(terms = terms2)

df_prods <- intersect(df1a, df2a)

#saving the preliminary lists
write.csv2(df_terms, file = "./codelists/vaccines/outputs/DTP_terms_raw.csv")
write.csv2(df_prods, file = "./codelists/vaccines/outputs/DTP_prod_raw.csv")


###--- second step search
###after screening the raw code lists, it was searched for smnomed concet IDs
###of key terms 


#defining the snomed concept IDs of interest
snomed <- c("3934008", "48028008", "61602008", "76668005", "127786006", "170399005", "281040003", 
            "281040003", "3128700000", "412373007", "41400400516")

#lookup of all terms with that concept ID
concepts <- concept_lookup(terms = snomed)
#checking mismatch between already identified terms
missed <- setdiff(concepts, df_terms)

#check whether terms were missed but relevant code was missed
#View(missed)
#print out the list of missed terms
write.csv2(missed, file = "./codelists/vaccines/outputs/DTP_terms_raw_snomedconcept.csv")
###--- third step search, searching for codes of the enhanced services code lists
# https://digital.nhs.uk/data-and-information/data-collections-and-data-sets/data-collections/quality-and-outcomes-framework-qof/quality-and-outcome-framework-qof-business-rules/enhanced-services-es-vaccination-and-immunisation-vi-and-core-contract-components-2021-2022
#the new pertussis codes were only pregnancy related and therefore not included
snomed_2<- as.character(c(417211006, 
                          417384007,  
                          866186002, 
                          868266002,
                          868267006, 
                          870670004,
                          871837004,
                          871838009,
                          9896511000001102,
                          414619005,
                          416144004,
                          416591003,
                          417615007, 
                          866227002,
                          868268001,
                          10167711000001101,  
                          318351000221106,
                          412763007,
                          412764001, 
                          842861000000101,
                          868274001
                          ,868277008, 
                          871878002, 
                          871883005,
                          16290681000119103,
                          247821000000102,
                          390865008,
                          412762002,
                          414620004,
                          868273007,
                          868276004,
                          871893003,
                          871894009,
                          2091000221104,
                          310306005,
                          310308006,
                          310551000000106,
                          313383003,
                          335051000000104,
                          414259000,
                          415507003,
                          415712004,
                          10170711000001107,
                          310307001, 
                          312870000,
                          379241000000107,
                          414001002,       
                          871888001,
                          1082431000000104,
                          2101000221107,
                          34771611000001101,
                          1082441000000108,
                          1082451000000106,
                          1082461000000109,
                          871895005,
                          7374311000001101,
                          7386611000001107,
                          11122111000001101,
                          8149311000001104,
                          8153411000001106,
                          8426011000001105,
                          8882411000001103,
                          8888711000001107,
                          26267211000001100,
                          8088811000001100,
                          8100611000001109,
                          10776511000001109,
                          10795711000001100,
                          34765811000001105,
                          34769511000001105))
concepts_2 <- concept_lookup(terms = snomed_2)
missed_2 <- setdiff(concepts_2, df_terms)
#inspection of missed terms
#View(missed_2)
#print out the list of missed terms
write.csv2(missed_2, file = "./codelists/vaccines/outputs/DTP_terms_raw_add.csv")
