###set up
library(stringr)
library(dplyr)
library(here)
here()

#working directory for creating GOLD lists, last update June 2021
setwd("J:/EHR-Working/Anne/Codelists/Vaccines/")

med_aurum<- read.delim("J:/EHR-Working/Anne/Read_code_tool_development/data_dictionaries/CPRDAurumMedical.txt", sep= "\t",  colClasses="character")
prod_aurum <- read.delim("J:/EHR-Working/Anne/Read_code_tool_development/data_dictionaries/CPRDAurumProduct.txt", sep= "\t",  colClasses="character")




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

terms1 <- c("pneumococc", "pneumovax", "pnu-imune", "PPV", "PCV" )
terms1a <- c("pneumococc", "pneumovax", "pnu-imune",  "PCV", "prevenar", "synflorix")
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

#missed medical terms
df1b <- term_lookup(terms = terms1a)
df_terms_2 <- intersect(df1b, df2)
missed_terms <- setdiff(df_terms_2, df_terms)

#missed products
df1b <- prod_lookup(terms = terms1a)
df_prods_b <- intersect(df1b, df2a)
length(df1a$Term.from.EMIS)
length(df1b$Term.from.EMIS)
missed_prod <- setdiff(df1b, df1a)


###safing the preliminary lists
write.csv2(df_terms, file = "J:/EHR-Working/Anne/Codelists/Vaccines/Pneu/Pneu_terms_raw.csv")
write.csv2(df_prods, file = "J:/EHR-Working/Anne/Codelists/Vaccines/Pneu/Pneu_prod_raw.csv")
write.csv2(missed_prod, file = "J:/EHR-Working/Anne/Codelists/Vaccines/Pneu/Pneu_prod_raw_add.csv")
write.csv2(missed_terms, file = "J:/EHR-Working/Anne/Codelists/Vaccines/Pneu/Pneu_terms_raw_add.csv")
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
snomed <- c("310578008", "12866006", "10100410000006110", "16660311000000100",
            "714821000000108")

concepts <- concept_lookup(terms = snomed)

#checking mismatch between already identified terms
missed <- setdiff(concepts, df_terms)

