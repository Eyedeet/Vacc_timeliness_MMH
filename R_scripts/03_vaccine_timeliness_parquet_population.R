####script to generate the study population and eventually safe a list of all 
####eligible patients

#get all patient files from the parquet files
setwd(parquet)
patient <- list.files(path = parquet, pattern = "\\Patient")
patient_df <- list()

#reading in the parquet files and fixing data formatting
for(i in 1:length(patient)){
  
  df <- as.data.table(arrow::read_parquet(file = patient[i]))
  df$regstartdate <- as.Date(df$regstartdate, format = "%d/%m/%Y")
  df$regenddate <- as.Date(df$regenddate, format = "%d/%m/%Y")
  df$yob <- as.integer(df$yob)
  df$mob <- as.integer(df$mob)
  df$accetable <- as.integer(df$acceptable)
  df$pracid <- as.integer(df$pracid)
  df$patid <- as.character(df$patid)
  
  patient_df[[i]] <- df
                              
}

#calculating a date of birth for all patients
for(i in 1:length(patient_df)){
  
  patient_df[[i]] <- patient_df[[i]][!is.na(mob)] #drop people with missing month of birth
  patient_df[[i]][, mob2 := ifelse(mob<=9, paste0(0, mob), mob)]
  patient_df[[i]][, dob:= as.Date(paste0("15/", mob2, "/", yob), format = "%d/%m/%Y")]
  patient_df[[i]][, mob2:= NULL]
}
#239448 patients dropped


#remove duplicates
sum(nrow(patient_df[[1]]), nrow(patient_df[[2]]), nrow(patient_df[[3]]))
for(i in 1:length(patient_df)){
  patient_df[[i]] <- unique(patient_df[[i]])
}
sum(nrow(patient_df[[1]]), nrow(patient_df[[2]]), nrow(patient_df[[3]]))
#no duplicates, overall 1819317 entries

safe_copy  <- patient_df
patient_df <- safe_copy

sum(nrow(safe_copy[[1]]), nrow(safe_copy[[2]]), nrow(safe_copy[[3]]))
#quality checks for all patient files 
for(i in 1:length(patient_df)){
  n1 <- nrow(patient_df[[i]])
  patient_df[[i]] <- patient_df[[i]][acceptable ==1] #keep acceptable patients only
  n2 <- nrow(patient_df[[i]])
  print(paste0("drop after accetpable flag check: ", (n1-n2)))
  
  patient_df[[i]] <- patient_df[[i]][ ifelse(!is.na(regenddate), regstartdate <= regenddate, 
                                                   TRUE)]
  n3 <- nrow(patient_df[[i]])
  print(paste0("drop after register dates check: ", (n2-n3)))
  
  patient_df[[i]] <- patient_df[[i]][regstartdate >= (dob-days(16))] #allowing 16days uncertainty
  n4 <- nrow(patient_df[[i]])
  print(paste0("drop after dob registy dates check: ", (n3-n4)))
  
   patient_df[[i]] <- patient_df[[i]][dob<= as.Date("15/02/2015", format = "%d/%m/%Y")] 
   n5<- nrow(patient_df[[i]])
   print(paste0("drop after dob range check: ", (n4-n5)))

}


#removed after acceptable
sum(15, 18, 0) #33 dropped
#removed dob registry date check
sum(6, 13, 0) #19 dropped
#overall 52 patients removed, now 1819265
sum(nrow(patient_df[[1]]), nrow(patient_df[[2]]), nrow(patient_df[[3]]))




# check for missing patids
for(i in 1:length(patient_df)){
  print(any(is.na(patient_df[[i]]$patid)))
} # no missing patids

#appending all the patids files together for a long list
patient_population <- patient_df[[1]]
for(i in 2:length(patient_df)){
  patient_population <- rbind(patient_population, patient_df[[i]])
}
#checking for correct summing up
nrow(patient_population) == (sum(nrow(patient_df[[1]]), nrow(patient_df[[2]]),
                                 nrow(patient_df[[3]])))



#excluding all the patiens with registration date after 01/02/2020
patient_population <- patient_population[regstartdate <= as.Date("01/02/2020", format = "%d/%m/%Y")]

#counting patients
nrow(patient_population)
#1,818,662 babies included




#---merging the baby files with the practice files
#prepparing the practice files
practice <- list.files(path = parquet, pattern = "\\Practice")
practice_df <- list()

#reading in the parquet files and fixing data formatting
for(i in 1:length(practice)){
  
  df <- as.data.table(arrow::read_parquet(file = practice[i]))
  df$lcd <- as.Date(df$lcd, format = "%d/%m/%Y")
  df$region <- as.integer(df$region)
  df$pracid <- as.integer(df$pracid)
  
  practice_df[[i]] <- df
}


#quality check - lcd not before start of study
for(i in 1:length(practice_df)){
  n1 <- nrow(practice_df[[i]])
  practice_df[[i]][, lcd >= as.Date("01/01/2006", format = "%d/%m/%Y")] 
  n2 <- nrow(practice_df[[i]])
  print(paste0("drop lcd check: ", (n1-n2)))

} # no invalid practices

#appending all the patids files together for a long list
all_practices <- practice_df[[1]]
for(i in 2:length(practice_df)){
  all_practices <- rbind(all_practices, practice_df[[i]])
}
#checking for correct summing up
nrow(all_practices) == (sum(nrow(practice_df[[1]]), nrow(practice_df[[2]]),
                                 nrow(practice_df[[3]])))




#---merging the patient file and the practice file
#making sure there are no duplicated
all_practices <- unique(all_practices)
patient_population <- unique(patient_population)

all_practices$pracid <- as.character(all_practices$pracid)
patient_population$pracid <- as.character(patient_population$pracid)
study_pop <- merge(patient_population, all_practices, all.x =TRUE, 
                   all.y= FALSE, by = "pracid")

population <- study_pop[, list(patid, pracid, gender, yob, mob, regstartdate,
                               regenddate, cprd_ddate, dob, lcd, region)]

#generate a end-of-follow-up date and a start of follow-up date
#population <- as.data.table(read_parquet(file = paste0(datafiles, "study_population.parquet")))
population[, bday5 := dob + years(5)]
population[, cprd_ddate := as.Date(cprd_ddate, format = "%d/%m/%Y")]
population[, end_fu := min(lcd,cprd_ddate, bday5, regenddate, na.rm = T), by ="patid"]
population[, start_fu := max(dob, regstartdate, na.rm = T), by = "patid"]
#drop these if the start of follow up is after the end of follow up
n <- population[start_fu <= end_fu]
nrow(population)-nrow(n) # 72,068 dropped because start of fu after firth bday
population <- population[start_fu <= end_fu]
population[, age_endfu := interval(dob, end_fu)%/%days(1)]
population[, age_startfu := ifelse(start_fu >= dob, (interval(dob, start_fu)%/%days(1)),
                                   (interval(start_fu, dob)%/%days(1)))]
population[, fu_time := age_endfu-age_startfu]
summary(population$age_startfu)
summary(population$age_endfu)
summary(population$fu_time)

write_parquet(population, 
              paste0(datafiles, "study_population.parquet"))
#---making a list of patients for linkage request
pop <- arrow::read_parquet(file = paste0(datafiles, "study_population.parquet"))

#list with possible linkages
links <- read.table(file = "J:/EHR Share/3 Database guidelines and info/CPRD Linkage Source Data Files/Version22/set_22_Source_Aurum/Aurum_enhanced_eligibility_January_2022.txt",
                    header = TRUE,
                    sep = "\t",
                    dec = ".")
links <- as.data.table(links, keep.rownames=FALSE, key=NULL, sorted=TRUE,
                       value.name="value", na.rm=FALSE)
links$patid <- as.character(links$patid)
nrow(pop)

#merging population file with linkage
pop <- merge(pop, links, all.x= T, all.y=F, by="patid" )
nrow(pop) #1746593

#cleaning the file
pop<- pop[, pracid := pracid.x]
col_delete <- c("pracid.y", "sgss_e", "chess_e", "hes_op_e",
                  "hes_did_e", "cr_e", "sact_e", "rtds_e",
                  "mhds_e", "icnarc_e","hes_ae_e","pracid.x" )
for(i in 1:length(col_delete)){
  
  pop <- pop[, col_delete[i] := NULL]
}
pop$linkdate <- as.Date(pop$linkdate, format = "%d/%m/%Y")

nrow(pop)# 1,746,593patients so far

#drop those who are not eligible to any linkage
pop <- pop[any((hes_apc_e | ons_death_e | lsoa_e ) == 1)]
nrow(pop) # all patients are eligible for at least one

#list for HES 
hes <- pop[hes_apc_e == 1]
nrow(hes) #  1,698,912HES eligible
#list for ons_death_e
ons <- pop[ons_death_e == 1]
nrow(ons) #1,698,912 ONs eligible
#list for lsoa_e
lsoa <- pop[lsoa_e == 1]
nrow(lsoa) #1,704,427 LSOA eligible

#writing list with eligible patients
list_hes <- hes[, list(patid, hes_apc_e)]
list_ons <- ons[, list(patid, ons_death_e)]
list_lsoa <- lsoa[, list(patid, lsoa_e)]
population <- pop[, patid]

write.table(list_hes, file = paste0(datafiles, "patid_list_hes_apc.txt"), col.names=F,
            row.names=F, sep="", quote=F)
write.table(list_ons, file = paste0(datafiles, "patid_list_ons.txt"), col.names=F,
            row.names=F, sep="", quote=F)
write.table(list_lsoa, file = paste0(datafiles, "patid_list_lsoa.txt"), col.names=F,
            row.names=F, sep="", quote=F)
write.table(population, file = paste0(datafiles, "patid_list.txt"), col.names=F,
            row.names=F, sep="", quote=F)

