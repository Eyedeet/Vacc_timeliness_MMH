###Ethnicity algorithm using med codes in CPRD Aurum to determine the ethnicity
####of the included babies

###---preparation
#reading in the existing code list for ethnicity
eth_codes <- haven::read_dta(file = "codelist_ethnicity_aurum.dta")
eth_codes <- as.data.table(eth_codes)

obs.files <- list.files(path = paste0(parquet, "filtered/"), pattern = "\\Observation")


#extracting ethnicity codes only
setwd(paste0(parquet, "filtered/"))
df <- extractor_med(list.files = obs.files,
                    codelist = eth_codes)
length(unique(df$patid))#data of 1,505,009 babies extracted


#restrict observations to the follow up period
df <- df[obsdate <= as.Date("2020-02-01", format = "%Y-%m-%d")]
df <- df[, age := obsdate-dob]
#exclude ethnicity entries after end of follow-up when 
#child ages 5 years ( = 1825 days plus 15 days uncertainty = 1,840)
df <- df[, end_follow_up := age > days(1840)]
df <- df[end_follow_up == FALSE]
length(unique(df$patid)) #1,431,630 babies within follow-up period

#describing the overall given codes
janitor::tabyl(df$eth5)
janitor::tabyl(df$eth16)


#tagging patients with the same ethnicity entry within the same year
df<- df[order(patid)]
df <- df[, sysyear := year(obsdate)]
df[, duplicates := .N, by = c("patid", "sysyear", "medcodeid")]
#keeping the first ethnicity entry per year if there is a duplicate
df[duplicates > 1, first_entry := lapply(.SD, min), by = c("patid", "sysyear"), .SDcols = "obsdate"]
df[duplicates >1, keep_row := obsdate == first_entry]
df[duplicates == 1, keep_row := TRUE]
df <- df[keep_row == TRUE]

df[, duplicates := NULL]
df[, first_entry := NULL]
df[, keep_row := NULL]


#count the observations of ethnicity by patient
df <- df[, by = c("patid"), eth5_count := .N]

tmp <- df[, list(patid, eth5_count)]
tmp <- unique(tmp)
janitor::tabyl(tmp$eth5_count) #77.97% of the babies had only one ethnicity entry
# babies had up to 10 ethnicity records


###adding up ethnicity - 5 categories and 16 categories
max_NA <- function(x){
  return(max(x, na.rm=T))
}

df[eth5 == 0, eth5whitecount := .N, by = "patid"]
df[eth5 != 0, eth5whitecount := 0]
df[, eth5whitecount := lapply(.SD, max_NA), .SDcols = c("eth5whitecount"), by="patid"]
df[eth5 == 1, eth5sacount := .N, by = "patid"]
df[eth5 != 1, eth5sacount := 0]
df[, eth5sacount := lapply(.SD, max_NA), .SDcols = c("eth5sacount"), by="patid"]
df[eth5 == 2, eth5blackcount := .N, by = "patid"]
df[eth5 != 2, eth5blackcount := 0]
df[, eth5blackcount := lapply(.SD, max_NA), .SDcols = c("eth5blackcount"), by="patid"]
df[eth5 == 3, eth5othercount := .N, by = "patid"]
df[eth5 != 3, eth5othercount := 0]
df[, eth5othercount := lapply(.SD, max_NA), .SDcols = c("eth5othercount"), by="patid"]
df[eth5 == 4, eth5mixedcount := .N, by = "patid"]
df[eth5 != 4, eth5mixedcount := 0]
df[, eth5mixedcount := lapply(.SD, max_NA), .SDcols = c("eth5mixedcount"), by="patid"]
df[eth5 == 5, notstated5count := .N, by = "patid"]
df[eth5 != 5, notstated5count := 0]
df[, notstated5count := lapply(.SD, max_NA), .SDcols = c("notstated5count"), by="patid"]


df[eth16 ==1, british16count := .N, by = "patid"]
df[eth16 != 1, british16count := 0]
df[, british16count := lapply(.SD, max_NA), .SDcols = c("british16count"), by="patid"]
df[eth16 ==2, irish16count := .N, by = "patid"]
df[eth16 != 2, irish16count := 0]
df[, irish16count := lapply(.SD, max_NA), .SDcols = c("irish16count"), by="patid"]
df[eth16 ==3, otherwhite16count := .N, by = "patid"]
df[eth16 != 3, otherwhite16count := 0]
df[, otherwhite16count := lapply(.SD, max_NA), .SDcols = c("otherwhite16count"), by="patid"]
df[eth16 ==4, whitecarib16count := .N, by = "patid"]
df[eth16 != 4, whitecarib16count := 0]
df[, whitecarib16count := lapply(.SD, max_NA), .SDcols = c("whitecarib16count"), by="patid"]
df[eth16 ==5, whiteaf16count := .N, by = "patid"]
df[eth16 != 5, whiteaf16count := 0]
df[, whiteaf16count := lapply(.SD, max_NA), .SDcols = c("whiteaf16count"), by="patid"]
df[eth16 ==6, whiteasian16count := .N, by = "patid"]
df[eth16 != 6, whiteasian16count := 0]
df[, whiteasian16count := lapply(.SD, max_NA), .SDcols = c("whiteasian16count"), by="patid"]
df[eth16 ==7, othermixed16count := .N, by = "patid"]
df[eth16 != 7, othermixed16count := 0]
df[, othermixed16count := lapply(.SD, max_NA), .SDcols = c("othermixed16count"), by="patid"]
df[eth16 ==8, indian16count := .N, by = "patid"]
df[eth16 != 8, indian16count := 0]
df[, indian16count := lapply(.SD, max_NA), .SDcols = c("indian16count"), by="patid"]
df[eth16 ==9, pak16count := .N, by = "patid"]
df[eth16 != 9, pak16count := 0]
df[, pak16count := lapply(.SD, max_NA), .SDcols = c("pak16count"), by="patid"]
df[eth16 ==10, bangla16count := .N, by = "patid"]
df[eth16 != 10, bangla16count := 0]
df[, bangla16count := lapply(.SD, max_NA), .SDcols = c("bangla16count"), by="patid"]
df[eth16 ==11, otherasian16count := .N, by = "patid"]
df[eth16 != 11, otherasian16count := 0]
df[, otherasian16count := lapply(.SD, max_NA), .SDcols = c("otherasian16count"), by="patid"]
df[eth16 ==12, carib16count := .N, by = "patid"]
df[eth16 != 12, carib16count := 0]
df[, carib16count := lapply(.SD, max_NA), .SDcols = c("carib16count"), by="patid"]
df[eth16 ==13, african16count := .N, by = "patid"]
df[eth16 != 13, african16count := 0]
df[, african16count := lapply(.SD, max_NA), .SDcols = c("african16count"), by="patid"]
df[eth16 ==14, otherblack16count := .N, by = "patid"]
df[eth16 != 14, otherblack16count := 0]
df[, otherblack16count := lapply(.SD, max_NA), .SDcols = c("otherblack16count"), by="patid"]
df[eth16 ==15, chinese16count := .N, by = "patid"]
df[eth16 != 15, chinese16count := 0]
df[, chinese16count := lapply(.SD, max_NA), .SDcols = c("chinese16count"), by="patid"]
df[eth16 ==16, other16count := .N, by = "patid"]
df[eth16 != 16, other16count := 0]
df[, other16count := lapply(.SD, max_NA), .SDcols = c("other16count"), by="patid"]
df[eth16 ==17, notstated16count := .N, by = "patid"]
df[eth16 != 17, notstated16count := 0]
df[, notstated16count := lapply(.SD, max_NA), .SDcols = c("notstated16count"), by="patid"]



#flagging those patients who only have non stated ethnicity
df<- df[,  nonstatedonly := (notstated5count > 0 &
                               eth5whitecount == 0 &
                               eth5sacount == 0 &
                               eth5blackcount == 0 &
                               eth5othercount == 0 &
                               eth5mixedcount == 0)] 
janitor::tabyl(df$nonstatedonly) #5.02% have non stated ethnicity only

###finding the most common ethnicity, excluding ethnicity not stated
# Get Row wise max in R
df <- df[,eth5max_f :=  names(.SD)[max.col(.SD, ties.method = "first")], .SDcols = 34:38]
df <- df[,eth5max_l :=  names(.SD)[max.col(.SD, ties.method = "last")], .SDcols = 34:38]
df <- df[,eth16max_f :=  names(.SD)[max.col(.SD, ties.method = "first")], .SDcols = 40:55]
df <- df[,eth16max_l :=  names(.SD)[max.col(.SD, ties.method = "last")], .SDcols = 40:55]

#get the rows with equally common ethnicities, excluding non stated
df<- df[, equaleth5 := eth5max_f != eth5max_l & notstated5count == 0 ]
df<- df[, equaleth16 := eth16max_f != eth16max_l & notstated16count  ==0]
janitor::tabyl(df$equaleth5) #2.4% with equally recorded most common ethnicity


###---FINAL STEP - assigning ethnicity to patid
#case 1: ethnicity is not stated - assign not stated
eth <- df[nonstatedonly==T, ethnicity_5 := 5]
eth <- df[nonstatedonly==T, ethnicity_16 := 17]

tmp <- eth[nonstatedonly == T]
length(unique(tmp$patid))
#there are 86,243 babies with a non stated ethnicity


####case 2: assign the most common ethnicity
eth[equaleth5 == F & nonstatedonly == F, ethnicity_5c := eth5max_f]
eth[equaleth16 == F & nonstatedonly == F, ethnicity_16c := eth16max_f]

###case 3: if equal common
#finding the latest date of ethnicity record and using this record as eth5/16 code
eth[equaleth5 ==T, latestethdate := lapply(.SD, max), by = "patid", .SDcols = "enterdate"]
eth[equaleth5 == T, eth_pick := enterdate == latestethdate]
eth[equaleth5 == T & eth_pick == T, ethnicity_5 := eth5]

eth[equaleth16 ==T, latestethdate := lapply(.SD, max), by = "patid", .SDcols = "obsdate"]
eth[equaleth16 == T, eth_pick := enterdate == latestethdate]
eth[equaleth16 == T & eth_pick == T, ethnicity_16 := eth16]

#dropping columns with not used record
eth <- eth[equaleth5== F | (equaleth5 == T  & eth_pick == T)]

#translating the character back into ethnicity numbers
eth[ethnicity_5c == "eth5whitecount", ethnicity_5 := 0]
eth[ethnicity_5c == "eth5sacount", ethnicity_5 := 1]
eth[ethnicity_5c == "eth5blackcount", ethnicity_5 := 2]
eth[ethnicity_5c == "eth5othercount", ethnicity_5 := 3]
eth[ethnicity_5c == "eth5mixedcount", ethnicity_5 := 4]

eth[ethnicity_16c ==  "british16count", ethnicity_16 := 1]
eth[ethnicity_16c ==  "irish16count", ethnicity_16 := 2]
eth[ethnicity_16c ==  "otherwhite16count", ethnicity_16 := 3]
eth[ethnicity_16c ==  "whitecarib16count", ethnicity_16 := 4]
eth[ethnicity_16c ==  "whiteaf16count", ethnicity_16 := 5]
eth[ethnicity_16c ==  "whiteasian16count", ethnicity_16 := 6]
eth[ethnicity_16c ==  "othermixed16count", ethnicity_16 := 7]
eth[ethnicity_16c ==  "indian16count", ethnicity_16 := 8]
eth[ethnicity_16c ==  "pak16count", ethnicity_16 := 9]
eth[ethnicity_16c ==  "bangla16count", ethnicity_16 := 10]
eth[ethnicity_16c ==  "otherasian16count", ethnicity_16 := 11]
eth[ethnicity_16c ==  "carib16count", ethnicity_16 := 12]
eth[ethnicity_16c ==  "african16count", ethnicity_16 := 13]
eth[ethnicity_16c ==  "otherblack16count", ethnicity_16 := 14]
eth[ethnicity_16c ==  "chinese16count", ethnicity_16 := 15]
eth[ethnicity_16c ==  "other16count", ethnicity_16 := 16]



###special cases that people received conflicting codes on the same day
#pick last row
eth <- unique(eth[, list(patid, dob, pracid, ethnicity_5, ethnicity_16)])

eth[, same_entry := seq_len(.N), by = "patid"]
eth[, same_entry := rowid(patid)]
eth[, latest_entry := lapply(.SD, max), by = "patid",  .SDcols = "same_entry"]
eth[, keep := same_entry== latest_entry]
eth <- eth[keep == T]

which(duplicated(eth$patid))

###--- creating a clean ethnicity file
eth <- unique(eth[, list(patid, dob, pracid, ethnicity_5, ethnicity_16)])
janitor::tabyl(eth$ethnicity_5)
hist(eth$ethnicity_5)

janitor::tabyl(eth$ethnicity_16)
hist(eth$ethnicity_16)


write_parquet(eth, paste0(datafiles, "ethnicity.parquet"))