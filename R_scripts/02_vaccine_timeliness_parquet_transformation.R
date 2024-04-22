####Script to transform all the d.ta files in to the .parquet data formal


#path settings
path_out <- raw_data
path_in <- raw_data_zip

#reading in all the file paths
zipfolders <- dir(path_in, full.names = TRUE)
#unzip all text files in these folders
setwd(path_out)

for(i in 1:lenght(zipfolders)){

  unzip(zipfolders[i], list = FALSE, overwrite = TRUE,
        junkpaths = TRUE, exdir = ".", unzip = "internal",
        setTimes = FALSE)
}
###note there was an issue with corrupted zip files, 
#unzipped 40-36, 1-5, 16-20, the rest had to be done manually


#list all observation files
observation <- list.files(path = path_out, pattern = "\\Observation")
#list all practice files
practice <- list.files(path = path_out, pattern = "\\Practice")
#list all patient files
patient <- list.files(path = path_out, pattern = "\\Patient")
#list all drug issues 
drugs <- list.files(path = path_out, pattern = "\\DrugIssue")

###function to transform all the files for each category into parquett files
parquett_transform <- function(files){
  filenames <- tools::file_path_sans_ext(basename(files))
  
  for (i in cli::cli_progress_along(files)) {
    write_parquet(data.table(read.table(files[i], header = TRUE, sep = "\t", na.strings = "")), 
                  paste0(path_out, "/parquet_files/", filenames[i], ".parquet"))
  }
}



parquett_transform <- function(files){
  filenames <- tools::file_path_sans_ext(basename(files))
  
  for (i in cli::cli_progress_along(files)) {
    write_parquet(data.table(read.table(files[i], header = TRUE, sep = "\t", na.strings = "")), 
                  paste0(path_out, "/parquet_files/", filenames[i], ".parquet"))
  }
}


df <- read.delim(file = paste0(path_out,"/",  observation[1]), header = T, sep = "\t")


#writing parquett files
parquett_transform(files = patient)
parquett_transform(files = practice)
parquett_transform(files = drugs)
parquett_transform(files = observation)



