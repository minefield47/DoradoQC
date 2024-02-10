#' Import the summary tsv into R and subset by type
#' @author Auden Block
#' 
#' @description
#' Dorado summary creates a summary.tsv which can be read into R using read.delim(<file>, header=T,na.strings=c("","NA")). However, this can only read a single dataframe at a time and for duplex reads, is not subset into smaller dataframes. 
#' For a single file: The function imports the data using read.delim as a dataframe.
#' For a directory of reads: The function imports each .tsv file as a dataframe index of a list, then binds the list into a single dataframe. 
#' If Duplex reads are found: The imported dataframe is converted into a list of (1) duplex/simplex reads, (2) duplex reads, (3) simplex reads. 
#' If no duplex reads are found, the script returns the respective dataframe of imported reads.
#' 
#' @param file_or_directory The path to a single duplex summary file or directory of duplex summary files. 
#' @return Depending upon input, either a list containing three dataframes: (1) Both duplex/simplex reads, (2) only duplex reads, (3) only simplex reads, OR if no duplex data is found, a single dataframe of simplex reads.
#' 
#' @examples 
#' summary.list <- import.dorado.tsv(/Users/user/Dorado/summary.tsv);
#' summary.list <- import.dorado.tsv(/Users/user/Dorado/summary_directory);
#' @export
import.dorado.tsv <- function(file_or_directory)  {
  
    if (dir.exists(file_or_directory)) { #Check if argument is a directory and that a DoradoQCduplex_<file_name> does not exist. If true, lump the directory and execute. 
      all.reads <- do.call("rbind", lapply(list.files(path=file_or_directory, full.names = TRUE),read.delim, header = T, na.strings =c("","NA")))
      
      ifelse(is.na(all.reads$filename), 
             #If filenames has NA values, this indicated duplex data, so we want to automatically return a list of the duplex and simplex data.   
                   return(list(all.reads=all.reads,
                        duplex=subset(all.reads, is.na(all.reads$filename)), #Subset of only duplex values since duplex do not have a filename value.
                        simplex=subset(all.reads,!is.na(all.reads$filename))
                   )),
                   return(all.reads)
                   )
        
     #The above piped for readability:
     # list.files(path=file_or_directory, full.names = TRUE) %>%#List all summary files.
     #  lapply(., read.delim, header = T, na.strings =c("","NA")) %>% #Read files into R as a list of dataframes.
     #  dplyr::bind_rows() %>% #Merge the List into one mega dataframe.
     #  list(all=., #Original Dataframe.
     #       duplex=subset(., is.na(.$filename)), #Subset of only duplex values since duplex do not have a filename value.
     #       simplex=subset(.,!is.na(.$filename))
     #  ) %>%
     #  return()
     # 
        
    } else { #If it ain't a directory, it has to be a single file. 
        all.reads <-read.delim(file_or_directory, header = T, na.strings =c("","NA"))
        
        ifelse(is.na(all.reads$filename), 
               #If filename has NA values, this indicated duplex data, so we want to automatically return a list of the duplex and simplex data.   
               return(list(all.reads=all.reads,
                           duplex=subset(all.reads, is.na(all.reads$filename)), #Subset of only duplex values since duplex do not have a filename value.
                           simplex=subset(all.reads,!is.na(all.reads$filename))
               )),
               return(all.reads)
        )
        
      #Piped Version for readability:
        # all.summary.list <- read.delim(file_or_directory , header=T, na.strings = c("","NA")) %>% #Read in the file as a dataframe.
        #   list(all=., #Original dataframe. 
        #        duplex = subset(., is.na(.$filename)), #Subset of only duplex values since duplex do not have a filename value.
        #        simplex= subset(., !(is.na(.$filename)))
        #   ) %>%
        #   return()
        # 
    }
}  
