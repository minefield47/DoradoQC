# Metadata ----------------------------------------------------------------
#DoradoQC_duplex
# A R-based quality control for Dorado duplex summary files.  
# Author: Auden Block
#Contact: arb {@} uncw {.} edu
# Last Modification: 11/12/2024
#
# Libraries ---------------------------------------------------------------

library(ggplot2)
library(tools)
library(ggExtra)
library(utils)
library(dplyr)

# No PDF output -----------------------------------------------------------


#Prevents Rplot.pdf output
if(!interactive()) pdf(NULL)


# Variables (User's Edit) -------------------------------------------------

#Set Q-Score
qscore <- 17
print(paste0("Current Q-Score is set to: ", qscore))

#Colors Used for Quality Scoring. 
colors <- c("#e76f51", "#2a9d8f")

#Set Binning for read length. For those expecting ultra-long reads: set this value higher
length_bin <- 5000
print(paste0("Current Binning for Read Length is set to: ", length_bin))




# Functions ---------------------------------------------------------------

#To delete after. 
indiv <- "/Users/auden/call_fast1.tsv"
dir <- "/Users/auden/call_fast_testing"
dir. <- "/Users/auden/call_fast_testing/"



input <- function(x)  {
  if (dir.exists(x) && !(grepl("DoradoQCduplex_", x))){ #Check if argument is a directory and that a DoradoQCduplex_<file_name> does not exist. If true, lump the directory and execute. 
    print("New Directory Detected")
    #Create variable of what new directory should be named.
    dir <- paste0(getwd(),"/", basename(x),"_DoradoQC_duplex")
    #Create directory.
    dir.create(path=dir)
    
    all.summary.list <- list.files(path=x, full.names = TRUE) %>% #List all summary files.
      lapply(., read.delim, header = T, na.strings =c("","NA")) %>% #Read files into R as a list of dataframes.
      bind_rows() %>% #Merge the List into one mega dataframe.
      list(all=., #Original Dataframe.
           duplex=subset(., is.na(.$filename)), #Subset of only duplex values since duplex do not have a filename value.
           simplex=subset(., !(is.na(.$filename))), #Subset only those with a file name, also known as simplex reads.
           dir=dir) #Directory created name to be passed to next function.
    
  } else if (dir.exists(x) && (grepl("DoradoQCduplex_", x))){ #Same as previous, except if a DoradoQCduplex_ directory exists, skip the input. 
    print(paste0("A summary of the directory <",x,"> currently exists already! Rename the directory or summary file and try again."))
    next
  } else { #If it ain't a directory, it has to be a file. 
    print("Individual File")
    if (dir.exists(paste0("DoradoQCduplex_",tools::file_path_sans_ext(x)))) { #Make sure duplex has not been done previously on this model. 
      paste0("A summary directory for the file <",x,"> currently exists already! Rename the directory or summary file and try again.")
      next
    }else {
      print("New Individual file Detected")
  
      #Create variable of what new directory should be named.
      dir <- paste0(getwd(),"/", basename(x),"_DoradoQC_simplex")
      #Create  directory.
      dir.create(path=dir)
      
      all.summary.list <- read.delim(x , header=T, na.strings = c("","NA")) %>% #Read in the file as a dataframe.
        list(all=., #Original dataframe. 
             duplex = subset(., is.na(.$filename)), #Subset of only duplex values since duplex do not have a filename value.
             simplex= subset(., !(is.na(.$filename))), #Subset only those with a file name, also known as simplex reads.
             dir = dir) #Directory created name to be passed to next function.
      
    }
  }
  #Pass the list to the next function, or for list to be saved.
  all.summary.list
}  

  
graphing <- function(){
  
}


# Import Detection --------------------------------------------------------

#Summary files from command arguments.
args <- commandArgs(trailingOnly = TRUE)

#To remove after testing.
args <- "/Users/auden/call_fast_testing"
x <- "/Users/auden/DoradoQC_hacsummary"

#Check that some input file is given.
if (length(args)==0) {
  stop("A summary file or directory is required.")
}

for (x in args) {
  all.summary.reads <- x %>% 
    input() %>% 
    graphing()
}



  
  