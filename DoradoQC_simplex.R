
# Metadata ----------------------------------------------------------------
#DoradoQC_simplex
# A simple(x) R-based quality control for Dorado summary files. 
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


#Prevent Rplot.pdf output
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


# Input Detection and Sorting Function ------------------------------------


input <- function(x)  {
  if (dir.exists(x) && !(grepl("DoradoQCduplex_", x))){ #Check if argument is a directory and that a DoradoQCduplex_<file_name> does not exist. If true, lump the directory and execute. 
    print("New Directory Detected")
    #Create variable of what new directory should be named.
    print(x)
    
    #Create variable of what new directory should be named.
    dir <- paste0(getwd(),"/", basename(x),"_DoradoQC_simplex")
    #Create the directory.
    dir.create(path=dir)
    #Create a list of the files within called argument directory.
    list.files(path=x)
    
    all.summary.list <- list.files(path=x, full.names = TRUE) %>% #List all summary files.
      lapply(., read.delim, header = T, na.strings =c("","NA")) %>% #Read files into R as a list of dataframes.
      bind_rows() %>% #Merge the list into a single dataframe.
      list(reads = ., 
           dir = dir) #Create a new list of reads and the directory created for the graphing function. 
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
      #Create the directory.
      dir.create(path=dir)
      
      all.summary.list <- x %>% 
        read.delim(. , header=T, na.strings = c("","NA")) %>% #read file into R as a dataframe.
        list( reads = . ,
              dir = dir) #Create a new list of reads and the directory created for the graphing function. 
    }
  }
  #Return the list. Must be kept at the end of the function.
  all.summary.list
}  



graphing <- function(all.summary.list) {
  setwd(all.summary.list$dir)
  #Sort by start time
  all.summary.list$reads <- all.summary.list$reads[order(all.summary.list$reads$template_start),]
  
  #Data Mutations  
  #Seconds to hours
  all.summary.list$reads[,"start_hours"] <- all.summary.list$reads$template_start / (60*60)
  
  #create column of cumulative reads over time
  all.summary.list$reads[,"cumreads"] <- cumsum(all.summary.list$reads$sequence_length_template)
    
  #Histogram of Mean Q-Score
   histogram.meanq <- ggplot(all.summary.list$reads, aes(x=mean_qscore_template, fill=(mean_qscore_template >= qscore))) + 
      geom_histogram(binwidth = 1) +
      labs(x = "Mean Q-Score") + 
      theme_classic()+
      scale_fill_manual(name=paste0("Q-Score: ", qscore), values=colors, labels=c("Below Q-Score", "Above Q-Score"))
    ggsave("meanqscore_histogram.png")
    print("Histogram of Mean Q-Score created")
  
  #Histogram of Read Lengths
   histogram.read <- ggplot(all.summary.list$reads, aes(x=sequence_length_template, fill=(mean_qscore_template >= qscore))) + 
      geom_histogram(binwidth = length_bin, alpha=0.5) +
      labs(x = paste0("Read Length (Binning=", length_bin,")")) + 
      theme_classic()+
      scale_fill_manual(name=paste0("Q-Score: ", qscore), values=colors, labels=c("Below Q-Score", "Above Q-Score"))
    ggsave("sequencelength_histogram.png")
    print("Histogram of Read Lengths created")
  
  #Scatterplot with histogram margins

    scatter.histo <- ggplot(all.summary.list$reads, aes(x=sequence_length_template, y=mean_qscore_template, color=(mean_qscore_template >= qscore))) + 
      geom_point() +
      labs(y="Mean Q-Score", x= "Read Length") +
      theme_classic() +
      scale_color_manual(name=paste0("Q-Score: ", qscore), values=colors, labels=c("Below Q-Score", "Above Q-Score"))
    scatter.histo <- ggMarginal(scatter.histo, theme_classic() + theme(legend.position = "left"), type="histogram", binwidth=1) #Bin Length has to be 1 for the Quality Score index
    ggsave("sequencelength_qscore_scatter_with_histogram.png", plot = scatter.histo)
    print("Scatter with Histogram Margins created")
  
    
    
  #Cumulative Sum over time 
    #Create subsample of reads over quality score
      subsample <- subset(all.summary.list$reads, (all.summary.list$reads$mean_qscore_template>=qscore))
    #Recalculate the cumulative sum  
      subsample[,"cumreads"] <- cumsum(subsample$sequence_length_template)
    #Plot:  
   cum.sum <- ggplot(all.summary.list$reads, aes(start_hours, cumreads, color = colors[1])) +
     #All Reads
      geom_point() +
     #Subsamples
      geom_point(data = subsample, aes(start_hours, cumreads, color= colors[2])) +
      theme_classic()+
      theme(legend.position="bottom")+
      scale_color_manual(name=paste0("Q-Score: ", qscore), values=colors, labels=c("All Reads", "Above Specified Q-Score")) +
      labs(x="Run Time (s)", y="Cumulative Reads", color ="the legend")
    ggsave("runtime_vs_cumulativereads_filtered.png")
    print("Cumulative Sum over Time created")
  
}


# Execute the functions ---------------------------------------------------

#Pull summary files from command arguments.
args <- commandArgs(trailingOnly = TRUE)


#Check that some input file is given.
if (length(args)==0) {
  stop("A summary file or directory is required.")
}



for (x in args) {
   x %>% 
    input() %>%
      graphing()
}
