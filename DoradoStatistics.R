
# Metadata ----------------------------------------------------------------
#DoradoQC
# A simple R-based quality control for dorado summary files. 
# Author: Auden Block
#
#
# Libraries ---------------------------------------------------------------

library(ggplot2)
library(readxl)
library(tools)
library(ggExtra)
library(dplyr)

#Prevent Rplot.pdf output
if(!interactive()) pdf(NULL)


# Variables (User's Edit) -------------------------------------------------

#Set Q-Score
qscore <- 17
print(paste0("Current Q-Score is set to: ", qscore))
#Set Binning for read length, for those expecting long reads set this value higher
length_bin <- 5000
print(paste0("Current Binning for Read Length is set to: ", length_bin))

colors <- c("#e76f51", "#2a9d8f")

# Scripting (Do not Modify) -----------------------------------------------

# Graph Function ----------------------------------------------------------

graphing <- function(i) {
  
  #Sort by start time
  summary <- summary[order(summary$template_start),]
  
  #Data Mutations  
  #Seconds to hours
  summary[,"start_hours"] <- summary$template_start / (60*60)
  
  #create column of cumulative reads over time
  summary[,"cumreads"] <- cumsum(summary$sequence_length_template)
    
  #Histogram of Mean Q-Score
    ggplot(summary, aes(x=mean_qscore_template, fill=(mean_qscore_template >= qscore))) + 
      geom_histogram(binwidth = 1) +
      labs(x = "Mean Q-Score") + 
      theme_classic()+
      scale_fill_manual(name=paste0("Q-Score: ", qscore), values=colors, labels=c("Below Q-Score", "Above Q-Score"))
    ggsave(paste0(dir,"/meanqscore_histogram.png"))
    print("Histogram of Mean Q-Score created")
  
  #Histogram of Read Lengths
    ggplot(summary, aes(x=sequence_length_template, fill=(mean_qscore_template >= qscore))) + 
      geom_histogram(binwidth = length_bin, alpha=0.5) +
      labs(x = paste0("Read Length (Binning=", length_bin,")")) + 
      theme_classic()+
      scale_fill_manual(name=paste0("Q-Score: ", qscore), values=colors, labels=c("Below Q-Score", "Above Q-Score"))
    ggsave(paste0(dir,"/sequencelength_histogram.png"))
    print("Histogram of Read Lengths created")
  
  #Scatterplot with histogram margins
  
    plot <- ggplot(summary, aes(x=sequence_length_template, y=mean_qscore_template, color=(mean_qscore_template >= qscore))) + 
      geom_point() +
      labs(y="Mean Q-Score", x= "Read Length") +
      theme_classic() +
      scale_color_manual(name=paste0("Q-Score: ", qscore), values=colors, labels=c("Below Q-Score", "Above Q-Score"))
    plot <- ggMarginal(plot + theme_classic() + theme(legend.position = "left"), type="histogram", binwidth=1)    
    ggsave(paste0(dir,"/sequencelength_qscore_scatter_with_histogram.png"), plot = plot)
    print("Scatter with Histogram Margins created")
  
  
  #Cumulative Sum over time 
    #Create subsample of reads over quality score
      subsample <- subset(summary, (summary$mean_qscore_template>=qscore))
    #Recalculate the cumulative sum  
      subsample[,"cumreads"] <- cumsum(subsample$sequence_length_template)
    #Plot:  
    ggplot() +
      geom_point(data = summary, aes(start_hours, cumreads, color = colors[1])) +
      geom_point(data = subsample, aes(start_hours, cumreads, color= colors[2])) +
      theme_classic()+
      theme(legend.position="bottom")+
      scale_color_manual(name=paste0("Q-Score: ", qscore), values=colors, labels=c("Below Q-Score", "Above Q-Score")) +
      labs(x="Run Time (s)", y="Cumulative Reads", color ="the legend")
    ggsave(paste0(dir,"/runtime_vs_cumulativereads_filtered.png"))
    print("Cumulative Sum over Time created")
  
}



# Input Detection and Sorting ---------------------------------------------

#Summary files from command arguments.
args <- commandArgs(trailingOnly = TRUE)

#To remove after testing.
args <- c("hac_summary")


#Check that some input file is given.
if (length(args)==0) {
  stop("A summary file or directory is required.")
}

for (x in args){
  if (dir.exists(x)) { #Is it a directory and not a file? 
    print("Directory Detected")
    if (grepl("DoradoQC_", x)) { #Does a summary directory already exist?
      print(paste0("A summary of the directory <",x,"> currently exists already! Rename the directory or summary file and try again."))
      next
    } else {
    print("New directory detected")
    dir <- paste0("DoradoQC_",tools::file_path_sans_ext(x))
    files <- list.files(path=x)
    summary.list <- lapply(files, read.delim)
    summary <- bind_rows(summary.list)
    print("Summary File Created")
    dir.create(dir)
    graphing(summary)
    }
  } else {
    print("Individual file detected")
  #Create a directory name from the file name without the extension. 
     dir <- paste0("DoradoQC_",tools::file_path_sans_ext(x))
  
  #Check if a directory named after the summary file already exists, if yes = stop, else create directory. 
      if (dir.exists(dir)) {
      paste0("A summary directory for the file <",x,"> currently exists already! Rename the directory or summary file and try again.")
      next
      }else {
      print("creating individual directory")
      dir.create(dir)
     # Import the tsv for each file. 
      summary <- read.delim(x)
      #Run the graphing function. 
      graphing(summary)
      }
  }
}