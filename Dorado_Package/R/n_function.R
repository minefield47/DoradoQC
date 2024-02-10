#' Contiguity Quality. 
#' @author Auden Block
#' @concept n.50
#' @description
#' Typically represented as the n.50, the function defines the contiguity quality of a given set of contigs. 
#' The n.function returns the sequence length of the shortest contig at the given percentage of the genome. For example, an n.50 represents the shortest read length to represent 50% of the genome. 
#' For more information, please see: https://ucdavis-bioinformatics-training.github.io/2020-Genome_Assembly_Workshop/kmers/QAQC
#' @param  read.length A vector of the read length of a given dataset.
#' @param percentage (default = 50) The percentage at which to evaluate.
#' @return The n.percentage of the given input dataframe.
#' @examples 
#' n.function(summary.list$simplex$sequence_length_template);
#' n.function(sequence_length, 90);
#' n.function(c(5,3,55,24,6,3,1,5,7,9,75,3,2,12,6,45,3,565,57))
#' @export
n.function <- function(read.length, percentage=50) {
  limit <- (sum(read.length) * (percentage/100)) # y/100 gives a decimal (i.e percent) wanted for the N__.
  read.length <-  sort(read.length, decreasing=T) #Sort by decreasing order.
  #Set variables for this function.
  t <- 0 #Current number of bases added. 
  z <- 1 #Row caller
  #Actual N__ Calculation. 
  while (t <= limit) {
    t <- (t + read.length[z])
    z <- z + 1 
  } 
  z <- z - 1 #Since the last while loop adds one to z, we need to remove it. 
  return(read.length[z]) #Return the value in megabases
}
