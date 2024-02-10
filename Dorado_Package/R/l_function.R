#' Contiguity Quality. 
#' @author Auden Block
#' @concept l.50
#' @description
#' Typically represented as the l.50, the function defines the count of contigs whoses sum length represents half the genome.
#' For more information, please see: https://ucdavis-bioinformatics-training.github.io/2020-Genome_Assembly_Workshop/kmers/QAQC
#' @param  read.length A vector of the read length of a given dataset.
#' @param percentage (default = 50) The percentage at which to evaluate.
#' @return The n.percentage of the given input dataframe.
#' @examples 
#' n.function(summary.list$simplex$sequence_length_template);
#' n.function(sequence_length, 90);
#' n.function(c(5,3,55,24,6,3,1,5,7,9,75,3,2,12,6,45,3,565,57))
#' @export
l.function <- function(read.length,percentage=50){
  limit <- (sum(read.length) * (percentage/100)) # y/100 gives a decimal (i.e percent) wanted for the N__.
  read.length <-  sort(read.length, decreasing=T) #Sort by decreasing order.
  #Set variables for this function.
  t <- 0 #Current number of reads added. 
  z <- 1 #Indexer
  
  #Actual L__ Calculation. 
  while (t <= limit) {
    t <- (t + read.length[z])
    z <- z + 1 
  } 
  z <- z-1 #The last call of the while loop will add one to z, so it needs to be removed to get the proper increment
  return(z) #Return the value in megabases
  
  
}
