#' Mutate the duplex dataframe to include information about the simplex parents from the simplex dataframe. 
#' @author Auden Block
#' 
#' @param duplex.df A dataframe of duplex reads
#' @param simplex.df A dataframe of simplex reads from which the duplex reads were generated
#' @return The duplex dataframe with each duplex read now having the length and quality of its simplex parents. 
#' 
#' @examples 
#' duplex.with.parents.df <- duplex.parents.df(duplex.df, simplex.df);
#' summary.list$duplex <- duplex.parents.df(summart.list$duplex, summary.list$duplex);
#' @export
duplex.parents <- function(duplex, simplex) {
  if (!is.data.frame(duplex) | !is.data.frame(simplex)){ #If either the duplex or simplex parameter given is not a dataframe, return an error. 
    stop("The duplex and simplex parameter must be a dataframe.")
  } else {
  
  
  #Create two new columns with names "template_id" and "complement_id" by splitting the duplex read name into the respective simplex parents. 
  duplex["complement_id"] <- sapply(str_split(duplex$read_id, ";"), `[`,1)
  duplex["template_id"] <- sapply(str_split(duplex$read_id, ";"), `[`,2)
  #Differentiation of Duplex Length and Q-Score.
  #For comparisons later, need the column names to be different. 
  
  #Change the name of the duplex sequence length column to duplex_length.
  names(duplex)[names(duplex) == 'sequence_length_template'] <- 'duplex_sequence_length'
  #Change the name of Q-Score. 
  names(duplex)[names(duplex) == 'mean_qscore_template'] <- 'duplex_mean_qscore'
  
  
  #Pull the matching reads from the simplex dataframe that match the complement column,
  #remove all but the read id, sequence length, and mean q score, 
  #and merge into the duplex dataframe. 
  duplex <- merge(x=duplex, simplex[simplex$read_id %in% duplex$template_id,c("read_id", "sequence_length_template", "mean_qscore_template")], by.x = "template_id", by.y= "read_id")
  
  #Piped Version here for read-ability: 
  #
  #duplex <- simplex[simplex$read_id %in% duplex$template_id,] %>% 
  #   .[c("read_id", "sequence_length_template", "mean_qscore_template")] %>% 
  #   merge(duplex, ., by.x = "template_id", by.y= "read_id")
  
  
  
  
  #Change the name of the sequence_length_template column to be the template length
  names(duplex)[names(duplex) == 'sequence_length_template'] <- 'template_length'
  #Change the name of Q-Score. 
  names(duplex)[names(duplex) == 'mean_qscore_template'] <- 'template_mean_qscore'
  
  
  #Pull the matching reads from the simplex dataframe that match the complement column,
  #remove all but the read id, sequence length, and mean q score, 
  #and merge into the duplex dataframe. 
  duplex <- merge(x=duplex, simplex[simplex$read_id %in% duplex$complement_id,c("read_id", "sequence_length_template", "mean_qscore_template")], by.x = "complement_id", by.y= "read_id")
  
  
  # Piped Version here for readability:
  #   duplex <- simplex[simplex$read_id %in% duplex$complement_id,] %>% 
  #   .[c("read_id", "sequence_length_template", "mean_qscore_template")] %>%
  #   merge(duplex, ., by.x = "complement_id", by.y= "read_id")
  
  
  #Change the name of the sequence_length_template column to be the complement length
  names(duplex)[names(duplex) == 'sequence_length_template'] <- 'complement_length'
  #Change the name of Q-Score. 
  names(duplex)[names(duplex) == 'mean_qscore_template'] <- 'complement_mean_qscore'
  
  return(duplex)
  }
}
