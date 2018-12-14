library(tidyverse)
#this will give out the graph of anlayzed result of how many years of term were lasting for each state from 1998 to 2016.
#this contains a data cleaning process too.

#assumption: Eletion_Data is the original data which will be input for the function argument.

analyze_length_of_term_per_year = function(Election_Data){
  # first, slice and concatenate the necessary columns from the original data:
  e_data <- Election_Data[c("District","Winning_Party","Incumbent_Party","First_year_incumbent_elected","Election_Year")]
  
  #then, we need to convert each column to numeric for the convenience for anlaysis
  
  e_data$numeric_first_yr_incumbent_elected <- as.numeric(e_data$First_year_incumbent_elected)
  e_data$numeric_election_yr <- as.numeric(e_data$Election_Year)
  
  #now we will loop through each row and compare whether incumbent party and winning party were changed.
  #if changed, we will put NA. later, they will be removed since we won't count this type of the row.
  # before loop through, we will remove missing value rows to avoid compile error at logical comparison
  e_data <- e_data[complete.cases(e_data),]
  
  N <- length(e_data$District)
  for (i in 1: N) {
    if((e_data$Winning_Party[i]) != (e_data$Incumbent_Party[i])){
      e_data$Winning_Party[i] <- NA
    }
  }
  #then we need to slice non necessary columns since no need to consider anymore
  
  e_data <-e_data[c("District","Winning_Party","numeric_first_yr_incumbent_elected","numeric_election_yr")]
  
  #now, all unnecessary information(rows containing non-numeric character numbers) will be removed at here
  #also, change the name of the winning parties to current parties
  e_data <- e_data[complete.cases(e_data),]
  e_data$term_length_parties<-(e_data$numeric_election_yr - e_data$numeric_first_yr_incumbent_elected)
  
  colnames(e_data) <- c("districit","current_parties","numeric_first_yr_incumbent_elected","numeric_election_yr","term_length_parties")
  
  #things to do: make plot of showing the 2016's district VS length 
  #-> maybe combine all district vs mean(length)
  
  
  # new dataframe, which has STATE(combining districts) and avg of last column(term_length_parties)
  
  e_data_2 <- mutate(e_data, state = substr(districit,1,4))
  e_data_2 <- group_by(e_data_2, state, numeric_election_yr)
  e_data_2 <- summarise(e_data_2, avg_term_length_parties = mean(term_length_parties))
  
  #Ohio has negative number which seems incorrectly recorded as -1.172932e+06(2012)... so I will put avg of 2010 and 2014 temporarily.
  #e_data_2 <- filter(e_data_2, state!='Ohio')
  #a <- e_data_2$avg_term_length_parties[, e_data_2$state=='Ohio' && e_data_2$numeric_election_yr==2010]
  #b <- e_data_2$avg_term_length_parties[, c(e_data_2$state=='Ohio', e_data_2$numeric_election_yr==2014)]
  
  ohio_2012 <- filter(e_data_2, state=='Ohio', numeric_election_yr==2010)$avg_term_length_parties
  ohio_2014 <- filter(e_data_2, state=='Ohio', numeric_election_yr==2014)$avg_term_length_parties
  
  
  e_data_2$avg_term_length_parties[e_data_2$avg_term_length_parties<0] <- mean(ohio_2012,ohio_2014)
  
  
  # plot for each state from 1998 - 2016: liner plot, color with the legend
  ggplot(e_data_2, aes(x=numeric_election_yr, y=avg_term_length_parties, group=state, color=state)) + geom_line() + geom_point() 
  }