#' this function wil counts how many parties were changed when we compare incumbent vs winning parties for EACH YEAR. 
#' as a result, below will be shown
#' cat("last year",currentYear,"", count, "\n") will be executed for each year while iterating and at the end you will see below
#' last year 2016  13
#' last year 2014  21
#' last year 2012  51
#' last year 2010  71
#' last year 2008  32
#' last year 2006  32
#' last year 2004  15
#' last year 2002  31
#' last year 2000  20
#' last year 1998  17
#' @param inputed data of election of years from 1998 to 2016
#' @export
#' @example 
#' counting_changed_parties_incumb_vs_wining(Election_Data)


load('Election_Data2.Rdata')
N <- length(Election_Data$District)

counting_changed_parties_incumb_vs_wining = function(Election_Data){
    currentYear <- 0
    for (i in 1: N) {
        if(currentYear != Election_Data$Election_Year[i]) {
            if(currentYear!=0){
            cat("last year",currentYear,"", count, "\n")
            }
            count <- 0

        }
        currentYear <-Election_Data$Election_Year[i]
        inc <- Election_Data$Incumbent_Party[i]
        win <- Election_Data$Winning_Party[i]
        if (!is.na(inc) & !is.na(win) & inc !=win) count <- count +1;
        if (is.na(inc) | is.na(win)) count <- count +1;
    }
     cat("last year",currentYear,"", count, "\n")
}
