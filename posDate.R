 pos_Date <- function(month){
   l_day <- as.character(lastDay[substr(month,1,3)])
   str_date <- paste(l_day, month, sep="-")
   message(str_date)
   return(as.Date(str_date, "%d-%b-%y"))
 }
