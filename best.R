best <- function(state, outcome){
	
	## READ outcome data
	data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
	
	## check State and Outcome are valid
	valid_outcomes <- data.frame(Index=c(11L,17L,23L), Cause=c("heart attack","heart failure","pneumonia"))

	if( (state %in% unique(data$State)) && (outcome %in% valid_outcomes$Cause)){
		index <- valid_outcomes$Index[valid_outcomes$Cause==outcome]	
				
		## get the subset of data for the state and outcome
		chosen_data <- data[data$State==state,c(index,1,2,6,7,8,9)]
		chosen_data[,1] <- as.numeric(chosen_data[,1])
		
		## sort the data based on hospital name
		chosen_data <- chosen_data[with(chosen_data, order(chosen_data[,"Hospital.Name"])),]
		chosen_data <- subset(chosen_data, chosen_data[,1]==min(chosen_data[,1],na.rm=TRUE))
		
		## find hospital names with the lowest value of the parameter
		hospital_names <- chosen_data[,"Hospital.Name"]
		
		## As its a sorted list, return the first element
		
		return(hospital_names[1])
		
	} else if (!(state %in% unique(data$State))){
		
		stop('invalid state')
	} else if (!(outcome %in% valid_outcomes$Cause)){
		stop('invalid outcome')
	}	
	
}