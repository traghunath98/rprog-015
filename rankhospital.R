rankhospital <- function(state,outcome,num){
	
	## READ outcome data
	data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
	
	## check State and Outcome are valid
	
	valid_outcomes <- data.frame(Index=c(11L,17L,23L), Cause=c("heart attack","heart failure","pneumonia"))

	if( (state %in% unique(data$State)) && (outcome %in% valid_outcomes$Cause)){
		index <- valid_outcomes$Index[valid_outcomes$Cause==outcome]	
				
		## get the subset of data for the state and outcome
		data[,index] <- as.numeric(data[,index])
		chosen_data <- subset(data, (data$State==state & !is.na(data[,index])))
		
		chosen_data <- chosen_data[,c(index,1,2,6,7,8,9)]
		
		## sort the data based on outcome and then hospital name
		chosen_data <- chosen_data[order(chosen_data[,1],chosen_data[,3]),]
		data_size <- nrow(chosen_data)
		
		## assign a rank to the data
		chosen_data$Rank <- 1:data_size		
		
		## validate the num argument
		if(class(num)=="numeric"){
			if(num <= data_size){
				hospital_name <- chosen_data[ chosen_data$Rank==num,"Hospital.Name"]
				return(hospital_name)
			} else {
				return(NA)
			}
		} else if(class(num)=="character"){
			if(num=="best"){
				return(chosen_data[1,"Hospital.Name"])
			} else if(num=="worst") {
				return(chosen_data[data_size,"Hospital.Name"])
			} else {
				stop("invalid num parameter")
			}
		
		}
			
	} else if (!(state %in% unique(data$State))){
		
		stop('invalid state')
		
	} else if (!(outcome %in% valid_outcomes$Cause)){
		
		stop('invalid outcome')
	
	}	
	
}