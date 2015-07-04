rankall <- function(outcome, num = "best"){
	
	## READ outcome data
	data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
	
	## Valid States and Outcomes
	all_states <- sort(unique(data$State))
	valid_outcomes <- data.frame(Index=c(11L,17L,23L), Cause=c("heart attack","heart failure","pneumonia"))

	if(outcome %in% valid_outcomes$Cause){
		
		index <- valid_outcomes$Index[valid_outcomes$Cause==outcome]	
		data[,index] <- as.numeric(data[,index])
		state_hospital <- data.frame( hospital=character(), state=character(), stringsAsFactors=FALSE)
				
		## for each state, find the hospital of the given rank
		for(i in 1:length(all_states)){
		
			state <- all_states[i]		

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
					
					state_hospital[i,] <- chosen_data[chosen_data$Rank==num,c("Hospital.Name","State")]
					
				} else {
			
					state_hospital[i,] <- c(NA,state)
		
				}
			} else if(class(num)=="character"){
				if(num=="best"){
					
					state_hospital[i,] <- chosen_data[chosen_data$Rank==1,c("Hospital.Name","State")]
					
				} else if(num=="worst") {
					
					state_hospital[i,] <- chosen_data[chosen_data$Rank==data_size,c("Hospital.Name","State")]
					
				} else {
					
					state_hospital[i,] <- c(NA,state)
					
				}
		
			}	
			
			}
			return(state_hospital)
			 
		} else {
		
			stop('invalid outcome')
	
		}		
	}