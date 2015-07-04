corr <- function(directory, threshold=0){
	
	v_corr_list <- vector('numeric')
	
	if(file.exists(directory)){
		
		# if the directory is valid, get list of complete files with no of observations
		df_obs <- complete(directory)
		
		#filter the list based on threshold 
		df_obs <- subset(df_obs, df_obs$nobs > threshold)
		
		#get list of filenames to compute corr
		if(length(df_obs$id > 0)){
		
			v_files_list <- paste(sprintf("%03d",df_obs$id),".csv",sep="")
		
			#iterate through list of files and compute correlations
			for(j in seq_along(v_files_list)){
				df_data <- read.csv(file.path(directory,v_files_list[j]),stringsAsFactors=FALSE)
				v_corr_list <- c(v_corr_list, cor(x=df_data$sulfate, y=df_data$nitrate, use="pairwise.complete.obs"))
			}
	
		}
			
	}
	# return the v_corr_list
	v_corr_list	
}