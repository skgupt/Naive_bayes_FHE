#*************************************************
#analyze_predictions
#
#inputs: z (the dataset encrypted)
#	 t (the test set encrypted)
#output: a row vector including statistical results such as
# average prediction accuracy and average computation time for classifier  
#**************************************************

analyze_predictions <- function(z,t){
	predictions <- matrix(0,1,nrow(t))
	times <- matrix(0,1,nrow(t));
	counter <- 0;
	i <- 1;
	j <- 1;
	imax <- nrow(t);
	while (i <= imax){      cat("row=",i,"\n",sep="");
		start <- Sys.time();	
		x <- NB_predict(t[i,],z);
		if (is.finite(x)){
			predictions[1,j] <- x
		}		
		end <- Sys.time();
		times[i] <- as.numeric(difftime(end,start,units='mins'));
if (is.finite(x)){			cat("Conditional Probability: ",predictions[1,j],"\n",sep="");
}
else{
	print("Result is not finite. Not included in analysis.");
}		
		cat("Elapsed time: ",times[i]," minutes","\n",sep="");
		if(!(is.finite(x))){
			j <- j-1;
			
		}
		if (i == floor(imax * counter * 0.10)){
			cat("Predictions completed: ",counter * 10,"%",sep="");
		count <- counter + 1;
		}
		j <- j+1;
		i <- i + 1;
	}

	A <- matrix(0,1,4);
	A[1,1] <- as.numeric(round(mean(predictions),3));
	A[1,2] <- as.numeric(round(sd(predictions),3));
	A[1,3] <- as.numeric(round(mean(times),2));
	A[1,4] <- as.numeric(round(sd(times),2));

	return(A);
}

#*************************************************
#create_statistics
#
#inputs: predictions_table (statistics of Naive-Bayes predictions)
#	 
#output: formatted table of statistics  
#**************************************************

create_statistics <- function(predictions_table){

	colnames <- c("Prediction Accuracy (average)", "Prediction Accuracy (standard deviation)",
	"Computational Time (average)", "Computational Time (standard deviation)");
	rownames <- c("250","500", "750","1000", "1151");

write.table(as.table(predictions_table),file=paste("Tables/predictions_table_degree",degree,".txt",sep=""),
col.names=colnames,row.names=rownames,quote=FALSE,sep="\t");
}


