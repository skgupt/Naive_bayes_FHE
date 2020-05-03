#*************************************************
#prob_result
#
#inputs: val (the outcome encrypted, 0 means outcome
#doesn't exist and 1 means outcome exists)
#	 y (the dataset encrypted)
#output: the probability result has value val        
#
#comments: because comparison between encrypted data
#values isn't implemented in the homomorphic encryption
#package, val must be decrypted. This may need garbage
#collection.
#**************************************************
prob_result <- function(val,y){
	c = enc(key$pk,0);
	for (i in seq(from=1,to=nrow(y))){
		c <- c + y[i,20];
	}
	if (dec(key$sk,val) == 0){
		return(enc(key$pk,nrow(y)) - c);	
	}
	else if (dec(key$sk,val) == 1){
		return(c);
	}
}

#*************************************************
#conditionalprob_withindex
#
#inputs: index (the column in the dataset of interest)
#	 val (the value at the column of the test data, encrypted)
#	 yval (the value of the outcome of the test data, encrypted)
#        y (the dataset, encrypted)
#output: the conditional probability of the column value of interest        
#
#comments: because comparison between encrypted data values
#isn't implemented in the #homomorphic encryption package, val
#must be decrypted. This may need garbage collection.
#**************************************************

conditionalprob_withindex <- function(index,val,yval,y){ 
	c <- enc(key$pk,0); 
	for (i in seq(from=1,to=nrow(y))){
		
		if (dec(key$sk,y[i,20] - yval) == 0){	
			if (dec(key$sk,y[i,index] - val) == 0){
				c <- c+enc(key$pk,1);
			}
		}
	}
	
	return(c);
}

#*************************************************
#conditionalprob
#
#inputs: val (the value at the column of the test data, encrypted)
#	 yval (the value of the outcome of the test data, encrypted)
#        y (the dataset, encrypted)
#output: the conditional probability as a function of column values        
#
#comments: because comparison between encrypted data values
#isn't implemented in the homomorphic encryption package, val must
#be decrypted. This may need garbage collection.
#**************************************************

conditionalprob <- function(vals,yval,y){
	result <- matrix(enc(key$pk,0),1,2);
	result[1,1] <- enc(key$pk,1);
	result[1,2] <- enc(key$pk,0); 
	for (i in seq(from=1, to=ncol(y)-1)){
		cp <- conditionalprob_withindex(i,vals[i],yval,y);		
		b <- enc(key$pk,as.double(dec(key$sk,result[1,1])));	
			result[1,1] <- cp * b;
       		
		numdigits <- floor(log10(dec(key$sk,result[1,1]))) + 1;

		if (numdigits > 6){
			suffix <- numdigits - 6;
			result[1,1] <- enc(key$pk,as.double(floor((dec(key$sk,
			result[1,1]))/(10^suffix))));
			result[1,2] <- result[1,2] + enc(key$pk,suffix);
		}
	}
	return(result);
}

#*************************************************
#classifier_for_output
#
#inputs: t (the columns of the test set, encrypted, excluding
# the outcome)
#	 outputvalue (the value of the outcome)
#        y (the dataset, encrypted)
#output: the classifer prediction for outputvalue        
#
#**************************************************

classifier_for_output <- function(t,outputvalue,y){

	A <- matrix(enc(key$pk,0),1,2);
	A <- conditionalprob(t,outputvalue,y);
	
	denominator <- (as.double(dec(key$sk,prob_result(outputvalue,y))))^18;
	denexponent <- 0;
	
	numdigits <- floor(log10(denominator)) + 1
	if (numdigits > 6){
			exponent <- numdigits - 6;
			denominator <- as.double(floor((denominator/(10^denexponent))));
	}

	numerator <- dec(key$sk,A[1,1]);
	numexponent <- dec(key$sk,A[1,2]);
	
	exponent <- numexponent - denexponent;
	
	prob <- (numerator/denominator) * 10^exponent;
	return(prob);
}

#*************************************************
#NB_predict
#
#inputs: t (the columns of the test set, encrypted, including the diagnosis)
#        y (the dataset, encrypted)
#output: the classifer prediction        
#
#**************************************************

NB_predict <- function(t,y){
	x <- t[1:19];
	yval <- t[20];
	
	if (dec(key$sk,yval) == 0){
		yval_alternate = enc(key$pk,1);
	}
	else{
		yval_alternate = enc(key$pk,0);
	}
	a <- classifier_for_output(x,yval,y);
	b <- classifier_for_output(x,yval_alternate,y);

	return(a/(a+b));
}


