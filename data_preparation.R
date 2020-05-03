#*****************************
#data_preparation.R
#Determines labels for data values
# in original dataset
#and encrypts them with FandV Fully Homomorphic Encryption
#*****************************

#library files, see README.txt for more information 
#on HomomorphicEncryption package
library(dplyr)
library(HomomorphicEncryption)
#Reads dataset from dataset in CSV format
dataset = read.csv("/home/santosh/Thesis/dataset.arff", 
header = FALSE)

cat("Data Size: ",data_size," rows","\n",sep="");
dataset <- dataset[1:data_size,];
	
#Creates a new array that will have the data as labels 
from 0 to 6 according to their #value
	dataset_labelled <- dataset

	#Updates dataset_labelled with the labels
	for (i in seq(from=3, to=ncol(dataset) - 2)){
		arr <- dataset[,i]
		m1 <- min(arr)
		m2 <- max(arr)
		r <- m2 - m1;	
		for (j in seq(from=1, to=data_size)){
			if (arr[j] < m1){
				dataset_labelled[j,i] <- as.double(round(0,0));
			}	
			else if ((arr[j] - m1)/r < 1/5){
				dataset_labelled[j,i] <- as.double(round(1,1))
			}
			else if ((arr[j] - m1)/r < 2/5){
					dataset_labelled[j,i] <- as.double(round(2,2))
			}
			else if ((arr[j] - m1)/r < 3/5){
				dataset_labelled[j,i] <- as.double(round(3,3))
			}
			else if ((arr[j] - m1)/r < 4/5){
				dataset_labelled[j,i] <- as.double(round(4,4))
			}
			else if ((arr[j] - m1)/r < 1){
				dataset_labelled[j,i] <- as.double(round(5,5))
			}
			else {
				dataset_labelled[j,i] <- as.double(round(6,6))
			}
		}
	}

	print("Dataset labelling completed")

	#Creates a training set and test set
	training_set <- dataset_labelled[1:floor(0.85 * nrow(dataset_labelled)),]
	test_set <- dataset_labelled[(floor(0.85 * nrow(dataset_labelled)) + 1):
	nrow(dataset_labelled),]

	print("Created training set and test set")
	start <- Sys.time();

	#Prepare encryption
	params <- pars("FandV",d=degree)
	key <- keygen(params)
		
	#Encrypt dataset
	z <- matrix(enc(key$pk,0),nrow(training_set),ncol(training_set))

	for (i in seq(from=1, to=nrow(training_set))){
		for (j in seq(from=1, to=ncol(training_set))){
			z[i,j] <- enc(key$pk,training_set[i,j])
		}
	}

	#Encrypting test set
	t <- matrix(enc(key$pk,0),nrow(test_set),ncol(test_set))
	for (i in seq(from=1, to=nrow(test_set))){
		for (j in seq(from=1, to=ncol(test_set))){
			t[i,j] <- enc(key$pk,test_set[i,j])
		}
	}
	
	end = Sys.time();

	print("Encryption completed")

	encryption_time_elapsed = round(as.numeric(difftime(end,start,
	units='mins')),2);

	cat("Time elapsed for encryption for degree ",degree,": ",
	encryption_time_elapsed," minutes","\n",sep="");




 

