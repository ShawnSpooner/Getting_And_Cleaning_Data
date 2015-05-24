##Getting and Cleaning Data
To run this script load it into R and execute it. This will create a local data directory, download the data, unzip the zip file, and run the analysis to generate the tidy dataset. The tidy dataset will then be written into the data directory as tidy.txt

###Fields Dropped
All observations of discrete variables were dropped from the orinal data set, keeping only the mean and standard deviations over observations.

###Naming Conventions
Field name consituents are renamed as follows:

* f -> frequency
* t -> time
* std() -> deviation
* mean() -> mean 