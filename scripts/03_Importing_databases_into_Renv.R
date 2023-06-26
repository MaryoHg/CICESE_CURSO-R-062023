## -------------------------------------- ##
## Importing databases into R environment ##
## -------------------------------------- ##
set.seed(123999)


	## Functions to import data frames into R envinronment and its mainly type (not restricted to)
	utils::read.csv()	## to load a csv dataframe, including from the internet
	utils::read.delim()	## to load a tsv, or any dataframe
	utils::read.table()	## to load a tsv, or text, or any dataframe
	utils::read.delim()	## to load a tsv, or any dataframe
	
	## From Microsoft Excel databases only
	readxl::read_excel(path = '/PATH/file.xlsx', sheet = 1, col_names = T)
	xlsx::read.xlsx(file = '/PATH/file')
	
	## Other statisticals programs databases
	haven::read_sas()
	haven::read_spss()
	
	
## -------------------------------------------------------------- ##
## USING ALL FUNCTION TO READ DIFFERENT TYPES OF DATABASES INTO R ##
## -------------------------------------------------------------- ##
	
	## From the internet: a csv file
	## Including from the web:
	df <- utils::read.csv(file = 'https://raw.githubusercontent.com/OHI-Science/data-science-training/master/data/gapminder_wide.csv')
	
	## Reading all types of dataframes with a single function:
	df2 <- read.delim(file = 'data/file.tsv', stringsAsFactors = T)
	df2 <- read.delim(file = 'data/file.csv', header = T, sep = ',', stringsAsFactors = F)
	df2 <- read.delim(file = 'data/file.txt', header = T, sep = ' ', stringsAsFactors = F)
	
	str(df2)
	
	
##
	