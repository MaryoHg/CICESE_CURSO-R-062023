##------------------------------------------------------##
## Convirtiendo una base de datos a diferentes formatos ##
##------------------------------------------------------##

set.seed(123999)
library (xlsx)

	## 1. Reading or loading the database: iris dataset
	data(iris)
	
	## Saving as a CSV file:
		## También puedes usar la función utils::write.csv()
		## Ver ayuda con ??write.csv()
	
	write.table(x = iris, quote = F, sep = ',', row.names = F, file = "file.csv")
	
	## Saving as a TSV file:
	write.table(x = iris, quote = F, sep = '\t', row.names = F, file = "file.tsv")
	
	## Saving as a TXT file:
	write.table(x = iris, quote = F, sep = ' ', row.names = F, file = "file.txt")
	
	## Saving as an Excel File:
	xlsx::write.xlsx(x = iris, sheetName = "iris", row.names = F, file = 'file.xlsx')
	