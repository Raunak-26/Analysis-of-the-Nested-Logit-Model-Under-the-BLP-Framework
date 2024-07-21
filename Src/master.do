**master file**

clear all
set more off, perm

* set directories
gl root "C:\Users\Raunak\OneDrive\Desktop\Master Thesis" //root directory
	gl src "$root\src\" //source code directory
	gl in "$root\in\" //input file directory
	gl out "$root\out\" //output file directory
	gl temp "$root\temp\" //temporary file directory

* prepare data

qui do "$src\empirical_application_NL.do"

