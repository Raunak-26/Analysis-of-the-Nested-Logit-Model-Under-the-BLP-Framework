*** Empirical Application using mergesim package on cars data ***

* Install the package*

//	net from http://www.bjornerstedt.org/stata/mergersim
//	net install mergersim
//ssc install estout

*Get updates*

//	adoupdate, update


use "$in\cars1.dta", clear

est clear  // clear the stored estimates
estpost summarize country segment domestic firm qu price horsepower fuel width height pop ngdp
ereturn list // list the stored locals
esttab using "summ_table.tex", replace ///
	cells ("sum(fmt(%6.0fc)) mean sd min(fmt(%6.0fc)) max(fmt(%6.0fc)) count(fmt(%6.0fc))")nonumber ///
	nomtitle nonote noobs booktabs ///
	collabels("Sum" "Mean" "SD" "Min" "Max" "N")


label list firm
label list market

egen yearcountry=group(year country), label
xtset co yearcountry

gen log_pop = ln(pop)
gen msize = pop/4

//gen share = qu/msize
//egen sum_share = total(share), by(country year)
//gen share0 = 1 - sum_share
//sum share share0

//gen M_ls = ln(share/share0)

//egen denom1 = total(share), by(country year segment domestic)
//gen M_lsjh = ln(share/denom1)

//egen denom = total(share),  by(country year segment)
//gen M_lshg = ln(denom1/denom)

mergersim init, nests(segment domestic) price(price) quantity(qu) marketsize(msize) firm(firm)

egen charac = total(horsepower + fuel + width + height + weight), by(segment firm)
egen instr = total(horsepower + fuel + width + height ), by(segment firm brand)

ivregress 2sls M_ls (price = horsepower fuel width height instr) M_lsjh M_lshg horsepower fuel width height log_pop, vce(robust)


estat firststage
estat endogenous

mergersim market if year == 1990
mergersim market if year == 1990 & country == 2 //France
mergersim market if year == 1990 & country == 3 //Germany

mergersim market if year == 1990 & country == 2 & segment == 1
mergersim market if year == 1990 & country == 2 & segment == 2
mergersim market if year == 1990 & country == 2 & segment == 3
mergersim market if year == 1990 & country == 2 & segment == 4
mergersim market if year == 1990 & country == 2 & segment == 5

mergersim market if year == 1990 & country == 3 & segment == 1
mergersim market if year == 1990 & country == 3 & segment == 2
mergersim market if year == 1990 & country == 3 & segment == 3
mergersim market if year == 1990 & country == 3 & segment == 4
mergersim market if year == 1990 & country == 3 & segment == 5


mergersim simulate if year == 1990 & country == 2, seller(16) buyer(18) detail 

gen perc_price_ch_FR=M_price_ch*100
graph bar (mean) perc_price_ch_FR if country==2&year==1990, ///
	over(firm, sort(perc_price_ch) descending label(angle(vertical))) ///
	ytitle(Percentage) title(Average percentage price increase per firm)


mergersim simulate if year == 1990 & country == 3, seller(26) buyer(15) detail 

gen perc_price_ch_DE=M_price_ch*100
graph bar (mean) perc_price_ch_DE if country==3&year==1990, ///
	over(firm, sort(perc_price_ch) descending label(angle(vertical))) ///
	ytitle(Percentage) title(Average percentage price increase per firm)
	
* Divestiture*

generate firm_rem=firm
replace firm_rem=16 if firm==18 // original merger
replace firm_rem= 5 if brand == 20 // divestiture
quietly mergersim init, nests(segment domestic) price(price) quantity(qu) marketsize(msize) firm(firm)
quietly mergersim simulate if year == 1990 & country == 2, seller(16) buyer(18)
mergersim simulate if year == 1990 & country == 2, newfirm(firm_rem) detail

gen perc_price_ch_FR_divest=M_price_ch*100
graph bar (mean) perc_price_ch_FR_divest if country==2&year==1990, ///
	over(firm, sort(perc_price_ch) descending label(angle(vertical))) ///
	ytitle(Percentage) title(Average percentage price increase per firm)


	
save "$temp\empirical_application.dta", replace