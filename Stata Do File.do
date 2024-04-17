//Vaughn Hajra


//First, summary statistics
import delimited "/Users/vaughn/Desktop/FinalData.csv", clear varnames(1)

//drop NA values
drop if close == "NA"

drop if event_id == 0

drop v1
drop v2

//convert to float
destring close, replace

destring day, replace
destring afinn, replace
destring bing, replace
destring retwe, replace

//make new date columns
generate new_date = date(date, "YMD")
format new_date %tdNN/DD/CCYY

generate new_event_date = date(event_date, "YMD")
format new_event_date %tdNN/DD/CCYY

drop date
drop event_date

sort event_id new_date
by event_id: gen datenum = _n

drop if new_date < date("2015-06-16", "YMD") //We want AFTER Trump declared candidacy for office

count


histogram afinn, aspectratio(1) //normally distributed with SD = ~4
histogram bing, aspectratio(1) //normally distributed with SD = ~1.75


duplicates drop event_id, force

//Now finding values for table
sort event_id
count //311 total tweets

//Positive tweets
count if bing > 0 & afinn > 0 //85
count if bing > 0 & afinn == 0 //13
count if bing == 0 & afinn > 0 //21

//Neutral tweets
count if bing == 0 & afinn == 0 //39

//Negative tweets
count if bing < 0 & afinn < 0 //97
count if bing < 0 & afinn == 0 //11
count if bing == 0 & afinn < 0 //26

//Contradictory tweets
count if bing < 0 & afinn > 0 //13
count if bing > 0 & afinn < 0 //6

//Now let's look at the unique events
levelsof ticker
tab ticker

sort ticker
by ticker: summarize bing afinn


levelsof sector
tab sector

sort sector
summarize bing afinn if sector == "Consumer Discretionary"
summarize bing afinn if sector == "Energy"
summarize bing afinn if sector == "Financials"
summarize bing afinn if sector == "Health Care"
summarize bing afinn if sector == "Industrials"
summarize bing afinn if sector == "Information Technology"
summarize bing afinn if sector == "Telecommunications Services"


graph box bing, over(sector)


histogram retweets, frequency





//First setting up Fama-French dataset for stata
import delimited "/Users/vaughn/Desktop/Fama-French Data - Sheet1.csv", clear varnames(1)





//next, let's calculate abnormal returns for all tweets!
//Events study stata file
import delimited "/Users/vaughn/Desktop/FinalData.csv", clear varnames(1)

merge m:1 date using "/Users/vaughn/Desktop/FFdata.dta"

//drop NA values
drop if close == "NA"

//convert to float
destring close, replace

destring day, replace
destring afinn, replace
destring bing, replace
destring retwe, replace





//make new date columns
generate new_date = date(date, "YMD")
format new_date %tdNN/DD/CCYY

generate new_event_date = date(event_date, "YMD")
format new_event_date %tdNN/DD/CCYY

drop date
drop event_date

sort event_id new_date
by event_id: gen datenum = _n

drop if new_date < date("2015-06-16", "YMD") //We want AFTER Trump declared candidacy for office

//now starting to follow tutorial

gen mktclose = close if event_id == 0

bysort new_date (mktclose): replace mktclose = mktclose[1]

sort event_id new_date

drop if event_id == 0

//now to calculate CAAR
drop if mktclose ==.
rename day dif

destring dif, replace

by event_id: gen event_window = 1 if dif >= -10 & dif <= 10

egen count_event_obs=count(event_window), by(event_id)



//Now generate estimation window

by event_id: gen estimation_window = 1 if dif < -10 & dif >= -210

egen count_est_obs = count(estimation_window), by(event_id)

replace event_window = 0 if event_window==.
replace estimation_window = 0 if estimation_window==.

tab event_id if count_event_obs < 10
tab event_id if count_est_obs < 200


drop if count_event_obs < 3
drop if count_est_obs < 100
//declareing panel dataset
xtset event_id datenum
//generate return
gen return = (close/L.close) - 1

replace return = return - rf

//generate market return
gen mkt_return = (mktclose - L.mktclose) - 1

replace mkt_return = mkt_return - mktrf

//generate normal return for each company using a for loop
gen normal_return =.
egen id=group(event_id)

gen SE_residual =.

levelsof event_id //288 unique variables

forvalues i=1(1)288 {
	reg return mkt_return hml smb if id == `i' & estimation_window==1
	predict p if id == `i'
	predict std_err_residual, stdr
	replace SE_residual = std_err_residual if id == `i'
	replace normal_return = p if id == `i' /*& event_window ==1*/
	drop p
	drop std_err_residual
}

sort event_id datenum
gen abnormal_return = return - normal_return

by event_id: egen cumulative_abnormal_return = total(abnormal_return) if event_window==1

//now calculate standard error
sort event_id datenum
by event_id: egen ar_sd_est = sd(abnormal_return) if estimation_window == 1

//then extend to all observations within id
bysort event_id: egen ar_sd = max(ar_sd_est)

corr SE_residual ar_sd

drop ar_sd_est // as they're about the same

gen test = (1/sqrt(20)) * (cumulative_abnormal_return / ar_sd)

list event_id cumulative_abnormal_return test if dif==0

reg cumulative_abnormal_return if dif==0

//now let's add controls all together
destring retweets, replace

destring afinn, replace

destring bing, replace

gen interaction_bing = retweets * bing
gen interaction_afinn = retweets * afinn

gen log_retweets = ln(retweets)


reg cumulative_abnormal_return log_retweets bing if dif==0

reg cumulative_abnormal_return log_retweets afinn if dif==0

reg cumulative_abnormal_return retweets bing if dif==0

reg cumulative_abnormal_return retweets afinn if dif==0



// Aggregate the data by day and calculate the daily average
collapse (mean) daily_avg_ar=abnormal_return, by(dif)

drop if dif < -10

//calculating daily average car
gen daily_avg_car = sum(daily_avg_ar)


// Create a line graph of the daily average cumulative abnormal returns
twoway line daily_avg_car dif, ///
    xtitle("Days") ytitle("Cumulative Average Abnormal Return") ///
    title("Cumulative Average Abnormal Returns for All Tweets") ///
	yscale(range(-.015,.015))
//Then use graph editor to adjust tickmarks and add line at y = 0
	
	
	
	
	
	
	
	

*NOW WE DO JUST POSITIVE TWEETS*


//Events study stata file
import delimited "/Users/vaughn/Desktop/FinalData.csv", clear varnames(1)

merge m:1 date using "/Users/vaughn/Desktop/FFdata.dta"
//drop NA values
drop if close == "NA"

//convert to float
destring close, replace



//make new date columns
generate new_date = date(date, "YMD")
format new_date %tdNN/DD/CCYY

generate new_event_date = date(event_date, "YMD")
format new_event_date %tdNN/DD/CCYY

drop date
drop event_date

sort event_id new_date
by event_id: gen datenum = _n

drop if new_date < date("2015-06-16", "YMD") //We want AFTER Trump declared candidacy for office

//now starting to follow tutorial

gen mktclose = close if event_id == 0

bysort new_date (mktclose): replace mktclose = mktclose[1]

sort event_id new_date

drop if event_id == 0

destring day, replace
destring afinn, replace
destring bing, replace
destring retwe, replace

drop if bing < 0
drop if afinn < 0
drop if bing == 0 & afinn == 0


//now to calculate CAAR
drop if mktclose ==.
rename day dif

destring dif, replace

by event_id: gen event_window = 1 if dif >= -10 & dif <= 10

egen count_event_obs=count(event_window), by(event_id)



//Now generate estimation window

by event_id: gen estimation_window = 1 if dif < -10 & dif >= -210

egen count_est_obs = count(estimation_window), by(event_id)

replace event_window = 0 if event_window==.
replace estimation_window = 0 if estimation_window==.

tab event_id if count_event_obs < 10
tab event_id if count_est_obs < 200


drop if count_event_obs < 3
drop if count_est_obs < 100
//declareing panel dataset
xtset event_id datenum
//generate return
gen return = (close/L.close) - 1

replace return = return - rf

//generate market return
gen mkt_return = (mktclose - L.mktclose) - 1

replace mkt_return = mkt_return - mktrf

//generate normal return for each company using a for loop
gen normal_return =.
egen id=group(event_id)

gen SE_residual =.

levelsof event_id //gives 107 unique events

forvalues i=1(1)107 {
	reg return mkt_return hml smb if id == `i' & estimation_window==1
	predict p if id == `i'
	predict std_err_residual, stdr
	replace SE_residual = std_err_residual if id == `i'
	replace normal_return = p if id == `i' /*& event_window ==1*/
	drop p
	drop std_err_residual
}

sort event_id datenum
gen abnormal_return = return - normal_return

by event_id: egen cumulative_abnormal_return = total(abnormal_return) if event_window==1

//now calculate standard error
sort event_id datenum
by event_id: egen ar_sd_est = sd(abnormal_return) if estimation_window == 1

//then extend to all observations within id
bysort event_id: egen ar_sd = max(ar_sd_est)

corr SE_residual ar_sd

drop ar_sd_est // as they're about the same

gen test = (1/sqrt(20)) * (cumulative_abnormal_return / ar_sd)

list event_id cumulative_abnormal_return test if dif==0

reg cumulative_abnormal_return if dif==0


// Aggregate the data by day and calculate the daily average
collapse (mean) daily_avg_ar=abnormal_return, by(dif)

drop if dif < -10

//calculating daily average car
gen daily_avg_car = sum(daily_avg_ar)


// Create a line graph of the daily average cumulative abnormal returns
twoway line daily_avg_car dif, ///
    xtitle("Days") ytitle("Cumulative Average Abnormal Return") ///
    title("Cumulative Average Abnormal Returns for Positive Tweets") ///
	yscale(range(-.015,.015))
//Then use graph editor to adjust tickmarks and add line at y = 0
	

	
	
	
	

	
	
*NOW WE DO JUST NEGATIVE  TWEETS*


//Events study stata file
import delimited "/Users/vaughn/Desktop/FinalData.csv", clear varnames(1)

merge m:1 date using "/Users/vaughn/Desktop/FFdata.dta"


//drop NA values
drop if close == "NA"

//convert to float
destring close, replace



//make new date columns
generate new_date = date(date, "YMD")
format new_date %tdNN/DD/CCYY

generate new_event_date = date(event_date, "YMD")
format new_event_date %tdNN/DD/CCYY

drop date
drop event_date

sort event_id new_date
by event_id: gen datenum = _n

drop if new_date < date("2015-06-16", "YMD") //We want AFTER Trump declared candidacy for office

//now starting to follow tutorial

gen mktclose = close if event_id == 0

bysort new_date (mktclose): replace mktclose = mktclose[1]

sort event_id new_date

drop if event_id == 0

destring day, replace
destring afinn, replace
destring bing, replace
destring retwe, replace

drop if bing > 0
drop if afinn > 0
drop if bing == 0 & afinn == 0

//now to calculate CAAR
drop if mktclose ==.
rename day dif

destring dif, replace

by event_id: gen event_window = 1 if dif >= -10 & dif <= 10

egen count_event_obs=count(event_window), by(event_id)



//Now generate estimation window

by event_id: gen estimation_window = 1 if dif < -10 & dif >= -210

egen count_est_obs = count(estimation_window), by(event_id)

replace event_window = 0 if event_window==.
replace estimation_window = 0 if estimation_window==.

tab event_id if count_event_obs < 10
tab event_id if count_est_obs < 200


drop if count_event_obs < 3
drop if count_est_obs < 100
//declareing panel dataset
xtset event_id datenum
//generate return
gen return = (close/L.close) - 1

replace return = return - rf

//generate market return
gen mkt_return = (mktclose - L.mktclose) - 1

replace mkt_return = mkt_return - mktrf

//generate normal return for each company using a for loop
gen normal_return =.
egen id=group(event_id)

gen SE_residual =.

levelsof event_id //gives 131 unique events

forvalues i=1(1)131 {
	reg return mkt_return smb hml if id == `i' & estimation_window==1
	predict p if id == `i'
	predict std_err_residual, stdr
	replace SE_residual = std_err_residual if id == `i'
	replace normal_return = p if id == `i' /*& event_window ==1*/
	drop p
	drop std_err_residual
}

sort event_id datenum
gen abnormal_return = return - normal_return

by event_id: egen cumulative_abnormal_return = total(abnormal_return) if event_window==1

//now calculate standard error
sort event_id datenum
by event_id: egen ar_sd_est = sd(abnormal_return) if estimation_window == 1

//then extend to all observations within id
bysort event_id: egen ar_sd = max(ar_sd_est)

corr SE_residual ar_sd

drop ar_sd_est // as they're about the same

gen test = (1/sqrt(20)) * (cumulative_abnormal_return / ar_sd)

list event_id cumulative_abnormal_return test if dif==0

reg cumulative_abnormal_return if dif==0



* NOW GRAPHING *

// Aggregate the data by day and calculate the daily average
collapse (mean) daily_avg_ar=abnormal_return, by(dif)

drop if dif < -10

//calculating daily average car
gen daily_avg_car = sum(daily_avg_ar)




// Create a line graph of the daily average cumulative abnormal returns
twoway line daily_avg_car dif, ///
    xtitle("Days") ytitle("Cumulative Average Abnormal Return") ///
    title("Cumulative Average Abnormal Returns for Negative Tweets") ///
	yscale(range(-.015,.015))
//Then use graph editor to adjust tickmarks and add line at y = 0
	
	
	
	
	
	
	
	
	
	
	
	
*Now controlling for retweets - first, more reach *
	
//Events study stata file
import delimited "/Users/vaughn/Desktop/FinalData.csv", clear varnames(1)

merge m:1 date using "/Users/vaughn/Desktop/FFdata.dta"

//drop NA values
drop if close == "NA"

//convert to float
destring close, replace

destring day, replace
destring afinn, replace
destring bing, replace
destring retwe, replace

//make new date columns
generate new_date = date(date, "YMD")
format new_date %tdNN/DD/CCYY

generate new_event_date = date(event_date, "YMD")
format new_event_date %tdNN/DD/CCYY

drop date
drop event_date

sort event_id new_date
by event_id: gen datenum = _n

drop if new_date < date("2015-06-16", "YMD") //We want AFTER Trump declared candidacy for office

//now starting to follow tutorial

gen mktclose = close if event_id == 0

bysort new_date (mktclose): replace mktclose = mktclose[1]

sort event_id new_date

drop if event_id == 0

destring retweets, replace
summarize retweets

drop if retweets > 15000

//now to calculate CAAR
drop if mktclose ==.
rename day dif

destring dif, replace

by event_id: gen event_window = 1 if dif >= -10 & dif <= 10

egen count_event_obs=count(event_window), by(event_id)



//Now generate estimation window

by event_id: gen estimation_window = 1 if dif < -10 & dif >= -210

egen count_est_obs = count(estimation_window), by(event_id)

replace event_window = 0 if event_window==.
replace estimation_window = 0 if estimation_window==.

tab event_id if count_event_obs < 10
tab event_id if count_est_obs < 200


drop if count_event_obs < 3
drop if count_est_obs < 100
//declareing panel dataset
xtset event_id datenum
//generate return
gen return = (close/L.close) - 1

replace return = return - rf

//generate market return
gen mkt_return = (mktclose - L.mktclose) - 1

replace mkt_return = mkt_return - mktrf

//generate normal return for each company using a for loop
gen normal_return =.
egen id=group(event_id)

gen SE_residual =.



levelsof event_id //132 unique variables less, 159 greater

forvalues i=1(1)159 {
	reg return mkt_return smb hml if id == `i' & estimation_window==1
	predict p if id == `i'
	predict std_err_residual, stdr
	replace SE_residual = std_err_residual if id == `i'
	replace normal_return = p if id == `i' /*& event_window ==1*/
	drop p
	drop std_err_residual
}

sort event_id datenum
gen abnormal_return = return - normal_return

by event_id: egen cumulative_abnormal_return = total(abnormal_return) if event_window==1

//now calculate standard error
sort event_id datenum
by event_id: egen ar_sd_est = sd(abnormal_return) if estimation_window == 1

//then extend to all observations within id
bysort event_id: egen ar_sd = max(ar_sd_est)

corr SE_residual ar_sd

drop ar_sd_est // as they're about the same

gen test = (1/sqrt(20)) * (cumulative_abnormal_return / ar_sd)

list event_id cumulative_abnormal_return test if dif==0

reg cumulative_abnormal_return if dif==0


// Aggregate the data by day and calculate the daily average
collapse (mean) daily_avg_ar=abnormal_return, by(dif)

drop if dif < -10

//calculating daily average car
gen daily_avg_car = sum(daily_avg_ar)




// Create a line graph of the daily average cumulative abnormal returns
twoway line daily_avg_car dif, ///
    xtitle("Days") ytitle("Average Cumulative Abnormal Return") ///
    title("Average Cumulative Abnormal Returns Over Time - More Reach") ///
	yscale(range(-.010,.010))
//Then use graph editor to adjust tickmarks and add line at y = 0
	
	
	
	
	
	
	
	
*Now controlling for retweets - next, less reach *
	
//Events study stata file
import delimited "/Users/vaughn/Desktop/FinalData.csv", clear varnames(1)

merge m:1 date using "/Users/vaughn/Desktop/FFdata.dta"

//drop NA values
drop if close == "NA"

//convert to float
destring close, replace

destring day, replace
destring afinn, replace
destring bing, replace
destring retwe, replace

//make new date columns
generate new_date = date(date, "YMD")
format new_date %tdNN/DD/CCYY

generate new_event_date = date(event_date, "YMD")
format new_event_date %tdNN/DD/CCYY

drop date
drop event_date

sort event_id new_date
by event_id: gen datenum = _n

drop if new_date < date("2015-06-16", "YMD") //We want AFTER Trump declared candidacy for office

//now starting to follow tutorial

gen mktclose = close if event_id == 0

bysort new_date (mktclose): replace mktclose = mktclose[1]

sort event_id new_date

drop if event_id == 0

destring retweets, replace
summarize retweets

drop if retweets < 15000

//now to calculate CAAR
drop if mktclose ==.
rename day dif

destring dif, replace

by event_id: gen event_window = 1 if dif >= -10 & dif <= 10

egen count_event_obs=count(event_window), by(event_id)



//Now generate estimation window

by event_id: gen estimation_window = 1 if dif < -10 & dif >= -210

egen count_est_obs = count(estimation_window), by(event_id)

replace event_window = 0 if event_window==.
replace estimation_window = 0 if estimation_window==.

tab event_id if count_event_obs < 10
tab event_id if count_est_obs < 200


drop if count_event_obs < 3
drop if count_est_obs < 100
//declareing panel dataset
xtset event_id datenum
//generate return
gen return = (close/L.close) - 1

replace return = return - rf

//generate market return
gen mkt_return = (mktclose - L.mktclose) - 1

replace mkt_return = mkt_return - mktrf

//generate normal return for each company using a for loop
gen normal_return =.
egen id=group(event_id)

gen SE_residual =.



levelsof event_id //132 unique variables less, 159 greater

forvalues i=1(1)132 {
	reg return mkt_return smb hml if id == `i' & estimation_window==1
	predict p if id == `i'
	predict std_err_residual, stdr
	replace SE_residual = std_err_residual if id == `i'
	replace normal_return = p if id == `i' /*& event_window ==1*/
	drop p
	drop std_err_residual
}

sort event_id datenum
gen abnormal_return = return - normal_return

by event_id: egen cumulative_abnormal_return = total(abnormal_return) if event_window==1

//now calculate standard error
sort event_id datenum
by event_id: egen ar_sd_est = sd(abnormal_return) if estimation_window == 1

//then extend to all observations within id
bysort event_id: egen ar_sd = max(ar_sd_est)

corr SE_residual ar_sd

drop ar_sd_est // as they're about the same

gen test = (1/sqrt(20)) * (cumulative_abnormal_return / ar_sd)

list event_id cumulative_abnormal_return test if dif==0

reg cumulative_abnormal_return if dif==0


// Aggregate the data by day and calculate the daily average
collapse (mean) daily_avg_ar=abnormal_return, by(dif)

drop if dif < -10

//calculating daily average car
gen daily_avg_car = sum(daily_avg_ar)




// Create a line graph of the daily average cumulative abnormal returns
twoway line daily_avg_car dif, ///
    xtitle("Days") ytitle("Average Cumulative Abnormal Return") ///
    title("Average Cumulative Abnormal Returns Over Time - Less Reach") ///
	yscale(range(-.010,.010))
//Then use graph editor to adjust tickmarks and add line at y = 0







	
	
	
	
	
*Now controlling just individual sectors (replace n and name)*
	
//Events study stata file
import delimited "/Users/vaughn/Desktop/FinalData.csv", clear varnames(1)

merge m:1 date using "/Users/vaughn/Desktop/FFdata.dta"


//drop NA values
drop if close == "NA"

//convert to float
destring close, replace

destring day, replace
destring afinn, replace
destring bing, replace
destring retwe, replace

//make new date columns
generate new_date = date(date, "YMD")
format new_date %tdNN/DD/CCYY

generate new_event_date = date(event_date, "YMD")
format new_event_date %tdNN/DD/CCYY

drop date
drop event_date

sort event_id new_date
by event_id: gen datenum = _n

drop if new_date < date("2015-06-16", "YMD") //We want AFTER Trump declared candidacy for office

//now starting to follow tutorial

gen mktclose = close if event_id == 0

bysort new_date (mktclose): replace mktclose = mktclose[1]

sort event_id new_date

drop if event_id == 0

destring retweets, replace
summarize retweets

keep if sector == "Consumer Discretionary"

//now to calculate CAAR
drop if mktclose ==.
rename day dif

destring dif, replace

by event_id: gen event_window = 1 if dif >= -10 & dif <= 10

egen count_event_obs=count(event_window), by(event_id)



//Now generate estimation window

by event_id: gen estimation_window = 1 if dif < -10 & dif >= -210

egen count_est_obs = count(estimation_window), by(event_id)

replace event_window = 0 if event_window==.
replace estimation_window = 0 if estimation_window==.

tab event_id if count_event_obs < 10
tab event_id if count_est_obs < 200


drop if count_event_obs < 3
drop if count_est_obs < 100
//declareing panel dataset
xtset event_id datenum
//generate return
gen return = (close/L.close) - 1

replace return = return - rf

//generate market return
gen mkt_return = (mktclose - L.mktclose) - 1

replace mkt_return = mkt_return - mktrf

//generate normal return for each company using a for loop
gen normal_return =.
egen id=group(event_id)

gen SE_residual =.



levelsof event_id //19 unique variables

forvalues i=1(1)3 {
	reg return mkt_return smb hml if id == `i' & estimation_window==1
	predict p if id == `i'
	predict std_err_residual, stdr
	replace SE_residual = std_err_residual if id == `i'
	replace normal_return = p if id == `i' /*& event_window ==1*/
	drop p
	drop std_err_residual
}

sort event_id datenum
gen abnormal_return = return - normal_return

by event_id: egen cumulative_abnormal_return = total(abnormal_return) if event_window==1

//now calculate standard error
sort event_id datenum
by event_id: egen ar_sd_est = sd(abnormal_return) if estimation_window == 1

//then extend to all observations within id
bysort event_id: egen ar_sd = max(ar_sd_est)

corr SE_residual ar_sd

drop ar_sd_est // as they're about the same

gen test = (1/sqrt(20)) * (cumulative_abnormal_return / ar_sd)

list event_id cumulative_abnormal_return test if dif==0

reg cumulative_abnormal_return if dif==0









*NOW WE DO JUST POSITIVE TWEETS by sector*


//Events study stata file
import delimited "/Users/vaughn/Desktop/FinalData.csv", clear varnames(1)

merge m:1 date using "/Users/vaughn/Desktop/FFdata.dta"
//drop NA values
drop if close == "NA"

//convert to float
destring close, replace



//make new date columns
generate new_date = date(date, "YMD")
format new_date %tdNN/DD/CCYY

generate new_event_date = date(event_date, "YMD")
format new_event_date %tdNN/DD/CCYY

drop date
drop event_date

sort event_id new_date
by event_id: gen datenum = _n

drop if new_date < date("2015-06-16", "YMD") //We want AFTER Trump declared candidacy for office

//now starting to follow tutorial

gen mktclose = close if event_id == 0

bysort new_date (mktclose): replace mktclose = mktclose[1]

sort event_id new_date

drop if event_id == 0

destring day, replace
destring afinn, replace
destring bing, replace
destring retwe, replace

drop if bing < 0
drop if afinn < 0
drop if bing == 0 & afinn == 0

keep if sector == "Information Technology"



//now to calculate CAAR
drop if mktclose ==.
rename day dif

destring dif, replace

by event_id: gen event_window = 1 if dif >= -10 & dif <= 10

egen count_event_obs=count(event_window), by(event_id)



//Now generate estimation window

by event_id: gen estimation_window = 1 if dif < -10 & dif >= -210

egen count_est_obs = count(estimation_window), by(event_id)

replace event_window = 0 if event_window==.
replace estimation_window = 0 if estimation_window==.

tab event_id if count_event_obs < 10
tab event_id if count_est_obs < 200


drop if count_event_obs < 3
drop if count_est_obs < 100
//declareing panel dataset
xtset event_id datenum
//generate return
gen return = (close/L.close) - 1

replace return = return - rf

//generate market return
gen mkt_return = (mktclose - L.mktclose) - 1

replace mkt_return = mkt_return - mktrf

//generate normal return for each company using a for loop
gen normal_return =.
egen id=group(event_id)

gen SE_residual =.

levelsof event_id //gives 55 unique events for CD, 23 for IT

forvalues i=1(1)23 {
	reg return mkt_return hml smb if id == `i' & estimation_window==1
	predict p if id == `i'
	predict std_err_residual, stdr
	replace SE_residual = std_err_residual if id == `i'
	replace normal_return = p if id == `i' /*& event_window ==1*/
	drop p
	drop std_err_residual
}

sort event_id datenum
gen abnormal_return = return - normal_return

by event_id: egen cumulative_abnormal_return = total(abnormal_return) if event_window==1

//now calculate standard error
sort event_id datenum
by event_id: egen ar_sd_est = sd(abnormal_return) if estimation_window == 1

//then extend to all observations within id
bysort event_id: egen ar_sd = max(ar_sd_est)

corr SE_residual ar_sd

drop ar_sd_est // as they're about the same

gen test = (1/sqrt(20)) * (cumulative_abnormal_return / ar_sd)

list event_id cumulative_abnormal_return test if dif==0

reg cumulative_abnormal_return if dif==0








*NOW WE DO JUST NEGATIVE  TWEETS by sector*


//Events study stata file
import delimited "/Users/vaughn/Desktop/FinalData.csv", clear varnames(1)

merge m:1 date using "/Users/vaughn/Desktop/FFdata.dta"


//drop NA values
drop if close == "NA"

//convert to float
destring close, replace



//make new date columns
generate new_date = date(date, "YMD")
format new_date %tdNN/DD/CCYY

generate new_event_date = date(event_date, "YMD")
format new_event_date %tdNN/DD/CCYY

drop date
drop event_date

sort event_id new_date
by event_id: gen datenum = _n

drop if new_date < date("2015-06-16", "YMD") //We want AFTER Trump declared candidacy for office

//now starting to follow tutorial

gen mktclose = close if event_id == 0

bysort new_date (mktclose): replace mktclose = mktclose[1]

sort event_id new_date

drop if event_id == 0

destring day, replace
destring afinn, replace
destring bing, replace
destring retwe, replace

drop if bing > 0
drop if afinn > 0
drop if bing == 0 & afinn == 0
keep if sector == "Information Technology"

//now to calculate CAAR
drop if mktclose ==.
rename day dif

destring dif, replace

by event_id: gen event_window = 1 if dif >= -10 & dif <= 10

egen count_event_obs=count(event_window), by(event_id)



//Now generate estimation window

by event_id: gen estimation_window = 1 if dif < -10 & dif >= -210

egen count_est_obs = count(estimation_window), by(event_id)

replace event_window = 0 if event_window==.
replace estimation_window = 0 if estimation_window==.

tab event_id if count_event_obs < 10
tab event_id if count_est_obs < 200


drop if count_event_obs < 3
drop if count_est_obs < 100
//declareing panel dataset
xtset event_id datenum
//generate return
gen return = (close/L.close) - 1

replace return = return - rf

//generate market return
gen mkt_return = (mktclose - L.mktclose) - 1

replace mkt_return = mkt_return - mktrf

//generate normal return for each company using a for loop
gen normal_return =.
egen id=group(event_id)

gen SE_residual =.

levelsof event_id //gives 102 unique events for CD, 22 for IT

forvalues i=1(1)22 {
	reg return mkt_return smb hml if id == `i' & estimation_window==1
	predict p if id == `i'
	predict std_err_residual, stdr
	replace SE_residual = std_err_residual if id == `i'
	replace normal_return = p if id == `i' /*& event_window ==1*/
	drop p
	drop std_err_residual
}

sort event_id datenum
gen abnormal_return = return - normal_return

by event_id: egen cumulative_abnormal_return = total(abnormal_return) if event_window==1

//now calculate standard error
sort event_id datenum
by event_id: egen ar_sd_est = sd(abnormal_return) if estimation_window == 1

//then extend to all observations within id
bysort event_id: egen ar_sd = max(ar_sd_est)

corr SE_residual ar_sd

drop ar_sd_est // as they're about the same

gen test = (1/sqrt(20)) * (cumulative_abnormal_return / ar_sd)

list event_id cumulative_abnormal_return test if dif==0

reg cumulative_abnormal_return if dif==0


	
