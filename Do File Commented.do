// Vaughn Hajra
// Events study stata file

// Import data from a CSV file
import delimited "/Users/vaughn/Desktop/Stata_Ready_Negative.csv", clear varnames(1)

// Drop rows with missing values in the "close" column
drop if close == "NA"

// Convert the "close" column to a float
destring close, replace

// Create new date columns
generate new_date = date(date, "YMD")
format new_date %tdNN/DD/CCYY

generate new_event_date = date(event_date, "YMD")
format new_event_date %tdNN/DD/CCYY

// Drop the original date columns
drop date
drop event_date

// Sort the data by "event_id" and "new_date"
sort event_id new_date

// Generate a unique identifier for each row within each "event_id" group
by event_id: gen datenum = _n

// Drop rows with dates earlier than June 16, 2015
drop if new_date < date("2015-06-16", "YMD") // We want data after Trump declared candidacy for office

// Start following a tutorial

// Generate a variable "mktclose" for the market close price when "event_id" is 0
gen mktclose = close if event_id == 0

// For each "event_id" group, replace "mktclose" with its first value within the group
bysort new_date (mktclose): replace mktclose = mktclose[1]

// Sort the data again by "event_id" and "new_date"
sort event_id new_date

// Drop rows where "event_id" is 0
drop if event_id == 0

// Calculate Cumulative Abnormal Returns (CAAR)

// Identify event windows for calculating CAAR
drop if mktclose ==.
rename day dif

by event_id: gen event_window = 1 if dif >= -2 & dif <= 2

// Count the number of observations in each event window
egen count_event_obs = count(event_window), by(event_id)

// Generate estimation windows

by event_id: gen estimation_window = 1 if dif < -10 & dif >= -210

// Count the number of observations in each estimation window
egen count_est_obs = count(estimation_window), by(event_id)

// Set "event_window" and "estimation_window" to 0 where they are missing
replace event_window = 0 if event_window == .
replace estimation_window = 0 if estimation_window == .

// Identify "event_id" groups with fewer than 5 observations in the event window
tab event_id if count_event_obs < 5

// Identify "event_id" groups with fewer than 200 observations in the estimation window
tab event_id if count_est_obs < 200

// Drop rows with fewer than 3 observations in the event window
drop if count_event_obs < 3

// Drop rows with fewer than 100 observations in the estimation window

// Declare the dataset as a panel dataset
xtset event_id datenum

// Generate the return variable
gen return = (close / L.close) - 1

// Generate the market return variable
gen mkt_return = (mktclose - L.mktclose) - 1

// Generate normal returns for each company using a loop

gen normal_return =.
egen id = group(event_id)

gen SE_residual =.

levelsof event_id

forvalues i = 1(1)59 {
    reg return mkt_return if id == `i' & estimation_window == 1
    predict p if id == `i'
    predict std_err_residual, stdr
    replace SE_residual = std_err_residual if id == `i'
    replace normal_return = p if id == `i' /* & event_window == 1 */
    drop p
    drop std_err_residual
}

// Sort the data by "event_id" and "datenum"
sort event_id datenum

// Calculate abnormal returns
gen abnormal_return = return - normal_return

// Calculate cumulative abnormal returns (CAR) within event windows
by event_id: egen cumulative_abnormal_return = total(abnormal_return) if event_window == 1

// Calculate standard deviation of abnormal returns within estimation windows
sort event_id datenum
by event_id: egen ar_sd_est = sd(abnormal_return) if estimation_window == 1

// Extend the standard deviation of abnormal returns to all observations within each "event_id" group
bysort event_id: egen ar_sd = max(ar_sd_est)

// Calculate the correlation between SE_residual and ar_sd
corr SE_residual ar_sd

// Drop ar_sd_est as they are about the same
drop ar_sd_est

// Calculate a test statistic
gen test = (1 / sqrt(5)) * (cumulative_abnormal_return / ar_sd)

// List event_id, cumulative abnormal return, and test statistics for rows where dif is 0
list event_id cumulative_abnormal_return test if dif == 0

// Perform regression on cumulative abnormal returns for rows where dif is 0
reg cumulative_abnormal_return if dif == 0
