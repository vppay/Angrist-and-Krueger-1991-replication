////Table 5////
use "C:\Tugas Semester 5\Mikroekonometrika\PS 7\Replication\NEW7080.dta", clear
rename (v1 v2 v4 v5 v6 v9 v10 v11 v12 v13 v16 v18 v19 v20 v21 v24 v25 v27) (AGE AGEQ EDUC ENOCENT ESOCENT LWKLYWGE MARRIED MIDATL MT NEWENG CENSUS QOB RACE SMSA SOATL WNOCENT WSOCENT YOB)

////YOB dummies
replace YOB=YOB-1900 if YOB >=1900

foreach i of numlist 0/9 {
gen YR`i'=0
replace YR`i'=1 if YOB==20+`i' | YOB==30+`i' | YOB==40+`i' 
}
////QOB dummies
foreach i of numlist 1/4 {
gen QTR`i'=0
replace QTR`i'=1 if QOB==`i'
}
////QOB*YOB dummies 
foreach j of numlist 1/3 {
foreach i of numlist 0/9 {
gen QTR`j'YR`i'=QTR`j'*YR`i'
}
}
////Select Particular Men Born
gen COHORT=2029
replace COHORT=3039 if YOB<=39 & YOB >=30
replace COHORT=4049 if YOB<=49 & YOB >=40
replace AGEQ=AGEQ-1900 if CENSUS==80
gen AGEQSQ= AGEQ*AGEQ
keep if COHORT>3000 & COHORT <3040

////Start Regression 
eststo clear
reg  LWKLYWGE EDUC  YR0-YR8 
estadd local yob "Yes"
estadd local region "No"
eststo model1

ivregress 2sls LWKLYWGE YR0-YR8 (EDUC = QTR1YR0-QTR1YR9 QTR2YR0-QTR2YR9 QTR3YR0-QTR3YR9 YR0-YR8)
estadd local yob "Yes"
estadd local region "No"
eststo model2
estat overid
estadd scalar overid_chi2 = r(sargan): model2
estadd scalar overid_df = r(df): model2

reg  LWKLYWGE EDUC  YR0-YR8 AGEQ AGEQSQ 
estadd local yob "Yes"
estadd local region "No"
eststo model3

ivregress 2sls LWKLYWGE YR0-YR8 AGEQ AGEQSQ (EDUC = QTR1YR0-QTR1YR9 QTR2YR0-QTR2YR9 QTR3YR0-QTR3YR9 YR0-YR8)
estadd local yob "Yes"
estadd local region "No"
eststo model4
estat overid
estadd scalar overid_chi2 = r(sargan): model4
estadd scalar overid_df = r(df): model4

reg  LWKLYWGE EDUC  RACE MARRIED SMSA NEWENG MIDATL ENOCENT WNOCENT SOATL ESOCENT WSOCENT MT YR0-YR8  
estadd local yob "Yes"
estadd local region "Yes"
eststo model5

ivregress 2sls LWKLYWGE YR0-YR8 RACE MARRIED SMSA NEWENG MIDATL ENOCENT WNOCENT SOATL ESOCENT WSOCENT MT  (EDUC = QTR1YR0-QTR1YR9 QTR2YR0-QTR2YR9 QTR3YR0-QTR3YR9 YR0-YR8)
estadd local yob "Yes"
estadd local region "Yes"
eststo model6
estat overid
estadd scalar overid_chi2 = r(sargan): model6
estadd scalar overid_df = r(df): model6

reg  LWKLYWGE EDUC  RACE MARRIED SMSA NEWENG MIDATL ENOCENT WNOCENT SOATL ESOCENT WSOCENT MT YR0-YR8 AGEQ AGEQSQ 
estadd local yob "Yes"
estadd local region "Yes"
eststo model7

ivregress 2sls LWKLYWGE YR0-YR8 RACE MARRIED SMSA NEWENG MIDATL ENOCENT WNOCENT SOATL ESOCENT WSOCENT MT AGEQ AGEQSQ (EDUC = QTR1YR0-QTR1YR9 QTR2YR0-QTR2YR9 QTR3YR0-QTR3YR9 YR0-YR8)
estadd local yob "Yes"
estadd local region "Yes"
eststo model8
estat overid
estadd scalar overid_chi2 = r(sargan): model8
estadd scalar overid_df = r(df): model8

////Table Decoration 
label variable EDUC   "Years of education"
label variable RACE   "Race (1 = black)"
label variable SMSA   "SMSA (1 = center city)"
label variable MARRIED   "Married (1 = married)"
label variable AGEQ   "Age"
label variable AGEQSQ   "Age-squared"

esttab, se label keep(EDUC RACE SMSA MARRIED AGEQ AGEQSQ) order(EDUC RACE SMSA MARRIED AGEQ AGEQSQ) title("Table V OLS and TSLS estimates of the return to education for men born 1930 - 1939: 1980 census") nonumbers mtitles("(1) OLS" "(2) TSLS" "(3) OLS" "(4) TSLS" "(5) OLS" "(6) TSLS" "(7) OLS" "(8) TSLS") stats(yob region overid_chi2 overid_df, labels("9 Year-of-birth dummies" "8 Region of residence dummies" "χ2" "dof"))

////Export Table 5
esttab using "C:\Tugas Semester 5\Mikroekonometrika\PS 7\Replication\Tables\Table V.rtf", se label keep(EDUC RACE SMSA MARRIED AGEQ AGEQSQ) order(EDUC RACE SMSA MARRIED AGEQ AGEQSQ) title("Table V OLS and TSLS Estimates of The Return to Education for Men Born 1930 - 1939: 1980 Census") nonumbers mtitles("(1) OLS" "(2) TSLS" "(3) OLS" "(4) TSLS" "(5) OLS" "(6) TSLS" "(7) OLS" "(8) TSLS") stats(yob region overid_chi2 overid_df, labels("9 Year-of-birth dummies" "8 Region of residence dummies" "χ2" "dof"))
