////TABLE 3////
use "C:\Tugas Semester 5\Mikroekonometrika\PS 7\Replication\NEW7080.dta", clear
rename (v1 v2 v4 v5 v6 v9 v10 v11 v12 v13 v16 v18 v19 v20 v21 v24 v25 v27) (AGE AGEQ EDUC ENOCENT ESOCENT LWKLYWGE MARRIED MIDATL MT NEWENG CENSUS QOB RACE SMSA SOATL WNOCENT WSOCENT YOB)

////YOB dummies
replace YOB = YOB - 1900 if YOB >= 1900

foreach i of numlist 0/9 {
    gen YR`i' = (YOB==20+`i' | YOB==30+`i' | YOB==40+`i')
}
////QOB dummies
foreach i of numlist 1/4 {
    gen QTR`i' = (QOB==`i')
}

foreach j of numlist 1/3 {
    foreach i of numlist 0/9 {
        gen QTR`j'YR`i' = QTR`j' * YR`i'
    }
}

////Gen cohort
gen COHORT = 2029
replace COHORT = 3039 if YOB >= 30 & YOB <= 39
replace COHORT = 4049 if YOB >= 40 & YOB <= 49
replace AGEQ = AGEQ - 1900 if CENSUS == 80
gen AGEQSQ = AGEQ * AGEQ

////Create Table 3
capture program drop make_panel
program define make_panel
    args cohort_val panel_label file_name
    
    //Calculate means
    sum LWKLYWGE if QTR1==1 & COHORT==`cohort_val'
    local wage_q1 = string(r(mean), "%9.4f")
    sum LWKLYWGE if QTR1!=1 & COHORT==`cohort_val'
    local wage_nonq1 = string(r(mean), "%9.4f")
    
    sum EDUC if QTR1==1 & COHORT==`cohort_val'
    local educ_q1 = string(r(mean), "%9.4f")
    sum EDUC if QTR1!=1 & COHORT==`cohort_val'
    local educ_nonq1 = string(r(mean), "%9.4f")
    
    //Regressions
    reg LWKLYWGE QTR1 if COHORT==`cohort_val'
    local wage_diff = string(_b[QTR1], "%9.5f")
    local wage_se = string(_se[QTR1], "%9.5f")
    
    reg EDUC QTR1 if COHORT==`cohort_val'
    local educ_diff = string(_b[QTR1], "%9.4f")
    local educ_se = string(_se[QTR1], "%9.5f")
    
    //Wald estimate
    quietly sureg (eq1: LWKLYWGE QTR1) (eq2: EDUC QTR1) if COHORT==`cohort_val'
    quietly nlcom ratio: [eq1]_b[QTR1]/[eq2]_b[QTR1]
    local wald = string(r(b)[1,1], "%9.4f")
    local wald_se = string(sqrt(r(V)[1,1]), "%9.4f")
    
    //OLS
    reg LWKLYWGE EDUC if COHORT==`cohort_val'
    local ols = string(_b[EDUC], "%9.4f")
    local ols_se = string(_se[EDUC], "%9.4f")
    
    //Write to file
    file open myfile using "`file_name'", write replace
    
    file write myfile "`panel_label'" _n(2)
    file write myfile _col(25) "(1)" _col(40) "(2)" _col(55) "(3)" _n
    file write myfile _col(20) "Born in 1st" _col(35) "Born in 2nd," _col(50) "Difference" _n
    file write myfile _col(20) "quarter" _col(35) "3rd, or 4th" _col(50) "(std. error)" _n
    file write myfile _col(20) "of year" _col(35) "quarter of year" _col(50) "(1) - (2)" _n
    file write myfile _dup(80) "-" _n(2)
    
    file write myfile "ln (wkly. wage)" _col(25) "`wage_q1'" _col(40) "`wage_nonq1'" _col(55) "`wage_diff'" _n
    file write myfile _col(55) "(`wage_se')" _n(2)
    
    file write myfile "Education" _col(25) "`educ_q1'" _col(40) "`educ_nonq1'" _col(55) "`educ_diff'" _n
    file write myfile _col(55) "(`educ_se')" _n(2)
    
    file write myfile "Wald est. of return to education" _col(55) "`wald'" _n
    file write myfile _col(55) "(`wald_se')" _n(2)
    
    file write myfile "OLS return to education" _col(55) "`ols'" _n
    file write myfile _col(55) "(`ols_se')" _n
    
    file write myfile _dup(80) "-" _n
    file close myfile
    
    display "`panel_label' saved to `file_name'"
end

////Export Table 3
make_panel 2029 "PANEL A: WALD ESTIMATES FOR 1970 CENSUS—MEN BORN 1920-1929" "C:\Tugas Semester 5\Mikroekonometrika\PS 7\Replication\Tables\Panel_A.txt"

make_panel 3039 "PANEL B: WALD ESTIMATES FOR 1980 CENSUS—MEN BORN 1930-1939" "C:\Tugas Semester 5\Mikroekonometrika\PS 7\Replication\Tables\Panel_B.txt"
