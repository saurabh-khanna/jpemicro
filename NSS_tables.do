*This do file replicates Table 5.2 and Table A1 of Affirmative Action, Faculty Productivity and Caste Interactions: Evidence from Engineering Colleges in India. 
*Before running this file, please download the raw NSS data and extract it as .dta files using NESSTAR. More details can be found in the README file.
clear all
set more off
global source " " //replace with directory where you save the extracted .dta files from NESSTAR

*start with merging all blocks
*Blocks 1 through 3 are at the household level. Household ID is defined as FSU_Serial_No+Hamlet_Group_Sub_Block_No+ Second_Stage_Stratum_No+Sample_Hhld_No
*Blocks 4 through 7 are at the individual-level. unique Person ID is Person Serial Number+HHID
*Block 8 reports item-wise consumption expenditure -- let's construct a household-level file reporting total MPCE.
use "$source\Block_8.dta", clear
keep HHID Item_Group_Srl_No Value_of_Consumption_Last_30_Day
keep if Item_Group_Srl_No=="40" //item_group_srl_no ==40 reports the total consumption -- see https://microdata.gov.in/NADA/index.php/catalog/127/data-dictionary/F14?file_name=Block_8_Household%20consumer%20expenditure
drop Item_Group_Srl_No
rename Value_of_Consumption_Last_30_Day mpce
save "$source\block8_mpce.dta", replace

*Merge blocks
use "$source\Block_1_2.dta", clear
merge 1:1 FSU_Serial_No Hamlet_Group_Sub_Block_No Second_Stage_Stratum_No Sample_Hhld_No using "$source\Block_3.dta"
drop _merge
merge 1:m HHID using "$source\Block_4.dta"
drop _merge
merge 1:1 HHID Person_Serial_No using "$source\Block_5_1.dta"
drop _merge
merge 1:1 HHID Person_Serial_No using "$source\Block_5_2.dta"
drop _merge
merge 1:m HHID Person_Serial_No using "$source\Block_5_3.dta"
drop _merge
*only keep records of primary activity
keep if Srl_no_of_Activity=="1"
merge 1:1 HHID Person_Serial_No using "$source\Block_6.dta"
drop _merge
merge 1:1 HHID Person_Serial_No using "$source\Block_7.dta"
drop _merge
merge m:1 HHID using "$source\block8_mpce.dta"
drop _merge


*generate person identifier= Household Identifier + Person Serial Number
gen PID=HHID+Person_Serial_No

keep HHID Age PID Social_Group Enterprise_Type Sex Sector Usual_Principal_Activity_Status Wage_and_Salary_Earnings_Total General_Education Multiplier_comb NCO_2004 District_Code mpce

* value labels based on Data Dictionary for Blocks 3, 4 and 5_1 (see https://microdata.gov.in/NADA/index.php/catalog/127/data-dictionary)
*destring variables, create and attach value-labels
destring General_Education, replace
label define General_Education 1 "not literate" 2 "literate without formal schooling:  EGS/ NFEC/ AEC"  3 "TLC" 4 "others" 5 "literate: below primary" 6 "primary" 7 "middle" 8 "secondary" 10 "higher secondary" 11 "diploma/certificate course" 12 "graduate" 13 "postgraduate and above" 999 "Missing"
label values General_Education General_Education

destring Usual_Principal_Activity_Status, replace
label define Status 11 "worked in hh enterprise (self-employed): own account worker" 12 "worked in hh enterprise (self-employed): employer" 21 "worked as helper in hh enterprise (unpaid family worker)" 31 "Worked as Regular Wage/Salaried Employee" 41 "Casual Wage Labour in Public Works other than NREG" 42 "Casual Wage Labour in NREG" 51 "Worked as Casual Wage Labour in Other Kinds of Work" 61 "had work in household enterprise (self-employed) but did not work due to sickness" 62 "had work in household enterprise (self-employed) but did not work due to other reasons" 71 "Had regular salaried work but didn't work due to sickness" 72 "Had regular salaried work but didn't work due to other reasons" 81 "did not work but was seeking work" 82 "did not work but was available for work" 91 "attended educational institution" 92 "attended domestic duties only" 93 "tended domestic duties, engaged in free collection of goods, sewing, tailoring, weaving, etc. for household use" 94 "rentiers, pensioners , remittance recipients, etc" 95 "not able to work due to disability" 97 "others (including begging, prostitution,  etc.)" 98 "did not work  due to temporary sickness (for casual workers only)" 999 "Missing"
label values Usual_Principal_Activity_Status Status

destring Sector, replace
label define Sector 1 "Rural" 2 "Urban"
label values Sector Sector
gen urban=Sector==2
label var urban "Urban"

destring Enterprise_Type, replace
label def Enterprise_Type 1 "proprietary: male" 2 "proprietary: female" 3 "partnership with members from same hh" 4 "partnership with members from different hh" 5" Government/public sector" 6 "Public/Private limited company" 7 "Co-operative societies/trust/other non profit institutions" 8 "employer’s households(i.e., private households employing  maid servant, watchman, cook, etc.)" 9 "others" 999 "Missing"
label values Enterprise_Type Enterprise_Type

destring Social_Group, replace
label define Social_Group 1 "ST" 2 "SC" 3 "OBC" 9 "General"
label values Social_Group Social_Group
gen reservation=inrange(Social_Group, 1, 3)
label define reservation 0 "General Category" 1 "Reservation Category", replace
label values reservation reservation

destring Sex, replace
label define Sex 1 "Male" 2 "Female"
label values Sex Sex
gen female=Sex==2
label var female "Female"

rename Age age
label var age "Age"
gen age_sq=age*age
label var age_sq "Age$^2$"

label var mpce "Monthly Per Capita Consumption Expenditure"
rename Multiplier_comb multiplier

destring District_Code, replace
rename District_Code district
destring NCO_2004, force gen(nco2004)

***EDUCATION VARIABLES
*generate years of schooling variable
gen years_of_schooling=.
replace years_of_schooling=0 if General_Education==1 //not literate
replace years_of_schooling=1 if inrange(General_Education, 2, 4) //literate without formal schooling, TLC, others 
replace years_of_schooling=3 if General_Education==5 //literate below primary
replace years_of_schooling=5 if General_Education==6 //primary
replace years_of_schooling=8 if General_Education==7 //middle
replace years_of_schooling=10 if General_Education==8 //secondary
replace years_of_schooling=12 if General_Education==10 //higher secondary
replace years_of_schooling=13 if General_Education==11 //diploma/certificate course
replace years_of_schooling=15 if General_Education==12 // graduate
replace years_of_schooling=16 if General_Education==13 //postgrad and above
label var years_of_schooling "Years of Schooling"

*proportion graduating high school
gen high_school_graduate=years_of_schooling>=12
replace high_school_graduate=high_school_graduate*100 //multiplying by 100 for summary table
label var high_school_graduate "Proportion Graduating High School"

*proportion graduating college
gen college_graduate=years_of_schooling>12
replace college_graduate=college_graduate*100 //multiplying by 100 for summary table
label var college_graduate "Proportion Graduating College"

*proportion postgraduate and above
gen post_graduate=General_Education==13
replace post_graduate=post_graduate*100 //multiplying by 100 for summary table
label var post_graduate "Proportion with Masters or Higher"
*proportion postgraduate and above between ages 25 and 50
gen post_graduate_25to50=post_graduate if inrange(age, 25, 50)
label var post_graduate_25to50 "Proportion with Masters or Higher (Ages 25-50)"


*****EMPLOYMENT VARIABLES
*proportion employed in regular wage/salaried employment
gen employed_regular=Usual_Principal_Activity_Status==31|Usual_Principal_Activity_Status==12 //worked in hh enterprise (self-employed): employer and  Worked as Regular Wage/Salaried Employee
replace employed_regular=employed_regular*100 //multiplying by 100 for summary table
label var employed_regular "Proportion in Regular Wage/Salaried or Self-Employment"
*employed in public sector job
gen government_job=Enterprise_Type==5
label var government_job "Public Sector"
label define government_job 0 "Not Public" 1 "Public Sector"
label values government_job government_job
*employed in private sector job
gen private_job=Enterprise_Type==6|Enterprise_Type==7
label var private_job "Private Sector"
label define private_job 0 "Not Private" 1 "Private Sector"
label values private_job private_job


****WAGES VARIABLES
label var Wage_and_Salary_Earnings_Total "Weekly Wages (Rupees)"
*wages and salaries of college grads and above
gen collegegrad_wages=.
replace collegegrad_wages= Wage_and_Salary_Earnings_Total if college_graduate==100
label var collegegrad_wages "Weekly Wages: College Graduates"
gen collegegrad_wages_25to35 = collegegrad_wages if inrange(age, 25, 35)
label var collegegrad_wages_25to35 "Weekly Wages: College Graduates (Ages 25-35)"

*keep positive wage earners
drop if Wage_and_Salary_Earnings_Total==0
gen log_wages=ln(Wage_and_Salary_Earnings_Total)
label var log_wages "ln(Wages)"

*GENERATE SUMMARY STATS FOR TABLE A1
eststo summary_gen: estpost tabstat years_of_schooling high_school_graduate college_graduate post_graduate post_graduate_25to50 employed_regular mpce Wage_and_Salary_Earnings_Total collegegrad_wages collegegrad_wages_25to35 if inrange(age, 25, 64)&reservation==0, c(stat) stat(mean sd n)
eststo summary_res: estpost tabstat years_of_schooling high_school_graduate college_graduate post_graduate post_graduate_25to50 employed_regular mpce Wage_and_Salary_Earnings_Total collegegrad_wages collegegrad_wages_25to35 if inrange(age, 25, 64)&reservation==1, c(stat) stat(mean sd n)
eststo diff: estpost ttest years_of_schooling high_school_graduate college_graduate post_graduate post_graduate_25to50 employed_regular mpce Wage_and_Salary_Earnings_Total collegegrad_wages collegegrad_wages_25to35 if inrange(age, 25, 64),  by(reservation) unequal

replace college_graduate=college_graduate/100
label var college_graduate "College Graduate"

*GENERATE REGRESSION OUTPUT FOR TABLE 5.2
eststo m1: reg log_wages i.college_graduate##i.reservation if inrange(age, 25, 64) [pweight=multiplier], robust cluster(district)
eststo m2: reg log_wages i.college_graduate##i.reservation age age_sq i.female i.urban if inrange(age, 25, 64) [pweight=multiplier], robust cluster(district)
eststo m3: areg log_wages i.college_graduate##i.reservation age age_sq i.female i.urban if inrange(age, 25, 64) [pweight=multiplier], absorb(nco2004) robust cluster(district)
eststo m4: areg log_wages i.college_graduate##i.reservation age age_sq i.female i.urban if inrange(age, 25, 45) [pweight=multiplier], absorb(nco2004) robust cluster(district)
eststo m5: areg log_wages i.college_graduate##i.reservation age age_sq i.female i.urban if inrange(age, 25, 64)&government_job==1 [pweight=multiplier], absorb(nco2004) robust cluster(district)
eststo m6: areg log_wages i.college_graduate##i.reservation age age_sq i.female i.urban if inrange(age, 25, 64)&private_job==1 [pweight=multiplier], absorb(nco2004) robust cluster(district)


*Compile Tables
*Compile Table A1:
esttab summary_gen summary_res diff using "$source\jpe_micro_tables.tex", cells("mean(pattern(1 1 0) fmt(2)) b(star pattern(0 0 1) fmt(2))" "sd(pattern(1 1 0) par fmt(2)) se(pattern(0 0 1) par fmt(2))" "count(pattern(1 1 0) fmt(0))") label replace
*Compile Table 5.2
esttab m1 m2 m3 m4 m5 m6  using "$source\jpe_micro_tables.tex", star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(3)) se(par fmt(2))) label nobase noomit append
