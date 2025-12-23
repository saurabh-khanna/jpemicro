# Affirmative Action, Faculty Productivity and Caste Interactions: Evidence from Engineering Colleges in India

**Authors**: Robert Fairlie, Saurabh Khanna, Prashant Loyalka, Gagandeep Sachdeva

Data available at [JPE Micro Dataverse Site](https://doi.org/10.7910/DVN/N2NKEG).

```bibtex
@data{DVN/N2NKEG_2025,
author = {Fairlie, Robert and Khanna, Saurabh and Loyalka, Prashant and Sachdeva, Gagandeep},
publisher = {Harvard Dataverse},
title = {{Replication Data for: Affirmative Action, Faculty Productivity and Caste Interactions: Evidence from Engineering Colleges in India}},
UNF = {UNF:6:qeyZgzh755yMfO54GvLvOA==},
year = {2025},
version = {DRAFT VERSION},
doi = {10.7910/DVN/N2NKEG},
url = {https://doi.org/10.7910/DVN/N2NKEG}
}
```

## Replication Files Guide

This document maps CSV data files to the tables and figures in the paper.

---

## Main Tables

| Table | Title | CSV File(s) |
|-------|-------|-------------|
| **3.1** | Faculty and Student Characteristics in Engineering Colleges in India | `main/faculty_national.csv` (Faculty) <br> `main/students_national.csv` (Students) |
| **5.1** | Faculty Qualifications by Reservation Status | `main/faculty_national.csv` |
| **5.2** | Returns to Education by Reservation Status | NSS Microdata (see instructions below) |
| **5.3** | Faculty Differences and Balance Checks (Random Assignment Sample) | `main/random_assignment.csv` |
| **5.4** | Regressions for Student Course Grades (Quality of Instruction) | `main/random_assignment.csv` |
| **5.5** | Regressions for Follow-on Course Grades and Test Scores | `main/table_55_semester.csv` (Model 1) <br> `main/table_55_course.csv` (Model 2) <br> `main/table_55_skills.csv` (Model 3) |
| **5.6** | Regressions for Additional Educational Outcomes | `main/table_56_attend.csv` (Model 1) <br> `main/random_assignment.csv` (Models 2-3) <br> `main/table_56_research.csv` (Model 4) |
| **5.7** | Additional Educational Outcomes (Second Cohort) | `main/table_57.csv` |
| **5.8** | Regressions for Number of Publications per Year | `main/faculty_national.csv` |
| **5.9** | Regressions for Funding Received | `main/faculty_national.csv` |
| **5.10** | Regressions for Administrative Positions Held | `main/faculty_national.csv` |
| **5.11** | Course Grades: Reservation Faculty × Reservation Students | `main/random_assignment.csv` |
| **5.12** | Course Grades: Teacher-Like-Me Interactions | `main/random_assignment.csv` |
| **5.13** | Course Grades: Detailed Reservation Groups (SC/ST, OBC) | `main/random_assignment.csv` |
| **5.14** | Course Grades: Same Reservation Category Group | `main/random_assignment.csv` |

---

## Appendix Tables

| Table | Title | CSV File(s) |
|-------|-------|-------------|
| **A1** | Descriptive Statistics (NSS Micro Data) | NSS Microdata (see instructions below) |
| **B1** | Faculty and Student Characteristics (Random Assignment Sample) | `main/random_assignment.csv` |
| **C1** | Course Assignments by Faculty Reservation Status | Panel A: `appendix/table_C1_a.csv` <br> Panel B: `main/random_assignment.csv` <br> Panel C: `appendix/table_C1_c.csv` |
| **D1** | Reservation and General Category Student Differences | `main/students_national.csv` |
| **E** | Value-Added Measures of Faculty Productivity (Figures) | `appendix/figure_E.csv` |
| **F1** | Weekly Hours Spent on Teaching-Related Activities | `main/random_assignment.csv` |
| **F2** | Teaching Practices Inventory Measures | `main/random_assignment.csv` |
| **G1** | Main Results without Student Fixed Effects | `main/random_assignment.csv` |
| **G2** | Inverse-Weighted Observations by Classroom Size | `main/random_assignment.csv` |
| **G3** | Disaggregated Reservation Groups | `main/random_assignment.csv` |
| **H1** | Follow-on Grades and Test Scores (Interactions) | `main/table_55_semester.csv` <br> `main/table_55_course.csv` <br> `main/table_55_skills.csv` <br> `main/students_national.csv` <br> `main/random_assignment.csv` |
| **H2** | Additional Educational Outcomes (Interactions) | `main/table_56_attend.csv` <br> `main/random_assignment.csv` <br> `main/table_56_research.csv` |
| **I1** | Balance Checks (Second Cohort of Students) | `appendix/table_I1.csv` |
| **I2** | Additional Outcomes with Interactions (Second Cohort) | `main/table_57.csv` |
| **J1** | Number of International Publications per Year | `main/faculty_national.csv` |
| **J2** | SCI, EI or SSCI Publications per Year | `main/faculty_national.csv` |
| **J3** | Government Funding Received | `main/faculty_national.csv` |
| **J4** | Private Funding Received | `main/faculty_national.csv` |
| **J5** | Research, Funding, and Administration (Student-Course Level) | `main/random_assignment.csv` |

---

## Appendix Figures

| Figure | Title | CSV File |
|--------|-------|----------|
| **E1** | PDFs of Faculty Fixed Effects | `appendix/figure_E.csv` |
| **E2** | CDFs of Faculty Fixed Effects | `appendix/figure_E.csv` |


---

## NSS Microdata Instructions

Tables **5.2** and **A1** use microdata from the 68th Round of India's National Sample Survey's Employment and Unemployment Survey.

### Steps to Replicate:

1. **Download** the microdata setup from:  
   https://microdata.gov.in/NADA/index.php/catalog/127/study-description  
   (Reference ID: DDI-IND-MOSPI-NSSO-68-10-2013)

2. **Install** NESSTAR's Extraction Software (included with the microdata download)

3. **Extract** each block as a `.dta` file into the source folder

4. **Run** the Stata do-file `NSS_tables.do` to replicate the tables

---

## Code Files

| File | Description |
|------|-------------|
| `R_code.Rmd` | R Markdown file with all replication code |
| `R_code.md` | Rendered markdown output |
| `NSS_tables.do` | Stata do-file for NSS microdata tables |

---

## Requirements

### R Packages

```r
pacman::p_load(
  haven, estimatr, extrafont, texreg, janitor, hrbrthemes, 
  xtable, papeR, tidyverse, Hmisc, skimr, datawizard, 
  lme4, lattice, broom, fixest, lfe, survey, patchwork, mfx
)
```

### Stata
Required for NSS microdata replication only.
