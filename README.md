# Affirmative Action, Faculty Productivity and Caste Interactions: Evidence from Engineering Colleges in India

**Authors**: Robert Fairlie, Saurabh Khanna, Prashant Loyalka, Gagandeep Sachdeva

## Replication Files Guide

This document maps CSV data files to the tables and figures in the paper.

---

## Main Tables

| Table | Title | CSV File(s) |
|-------|-------|-------------|
| **3.1** | Faculty and Student Characteristics in Engineering Colleges in India | `main/table_31_51_58_59_510.csv` (Faculty) <br> `main/table_31_stu.csv` (Students) |
| **5.1** | Faculty Qualifications by Reservation Status | `main/table_31_51_58_59_510.csv` |
| **5.2** | Returns to Education by Reservation Status | NSS Microdata (see instructions below) |
| **5.3** | Faculty Differences and Balance Checks (Random Assignment Sample) | `main/table_53_54.csv` |
| **5.4** | Regressions for Student Course Grades (Quality of Instruction) | `main/table_53_54.csv` |
| **5.5** | Regressions for Follow-on Course Grades and Test Scores | `main/table_55_model1.csv` (Model 1) <br> `main/table_55_model2.csv` (Model 2) <br> `main/table_55_model3.csv` (Model 3) |
| **5.6** | Regressions for Additional Educational Outcomes | `main/table_56_model1.csv` (Model 1) <br> `main/table_53_54.csv` (Models 2-3) <br> `main/table_56_model4.csv` (Model 4) |
| **5.7** | Additional Educational Outcomes (Second Cohort) | `main/table_57.csv` |
| **5.8** | Regressions for Number of Publications per Year | `main/table_31_51_58_59_510.csv` |
| **5.9** | Regressions for Funding Received | `main/table_31_51_58_59_510.csv` |
| **5.10** | Regressions for Administrative Positions Held | `main/table_31_51_58_59_510.csv` |
| **5.11** | Course Grades: Reservation Faculty × Reservation Students | `main/table_53_54.csv` |
| **5.12** | Course Grades: Teacher-Like-Me Interactions | `main/table_53_54.csv` |
| **5.13** | Course Grades: Detailed Reservation Groups (SC/ST, OBC) | `main/table_53_54.csv` |
| **5.14** | Course Grades: Same Reservation Category Group | `main/table_53_54.csv` |

---

## Appendix Tables

| Table | Title | CSV File(s) |
|-------|-------|-------------|
| **A1** | Descriptive Statistics (NSS Micro Data) | NSS Microdata (see instructions below) |
| **B1** | Faculty and Student Characteristics (Random Assignment Sample) | `main/table_53_54.csv` |
| **C1** | Course Assignments by Faculty Reservation Status | Panel A: `appendix/table_C1_panela.csv` <br> Panel B: `main/table_53_54.csv` <br> Panel C: `appendix/table_C1_panelc.csv` |
| **D1** | Reservation and General Category Student Differences | `main/table_31_stu.csv` |
| **E** | Value-Added Measures of Faculty Productivity (Figures) | `appendix/figure_E_faculty_effects.csv` |
| **F1** | Weekly Hours Spent on Teaching-Related Activities | `main/table_53_54.csv` |
| **F2** | Teaching Practices Inventory Measures | `main/table_53_54.csv` |
| **G1** | Main Results without Student Fixed Effects | `main/table_53_54.csv` |
| **G2** | Inverse-Weighted Observations by Classroom Size | `main/table_53_54.csv` |
| **G3** | Disaggregated Reservation Groups | `main/table_53_54.csv` |
| **H1** | Follow-on Grades and Test Scores (Interactions) | `main/table_55_model1.csv` <br> `main/table_55_model2.csv` <br> `main/table_55_model3.csv` <br> `main/table_31_stu.csv` <br> `main/table_53_54.csv` |
| **H2** | Additional Educational Outcomes (Interactions) | `main/table_56_model1.csv` <br> `main/table_53_54.csv` <br> `main/table_56_model4.csv` |
| **I1** | Balance Checks (Second Cohort of Students) | `appendix/table_I1.csv` |
| **I2** | Additional Outcomes with Interactions (Second Cohort) | `main/table_57.csv` |
| **J1** | Number of International Publications per Year | `main/table_31_51_58_59_510.csv` |
| **J2** | SCI, EI or SSCI Publications per Year | `main/table_31_51_58_59_510.csv` |
| **J3** | Government Funding Received | `main/table_31_51_58_59_510.csv` |
| **J4** | Private Funding Received | `main/table_31_51_58_59_510.csv` |
| **J5** | Research, Funding, and Administration (Student-Course Level) | `main/table_53_54.csv` |

---

## Appendix Figures

| Figure | Title | CSV File |
|--------|-------|----------|
| **E1** | PDFs of Faculty Fixed Effects | `appendix/figure_E_faculty_effects.csv` |
| **E2** | CDFs of Faculty Fixed Effects | `appendix/figure_E_faculty_effects.csv` |

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
