---
title: "Pittsburgh Sleep Quality Index"
subtitle: "Calculation of components and global score"
---

## Background
Despite the prevalence of sleep complaints among psychiatric patients, few questionnaires have been specifically designed to measure sleep quality in clinical populations. The Pittsburgh Sleep Quality Index (PSQI) is a self-rated questionnaire which assesses sleep quality and disturbances over a 1-month time interval. Nineteen individual items generate seven “component” scores: subjective sleep quality, sleep latency, sleep duration, habitual sleep efficiency, sleep disturbances, use of sleeping medication, and daytime dysfunction. The sum of scores for these seven components yields one global score.

## The script   
### Data requirements  
#### Column names
This script assumes the questionnaire column-names all start with the string "**PSQI_**" followed by the question number (**PSQI_1**, **PSQI_2**, etc). Questions with multiple subquestions should be named in a similar manner, suffixed by the alphabetical index (**PSQI_5a**, **PSQI_5b** etc.). For questions 5j and 10j, the frequency of occurence should have the names **PSQI_5j** and **PSQI_10e**, and the freehand explanations should have any type of **suffix** after this to indicate a text answers (i.e. **PSQI_5j_Desc** or **PSQI_5j_string**, **PSQI_5j_freehand**). 

As an example, LCBC has the following set-up:  
**PSQI_1**         
**PSQI_2**         
**PSQI_3**         
**PSQI_4**         
**PSQI_5a**  **PSQI_5b**  **PSQI_5c**  **PSQI_5d**  **PSQI_5e**  **PSQI_5f**  **PSQI_5g**  **PSQI_5h**  **PSQI_5i** **PSQI_5j**   **PSQI_5j_Coded**  **PSQI_5j_Desc**   
**PSQI_6**        
**PSQI_7**         
**PSQI_8**         
**PSQI_9**         
**PSQI_10**  **PSQI_10a**  **PSQI_10b**  **PSQI_10c**  **PSQI_10d**  **PSQI_10e**  **PSQI_10e_Desc**  **PSQI_10e_Coded**    
**PSQI_11a**       **PSQI_11b**       **PSQI_11c**       **PSQI_11d**  

#### 4-option questions coding
All 4-option questions need to be coded **0-3**, not **1-4**.

#### Time formats
For question 1 and 3 (bedtime and rising time), data should be punched as "**HH:MM**". 
Question 2 should be punched as minutes in numbers
Question 4 can be punched both as decimal hours or "**HH:MM**".

### Output   
The script's output is the entire data frame provided, with 8 columns added (7 Components + global score).

### Running the script  
The script is an R-function. Meaning the script file (Fact_PSQI.R) can be sourced from within R, and used as a normal function. It's only input argument is being provided the data that include the PSQI values.

```
source("Fact_PSQI.R")
Fact_PSQI(DATA)
```

If question 4 is punched as "**HH:MM**", run the script with the option "Q4='HH:MM'"
```
source("Fact_PSQI.R")
Fact_PSQI(DATA, Q4='HH:MM')
```

## References  
Buysse et al. (1989) [The Pittsburgh sleep quality index: A new instrument for psychiatric practice and research]("http://www.psy-journal.com/article/0165-1781(89)90047-4/pdf"), _Psychiatry Research_, 28:2, 193-213
