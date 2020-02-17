# Questionnaires 0.0.1

## 15.01.2020
* error found affecting all `ehi` results, now fixed
   - error did not reverse values correctly and so almost all LQ landed between -20 and 20. 

## 04.10.2019
* bdi_compute_sum added option for minimum answered. Some participants by accident skip one or two questions, add possibility to still compute despite missing some answers.
* calculation of component 5 which needs answers to all the sub-questions of question 5 mistakenly also included 5a, which is NOT part of this component  


# Questionnaires 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.  
* redid the repo so it is an R package, rather that scripts collection  
* all functions have questionnaire acronym prefixed for easy access  
* all functions work directly on MOAS data, but may be used on other data by specifying which columns in the dataset corresponds to needed data  
* unit tests added  

