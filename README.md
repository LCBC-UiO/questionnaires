# Questionnaires - LCBC-conversions

The repository contains functions to run coversions and calculate components from commonly used questionnaires in LCBC research. 

The questionnaires covered so far:  
- PSQI - Pittsburgh Sleep Quality Inventory
- IPAQ - International Physical Activity Questionnaire
- EHI - Edinburgh Handedness Inventory
- BDI - Beck Depression Inventory
- GDS - Geriatric Depression Scale
- TAS - 


All functions in this package are prefixed with the name of the questionnaire the function is intended for (i.e. `psqi_`, `ipaq_` etc.). 
Column specifications may be manually inputed, but if columns are named after as the function expects (i.e. MOAS standard), the functions generally work without manual input. 
The functions that will run all (most) necessary steps to completely calculate components and sums are named as `questionnaire_compute` (i.e. `psqi_compute()`, `ipaq_compute`, etc.). These functions all have the option to `keep_all` which takes a `TRUE` or `FALSE` statement on whether the data should be appended to the input data, or just to return the computed columns.

Vignettes are on their way, and so far there are only vignettes for PSQI and IPAQ which  just describe the background for the computations. 
