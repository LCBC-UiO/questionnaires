# Questionnaires - LCBC-conversions

Under development/change, changing into an R package.

SO far only developed for PSQI.

The function `compute_psqi` will as default calculate all components, and ise column naming conventions as in the LCBC MOAS data. 
Column specifications may be manually inputed, and so can the option to add the PSQI calculations to the data supplied (`keep_all = TRUE`), or just output the computations (`keep_all = FALSE`). 
You may also specify which components to calculate by integer vector to the `component` option. Global calculation will only be implemented if all 7 components are calculated. 
