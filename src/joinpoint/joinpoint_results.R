"Fits a Joinpoint Regression model and results in fitted values where data does
not conmtain Zero, or missing values. When no significant changepoints can be 
identified by the model, then the fitted values are results of only log-linear 
model itself
Outputs a CSV with model results (joinpoint_resukts.csv) and a datafile to be 
used in R shiny app (joinpoint_for_shiny_df.fst).

Usage: src/joinpoint/joinpoint_results.R --input=<input> --output=<output>

Options:
--input=<input>       Path to data directory with rate data
--output=<output>     Path to results directory for model output
" -> doc

