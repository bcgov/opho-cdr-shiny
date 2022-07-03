# BC Chronic Disease Bayesian Temporal Smoothing Analysis and Joinpoint Analysis Pipeline 
# Date: 2022-06-26

all: src/eda/01_modeling_eda_loess.html src/eda/02_modeling_eda_inla.html src/eda/03_model_overview.html src/joinpoint/joinpoint_method.html\
results/model results/model/HSCPrevalence results/model/IncidenceRate results/model/LifePrevalence\
	

# Pre-process data for EDA (Temporal Smoothing Analysis)
data/processed/hsc_prevalence_combined.csv data/processed/incidence_rate_combined.csv data/processed/life_prevalence_combined.csv : src/eda/00_eda_preprocess.R
	Rscript src/eda/00_eda_preprocess.R --input="data/Data_T_CHSA" --out_dir="data/processed"

# Pre-process data for Joinpoint Regression
 data/processed/joinpoint_df.csv : src/joinpoint/joinpoint_wrangling.R
	Rscript src/joinpoint/joinpoint_wrangling.R --input="data/Data_T_CHSA" --out_dir="data/processed"

# Generate EDA reports (Temporal Smoothing Analysis)
src/eda/01_modeling_eda_loess.html : src/eda/01_modeling_eda_loess.Rmd data/processed/incidence_rate_combined.csv
	Rscript -e "rmarkdown::render('src/eda/01_modeling_eda_loess.Rmd', output_format = 'html_document')"
		
src/eda/02_modeling_eda_inla.html : src/eda/02_modeling_eda_inla.Rmd data/processed/hsc_prevalence_combined.csv data/processed/incidence_rate_combined.csv data/processed/life_prevalence_combined.csv
	Rscript -e "rmarkdown::render('src/eda/02_modeling_eda_inla.Rmd', output_format = 'html_document')"

# Fit model
results/model/HSCPrevalence : src/model/01_analysis.R
	Rscript src/model/01_analysis.R --input="data/Data_T_CHSA/HSCPrevalence" --output="results/model/HSCPrevalence"
		
results/model/IncidenceRate : src/model/01_analysis.R
	Rscript src/model/01_analysis.R --input="data/Data_T_CHSA/IncidenceRate" --output="results/model/IncidenceRate"
	
results/model/LifePrevalence : src/model/01_analysis.R
	Rscript src/model/01_analysis.R --input="data/Data_T_CHSA/LifePrevalence" --output="results/model/LifePrevalence"

# Generate joinpoint regression results and create data to feed R Shiny app
results/model : src/joinpoint/joinpoint_results.R
	Rscript src/joinpoint/joinpoint_results.R --input="data/processed" --output="results/model"

# Generate model overview report
src/eda/03_model_overview.html : src/eda/03_model_overview.Rmd data/processed/hsc_prevalence_combined.csv data/processed/incidence_rate_combined.csv data/processed/life_prevalence_combined.csv
	Rscript -e "rmarkdown::render('src/eda/03_model_overview.Rmd', output_format = 'html_document')"
	
# Generate additional EDA reports
src/eda/04_modeling_eda_tweedie.html : src/eda/04_modeling_eda_tweedie.Rmd data/processed/hsc_prevalence_combined.csv data/processed/incidence_rate_combined.csv data/processed/life_prevalence_combined.csv
	Rscript -e "rmarkdown::render('src/eda/04_modeling_eda_tweedie.Rmd', output_format = 'html_document')"
	
src/eda/05_modeling_eda_gamma_2.html : src/eda/05_modeling_eda_gamma_2.Rmd data/processed/hsc_prevalence_combined.csv data/processed/incidence_rate_combined.csv data/processed/life_prevalence_combined.csv
	Rscript -e "rmarkdown::render('src/eda/05_modeling_eda_gamma_2.Rmd', output_format = 'html_document')"

# Develop method paper of Joinpoint Regression
src/joinpoint/joinpoint_method.html : src/joinpoint/joinpoint_method.Rmd data/processed/joinpoint_df.csv  
	Rscript -e "rmarkdown::render('src/joinpoint/joinpoint_method.Rmd', output_format = 'html_document')"

clean:
	rm -rf data/processed
	rm -rf src/eda/01_modeling_eda_loess.html src/eda/02_modeling_eda_inla.html
	rm -rf results
	rm -rf src/eda/03_model_overview.html
	rm -rf src/eda/04_modeling_eda_tweedie.html src/eda/05_modeling_eda_gamma_2.html
	rm -rf src/joinpoint/joinpoint_method.html