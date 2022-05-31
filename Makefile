# BC Chronic Disease Bayesian Temporal Smoothing Analysis Pipeline
# Date: 2022-05-31

all: src/eda/01_modeling_eda_loess.html src/eda/02_modeling_eda_inla.html src/eda/03_model_overview.html\
	results/model/HSCPrevalence results/model/IncidenceRate results/model/LifePrevalence 

# Pre-process data for EDA
data/processed/hsc_prevalence_combined.csv data/processed/incidence_rate_combined.csv data/processed/life_prevalence_combined.csv : src/eda/00_eda_preprocess.R
		Rscript src/eda/00_eda_preprocess.R --input="data/Data_T_CHSA" --out_dir="data/processed"

# Generate EDA reports
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

# Generate model overview report
src/eda/03_model_overview.html : src/eda/03_model_overview.Rmd data/processed/hsc_prevalence_combined.csv data/processed/incidence_rate_combined.csv data/processed/life_prevalence_combined.csv
		Rscript -e "rmarkdown::render('src/eda/03_model_overview.Rmd', output_format = 'html_document')"

clean:
		rm -rf data/processed
		rm -rf src/eda/01_modeling_eda_loess.html src/eda/02_modeling_eda_inla.html
		rm -rf results
		rm -rf src/eda/03_model_overview.html