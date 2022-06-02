<!-- Add a project state badge
See https://github.com/BCDevExchange/Our-Project-Docs/blob/master/discussion/projectstates.md
If you have bcgovr installed and you use RStudio, click the 'Insert BCDevex Badge' Addin. -->

# BC Chronic Disease Dashboard

The BC Chronic Disease Registry (CDR) is a data product that captures
information about the rate of new and persistent cases of chronic
diseases across the province. Age-standardized rates of disease are
studied for different regions, including HAs (Health Authorities) and
CHSAs (Community Health Service Areas), as well as for demographic
variables such as sex.

In this project we aim to create an interactive dashboard that will
allow users of all technical expertise to explore and visualize spatial
and temporal information of the disease rates in the data, and to
develop an analysis pipeline that will describe the temporal trends in
the data.

### Proposal

Our proposal is linked
[here](https://github.com/bcgov/opho-cdr-shiny/blob/main/docs/proposal/capstone-proposal.pdf).

To render our proposal locally, clone this GitHub repository, install R
and the required dependencies, and run the following command at the
command line/terminal from the root directory:

```
Rscript -e "rmarkdown::render('docs/proposal/capstone-proposal.Rmd')"
```

### Usage

Describe data being used (links), and how to run. Eg:

There are four core scripts that are required for the analysis, they
need to be run in order:

-   01_clean.R
-   02_analysis.R
-   03_visualize.R
-   04_output.R

#### Example

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```

### Project Status

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/opho-cdr-shiny/issues/).

### How to Contribute

If you would like to contribute, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

### Dependencies

-   R version 4.2.0 and R packages:

    -   here=1.0.1
    -   tidyverse=1.3.1
    -   ggplot2=3.3.6
    -   R-INLA=22.05.07
    -   docopt=0.7.1
    -   shiny=1.7.1
    -   shinyjs=2.1.0
    -   plyr=1.8.7
    -   leaflet=2.1.1
    -   sp=1.4-7
    -   rgdal=1.5-32
    -   shinythemes=1.2.0
    -   plotly=4.10.0
    -   scales=1.2.0
    -   shinycssloaders=1.0.0
    -   rgeos=0.5-9

-   GNU make 3.81

### License

```
Copyright 2022 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
```
---
*This project was created using the [bcgovr](https://github.com/bcgov/bcgovr) package.* 
