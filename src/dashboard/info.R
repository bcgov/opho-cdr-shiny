
about<-"<br/><br/>
  This dashboard facilitates the exploration and visualization of spatial and temporal
  trends of 25 different chronic diseases across the Province of British Columbia. The data 
  is sourced from the BC Chronic Disease Registry (BCCDR). Data is available for three different rate types, 
  each as a crude rate or age-standardized rate, and is available at the Health Authority (HA) level or the
  Community Health Services Area (CHSA) level. The data is aggregated per geography, year, and gender. 
  Detailed information and definitions of the data can be found in the corresponding information pages.
  
  <br/><br/>
  The dashboard contains 3 tabs, each of which are described as follows: <br/>
  <ul>
   <li><b>By Disease</b></li>
   This tab allows for the comparisons of one disease over several HAs or CHSAs.
   In this tab the user should select a disease, a rate type, geography type, gender, and year. 
   The user can specify multiple HAs or CHSAs. After data has been selected, the following
   visualizations will be displayed:
   <ul>
    <li> A choropleth map showing the distribution of the chosen rate type across the Province of British Columbia in a given year </li>
    <li> A bar graph showing the chosen rate type for all selected health regions, with upper and lower bounds of the rate </li>
    <li> A line chart showing trend of chosen rate type for all selected health regions from 2001 to 2020. </li>
   </ul>
   All visualizations have interactive elements, where detailed information will appear upon mouse hover. 
   An animation of changes over year is also available. 
   <br/><br/>
   <li><b>By Region</b></li>
   This tab allows for the comparisons of several diseases in one particular HA or CHSA.
   In this tab the user should select a rate type, disease(s) of interest, geography type, health region,
   year, and gender. After data has been selected, the following
   visualizations will be displayed:
   <ul>
    <li> A bar graph showing the chosen rate type for all selected diseases </li>
    <li> A line chart showing trend of chosen rate type for all selected diseases from 2001 to 2020. </li>
    <li> A map showing location of selected health region </li>
   </ul>
   <li><b>Data</b></li>
   This tab retrieves all data specified by the user. In this tab the user should select a rate type, 
   disease(s), geography type, health region(s), year range, and gender. A data table showing the selected
   data wil be displayed, and there is an option for the user to download the selected data.<br/><br/>
  </ul>
  <br/>
  <b>Note:</b> The year selector is based on Ministry of Health fiscal years. For example, the year 2001 represents data 
  from April 1, 2001 to March 31, 2002. 
"


rates<-"Three different rate types are available. The definitions of each are as follows:
       <ul>
         <li><b>Incidence Rate</b> : The rate at which new cases of disease occur in a 
         specified population during a specified time period. 
         It is calculated as the number of new cases in the population at-risk in a 
         specified period of time divided by the person-time at risk or the number of persons at risk 
         (i.e., mid-year population in a reporting year minus previous year's prevalent cases) in the same period. 
         <br/><br/>
         <p style='margin-left: 40px'>Incidence rate  = (number of newly identified cases in a reporting year) / 
         (mid-year population at risk in the reporting year) * 10<sup>n</sup></p></li>
         <li><b>Lifetime Prevalence</b>: proportion of individuals who have had the condition for at least part of 
         their lives at any time during their life course. In the BCCDR, this refers to the proportion of residents 
         who were diagnosed/identified as a case at least once and were still alive and residing in the province during 
         a reporting time period (fiscal year). Once the case definition criteria are met in a year, cases are then
         carried forward to count as a case every year thereafter until the person's death, their migration out of BC,
         or the absence of follow-up. 
         <br/><br/>
         <p style='margin-left: 40px'> Lifetime prevalence = (number of residents ever identified with a disease 
         in a reporting year) / (mid-year population in the reporting year) * 10<sup>n</sup></p></li>
         <li><b>Active Healthcare Contact (HSC) Prevalence</b>: For relapsing-remitting diseases, the BCCDR measures 
         active healthcare contact prevalence. Cases are counted if they previously met case definition criteria 
         for a disease, continued to live and receive healthcare services for the disease again in BC during a
         later reporting period. That is, cases are counted for a reporting period if the patient seek healthcare 
         services for relapsing-remitting conditions again after the fiscal year when they were ascertained as a case. 
         <br/><br/>
         <p style='margin-left: 40px'> Active healthcare contact prevalence = (number of patients receiving
         healthcare services for a disease in a reporting year) /(mid-year population in the reporting year) * 10<sup>n</sup>
         </p></li>
       </ul>
       <br/><br/>
      Each of the rates is available as either a crude rate or age-standardized rate. The defintions of these two types are as follows:
       <ul>
        <li><b>Age-Standardized</b>:
        To account for differences in the age structure of different geographical regions, rates are calculated 
        as if all regions shared the same age structure, that of the 2011 Canadian Census. Age-standardized rates 
        are appropriate for comparing regions or trends over time. They are not a good representation of the 
        burden of disease in the population.</li>
        <li><b>Crude</b>:
        These rates are not adjusted to the standard population, and represent the number of cases in a
        specific geographic region divided by the population/population-at-risk in that region. 
        Crude rates are representative of the burden of disease in the population.
        </li>
       </ul>
       
    "