
about_info<-"<br/><br/>
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
  <br/>
  <br/>

"


rate_info<-"Three different rate types are available. The definitions of each are as follows. Note for each definition,
        the value of n depends on whether the disease is rare or not and is typically set to 2 or 3 in the BCCDR.
       <ul>
         <li><b>Incidence Rate</b> : The rate at which new cases of disease occur in a 
         specified population during a specified time period. 
         It is calculated as the number of new cases in the population at-risk in a 
         specified period of time divided by the person-time at risk or the number of persons at risk 
         (i.e., mid-year population in a reporting year minus previous year's prevalent cases) in the same period. 
         <br/><br/>
         <p style='margin-left: 40px'><i>Incidence rate  = (number of newly identified cases in a reporting year) / 
         (mid-year population at risk in the reporting year) * 10<sup>n</sup></i></p></li>
         <li><b>Lifetime Prevalence</b>: The proportion of individuals who have had the condition for at least part of 
         their lives at any time during their life course. In the BCCDR, this refers to the proportion of residents 
         who were diagnosed/identified as a case at least once and were still alive and residing in the province during 
         a reporting time period (fiscal year). Once the case definition criteria are met in a year, cases are then
         carried forward to count as a case every year thereafter until the person's death, their migration out of BC,
         or the absence of follow-up. Lifetime prevalence is helpful for determining the prevalence of life-long chronic diseases, 
         but less so for determining the prevalence of relapsing-remitting conditions 
         (e.g., mood disorders, substance use problem, etc.). For BCCDR reporting, lifetime prevalence is calculated 
         for both life-long conditions (e.g., diabetes) and relapsing-remitting conditions (e.g., anxiety and depressive disorders).
         Users should note that lifetime prevalence proportions for those relapsing-remitting conditions are high and do not
         represent the current burden of such conditions in the province. Caution is also needed when comparing to other 
         reports where the proportions of a population with active symptoms or treatments for these conditions. 
         <br/><br/>
         <p style='margin-left: 40px'><i> Lifetime prevalence = (number of residents ever identified with a disease 
         in a reporting year) / (mid-year population in the reporting year) * 10<sup>n</sup></i></p></li>
         <li><b>Active Healthcare Contact (HSC) Prevalence</b>: For relapsing-remitting diseases, the BCCDR measures 
         active healthcare contact prevalence. Cases are counted if they previously met case definition criteria 
         for a disease, continued to live and receive healthcare services for the disease again in BC during a
         later reporting period. That is, cases are counted for a reporting period if the patient seek healthcare 
         services for relapsing-remitting conditions again after the fiscal year when they were ascertained as a case. 
         This prevalence measure is useful for describing the existing burden of service utilization directly 
         related to relapsing-remitting diseases. 
         <br/><br/>
         <p style='margin-left: 40px'><i> Active healthcare contact prevalence = (number of patients receiving
         healthcare services for a disease in a reporting year) /(mid-year population in the reporting year) * 10<sup>n</sup>
         </i></p></li>
       </ul>
       <br/>
      Each of the above rates is available as either a crude rate or age-standardized rate. 
      The defintions of these two types are as follows:<br/><br/>
       <ul>
        <li><b>Age-Standardized</b>:
        To account for differences in the age structure of different geographical regions, rates are calculated 
        as if all regions shared the same age structure, that of the 2011 Canadian Census. Age-standardized rates 
        are appropriate for comparing regions or trends over time. They are not a good representation of the 
        burden of disease in the population.</li>
        <br/><br/>
        <li><b>Crude</b>:
        These rates are not adjusted to the standard population, and represent the number of cases in a
        specific geographic region divided by the population/population-at-risk in that region. 
        Crude rates are representative of the burden of disease in the population.
        </li>
       </ul>
    "


disease_info <- "The BCCDR includes 25 chronic disease registries built and maintained by the Office 
                of the Provincial Health Officer (OPHO) with new registries being brought on over time. 
                The Chronic Disease Registries draws on data from several administrative data sources, 
                including Medical Services Plan (MSP), Discharge Abstract Databases (DAD), 
                PharmaNet (PNET), and  Client Roster (CR). Using the BCCDR data, it is possible 
                to estimate the incidence and lifetime prevalence 
                of 25 chronic conditions in BC, as well as active healthcare contact 
                prevalence for 11 relapsing-remitting diseases (marked with a star in the list below):<br/><br/>
                
                <b>Chronic Respiratory Diseases</b><br/>
                <p style='margin-left: 40px'> 
                      Asthma*<br/>
                      Chronic obstructive pulmonary disease<br/>
                </p>                              
                <b>Cardiovascular Diseases </b>  <br/> 
                <p style='margin-left: 40px'> 
                      Acute myocardial infarction*<br/>
                      Heart failure <br/>
                      Ischemic heart disease <br/>
                      Stroke*
                      <p style='margin-left: 60px'> 
                            Haemorrhagic stroke* <br/>
                            Ischemic stroke* <br/>
                            Transient ischemic attack* <br/>
                            </p>
                </p>
                <b>Neurological Disorders </b><br/> 
                <p style='margin-left: 40px'> 
                      Alzheimer's and other dementias <br/> 
                      Epilepsy <br/> 
                      Multiple sclerosis <br/> 
                      Parkinson's disease <br/> 
                </p>
                <b>Mental and Substance Use Disorders</b><br/> 
                 <p style='margin-left: 40px'> 
                      Depressive disorders*<br/> 
                      Anxiety and mood disorders*<br/> 
                      Schizophrenia & delusional disorders*<br/> 
                      Substance use disorders*<br/> 
                </p>
                <b>Musculoskeletal Disorders</b><br/> 
                <p style='margin-left: 40px'>
                      Gout*<br/> 
                      Osteoarthritis<br/> 
                      Osteoporosis<br/> 
                      Rheumatoid arthritis<br/> 
                      Juvenile idiopathic arthritis<br/> 
                </p>
                <b>Diabetes and Kidney Diseases </b>    
                <p style='margin-left: 40px'>
                      Diabetes mellitus<br/>
                      Chronic kidney disease<br/>
                      Hypertensive Diseases <br/>     
                      High blood pressure (hypertension)<br/>
                </p>
                
                For more information on these diseases, click 'Show PDF' button below.

"


data_dict_info <-"
  Data for Health Boundaries containing 25% First Nations population or larger are suppressed, and will be shown as NA.  
  True zeros (0) remain in the data set.
  <br/><br/>
  Modeled data is available for all Age-Standardized rates in CHSAs for Total Sex data. The modelled curves
  show the changes in disease rates smoothened out over time to capture the overall trends within
  each health boundary. For rare conditions or smaller regions, the modelled data may appear to be 
  missing due to low rates. Additional information about the modelling procedure can be found <a href='https://github.com/bcgov/opho-cdr-shiny'>here</a>.
  <br/>

  <table border='1'>
    <tr>
      <th>Variable Name</th>
      <th>Description</th>
      <th>Sample Values</th>
    </tr>
    <tr>
      <td><b>Fiscal Year</b></td>
      <td>Label for the fiscal year (April 1 to March 31 of the following year).</td>
      <td><q>2001/2002</q> up to <q>2020/2021</q></td>
    </tr>
    <tr>
      <td><b>Disease</b></td>
      <td>Name of the chronic condition.</td>
      <td><q>Asthma</q>, <q>Depression</q>, <q>Heart Failure</q></td>
    </tr>
    <tr>
      <td><b>Sex</b></td>
      <td>Gender category.</td>
      <td><q>T</q>, <q>M</q>, or <q>F</q>. T includes Male, Female, and Unknown.</td>
    </tr>
    <tr>
      <td><b>Geography</b></td>
      <td>The type of health boundary used to separate geographic areas.</td>
      <td><q>HA</q> or <q>CHSA</q></td>
    </tr>
    <tr>
      <td><b>Health Boundary</b></td>
      <td>The HA or CHSA region of interest.</td>
      <td><q>Fraser</q>, <q>Vancouver Coastal</q>, <q>Fernie</q>, <q>Campbell River</q></td>
    </tr>
    <tr>
      <td><b>Numerator</b></td>
      <td>Incidence: Number of newly identified cases in a reporting year.
      <br/> Life prevalence: Number of residents ever identified with a disease in a reporting year. 
      <br/> Active healthcare contact prevalence: Number of patients receiving healthcare services for a disease in a reporting year.</td>
      <td>0, 1, 2, 23, 103, 457, 685</q></td>
    </tr>
    <tr>
      <td><b>Denominator</b></td>
      <td>Incidence: Mid-year population at risk in the reporting year.
      <br/>Life prevalence and active healthcare contact prevalence: Mid-year population in the reporting year.</td>
      <td>11, 269, 3972, 10185, 52974, 80356</q></td>
    </tr>
    <tr>
      <td><b>Crude Rate Per 1000</b></td>
      <td>Crude rate per 1000 people. Equivalent to Numerator/Denominator * 1000.</td>
      <td> 0 (if Numerator = 0), 0.95, 5.87, 19.46</q></td>
    </tr>
    <tr>
      <td><b>Crude 95% CI</b></td>
      <td>95% lower and upper confidence level on the crude rate per 1000 people. 
      <br/>Assumes Numerator is normally-distributed and Denominator is known exactly.</td>
      <td> (NA,NA) (if Numerator = 0), (0.12,3.4), (2.15,12.77), (9.33,35.78)</q></td>
    </tr>
    <tr>
      <td><b>Crude Variance</b></td>
      <td>Variance on the crude rate per 1000 people. 
      <br/>Calculated assuming Numerator is Poisson-distributed and Denominator is known exactly.</td>
      <td>0 (if Numerator = 0), 4.54E-07, 1.35E-06, 3.79E-05</q></td>
    </tr>
    <tr>
      <td><b>Std Rate Per 1000</b></td>
      <td>Age-standardized rate per 1000 people. Calculated using a weighted average of the number
      <br/>of occurrences in the local population according to the age distribution of the standard population.</td>
      <td> 0 (if Numerator = 0), 0.76, 6.11, 15.36</q></td>
    </tr>
    <tr>
      <td><b>Standardized 95% CI</b></td>
      <td>95% lower and upper confidence level on the age-standardized rate per 1000 people. 
      <br/>Assumes Numerator is normally-distributed and Denominator is known exactly.</td>
      <td> (NA,NA) (if Numerator = 0), (0.09,2.78), (4.74,7.75), (14.57,16.54)</q></td>
    </tr>
    <tr>
      <td><b>Std Variance</b></td>
      <td>Calculated assuming age-weighted Numerator is Poisson-distributed and Denominator is known exactly.</td>
      <td>  0 (if Numerator = 0), 2.96E-07, 5.52E-07, 3.53E-07</q></td>
    </tr>
  </table>
"
