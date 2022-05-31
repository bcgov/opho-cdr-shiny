
about<-"<br/><br/>
  This dashboard facilitates the exploration and visualization of spatial and temporal
  trends of 25 different chronic diseases across the Province of British Columbia. The data 
  is sourced from the BC Chronic Disease Registry (BCCDR). Data is available for three different rate types, 
  
  <br/><br/>
  The dashboard contains 3 tabs, each of which are described below .<br/>
  <ul>
   <li><b>By Disease</b></li>
   This tab allows for the trend comparisons of one disease over several Health Authorities(HA)  
   or Community Health Service Areas (CHSA). In this tab the user should select a rate type, 
   a disease, geography type, year, and gender. The user can also optionally specify 
   multiple HAs or CHSAs. <br/><br/>
   <li><b>By Region</b></li>
   This tab allows for the trend comparisons of several diseases in one particular HA or CHSA.
   In this tab the user should select a rate type, disease(s) of interest, geography type,
   year, and gender. <br/><br/>
   <li><b>Data</b></li>
   This tab retrieves all data specified by the user. In this tab the user should select a rate type,
   disease(s), health region(s), year range, and gender. There is also an option for the user to
   download the selected data.<br/><br/>
  </ul>"


rates<-"The definitions of the rate types are provided below.
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
                            "