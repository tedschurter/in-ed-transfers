
---

#### **About**

##### This repository aggregates, cleans, visualizes and maps data related to school transfers within Indiana's primary and secondary school system. The final output is a [webpage](https://tedschurter.github.io/in-ed-transfers/transfer-combo.html) that explains how transfers work and explores the transfer relationships among various school corporations and schools throughout the state. In addition to the explanatory graphics, the project includes three maps that are incoporated into the page:

  - The **[Outgoing transfers](https://tedschurter.github.io/in-ed-transfers/20240617-outgoing-transfers.html)** map visualizes the number of students who transfer from a school corporation (selected with the filter on the upper left-hand side) to another public school corporation, public charter school or private school. Popup charts display the number of transfers from the selected school corporation to enrolled schools/corporations and visually contrasts their available test scores. 
  
  - The **[Incoming transfers](https://tedschurter.github.io/in-ed-transfers/20240617-incoming-transfers.html)** map visualizes the number of students that transfer to the enrolled school/corporation (selected with the filter on the upper left-hand side) from school corporations that sent transfer students. Popup charts display the number of transfers from a school corporation to the selected enrolled school/corporation and visually contrasts their available test scores.

  - The **[Net transfers](https://tedschurter.github.io/in-ed-transfers/20240618-transfer-map.html)** map visualizes the number of transfer students entering or leaving a school corporation per 100 students that have legal settlement within the corporation. A popup chart visualizes the 2024 rate for each school corporation.
  
---

#### **Data**

##### This project's data was downloaded from the [Indiana Department of Education](https://www.in.gov/doe/it/data-center-and-reports/). 

---
  
#### **Data Scripts**
 

**Script** | **Description** | **Exports** 
:---|:---|:---|
[20240607_schl_corp.R](scripts/20240607_schl_corp.R)|Creates dataframe of school corporation boundaries downloaded from Tigris package with names that align with updated information from Indiana Department of Education (IDOE) school directory data. Geocodes IDOE addresses and confirms they fall within geographic boundaries provided by Tigris package. |[20240607_corporation_bounds.geojson](clean_data/20240607_corporation_bounds.geojson) [20240607_corporation_pts.csv](clean_data/20240607_corporation_pts.csv) [20240607_corporation_pts.geojson](clean_data/20240607_corporation_pts.geojson)
[20240612_test_scores.R](scripts/20240612_test_scores.R)|Imports and aggregates 2018 to 2023 test score data for Indiana's state-administered tests (ILEARN, IREAD) and SATs and calculates percentage of students that meet or exceed state expectations for each by school corporation and enrolled school.|[20240612_iread_23.csv](clean_data/20240612_iread_23.csv) <br> [20240612_learn_ela_23.csv](clean_data/20240612_learn_ela_23.csv) <br> [20240612_iread_23.csv](clean_data/20240612_iread_23.csv) <br> [20240612_ilearn_math_23.csv](clean_data/20240612_ilearn_math_23.csv) <br> [20240612_ilearn_sc_23.csv](clean_data/20240612_ilearn_sc_23.csv)<br>[20240612_ilearn_ss_23.csv](clean_data/20240612_ilearn_ss_23.csv) <br> [20240612_ilearn_bio_23.csv](clean_data/20240612_ilearn_bio_23.csv) <br> [20240612_tst_scores_23.csv](clean_data/20240612_tst_scores_23.csv)
[20240612_schl_enrollment.R](scripts/20240612_schl_enrollment.R) |Imports and aggregates IDOE enrollment data for 2018 to 2024. Also exports enrollment data through 2023 with aggregated 2023 test score data (most recent test data available at the time).|[20240612_enrollment_14_24.csv](clean_data/20240612_enrollment_14_24.csv) <br> [test_nrl_str_23.csv](clean_data/test_nrl_str_23.csv)
[20240612_transfers.R](scripts/20240612_transfers.R)| Imports and aggregates student transfer data totals from 2018 to 2024; clean and update school corporation names.|[20240612_transfers_18_24.csv](clean_data/20240612_transfers_18_24.csv) <br> [20240613_schl_xfr_pts.R](scripts/20240613_schl_xfr_pts.R) |Joins school corporation location point data to transfer data and adds geolocated private school points. Exports transfer data with lat and longitude of school corporation/private school locations for mapping.|[20240613_transfer_pts_18_24.csv](clean_data/20240613_transfer_pts_18_24.csv)
[20240614_transfers_long.R](scripts/20240614_transfers_long.R)|Convert wide transfer data to long for use in charts, maps. Export csv. |[20240614_sxfer.csv](clean_data/20240614_sxfer.csv)
[20240614_incoming_xfr_plots.R](scripts/20240614_incoming_xfr_plots.R)|For every school that receives *incoming* transfers create combination chart that 1.) provides the context for and shows the volume of transfers from 2018 to present between the enrolled school and the settlement school and 2.) contrasts the test scores between the enrolled school and the settlement school. Uploads charts to AWS S3 bucket. Charts used as popups on Leaflet map.| 16,234 png files. <br> Example filename: it-0015-0025.png
[20240614_nrl_plots.R](scripts/20240614_nrl_plots.R)|For every school that sends *outgoing* transfers create combination chart that 1.) provides the context for and shows the volume of transfers from 2018 to present between the settlement school and the enrolled school 2.) contrasts the test scores between the settlement school and the enrolled school. Uploads charts to AWS S3 bucket. Charts used as popups on Leaflet map.| 16,324 png files. <br> Example filename: 0015-0025.png
[20240614_stlmt_plots.R](scripts/20240614_stlmt_plots.R)|Creates small multiple chart showing volume of outgoing transfers by transfer type for Indiana school corporations. Uploads charts to AWS S3 bucket. Charts used as popups on Leaflet map.| 294 png files. <br> Example filename: 0015-0015.png
[20240619_net_plots.R](scripts/20240619_net_plots.R)| Creates charts that visualize net transfers for school corporations. Uploads charts to AWS S3 bucket. Charts used as popups on Leaflet map.| 290 png files. <br> Example filename: net-0015.png
[20240724_page_charts.R](scripts/20240724_page_charts.R)|Charts for project webpage. Includes line charts for absolute and relative changes over time for transfer type and enrollment; scatter plot Visualizes schools and corporations on a grid based on how many corporations they receive transfer students from and how many schools/corporations they send transfer students to.|new-scatter_03a.svg <br> typ-trend.svg <br> res_pc.svg
[20240724_page_scrolly_charts.R](scripts/20240724_page_scrolly_charts.R)|Incrementally changing charts for scrollytelly section of project webpage|lst_1.png - lst_6.png <br> scrolly2-mult_schls-1.png - scrolly2-mult_schls-5.png <br> scrolly3_in_xfr_1.png - scrolly3_in_xfr_3.png




#### **Webpage Scripts**
 

**Script** | **Description** | **Exports** 
:---|:---|:---|
[index.html](docs/index.html)| Webpage for project includes charts and maps, hosted on Github pages. 
[20240617-outgoing-transfers.Rmd](docs/20240617-outgoing-transfers.Rmd)| Uses [Crosstalk](https://rstudio.github.io/crosstalk/) to create a filterable Leaflet map of *outgoing* transfers from every school corporation in Indiana|[20240617-outgoing-transfers.html](https://tedschurter.github.io/in-ed-transfers/20240617-outgoing-transfers.html) 
[20240617-incoming-transfers.Rmd](docs/20240617-incoming-transfers.Rmd)| Uses [Crosstalk](https://rstudio.github.io/crosstalk/)  to create a filterable Leaflet map of *incoming* transfers to every school/corporation in Indiana that receives transfer students from another school corporation|[20240617-incoming-transfers.html](https://tedschurter.github.io/in-ed-transfers/20240617-incoming-transfers) 
[20240618-transfer-map.Rmd](docs/20240618-transfer-map.Rmd)| Uses Leaflet map to visualize the net rate of transfer students in and out of Indiana school corporations |[20240618-transfer-map.html](https://tedschurter.github.io/in-ed-transfers/20240618-transfer-map.Rmd) 
