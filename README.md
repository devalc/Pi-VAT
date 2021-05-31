# Pi-VAT
***
<p align="center">
  <img src="https://raw.githubusercontent.com/devalc/In-WPaT/main/www/Pi-VAT_hex.svg" width=50%/>
</p>

This repository contains a post-processing, interactive visualization, and analysis tool (Pi-VAT). It is an interactive tool for synthesis and decision support using multi-watershed, multi-scenario outputs from complex, geo-spatial water quality models. It is currently programmed to process outptus from two models:

- Online interface ([WEPPcloud](<https://wepp.cloud/weppcloud/>)) for the Watershed Erosion Prediction Project (WEPP) 
- Soil and Water Assessment Tool ([SWAT](<https://swat.tamu.edu/>)). 

Based on the multi-watershed, multi-scenario simulations from these models this tool facilitates interactive identification of sources areas and hotspots of the water quality metric of concern and areas that are suitable for targeted management. For these areas the tool also faciliates comparative analysis by quantifying and visualizing differences between different scenarios. 


## Find this application in action here:

[Pi-VAT](<https://cdeval.shinyapps.io/Pi-VAT/>)

## How to prepare WEPPcloud outputs for Pi-VAT?

We have an approach on WEPPcloud to do this but we working on its seamless integration with Pi-VAT. In the mean time if you would like to create such files and evaluate in Pi-VAT please reach out to us and we can help you create them.

## How to prepare SWAT outputs for Pi-VAT?

If you want to know how to create files from SWAT outputs that are compatible with this tool
please click on the link below to navigate to an example R script that you can
repurpose for synthesizing your runs:


- [Summarize SWAT HRUs for Pi-VAT:](<https://raw.githubusercontent.com/devalc/Pi-VAT/main/Other_R/summarise_swat_hru_results.R>)

- [Summarize SWAT RCH for Pi-VAT:](<https://raw.githubusercontent.com/devalc/Pi-VAT/main/Other_R/summarise_swat_reach_results.R>)

- [Summarize SWAT subbasin for Pi-VAT:](<https://raw.githubusercontent.com/devalc/Pi-VAT/main/Other_R/summarise_swat_subbasin_results.R>)

### SWAT templates for Pi-VAT

[Templates for the input data (also used as default data in the tool) are available here](<https://github.com/devalc/Pi-VAT/tree/main/data>)

- HRU template file has "__hru__" in the filename
- Reach template file has "__rch__" in the filename
- Subbasin template file has "__sub__" in the filename
