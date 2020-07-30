# medExtractR

The **medExtractR** package provides a tool to extract medication dosing information from free-text clinical notes. For a specified drug of interest, the system will identify drug entities such as strength, dose amount (units of the drug taken), frequency, or drug intake time.

Users will typically call the function `medExtractR`, specifying the drug names of interest for a clinical note. The `medExtractR` function also heavily relies on the `extract_entities` function, which focuses on identifying drug entities within a search window around the drug name. 

Install from CRAN using `install.packages("medExtractR")`.
