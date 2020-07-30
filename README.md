# Automated Systematic Search Deduplication tool (ASySD)

## Background
Removing duplicate references obtained from different databases is an essential step when conducting and updating systematic literature reviews. ASySD is a tool to automatically identify and remove duplicate records. 

## Shiny application
The tool is written in R and has been created as a Shiny web app available [online](https://camarades.shinyapps.io/RDedup/). For very large datasets (>50,000 records) it is advisable to download the code and run locally as a Shiny app within RStudio. 

## Shiny app functionality
Users can deduplicate records by following these steps: 
1. Upload reference library as an .XML file direct from Endnote, a .csv file or a .txt tab delimited file. 
2. Specify any labelled records to preferentially keep in the library e.g. keep references obtained in a previous search labelled as "old" over the same records found in a new search
3. Navigate to the **Deduplicate data** tab and click a single button in the **Automated Deduplication** section to remove duplicates automatically. Depending on the size of the dataset, this can take several minutes. ASySD will highlight how many records have been removed.
4. Remove any additional duplicates manually under the **Manual Deduplication** section. Select the IDs you want to remove from the side-by-side table of matching duplicates and click to remove from your reference library
5. Download unique reference library (there are several options here - for example to download your library after automated de-duplication only or to download the reference pairs ASySD detected) 

## Underlying de-duplication algorithm
The features of the underyling code are
1. Cleaning and formatting of references to improve matching (e.g. removing punctuation, captialising all text) 
2. Several rounds of reference matching using the existing [RecordLinkage R package](https://rdrr.io/cran/RecordLinkage/) with different blocking "must match 100% on" criteria
3. Additional filtering criteria to ensure matches are genuine matches - this is based on a heuristic approach optimised with several existing CAMARADES systematic search datasets 
4. Filtering criteria to determine if a match MAY be a duplicate - if so these pairs are presented for manual de-duplication 
5. Removal of duplicate records based on label (if specified to keep specific label preferentially) or based on which record has an abstract, preferentially removing the one with a missing abstract

## Tool performance 
Performance evaluation for the ASySD tool versus other automated de-duplication tools is [ongoing](10.17605/OSF.IO/W3MAK). 


