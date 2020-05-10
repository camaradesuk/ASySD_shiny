#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


# Load packages ----
require(shiny)
require(readr)
require(DT)
require(stringr)
require(dplyr)
require(XML)
require(RCurl)
require(shinythemes)
library(htmlwidgets)
library(shinyWidgets)
require(RecordLinkage) #contains compare.dedup function
options(shiny.maxRequestSize=300*1024^2)
source("functions/dedup_refs_app.R")

# App title ----
ui <- navbarPage("ASySD",
  
  theme = shinytheme("flatly"),
  
 # Upload files panel  ----
  tabPanel("Uploading Files",
  
    fluidPage(
    
# Input: Select a input type ----

h3("Upload and view reference library"),

shinyWidgets::radioGroupButtons(
  inputId = "fileType",
  label = "Chose a file type",
  choices = c("Endnote XML", 
              "CSV", "TXT"),
  individual = TRUE,
  checkIcon = list(
    yes = tags$i(class = "fa fa-circle", 
                 style = "color: steelblue"),
    no = tags$i(class = "fa fa-circle-o", 
                style = "color: steelblue"))
),


# Input: Select a file ----

fileInput("uploadfile", "Choose a file to upload",
          multiple = FALSE,
          placeholder = "No file selected"),


# Input: Label references to keep in dataset 

textOutput("If you want to preferentially keep records with a specific label e.g. old records if you have an updated search, specify it below. Otherwise click Deduplicate as normal"),

selectInput(inputId = "keepIn", 
            label = "Specify labelled references to keep in the dataset",
            choices = "",
            selected = "De-duplicate as norman",
            multiple = FALSE),

# Output: Datatable ----

   DTOutput("contents"),

     conditionalPanel("$('#contents').hasClass('recalculating')",
                  tags$div('Loading table ... ')))),

# Deduplicate files panel  ----
tabPanel("Deduplicate Data",

  fluidPage(
    
       # Download refs -----

# Deduplicate refs -----
      
h3("Automated deduplication"),

  # Button
  shinyWidgets::actionBttn(
    inputId = "dedupbutton",
    label = "Click to deduplicate references",
    style = "unite",
    color = "success"
  ),
  
 # Output: Data file ----

br(),
br(),

conditionalPanel("$('#ASySD_results_removed').hasClass('recalculating')",
                 tags$div('Loading deduplication results ...')),

textOutput("ASySD_results_removed"),
textOutput("ASySD_results_unique"),
textOutput("ASySD_results_manual"),
  br(),
  br(),
  hr(),
h3("Manual deduplication"),
  p(strong("The potential reference pairs for manual deduplication will be displayed in the table below. Please select the RecordIDs you want to remove and click the button to remove")),
  br(),

# Button
shinyWidgets::actionBttn(
  inputId = "manualdedupsubmit",
  label = "Click to remove all selected references from your library",
  style = "unite",
  color = "success"
),

br(),
br(),

textOutput("Manual_results"),

DTOutput("dedup"),
  
  conditionalPanel("$('#dedup').hasClass('recalculating')",
                   tags$div('Loading table ... '))
)),

tabPanel("Downloads",


h3("Download unique references and reference pairs"),

br(),


p("Download unique records after automated and manual deduplication"),
downloadButton("downloadUniqueDataManual", "Download unique references - including manual deduplication"),

br(),
br(),
br(),

p("Download SyRF-ready unique records after automated and manual deduplication which you can upload directly to a SyRF project"),
p("To find out more about our systematic review platform SyRF, visit", tags$a(href="http://syrf.org.uk/", "SyRF.org.uk")),
downloadButton("downloadSyRFData", "Download SyRF-ready csv - including manual deduplication"),

br(),
br(),
br(),

p("Download unique records after automated deduplication"),
downloadButton("downloadData", "Download unique references - automated deduplication"),

br(),
br(),
br(),


p("Download all matching pairs of records detected by automated deduplication"),
downloadButton("downloadPairs", "Download reference pairs - automated deduplication"),

br(),
br(),
br(),


p("Download all potentially matching pairs of records detected by automated deduplication - please note that many of these will not 
           be true matches as they have not been filtered by our true match algorithm"),
downloadButton("downloadAllPairs", "Download all potentially matching pairs")),

tabPanel("About",
         
         fluidPage(
           
           p("The", strong("Automated Systematic Search Deduplication tool (AsySD)"), "allows users to input a dataset and remove duplicate publications"),
           
           p("Currently the tool accepts .csv and .txt input with the following columns:-"),
           
           tags$div(
             tags$ul(
               tags$li("Author"), 
               tags$li("Year"),
               tags$li("Abstract"),
               tags$li("Journal"),
               tags$li("DOI"),
               tags$li("Pages"),
               tags$li("Volume"),
               tags$li("Number"),
               tags$li("RecordID")
             )
           ),
           p("The app also accepts .xml input exported from Endnote"),
           
           hr(),
           
           p("This tool was developed in the CAMARADES group by", tags$a(href="https://www.researchgate.net/profile/Kaitlyn_Hair", "Kaitlyn Hair")),
           p("The record matching function underlying this tool uses the", tags$a(href="https://rdrr.io/cran/RecordLinkage/", "RecordLinkage"), "package, created by Murat Sariyar and Andreas Borg"),
           p("The code underlying this application is available on", tags$a(href="https://github.com/kaitlynhair/RDedup", "GitHub")),
           p("If you want to use this application for your systematic review, please cite Hair, K 2019. RDedup, https://github.com/kaitlynhair/RDedup")
           
         )))

server <- function(input, output, session){
  
RefData <- reactive({ 
  
  validate(
    need(input$uploadfile, 'Please choose a file to upload')
  )

    if (input$fileType=="Endnote XML"){
    
    library(XML)
    library(RCurl)
        
    newdat<-xmlParse(input$uploadfile$datapath)
    x <-  getNodeSet(newdat,'//record')
        
     xpath2 <-function(x, ...){
      y <- xpathSApply(x, ...)
      y <- gsub(",", "", y)  # remove commas if using comma separator
      ifelse(length(y) == 0, NA,  paste(y, collapse=", "))
        }
     
     newdat <- data.frame(
      Author = sapply(x, xpath2, ".//contributors/authors", xmlValue),
      Year   = sapply(x, xpath2, ".//dates/year", xmlValue),
      Journal = sapply(x, xpath2, ".//periodical/full-title", xmlValue),
      DOI = sapply(x, xpath2, ".//electronic-resource-num", xmlValue),
      Title = sapply(x, xpath2, ".//titles/title", xmlValue),
      Pages = sapply(x, xpath2, ".//pages", xmlValue),
      Volume = sapply(x, xpath2, ".//volume", xmlValue),
      Number = sapply(x, xpath2, ".//number", xmlValue),
      Abstract = sapply(x, xpath2, ".//abstract", xmlValue),
      RecordID = sapply(x, xpath2, ".//rec-number", xmlValue),
      ISBN = sapply(x, xpath2, ".//isbn", xmlValue),
      SecondaryTitle = sapply(x, xpath2, ".//titles/secondary-title", xmlValue),
      "PDF Relative Path" = sapply(x, xpath2, ".//urls/pdf-urls", xmlValue), 
      Url = sapply(x, xpath2, ".//urls/related-urls", xmlValue),
      Label = sapply(x, xpath2, ".//label", xmlValue)
      )
    
      newdat <- newdat %>%
       mutate(Label = ifelse(is.na(Label), "NA", paste(Label)))
       return(newdat)
    }
    
    else if (input$fileType=="CSV"){
      
        newdat <- read.csv(input$uploadfile$datapath) %>%
          select(Author,
                 Year,
                 Journal,
                 DOI,
                 Title,
                 Pages,
                 Volume,
                 Number,
                 Abstract,
                 RecordID,
                 ISBN,
                 Label)
        
        newdat <- newdat %>%
          mutate(Label = ifelse(is.na(Label), "NA", paste(Label))) %>%
          mutate(Label = ifelse(is.na(ISBN), "NA", paste(ISBN)))
        
        return(newdat)
    }
  
    
    else if (input$fileType=="TXT"){
      
        newdat <- read.table(input$uploadfile$datapath) %>%
          select(Author,
                 Year,
                 Journal,
                 DOI,
                 Title,
                 Pages,
                 Volume,
                 Number,
                 Abstract,
                 RecordID,
                 ISBN,
                 Label)
        
        newdat <- newdat %>%
          mutate(Label = ifelse(is.na(Label), "NA", paste(Label))) %>%
          mutate(Label = ifelse(is.na(ISBN), "NA", paste(ISBN)))
        
        return(newdat)
      }
 })

# Get choices for keep label

observe({
  updateSelectInput(session,
                    "keepIn",
                    choices = c("De-duplicate as normal", RefData()$Label))
})
  
# Datatable of input data ---- 
 
output$contents <- renderDT(
  RefData() %>%
      select(-Abstract, -Label), 
    options = list(pageLength = 10)
  )
    
 observeEvent(input$dedupbutton, {
 print(paste("Deduplicating..."))
   })

dedupData <- eventReactive(input$dedupbutton,{

   result <- dedup_refs_app(RefData(),
                        LabelKeep = input$keepIn)
   return(result)
 })



dedupData_removemanual <- eventReactive(input$manualdedupsubmit,{
  
  removeManual <- dedupData()$ManualDedup %>%
    select(Author1, Author2, Title1, Title2, Year1, Year2, Journal1, Journal2, DOI1, DOI2, RecordID1, RecordID2)
 
  removeManual <- removeManual[input$dedup_cells_selected]
  
  dedupdat <- dedupData()$Unique %>%  
    filter(!RecordID %in% removeManual)
  
  return(dedupdat)
  
})

# Datatable of data for manual deduplication ----
  
output$ASySD_results_removed <- renderText({

  refsremoved <- dedupData()$DuplicateRefsRemoved
  refsremoved <- as.numeric(length(refsremoved$RecordID))

  paste("ASySD has removed ", refsremoved, "references from your reference library.")
 
})


output$ASySD_results_unique <- renderText({
  
 
  uniquerefs <- dedupData()$Unique
  uniquerefs <- as.numeric(length(uniquerefs$RecordID))
  
  paste("There are", uniquerefs, "remaining.")
  
})


output$ASySD_results_manual <- renderText({
  
 
  manualrefs <- dedupData()$ManualDedup
  manualrefs <- as.numeric(length(manualrefs$RecordID1))
  
  paste(manualrefs, "pairs of records require manual deduplication.")
  
})

output$Manual_results <- renderText({
  
  
  unique_manual <- as.numeric(length(dedupData_removemanual()$RecordID))
  manualremove <-  as.numeric(length(dedupData()$Unique$RecordID)) - unique_manual 
  
  paste(manualremove, "records were removed manually", unique_manual, "unique records remain")
  
})


 output$dedup <- renderDT(
   dedupData()$ManualDedup %>%
     select(Author1, Author2, Title1, Title2, Year1, Year2, Journal1, Journal2, DOI1, DOI2, RecordID1, RecordID2),
  options = list(pageLength = 10),
   selection=list(mode="multiple", target="cell"))


 syrfoutput <- reactive({
   
  unique_refs <- dedupData_removemanual() %>%
   rename("Publication Name" = Journal,
          "PDF Relative Path" = PDF.Relative.Path,
          "Authors" = Author) %>%
   mutate("Reference Type" = "Journal Article",
          "Alternate Name" = RecordID,
          "Author Address" = "",
          "Keywords" = RecordID,
          Year = ifelse(is.na(Year), "0000", paste(Year)),
          Authors = ifelse(is.na(Authors), "", paste(Authors)),
          Year = as.integer(Year))
 
  unique_refs <- unique_refs %>%
   select(Title, Authors, "Publication Name", "Alternate Name", Abstract, Url, "Author Address", Year, DOI, Keywords, "Reference Type", "PDF Relative Path")
 
  unique_refs["PDF Relative Path"] <- as.data.frame(sapply(unique_refs["PDF Relative Path"], function(x) gsub("internal-pdf://", "", x)))
 
  unique_refs$Year = as.character(paste(unique_refs$Year))
 
 })
 
# Downloadable csv of selected dataset ----
 output$downloadData <- downloadHandler(
    filename = function() {
      paste("dedupdata", ".csv", sep = ",")
    },
    content = function(file) {
      write.csv(dedupData()$Unique, file, row.names = TRUE)
      print(nrow(dedupData()$Unique))
    })
 
 output$downloadSyRFData <- downloadHandler(
   filename = function() {
     paste("syrf_search", ".csv", sep = ",")
   },
   content = function(file) {
     write.csv(syrfoutput(), file, row.names = TRUE)
     print(nrow(syrfoutput()))
   })


output$downloadUniqueDataManual <- downloadHandler(
  filename = function() {
    paste("dedupdata_withmanualdedup", ".csv", sep = ",")
  },
  content = function(file) {
    write.csv(dedupData_removemanual(), file, row.names = TRUE)
    print(nrow(dedupData_removemanual()))
  })

# Downloadable csv of pairs ----
output$downloadPairs <- downloadHandler(
  filename = function() {
    paste("pairs", ".csv", sep = ",")
  },
  content = function(file) {
    write.csv(dedupData()$TruePairs, file, row.names = TRUE)
    print(nrow(dedupData()$TruePairs))
  })

# Downloadable csv of references to deduplicate manually  ----
output$downloadManual <- downloadHandler(
  filename = function() {
    paste("pairs", ".csv", sep = ",")
  },
  content = function(file) {
    write.csv(dedupData()$ManualDedup, file, row.names = TRUE)
    print(nrow(dedupData()$ManualDedup))
  })

output$downloadAllPairs <- downloadHandler(
  filename = function() {
    paste("allpairs", ".csv", sep = ",")
  },
  content = function(file) {
    write.csv(dedupData()$PotentialPairs, file, row.names = TRUE)
    print(nrow(dedupData()$PotentialPairs))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)



