# Load packages ----
require(shiny)
require(readr)
require(DT)
require(stringr)
require(dplyr)
library(networkD3)
library(rsconnect)
library(XML)
library(RCurl)
library(shiny)
library(ASySD)
library(shinythemes)
library(shinycssloaders)
library(htmlwidgets)
library(shinyWidgets)
library(bslib)
library(RecordLinkage) #contains compare.dedup function
options(shiny.maxRequestSize=1000*1024^2, timeout = 40000000)

# UI ----
ui <- navbarPage(
  
  title="ASySD",
  
  theme = bs_theme(bg = "rgb(251, 251, 251)", 
                   secondary = "#754E9B", 
                   success = "#ABB9E4", 
                   info = "#82D173", 
                   warning = "#FFC07F", 
                   danger = "#C1666B",
                   font_scale = NULL, 
                   bootswatch = "flatly", 
                   fg = "#000"),
                 
  

# UI side panel ----
  tabPanel("Upload Citations",
  
    sidebarLayout(
      
      sidebarPanel(
             
    
# Input: select an input type  ----
h3("Upload citations file"),

shinyWidgets::radioGroupButtons(
  inputId = "fileType",
  label = "Chose a file type",
  choices = c("Endnote XML", 
              "CSV", "TXT"),
  status="primary",
  individual = TRUE,
  checkIcon = list(
    yes = icon("ok", 
               lib = "glyphicon"),
    no = icon("remove",
              lib = "glyphicon"))
),


# Input: select a file to upload----
fileInput("uploadfile", "Choose a file to upload",
          multiple = FALSE,
          placeholder = "No file selected")
),



# UI main panel -----
mainPanel(
  
  h3("Preview citations"),
 
  textOutput("Post_upload_text"),
  
  # Output: datatable of citations uploaded ----
  DT::dataTableOutput("citation_table") %>% withSpinner(color="#96c296"))
)),


# UI auto deduplication page  ----
tabPanel("Deduplicate",

    tabsetPanel(
           tabPanel("Automated deduplication", 
                    
                    sidebarLayout(
                      
                      sidebarPanel(
                        
                        h3("Auto-deduplication options"),
                   
                    # Text about ASySD ----     
                    textOutput("ASySD_pretext"),
                    
                    h5("Unique ID"),
                    
                    # User option: select an ID column ----
                    uiOutput("Id_col_picker"),
                    
                    h5("Citation to keep from each duplicate set"),
                    
                    #  User option: how to determine which citation to keep ----
                    uiOutput("Keep_citation_picker"),
                    
                    # Conditional user option: Select references to keep in dataset ----
                    uiOutput("keepIn"),
                    
                      
# User input: auto deduplicate button -----
  shinyWidgets::actionBttn(
    inputId = "dedupbutton",
    label = "Remove duplicates",
    style = "pill",
    color = "primary"
  ) %>% htmltools::tagAppendAttributes(style =  "background-color: #754E9B")
),
  
mainPanel(
  
  h3("Auto-deduplication results"),
  
  # Output: text displaying dedup results ----
  htmlOutput("ASySD_results")
)
)),

tabPanel("Manual deduplication",
  
sidebarLayout(
  
  sidebarPanel(
    
    h3("Manual deduplication options"),
    
    textOutput("Manual_pretext"),
    
    # Button
    shinyWidgets::actionBttn(
      inputId = "manualdedupsubmit",
      label = "Remove duplicates",
      style = "pill",
      color = "primary"
    ) %>% htmltools::tagAppendAttributes(style =  "background-color: #754E9B"),
    
    h3("Manual deduplication results"),
    
    htmlOutput("Manual_results")
    ),
    
  mainPanel(
    
    h3("Manual deduplication selection"),
    
DTOutput("manual_dedup_dt")

))))),

tabPanel("Summary",
         
         h3("Summary of deduplication steps"),

  sankeyNetworkOutput("sankey"),
  
        h3("Download citations")

),

tabPanel("About",
         
         fluidPage(
           
           p("The", strong("Automated Systematic Search Deduplication tool (AsySD)"), "allows users to input a dataset and remove duplicate publications."),
           
           tags$img(class = "img-responsive img-rounded center-block",
                    src="updated_logo.png",height=175,width=120, align="center"),
           
           p("This tool was developed in the CAMARADES group by", tags$a(href="https://www.researchgate.net/profile/Kaitlyn_Hair", "Kaitlyn Hair")),
           p("The record matching function underlying this tool uses the", tags$a(href="https://rdrr.io/cran/RecordLinkage/", "RecordLinkage"), "package, created by Murat Sariyar and Andreas Borg"),
           p("The code underlying this application is available on", tags$a(href="https://github.com/kaitlynhair/ASySD", "GitHub")),
           p("If you want to use this application for your systematic review, please cite: Hair K, Bahor Z, Macleod M, Liao J, Sena ES. The Automated Systematic Search Deduplicator (ASySD): 
             a rapid, open-source, interoperable tool to remove duplicate citations in biomedical systematic reviews. bioRxiv; 2021. DOI: 10.1101/2021.05.04.442412.")
           
         )))

server <- function(input, output, session){
  
# Citations uploaded - reactive  
RefData <- reactive({ 
  
  shiny::validate(
    shiny::need(input$uploadfile != "", 
                "No citations uploaded yet")
  )
  
  input$uploadfile

    isolate(
      
    if(input$fileType=="Endnote XML"){
      
   citations <- ASySD::load_search(input$uploadfile$datapath, method="endnote")
    }
      
      else if(input$fileType == "CSV"){
        
  citations <- ASySD::load_search(input$uploadfile$datapath, method="csv")
        
      }   else{
  
  citations <- ASySD::load_search(input$uploadfile$datapath, method="txt")
      }
    )
    
  })

# Get ref ID choice
output$Id_col_picker <- renderUI({
  
  selectInput(
    inputId = "Id_col_picker",
    label = "Select a column which contains the unique ID for each citation", 
    choices = (unique(names(RefData()))), # col names in ref data
    selected = "record_id"
  )
})


# when citations uploaded, render unique citation picker
observeEvent(input$uploadfile, { 

output$Keep_citation_picker <- renderUI({

shinyWidgets::radioGroupButtons(
  inputId = "Keep_citation_picker",
  label = "If duplicates exist, which citation should be preferentially retained?",
  choices = c("Citation with abstract", "Citation with a specific label"), 
  selected = c("Citation with abstract"),
  status="primary",
  individual = TRUE,
  checkIcon = list(
    yes = icon("ok", 
               lib = "glyphicon"),
    no = icon("remove",
              lib = "glyphicon"))
)
})

})

# Get choices for keep label
output$keepIn <- renderUI({
  
  shiny::validate(
    shiny::need(input$Keep_citation_picker != "", "")
  )
  
  
  if (input$Keep_citation_picker == "Citation with a specific label") {
  
  
  selectInput(inputId = "keepIn", 
              label = "Specify labelled references to keep in the dataset",
              choices = unique(RefData()$label),
              selected = RefData()$label[1],
              multiple = FALSE)
  }
})


# Datatable of input data ---
 output$citation_table <- renderDT({
   
   preview_10 <- RefData() %>%
     dplyr::select(author, title, year, journal, abstract, doi, number, pages, volume, isbn, label)
   
   preview_10 <- preview_10[c(1:10),] 
     
   DT::datatable(preview_10,
   options = list(dom = 't',
                  scrollX = TRUE,
                  fixedColumns = TRUE,
                  columnDefs = list(list(
                    targets = "_all",
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data != null && data.length > 25 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 25) + '...</span>' : data;",
                      "}")
                    ))),
   class = "display")
 })

 # ASySD user text ----
 output$Post_upload_text <- renderText({
   
   original_refs_n <- as.numeric(nrow(RefData()))
   
   paste("You have uploaded", original_refs_n, "citations. Preview the first 10 below.")
   
 })
 
 
 # ASySD user text ----
 output$ASySD_pretext <- renderText({
   paste("Configure the options below, then click the button to automatically 
remove duplicates.")
   
 })
 
# Action: fomrat citations to dedup ----
 citations_to_dedup <- eventReactive(input$dedupbutton,{
   
   id_col <- sym(input$Id_col_picker)
   id_col <- enquo(id_col)
   
   citations <- RefData() %>%
     mutate(source = label) %>%
     mutate(record_id = !!id_col)
   
 })
 
 # Action: ASySD auto dedup ----
 auto_dedup_result <- eventReactive(input$dedupbutton,{

  result <- dedup_citations(citations_to_dedup(),
                                   keep_source = input$keepIn,
                                   merge_citations = TRUE)
   return(result)
 })

 
 # Output: ASySD auto dedup results text ----
 output$ASySD_results <- renderText({
   
   uniquerefs_n <- as.numeric(length(auto_dedup_result()$unique$duplicate_id))
   original_refs_n <- as.numeric(nrow(RefData()))
   removed_n <- original_refs_n - uniquerefs_n
   
   paste("From a total of", original_refs_n, "citations, ASySD has removed ", "<font color=\"#FF0000\"><b>", removed_n,  "<font color=\"#000000\">", "duplicates.",
         "There are", "<font color=\"#00FF00\"><b>", uniquerefs_n, "remaining.")
   
 })
 
# Action: remove manually selected duplicates ----
manual_dedup_result <- eventReactive(input$manualdedupsubmit,{
  
  removeManual <- auto_dedup_result()$manual %>%
    select(author1, author2, title1, title2, year1, year2, journal1, journal2, doi1, doi2, record_id1, record_id2)
  
  duplicates <- removeManual[input$manual_dedup_dt_rows_selected,]
    
  unique_citations <- auto_dedup_result()$unique
   after <- dedup_citations_add_manual(citations_to_dedup(),
                             merge_citations = TRUE,
                             additional_pairs = duplicates)
})

 # Action: ASySD manual dedup pre text ----
output$Manual_pretext <- renderText({
 
  manualrefs <- auto_dedup_result()$manual
  manualrefs <- as.numeric(length(manualrefs$record_id1))
  
  paste(manualrefs, "pairs of citations require manual deduplication. Review the pairs in the table
        to the right. Select all rows which contain duplicate pairs and click the button below to remove extra
        duplicates.")
  
  
})

 # Action: ASySD manual dedup results text ----
output$Manual_results <- renderText({
  
  
  unique_manual <- as.numeric(length(manual_dedup_result()$duplicate_id))
  manualremove <-  as.numeric(length(auto_dedup_result()$unique$duplicate_id)) - unique_manual 
  
  paste(manualremove, "additional citations were removed manually", unique_manual, "unique citations now remain")
  
})

 # Output: manual dedup datatable -----
 output$manual_dedup_dt <- renderDT(
   auto_dedup_result()$manual %>%
     select(author1, author2, title1, title2, year1, year2, journal1, journal2, doi1, doi2, record_id1, record_id2),
  options = list(pageLength = 10,
                 dom = 't',
                 scrollX = TRUE,
                 fixedColumns = TRUE,
  columnDefs = list(list(visible=FALSE, targets=c(11,12)))),
   selection=list(mode="multiple"))

 
 # Output: sankey diagram ---
 output$sankey <- renderSankeyNetwork({
   
   n_search <- original_refs_n <- as.numeric(nrow(RefData()))
   n_unique_auto <- as.numeric(length(auto_dedup_result()$unique$duplicate_id))
   n_unique_manual <- as.numeric(length(manual_dedup_result()$duplicate_id))
  
   links <- data.frame(source = 
                         c(paste0("Original citations (", n_search, ")"),
                           paste0("Original citations (", n_search, ")"),
                           paste0("Remaining citations (", n_unique_auto, ")"),
                           paste0("Remaining citations (", n_unique_auto, ")")),
                       target = 
                         c(paste0("Remaining citations (", n_unique_auto, ")"),
                           paste0("Auto-dedup removed (", n_search - n_unique_auto, ")"), 
                           paste0("Unique citations (", n_unique_manual, ")"),
                           paste0("Manually removed (", n_unique_auto - n_unique_manual, ")")),
                       value = c(n_search, n_unique_auto,  n_search - n_unique_auto, n_unique_auto - n_unique_manual))
   
   # From these flows we need to create a node data frame: it lists every entities involved in the flow
   nodes <- data.frame(
     name=c(as.character(links$source), 
            as.character(links$target)) %>% unique()
   )
   
   # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
   links$IDsource <- match(links$source, nodes$name)-1 
   links$IDtarget <- match(links$target, nodes$name)-1
   
   # Make the Network
   p <- sankeyNetwork(Links = links, Nodes = nodes,
                      Source = "IDsource", Target = "IDtarget",
                      Value = "value", NodeID = "name", 
                      sinksRight=FALSE, nodeWidth =30, height = 100, width = 800, fontSize = 14,
                      units = "Citations")
   p
   
 })


}

# Run the application 
shinyApp(ui = ui, server = server)



