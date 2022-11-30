# Load packages ----
require(shiny)
require(readr)
require(DT)
require(stringr)
require(dplyr)
library(networkD3)
library(rsconnect)
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

  tags$head(includeHTML(("google-analytics.html"))),

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
  tabPanel("Upload",

           sidebarLayout(

             sidebarPanel(


               # Input: select an input type  ----
               h4("Upload citations file ", icon("upload")),

               shinyWidgets::prettyRadioButtons(
                 inputId = "fileType",
                 label = "Chose a file type",
                 inline = TRUE,
                 choices = c("Endnote XML",
                             "CSV", "Tab delimited"),
                 status="primary"),

               uiOutput("ASySD_pre_upload"),
               br(),

               # Input: select a file to upload----
               fileInput("uploadfile", "Choose a file to upload",
                         multiple = FALSE,
                         placeholder = "No file selected")
             ),

             # UI main panel -----
             mainPanel(

               h4("Preview citations"),

               textOutput("Post_upload_text"),

               # Output: datatable of citations uploaded ----
               DT::dataTableOutput("citation_table") %>% withSpinner(color="#754E9B", type=7))
           )),


  # UI auto deduplication page  ----
  tabPanel("Deduplicate",

           tabsetPanel(
             tabPanel("Automated deduplication",

                      sidebarLayout(

                        sidebarPanel(

                          h4("Auto-deduplication options ", icon("cog")),

                          # Text about ASySD ----
                          textOutput("ASySD_pretext"),

                          h5("Unique ID"),

                          # User option: select an ID column ----
                          uiOutput("Id_col_picker"),

                          h5("Citation to keep from each duplicate set"),

                          #  User option: how to determine which citation to keep ----
                          uiOutput("Keep_citation_picker"),

                          # Conditional user option: Select references to keep in dataset ----
                          uiOutput("keepLabel"),

                          # Conditional user option: Select references to keep in dataset ----
                          uiOutput("keepSource"),


                          # User input: auto deduplicate button -----
                          shinyWidgets::actionBttn(
                            inputId = "dedupbutton",
                            label = "Remove duplicates",
                            style = "pill",
                            color = "primary"
                          ) %>% htmltools::tagAppendAttributes(style =  "background-color: #754E9B")
                        ),

                        mainPanel(

                          h4("Auto-deduplication results"),

                          # Output: text displaying dedup results ----
                          htmlOutput("ASySD_results") %>% withSpinner(color="#754E9B", type=7)
                        )
                      )),

             tabPanel("Manual deduplication",


                          h4("Manual deduplication ", icon("check-square")),

                          textOutput("Manual_pretext"),

                          br(),

                          # Button
                          shinyWidgets::actionBttn(
                            inputId = "manualdedupsubmit",
                            label = "Remove duplicates",
                            style = "pill",
                            color = "primary"
                          ) %>% htmltools::tagAppendAttributes(style =  "background-color: #754E9B"),

                          htmlOutput("Manual_results") %>% withSpinner(color="#754E9B", type=7),


                          h4("Duplicate pair selection"),

                          DTOutput("manual_dedup_dt")

                        ))),

  tabPanel("Summary",

           h4("Summary of deduplication steps"),

           sankeyNetworkOutput("sankey") %>% withSpinner(color="#754E9B", type=7)
  ),

  tabPanel("Download",

           h4("Download unique citations", icon("download")),

           shinyWidgets::prettyRadioButtons(
             inputId = "export_type",
             label = "Chose an export type",
             choiceNames = c("Endnote tab delimited", "RIS", "SyRF CSV", "BibTex"),
             choiceValues = c("txt", "ris", "csv", "bib"),
             status="success"),

           downloadBttn(
             "download",
             label = "Download citations",
             style = "unite",
             color = "primary",
             size = "sm",
             block = FALSE,
             no_outline = TRUE
           )

  ),

  tabPanel("About",

           fluidPage(

             p("The", strong("Automated Systematic Search Deduplication tool (AsySD)"), "allows users to input a dataset and remove duplicate publications."),

             tags$img(class = "img-responsive img-rounded center-block",
                      src="updated_logo.png",height=150,width=150, align="center"),

             p("This tool was developed in the CAMARADES group by", tags$a(href="https://www.researchgate.net/profile/Kaitlyn_Hair", "Kaitlyn Hair."),
             "If you have any questions about the tool, please raise an issue on the GitHub (see below) or email her at kaitlyn.hair@ed.ac.uk"),
             p("The record matching function underlying this tool uses the", tags$a(href="https://rdrr.io/cran/RecordLinkage/", "RecordLinkage"), "package, created by Murat Sariyar and Andreas Borg"),
             p("The code underlying this application is available on", tags$a(href="https://github.com/kaitlynhair/ASySD", "GitHub")),
             p("If you want to use this application for your systematic review, please cite:                          ",
               em("Hair K, Bahor Z, Macleod M, Liao J, Sena ES. The Automated Systematic Search Deduplicator (ASySD):
             a rapid, open-source, interoperable tool to remove duplicate citations in biomedical systematic reviews. bioRxiv; 2021. DOI: 10.1101/2021.05.04.442412."))

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

      }  else if(input$fileType == "CSV"){

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

      shinyWidgets::prettyRadioButtons(
        inputId = "Keep_citation_picker",
        label = "If duplicates exist, which citation should be preferentially retained?",
        choices = c("Citation with abstract", "Citation with a specific label", "Citation from a specific source"),
        selected = c("Citation with abstract"),
        status="primary")
    })

  })

  # Get choices for keep label
  output$keepLabel <- renderUI({

    shiny::validate(
      shiny::need(input$Keep_citation_picker != "", "")
    )


    if (input$Keep_citation_picker == "Citation with a specific label") {


      selectInput(inputId = "keepLabel",
                  label = "Specify labelled references to keep in the dataset",
                  choices = unique(RefData()$label),
                  selected = RefData()$label[1],
                  multiple = FALSE)
    }
  })


  # Get choices for keep label
  output$keepSource <- renderUI({

    shiny::validate(
      shiny::need(input$Keep_citation_picker != "", "")
    )


    if (input$Keep_citation_picker == "Citation from a specific source") {


      selectInput(inputId = "keepSource",
                  label = "Specify source to keep in the dataset",
                  choices = unique(RefData()$source),
                  selected = RefData()$source[1],
                  multiple = FALSE)
    }
  })


  # Datatable of input data ---
  output$citation_table <- renderDT({

    preview_10 <- RefData()[c(1:10),]

    preview_10 <- preview_10 %>%
      dplyr::select(record_id, author, title, year, journal, abstract, doi, number, pages, volume, isbn, label, source)

    DT::datatable(preview_10,
                  options = list(dom = 't',
                                 scrollX = TRUE,
                                 fixedColumns = TRUE,
                                 columnDefs = list(list(
                                   targets = "_all",
                                   render = JS(
                                     "function(data, type, row, meta) {",
                                     "return type === 'display' && data != null && data.length > 20 ?",
                                     "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                                     "}")
                                 ))),

                  rownames = FALSE,
                  class = "display")
  })

  # ASySD user text ----
  output$Post_upload_text <- renderText({

    original_refs_n <- as.numeric(nrow(RefData()))

    paste("You have uploaded", original_refs_n, "citations. Preview the first 10 below.")

  })

  output$ASySD_pre_upload <- renderUI({

    shiny::validate(
      shiny::need(input$fileType != "", "")
    )

    if (input$fileType == "Endnote XML") {

      str1 <- paste("Formatting requirements:")
      str2 <- paste("From Endnote, select references and export to an XML file")

      HTML(paste("<b>", str1, "</b>", "<br>", str2, "<br>"))

    }

    else if (input$fileType == "CSV") {

      str1 <- paste("Formatting requirements:")
      str2 <- paste("Within excel or another program, ensure your data has the following columns:
      author, year, journal, doi, title, pages, volume, number,
      abstract, record_id, isbn, label. The column order does not matter. It also does not matter if some
      columns are blank.")

      HTML(paste("<b>", str1, "</b>", "<br>", str2, "<br>"))
    }
    else if (input$fileType == "Tab delimited") {

      str1 <- paste("Formatting requirements:")
      str2 <- paste("From Endnote, select references and export to an tab delimited (.txt) file")

      HTML(paste("<b>", str1, "</b>", "<br>", str2, "<br>"))
    }
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
                              keep_source = input$keepSource,
                              keep_label = input$keepLabel,
                              merge_citations = TRUE)
    return(result)
  })


  # Output: ASySD auto dedup results text ----
  output$ASySD_results <- renderText({

    uniquerefs_n <- as.numeric(length(auto_dedup_result()$unique$duplicate_id))
    original_refs_n <- as.numeric(nrow(RefData()))
    removed_n <- original_refs_n - uniquerefs_n
    manual_pairs_n <- as.numeric(length(auto_dedup_result()$manual$record_id1))

    paste("<font color=\"#565656\"><b>", "From a total of", original_refs_n, "citations, ASySD has removed ", "<font color=\"#FF0000\"><b>", removed_n,  "<font color=\"#565656\">", "duplicates.",
          "There are", uniquerefs_n, "remaining.", manual_pairs_n, "possible duplicate pairs have been flagged for manual deduplication. Go to the
          manual deduplication tab to check these suggested duplicates.")

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
    datatable(auto_dedup_result()$manual[,c(1,2,4,5,10,11,13,14,16,17,19,20,22,23,28,29,31,32)],
              rownames = FALSE,
              options = list(pageLength = 20,
                             fixedColumns = TRUE,
                             scrollX = TRUE,
                             columnDefs = list(list(visible=FALSE, targets=c(16,17)),
                                               list(
                                                 targets = c(2,3),
                                                 render = JS(
                                                   "function(data, type, row, meta) {",
                                                   "return type === 'display' && data != null && data.length > 30 ?",
                                                   "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                                   "}")
                                               ),
                                               list(
                                                 targets = c(1,2,13,14),
                                                 render = JS(
                                                   "function(data, type, row, meta) {",
                                                   "return type === 'display' && data != null && data.length > 20 ?",
                                                   "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                                                   "}")
                                               ))))
  )



  # Output: sankey diagram ---
  output$sankey <- renderSankeyNetwork({

    n_search <- original_refs_n <- as.numeric(nrow(RefData()))
    n_unique_auto <- as.numeric(length(auto_dedup_result()$unique$duplicate_id))
    try(n_unique_manual <- as.numeric(length(manual_dedup_result()$duplicate_id)), silent=TRUE)

    if(exists("n_unique_manual")){

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
                        value = c(n_search, n_search - n_unique_auto, n_unique_manual, n_unique_auto - n_unique_manual))

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

    }

    else{


      links <- data.frame(source =
                            c(paste0("Original citations (", n_search, ")"),
                              paste0("Original citations (", n_search, ")")),
                            target =
                            c(paste0("Unique citations (", n_unique_auto, ")"),
                              paste0("Auto-dedup removed (", n_search - n_unique_auto, ")")),
                          value = c(n_unique_auto,  n_search - n_unique_auto))

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

    }
  })

  final_results <- reactive({

    if(exists("manual_dedup_result()")){

      final <- manual_dedup_result()

    }  else {

    final <- auto_dedup_result()$unique

    }

    return(final)
  })

  # output: download unique citations - endnote
  output$download<- downloadHandler(

        filename = function() {
          paste0("unique_citations.", input$export_type)
        },

        content = function(file) {

        ASySD::write_citations(final_results(), type = input$export_type, file)
        }
    )
}
# Run the application
shinyApp(ui = ui, server = server)



