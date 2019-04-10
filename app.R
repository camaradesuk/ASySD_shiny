#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Load packages
require(shiny)
require(readr)
require(DT)
require(stringr)
require(dplyr)
require(XML)
require(RCurl)
require(shinythemes)
require(RecordLinkage) #contains compare.dedup function
options(shiny.maxRequestSize=300*1024^2)


# App title ----

ui <- navbarPage("Deduplicator",
  
  theme = shinytheme("flatly"),
  
  tabPanel("Uploading Files",
# Sidebar layout with input and output definitions ----
    sidebarLayout(
     sidebarPanel(
    
    # Input: Select a file ----
    fileInput("file1", "Choose an XML file",
              multiple = FALSE)),
  
    mainPanel(
      
      # Output: Data file ----
      DTOutput("contents")))),

conditionalPanel("$('#contents').hasClass('recalculating')", 
                 tags$div('Loading table ... ')),

tabPanel("Deduplicate Data",
  sidebarLayout(
    sidebarPanel(
                 
  # Button
  actionButton("dedupbutton", "Deduplicate"),
  downloadButton("downloadData", "Download")
  ),

# Main panel for displaying outputs ----
  mainPanel(
    
    # Output: Data file ----
    DTOutput("dedup")))),



conditionalPanel("$('#dedup').hasClass('recalculating')", 
                 tags$div('Loading table ... '))
)

server <- function(input, output) {
  
  XMLData <- reactive({ 
    
    req(input$file1)
    
    library(XML)
    library(RCurl)
    
    xmldat<-xmlParse(input$file1$datapath)
    x <-  getNodeSet(xmldat,'//record')
    
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
      SecondaryTitle = sapply(x, xpath2, ".//titles/secondary-title", xmlValue)
    )
      
    return(newdat)
    
  })
    
  XMLDataFormatted <- reactive({ 

    newdatformatted <- XMLData()
    
    newdatformatted <- newdatformatted %>%
      mutate(Journal=ifelse(is.na(Journal), paste(SecondaryTitle), paste(Journal))) %>%
      select(Author, Title, Year, Journal, Abstract, DOI, Number, Pages, Volume, RecordID)

    newdatformatted$Author <- as.character(newdatformatted$Author)

    newdatformatted <- newdatformatted %>%
      mutate(Author = ifelse(Author=="", "Unknown", Author)) %>%
      mutate(Author = ifelse(is.na(Author), "Unknown", Author)) %>%
      mutate(Author = ifelse(Author=="Anonymous", "Unknown", Author))

    #Make all upper case
    newdatformatted <- as.data.frame(sapply(newdatformatted, toupper))

    ##Get rid of punctuation and DOI Differences - not in original, edit
    toRemove <- c("HTTP://DX.DOI.ORG/", "DOI:", "DOI", "DOI: ")
    for (tR in toRemove) {
      newdatformatted$DOI <- gsub(tR, "", newdatformatted$DOI)
    }

  newdatformatted["Title"] <- as.data.frame(sapply(newdatformatted["Title"], function(x) gsub("[[:punct:]]", "", x)))
  newdatformatted["Year"] <- as.data.frame(sapply(newdatformatted["Year"], function(x) gsub("[[:punct:]]", "", x)))

    newdatformatted <- unique(newdatformatted)

    newdatformatted<-newdatformatted %>%
      filter(!is.na(RecordID))
  
return(newdatformatted)
  
})

output$contents <- renderDT(
  XMLData(), 
  options = list(pageLength = 10)
  )

  
observeEvent(input$dedupbutton, {
  print(paste("Deduplicating..."))
})

dedupData <- eventReactive(input$dedupbutton,{ 

  ##Run compare.dedup function and block by Title&Pages OR Title&Author OR Title&Abstract OR Title&DOI
  newpairs = compare.dedup(XMLDataFormatted(), blockfld = list(c(2,8), c(1,2), c(2,5), c(2,6)), strcmp = TRUE, exclude= "RecordID")
  #Create df of pairs
  linkedpairs <- as.data.frame(newpairs$pairs)

  #Run compare.dedup function and block by Author&Year&Pages OR Year&Pages&DOI 
  newpairs2 = compare.dedup(XMLDataFormatted(), blockfld = list(c(1,3,8), c(3,8,6)), strcmp = TRUE, exclude= c("RecordID"))
  #Create df of pairs
  linkedpairs2 <- as.data.frame(newpairs2$pairs)


  #Run compare.dedup function and block by Year&Pages&Volume OR Year&Issue&Volume
  newpairs3 = compare.dedup(XMLDataFormatted(), blockfld = list(c(3,8,9), c(3,7,9)), strcmp = TRUE, exclude= "RecordID")
  #Create df of pairs
  linkedpairs3 <- as.data.frame(newpairs3$pairs)

  #Run compare.dedup function and block by Author&Year OR Title&Year
  newpairs4 = compare.dedup(XMLDataFormatted(), blockfld = list(c(1,3), c(3,2)), strcmp = TRUE, exclude= "RecordID")
  #Create df of pairs
  linkedpairs4 <- as.data.frame(newpairs4$pairs)
  
  SeePairs <- rbind(linkedpairs, linkedpairs2, linkedpairs3, linkedpairs4)

  SeePairs <- SeePairs  %>%
    mutate(Author1 = XMLData()$Author[id1]) %>%
    mutate(Author2 = XMLData()$Author[id2])

  SeePairs <- SeePairs %>%
    mutate(Title1 =XMLData()$Title[id1]) %>%
    mutate(Title2 = XMLData()$Title[id2]) %>%
    mutate(Abstract1 = XMLData()$Abstract[id1]) %>%
    mutate(Abstract2 = XMLData()$Abstract[id2]) %>%
    mutate(DOI1 =XMLData()$DOI[id1]) %>%
    mutate(DOI2 =XMLData()$DOI[id2])

  SeePairs <- SeePairs  %>%
    mutate(Year1= XMLData()$Year[id1]) %>%
    mutate(Year2= XMLData()$Year[id2]) %>%
    mutate(Number1 = XMLData()$Number[id1]) %>%
    mutate(Number2 = XMLData()$Number[id2]) %>%
    mutate(Pages1 = XMLData()$Pages[id1]) %>%
    mutate(Pages2 = XMLData()$Pages[id2]) %>%
    mutate(Volume1 = XMLData()$Volume[id1]) %>%
    mutate(Volume2 = XMLData()$Volume[id2])

  SeePairs <- SeePairs  %>%
    mutate(Journal1 = XMLData()$Journal[id1]) %>%
    mutate(Journal2 = XMLData()$Journal[id2]) %>%
    mutate(RecordID1= XMLData()$RecordID[id1]) %>%
    mutate(RecordID2 = XMLData()$RecordID[id2])

  SeePairs <- SeePairs %>%
    select(id1, id2, Author1, Author2, Author, Title1, Title2, Title, Abstract1, Abstract2, Abstract, Year1, Year2, Year, Number1, Number2, Number, Pages1, Pages2, Pages, Volume1, Volume2, Volume, Journal1, Journal2, Journal, DOI1, DOI2, DOI, RecordID1, RecordID2)

  SeePairs <- SeePairs %>%
    mutate(Abstract = ifelse(is.na(Abstract1) & is.na(Abstract2), 0, Abstract)) %>%
    mutate(Pages = ifelse(is.na(Pages1) & is.na(Pages2), 1, Pages)) %>%
    mutate(Volume = ifelse(is.na(Volume1) & is.na(Volume2), 1, Volume)) %>%
    mutate(Number = ifelse(is.na(Number1) & is.na(Number2), 1, Number)) %>%
    mutate(DOI = ifelse(is.na(DOI1) & is.na(DOI2), 0, DOI)) %>%
    mutate(Abstract = ifelse(Abstract1=="" & Abstract2=="", 0, Abstract)) %>%
    mutate(Pages = ifelse(Pages1=="" & Pages2=="", 1, Pages)) %>%
    mutate(Volume = ifelse(Volume1=="" & Volume2=="", 1, Volume)) %>%
    mutate(Number = ifelse(Number1=="" & Number2=="", 1, Number)) %>%
    mutate(DOI = ifelse(DOI1=="" & DOI2=="", 0, DOI))


  SeePairsFiltered <- SeePairs %>%
    filter(
      (Pages>0.8 & Volume>0.8 & Title>0.90 & Abstract>0.90 & Author>0.50 & Journal>0.6) |
        (Pages>0.8 & Number>0.8 & Title>0.90 & Abstract>0.90 & Author>0.50 & Journal>0.6) |
        (Volume >0.8 & Number>0.8 & Title>0.90 & Abstract>0.90 & Author>0.50  & Journal>0.6) |

        (Volume >0.8 & Number>0.8 & Title>0.90 & Abstract>0.90 & Author>0.8) |
        (Volume>0.8 & Pages>0.8 & Title>0.90 & Abstract>0.9 & Author>0.8) |
        (Pages>0.8 & Number>0.8 & Title>0.90 & Abstract>0.9 & Author>0.8) |

        (DOI>0.95 & Author>0.75 & Title>0.9 & Volume >0.8) |

        (Title>0.80 & Abstract>0.90 & Volume>0.85 & Journal>0.65 & Author>0.9) |
        (Title>0.90 & Abstract>0.80 & Volume>0.85 & Journal>0.65 & Author>0.9)|

        (Pages>0.8 & Volume>0.8 & Title>0.90 & Abstract>0.8 & Author>0.9 & Journal>0.75) |
        (Pages>0.8 & Number>0.8 & Title>0.90 & Abstract>0.80 & Author>0.9 & Journal>0.75) |
        (Volume>0.8 & Number>0.8 & Title>0.90 & Abstract>0.8 & Author>0.9  & Journal>0.75) |

        (Title>0.9 & Author>0.9 & Abstract>0.9 & Journal >0.7)|

        (Pages>0.9 & Number>0.9 & Title>0.90 & Author>0.80 & Journal>0.6) |
        (Number>0.9 & Volume>0.9 & Title>0.90 & Author>0.90 & Journal>0.6) |
        (Pages>0.9 & Volume>0.9 & Title>0.90 & Author>0.80 & Journal>0.6) |

        (Pages>0.8 & Volume>0.8 & Title>0.90 & Author>0.80 & Journal>0.9) |
        (Number>0.8 & Volume>0.8 & Title>0.90 & Author>0.80 & Journal>0.9)|
        (Number>0.8 & Pages>0.8 & Title>0.90 & Author>0.80 & Journal>0.9))


  SeePairs <- unique(SeePairs)

  SeePairsFiltered <- unique(SeePairsFiltered)

  #Find papers with different years
  SeePairsFiltered$Year1 <- as.numeric(as.character(SeePairsFiltered$Year1))
  SeePairsFiltered$Year2 <- as.numeric(as.character(SeePairsFiltered$Year2))

  YearsDiff <- SeePairsFiltered[which(SeePairsFiltered$Year1 != SeePairsFiltered$Year2),]
  YearsNotVeryDiff1 <- YearsDiff[which(YearsDiff$Year1 == YearsDiff$Year2+1 ),]
  YearsNotVeryDiff2 <- YearsDiff[which(YearsDiff$Year1 == YearsDiff$Year2-1 ),]

  YearsNotVeryDiff <- rbind(YearsNotVeryDiff1, YearsNotVeryDiff2)
  YearsNotVeryDiff <- unique(YearsNotVeryDiff)

  YearsVeryDiff <- YearsDiff[which(!rownames(YearsDiff) %in% rownames(YearsNotVeryDiff)),]

  ManualDedup <- YearsVeryDiff

  SeePairsFiltered <- SeePairsFiltered[which(!rownames(SeePairsFiltered) %in% rownames(YearsVeryDiff)),]

  SeePairsFiltered <- unique(SeePairsFiltered)

  SeePairsFiltered$RecordID1 <- as.character(SeePairsFiltered$RecordID1)
  SeePairsFiltered$RecordID2 <- as.character(SeePairsFiltered$RecordID2)

  set.seed(123)
  SeePairsFilteredCheckSample <- SeePairsFiltered[sample(500),]

  dedupdat <- XMLData()
  dedupdat$RecordID <- as.character(dedupdat$RecordID)

  SeePairsToDedup <- SeePairsFiltered

  #Keep latest year
  linkedpairskeepyear1 <- SeePairsToDedup[which(as.numeric(SeePairsToDedup$Year1) > as.numeric(SeePairsToDedup$Year2)),]
  removerefs2 <- unique(linkedpairskeepyear1$RecordID2)

  SeePairsToDedup <- SeePairsToDedup[which(!rownames(SeePairsToDedup) %in% rownames(linkedpairskeepyear1)),]
  dedupdat <- dedupdat[which(!dedupdat$RecordID %in% removerefs2),]
  
  linkedpairskeepyear2 <- SeePairsToDedup[which(as.numeric(SeePairsToDedup$Year2) > as.numeric(SeePairsToDedup$Year1)),]
  removerefs3 <- unique(linkedpairskeepyear2$RecordID1)
  removerefs3unique <- removerefs3[!removerefs3 %in% removerefs2]

  SeePairsToDedup <- SeePairsToDedup[which(!rownames(SeePairsToDedup) %in% rownames(linkedpairskeepyear2)),]
  dedupdat <- dedupdat[which(!dedupdat$RecordID %in% removerefs3),]

  #Keep 2 for all other references
  removedalreadyID <- c(removerefs2, removerefs3)
  removedalreadyID <- unique(removedalreadyID)
  removedalready <- c(rownames(linkedpairskeepyear2), rownames(linkedpairskeepyear1))

  linkedpairskeep2 <- SeePairsToDedup[which(!rownames(SeePairsToDedup) %in% removedalready),]
  removerefs4 <- unique(linkedpairskeep2$RecordID1)
  SeePairsToDedup <- SeePairsToDedup[which(!rownames(SeePairsToDedup) %in% rownames(linkedpairskeep2)),]
  dedupdat <- dedupdat[which(!dedupdat$RecordID %in% removerefs4),]
  return(dedupdat)
  
})

 output$dedup <- renderDT(
    
    DT::datatable(dedupData(), 
      options = list(pageLength = 10))
    )
  
  # Downloadable csv of selected dataset ----
 output$downloadData <- downloadHandler(
    filename = function() {
      paste("dedupdata", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dedupData(), file, row.names = FALSE)
    })
}

# Run the application 
ShinyDedup <- shinyApp(ui = ui, server = server)



