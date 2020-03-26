format_for_dedup <- function(x,
                              Author = "Author",
                              Title = "Title",
                              Year = "Year",
                              Journal = "Journal",
                              Abstract = "Abstract",
                              DOI = "DOI",
                              Number = "Number",
                              Pages = "Pages",
                              Volume = "Volume",
                              RecordID = "RecordID",
                              Label = "Label")
                             {
  
  x <- x %>%
   rename("Author" = Author,
          "Title" = Title,
          "Year" = Year,
          "Journal" = Journal,
          "Abstract" = Abstract,
          "DOI" = DOI,
          "Number" = Number,
          "Pages" = Pages,
          "Volume" = Volume,
          "RecordID" = RecordID,
          "Label" = Label)

  #Ensure columns are in this order
  newdatformatted <- x %>%
    select(Author, Title, Year, Journal, Abstract, DOI, Number, Pages, Volume, RecordID, Label) 
  
  newdatformatted$Author <- as.character(newdatformatted$Author)
  
  newdatformatted <- newdatformatted %>%
    mutate(Author = ifelse(Author=="", "Unknown", Author)) %>%
    mutate(Author = ifelse(is.na(Author), "Unknown", Author)) %>%
    mutate(Author = ifelse(Author=="Anonymous", "Unknown", Author))
  
  #Make all upper case
  newdatformatted <- as.data.frame(sapply(newdatformatted, toupper))
  
  ##Get rid of punctuation and DOI Differences
  toRemove <- c("HTTP://DX.DOI.ORG/", "DOI:", "DOI", "DOI: ")
  for (tR in toRemove) {
    newdatformatted$DOI <- gsub(tR, "", newdatformatted$DOI)
  }
  newdatformatted["Title"] <- as.data.frame(sapply(newdatformatted["Title"], function(x) gsub("[[:punct:]]", "", x)))
  newdatformatted["Year"] <- as.data.frame(sapply(newdatformatted["Year"], function(x) gsub("[[:punct:]]", "", x)))
  newdatformatted["Abstract"] <- as.data.frame(sapply(newdatformatted["Abstract"], function(x) gsub("[[:punct:]]", "", x)))
  
  newdatformatted <- unique(newdatformatted)
  
  newdatformatted<-newdatformatted %>%
    filter(!is.na(RecordID))

  newdatformatted <- newdatformatted %>%
    mutate(Author = ifelse(Author=="NA", NA, paste(Author))) %>%
    mutate(Year = ifelse(Year=="NA", NA, paste(Year))) %>%
    mutate(Title = ifelse(Title=="NA", NA, paste(Title))) %>%
    mutate(Number = ifelse(Number=="NA", NA, paste(Number))) %>%
    mutate(Volume = ifelse(Volume=="NA", NA, paste(Volume))) %>%
    mutate(Pages = ifelse(Pages=="NA", NA, paste(Pages))) %>%
    mutate(Abstract = ifelse(Abstract=="NA", NA, paste(Abstract))) %>%
    mutate(DOI = ifelse(DOI=="NA", NA, paste(DOI))) %>%
    mutate(Journal = ifelse(Journal=="NA", NA, paste(Journal)))
  
  return(newdatformatted)

  }