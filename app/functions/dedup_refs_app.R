# Deduplicate references 

dedup_refs_app <-function(x,
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
                      Label = "Label",
                      LabelKeep = "De-duplicate as normal"){
  
  
  # Rename columns if necessary
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
  
  require(RecordLinkage)
  
  
  if(LabelKeep == "De-duplicate as normal"){
    
    dedup_refs(x)
 
      
    }
  
  else {
    
    dedup_labelled_refs(x, LabelKeep = LabelKeep)
        
      }
  
}
