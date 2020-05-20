source('~/Kaitlyn/Projects/RDedup/app/functions/dedup_labelled_step.R')

dedup_labelled_refs <- function (x,
                                 LabelKeep = ""){
  
  
  
  result1 <- dedup_labelled_step(x, 
                                 LabelKeep = LabelKeep)
  manual1 <- result1$ManualDedup
  unique1 <- result1$Unique
  pairs1 <- result1$TruePairs
  allmatches1 <- result1$PotentialPairs
  removed1 <- result1$DuplicateRefsRemoved
  
  unique1<-unique1 %>%
    select(RecordID, Author, Year, Title, Journal, Abstract, Volume, Number, Pages, DOI, Label, ISBN)
  
  manual_otherway <- manual1 %>%
    mutate(id1 = RecordID2) %>%
    mutate(id2= RecordID1) %>%
   rename(Title1 = Title2,
          Title2 = Title1, 
          Author1 = Author2,
          Author2 = Author1,
          Year1 = Year2,
          Year2 = Year1,
          Journal1 = Journal2,
          Journal2 = Journal1,
          Abstract1 = Abstract2,
          Abstract2 = Abstract1,
          Volume1 = Volume2,
          Volume2 = Volume1,
          Number1 = Number2,
          Number2 = Number1,
          Pages1 = Pages2,
          Pages2 = Pages1,
          DOI1 = DOI2,
          DOI2 = DOI1,
          Label1 = Label2,
          Label2 = Label1,
          ISBN1 = ISBN2,
          ISBN2 = ISBN1,
          RecordID1 = RecordID2,
          RecordID2 = RecordID1
          ) 
    
  
  result2 <- dedup_refs(result1$Unique)
  
  manual2id <- result2$ManualDedup 
  manual2 <-result2$ManualDedup  %>%
    mutate(id1 = RecordID1,
           id2 = RecordID2)
  
  manual2 <- anti_join(manual2, manual_otherway)
  
  unique2 <- result2$Unique
  pairs2 <- result2$TruePairs
  allmatches2 <- result2$PotentialPairs
  removed2 <- result2$DuplicateRefsRemoved
  
  unique2<-unique2 %>%
    select(RecordID, Author, Year, Title, Journal, Abstract, Volume, Number, Pages, DOI, Label, ISBN)
  
  manual <- rbind(manual1, manual2)
  manual <- manual %>%
    group_by(RecordID1, RecordID2) %>%
    mutate(id1 = first(id1)) %>%
    mutate(id2 = first(id2)) %>%
    ungroup()
  
  manual <- unique(manual)
  
  
  unique <- rbind(unique1, unique2)
  unique <- unique(unique)
  
  pairs <- rbind(pairs1, pairs2)
  pairs <- unique(pairs)
  
  allmatches <- rbind(allmatches1, allmatches2)
  allmatches<-unique(allmatches)
  
  removed <- rbind(removed1, removed2)
  
  return(list("ManualDedup" = manual,
              "Unique" = unique,
              "TruePairs" = pairs,
              "PotentialPairs" = allmatches,
              "DuplicateRefsRemoved" = removed))
}