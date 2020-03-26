dedup <-function(x){
  
  newdatformatted<- x %>%
    select(Author, Title, Year, Journal, Abstract, DOI, Number, Pages, Volume, RecordID, Label)
  
  ##Run compare.dedup function and block by Title&Pages OR Title&Author OR Title&Abstract OR Title&DOI
  newpairs = compare.dedup(x, blockfld = list(c(2,8), c(1,2), c(2,5), c(2,6)), strcmp = TRUE, exclude=c("RecordID", "Label"))
  
  #Create df of pairs
  dfpairs <- as.data.frame(newpairs$pairs)
  linkedpairs <- dfpairs
  
  #Run compare.dedup function and block by Author&Year&Pages OR Year&Pages&DOI 
  newpairs2 = compare.dedup(x, blockfld = list(c(1,3,8), c(3,8,6)), strcmp = TRUE, exclude= c("RecordID", "Label"))
  
  #Create df of pairs
  dfpairs2 <- as.data.frame(newpairs2$pairs)
  linkedpairs2 <- dfpairs2
  
  #Run compare.dedup function and block by Year&Pages&Volume OR Year&Issue&Volume
  newpairs3 = compare.dedup(x, blockfld = list(c(3,8,9), c(3,7,9)), strcmp = TRUE, exclude=c("RecordID", "Label"))
  
  #Create df of pairs
  dfpairs3 <- as.data.frame(newpairs3$pairs)
  linkedpairs3 <- dfpairs3
  
  #add title+volume, title+journal 29/03/19,
  newpairs4 = compare.dedup(x, blockfld = list(c(1,3), c(3,2), c(2,9), c(2,4)), strcmp = TRUE, exclude=c("RecordID", "Label"))
  
  dfpairs4 <- as.data.frame(newpairs4$pairs)
  linkedpairs4 <- dfpairs4 
  
  SeePairs <- rbind(linkedpairs, linkedpairs2, linkedpairs3, linkedpairs4)
  
  SeePairs <- unique(SeePairs)
  
  SeePairs <- SeePairs  %>%
    mutate(Author1 = RefData()$Author[id1]) %>%
    mutate(Author2 = RefData()$Author[id2]) 
  
  SeePairs <- SeePairs %>%
    mutate(Title1 =RefData()$Title[id1]) %>%
    mutate(Title2 = RefData()$Title[id2]) %>%
    mutate(Abstract1 = RefData()$Abstract[id1]) %>%
    mutate(Abstract2 = RefData()$Abstract[id2]) %>%
    mutate(DOI1 =RefData()$DOI[id1]) %>%
    mutate(DOI2 =RefData()$DOI[id2])
  
  SeePairs <- SeePairs  %>%
    mutate(Year1= RefData()$Year[id1]) %>%
    mutate(Year2= RefData()$Year[id2]) %>%
    mutate(Number1 = RefData()$Number[id1]) %>%
    mutate(Number2 = RefData()$Number[id2]) %>%
    mutate(Pages1 = RefData()$Pages[id1]) %>%
    mutate(Pages2 = RefData()$Pages[id2]) %>%
    mutate(Volume1 = RefData()$Volume[id1]) %>%
    mutate(Volume2 = RefData()$Volume[id2]) 
  
  SeePairs <- SeePairs  %>%
    mutate(Journal1 = RefData()$Journal[id1]) %>%
    mutate(Journal2 = RefData()$Journal[id2]) %>%
    mutate(RecordID1= RefData()$RecordID[id1]) %>%
    mutate(RecordID2 = RefData()$RecordID[id2]) %>%
    mutate(Label1 = RefData()$Label[id1]) %>%
    mutate(Label2 = RefData()$Label[id2])
  
  SeePairs <- SeePairs %>%
    select(id1, id2, Author1, Author2, Author, Title1, Title2, Title, Abstract1, Abstract2, Abstract, Year1, Year2, Year, Number1, Number2, Number, Pages1, Pages2, Pages, Volume1, Volume2, Volume, Journal1, Journal2, Journal, DOI1, DOI2, DOI, RecordID1, RecordID2, Label1, Label2)
  
  SeePairs <- SeePairs %>%
    mutate(Abstract = ifelse(is.na(Abstract1) & is.na(Abstract2), 0, Abstract)) %>%
    mutate(Pages = ifelse(is.na(Pages1) & is.na(Pages2), 1, Pages)) %>%
    mutate(Volume = ifelse(is.na(Volume1) & is.na(Volume2), 1, Volume)) %>%
    mutate(Number = ifelse(is.na(Number1) & is.na(Number2), 1, Number)) %>%
    mutate(DOI = ifelse(is.na(DOI1) & is.na(DOI2), 0, DOI)) 
  
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
  
  #Added part 18/04/19 due to issues with 2 part papers or those with very similar titles with same journal/author/year being considered duplicates (more of an issue for older studies)
  
  SeePairsFilteredDOIBAD <- SeePairsFiltered %>%
    filter(!(is.na(DOI)| DOI > 0.99 | DOI ==0 | DOI < 0.99 & Pages>0.9 & Abstract>0.9))
  
  SeePairsFiltered<- SeePairsFiltered %>%
    filter(is.na(DOI)| DOI > 0.99 | DOI ==0 | DOI < 0.99 & Pages>0.9 & Abstract>0.9)
  
  SeePairsFiltered <- unique(SeePairsFiltered)
  
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
  
  dedupdat <- RefData()
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
  
  linkedpairskeep1 <- SeePairsToDedup[which(is.na(SeePairsToDedup$Abstract2) & !is.na(SeePairsToDedup$Abstract1)),]
  removerefs1 <- unique(linkedpairskeep1$RecordID2)
  SeePairsToDedup <- SeePairsToDedup[which(!rownames(SeePairsToDedup) %in% rownames(linkedpairskeep1)),]
  dedupdat <- dedupdat[which(!dedupdat$RecordID %in% removerefs1),]
  
  #Keep 2 for all other references 
  removedalreadyID <- c(removerefs2, removerefs3, removerefs1)
  removedalreadyID <- unique(removedalreadyID)
  removedalready <- c(rownames(linkedpairskeepyear2), rownames(linkedpairskeepyear1), rownames(linkedpairskeep1))
  
  linkedpairskeep2 <- SeePairsToDedup[which(!rownames(SeePairsToDedup) %in% removedalready),]
  removerefs4 <- unique(linkedpairskeep2$RecordID1)
  SeePairsToDedup <- SeePairsToDedup[which(!rownames(SeePairsToDedup) %in% rownames(linkedpairskeep2)),]
  dedupdat <- dedupdat[which(!dedupdat$RecordID %in% removerefs4),]
  
  dedupdat <- unique(dedupdat)
  
  checkremovedalreadyID <- c(removerefs1, removerefs2, removerefs3, removerefs4)
  checkremovedalreadyID <-unique(checkremovedalreadyID)
  
  dedupdat<-unique(dedupdat)
  
  removedat <- RefData()
  
  removedat <- RefData()[which(removedat$RecordID %in% checkremovedalreadyID),]
  
  #Check if same
  removedat <- removedat[which(!removedat$RecordID %in% dedupdat$RecordID),]
  
  IDsMatched <- c(SeePairsFiltered$RecordID1, SeePairsFiltered$RecordID2)
  
  MaybePairs <- SeePairs %>%
    filter(!RecordID1 %in% IDsMatched) %>%
    filter(!RecordID2 %in% IDsMatched) %>%
    filter(Title>0.80 & Author>0.75)
  
  MaybePairs <- rbind(MaybePairs, ManualDedup)
  MaybePairs <- unique(MaybePairs)   
  
  return(list("ManualDedup" = MaybePairs,
              "Unique" = dedupdat,
              "TruePairs" = SeePairsFiltered,
              "PotentialPairs" = SeePairs))
  
}
