aggregator <- function(data, VOIs, GOIs, ALLFUNC='mean', all=F){
  VAR_list <- GRP_list <- list()
  data[sapply(data, is.factor)] <- lapply(data[sapply(data, is.factor)], as.character)
  if(all == T & length(GOIs) == 1){ warning("Ignoring 'all=T'. Only 1 group given") }
  
  if(length(ALLFUNC) == 2 & ALLFUNC[1] == 'quantile'){ALLFUNC <- list(ALLFUNC)}
  for(FUNC_index in 1:length(ALLFUNC)){
    FUNC <- ALLFUNC[[FUNC_index]][1]
    for(i in 1:length(GOIs)){  GRP_list[[i]] <- data[[GOIs[i]]]}
    for(i in 1:length(VOIs)){  VAR_list[[i]] <- data[[VOIs[i]]]}
    GRP_list <- GRP_list
    if(FUNC != 'count'){
      if(FUNC != "quantile" ){newdata <- aggregate(VAR_list, by=GRP_list, FUN=FUNC, na.rm=T)} else {newdata <- aggregate(VAR_list, by=GRP_list, FUN=FUNC, na.rm=T, probs=as.numeric(ALLFUNC[[FUNC_index]][2])/100)}
    } else {
      newsum  <- aggregate(VAR_list, by=GRP_list, FUN='sum', na.rm=T)
      newavg  <- aggregate(VAR_list, by=GRP_list, FUN='mean', na.rm=T)
      newdata <- as.data.frame(cbind(newsum[,1:(ncol(newsum)-length(VOIs))],(newsum[,(ncol(newsum)-length(VOIs)+1):ncol(newsum)])/(newavg[,(ncol(newavg)-length(VOIs)+1):ncol(newavg)])), stringsAsFactors = F)
    }
    FUNC_final       <- paste(as.character(ALLFUNC[[FUNC_index]]), collapse='')
    newdata$Function <- FUNC_final
    newdata          <- newdata[,c(which(colnames(newdata)=="Function"), which(colnames(newdata)!="Function"))]
    names(newdata)   <- c("Function", GOIs, VOIs)

    if(FUNC_final == 'sd'){
      NA_data <- newdata[is.na(newdata[length(newdata)]),]
      if(nrow(NA_data) > 0){ warning(paste0("Only 1 data entry present for ", nrow(NA_data), " group combinations. Calculation of sd not possible: Returning NA"))}
    }
    
    if(all == T & length(GOIs) > 1){ # Add lines for all combinations when preferred
      existing_combs    <- nrow(newdata)
      combs             <- list()
      for(i in 1:length(GOIs)){  combs[[i]] <- unique(newdata[,GOIs[i]]) }
      ALLCOMB           <- expand.grid(combs)
      names(ALLCOMB)    <- GOIs
      
      if(FUNC_final == 'count'){ALLCOMB[(length(GOIs) + 1):(length(GOIs)+length(VOIs))] <-  0
      } else {ALLCOMB[(length(GOIs) + 1):(length(GOIs)+length(VOIs))] <- NA}
      ALLCOMB$Function  <- FUNC_final
      ALLCOMB           <- ALLCOMB[, c(which(colnames(ALLCOMB)=="Function"), which(colnames(ALLCOMB)!="Function"))]
      colnames(ALLCOMB) <- c("Function", GOIs,VOIs)
      newdata           <- rbind(newdata, ALLCOMB)
      newdata$doubles   <- duplicated(newdata[,GOIs])
      newdata           <- newdata[newdata$doubles == F,]
      newdata$doubles   <- NULL
      added_combs       <- nrow(newdata) - existing_combs
      if(FUNC_index == 1){print(paste0(added_combs, " of the ", nrow(newdata), " possible unique group combinations were not present"))}
    } 
    if(FUNC_index == 1){      allnewdata <- newdata    } else { allnewdata <- rbind(allnewdata, newdata)}
  }
  rownames(allnewdata) <- c()
  return(allnewdata)}

sort_df <- function(data, colnames){
  for(i in length(colnames):1){
    if(i == length(colnames)){
      new_df <- data[,c(which(colnames(data)==colnames[i]), which(colnames(data)!=colnames[i]))]
    } else {      new_df <- new_df[,c(which(colnames(new_df)==colnames[i]), which(colnames(new_df)!=colnames[i]))]    }}
  return(new_df)
}
