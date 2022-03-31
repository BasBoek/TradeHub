merge_dfs <- function(in_folder, PATTERN, out_folder, out_file, ID_name, colname){
  
  out_path <- paste0(out_folder, "/", out_file)
  
  if(file.exists(out_path) == F){
    print("create df and write csv")
    
    loc_files <- list.files(in_folder, full.names = T, pattern=PATTERN)
    for(i in 1:length(loc_files)){
      
      print(i)
      new        <- read.csv(loc_files[i], stringsAsFactors = F)
      new        <- new[,c(ID_name, colname)]
      
      date       <- strsplit(loc_files[i], "/")[[1]][5]
      date       <- substr(date, 1, nchar(date)-4)
      names(new) <- c(ID_name, date)
      
      if(i == 1){
        df       <- new
      } else {
        df       <- merge(df, new, by=ID_name, all.x=T, all.y=T)
      }
    }
    df[is.na(df)] = 0
    df[is.null(df)] = 0
    
    # write and read csv
    write.csv(df, out_path, row.names=F)
    
  } else {
    print("load df by reading csv")
    
    df <- read.csv(out_path, stringsAsFactors = F)
  }
  return(df)
}
