# interpolator: calculates values in between. Needs refinement in order to make generic!
# agg_fun: aggregator for number of variables of interest. Automatically combines them
# Getting nth element of every list in the list

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Works if you have a recent version of RStudio.

create_rand_color <- function(df, colname, newcolname){
  temp <- unique(df[,colname])
  newcols <- rand_color(length(temp), hue = NULL, luminosity = "random", transparency = 0)
  new_df <- as.data.frame(cbind(temp, newcols), stringsAsFactors = F)
  colnames(new_df) <- c(colname, newcolname)
  result <- merge(df, new_df, by=colname)
}

data       <- graphdata
GOIs       <- "ON_cat"
VOIs       <- "NFRV_OxM0"
ALLFUNC    <- "count"
FUNC_index <- 1

# Aggregate multiple columns
aggregator <- function(data, VOIs, GOIs, ALLFUNC='mean', all=F){
  VAR_list <- GRP_list <- list()
  data[sapply(data, is.factor)] <- lapply(data[sapply(data, is.factor)], as.character)
  if(all == T & length(GOIs) == 1){ warning("Ignoring 'all=T'. Only 1 group given") }
  
  if(length(ALLFUNC) == 2 & ALLFUNC[1] == 'quantile'){ALLFUNC <- list(ALLFUNC)}
  for(FUNC_index in 1:length(ALLFUNC)){
    FUNC <<- ALLFUNC[[FUNC_index]][1]
    for(i in 1:length(GOIs)){  GRP_list[[i]] <- data[[GOIs[i]]]}
    for(i in 1:length(VOIs)){  VAR_list[[i]] <- data[[VOIs[i]]]}
    GRP_list <<- GRP_list
    if(FUNC != 'count'){
      if(FUNC != "quantile" ){newdata <- aggregate(VAR_list, by=GRP_list, FUN=FUNC, na.rm=T)} else {newdata <- aggregate(VAR_list, by=GRP_list, FUN=FUNC, na.rm=T, probs=as.numeric(ALLFUNC[[FUNC_index]][2])/100)}
    } else {
      newsum  <- aggregate(VAR_list, by=GRP_list, FUN='sum', na.rm=T)
      newavg  <- aggregate(VAR_list, by=GRP_list, FUN='mean', na.rm=T)
      newdata <- as.data.frame(cbind(newsum[,1:(ncol(newsum)-length(VOIs))],(newsum[,(ncol(newsum)-length(VOIs)+1):ncol(newsum)])/(newavg[,(ncol(newavg)-length(VOIs)+1):ncol(newavg)])), stringsAsFactors = F)
    }
    FUNC_final       <<- paste(as.character(ALLFUNC[[FUNC_index]]), collapse='')
    newdata$Function <- FUNC_final
    newdata          <- newdata[,c(which(colnames(newdata)=="Function"), which(colnames(newdata)!="Function"))]
    names(newdata)   <- c("Function", GOIs, VOIs)
    newdata          <<-  newdata # REMOVE WHEN DONE
    
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

# Define numerical and grouping variables
df      <- mtcars
df$cyl  <- as.factor(df$cyl)
df$vs   <- as.character(df$vs)
df$am   <- as.character(df$am)
df$gear <- as.character(df$gear)

VOIs    <- c("mpg", "qsec", "carb")    # Numerical variables of interest (VOIs)
GOIs    <- c("am","vs", "gear")      # Grouping (character / factor) variables of interest

# # Define numerical and grouping variables
# df     <- iris
# df$cyl <- as.factor(df$Petal.Width)
# 
# VOIs     <- c("Sepal.Length", "Sepal.Width", "Petal.Length")    # Numerical variables of interest (VOIs)
# GOIs     <- c("Species", "Petal.Width")      # Grouping (character / factor) variables of interest

# EXAMPLES: Create different kind of statistics from the dataset
stats1  <- 'sum'
stats2  <- c('count', 'sum', 'sd', 'min', 'median')
stats3  <- list('count', 'sum', 'sd', 'min', 'median')
stats4  <- c('quantile', 40)
stats5  <- list(c('quantile', 20), c('quantile', 90))
stats6  <- list('count', c('quantile', 15), 'sum', c('quantile', 35), c('quantile', 50), c('quantile', 90))
stats7  <- 'sd'
stats8  <- 'range'

newdata <- aggregator(df, c("disp", "wt"), c("cyl", "vs", "am"), c("count", "mean", "sum"))

df1     <- aggregator(df, VOIs, GOIs, stats1)
df2     <- aggregator(df, VOIs, GOIs, stats2, all=T)
df3     <- aggregator(df, VOIs, GOIs, stats3, all=T)
df4     <- aggregator(df, VOIs, GOIs, stats4, all=T)
df5     <- aggregator(df, VOIs, GOIs, stats5, all=T)
df6     <- aggregator(df, VOIs, GOIs, stats6, all=T)
df7     <- aggregator(df, VOIs, GOIs, stats7, all=T)
df8     <- aggregator(df, VOIs, GOIs, stats8, all=T)


# sort_df: Putting a series of columns in front of the table
sort_df <- function(data, colnames){
  for(i in length(colnames):1){
    if(i == length(colnames)){
      new_df <- data[,c(which(colnames(data)==colnames[i]), which(colnames(data)!=colnames[i]))]
    } else {      new_df <- new_df[,c(which(colnames(new_df)==colnames[i]), which(colnames(new_df)!=colnames[i]))]    }}
  return(new_df)
}

# For ggplot -> define breaks in boxplot
bp.vals <- function(x, probs=c(0.1, 0.25, 0.75, 0.9)) {
  r        <- quantile(x, probs=probs , na.rm=TRUE)
  r        <- c(r[1:2], exp(mean(log(x))), r[3:4])
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r}

# Function to calculate intersection points of a function.
calculate_MN <- function(int_99, cf1_99, cf2_99, target_yield){
  if(int_99 > 0 & cf1_99 < 0 & cf2_99 < 0){
    fun <- function (x) int_99 + cf1_99 * 0.99^x + cf2_99*x - target_yield 
    maximum <- optimize(fun, interval=c(0, 200), maximum=TRUE)$objective
    limit   <- optimize(fun, interval=c(0, 200), maximum=TRUE)$maximum
    if(maximum > 0){
      curve(fun(x), 0, 200)
      N_calc <- uniroot(fun, c(-100, limit))$root # Use limit in order to be sure that the lowest N value is picked
      if(N_calc < 0){N_calc <- NA}
    } else {    N_calc <- NA  }
  } else { N_calc <- NA}
  return(N_calc)
}
df$new_var1 <- 0.99^df$N # Step 1: Transform variables according to what function / formula you want to apply. E.g.:
df$new_var2 <- df$N^2
linear_model <- lm(Y ~ N + new_var1 + new_var2, data=df) # Step 2: Put transformed variables into a linear model, example:
summary(linear_model) # Step 3: Determine (in the above case 4) coefficients of the model / or another statements that will show the coefficients
fun <- function (x) AA + BB * x + CC * 0.99^x + DD * x^2 # Step 4: Use the coefficients to define the function your function below (now you do not need to use the transformed variables)
curve(fun(x), 0, 200) # Plot it with a range of choice

# Getting xth element of a list (and glue xth and yth element together)
temp                 <- strsplit(appl_fert$FARMER_REGION_TREATMENT, split="_")
appl_fert$FARMERNAME <- paste0(as.character(lapply(temp, "[[", 1)), "_", as.character(lapply(temp, "[[", 2)))


rm(list=ls())

# Add a character in between an existing string 
add_char <- function(data, after_which_char_replacement, char_to_add, remove_empty_originals)
for(i in 1:nrow(data)){
  LOC <- after_which_char_replacement
  code <- data$SOIL_SAMPLE_CODE[i]
  newcode <- paste0(substr(code,1,LOC), char_to_add, substr(code,LOC+1,nchar(code)) )
  if(remove_empty_originals == "y"){
    if(newcode == char_to_add){
      newcode <- ""
    }
    print(newcode)
  }
  data$SOIL_SAMPLE_CODE[i] <- newcode
}

# Write function that interpolates colours !!

begin_color <- "#00FF00"
floor(245/16)
(245/16 - floor(245/16)) * 16


# Restructuring data
library(reshape)
data <- mtcars
test <- melt(data, id=c("cyl", "mpg", "disp", "drat", "wt"))

# read data and remove redundant columns
data     <- read.csv("M:/My Documents/BASTIAEN/PROJECTS/Small projects/Joy/Interpolate_values/data/Nigeria_Exp.2017_LAI_calculations.csv")
data     <- data[, -which(names(data) %in% c("X","X.1"))]
data$MAP <- round(data$MAP / 12 * 365) # convert month to Julian day (rounded)
varnames <- names(data[4:12])


x_values           <- "MAP" # Column with the - yet - missing values
COLUMNNAME         <- "LAI" # variable of interest
VARCOMBI           <- 1

# Copy files from one to another location, leaving the data structure intact ------------

# identify the folders (FOI = Folder of interest, TF = target folder)
FOI    <- "M:/My Documents/BASTIAEN"
TF     <- "D:/M"
copy_files <- function(FOI = "M:/My Documents/BASTIAEN", TF = "D:/M"){
  # find the files that you want
  file_list          <- list.files(FOI, full.names=T, recursive = T)
  file_list_rel      <- substr(file_list, nchar(FOI)+2, nchar(file_list))
  file_waterfall     <- strsplit(file_list_rel, "/")
  
  lijst <- list()
  lijst_index <- 1
  for(LIST_EL in 1:length(file_waterfall)){
    folders_file <- unlist(file_waterfall[LIST_EL])
    n_elements <- length(folders_file)
    if(n_elements > 1){
      for(i in 1:n_elements){
        if(i == 1){
          subdir <- folders_file[i]
          dir.create(file.path(TF, subdir))
        } else {
          if(i < n_elements){
            subdir <- paste0(subdir, "/", folders_file[i]) 
            dir.create(file.path(TF, subdir))
          } else {
            new_fileloc <- paste0(TF, "/", subdir)
            old_fileloc <- paste0(FOI, "/", subdir, "/", folders_file[i])
            if(nchar(old_fileloc) < 256 & nchar(new_fileloc) < 256){
              file.copy(old_fileloc, new_fileloc, recursive = T, copy.date = T)
              print(folders_file[i])
            } else {
              cat(paste("TOO LONG: ", new_fileloc, "\n\n"))
              lijst[lijst_index] <- old_fileloc
              lijst_index <- lijst_index + 1
            }
          }
        }
      }
    } else {
      file.copy(paste0(FOI, "/", folders_file), paste0(TF), recursive = T, copy.date = T)
      print(folders_file)
    }
  }
  too_long <- unlist(lijst)
  cat("List of files with too long name to copy: \n\n")
  print(too_long)
}


# Create data entry values with equal nr of characters
equalNames <- function(Ycolname, Xcolname, dataset, maxChar){
  dataset[,which(colnames(dataset) == Ycolname)] = as.character(dataset[,which(colnames(dataset) == Ycolname)])
  dataset[,which(colnames(dataset) == Xcolname)] = as.character(dataset[,which(colnames(dataset) == Xcolname)])
  
  for(r in 1:nrow(dataset)){
    
    addtext = c()
    if(is.na(dataset[r,which(colnames(dataset)==Ycolname)]) == F) {
      if(nchar(dataset[r,which(colnames(dataset)==Ycolname)]) < maxChar){
        addtext = paste(replicate(maxChar - nchar(dataset[r,which(colnames(dataset)==Ycolname)]), "_"), collapse = "")
        dataset[r,which(colnames(dataset)==Ycolname)] = paste(dataset[r,which(colnames(dataset)==Ycolname)], addtext, sep = "")
      }
    }
    addtext = c()
    if(is.na(dataset[r,which(colnames(dataset)==Xcolname)]) == F) {
      if(nchar(dataset[r,which(colnames(dataset)==Xcolname)]) < maxChar){
        addtext = paste(replicate(maxChar - nchar(dataset[r,which(colnames(dataset)==Xcolname)]), "_"), collapse = "")
        dataset[r,which(colnames(dataset)==Xcolname)] = paste(dataset[r,which(colnames(dataset)==Xcolname)], addtext, sep = "")
      }
    }
  }
  return(dataset)
}
test <- equalNames(Ycolname="???", Xcolname="???", dataset=data, maxChar=50)

# Definition of what variables are grouping (1, 2 or 3)
# Define column that are x_values
x1 <- as.data.frame(sort(round(runif(10, 0, 100))))
x2 <- as.data.frame(round(runif(10, 0, 1000)))
data  <- cbind(x1, x2)
names(data) <- c("x", "y")
data$group <- "henk"
data$grooup2 <- 'piet'

interpolator <- function(COLUMNNAME, data, grouping_variables, x_values){
  i_main <- 0 ## Initialize iterator
  
  nr_var <- length(grouping_variables)
  if(nr_var < 1 | nr_var > 3){return(print("number of grouping variables is less than 1 or more than 3"))}
  if(nr_var == 1){combinations <- expand.grid(unique(data[,grouping_variables[1]]))}
  if(nr_var == 2){combinations <- expand.grid(unique(data[,grouping_variables[1]]), unique(data[,grouping_variables[2]]))}
  if(nr_var == 3){combinations <- expand.grid(unique(data[,grouping_variables[1]]), unique(data[,grouping_variables[2]]),unique(data[,grouping_variables[3]]))}

  # Create max_value+1 number of empty rows to be filled
  max_value                       <- max(data[,x_values])
  data_new_template               <- data[0,c(grouping_variables, x_values)]
  data_new_template[max_value+1,] <- NA
  
  for(VARCOMBI in 1:nrow(combinations)){
    
    i_main   <- i_main + 1
    
    # This for loop determines the values in between to sampling dates
    if(nr_var == 1){s <- data[data[,grouping_variables[1]] == combinations[VARCOMBI,1], c(COLUMNNAME, x_values)]}
    if(nr_var == 2){s <- data[data[,grouping_variables[1]] == combinations[VARCOMBI,1] & data[,grouping_variables[2]] == combinations[VARCOMBI,2], c(COLUMNNAME, x_values)]}
    if(nr_var == 3){s <- data[data[,grouping_variables[1]] == combinations[VARCOMBI,1] & data[,grouping_variables[2]] == combinations[VARCOMBI,2] & data[,grouping_variables[3]] == combinations[VARCOMBI,3], c(COLUMNNAME, x_values)]}

    s <- rbind(c(0,0), s)
    s <- s[order(s[,x_values]),]
    
    iterator <- nrow(s) - 1
    naming   <- names(s)
    
    
    for(i in 1:iterator){
      day_start       <- s[i,2]
      day_end         <- s[i+1,2]
      var_start       <- s[i,1]
      var_end         <- s[i+1,1]
      days            <- day_end - day_start
      var_diff        <- var_end - var_start
      step            <- var_diff / days
      missing_days    <- (day_start+1):(day_end-1)
      missing_vals    <- (missing_days - day_start) * step + var_start
      
      new_vals        <- as.data.frame(cbind(missing_vals, missing_days))
      names(new_vals) <- naming
      s               <- rbind(s, new_vals)
    }

    if(i_main == 1){
      data_new          <- data_new_template
      if(nr_var == 1){
        data_new[,1]    <- combinations[VARCOMBI,1]}
      if(nr_var == 2){
        data_new[,1]    <- combinations[VARCOMBI,1]
        data_new[,2]    <- combinations[VARCOMBI,2]}
      if(nr_var == 3){
        data_new[,1]    <- combinations[VARCOMBI,1]
        data_new[,2]    <- combinations[VARCOMBI,2]
        data_new[,3]    <- combinations[VARCOMBI,3]}
    
      data_new[, x_values]   <- s[, x_values]
      data_new[, COLUMNNAME] <- s[,1]
      
    } else {
      temp              <- data_new_template
      if(nr_var == 1){
        temp[,1]        <- combinations[VARCOMBI,1]}
      if(nr_var == 2){
        temp[,1]        <- combinations[VARCOMBI,1]
        temp[,2]        <- combinations[VARCOMBI,2]}
      if(nr_var == 3){
        temp[,1]        <- combinations[VARCOMBI,1]
        temp[,2]        <- combinations[VARCOMBI,2]
        temp[,3]        <- combinations[VARCOMBI,3]}
        
      temp[, x_values]       <- s[, x_values]
      temp[, COLUMNNAME]     <- s[,1]
      data_new               <- rbind(data_new, temp)
    }
    
  }
  
  return(data_new)
}

interpolator("group", data, "henk", "x")

variable <- "LAI"
var <- interpolator(x, "y", c("henk", "piet"), "x") 

sort(unique(data$Treat))
treatment <- "NFPFK4"
location <- "Benue"
var_selection <- var[var$Location == location & var$Treat == treatment,]
plot(var_selection$MAP, var_selection$LAI, main=treatment)

# Superscript subscript
plot(1,1, main=expression('title'^2))  #superscript
plot(1,1, main=expression('title'[2])) #subscript


# Best model
install.packages("olsrr")
library(olsrr)

model <- lm(CNC ~ PM + HF + HC + WF + WM + PF + FR + FS + PC + OF, data = df)

best_model <- ols_step_best_subset(model)   # What is the best model given number of variables
plot(best_model)

best_model2 <- ols_step_all_possible(model) # What is the best model given all possibilities
plot(best_model2)


# Creating correlation matrix
panel.cor <- function(x, y){
  usr     <- par("usr"); on.exit(par(usr))
  #par(usr = c(0, 1, 0, 1))
  r       <- round(cor(x, y, use="complete.obs"), digits=2)
  txt     <- paste0(r)
  cex.cor <- 1.2#0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor)
}
upper.panel<- function(x, y){ points(x,y, pch = 19)}
blub <- pairs(df_NFRV[,unlist(lapply(df_NFRV, is.numeric))], lower.panel = panel.cor,  upper.panel = upper.panel)



