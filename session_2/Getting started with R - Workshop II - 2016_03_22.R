# Getting started with R (PART II) - R for Stata users!
  # Scripts adapted from Muenchen & Hilbe 2010 ("R for Stata Users")
        # Selecting variables and observations (Chapters 7-9)
        # Data Management and working with data frames (Chapter 10)


# Daisy2 - Cohort study with 9383 observations from 8441 cows in 42 herds from 4 regions.
  # Source: VER2, contributor John Morton
  # These data are a subset of the data collected during a large prospective longitudinal study
  # carried out in 9 regions of Australia. For many of the analyses, a subset of 7 herds with
  # high rates of reproductive diseases was used.

  # Variables - explained
    # region --> Region (1-4)
    # herd --> Herd Number
    # cow --> Cow number (unique)
    # study_lact --> Study laction number (1st or 2nd in study period)
    # herd_size --> Herd size
    # mwp --> Minimum wait period for herd (days)
    # parity --> Lactation number
    # milk120 --> Milk volume (l) in first 120 days of lactation
    # calv_dt --> Calving date
    # cf --> Calving to first service interval (days)
    # fs --> Conception at first service (0/1)
    # cc --> Calving to conception interval (days)
    # wpc --> Interval from wait period to conception (days)
    # spc --> Service to conception
    # twin --> Twins born ("no"/"yes")
    # dyst --> Dystocia at calving ("no"/"yes")
    # rp --> Retained placenta at calving ("no"/"yes")
    # vag_disch --> Vaginal discharge observed ("no"/"yes")
    # h7 --> Indicator for 7 herd subset


# SELECTING VARIABLES AND OBSERVATIONS ####
  getwd()
  setwd("C:/workshop")
  
  # Load Daisy2 from CSV file (*.csv)
  daisy <- read.csv("daisy2.csv", sep=",")


# Chapter 7: Selecting Variables ####
# 7.1-7.2 Selecting all variables ####
 summary(daisy) # calling the dataframe will list all variables

# 7.3 ...with index numbers ####
  summary(daisy[,]) # rows,columns
  summary(daisy[,12]) # 12th column = cc = Calving to Conception Interval

  summary(daisy[12]) # remove ',' and R assumes column (with dataframe)

  # Stata: "summarize cf-spc"
  summary(daisy[, c(10,11,12,13,14)])
  summary(daisy[,10:14])

  summary(daisy[,-(10:14)]) # Remove columns 10-14
  summary(daisy[,-10:14]) # Doesn't work... try these 2 examples
    -(10:14)
    -10:14

  myQindex1 <- c(10,11,14) # Make your Query list
  myQindex2 <- c(3,10:14,17) # Make your Query list
  summary(daisy[myQindex1])
  summary(daisy[myQindex2])

  # Get column numbers for names:
  names(daisy)
  data.frame(names(daisy))

  # How many variables?
  ncol(daisy)
  summary(daisy[1:ncol(daisy)])

  summary(daisy[4:ncol(daisy)])
# 7.4 ...with variable names ####
  names(daisy)

  summary(daisy[,"twin"]) # row,columns... same as before
  summary(daisy["twin"])

  summary(daisy[c("cf", "fs","cc","wpc","spc")])

# 7.5 ...using logic ####
  # TRUE/FALSE = 1/0

  summary(daisy[c(FALSE,FALSE,FALSE,FALSE,FALSE
                  ,FALSE,FALSE,FALSE,FALSE
                  ,TRUE,TRUE,TRUE,TRUE,TRUE
                  ,FALSE,FALSE,FALSE,FALSE,FALSE)])

  summary(daisy[as.logical(c(0,0,0,0,0,0,0,0,0
                             ,1,1,1,1,1
                             ,0,0,0,0,0))])

  # What's going on here?
  summary(daisy[c(FALSE,FALSE,TRUE)])

  # Generating your logical list
  names(daisy)=="twin"

  
  summary(daisy[names(daisy)=="twin"])
  summary(daisy["twin"])

  # NOT ("!") operator
  summary(daisy[names(daisy)!="twin"]) # This works...
  summary(daisy[,!="twin"]) # This doesn't work!...

  # Create conditional list
  myQnames <- names(daisy)=="cf" |
              names(daisy)=="fs" |
              names(daisy)=="cc" |
              names(daisy)=="wpc" |
              names(daisy)=="spc"

  summary(daisy[myQnames])

# 7.6 ...using string search ####
  # Stata: "keep c*"

  # Use the 'grep' fuction ("Global Regular Expression Print")
  grep("^c", names(daisy)) # "^c" beginning with 'c'
  grep("^c", names(daisy), value=TRUE)  # value: TRUE=string
  
  myQnames <- grep("^c", names(daisy)) # +/- value=T
  summary(daisy[myQnames])

# 7.7 ...using $ notation ####
  summary(daisy$twin)

  summary(c(daisy$cf,daisy$cc)) # NOT GOOD! ...
    head(c(daisy$cf,daisy$cc)) # Appended the two columns in one vector

  # Need to combine then into a single OBJECT (eg dataframe)
  summary(data.frame(daisy$cf,daisy$twin))
    head(data.frame(daisy$cf,daisy$twin))

# 7.8 ...using component names ####
# 7.8.1 ATTACH function ####  
  attach(daisy)

  summary(twin)
  summary(cf,twin)
  
  summary(data.frame(twin,cf))

  detach(daisy) # Need to detach before attaching anotherdataframe
                # Quitting R automatically detaches

# 7.8.2 WITH function ####
  # Specify the dataframe with 'with':
  with(daisy, summary(data.frame(cf,twin)))
  
# 7.8.3 ...with variable names in formulas ####
  lm(daisy$milk120 ~ daisy$rp + daisy$twin + as.factor(daisy$region))
  lm(milk120 ~ rp + twin + as.factor(region), data=daisy)
  
  summary(model1)

# 7.9 ...with SUBSET function ####
  # Stata: sum cf-spc
  head(subset(daisy, select=cf:spc))
  summary(subset(daisy, select=cf:spc))

  head(subset(daisy, select=c(cf:spc,dyst)))

#Skipped a few examples...

# 7.12 Saving selected variables to a new dataset ####
  myqs <- daisy[10:14]
  myqs <- daisy[c("cf","fs","cc","wpc","spc")]

  myqs <- data.frame(daisy$cf, daisy$fs, daisy$cc, 
                     daisy$wpc, daisy$spc)
  #Notice "daisy' in from of names...

  myqs <- data.frame(cf=daisy$cf, fs=daisy$fs, cc=daisy$cc, 
                     wpc=daisy$wpc, spc=daisy$spc)

  attach(daisy)
    myqs <- data.frame(cf,fs,cc,wpc,spc)
  detach(daisy)

  myqs <- subset(daisy, select=cf:spc)
  
  head(myqs)


# Chapter 8: Selecting Observations ####
# 8.1-8.2 Selecting all observations ####
  summary(daisy) # all observations are included

# 8.3 ...using index value ####
  daisy[5,] # row/columns... picking 'cow #5'

  summary(daisy[c(5,6,7,8), ])
  summary(daisy[5:8, ])

  summary(daisy[-c(1,2,3,4,5), ] ) # Excludes first 5 observations
  summary(daisy[-(1:5), ] ) # Excludes first 5 observations

  myOIndex <- c(5,6,7,8)
  summary(daisy[myOIndex,])

  
  myOIndex <- c(1,3,5:50, 75, 77, 90:500)
  summary(daisy[myOIndex,])
  
  # There are other ways to generate sequential data...
  # Slight tangent off course....
      # Sequence patterns
        seq(from=5, to=50, by=5)
        seq(5,50,5)

      # Repeated patterns
        rep(1:2, each=4, times=1)
        rep(1:3, each=1, times=3)
        rep(c(20,23,28), each=2, times=3)
  
  # Get the index values for twins:
  myTIndex <- grep("^y", daisy$twin)  # Get list of 'yes'
  summary(daisy[myTIndex,])

  #NOTE: Index values are dynamic... if you sort the dataset
  #this will change the index values!!!

  nrow(daisy) # reports the number of rows
  summary(daisy[1:nrow(daisy),]) # includes all observations

# 8.4. ...using row names ####
  # Each dataframe has row names (these are 'strings')
  row.names(daisy) # Notice "" = string!

  summary(daisy[c("1","2","3"),])

# 8.5 ...using logic ####
  myRows <- c(T,T,T,T,T,F,F,F,F,F)
  head(daisy[myRows,], 20)

  myBinary <- c(1,1,1,1,1,0,0,0,0,0)
  head(daisy[myBinary,], 20) # What happened? ...repeated 1 5x

  myRows <- as.logical(myBinary)
  head(daisy[myRows,], 20) # Now it works...

  daisy$twin=="yes"
  head(daisy$twin=="yes", 50)

  daisy[daisy$twin=="yes",]

  # BUT WHAT ABOUT MISSING VALUES???
  # Example: parity, milk120, cf, fs, cc, spc, and spc have NAs...

  head(daisy[daisy$cf>=60,]) # What happened to 4th & 6th observation?

  which(daisy$cf>=60) # ignores NAs and FALSE
  cf_60 <- which(daisy$cf>=60)

  head(daisy[cf_60,]) # Excluded NAs
  head(daisy[-cf_60,]) # Includes NAs and TRUE
  summary(daisy[-cf_60,"cf"])
  summary(daisy[cf_60,"cf"])

  # More complicated logic (2+ conditions)
  YoungFertileTwin <- which(daisy$parity<=2
                      & daisy$cf<60
                      & daisy$twin=="yes")
  YoungFertileTwin

  # Comparing the many ways to analyze those cows with twins:
  myTwins <- which( daisy$twin=="yes")
  myTwins <- which( daisy[15] == "yes")
  myTwins <- which( daisy["twin"] == "yes")
  with(daisy,
       myTwins <- which(twin=="yes")
      )
  attach(daisy)
    myTwins <- which(twin=="yes")
  detach(daisy)

# 8.7 ...using the SUBSET function ####
  head(subset(daisy, subset=twin=="yes"), 20)
  head(subset(daisy, twin=="yes"), 20) # remove 'subset='

  subset(daisy, parity<=2 & cf<60 & twin=="yes")

# 8.8 Saving selected observations to a new dataframe
  YoungFertileTwin <- which(daisy$parity<=2
                          & daisy$cf<60
                          & daisy$twin=="yes")

  YoungFertileTwin <- subset(daisy, parity<=2 
                          & cf<60 & twin=="yes")


# Chapter 9: Selecting Observations and Variables ####
# 9.1 The SUBSET function ####
  # Select observations with 'subset='
  # Select variables with 'select='
  summary(
    subset(daisy,
           subset=parity<=2 & cf<60 & twin=="yes",
           select=c(cow, parity, milk120, cf, twin) )
    )

  summary(
    subset(daisy, parity<=2 & cf<60 & twin=="yes",
           c(cow, parity, milk120, cf, twin) )
  ) # you can remove 'subset=' and 'select=' if order is kept

  mySubsetData <- subset(daisy, parity<=2 & cf<60 & twin=="yes",
                   c(cow, parity, milk120, cf, twin) )

  summary(mySubsetData)

# 9.2 Observations by logic & Variables by name ####
  # MOST PRACTICAL!!!! (...for stata users)

  summary(
    daisy[ which(daisy$parity<=2 & daisy$cf<60 & daisy$twin=="yes") ,
           c("cow","parity","milk120","cf")]
    ) # Similar to 'subset', but "string" index value

  # Could improve legibility by defning both index first
    myObs <- which(daisy$parity<=2 & daisy$cf<60 & daisy$twin=="yes")
    myVars <- c("cow","parity","milk120","cf")

    summary(daisy[myObs,myVars])

# 9.3 Names to select both ####
  summary(daisy[
      c("5","6","7","8","9","10"),
      c("cow","parity","milk120","cf")
      ] )
  #Not very practical!!!

# 9.4 Number index values to select both ####
  summary(daisy[
      c(5,6,7,8,9,10),
      c(3,7,8,10)
      ] )
  #Not very practical!!!

# 9.5 Logic to select both ####
  # Skipped - use %in% operator... not practical!

# Clean up
  rm(list = ls())  # Clear entire workspace
  # Ctrl+L will clear the Console


# _ ####
# DATA MANAGEMENT AND WORKING WITH DATA FRAMES ####
  getwd()
  setwd("C:/workshop")
  
  # Load Daisy2 from CSV file (*.csv)
  daisy <- read.csv("daisy2.csv", sep=",")


# Chapter 10: Data Management ####
# 10.1 Transforming variables ####
  daisy$lnMilk120 <- log(daisy$milk120) # natural log
                                        # log10 = base 10

  daisy$average <- (daisy$cf + daisy$fs
                   + daisy$cc + daisy$wpc) / 4

  daisy <- transform(daisy,
    average=(cf + fs + cc + wpc)/4
  ) # transform is Similar to 'attach'

  daisy <- transform(daisy,
    score1=(cf + fs)/2,
    score2=(cc + wpc)/2
    ) # generate multiple new variables

  # Note the variables have to exist for this work... Example:
  daisy <- transform(daisy,
    score3=(cf + fs)/2,
    score4=(cc + wpc)/2,
    mean3_4=(score3 + score4)/2
  ) # This won't work

  #But breaking it up in two steps will work:
  daisy <- transform(daisy,
    score3=(cf + fs)/2,
    score4=(cc + wpc)/2
  )
  daisy <- transform(daisy,
    mean3_4=(score3 + score4)/2
  )

  # Column bind function
  daisy <- data.frame(cbind(daisy, newName=0)) # Generate constant
  daisy["newName"] <- (daisy$cf + daisy$fs
                     + daisy$cc + daisy$wpc) / 4 # Fill it with values

  # Caution with 'attach' function...
  # Think of attach as generating a temp copy of data frame
  attach(daisy) # Warning means the names were already in memory
    head(newName)
    daisy$newName <- newName^0.5
    head(newName)

  attach(daisy)
    head(newName)
  detach(daisy)

  # Dropping the new variables (all found at the end of dataset)
  daisy$lnMilk120 <- NULL
  daisy <- daisy[-20]
  daisy[(20:ncol(daisy))] <- list(NULL)

# 10.2 Functions or Commands? APPLY function ####
  # Using equations (like above) has some problems such
  #as not dealing with missing values appropriately

  #Functions, on the other hand can handle these 
  # (eg MEAN function)

  # R needs to APPLY functions on variables|observations
  # APPLY: returns values obtained by applying a function
  # to margins of a matrix

# 10.2.1 APPLYING the MEAN function
  # APPLY function
  mean(daisy$cf, na.rm=TRUE) # Returns one value within column

  myQmatrix <- as.matrix(daisy[,10:14])
  head(myQmatrix)

  mean(myQmatrix, na.rm=T) # OK... but not interesting!
  apply(myQmatrix, 2, mean, na.rm=T) # 1 = rows, 2 = columns

  apply(myQmatrix, 1, mean, na.rm=T) # 1 = rows
    
  rowMeans(myQmatrix, na.rm=T) # Popular choice...so rowMeans exist

  #Add new average variable
  daisy$average <- apply(myQmatrix, 1, mean, na.rm=T)
  daisy$average <- rowMeans(myQmatrix, mean, na.rm=T)

  #LAPPLY function (List APPLY function - for data frames)
  # With 'List', there is not need to specify row or column...
  # it knows you want results in a list
  lapply(daisy[,10:14], mean, na.rm=T) # Reports in a list

  sapply(daisy[,10:14], mean, na.rm=T) # Reports in a vector
  # Use other summary statistics: sd, var, median

# 10.2.2 Finding N or NVALID
  length(daisy$milk120) # N
  
  is.na(daisy$milk120) # Finds the missing values
  !is.na(daisy$milk120) # Find the non-missing values
  sum(!is.na(daisy$milk120)) # Counts the non-missing values

  summary(daisy$milk120)

# 10.3 Conditional transformations ####
  # Split parity into new dichotomous variable (<3yrs = 0, 3+yrs = 1)
  daisy$par_old <- ifelse(daisy$parity>=3, 1, 0)
  head(daisy, 20) # Similar to IF in Excel... skips missing

  # 0/1 outcomes can also be generated by:
  daisy$par_old <- as.numeric(daisy$parity>=3)

  daisy$old_twin <- ifelse(daisy$parity>=3 
                         & daisy$twin=="yes", 1, 0)
  sum(daisy$old_twin, na.rm=T)

  # More than 2 categories?
  daisy$par_cat[daisy$parity == 1] <- 1
  daisy$par_cat[daisy$parity == 2] <- 2
  daisy$par_cat[daisy$parity == 3] <- 3
  daisy$par_cat[daisy$parity >= 4] <- 4
  summary(daisy$par_cat)


# 10.5 Finding complete observations ####
  myNoMissing <- na.omit(daisy)
  head(myNoMissing, 20) # Wow.. lost a lot of observations
    length(daisy[,1]) # 9383 obs
    length(myNoMissing[,1]) # now... 5683

  #using logic
  complete.cases(daisy)
  myNoMissing <- daisy[complete.cases(daisy),]

  # Incompeltes
  myIncompletes <- daisy[!complete.cases(daisy),]
  head(myIncompletes, 20)

# 10.6 Renaming variables ####
  library("reshape") # 'rename' function !
  
  myChanges <- c(cf="x1", fs="x2", cc="x3", wpc="x4", spc="x5")
  myChanges

  daisy <- rename(daisy, myChanges)

  names(daisy) # See the vectors of string names
  names(daisy) <- c("region","herd","cow","study_lact",
                    "herd_size","mwp","parity","milk120",
                    "calv_dt","cf","fs","cc","wpc","spc",
                    "twin","dyst","rp","vag_disch","h7",
                    "newName","par_cat","par_old","old_twin")
  # Rename by index value
  daisy <- rename(daisy, myChanges)
  names(daisy)[10:14] <- c("cf","fs","cc","wpc","spc")

  #Lots of other examples in book...

# 10.7 Recoding variables ####
  # Using the previously generated parity categorical variable
  daisy$par_cat[daisy$parity == 1] <- 1
  daisy$par_cat[daisy$parity == 2] <- 2
  daisy$par_cat[daisy$parity == 3] <- 3
  daisy$par_cat[daisy$parity >= 4] <- 4

  # Re-order in reverse oder:
  daisy$par_catr1 <- 5-daisy$par_cat # 5-4 = 1, 5-3 = 2...

# 10.7.1 Recoding variables:
  library("car")
  daisy$par_catr2 <- recode(daisy$par_catr1, "1=4; 2=3; 3=2; 4=1")

  head(daisy$parity, 20)
  head(daisy$par_cat, 20)
  head(daisy$par_catr1, 20)
  head(daisy$par_catr2, 20)

# 10.9 Appending datasets ####
  # Appending - need the exact same variables!!!
  twins <- daisy[ which(twin=="yes"), ]
  length(twins$cow)

  nontwins <- daisy[ which(twin=="no"), ]
  length(nontwins$cow)

  both <- rbind(twins, nontwins)
  length(both$cow)

  # Different varaibles
  twins$milk120 <- NULL
  both <- rbind(twins, nontwins) # Doesn't work!

  # Reshape package fix:
  library("reshape")

  both <- rbind.fill(twins, nontwins)
  head(both, 200)

  # Build dataframe with missing variable filled with NAs
  twins <- data.frame(twins, milk120=NA)
  both <- rbind(twins, nontwins) # Now it works!
  
# 10.10 Merging (Joining) datasets ####
  # Need a unique ID
# (or combination of variables to uniquely identify)
# 'cow' is not unique... repeated calving for some cows...
  daisy$id <- row.names(daisy)
  daisyleft <- daisy[c("id","region","herd","cow","study_lact",
                       "herd_size","mwp","parity","milk120")]
  daisyright <- daisy[c("id","calv_dt","cf","fs","cc","wpc",
                        "spc","twin","dyst","rp","vag_disch")]

  both <- merge(daisyleft, daisyright, by="id")

  # 'cow' and 'parity' would uniquely identify our cows
  daisyleft <- daisy[c("region","herd","cow","study_lact",
                     "herd_size","mwp","parity","milk120")]
  daisyright <- daisy[c("cow","parity","calv_dt","cf","fs",
                        "cc","wpc","spc","twin","dyst","rp","vag_disch")]

  both <- merge(daisyleft, daisyright, by=c("cow","parity"))
    # Look at # obs... I don't think cow and parity are unique!!!

  #If the variable names were different for each dataset
  names(daisyleft)[3] <- "cowid"
  names(daisyleft)[7] <- "age"

  both <- merge(daisyleft, daisyright, 
                by.x=c("cowid","age"),
                by.y=c("cow","parity")
          )

# 10.11 Collapse (aggregate) datasets ####
  # Advantages of R over Stata
    # 1) All in one step
    # 2) Aggregate with EVERY function
    #  ...not just those in collapse command
    # 3) Hold aggregated results

  #For these sets of command, attach a dataset is easier
  # some commands get stopped by commas (so attach is good)
  attach(daisy)

#10.11.1 Aggregate function:
  myAgg1 <- aggregate(milk120,
                by=data.frame(region),
                mean, na.rm=TRUE) # na.rm is passed to the mean function
  myAgg1

  # Two variables: by region and twin-status
  myAgg2 <- aggregate(milk120,
                    by=data.frame(region,twin),
                    mean, na.rm=TRUE) # na.rm is passed to the mean function
  myAgg2 # Single values (eg summary statistics)

#10.11.2 TAPPLY function:
# use TAPPLY (Table APPLY function)
  myAgg2 <- tapply(milk120,
                    data.frame(region,twin),
                   mean, na.rm=T)
  myAgg2 # Stored as a numeric matrix
    mode(myAgg2)
    class(myAgg2)

  # Let's get the range (something Stata can't do)
  myAgg2 <- tapply(milk120,
                 data.frame(region,twin),
                 range, na.rm=T)
  myAgg2 # lists the objects within the matrix
    myAgg2[4,2] # row,column... results for region 4, no twins

#10.11.3 Merging back to original dataset:
  #Re-merge region means (myAgg1) back to master dataset
  myAgg1
  daisy_Agg1 <- merge(daisy, myAgg1,
          by="region")
  names(daisy_Agg1)[27] <- "reg_milk"

# 10.11.4 Tabular aggregation (tabulate function in Stata):
  table(daisy$twin)
  
  attach(daisy)
    myCounts <- table(twin,region) # Numeric table
    myCounts
      mode(myCounts)
      class(myCounts)

  # Change to data frame
  myCountsDF <- as.data.frame(myCounts)
  myCountsDF

  # Another exmple with 3 categories!
  myCounts <- table(twin,region,par_cat)
  myCountsDF <- as.data.frame(myCounts)
  myCountsDF

# 10.12 By or Split-File processing ####
  # Repeat analysis for every level
  # We did one method in 10.11.1

  mean( daisy[c("milk120","cf","fs","cc","wpc","spc") ],
        na.rm=TRUE) # I can't get this work!?!

  myBYout <- by(daisy[c("milk120","cf","fs","cc","wpc","spc") ] ,
                daisy["twin"],
                mean, na.rm=TRUE) # This doesn't work either!
  
  # Will use only one variable at a time
  myBYout <- by(daisy$milk120 ,
              daisy["twin"],
              mean, na.rm=TRUE) # This doesn't work either!
  myBYout
  class(myBYout) # 'by'

  # Convert to data frame
  myBYdata <- as.data.frame( (as.table(myBYout)))
  myBYdata

  # Using 'by' to return multiple values!?! (Stata can't do that)
  myVars <- c("milk120","cf","fs","cc","wpc","spc")
  myBys <- daisy[c("region","par_cat")] # play around with this list

  myBYout <- by(daisy[myVars],
                myBys, range, na.rm=TRUE)
  myBYout
    mode(myBYout) # 'list'
    class(myBYout) # 'by'
    names(myBYout) # 'NULL'
  myBYout[[2]] # double brackets for lists
  
  # Store as a data frame?
  myBYoutDF <- as.data.frame(myBYout) # Can't do it!!!
  
  myBYdata <- data.frame(
    rbind(myBYout[[1]], myBYout[[2]], myBYout[[3]], myBYout[[4]]
          , myBYout[[5]], myBYout[[6]], myBYout[[7]], myBYout[[8]]
          , myBYout[[9]], myBYout[[10]], myBYout[[11]], myBYout[[12]]
          , myBYout[[13]], myBYout[[14]], myBYout[[15]], myBYout[[16]] )
    )
  myBYdata # What a mess and a pain!?! (note missing were skipped)

  # do.call runs the command on all components of a list
  myBydata <- data.frame(do.call(rbind,myBYout))
  myBydata # Easier, but still skipped the missing

# 10.13 Removing duplicate observations ####
  myDuplicates <- rbind(daisy, daisy[1:2,])
  length(myDuplicates$cow)
  tail(myDuplicates, 5) # Repeated first two obs at the bottom

  myNoDuplicates <- unique(myDuplicates) # Removed, but didn't show
  length(myNoDuplicates$cow)
  tail(myNoDuplicates, 5)

  # Use 'duplicated' command to show the duplicates
  myDuplicates <- rbind(daisy, daisy[1:2,])

  myDuplicates$Duplicated <- duplicated(myDuplicates)
  head(myDuplicates, 5) # The first ones are not indicated
  tail(myDuplicates, 5)

  myDuplicates[myDuplicates$Duplicated, ]
  
  myNoDuplicates <- myDuplicates[!myDuplicates$Duplicated,-27]

# 10.14 Sorting data frames ####
  # In R, sorting is not necessary for functions (unlike Stata: bysort and merging)
  daisy[c(1,2,3,4), ]
  daisy[c(4,3,2,1),]

  # Store the order of observations, sorted by parity
  myP <- order(daisy$parity)
  head(daisy[myP,], 10)

  # Store the order of observations, sorted by descending parity and acending milk120
  myPM <- order(-daisy$parity, daisy$milk120)
  head(daisy[myPM,], 10)

  mydataSorted <- daisy[myPM,]

# Clean up
  detach(daisy)
  rm(list = ls())  # Clear entire workspace
  # Ctrl+L will clear the Console


