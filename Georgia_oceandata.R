##############
# Extracting oceanography data for Georgia Mergand's Masters research 

# Author: Claire Mason claire.mason@utas.edu.au

# Last update: Feb 23, 2021 - have updated sampling dates from Georgia, added in soi and sam values, fixed for loops which weren't working from last time 
            ### changed to d[grep... ] rather than dates$Code[grep... ] which makes it a list of relevant dates rather                    than just a vector of grouping names )) 


###-----------------------------

# Research aims- 
  # Here, we aim to extract average values of oceanography/environmental variables within the known foraging range of shy albatross to match with scat sampling (Julie McInnes phd data) that infers diet. 
  # There are 7 years of diet data, across 4 breeding stages (Incubation, Brood, Chick rearing and Non breeding). The dates of interest for each year:stage combo are different and dependent on the sampling period in the field and 7 days prior. 
  # This data is solely Albatross Island birds, and the methods used to create 'home range' polygons for each of the 4 breeding stages can be found in the FRDC Report written by Julie McInnes and Claire Mason.

# What this code does- 
  # imports 'home range' shapefiles of breeding stage which become the extent that we extract ocean data from 
  # fills in dates between a specified start and end date for each sampling period that is imported from an excel file  
  # extract ocean variables for the list of dates, within the relevant shapefile, and then averages all the values
  # the output of this code is a single averaged value for each ocean variable, for each Code/'Grouping' which is a Stage/Year combination 


#------------------------
# required packages 
library(readxl);library(tidyverse);library(raster);library(rgdal);library(rgeos);library(maptools)

#-------------------------
# Input data 

###### 1. Shy albatross foraging range 

# four polygons for broad home range (95% UD) during Incubation (Sept-Oct), Brood (Dec), Chick rearing (Jan-Mar) and Non-breeding stages (May-Aug). 
# Created by Claire Mason, Methods for creating these can be found in the FRDC report by Julie McInnes. 

inc_shp <- readOGR(dsn="frdc_shapefiles/incALL_95.shp", layer="incALL_95")
br_shp <- readOGR(dsn="frdc_shapefiles/br_95_AIonly.shp", layer="br_95_AIonly")
chk_shp <- readOGR(dsn="frdc_shapefiles/chk_95_onlyAI.shp", layer="chk_95_onlyAI")
nbr_shp <- readOGR(dsn="frdc_shapefiles/nonbr_95_updated.shp", layer="nonbr_95_updated")


##### 2. Collection/field work timing info 

# import dataframe with Start DT, End DT columns created by Georgia based on field collection period and 7 days prior
dates <- read_excel("sampling_period_Georgia_updated.xlsx") # new data also includes SOI and SAM values for each Code/Grouping


# convert date times to POSTIXCT 
dates$startdt <- as.POSIXct(strptime(paste(dates$StartDT), format="%Y-%m-%d %H:%M:%S")) # is this "format=" is what you've got (not what you want)
dates$enddt <- as.POSIXct(strptime(paste(dates$EndDT), format="%Y-%m-%d %H:%M:%S"))


###### 3. Raadtools oceanographic data 

#devtools::install_github("AustralianAntarcticDivision/raadtools")
library(raadtools)


#-------------------------
# Extract ocean data , these are the three oceanography variables we are interested in 
    # readchla()
    # readssh()
    # readsst()


####### 1. create list of 'dates of interest' for each year:stage, this is the length of the field trip + 7 days prior 

# create function to fill in dates between a start and end dt
date_seq <- function(start, end) {seq.Date(lubridate::ymd(start), lubridate::ymd(end), by="days")}
  
# check 
#date_seq(incdates$startdt[1], incdates$enddt[1])

## ###### for loop ########
dates2 <- na.omit(dates) # for loop doesn't work if there are NAs 

# make blank lists to save outputs into 
o <- list()
o2 <- list()

# for loop- for each Code (StageYear ID) create list of dates between start and end date
for (i in unique(dates2$Code)) {  
  
  # extract row of interest, which includes start and end date value 
  a <- dates2[dates2$Code ==i ,]
  
  # create vector of all dates between start and end date using function I made above 
  b <- date_seq(a$startdt, a$enddt)
  
  # add vector of dates to a list for each year:stage Code
  o[[a$Code==i]] <- as.Date(strptime(paste(b), format="%Y-%m-%d")) # as PoSIXct added AEDT/AEST timezone to the timestamp .. might of caused errors? 
  
  # save outputs
  o2[[i]] <- o

}

o2 # have a look at the output of the loop 

# unlist but preserve datetime format 
d <- do.call("c", o2)

# gives number of days in each sampling period 
summary(d)


# test run to see if output can be used in raadtools readXXX functions- worked yay :) 
test <- readsst((d[[2]]), 
                time.resolution="daily", 
                xylim = inc_shp, 
                varname="sst")

summary(test)



####### 2. Extracting values 

# for loop to extract average values for each grouping of dates 


# start with Incubation 
names(d)
dates2
unique(dates2$Code)

#inc_dates <- dates2$Code[grep("Inc", dates2$Code)] !! WRONG , the issue! Need the list of relevant dates not just the Code

inc_dates <- d[grep("Inc", dates2$Code)]


# blank list to save outputs of loop to 
inc_vals <- list()

#for loop 
for (i in names(inc_dates)) {  # for each Code (StageYear ID) create list of dates between start and end date
    
  #run raadtools on the list of dates 
  sst <- readsst((inc_dates[[i]]), 
                  time.resolution="daily", 
                  xylim = inc_shp)
  
  #calculate mean 
  sst_val <- data.frame(sst=mean(na.omit(sst@data@values)))
  
  #save
  inc_vals$sst[[i]] <-  sst_val
  
  # sea surface height extract 
  ssh <- readssh((inc_dates[[i]]), 
                 time.resolution="daily", 
                 xylim = inc_shp)
  
   # calculate mean
  ssh_val <- mean(na.omit(ssh@data@values))
  
  #save 
  inc_vals$ssh[[i]] <-  ssh_val

  # chl-a extract 
  chla <- readchla(inc_dates[[i]], 
                   xylim= inc_shp)

  # calculate mean
  chla_val <- data.frame(chla=mean(na.omit(chla@data@values)))
  
    # save
  inc_vals$chla[[i]] <-  chla_val
  
 }



#create dataframe from outputs 
inc_vals # original output
inc_df <- data.frame(sst=unlist(inc_vals$sst), ssh=unlist(inc_vals$ssh), chla=unlist(inc_vals$chla))
rownames(inc_df) <- dates2$Code[grep("Inc", dates2$Code)]

# add in soi and sam 
index <- dates[, c(3,9,10) ] # extract SOI and SAM values from sampling dates spreadsheet that Georgia added in 
inc_df$Grouping <- rownames(inc_df)
inc_df <- left_join(inc_df, index, by="Grouping" )


############################################################
# Brood 

#list all the years with data 
br_dates <- d[grep("Br", dates2$Code)]

#create blank list to save outputs
br_vals <- list()

for (i in names(br_dates)) {
  
  sst <- readsst((br_dates[[i]]), 
                 time.resolution="daily", 
                 xylim = br_shp)
  
  #calculate mean 
  sst_val <- data.frame(sst=mean(na.omit(sst@data@values)))
  
  #save
  br_vals$sst[[i]] <-  sst_val
  
  
  # sea surface height extract 
  ssh <- readssh((br_dates[[i]]), 
                 time.resolution="daily", 
                 xylim = br_shp)
  
  # calculate mean
  ssh_val <- mean(na.omit(ssh@data@values))
  
  #save
  br_vals$ssh[[i]] <-  ssh_val
  
  #chl-a extract 
  chla <- readchla(br_dates[[i]], 
                   xylim= br_shp)
  
  # calculate mean
  chla_val <- data.frame(chla=mean(na.omit(chla@data@values)))
  
  
  # save
  br_vals$chla[[i]] <-  chla_val
  
}

#create dataframe from outputs 
br_vals 

br_df <- data.frame(sst=unlist(br_vals$sst), ssh=unlist(br_vals$ssh), chla=unlist(br_vals$chla))
rownames(br_df) <- dates2$Code[grep("Br", dates2$Code)]

# add in soi and sam 
#index <- dates[, c(3,9,10) ]
br_df$Grouping <- rownames(br_df)
br_df <- left_join(br_df, index, by="Grouping" )


###########################################################
# Chick rearing

# Codes to use 
chk_dates <- d[grep("Chk", dates2$Code)]

# blank list to save outputs to 
chk_vals <- list()

for (i in names(chk_dates)) {  
  
  sst <- readsst((chk_dates[[i]]), 
                 time.resolution="daily", 
                 xylim = chk_shp)
  
  #calculate mean 
  sst_val <- data.frame(sst=mean(na.omit(sst@data@values)))
  
  #save
  chk_vals$sst[[i]] <-  sst_val
  
  
  # sea surface height extract 
  ssh <- readssh((chk_dates[[i]]), 
                 time.resolution="daily", 
                 xylim = chk_shp)
  
  # calculate mean
  ssh_val <- mean(na.omit(ssh@data@values))
    chk_vals$ssh[[i]] <-  ssh_val
  
  #chl-a extract 
  chla <- readchla(chk_dates[[i]], 
                   xylim= chk_shp)
  
  # calculate mean
  chla_val <- data.frame(chla=mean(na.omit(chla@data@values)))
  
  
  # cbind output
  chk_vals$chla[[i]] <-  chla_val
  
}

#create dataframe from outputs 
chk_vals 

chk_df <- data.frame(sst=unlist(chk_vals$sst), ssh=unlist(chk_vals$ssh), chla=unlist(chk_vals$chla))

rownames(chk_df) <- dates2$Code[grep("Chk", dates2$Code)]


# add in soi and sam 
#index <- dates[, c(3,9,10) ]
chk_df$Grouping <- rownames(chk_df)
chk_df <- left_join(chk_df, index, by="Grouping" )

###########################################################
# Non breeding
nbr_dates <- d[grep("Nonbr", dates2$Code)]


nbr_vals <- list() # blank list to save outputs to 

for (i in names(nbr_dates)) {  
  
  sst <- readsst((nbr_dates[[i]]), 
                 time.resolution="daily", 
                 xylim = nonbr_shp)
  
  #calculate mean 
  sst_val <- data.frame(sst=mean(na.omit(sst@data@values)))
  
  #save
  nbr_vals$sst[[i]] <-  sst_val
  
  
  # sea surface height extract 
  ssh <- readssh((nbr_dates[[i]]), 
                 time.resolution="daily", 
                 xylim = nonbr_shp)
  
  # calculate mean
  ssh_val <- mean(na.omit(ssh@data@values))
  
  
  # save
  nbr_vals$ssh[[i]] <-  ssh_val
  
  #chl-a extract 
  chla <- readchla(nbr_dates[[i]], 
                   xylim= nonbr_shp)
  
  # calculate mean
  chla_val <- data.frame(chla=mean(na.omit(chla@data@values)))
  
  
  # save
  nbr_vals$chla[[i]] <-  chla_val
  
}

#create dataframe from outputs 
nbr_df <- data.frame(sst=unlist(nbr_vals$sst), ssh=unlist(nbr_vals$ssh), chla=unlist(nbr_vals$chla))
rownames(nbr_df) <- dates2$Code[grep("Nonbr", dates2$Code)]

# add in soi and sam 
#index <- dates[, c(3,9,10) ]
nbr_df$Grouping <- rownames(nbr_df)
nbr_df <- left_join(nbr_df, index, by="Grouping" )

### All together 
ocean_df <- rbind(inc_df, br_df, chk_df, nbr_df)

#reorder columns 
ocean_df <- ocean_df[, c(4, 1, 2, 3, 5, 6)]

# export table 
write.csv(ocean_df, "ocean_df_Feb23.csv")
