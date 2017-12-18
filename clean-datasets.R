# Data Import and Clean
# DSSA-1501 Data Exploration
# Fall 2017
# Robert Knaak

library(tidyverse)
library(lubridate) # Part of the Tidyverse to handle dates and times
library(dplyr)

library(readxl)
library(stringr)

setwd(dir = "C:/Users/rknaa/Documents/datascience/dataexploration-final-project/")

# Function adopted from https://www.kaggle.com/smiller933/fixing-airport-codes/code
#' Convert Department of Transportation 5 digit Airport ID values into
#' their corresponding 3 character IATA values.
#'
#' Any 3 character IATA values in the vector will pass through unmodified.
#'
#' @param x Vector. The input can be either numeric or character.
#' @return A character vector the same length as `x` with 5 digit Airport ID values
#'   replaced by their 3 character IATA values.
#' @seealso
#' @export
#' @examples
#' # The function will accept integers
#' x <- c(10135L, 10136L, 10140L)
#' Airport_ID_to_IATA (x)
#'
#' # The function can also work with character values, with IATA codes mixed in.
#' x <- c("10135", "DSM", "10136", "MSP", "10140")
#' Airport_ID_to_IATA (x)
#'
Airport_ID_to_IATA <- function (x) {
  
  #  Step 0: Initial setup
  #  Load the incoming vector into a data frame, so we can use dplyr functions to join things up.
  df <- data.frame (ID = as.character (x), stringsAsFactors = FALSE)
  #  Store the number of records - used to make sure joins do not introduce duplicates
  num_records = nrow (df)
  
  
  #  Step 1: Add the Description to the base data.
  dfAirportID <- read.csv ("L_AIRPORT_ID.csv",
                           colClasses=c("character", "character"), col.names=c("AirportID", "Description"))
  df <- dplyr::left_join (df, dfAirportID, by=c("ID" = "AirportID"))
  
  
  #  Step 2: Use Description to add the IATA_CODE to the base data.
  dfAirport <- read.csv ("L_AIRPORT.csv",
                         colClasses=c("character", "character"), col.names=c("IATA_CODE", "Description"))
  #
  #  There are duplicated airports. To solve this problem, clear out codes discontinued before 2015.
  #  BSM was discontinued in 1999.
  #  The IATA does not use NYL for Yuma, it uses YUM. So remove NYL.
  dfAirport <- dfAirport [! (dfAirport$IATA_CODE %in% c('BSM', 'NYL')),]
  
  df <- dplyr::left_join (df, dfAirport, by="Description")
  
  
  #  Step 3: Make sure we have the same number of rows that we started with
  #          If this error is triggered, steps will need to be made to eliminate
  #          duplicate key values.
  if (num_records != nrow (df)) {
    stop ("Due to duplicates in the data, the number of records has changed.")
  }
  
  
  #  Step 4: In cases where we didn't get a matching IATA_CODE, copy over the original value
  df$ID <- dplyr::coalesce (df$IATA_CODE, df$ID)
  
  
  #  Step 5: We are all done. Return the results.
  return (df$ID)
}

#flight data, monthly csv files downloaded from DOT

jan15flights <- read_csv('0115-ontime.csv')
feb15flights <- read_csv('0215-ontime.csv')
mar15flights <- read_csv('0315-ontime.csv')
apr15flights <- read_csv('0415-ontime.csv')
may15flights <- read_csv('0515-ontime.csv')
jun15flights <- read_csv('0615-ontime.csv')
jul15flights <- read_csv('0715-ontime.csv')
aug15flights <- read_csv('0815-ontime.csv')
sep15flights <- read_csv('0915-ontime.csv')
oct15flights <- read_csv('1015-ontime.csv')
nov15flights <- read_csv('1115-ontime.csv')
dec15flights <- read_csv('1215-ontime.csv')


#make one big data frame for 2015
totflights <- bind_rows(jan15flights,feb15flights,mar15flights,apr15flights,may15flights,
                        jun15flights,jul15flights,aug15flights,sep15flights,oct15flights,nov15flights,dec15flights)
#housekeeping, remove the dataframes we initialy loaded the data to
rm(jan15flights,feb15flights,mar15flights,apr15flights,may15flights,
   jun15flights,jul15flights,aug15flights,sep15flights,oct15flights,nov15flights,dec15flights)

glimpse(totflights)

#remove observations with NA for Arrival or Depature Delays
totflights <- totflights %>% filter (!is.na(DEP_DELAY))
totflights <- totflights %>% filter (!is.na(ARR_DELAY))

#convert DOT airport ids to IATA code, for sanity and easier matching to other data
totflights$ORIGIN_AIRPORT_ID <- Airport_ID_to_IATA (totflights$ORIGIN_AIRPORT_ID)
totflights$DEST_AIRPORT_ID <- Airport_ID_to_IATA (totflights$DEST_AIRPORT_ID)

# mutate columns to factors, certain functions work with columns as factors
totflights <-totflights %>% mutate(CARRIER = factor(CARRIER))
totflights <-totflights %>% mutate(DEST_AIRPORT_ID = factor(DEST_AIRPORT_ID))
totflights <-totflights %>% mutate(ORIGIN_AIRPORT_ID = factor(ORIGIN_AIRPORT_ID))



#mutate to add seperate month and month-year cols
totflights <- totflights %>% mutate( YEAR_MONTH = format(FL_DATE, "%Y-%m"), YEAR = format(FL_DATE, "%Y"),MONTH = format(FL_DATE, "%m"))
totflights <-totflights %>% mutate(MONTH = factor(MONTH))


#write out our clean data
write.csv(totflights,'totalflights.csv')
save(totflights,file="totalflights.Rda")
#load("totalflights.Rda")


#TSA claims data
claims<- read_excel('claims-data-2015-as-of-feb-9-2016.xlsx')

#check the column names
colnames(claims)

#rename lower case and remove the spaces

claims <- rename (claims, claimnumber = "Claim Number", datereceived ="Date Received",
                  incidentd = "Incident D", airportcode = "Airport Code", airportname="Airport Name",
                  airlinename = "Airline Name", claimtype="Claim Type", claimsite ="Claim Site",
                  itemcategory = "Item Category", closeamount ="Close Amount", disposition = "Disposition")

#split out the month and year for easy filter
claims <- claims %>% mutate(yearmonth = format(incidentd, "%Y-%m"), year = format(incidentd, "%Y"),month = format(incidentd, "%m"))

#filter out claims that did not occur in 2015
claims <- claims %>% filter(year == '2015')

#double check
colnames(claims)
View(claims)

#write out our clean data
write.csv(totflights,'cleanclaims.csv')
save(totflights,file="cleanclaims.Rda")
#load("cleanclaims.Rda")





