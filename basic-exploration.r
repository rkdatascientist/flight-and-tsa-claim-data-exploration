# Basic Data Exploration
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

#flight data, monthly csv files downloaded from DOT just January for size

janflights <- read_csv('0115-ontime.csv')
janflights <- janflights %>% filter (!is.na(DEP_DELAY))
janflights <- janflights %>% filter (!is.na(ARR_DELAY))

#convert DOT airport ids to IATA code, for sanity and easier matching to other data
janflights$ORIGIN_AIRPORT_ID <- Airport_ID_to_IATA (janflights$ORIGIN_AIRPORT_ID)
janflights$DEST_AIRPORT_ID <- Airport_ID_to_IATA (janflights$DEST_AIRPORT_ID)

# mutate columns to factors, certain functions work with columns as factors
janflights <-janflights %>% mutate(CARRIER = factor(CARRIER))
janflights <-janflights %>% mutate(DEST_AIRPORT_ID = factor(DEST_AIRPORT_ID))
janflights <-janflights %>% mutate(ORIGIN_AIRPORT_ID = factor(ORIGIN_AIRPORT_ID))



#mutate to add seperate month and month-year cols
janflights <- janflights %>% mutate( YEAR_MONTH = format(FL_DATE, "%Y-%m"), YEAR = format(FL_DATE, "%Y"),MONTH = format(FL_DATE, "%m"))
janflights <-janflights %>% mutate(MONTH = factor(MONTH))

#looking at the data 
summary(janflights)
levels(janflights$DEST_AIRPORT_ID)
levels(janflights$ORIGIN_AIRPORT_ID)
summary(janflights$ARR_DELAY)


#Basic statistics on ARR_Delay
min(totflights$DEP_DELAY)
mean(totflights$DEP_DELAY)
sd(janflights$ARR_DELAY, na.rm = TRUE)



#Looking for normality
hist(janflights$DEP_DELAY)
ggqqplot(janflights$DEP_DELAY, ylab = "Departure Delay",title='QQ Plot Departure Delay')

#Doesn't look like a normal distribution

#Further check for normality
#Take a random sample of 1000 to fite requiremnets for shapiro test
janflights1000 <- sample_n(janflights,1000)

shapiro.test(janflights1000$ARR_DELAY)
#p-value < 2.2e-16 less than .05 not normal distribution

ggqqplot(janflights1000$DEP_DELAY, ylab = "Departure Delay",title='QQ Plot Departure Delay')

#Still does not look like a normal distrbution











