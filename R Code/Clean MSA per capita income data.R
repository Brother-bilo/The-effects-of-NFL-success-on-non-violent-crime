library('tidyverse')

##2000 data wrangling
INC00 <- read.csv("C:/Users/Owner/Documents/ECON 485/Project on Crime/Income Data 00-05/msainc00.csv")

#create a df to modify
inc00 <- INC00 %>%
  #select msa row and pcinc row
  select(X, X.1) %>%
  #filter to extract desired msa data
  filter(str_detect(X,
                    "Austin|
                    |Baltimore|
                    |Boston|
                    |Buffalo|
                    |Forth Worth|
                    |Jacksonville, FL|
                    |Las Vegas|
                    |Los Angeles|
                    |Miami|
                    |New Orleans|
                    |New York|
                    |Oklahoma|
                    |Orlando|
                    |Phoenix|
                    |Pittsburgh|
                    |Rochester, NY|
                    |Salt Lake|
                    |San Diego|
                    |San Fran|
                    |Seattle")) %>%
  #rename column headers
  rename(msa = X) %>%
  rename(pcinc = X.1) %>%
  #add year column
  add_column(year = 2000) %>%
  #rename msa to common name
  mutate(msa = recode_factor(msa, 
                             "Austin-Round Rock-Georgetown, TX (Metropolitan Statistical Area)" = "Austin, TX",
                             "Baltimore-Columbia-Towson, MD (Metropolitan Statistical Area)" = "Baltimore, MD",
                             "Boston-Cambridge-Newton, MA-NH (Metropolitan Statistical Area)" = "Boston, MA",
                             "Buffalo-Cheektowaga, NY (Metropolitan Statistical Area)" = "Buffalo, NY",
                             "Fort Worth-Arlington, TX M.S.A." = "Fort Worth, TX",
                             "Jacksonville, FL (Metropolitan Statistical Area)" = "Jacksonville, FL",
                             "Las Vegas-Henderson-Paradise, NV (Metropolitan Statistical Area)" = "Las Vegas, NV",
                             "Los Angeles-Long Beach-Anaheim, CA (Metropolitan Statistical Area)" = "Los Angeles, CA",
                             "Miami-Fort Lauderdale-Pompano Beach, FL (Metropolitan Statistical Area)" = "Miami, FL",
                             "New Orleans-Metairie, LA (Metropolitan Statistical Area)" = "New Orleans, LA",
                             "New York-Newark-Jersey City, NY-NJ-PA (Metropolitan Statistical Area)" = "New York, NY",
                             "Oklahoma City, OK (Metropolitan Statistical Area)" = "Oklahoma City, OK",
                             "Orlando-Kissimmee-Sanford, FL (Metropolitan Statistical Area)" = "Orlando, FL",
                             "Phoenix-Mesa-Chandler, AZ (Metropolitan Statistical Area)" = "Phonenix, AZ",
                             "Pittsburgh, PA (Metropolitan Statistical Area)" = "Pittsburgh, PA",
                             "Rochester, NY (Metropolitan Statistical Area)" = "Rochester, NY",
                             "Salt Lake City, UT (Metropolitan Statistical Area)" = "Salt Lake City, UT",
                             "San Diego-Chula Vista-Carlsbad, CA (Metropolitan Statistical Area)" = "San Diego, CA",
                             "San Francisco-Oakland-Berkeley, CA (Metropolitan Statistical Area)" = "San Francisco, CA",
                             "Seattle-Tacoma-Bellevue, WA (Metropolitan Statistical Area)" = "Seattle, WA"))

#2001
INC01 <- read.csv("C:/Users/Owner/Documents/ECON 485/Project on Crime/Income Data 00-05/msainc01.csv")

inc01 <- INC01 %>%
  #select msa row and pcinc row
  select(X, X.1) %>%
  #filter to extract desired msa data
  filter(str_detect(X,
                    "Austin|
                    |Baltimore|
                    |Boston|
                    |Buffalo|
                    |Forth Worth|
                    |Jacksonville, FL|
                    |Las Vegas|
                    |Los Angeles|
                    |Miami|
                    |New Orleans|
                    |New York|
                    |Oklahoma|
                    |Orlando|
                    |Phoenix|
                    |Pittsburgh|
                    |Rochester, NY|
                    |Salt Lake|
                    |San Diego|
                    |San Fran|
                    |Seattle")) %>%
  #rename column headers
  rename(msa = X) %>%
  rename(pcinc = X.1) %>%
  #add year column
  add_column(year =2001) %>%
  #rename msa to common name
  mutate(msa = recode_factor(msa, 
                             "Austin-Round Rock-Georgetown, TX (Metropolitan Statistical Area)" = "Austin, TX",
                             "Baltimore-Columbia-Towson, MD (Metropolitan Statistical Area)" = "Baltimore, MD",
                             "Boston-Cambridge-Newton, MA-NH (Metropolitan Statistical Area)" = "Boston, MA",
                             "Buffalo-Cheektowaga, NY (Metropolitan Statistical Area)" = "Buffalo, NY",
                             "Fort Worth-Arlington, TX M.S.A." = "Fort Worth, TX",
                             "Jacksonville, FL (Metropolitan Statistical Area)" = "Jacksonville, FL",
                             "Las Vegas-Henderson-Paradise, NV (Metropolitan Statistical Area)" = "Las Vegas, NV",
                             "Los Angeles-Long Beach-Anaheim, CA (Metropolitan Statistical Area)" = "Los Angeles, CA",
                             "Miami-Fort Lauderdale-Pompano Beach, FL (Metropolitan Statistical Area)" = "Miami, FL",
                             "New Orleans-Metairie, LA (Metropolitan Statistical Area)" = "New Orleans, LA",
                             "New York-Newark-Jersey City, NY-NJ-PA (Metropolitan Statistical Area)" = "New York, NY",
                             "Oklahoma City, OK (Metropolitan Statistical Area)" = "Oklahoma City, OK",
                             "Orlando-Kissimmee-Sanford, FL (Metropolitan Statistical Area)" = "Orlando, FL",
                             "Phoenix-Mesa-Chandler, AZ (Metropolitan Statistical Area)" = "Phonenix, AZ",
                             "Pittsburgh, PA (Metropolitan Statistical Area)" = "Pittsburgh, PA",
                             "Rochester, NY (Metropolitan Statistical Area)" = "Rochester, NY",
                             "Salt Lake City, UT (Metropolitan Statistical Area)" = "Salt Lake City, UT",
                             "San Diego-Chula Vista-Carlsbad, CA (Metropolitan Statistical Area)" = "San Diego, CA",
                             "San Francisco-Oakland-Berkeley, CA (Metropolitan Statistical Area)" = "San Francisco, CA",
                             "Seattle-Tacoma-Bellevue, WA (Metropolitan Statistical Area)" = "Seattle, WA"))
  
#2002 data
INC02 <- read.csv("C:/Users/Owner/Documents/ECON 485/Project on Crime/Income Data 00-05/msainc02.csv")

inc02 <- INC02 %>%
  #select msa row and pcinc row
  select(X, X.1) %>%
  #filter to extract desired msa data
  filter(str_detect(X,
                    "Austin|
                    |Baltimore|
                    |Boston|
                    |Buffalo|
                    |Forth Worth|
                    |Jacksonville, FL|
                    |Las Vegas|
                    |Los Angeles|
                    |Miami|
                    |New Orleans|
                    |New York|
                    |Oklahoma|
                    |Orlando|
                    |Phoenix|
                    |Pittsburgh|
                    |Rochester, NY|
                    |Salt Lake|
                    |San Diego|
                    |San Fran|
                    |Seattle")) %>%
  #rename column headers
  rename(msa = X) %>%
  rename(pcinc = X.1) %>%
  #add year column 
  add_column(year = 2002) %>%
  #rename msa to common name
  mutate(msa = recode_factor(msa, 
                             "Austin-Round Rock-Georgetown, TX (Metropolitan Statistical Area)" = "Austin, TX",
                             "Baltimore-Columbia-Towson, MD (Metropolitan Statistical Area)" = "Baltimore, MD",
                             "Boston-Cambridge-Newton, MA-NH (Metropolitan Statistical Area)" = "Boston, MA",
                             "Buffalo-Cheektowaga, NY (Metropolitan Statistical Area)" = "Buffalo, NY",
                             "Fort Worth-Arlington, TX M.S.A." = "Fort Worth, TX",
                             "Jacksonville, FL (Metropolitan Statistical Area)" = "Jacksonville, FL",
                             "Las Vegas-Henderson-Paradise, NV (Metropolitan Statistical Area)" = "Las Vegas, NV",
                             "Los Angeles-Long Beach-Anaheim, CA (Metropolitan Statistical Area)" = "Los Angeles, CA",
                             "Miami-Fort Lauderdale-Pompano Beach, FL (Metropolitan Statistical Area)" = "Miami, FL",
                             "New Orleans-Metairie, LA (Metropolitan Statistical Area)" = "New Orleans, LA",
                             "New York-Newark-Jersey City, NY-NJ-PA (Metropolitan Statistical Area)" = "New York, NY",
                             "Oklahoma City, OK (Metropolitan Statistical Area)" = "Oklahoma City, OK",
                             "Orlando-Kissimmee-Sanford, FL (Metropolitan Statistical Area)" = "Orlando, FL",
                             "Phoenix-Mesa-Chandler, AZ (Metropolitan Statistical Area)" = "Phonenix, AZ",
                             "Pittsburgh, PA (Metropolitan Statistical Area)" = "Pittsburgh, PA",
                             "Rochester, NY (Metropolitan Statistical Area)" = "Rochester, NY",
                             "Salt Lake City, UT (Metropolitan Statistical Area)" = "Salt Lake City, UT",
                             "San Diego-Chula Vista-Carlsbad, CA (Metropolitan Statistical Area)" = "San Diego, CA",
                             "San Francisco-Oakland-Berkeley, CA (Metropolitan Statistical Area)" = "San Francisco, CA",
                             "Seattle-Tacoma-Bellevue, WA (Metropolitan Statistical Area)" = "Seattle, WA"))
  
#2003 data
INC03 <- read.csv("C:/Users/Owner/Documents/ECON 485/Project on Crime/Income Data 00-05/msainc03.csv")
  
inc03 <- INC03 %>%
  #select msa row and pcinc row
  select(X, X.1) %>%
  #filter to extract desired msa data
  filter(str_detect(X,
                    "Austin|
                    |Baltimore|
                    |Boston|
                    |Buffalo|
                    |Forth Worth|
                    |Jacksonville, FL|
                    |Las Vegas|
                    |Los Angeles|
                    |Miami|
                    |New Orleans|
                    |New York|
                    |Oklahoma|
                    |Orlando|
                    |Phoenix|
                    |Pittsburgh|
                    |Rochester, NY|
                    |Salt Lake|
                    |San Diego|
                    |San Fran|
                    |Seattle")) %>%
  #rename column headers
  rename(msa = X) %>%
  rename(pcinc = X.1) %>%
  #add year column 
  add_column(year = 2003) %>%
  #rename msa to common name
  mutate(msa = recode_factor(msa, 
                             "Austin-Round Rock-Georgetown, TX (Metropolitan Statistical Area)" = "Austin, TX",
                             "Baltimore-Columbia-Towson, MD (Metropolitan Statistical Area)" = "Baltimore, MD",
                             "Boston-Cambridge-Newton, MA-NH (Metropolitan Statistical Area)" = "Boston, MA",
                             "Buffalo-Cheektowaga, NY (Metropolitan Statistical Area)" = "Buffalo, NY",
                             "Fort Worth-Arlington, TX M.S.A." = "Fort Worth, TX",
                             "Jacksonville, FL (Metropolitan Statistical Area)" = "Jacksonville, FL",
                             "Las Vegas-Henderson-Paradise, NV (Metropolitan Statistical Area)" = "Las Vegas, NV",
                             "Los Angeles-Long Beach-Anaheim, CA (Metropolitan Statistical Area)" = "Los Angeles, CA",
                             "Miami-Fort Lauderdale-Pompano Beach, FL (Metropolitan Statistical Area)" = "Miami, FL",
                             "New Orleans-Metairie, LA (Metropolitan Statistical Area)" = "New Orleans, LA",
                             "New York-Newark-Jersey City, NY-NJ-PA (Metropolitan Statistical Area)" = "New York, NY",
                             "Oklahoma City, OK (Metropolitan Statistical Area)" = "Oklahoma City, OK",
                             "Orlando-Kissimmee-Sanford, FL (Metropolitan Statistical Area)" = "Orlando, FL",
                             "Phoenix-Mesa-Chandler, AZ (Metropolitan Statistical Area)" = "Phonenix, AZ",
                             "Pittsburgh, PA (Metropolitan Statistical Area)" = "Pittsburgh, PA",
                             "Rochester, NY (Metropolitan Statistical Area)" = "Rochester, NY",
                             "Salt Lake City, UT (Metropolitan Statistical Area)" = "Salt Lake City, UT",
                             "San Diego-Chula Vista-Carlsbad, CA (Metropolitan Statistical Area)" = "San Diego, CA",
                             "San Francisco-Oakland-Berkeley, CA (Metropolitan Statistical Area)" = "San Francisco, CA",
                             "Seattle-Tacoma-Bellevue, WA (Metropolitan Statistical Area)" = "Seattle, WA"))
  
  #2004 data
  INC04 <- read.csv("C:/Users/Owner/Documents/ECON 485/Project on Crime/Income Data 00-05/msainc04.csv")
  
  inc04 <- INC04 %>%
    #select msa row and pcinc row
    select(X, X.1) %>%
    #filter to extract desired msa data
    filter(str_detect(X,
                      "Austin|
                    |Baltimore|
                    |Boston|
                    |Buffalo|
                    |Forth Worth|
                    |Jacksonville, FL|
                    |Las Vegas|
                    |Los Angeles|
                    |Miami|
                    |New Orleans|
                    |New York|
                    |Oklahoma|
                    |Orlando|
                    |Phoenix|
                    |Pittsburgh|
                    |Rochester, NY|
                    |Salt Lake|
                    |San Diego|
                    |San Fran|
                    |Seattle")) %>%
    #rename column headers
    rename(msa = X) %>%
    rename(pcinc = X.1) %>%
    #add year column 
    add_column(year = 2004) %>%
  #rename msa to common name
  mutate(msa = recode_factor(msa, 
                             "Austin-Round Rock-Georgetown, TX (Metropolitan Statistical Area)" = "Austin, TX",
                             "Baltimore-Columbia-Towson, MD (Metropolitan Statistical Area)" = "Baltimore, MD",
                             "Boston-Cambridge-Newton, MA-NH (Metropolitan Statistical Area)" = "Boston, MA",
                             "Buffalo-Cheektowaga, NY (Metropolitan Statistical Area)" = "Buffalo, NY",
                             "Fort Worth-Arlington, TX M.S.A." = "Fort Worth, TX",
                             "Jacksonville, FL (Metropolitan Statistical Area)" = "Jacksonville, FL",
                             "Las Vegas-Henderson-Paradise, NV (Metropolitan Statistical Area)" = "Las Vegas, NV",
                             "Los Angeles-Long Beach-Anaheim, CA (Metropolitan Statistical Area)" = "Los Angeles, CA",
                             "Miami-Fort Lauderdale-Pompano Beach, FL (Metropolitan Statistical Area)" = "Miami, FL",
                             "New Orleans-Metairie, LA (Metropolitan Statistical Area)" = "New Orleans, LA",
                             "New York-Newark-Jersey City, NY-NJ-PA (Metropolitan Statistical Area)" = "New York, NY",
                             "Oklahoma City, OK (Metropolitan Statistical Area)" = "Oklahoma City, OK",
                             "Orlando-Kissimmee-Sanford, FL (Metropolitan Statistical Area)" = "Orlando, FL",
                             "Phoenix-Mesa-Chandler, AZ (Metropolitan Statistical Area)" = "Phonenix, AZ",
                             "Pittsburgh, PA (Metropolitan Statistical Area)" = "Pittsburgh, PA",
                             "Rochester, NY (Metropolitan Statistical Area)" = "Rochester, NY",
                             "Salt Lake City, UT (Metropolitan Statistical Area)" = "Salt Lake City, UT",
                             "San Diego-Chula Vista-Carlsbad, CA (Metropolitan Statistical Area)" = "San Diego, CA",
                             "San Francisco-Oakland-Berkeley, CA (Metropolitan Statistical Area)" = "San Francisco, CA",
                             "Seattle-Tacoma-Bellevue, WA (Metropolitan Statistical Area)" = "Seattle, WA"))

  #2005 data
  INC05 <- read.csv("C:/Users/Owner/Documents/ECON 485/Project on Crime/Income Data 00-05/msainc05.csv")
  
  inc05 <- INC05 %>%
    #select msa row and pcinc row
    select(X, X.1) %>%
    #filter to extract desired msa data
    filter(str_detect(X,
                      "Austin|
                    |Baltimore|
                    |Boston|
                    |Buffalo|
                    |Forth Worth|
                    |Jacksonville, FL|
                    |Las Vegas|
                    |Los Angeles|
                    |Miami|
                    |New Orleans|
                    |New York|
                    |Oklahoma|
                    |Orlando|
                    |Phoenix|
                    |Pittsburgh|
                    |Rochester, NY|
                    |Salt Lake|
                    |San Diego|
                    |San Fran|
                    |Seattle")) %>%
    #rename column headers
    rename(msa = X) %>%
    rename(pcinc = X.1) %>%
    #add year column 
    add_column(year = 2005)%>%
  #rename msa to common name
  mutate(msa = recode_factor(msa, 
                             "Austin-Round Rock-Georgetown, TX (Metropolitan Statistical Area)" = "Austin, TX",
                             "Baltimore-Columbia-Towson, MD (Metropolitan Statistical Area)" = "Baltimore, MD",
                             "Boston-Cambridge-Newton, MA-NH (Metropolitan Statistical Area)" = "Boston, MA",
                             "Buffalo-Cheektowaga, NY (Metropolitan Statistical Area)" = "Buffalo, NY",
                             "Fort Worth-Arlington, TX M.S.A." = "Fort Worth, TX",
                             "Jacksonville, FL (Metropolitan Statistical Area)" = "Jacksonville, FL",
                             "Las Vegas-Henderson-Paradise, NV (Metropolitan Statistical Area)" = "Las Vegas, NV",
                             "Los Angeles-Long Beach-Anaheim, CA (Metropolitan Statistical Area)" = "Los Angeles, CA",
                             "Miami-Fort Lauderdale-Pompano Beach, FL (Metropolitan Statistical Area)" = "Miami, FL",
                             "New Orleans-Metairie, LA (Metropolitan Statistical Area)" = "New Orleans, LA",
                             "New York-Newark-Jersey City, NY-NJ-PA (Metropolitan Statistical Area)" = "New York, NY",
                             "Oklahoma City, OK (Metropolitan Statistical Area)" = "Oklahoma City, OK",
                             "Orlando-Kissimmee-Sanford, FL (Metropolitan Statistical Area)" = "Orlando, FL",
                             "Phoenix-Mesa-Chandler, AZ (Metropolitan Statistical Area)" = "Phonenix, AZ",
                             "Pittsburgh, PA (Metropolitan Statistical Area)" = "Pittsburgh, PA",
                             "Rochester, NY (Metropolitan Statistical Area)" = "Rochester, NY",
                             "Salt Lake City, UT (Metropolitan Statistical Area)" = "Salt Lake City, UT",
                             "San Diego-Chula Vista-Carlsbad, CA (Metropolitan Statistical Area)" = "San Diego, CA",
                             "San Francisco-Oakland-Berkeley, CA (Metropolitan Statistical Area)" = "San Francisco, CA",
                             "Seattle-Tacoma-Bellevue, WA (Metropolitan Statistical Area)" = "Seattle, WA"))
  
 