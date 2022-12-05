library('tidyverse')

#2000 data
MSA00 <- read.csv("C:/Users/Owner/Documents/ECON 485/Project on Crime/MSA Data/MSA00.csv")

msa00 <- MSA00 %>%
  #keep columns by index
  select(Table.6, X, X.4) %>%
  #Rename Columns
  rename(MSA = Table.6) %>% 
  rename(pop = X) %>%
  rename(PropCrime = X.4) %>%
  #Remove rows that do not contain partial string "Rate" or "MSA"
  filter(str_detect(MSA, "M.S.A|Rate")) %>%
  #Shift Rate per 100,000 up one row to create a new prop crime variable
  mutate(pcrime = lead(PropCrime)) %>%
  #Find the MSAs I want
  filter(str_detect(MSA, 'Los Angeles-Long Beach, CA|
                    |New York, NY|
                    |Boston, MA-NH|
                    |Phoenix-Mesa, AZ|
                    |San Diego, CA|
                    |Baltimore, MD M.S.A.|
                    |San Francisco, CA|
                    |Seattle-Bellevue-Everett, WA|
                    |Miami, FL|
                    |Fort Worth-Arlington, TX|
                    |Orlando, FL|
                    |Las Vegas, NV-AZ|
                    |Salt Lake City-Ogden, UT|
                    |New Orleans, LA|
                    |Buffalo-Niagara Falls, NY|
                    |Austin-San Marcos, TX|
                    |Rochester, NY|
                    |Jacksonville, FL|
                    |Oklahoma City, OK|
                    |Pittsburgh, PA')) %>%
  #Rename all the MSA values to a more common reference
  mutate(msa = recode_factor(MSA, 
                             "Austin-San Marcos, TX M.S.A.4" = "Austin, TX",
                             "Baltimore, MD M.S.A.4" = "Baltimore, MD",
                             "Boston, MA-NH M.S.A." = "Boston, MA",
                             "Buffalo-Niagara Falls, NY M.S.A." = "Buffalo, NY",
                             "Fort Worth-Arlington, TX M.S.A." = "Fort Worth, TX",
                             "Jacksonville, FL  M.S.A." = "Jacksonville, FL",
                             "Las Vegas, NV-AZ M.S.A." = "Las Vegas, NV",
                             "Los Angeles-Long Beach, CA M.S.A." = "Los Angeles, CA",
                             "Miami, FL M.S.A." = "Miami, FL",
                             "New Orleans, LA M.S.A." = "New Orleans, LA",
                             "New York, NY M.S.A." = "New York, NY",
                             "Oklahoma City, OK M.S.A." = "Oklahoma City, OK",
                             "Orlando, FL M.S.A." = "Orlando, FL",
                             "Phoenix-Mesa, AZ M.S.A." = "Phonenix, AZ",
                             "Pittsburgh, PA M.S.A." = "Pittsburgh, PA",
                             "Rochester, NY M.S.A." = "Rochester, NY",
                             "Salt Lake City-Ogden, UT M.S.A." = "Salt Lake City, UT",
                             "San Diego, CA M.S.A." = "San Diego, CA",
                             "San Francisco, CA M.S.A." = "San Francisco, CA",
                             "Seattle-Bellevue-Everett, WA M.S.A." = "Seattle, WA")) %>%
  select(msa, pcrime, pop) %>%
  add_column(year = 2000)
write_csv(msa00,"C:\\Users\\Owner\\Documents\\ECON 485\\Project on Crime\\MSA Data\\msa00cleaned.csv")

#2001 Data (There will be no San Fran Data)
MSA01 <- read.csv("C:/Users/Owner/Documents/ECON 485/Project on Crime/MSA Data/MSA01.csv")

msa01 <- MSA01 %>%
  #keep columns by index
  select(Table.6, X, X.4) %>%
  #Rename Columns
  rename(MSA = Table.6) %>% 
  rename(pop = X) %>%
  rename(PropCrime = X.4) %>%
  #Remove rows that do not contain partial string "Rate" or "MSA"
  filter(str_detect(MSA, "M.S.A|Rate")) %>%
  #Shift Rate per 100,000 up one row to create a new prop crime variable
  mutate(pcrime = lead(PropCrime)) %>%
  #Find the MSAs I want
  filter(str_detect(MSA, 'Los Angeles-Long Beach, CA|
                    |New York, NY|
                    |Boston, MA-NH|
                    |Phoenix-Mesa, AZ|
                    |San Diego, CA|
                    |Baltimore, MD M.S.A.|
                    |San Francisco, CA|
                    |Seattle-Bellevue-Everett, WA|
                    |Miami, FL|
                    |Fort Worth-Arlington, TX|
                    |Orlando, FL|
                    |Las Vegas, NV-AZ|
                    |Salt Lake City-Ogden, UT|
                    |New Orleans, LA|
                    |Buffalo-Niagara Falls, NY|
                    |Austin-San Marcos, TX|
                    |Rochester, NY|
                    |Jacksonville, FL|
                    |Oklahoma City, OK|
                    |Pittsburgh, PA')) %>%
  #Rename all the MSA values to a more common reference
  mutate(msa = recode_factor(MSA, 
                             "Austin-San Marcos, TX M.S.A.4" = "Austin, TX",
                             "Baltimore, MD M.S.A.4" = "Baltimore, MD",
                             "Boston, MA-NH M.S.A." = "Boston, MA",
                             "Buffalo-Niagara Falls, NY M.S.A." = "Buffalo, NY",
                             "Fort Worth-Arlington, TX M.S.A." = "Fort Worth, TX",
                             "Jacksonville, FL  M.S.A." = "Jacksonville, FL",
                             "Las Vegas, NV-AZ M.S.A." = "Las Vegas, NV",
                             "Los Angeles-Long Beach, CA M.S.A." = "Los Angeles, CA",
                             "Miami, FL M.S.A." = "Miami, FL",
                             "New Orleans, LA M.S.A." = "New Orleans, LA",
                             "New York, NY M.S.A." = "New York, NY",
                             "Oklahoma City, OK M.S.A." = "Oklahoma City, OK",
                             "Orlando, FL M.S.A." = "Orlando, FL",
                             "Phoenix-Mesa, AZ M.S.A." = "Phonenix, AZ",
                             "Pittsburgh, PA M.S.A." = "Pittsburgh, PA",
                             "Rochester, NY M.S.A." = "Rochester, NY",
                             "Salt Lake City-Ogden, UT M.S.A." = "Salt Lake City, UT",
                             "San Diego, CA M.S.A." = "San Diego, CA",
                             "San Francisco, CA M.S.A." = "San Francisco, CA",
                             "Seattle-Bellevue-Everett, WA M.S.A." = "Seattle, WA")) %>%
  select(msa, pcrime, pop) %>%
  add_column(year = 2001)
write_csv(msa01,"C:\\Users\\Owner\\Documents\\ECON 485\\Project on Crime\\MSA Data\\msa01cleaned.csv")

#2003
MSA02 <- read.csv("C:/Users/Owner/Documents/ECON 485/Project on Crime/MSA Data/MSA02.csv")

msa02 <- MSA02 %>%
  #keep columns by index
  select(Table.6, X, X.4) %>%
  #Rename Columns
  rename(MSA = Table.6) %>% 
  rename(pop = X) %>%
  rename(PropCrime = X.4) %>%
  #Remove rows that do not contain partial string "Rate" or "MSA"
  filter(str_detect(MSA, "M.S.A|Rate")) %>%
  #Shift Rate per 100,000 up one row to create a new prop crime variable
  mutate(pcrime = lead(PropCrime)) %>%
  #Find the MSAs I want
  filter(str_detect(MSA, 'Los Angeles-Long Beach, CA|
                    |New York, NY|
                    |Boston, MA-NH|
                    |Phoenix-Mesa, AZ|
                    |San Diego, CA|
                    |Baltimore, MD M.S.A.|
                    |San Francisco, CA|
                    |Seattle-Bellevue-Everett, WA|
                    |Miami, FL|
                    |Fort Worth-Arlington, TX|
                    |Orlando, FL|
                    |Las Vegas, NV-AZ|
                    |Salt Lake City-Ogden, UT|
                    |New Orleans, LA|
                    |Buffalo-Niagara Falls, NY|
                    |Austin-San Marcos, TX|
                    |Rochester, NY|
                    |Jacksonville, FL|
                    |Oklahoma City, OK|
                    |Pittsburgh, PA')) %>%
  #Rename all the MSA values to a more common reference
  mutate(msa = recode_factor(MSA, 
                             "Austin-San Marcos, TX M.S.A." = "Austin, TX",
                             "Baltimore, MD M.S.A." = "Baltimore, MD",
                             "Boston, MA-NH M.S.A." = "Boston, MA",
                             "Buffalo-Niagara Falls, NY M.S.A." = "Buffalo, NY",
                             "Fort Worth-Arlington, TX M.S.A." = "Fort Worth, TX",
                             "Jacksonville, FL M.S.A." = "Jacksonville, FL",
                             "Las Vegas, NV-AZ M.S.A.3" = "Las Vegas, NV",
                             "Los Angeles-Long Beach, CA M.S.A." = "Los Angeles, CA",
                             "Miami, FL M.S.A." = "Miami, FL",
                             "New Orleans, LA M.S.A." = "New Orleans, LA",
                             "New York, NY M.S.A." = "New York, NY",
                             "Oklahoma City, OK M.S.A." = "Oklahoma City, OK",
                             "Orlando, FL M.S.A." = "Orlando, FL",
                             "Phoenix-Mesa, AZ M.S.A.3" = "Phonenix, AZ",
                             "Pittsburgh, PA M.S.A." = "Pittsburgh, PA",
                             "Rochester, NY M.S.A." = "Rochester, NY",
                             "Salt Lake City-Ogden, UT M.S.A." = "Salt Lake City, UT",
                             "San Diego, CA M.S.A." = "San Diego, CA",
                             "San Francisco, CA M.S.A." = "San Francisco, CA",
                             "Seattle-Bellevue-Everett, WA M.S.A." = "Seattle, WA")) %>%
  select(msa, pcrime, pop) %>%
  add_column(year = 2002)
write_csv(msa02,"C:\\Users\\Owner\\Documents\\ECON 485\\Project on Crime\\MSA Data\\msa02cleaned.csv")

#2003
MSA03 <- read.csv("C:/Users/Owner/Documents/ECON 485/Project on Crime/MSA Data/MSA03.csv")

msa03 <- MSA03 %>%
  #keep columns by index
  select(Table.6, X, X.6) %>%
  #Rename Columns
  rename(MSA = Table.6) %>% 
  rename(pop = X) %>%
  rename(PropCrime = X.6) %>%
  #Remove rows that do not contain partial string "Rate" or "MSA"
  filter(str_detect(MSA, "M.S.A|Rate")) %>%
  #Shift Rate per 100,000 up one row to create a new prop crime variable
  mutate(pcrime = lead(PropCrime)) %>%
  #Find the MSAs I want
  filter(str_detect(MSA, 'Los Angeles-Long Beach-Santa Ana, CA M.S.A.|
                    |New York-Northern New Jersey-Long Island, NY-NJ-PA M.S.A.|
                    |Boston-Cambridge-Quincy, MA-NH M.S.A.|
                    |Phoenix-Mesa-Scottsdale, AZ M.S.A.1|
                    |San Diego-Carlsbad-San Marcos, CA M.S.A.|
                    |Baltimore-Towson, MD M.S.A.|
                    |San Francisco-Oakland-Fremont, CA M.S.A.|
                    |Seattle-Tacoma-Bellevue, WA M.S.A|
                    |Miami-Fort Lauderdale-Miami Beach, FL M.S.A.|
                    |Orlando, FL|
                    |Las Vegas-Paradise, NV M.S.A.|
                    |Salt Lake City, UT M.S.A.|
                    |New Orleans-Metairie-Kenner, LA M.S.A.|
                    |Buffalo-Niagara Falls, NY|
                    |Austin-San Marcos, TX|
                    |Rochester, NY M.S.A.|
                    |Jacksonville, FL|
                    |Oklahoma City, OK|
                    |Pittsburgh, PA')) %>%
  #Rename all the MSA values to a more common reference
  mutate(msa = recode_factor(MSA, 
                             "Austin-San Marcos, TX M.S.A." = "Austin, TX",
                             "Baltimore-Towson, MD M.S.A." = "Baltimore, MD",
                             "Boston-Cambridge-Quincy, MA-NH M.S.A." = "Boston, MA",
                             "Buffalo-Niagara Falls, NY M.S.A." = "Buffalo, NY",
                             "Fort Worth-Arlington, TX M.S.A." = "Fort Worth, TX",
                             "Jacksonville, FL M.S.A." = "Jacksonville, FL",
                             "Las Vegas-Paradise, NV M.S.A." = "Las Vegas, NV",
                             "Los Angeles-Long Beach-Santa Ana, CA M.S.A." = "Los Angeles, CA",
                             "Miami-Fort Lauderdale-Miami Beach, FL M.S.A." = "Miami, FL",
                             "New Orleans-Metairie-Kenner, LA M.S.A." = "New Orleans, LA",
                             "New York-Northern New Jersey-Long Island, NY-NJ-PA M.S.A." = "New York, NY",
                             "Oklahoma City, OK M.S.A." = "Oklahoma City, OK",
                             "Orlando, FL M.S.A." = "Orlando, FL",
                             "Phoenix-Mesa-Scottsdale, AZ M.S.A.1" = "Phonenix, AZ",
                             "Pittsburgh, PA M.S.A." = "Pittsburgh, PA",
                             "Rochester, NY M.S.A." = "Rochester, NY",
                             "Salt Lake City, UT M.S.A." = "Salt Lake City, UT",
                             "San Diego-Carlsbad-San Marcos, CA M.S.A." = "San Diego, CA",
                             "San Francisco-Oakland-Fremont, CA M.S.A." = "San Francisco, CA",
                             "Seattle-Tacoma-Bellevue, WA M.S.A." = "Seattle, WA")) %>%
  select(msa, pcrime, pop) %>%
  add_column(year = 2003)
write_csv(msa03,"C:\\Users\\Owner\\Documents\\ECON 485\\Project on Crime\\MSA Data\\msa03cleaned.csv")


#2004
MSA04 <- read.csv("C:/Users/Owner/Documents/ECON 485/Project on Crime/MSA Data/MSA04.csv")

msa04 <- MSA04 %>%
  #keep columns by index
  select(Table.6, X, X.6) %>%
  #Rename Columns
  rename(MSA = Table.6) %>% 
  rename(pop = X) %>%
  rename(PropCrime = X.6) %>%
  #Remove rows that do not contain partial string "Rate" or "MSA"
  filter(str_detect(MSA, "M.S.A|Rate")) %>%
  #Shift Rate per 100,000 up one row to create a new prop crime variable
  mutate(pcrime = lead(PropCrime)) %>%
  #Find the MSAs I want
  filter(str_detect(MSA, 'Los Angeles-Long Beach-Santa Ana, CA M.S.A.|
                    |New York-Northern New Jersey-Long Island, NY-NJ-PA M.S.A.|
                    |Boston-Cambridge-Quincy, MA-NH M.S.A.|
                    |Phoenix-Mesa-Scottsdale, AZ M.S.A.|
                    |San Diego-Carlsbad-San Marcos, CA M.S.A.|
                    |Baltimore-Towson, MD M.S.A.|
                    |San Francisco-Oakland-Fremont, CA M.S.A.|
                    |Seattle-Tacoma-Bellevue, WA M.S.A|
                    |Miami-Fort Lauderdale-Miami Beach, FL M.S.A.|
                    |Orlando-Kissimmee, FL M.S.A|
                    |Las Vegas-Paradise, NV M.S.A.|
                    |Salt Lake City, UT M.S.A.|
                    |New Orleans-Metairie-Kenner, LA M.S.A.|
                    |Buffalo-Niagara Falls, NY|
                    |Austin-San Marcos, TX|
                    |Rochester, NY M.S.A.|
                    |Jacksonville, FL|
                    |Oklahoma City, OK|
                    |Pittsburgh, PA')) %>%
  #Rename all the MSA values to a more common reference
  mutate(msa = recode_factor(MSA, 
                             "Austin-San Marcos, TX M.S.A." = "Austin, TX",
                             "Baltimore-Towson, MD M.S.A." = "Baltimore, MD",
                             "Boston-Cambridge-Quincy, MA-NH M.S.A." = "Boston, MA",
                             "Buffalo-Niagara Falls, NY M.S.A." = "Buffalo, NY",
                             "Fort Worth-Arlington, TX M.S.A." = "Fort Worth, TX",
                             "Jacksonville, FL M.S.A." = "Jacksonville, FL",
                             "Las Vegas-Paradise, NV M.S.A." = "Las Vegas, NV",
                             "Los Angeles-Long Beach-Santa Ana, CA M.S.A." = "Los Angeles, CA",
                             "Miami-Fort Lauderdale-Miami Beach, FL M.S.A." = "Miami, FL",
                             "New Orleans-Metairie-Kenner, LA M.S.A." = "New Orleans, LA",
                             "New York-Northern New Jersey-Long Island, NY-NJ-PA M.S.A." = "New York, NY",
                             "Oklahoma City, OK M.S.A." = "Oklahoma City, OK",
                             "Orlando-Kissimmee, FL M.S.A" = "Orlando, FL",
                             "Phoenix-Mesa-Scottsdale, AZ M.S.A." = "Phonenix, AZ",
                             "Pittsburgh, PA M.S.A." = "Pittsburgh, PA",
                             "Rochester, NY M.S.A." = "Rochester, NY",
                             "Salt Lake City, UT M.S.A." = "Salt Lake City, UT",
                             "San Diego-Carlsbad-San Marcos, CA M.S.A." = "San Diego, CA",
                             "San Francisco-Oakland-Fremont, CA M.S.A." = "San Francisco, CA",
                             "Seattle-Tacoma-Bellevue, WA M.S.A." = "Seattle, WA")) %>%
  select(msa, pcrime, pop) %>%
  add_column(year = 2004)
write_csv(msa04,"C:\\Users\\Owner\\Documents\\ECON 485\\Project on Crime\\MSA Data\\msa04cleaned.csv")

#2005
MSA05 <- read.csv("C:/Users/Owner/Documents/ECON 485/Project on Crime/MSA Data/MSA05.csv")

pcrime = MSA05$X.7[MSA05$X=="Rate per 100,000 inhabitants"]
msa05 <- MSA05 %>%
  #keep columns by index
  select(Table.6, X.1) %>%
  #Rename Columns
  rename(MSA = Table.6) %>% 
  rename(pop = X.1) %>%
  #Find MSAs
  filter(str_detect(MSA, "M.S.A|M.D.")) %>%
  add_column(pcrime = pcrime) %>%
  filter(str_detect(MSA, "Los Angeles-Long Beach-Santa Ana, CA M.S.A.|
                    |New York-Northern New Jersey-Long Island, NY-NJ-PA M.S.A.|
                    |Boston-Cambridge-Quincy, MA-NH M.S.A.|
                    |Phoenix-Mesa-Scottsdale, AZ M.S.A.|
                    |San Diego-Carlsbad-San Marcos, CA M.S.A.|
                    |Baltimore-Towson, MD M.S.A.|
                    |San Francisco-Oakland-Fremont, CA M.S.A.|
                    |Seattle-Tacoma-Bellevue, WA M.S.A|
                    |Miami-Fort Lauderdale-Miami Beach, FL M.S.A.|
                    |Orlando-Kissimmee, FL M.S.A|
                    |Las Vegas-Paradise, NV M.S.A.|
                    |Salt Lake City, UT M.S.A.|
                    |New Orleans-Metairie-Kenner, LA M.S.A.|
                    |Buffalo-Niagara Falls, NY|
                    |Austin-Round Rock, TX M.S.A|
                    |Rochester, NY M.S.A.|
                    |Jacksonville, FL|
                    |Oklahoma City, OK|
                    |Pittsburgh, PA")) %>%
  #Rename all the MSA values to a more common reference
  mutate(msa = recode_factor(MSA, 
                             "Austin-Round Rock, TX M.S.A." = "Austin, TX",
                             "Baltimore-Towson, MD M.S.A." = "Baltimore, MD",
                             "Boston-Cambridge-Quincy, MA-NH M.S.A." = "Boston, MA",
                             "Buffalo-Niagara Falls, NY M.S.A." = "Buffalo, NY",
                             "Fort Worth-Arlington, TX M.S.A." = "Fort Worth, TX",
                             "Jacksonville, FL M.S.A." = "Jacksonville, FL",
                             "Las Vegas-Paradise, NV M.S.A." = "Las Vegas, NV",
                             "Los Angeles-Long Beach-Santa Ana, CA M.S.A.1" = "Los Angeles, CA",
                             "Miami-Fort Lauderdale-Miami Beach, FL M.S.A." = "Miami, FL",
                             "New Orleans-Metairie-Kenner, LA M.S.A." = "New Orleans, LA",
                             "New York-Northern New Jersey-Long Island, NY-NJ-PA M.S.A." = "New York, NY",
                             "Oklahoma City, OK M.S.A." = "Oklahoma City, OK",
                             "Orlando-Kissimmee, FL M.S.A." = "Orlando, FL",
                             "Phoenix-Mesa-Scottsdale, AZ M.S.A." = "Phonenix, AZ",
                             "Pittsburgh, PA M.S.A." = "Pittsburgh, PA",
                             "Rochester, NY M.S.A." = "Rochester, NY",
                             "Salt Lake City, UT M.S.A." = "Salt Lake City, UT",
                             "San Diego-Carlsbad-San Marcos, CA M.S.A." = "San Diego, CA",
                             "San Francisco-Oakland-Fremont, CA M.S.A." = "San Francisco, CA",
                             "Seattle-Tacoma-Bellevue, WA M.S.A." = "Seattle, WA")) %>%
  select(msa, pcrime, pop) %>%
  add_column(year = 2005) 
write_csv(msa05,"C:\\Users\\Owner\\Documents\\ECON 485\\Project on Crime\\MSA Data\\msa05cleaned.csv")



