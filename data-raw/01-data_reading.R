library(tidyverse)
library(pdftools)
library(openxlsx)
library(devtools)

## 2002 ####
file02 <- paste("~//wnvstormdata//data-raw//wnv_source//WNVsummary2002.pdf")

txt <- pdf_text(file02)

# get the working directory
wd <- getwd()

#write the file to the working directory
file_name <- paste0(wd, "/data-raw/", "temp.txt")
write(txt, file = file_name, sep = "\t")

fixed2 <- read.fwf(file_name, widths = c(15, 7, 3, 3,3,3,4,4,4,4,
                                         4,4,4,4,4,4,4,4,4,4,4,4,4,
                                         4,4,4,4,4,4,4,4), skip = 93, n = 42)

fixed2 <- fixed2 %>%
  select(-c(V2,V6)) %>%
  janitor::row_to_names(row_number = 1)

data02 <- fixed2 %>%
  pivot_longer(` JA`:NO2, names_to = "Week", values_to = "Cases")%>%
  mutate(Week = paste0(Week," 2002")) %>%
  rename(Parish = 1)


## 2003 ####
file03 <- paste('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//WNVsummary2003.pdf')


txt <- pdf_text(file03)

# get the working directory
wd <- getwd()

#write the file to the working directory
file_name <- paste0(wd, "/data-raw/", "temp.txt")
write(txt, file = file_name, sep = "\t")

fixed3 <- read.fwf(file_name, widths = c(16, 7, 3, 3,3,3,3,4,4,4,
                                         4,4,4,4,4,4,4,4,4,4,4,4,4,
                                         4,4,4,4,4,4,4,4,4,4,4,4,4,4), skip = 89, n = 53)

fixed3 <- fixed3 %>%
  select(-V2) %>%
  janitor::row_to_names(row_number = 1) %>%
  slice(-c(25))

fixed3[25,1] = "5 Calcasieu"
#data03 <- fixed3
data03 <- fixed3 %>%
  pivot_longer(`JA `:DE4, names_to = "Week", values_to = "Cases")%>%
  mutate(Week = paste0(Week," 2003")) %>%
  rename(Parish = 1)

## 2004 ####
file04 <- paste('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//EncephalitisWNVAnnualSummary2004.pdf')

txt <- pdf_text(file04)

# get the working directory
wd <- getwd()

#write the file to the working directory
file_name <- paste0(wd, "/data-raw/", "temp.txt")
write(txt, file = file_name, sep = "\t")

fixed4 <- read.fwf(file_name, widths = c(16, 13, 6, 5,5,5,5,4,4,5,
                                         6,5,5,5,5,6,4,5,4,4,4,9,9,
                                         6,9,6,7,7,7,6,5,8,4,6,4,4,4), skip = 107, n = 72)

data04 <- fixed4 %>%
  select(-V2) %>%
  slice(-2) %>%
  mutate(across(everything(), as.character))%>%
  mutate(across(everything(), str_squish))%>%
  janitor::row_to_names(row_number = 1) %>%
  rename(Parish = 1 ) %>%
  pivot_longer(`1-5`:`52`, names_to = "Week", values_to = "Cases") %>%
  mutate(Week = paste0(Week," 2004")) %>%
  filter(!str_detect(Parish, "V illi") & !is.na(Parish) & str_detect(Parish, "[:alpha:]"))

## 2005 ####

file05 <- paste('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//EncephalitisWNVAnnualSummary2005a.pdf')

txt <- pdf_text(file05)

# get the working directory
wd <- getwd()

#write the file to the working directory
file_name <- paste0(wd, "/data-raw/", "temp.txt")
write(txt, file = file_name, sep = "\t")

fixed5 <- read.fwf(file_name, widths = c(16, 13, 6, 5,4,5,5,4,4,5, #10
                                         6,5,5,5,6,8,6,3,7,6,4,5,3, #23
                                         6,9,6,7,9,8,6,5,6,4,4,4,4,4), skip = 110, n = 71)

data05 <- fixed5 %>%
  select(-V2) %>%
  slice(-2) %>%
  mutate(across(everything(), as.character))%>%
  mutate(across(everything(), str_squish))%>%
  janitor::row_to_names(row_number = 1) %>%
  rename(Parish = 1 )
data05$`32`[6]=2
data05$`33`[6] =2

data05 <- data05%>%
  pivot_longer(`1-5`:`51`, names_to = "Week", values_to = "Cases") %>%
  filter(str_detect(`Parish`, pattern = "[:alpha:]")==TRUE) %>%
  mutate(Week = paste0(Week," 2005"))


## 2006 ####

file06 <- paste('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//WNV2006Report.pdf')

txt <- pdf_text(file06)

# get the working directory
wd <- getwd()

#write the file to the working directory
file_name <- paste0(wd, "/data-raw/", "temp.txt")
write(txt, file = file_name, sep = "\t")

fixed6 <- read.fwf(file_name, widths = c(23, 20, 6, 8,6,6,6,4,4,5, #10
                                         6,5,5,8,6,8,3,3,8,9,10,9,9, #23
                                         10,9,10,9,9,8,6,5,6,4,7,4,6,4), skip = 147, n = 71)

data06 <- fixed6 %>%
  select(-V2) %>%
  slice(-2, -(51:55)) %>%
  mutate(across(everything(), as.character))%>%
  mutate(across(everything(), str_squish))%>%
  janitor::row_to_names(row_number = 1) %>%
  rename(Parish = 1 ) %>%
  pivot_longer(`6-9`:`52`, names_to = "Week", values_to = "Cases") %>%
  mutate(Week = paste0(Week," 2006"))

## 2007 ####

file07 <- paste('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//Final2007.pdf')

txt <- pdf_text(file07)

# get the working directory
wd <- getwd()

#write the file to the working directory
file_name <- paste0(wd, "/data-raw/", "temp.txt")
write(txt, file = file_name, sep = "\t")

fixed7 <- read.fwf(file_name, widths = c(23, 25, 6, 8,4,3,3,3,3,3, #10
                                         3,3,6,3,4,3,3,3,4,3,3,3,3, #23
                                         9,3,3,3,3,4,4,3,4,4,3,4), skip = 165, n = 71)

data07 <- fixed7 %>%
  select(-V2) %>%
  slice(-2) %>%
  mutate(across(everything(), as.character))%>%
  mutate(across(everything(), str_squish))%>%
  janitor::row_to_names(row_number = 1) %>%
  rename(Parish = 1 ) %>%
  pivot_longer(`14-17`:`52`, names_to = "Week", values_to = "Cases") %>%
  mutate(Week = paste0(Week," 2007"))

data07 <- data07 %>%
  filter(str_detect(`Parish`, pattern = "[:alpha:]")==TRUE) %>%
  filter(!str_detect(Parish, "Region Parish"))

## 2008 ####


file08 <- paste('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//WNV2008ReportFinal.pdf')

txt <- pdf_text(file08)

# get the working directory
wd <- getwd()

#write the file to the working directory
file_name <- paste0(wd, "/data-raw/", "temp.txt")
write(txt, file = file_name, sep = "\t")

fixed8_1 <- read.fwf(file_name, widths = c(23, 14, 4, 4,5,6,11,6,3,6, #10
                                           6,4,6,5,6,6,5,5,5,5,5,5,5, #23
                                           9,3,5,5,5,5,6,6,4,5,5,5,5,5,5), skip = 274, n = 13) %>%
  mutate(across(everything(), as.character))

fixed8_2 <- read.fwf(file_name, widths = c(23, 22, 3, 4,5,6,7,3,8,11, #10
                                           6,5,6,5,6,6,5,5,5,8,8,8,5, #23
                                           9,9,8,8,8,8,8,8,8,5,3,5,5,5,5), skip = 318, n = 13) %>%
  mutate(across(everything(), as.character))

fixed8_3 <- read.fwf(file_name, widths = c(23, 27, 3, 6,6,25,6,6,8,8, #10
                                           6,5,6,5,8,8,8,8,8,8,8,8,9, #23
                                           10,9,9,9,9,9,9,10,8,6,3,8,5,5), skip = 364, n = 8) %>%
  mutate(across(everything(), as.character))

data08 <- bind_rows(fixed8_1,fixed8_2,fixed8_3) %>%
  select(-V2) %>%
  slice(-c(2,14,15,27,28)) %>%
  mutate(across(everything(), str_squish))%>%
  janitor::row_to_names(row_number = 1) %>%
  rename(Parish = 1 ) %>%
  pivot_longer(`1-5`:`52`, names_to = "Week", values_to = "Cases") %>%
  mutate(Week = paste0(Week," 2008"),
         Cases = as.numeric(str_extract(Cases, "[:digit:]"))) %>%
  group_by(Parish, Week) %>%
  summarize(Cases = sum(Cases, na.rm = TRUE))%>%
  mutate(Cases = as.character(Cases))

# Cases = as.numeric(str_extract(Cases, "[:digit:]{1,2}"))

#pivot longer, group by weeks, then sum of cases

# data08 <- data08 %>%
#   filter(str_detect(`Rg Parish`, pattern = "[:alpha:]")==TRUE)

## 2009 ####

file09 <- paste('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//WNV2009ReportFinal1.pdf')

txt <- pdf_text(file09)

# get the working directory
wd <- getwd()

#write the file to the working directory
file_name <- paste0(wd, "/data-raw/", "temp.txt")
write(txt, file = file_name, sep = "\t")

fixed9_1 <- read.fwf(file_name, widths = c(23, 15, 4, 4,6,6,9,3,3,6, #10
                                           6,4,6,5,5,5,5,3,5,5,5,5,5, #23
                                           5,5,5,5,5,5,5,5,5,5,3,5,5,5,5), skip = 266, n = 9) %>%
  mutate(across(everything(), as.character))

fixed9_2 <- read.fwf(file_name, widths = c(23, 22, 4, 4,6,6,6,3,11,6, #10
                                           6,5,6,6,6,6,5,4,7,7,6,6,8, #23
                                           8,10,8,8,8,8,8,8,7,5,3,5,5,5,5), skip = 306, n = 7) %>%
  mutate(across(everything(), as.character))

fixed9_3 <- read.fwf(file_name, widths = c(23, 24, 3, 4,6,6,6,4,8,8, #10
                                           6,5,6,8,8,8,8,5,8,8,8,8,9, #23
                                           10,9,9,9,9,9,9,10,8,6,5,8,6,5,5), skip = 343, n = 9) %>%
  mutate(across(everything(), as.character))

data09 <- bind_rows(fixed9_1,fixed9_2,fixed9_3) %>%
  select(-V2) %>%
  slice(-c(2,10,11,17,18)) %>%
  mutate(across(everything(), str_squish))%>%
  janitor::row_to_names(row_number = 1) %>%
  rename(Parish = 1 ) %>%
  pivot_longer(`1-5`:`52`, names_to = "Week", values_to = "Cases") %>%
  mutate(Week = paste0(Week," 2009"),
         Cases = as.numeric(str_extract(Cases, "[:digit:]"))) %>%
  group_by(Parish, Week) %>%
  summarize(Cases = sum(Cases, na.rm = TRUE))%>%
  mutate(Cases = as.character(Cases))

## 2010 ####
file10 <- paste('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//WNV2010ReportFinal.pdf')

txt <- pdf_text(file10)

# get the working directory
wd <- getwd()

#write the file to the working directory
file_name <- paste0(wd, "/data-raw/", "temp.txt")
write(txt, file = file_name, sep = "\t")

fixed10_1 <- read.fwf(file_name, widths = c(23, 20, 15, 6,6,15,9,6,5,6, #10
                                            6,4,6,5,5,8,8,8,8,8,8,9,9, #23
                                            9,9,10,10,9,9,9,12,7,6,5,7,5,5), skip = 157, n = 22) %>%
  mutate(across(everything(), as.character))


data10 <- fixed10_1 %>%
  select(-V2) %>%
  slice(-c(2,11,12,13,17,18,19,22)) %>%
  mutate(across(everything(), str_squish))%>%
  janitor::row_to_names(row_number = 1) %>%
  rename(Parish = 1 ) %>%
  pivot_longer(`6-9`:`52`, names_to = "Week", values_to = "Cases") %>%
  mutate(Week = paste0(Week," 2010"),
         Cases = as.numeric(str_extract(Cases, "[:digit:]"))) %>%
  group_by(Parish, Week) %>%
  summarize(Cases = sum(Cases, na.rm = TRUE))%>%
  mutate(Cases = as.character(Cases))

## 2011 ####
file11 <- paste('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//WNV2011ReportFinal.pdf')

txt <- pdf_text(file11)

# get the working directory
wd <- getwd()

#write the file to the working directory
file_name <- paste0(wd, "/data-raw/", "temp.txt")
write(txt, file = file_name, sep = "\t")

fixed11 <- read.fwf(file_name, widths = c(23, 30, 9, 6,6,15,9,6,5,6, #10
                                          6,4,6,8,5,9,7,8,8,8,8,9,9, #23
                                          9,9,10,10,9,9,9,12,6,6,5,7,5,5), skip = 148, n = 22) %>%
  mutate(across(everything(), as.character))


data11 <- fixed11 %>%
  select(-V2) %>%
  slice(-c(2:6,14,12,13,18,19,20)) %>%
  mutate(across(everything(), str_squish))%>%
  janitor::row_to_names(row_number = 1) %>%
  rename(Parish = 1 ) %>%
  pivot_longer(`6-9`:`52`, names_to = "Week", values_to = "Cases") %>%
  mutate(Week = paste0(Week," 2011"),
         Cases = as.numeric(str_extract(Cases, "[:digit:]"))) %>%
  group_by(Parish, Week) %>%
  summarize(Cases = sum(Cases, na.rm = TRUE))%>%
  mutate(Cases = as.character(Cases))

## 2012 ####
file12 <- paste('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//WNVSummary12wk49.pdf')

txt <- pdf_text(file12)

# get the working directory
wd <- getwd()

#write the file to the working directory
file_name <- paste0(wd, "/data-raw/", "temp.txt")
write(txt, file = file_name, sep = "\t")

fixed12 <- read.fwf(file_name, widths = c(23, 30, 3, 6,6,8,5,6,5,5, #10
                                          6,4,6,5,5,5,4,5,5,8,5,5,5, #23
                                          5,5,5,5,5,5,5,7,6,6,5,7,5,5), skip = 155, n = 118) %>%
  mutate(across(everything(), as.character))


data12 <- fixed12 %>%
  select(-V2) %>%
  slice(-c(2,44:47,62:64,91,92)) %>%
  mutate(across(everything(), str_squish))%>%
  janitor::row_to_names(row_number = 1) %>%
  rename(Parish = 1 ) %>%
  pivot_longer(`6-9`:`52`, names_to = "Week", values_to = "Cases") %>%
  mutate(Week = paste0(Week," 2012"),
         Cases = as.numeric(str_extract(Cases, "[:digit:]"))) %>%
  group_by(Parish, Week) %>%
  summarize(Cases = sum(Cases, na.rm = TRUE))%>%
  mutate(Cases = as.character(Cases)) %>%
  filter(!str_detect(Parish, "Region Parish"))


## 2013 ####
file13 <- paste('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//WNVSummary13wk35.pdf')

txt <- pdf_text(file13)

# get the working directory
wd <- getwd()

#write the file to the working directory
file_name <- paste0(wd, "/data-raw/", "temp.txt")
write(txt, file = file_name, sep = "\t")

fixed13 <- read.fwf(file_name, widths = c(30, 30, 6, 6,6,8,5,6,5,5, #10
                                          6,4,6,5,5,5,5,5,5,8,5,5,5, #23
                                          5,5,5,5,5,6,7,7,6,6,5,7,5,5), skip = 223, n = 12) %>%
  mutate(across(everything(), as.character))


data13 <- fixed13 %>%
  select(-V2) %>%
  slice(-c(2:5,9)) %>%
  mutate(across(everything(), str_squish))%>%
  janitor::row_to_names(row_number = 1) %>%
  rename(Parish = 1 ) %>%
  pivot_longer(`6-9`:`52`, names_to = "Week", values_to = "Cases") %>%
  mutate(Week = paste0(Week," 2013"),
         Cases = as.numeric(str_extract(Cases, "[:digit:]"))) %>%
  group_by(Parish, Week) %>%
  summarize(Cases = sum(Cases, na.rm = TRUE)) %>%
  mutate(Cases = as.character(Cases))

## 2014 ####
file14 <- paste('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//ARBO_1452.pdf')

txt <- pdf_text(file14)

# get the working directory
wd <- getwd()

#write the file to the working directory
file_name <- paste0(wd,"/data-raw/", "temp.txt")
write(txt, file = file_name, sep = "\t")

fixed14 <- read.fwf(file_name, widths = c(35, 14, 4, 4,6,6,7,9,6,5, #10
                                          6,4,5,5,5,6,5,5,4,6, #20
                                          6,8,5,5,5,6,5,5,5,5, #30
                                          5,7,4,6,4,5,6,4), skip = 231, n = 16)

data14 <- fixed14 %>%
  select(-V2) %>%
  slice(-2) %>%
  mutate(across(everything(), as.character))%>%
  mutate(across(everything(), str_squish))%>%
  janitor::row_to_names(row_number = 1) %>%
  rename(Parish = 1 ) %>%
  pivot_longer(`1-5`:`52`, names_to = "Week", values_to = "Cases") %>%
  mutate(Week = paste0(Week," 2014")) %>%
  filter(!str_detect(Parish, "1"))

## 2015 ####
file15 <- paste('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//ARBO_1552.pdf')

txt <- pdf_text(file15)

# get the working directory
wd <- getwd()

#write the file to the working directory
file_name <- paste0(wd, "/data-raw/", "temp.txt")
write(txt, file = file_name, sep = "\t")

fixed15 <- read.fwf(file_name, widths = c(35, 14, 4, 4,6,6,7,7,5,5, #10
                                          6,4,5,5,5,6,5,6,4,6, #20
                                          6,8,5,5,5,6,5,5,5,5, #30
                                          5,7,4,6,4,5,6,4), skip = 233, n = 20)

data15 <- fixed15 %>%
  select(-V2) %>%
  slice(-2) %>%
  mutate(across(everything(), as.character))%>%
  mutate(across(everything(), str_squish))%>%
  janitor::row_to_names(row_number = 1) %>%
  rename(Parish = 1 ) %>%
  pivot_longer(`1-5`:`52`, names_to = "Week", values_to = "Cases") %>%
  mutate(Week = paste0(Week," 2015"))

## 2016 ####
file16 <- paste('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//2016//ARBO_1650.pdf')

txt <- pdf_text(file16)

# get the working directory
wd <- getwd()

#write the file to the working directory
file_name <- paste0(wd, "/data-raw/", "temp.txt")
write(txt, file = file_name, sep = "\t")

fixed16 <- read.fwf(file_name, widths = c(35, 25, 6, 6,6,6,7,6,6,5, #10
                                         6,5,5,5,5,6,4,5,4,5, #20
                                         6,9,5,5,5,6,5,5,5,6, #30
                                         5,8,4,6,4,5,6), skip = 234, n = 18)

data16 <- fixed16 %>%
  select(-V2) %>%
  slice(-2) %>%
  mutate(across(everything(), as.character))%>%
  mutate(across(everything(), str_squish))%>%
  janitor::row_to_names(row_number = 1) %>%
  rename(Parish = 1 ) %>%
  pivot_longer(`6-9`:`52`, names_to = "Week", values_to = "Cases") %>%
  mutate(Week = paste0(Week," 2016"))


## 2017 ####
file17 <- paste('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//2017//ARBO_1744.pdf')

txt <- pdf_text(file17)

# get the working directory
wd <- getwd()

#write the file to the working directory
file_name <- paste0(wd, "/data-raw/", "temp.txt")
write(txt, file = file_name, sep = "\t")

fixed17 <- read.fwf(file_name, widths = c(35, 12, 5, 6,6,7,7,6,6,5, #10
                                          6,5,5,5,5,6,4,5,4,5, #20
                                          6,8,5,5,5,6,3,5,5,5, #30
                                          5,6,4,5,4,5,6,6), skip = 234, n = 17)

data17 <- fixed17 %>%
  select(-V2) %>%
  slice(-2) %>%
  mutate(across(everything(), as.character))%>%
  mutate(across(everything(), str_squish))%>%
  janitor::row_to_names(row_number = 1) %>%
  rename(Parish = 1 ) %>%
  pivot_longer(`1-4`:`52`, names_to = "Week", values_to = "Cases") %>%
  mutate(Week = paste0(Week," 2017"))

## 2018 ####
file18 <- paste('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//2018//ARBO_1840.pdf')

txt <- pdf_text(file18)

# get the working directory
wd <- getwd()

#write the file to the working directory
file_name <- paste0(wd, "/data-raw/", "temp.txt")
write(txt, file = file_name, sep = "\t")

fixed18 <- read.fwf(file_name, widths = c(35,25,6,7,8,7,7,6,6,5, #10
                                          6,5,5,5,5,6,4,5,5,6, #20
                                          6,8,5,5,5,6,3,5,5,5, #30
                                          5,6,4,6,5,5,6), skip = 235, n = 23)

data18 <- fixed18 %>%
  select(-V2) %>%
  slice(-2) %>%
  mutate(across(everything(), as.character))%>%
  mutate(across(everything(), str_squish))%>%
  janitor::row_to_names(row_number = 1) %>%
  rename(Parish = 1 ) %>%
  pivot_longer(`5-8`:`52`, names_to = "Week", values_to = "Cases") %>%
  mutate(Week = paste0(Week," 2018")) %>%
  filter(!str_detect(Parish, "5"))

## 2019 ####
file19 <- paste('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//2019//ARBO_1945.pdf')

txt <- pdf_text(file19)

# get the working directory
wd <- getwd()

#write the file to the working directory
file_name <- paste0(wd, "/data-raw/", "temp.txt")
write(txt, file = file_name, sep = "\t")

fixed19 <- read.fwf(file_name, widths = c(35,25,6,7,7,7,7,5,3,5, #10
                                          6,5,3,3,5,6,4,4,5,6, #20
                                          4,3,3,5,7,7,3,4,3,3, #30
                                          3,6,8,6,4,3,6), skip = 238, n = 13)

data19 <- fixed19 %>%
  select(-V2) %>%
  slice(-2) %>%
  mutate(across(everything(), as.character))%>%
  mutate(across(everything(), str_squish))%>%
  janitor::row_to_names(row_number = 1) %>%
  rename(Parish = 1 ) %>%
  pivot_longer(`5-8`:`52`, names_to = "Week", values_to = "Cases") %>%
  mutate(Week = paste0(Week," 2019")) %>%
  filter(!str_detect(Parish, "1") & !str_detect(Parish, "3") & !str_detect(Parish, "4") & !str_detect(Parish,"5"))

## 2020 ####
file20 <- paste('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//2020//ARBO_2052_rev.pdf')

txt <- pdf_text(file20)

# get the working directory
wd <- getwd()

#write the file to the working directory
file_name <- paste0(wd, "/data-raw/", "temp.txt")
write(txt, file = file_name, sep = "\t")

fixed20 <- read.fwf(file_name, widths = c(35,13,3,7,7,6,7,6,6,5, #10
                                          6,5,5,5,5,6,4,5,5,6, #20
                                          6,6,5,5,5,6,3,5,5,4, #30
                                          5,5,4,5,5,5,5,5), skip = 234, n = 12)

fixed20[10,1] = "7 Caddo"

data20 <- fixed20 %>%
  select(-V2) %>%
  slice(-2) %>%
  mutate(across(everything(), as.character))%>%
  mutate(across(everything(), str_squish))%>%
  janitor::row_to_names(row_number = 1) %>%
  rename(Parish = 1 ) %>%
  pivot_longer(`1-4`:`52`, names_to = "Week", values_to = "Cases") %>%
  mutate(Week = paste0(Week," 2020")) %>%
  filter(!str_detect(Parish, "3") & !str_detect(Parish, "4") & !str_detect(Parish, "5") & !str_detect(Parish,"6"))



## 2021 ####
raw.files21 <- tibble(filename = list.files('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//2021//'))

raw.file.paths21 <- raw.files21  %>%
  mutate(filepath = paste0('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//2021//', filename))

## 2022 ####
raw.files22 <- tibble(filename = list.files('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//2022//'))

raw.file.paths22 <- raw.files22  %>%
  mutate(filepath = paste0('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//2022//', filename))


raw.file.paths <- raw.file.paths21 %>%
  bind_rows( raw.file.paths22[1:4,])

#i = 31

data21_22 = list()

for (i in 1:nrow(raw.file.paths)){
  txt <- pdf_text(raw.file.paths$filepath[i])

  # get the working directory
  wd <- getwd()

  #write the file to the working directory
  file_name <- paste0(wd, "/data-raw/", "temp.txt")
  write(txt, file = file_name, sep = "\t")

  # Convert to a table. Data is located starting line 25, and lasts 25 lines
  # P.S: I've tried this code with and without the "sep" argument. No change.
  #test1 <- read.table(file_name, fill = TRUE, skip = 71)

  #Parish to Total
  get_rownums <- read.fwf(file_name, widths = 24)

  start <- which(str_detect(get_rownums$V1,"Table 3"))
  end <- which(str_detect(get_rownums$V1,"Total"))[3]

  fixed <- read.fwf(file_name, widths = c(24, 125, 7, 7),
                    skip = start, n = (end-start))

#  dates <- txt[1]
  #dates <- str_extract(dates, "(?<=From )[:alpha:]{4,8} [:digit:]{1,2} - [:alpha:]{4,8} [:digit:]{1,2}")

  fixed <- fixed %>%  slice(4:nrow(fixed)) %>%
    janitor::row_to_names(row_number = 1) %>%
    select(1, contains("T", ignore.case=FALSE)) %>%
    rename(Parish = 1,  Cases = 2) %>%
    mutate(Week = str_extract(raw.file.paths$filename[i], "[:digit:]{4}") )%>%
    slice(-c(nrow(fixed))) %>%
    filter(!str_detect(Parish, "Total"))

  data21_22[[i]] = fixed
}

data21_22 <- do.call(bind_rows, data21_22)


## 2022 files ####
raw.files22 <- tibble(filename = list.files('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//2022//'))

raw.file.paths22 <- raw.files22  %>%
  mutate(filepath = paste0('~//Honors Thesis//WNV_Hurricane_datapackage//wnv_data//2022//', filename))

#i= 5
data22 = list()

for (i in 5:nrow(raw.file.paths22)){
  txt <- pdf_text(raw.file.paths22$filepath[i])

  # get the working directory
  wd <- getwd()

  #write the file to the working directory
  file_name <- paste0(wd, "/data-raw/", "temp.txt")
  write(txt, file = file_name, sep = "\t")

  # Convert to a table. Data is located starting line 25, and lasts 25 lines
  # P.S: I've tried this code with and without the "sep" argument. No change.
  #test1 <- read.table(file_name, fill = TRUE, skip = 71)

  #Parish to Total
  get_rownums <- read.fwf(file_name, widths = 24)

  start <- which(str_detect(get_rownums$V1,"Table 3"))
  end <- which(str_detect(get_rownums$V1,"Total"))[3]

  fixed <- read.fwf(file_name, widths = c(24, 115,8, 8, 7),
                    skip = start, n = (end-start))

  #  dates <- txt[1]
  #dates <- str_extract(dates, "(?<=From )[:alpha:]{4,8} [:digit:]{1,2} - [:alpha:]{4,8} [:digit:]{1,2}")

  fixed <- fixed %>%  slice(4:nrow(fixed)) %>%
    janitor::row_to_names(row_number = 1) %>%
    select(1, contains("NID", ignore.case=FALSE),contains("F", ignore.case=FALSE)) %>%
    rename(Parish = 1,  Cases1 = 2, Cases2=3) %>%
    mutate(Cases = as.character(as.numeric(Cases1) + as.numeric(Cases2)),
           Week = str_extract(raw.file.paths22$filename[i], "[:digit:]{4}") )%>%
    slice(-c(nrow(fixed))) %>%
    filter(str_detect(Parish, "LDH")) %>%
    select(Parish, Week, Cases)

  data22[[i]] = fixed
}

data22 <- do.call(bind_rows, data22)

## Altogether ####

data <- data02 %>%
  bind_rows(data03, data04, data05, data06, data07, data08, data09,
            data10, data11, data12, data13, data14, data15, data16,
            data17, data18,data19, data20) %>%
  mutate(Parish = str_extract(Parish, "(?<=[:digit:]).{1,30}")) %>%
  bind_rows(data21_22, data22) %>%
  mutate(Cases = as.numeric(Cases)) %>%
  mutate_if(is.numeric, ~replace_na(Cases, 0))

save(data, file ="data-raw//data_reading.rda")
#use_this::use_data(wnv_2002_2021_storm_exposure, overwrite = TRUE)


