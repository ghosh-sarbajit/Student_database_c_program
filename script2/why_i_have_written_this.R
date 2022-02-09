# file_reading

library(readxl)
#check the no. of worksheets your Excel Workbook has
excel_sheets("./data/AGE_MARCH.xlsx")


#import a specific worksheet either by referencing the sheet's name or its index (number) 
sheet2<-read_excel("./data/AGE_MARCH.xlsx", sheet="Summary", col_names = FALSE)

sheet1<-read_excel("./data/AGE_MARCH.xlsx", sheet="Sasmita  Parida")

excel_sheets("./data/AGE_MARCH.xlsx")


excel_sheets("./data/Polygon_VRICON Part 2_09_May_2020.xlsx")


table <- read_excel("E:/iMerit!/MyWorks/data/Minerva/2D_SEG_6(April)/Ratnakar Bari.csv")

table1 <- read.csv(file.choose(),header = T)



