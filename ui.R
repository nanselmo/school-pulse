library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(rCharts)
library(rjson)


roster<-read.csv("roster.csv")%>%
  select(StudentHomeroom=HR)
hrs<-filter(roster,!grepl("M$", StudentHomeroom))
hrs<-unique(roster[c("StudentHomeroom")])
hrs<-as.list(levels(hrs$StudentHomeroom))
hrs<-append(hrs, "Whole School")

# Define the overall UI
shinyUI(fluidPage(
  fluidRow(
    column(3,
           selectInput(inputId="HR_number", label="Your HR:", 
                       choices=hrs)
           )
  ),
  mainPanel(tabsetPanel(type = "tabs", 
                        tabPanel("Whole School and HR Data", chartOutput('mytable', 'datatables')), 
                        tabPanel("On-Track Flags", chartOutput('onTracktable', 'datatables'))))

))
