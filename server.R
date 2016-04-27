library(readxl)
library(ggplot2)
library(dplyr)
library(reshape2) 

source("weekly_report.R")

# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  ##get the HR data using Shiny's reactivity
  hrData <- reactive({
    if(input$HR_number!="Whole School"){
    hr_data = this_week_app%>%filter(StudentHomeroom==input$HR_number)%>%select(-StudentHomeroom, -StudentGradeLevel)
    }else{
      hr_data = this_week_app%>%select(-StudentHomeroom, -StudentGradeLevel)
    }
    if(substr(input$HR_number,1,1)=="B"){
      hr_data<-hr_data%>%select(-Level, -MonthlyUnits, - MonthlyMins, -Needs.Instruction, - LexiaEffic)}
    if(substr(input$HR_number,1,1)=="A"){
      hr_data<-hr_data%>%select(-NELA.Rct.Quiz, -NELA.Lexile.Level)}
    if(substr(input$HR_number,2,2)=="1"){
      hr_data<-hr_data%>%select(-Read, -Math, -Sci, -SS, -GPA, -gpaDiff)}
    return(hr_data)
    
  })
 
  # Generate an interactive table view of the data using dTable
  output$mytable <- renderChart2({
    dTable(hrData())
  })
  
  output$onTracktable <- renderChart2({
    dTable(admin_data)
  })
  
})