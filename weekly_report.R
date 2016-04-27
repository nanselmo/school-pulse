library(readxl)
library(ggplot2)
library(dplyr)
library(reshape2) 
#to generate reports
library(knitr)
library(markdown)
library(rmarkdown)


#merge function
merge.all <- function(by, ...) {
  frames <- list(...)
  return (Reduce(function(x, y) {merge(x, y, by = by, all = TRUE)}, frames))
}  # end merge.all


#Change the directory to wherever your excel files are located
#only for use locally, comment out before deploying to Shiny
#setwd("/Users/administrator/Google Drive/Chavez/Data/RStudio Analysis/Weekly-School-Pulse")

# subjects you want grades for
subjects = c("CHGO READING FRMWK","MATHEMATICS STD", "SCIENCE  STANDARDS", "SOCIAL SCIENCE STD" )

#load intervention file (a list of students with certain interventions)
intervention_students<-read_excel("intervention_roster.xlsx")

#load EL file (a list of students in the ELL Program)
el_students<-read.csv("EL-Students-04-26-16.csv", header=FALSE, 
                      col.names = c("StudentID","StudentName","StudentAge","StudentGrade","LanguageCode", "Language", "UnknownBoolean", "DisabilityCode", "ELL_Program_Year", "RPL", "WPL", "LPL", "SPL", "LitPL", "Composite", "TotalStudents"))
transition_el_students<-read.csv("EL-Transitioning-04-26-16.csv", header=FALSE, col.names = c("H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12", "H13","H14","H15","H16","H17","H18","H19","H20","H21","H22", "H23", "Homeroom",
                                                                                              "StudentID","StudentName","StudentAge","StudentGrade","LanguageCode", "Language", "UnknownBoolean", "DisabilityCode", "RPL", "WPL", "LPL", "SPL", "LitPL", "EntryDate1", "EntryDate2",  "TotalStudents", "Rubric", "ReportID", "Page", "ReportDate", "ReportTime"))
#use entry date to add a TM1 or TM2 to the student as their program year
transition_el_students$transitionTime<-(as.Date(Sys.Date())-as.Date(transition_el_students$EntryDate2, format="%m/%d/%Y"))/365
transition_el_students$ELL_Program_Year <-trunc(transition_el_students$transitionTime) +1

#only grab certain columns from EL files
el_students_slim<-el_students%>%select(StudentID, ELL_Program_Year)
transition_el_students_slim<-transition_el_students%>%select(StudentID, ELL_Program_Year)

#add PY prefix to the PY year number
el_students_slim <- transform(el_students_slim, ELL_Program_Year = sprintf('PY%d', ELL_Program_Year)) 

#add TM prefix to the transitioning students
transition_el_students_slim <- transform(transition_el_students_slim, ELL_Program_Year = sprintf('TY%d', ELL_Program_Year)) 

#create one dataframe with all el students (transitioning and in program)
all_el_students <- rbind(transition_el_students_slim, el_students_slim)


#load all of the excel files
## Grades (two weeks ago and current week)
two_weeks_grades<-read.csv("Grades-03-04-16.csv")%>%select(StudentID,SubjectName, FinalAvg)%>%filter(SubjectName %in% subjects)
this_week_grades<-read.csv("Grades-04-09-16.csv")%>%select(StudentID,SubjectName, FinalAvg)%>%filter(SubjectName %in% subjects)

## Attendance (previous and current week)
last_week_attend<-read.csv("Attendance-04-01-16.csv")%>%filter(Attendance.School=="CHAVEZ")%>%select(StudentID=Student.ID, Absences, Membership.Days)%>%mutate(Pct=round((Membership.Days-Absences)/Membership.Days, digits=2)) 
this_week_attend<-read.csv("Attendance-04-07-16.csv")%>%filter(Attendance.School=="CHAVEZ")%>%select(StudentID=Student.ID, Absences, Membership.Days)%>%mutate(Pct=round((Membership.Days-Absences)/Membership.Days, digits=2)) 

## Tardies (previous and current week)
last_week_tardies<-read.csv("Tardies-04-01-16.csv")%>%select(StudentID=ID, Tardies=Total)
this_week_tardies<-read.csv("Tardies-04-07-16.csv")%>%select(StudentID=ID, Tardies=Total)


## ST Math (two weeks ago and previos week)[ST Math isn't available til the following Monday]
last_week_jiji<-read.csv("JiJi-03-25-16.csv")%>%select(StudentID=school_student_id, JijiProgress=round(as.numeric(K_5_Progress)))
this_week_jiji<-read.csv("JiJi-04-08-16.csv")%>%select(StudentID=school_student_id, JijiProgress=round(as.numeric(K_5_Progress)))

## NEWSELA (Just need most current file (doesn't track trends yet))
this_week_newsela<-read.csv("Newsela-03-17-16.csv")%>%select(StudentID=cps_student_id, NELA.Rct.Quiz=quiz_max_percentile,NELA.Lexile.Level=avg_lexile_level)

## Lexia (previous and current week)
last_week_lexia<-read.csv("Lexia-04-01-16.csv")%>%filter(Month==4)
this_week_lexia<-read.csv("Lexia-04-07-16.csv")%>%filter(Month==4)

## From Google Form
this_week_news<-read.csv("Updates-04-07-16.csv")%>%filter(as.POSIXct(Timestamp)>=as.POSIXct("2016-04-01"))%>%rename(HR=Homerooms)

#create the roster and hr list
roster<-read.csv("Attendance-04-01-16.csv")%>%filter(Attendance.School=="CHAVEZ")%>%
  select(StudentID=Student.ID, StudentName=Student.Name, StudentHomeroom=Current.Homeroom,StudentGradeLevel=Current.Grade.Level)
hrs<-unique(roster[c("StudentHomeroom")])
hrs<-as.list(levels(hrs$StudentHomeroom))

#compare tardies
all_tardies<-merge(by="StudentID",last_week_tardies, this_week_tardies)
all_tardies$tardyDiff<-all_tardies$Tardies.y-all_tardies$Tardies.x
this_week_tardies<-merge(by="StudentID", this_week_tardies, select(all_tardies, StudentID, tardyDiff))
this_week_tardies$TardiesFull<-ifelse(this_week_tardies$tardyDiff==0,this_week_tardies$Tardies, paste(this_week_tardies$Tardies," (+",this_week_tardies$tardyDiff,")", sep=""))

#compare absences
all_absences<-merge(by="StudentID",last_week_attend, this_week_attend)
all_absences$absenceDiff<-all_absences$Absences.y-all_absences$Absences.x
this_week_attend<-merge(by="StudentID", this_week_attend, select(all_absences, StudentID, absenceDiff))
#this_week_attend$absencesFull<-paste(this_week_attend$Absences," (+",this_week_attend$absenceDiff,")", sep="")
this_week_attend$absencesFull<-ifelse(this_week_attend$absenceDiff==0,this_week_attend$Absences, paste(this_week_attend$Absences," (+",this_week_attend$absenceDiff,")", sep=""))

#compare ST Math
all_jiji<-merge(by="StudentID",last_week_jiji, this_week_jiji)
all_jiji$jijiGain<-round(all_jiji$JijiProgress.y-all_jiji$JijiProgress.x)
this_week_jiji<-merge(by="StudentID", this_week_jiji, select(all_jiji, StudentID, jijiGain))
this_week_jiji$JijiFull<-ifelse(this_week_jiji$jijiGain==0,this_week_jiji$JijiProgress,paste(this_week_jiji$JijiProgress," (+",this_week_jiji$jijiGain,")", sep=""))

#compare lexia
last_week_lexia=last_week_lexia%>%select(StudentID=Lexia.Username, End.of.Month.Level, Monthly.Units.Gained,Monthly.Minutes, Instr.Lex=Needs.Instruction, Last.Used, Units.To.Target)%>%
  #mutate(LexiaEffic=round(Monthly.Minutes/Monthly.Units.Gained))
  mutate(LexiaEffic=ifelse(Monthly.Minutes<10, "NA",ifelse(Monthly.Minutes>10 & Monthly.Units.Gained==0,"No Units", round(Monthly.Minutes/Monthly.Units.Gained))))

this_week_lexia<-this_week_lexia%>%
  select(StudentID=Lexia.Username, End.of.Month.Level, Monthly.Units.Gained,Monthly.Minutes, Instr.Lex=Needs.Instruction, Last.Used, Units.To.Target)%>%
  #mutate(LexiaEffic=round(Monthly.Minutes/Monthly.Units.Gained))
  mutate(LexiaEffic=ifelse(Monthly.Minutes<10, "NA",ifelse(Monthly.Minutes>10 & Monthly.Units.Gained==0,"No Units", round(Monthly.Minutes/Monthly.Units.Gained))))

this_week_lexia<-this_week_lexia%>%select(StudentID, Level=End.of.Month.Level, MonthlyUnits=Monthly.Units.Gained, MonthlyMins=Monthly.Minutes,Instr.Lex, LexiaEffic=round(LexiaEffic, digits=2))



##format the news/updates (A Google Form)
this_week_news$HR<-gsub(' -[A-z ]*', '' , this_week_news$HR)
this_week_news$Update<-as.character(this_week_news$Update)
this_week_news$HR<-as.list(strsplit(as.character(this_week_news$HR), ";"))
news_hr_list<-as.vector(unlist(as.list(this_week_news$HR)))


#Convert Grades to GPA
this_week_grades_wide<-dcast(this_week_grades, StudentID ~ SubjectName, value.var="FinalAvg")
this_week_grades$Points <- ifelse(this_week_grades$FinalAvg >=90, 4, ifelse(this_week_grades$FinalAvg >=80, 3,ifelse(this_week_grades$FinalAvg >=70, 2,ifelse(this_week_grades$FinalAvg >=60, 1,0))))
this_week_points_wide<-dcast(this_week_grades, StudentID ~ SubjectName, value.var="Points")
this_week_gpa <- transform(this_week_points_wide, GPA = round(rowMeans(this_week_points_wide[,-1], na.rm = TRUE), digits=2))
this_week_gpa_grades<-merge.all(by = c("StudentID"), this_week_gpa, this_week_grades_wide)%>%
  select(StudentID, `CHGO READING FRMWK`, `MATHEMATICS STD`, `SCIENCE  STANDARDS`, `SOCIAL SCIENCE STD`, GPA)%>%
  rename(Read=`CHGO READING FRMWK`, Math=`MATHEMATICS STD`, Sci=`SCIENCE  STANDARDS`, SS=`SOCIAL SCIENCE STD`)


two_weeks_grades_wide<-dcast(two_weeks_grades, StudentID ~ SubjectName, value.var="FinalAvg")
two_weeks_grades$Points <- ifelse(two_weeks_grades$FinalAvg >=90, 4, ifelse(two_weeks_grades$FinalAvg >=80, 3,ifelse(two_weeks_grades$FinalAvg >=70, 2,ifelse(two_weeks_grades$FinalAvg >=60, 1,0))))
two_week_points_wide<-dcast(two_weeks_grades, StudentID ~ SubjectName, value.var="Points")
two_week_gpa <- transform(two_week_points_wide, GPA = round(rowMeans(two_week_points_wide[,-1], na.rm = TRUE), digits=2))
two_weeks_gpa_grades<-merge.all(by = c("StudentID"), two_week_gpa, two_weeks_grades_wide)%>%
  select(StudentID, `CHGO READING FRMWK`, `MATHEMATICS STD`, `SCIENCE  STANDARDS`, `SOCIAL SCIENCE STD`, GPA)%>%
  rename(Read=`CHGO READING FRMWK`, Math=`MATHEMATICS STD`, Sci=`SCIENCE  STANDARDS`, SS=`SOCIAL SCIENCE STD`)

all_gpa_grades<-merge(by="StudentID", this_week_gpa_grades, two_weeks_gpa_grades, all=TRUE)
all_gpa_grades$gpaDiff<-round(all_gpa_grades$GPA.x-all_gpa_grades$GPA.y, digits=2)
this_week_gpa_grades<-merge(by="StudentID", this_week_gpa_grades, select(all_gpa_grades, StudentID, gpaDiff), all=TRUE)
this_week_gpa_grades$gpa.t<-ifelse(this_week_gpa_grades$gpaDiff==0,this_week_gpa_grades$GPA,paste(this_week_gpa_grades$GPA," (",this_week_gpa_grades$gpaDiff,")", sep=""))


#merge everything together by StudentID
this_week_complete<-merge.all(by = c("StudentID"), roster, this_week_gpa_grades, this_week_attend, 
                              this_week_tardies, this_week_jiji, this_week_newsela, this_week_lexia)%>%
                              filter(!is.na(StudentName))
this_week_complete$Tardies<-ifelse(is.na(this_week_complete$Tardies),0,this_week_complete$Tardies)

#Add On Track rating based on GPA and Attendance
this_week_complete=this_week_complete%>%mutate(OnTrack=
                                                 ifelse(this_week_complete$GPA>=3 & this_week_complete$Pct>=0.95,5,
                                                 ifelse(this_week_complete$GPA>=3 & this_week_complete$Pct>-0.90,4,      
                                                 ifelse(this_week_complete$GPA>=3 & this_week_complete$Pct>=0.80,3,
                                                 ifelse(this_week_complete$GPA>=3 & this_week_complete$Pct<0.80,2,
                                                 ifelse(this_week_complete$GPA>=2 & this_week_complete$Pct>=0.98,4,      
                                                 ifelse(this_week_complete$GPA>=2 & this_week_complete$Pct>=0.90,3,
                                                 ifelse(this_week_complete$GPA>=2 & this_week_complete$Pct<0.90,2,
                                                 ifelse(this_week_complete$GPA>=1 & this_week_complete$Pct>=0.95,3,
                                                 ifelse(this_week_complete$GPA>=1 & this_week_complete$Pct>=0.80,2,
                                                 ifelse(this_week_complete$GPA>=1 & this_week_complete$Pct<0.80,1,
                                                 ifelse(this_week_complete$GPA<1 & this_week_complete$Pct>=0.98,3,
                                                 ifelse(this_week_complete$GPA<1 & this_week_complete$Pct>=0.90,2,
                                                 ifelse(this_week_complete$GPA<1 & this_week_complete$Pct<0.90,1, "NA"))))))))))))))


#merge with intervention list
this_week_complete=merge.all(by=c("StudentID"),this_week_complete, select(intervention_students, -Name))

#merge with all EL Students list
this_week_complete=merge.all(by=c("StudentID"),this_week_complete, all_el_students)

#add Name Full column, which adds On-Track rating next to student Name
this_week_complete$NameFull=paste(this_week_complete$StudentName," (",this_week_complete$OnTrack,")")

#Move Full Name, Grade first 
this_week_complete <- this_week_complete[ ,c(32,33,30,1:29,31)]
this_week_complete<-rename(this_week_complete,ELL=ELL_Program_Year) 

#for web app
this_week_app<-select(this_week_complete, -gpa.t, -Membership.Days, -Pct, -absencesFull, -TardiesFull, -JijiFull, -Intervention, -StudentID, - NameFull)

#### MAKE THIS INTO A FUNCTION, MERGE EARLIER
#intervention=merge.all(by = c("StudentID"), select(this_week_complete, -StudentGradeLevel, -StudentHomeroom), select(intervention_students, StudentID, Intervention))
#intervention<-select(intervention, -StudentID)
#callan=filter(intervention,Intervention=="Callan")%>%select(-Intervention)
#shea=filter(intervention,Intervention=="Shea")%>%select(-Intervention)


#create data frame with just the comparrison values (for the pdf reports)
this_week_trends<-select(this_week_complete, -GPA, -gpaDiff, -Absences, -Membership.Days, -Pct, -absenceDiff, -Tardies, -tardyDiff, -JijiProgress, -jijiGain, -Level, -MonthlyMins -MonthlyUnits)

#this_week_trends<-this_week_trends%>%select(-MonthlyMins, -MonthlyUnits)


#function to filter by class, checks which grade the HR is in to use Lexia or Newsela
create_class_data<-function(HR_number, class=FALSE){
  
  this_week_complete_hr<-this_week_trends%>%filter(StudentHomeroom==HR_number)%>%select(-StudentHomeroom, -StudentID, -Intervention, -StudentGradeLevel)
  if(substr(HR_number,1,1)=="B"){
  this_week_complete_hr<-this_week_complete_hr%>%select(-Instr.Lex, - LexiaEffic)}
  if(substr(HR_number,1,1)=="A"){
    this_week_complete_hr<-this_week_complete_hr%>%select(-NELA.Rct.Quiz, -NELA.Lexile.Level)}
  if(substr(HR_number,2,2)=="1"){
    this_week_complete_hr<-this_week_complete_hr%>%select(-Read, -Math, -Sci, -SS, -gpa.t)}
  return (this_week_complete_hr%>%arrange(OnTrack)%>%select(-StudentName, -OnTrack))
}

#create list of students whose on track is 2 or less
create_admin_data<-function(grade1=0,grade2=8){
  admin_data<-this_week_app%>%filter(OnTrack<=2 & as.numeric(as.character(StudentGradeLevel))>=grade1 & as.numeric(as.character(StudentGradeLevel))<=grade2)
  #admin_data<-admin_data[1:16]%>%arrange(OnTrack,StudentGradeLevel,StudentHomeroom)
  return (admin_data%>%rename('GR'=StudentGradeLevel, 'HR'=StudentHomeroom, "T"=OnTrack, 'R'=Read, 'M'=Math, 'S'=Sci, "Abs"=Absences, "Abs(+/-)"=absenceDiff, "GPA(+/-)"=gpaDiff, "T(+/-)"=tardyDiff))
}

admin_data<-create_admin_data()

#creates the intervention data for Ms Callan and Ms Shea's students
create_intervention_data<-function(int_name){
  this_week_complete_int<-this_week_trends%>%filter(Intervention==int_name)%>%select(-StudentHomeroom, -StudentID)
  if(int_name=="Callan"){
    this_week_complete_int<-this_week_complete_int%>%select(-Instr.Lex, - LexiaEffic)}
  if(int_name=="Shea"){
    this_week_complete_int<-this_week_complete_int%>%select(-NELA.Rct.Quiz, -NELA.Lexile.Level)}
  if(int_name=="Shea"){
    this_week_complete_int<-this_week_complete_int%>%select(-Read, -Math, -Sci, -SS, -gpa.t)}
  return (this_week_complete_int%>%arrange(OnTrack)%>%select(-Intervention, -StudentName, -StudentGradeLevel, -OnTrack))
}

callan=create_intervention_data("Callan")
shea=create_intervention_data("Shea")

#Gets the news items for a specific HR to add to their pdf report
create_class_news<-function(HR_number){
  if(HR_number %in% news_hr_list)
  {news<-this_week_news$Update[HR_number==this_week_news$HR];print(news)}
  else 
    news="No Updates for your HR/Grade as of Thursday at 1pm"
  return(news)
}


#write a report using the data and news for each HR
writeReports<-function(){
  for (hr in hrs){
  rmarkdown::render('loop_weekly_report.Rmd',  # file 2
                    output_file =  paste(hr,"_Weekly_Report-4-07-16.pdf", sep=''), 
                    output_dir = '/Users/administrator/Desktop/WeeklyReports3',
                    params = list(
                      hr_data = create_class_data(hr),
                      hr_news = create_class_news(hr),
                      hr=hr),
                    "pdf_document"
  )
  
 }
}

#Just write one report
writeReport<-function(hr){
    rmarkdown::render('loop_weekly_report.Rmd',  # file 2
                      output_file =  paste(hr,"_Weekly_Report-05-02-16.pdf", sep=''), 
                      output_dir = '/Users/administrator/Desktop/WeeklyReports3',
                      params = list(
                        hr_data = create_class_data(hr),
                        hr_news = create_class_news(hr),
                        hr=hr),
                      "pdf_document"
    )
}

writeCallan<-function(){
    rmarkdown::render('loop_weekly_report.Rmd',  # file 2
                      output_file =  paste("Weekly_Report_Callan_04-07-16.pdf", sep=''), 
                      output_dir = '/Users/administrator/Desktop/WeeklyReports3',
                      params = list(
                        hr_data = callan,
                        hr_news = "None",
                        hr="Callan"),
                      "pdf_document"
    )
    
}

writeShea<-function(){
  rmarkdown::render('loop_weekly_report.Rmd',  # file 2
                    output_file =  paste("Weekly_Report_Shea_04-07-16.pdf", sep=''), 
                    output_dir = '/Users/administrator/Desktop/WeeklyReports3',
                    params = list(
                      hr_data = shea,
                      hr_news = "None",
                      hr="Shea"),
                    "pdf_document"
  )
  
}

writeAdmin<-function(){
  rmarkdown::render('loop_weekly_report.Rmd',  # file 2
                    output_file =  paste("Off_Track_Weekly_Report_04-07-16.pdf", sep=''), 
                    output_dir = '/Users/administrator/Desktop/WeeklyReports3',
                    params = list(
                      hr_data = create_admin_data(),
                      hr_news = "None",
                      hr="Whole School Off Track"),
                    "pdf_document")
  rmarkdown::render('loop_weekly_report.Rmd',  # file 2
                    output_file =  paste("UGC_Off_Track_Weekly_Report_04-07-16.pdf", sep=''), 
                    output_dir = '/Users/administrator/Desktop/WeeklyReports3',
                    params = list(
                      hr_data = create_admin_data(5,8),
                      hr_news = "None",
                      hr="Upper Grade Center Off Track"),
                    "pdf_document")
  rmarkdown::render('loop_weekly_report.Rmd',  # file 2
                    output_file =  paste("LGC_Off_Track_Weekly_Report_04-07-16.pdf", sep=''), 
                    output_dir = '/Users/administrator/Desktop/WeeklyReports3',
                    params = list(
                      hr_data = create_admin_data(1,4),
                      hr_news = "None",
                      hr="Lower Grade Center Off Track"),
                    "pdf_document")
  
}

