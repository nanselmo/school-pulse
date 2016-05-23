# School Pulse (for Chicago Public Schools)
Use CSV files from CPS to print student one-pagers (pdf) by Homeroom and report of off-track students

## Description
Using CSV files with student data (internal and external), this script will show change over time in Grades, Attendance, Tardies and On-Track Rating (as calculated by the University's Consortium on School Research). It also track trends in blended-learning usage and efficiency in ST Math, Lexia, and NEWSela (more to come). 

The output format is PDF, one-pagers. The outout can also be a Shiny App (as long as access is restricted or it's hosted locally on Shiny Server). 

This project was built in RStudio and is being created for Cesar Chavez, with the intent of being able to share district wide. This script works with the file structure for CPS-specific files, but it is easy enough to add a template so that any school/district could potentially use their student data files.  Ultimately, this tool will move to an online web-app where admins/teachers can upload these files to output a zipped folder with these PDFs.

##How to Use This

### Download files from CPS' tools.
 * **Grades** - [Gradebook](https://gradebook.cps.k12.il.us)>Reports>ES Cummulative Grades Extract>Save as .csv
 * **Attendance** - [Dashboard](https://dashboard.cps.edu/Dashboard)>School Profile>Att Detail (in left side panel)>Click on Bar Graph, current year> Click Options in the top right drop down>Export Detail> to CSV
 * **Tardies** - [Gradebook](https://gradebook.cps.k12.il.us)>Attendance>Excessive Absence Report>Select the Date, All, T (for Tardy)>Export to Excel
 * **English Learners** *(must have Principal Role)* - [SIM](https://sim.cps.k12.il.us)>My Report in top Nav Bar, then All Reports>ELL Reports> CPS Students in Program>Output as CSV
 * **Lexia** - [myLexia](https://mylexia.com)>View Progress Report>Export>Request Export> Check Email for Link
 * **ST Math** - emailed weekly, called the Progress Completion Report (PCR). (Copy and paste to new, non-password protected csv)
 * **NEWSela** - also emailed weekly, called Usage Report. (Copy and paste to new, non-password protected csv)
 * **Administrative Notes** - comes from a [Google Form](http://goo.gl/forms/GPIlJi7KRo) for administrators/clerks to add to
 
### Move and (optionally) Rename  Files
Now that those files are downloaded, save them into the same folder as weekly_report.R. You can choose to rename the files to make them easier, for example, I rename `% Student Attendance Details_20160415.csv` to `Attendance-04-15-16.csv`.

In order to show trends, you have to have two weeks of data saved in that folder. Because grades don't (typically) change too much over two weeks, I use grades in biweekly intervals instead of weekly. 

### Run in RStudio to generate PDFs 

1. Open weekly_report.R in RStudio
2. Click Source
3. In RStudio's console, type
  * `writeReports()` - will generate a report for every unique homeroom (pulled from the Attendance csv)
  *  `writeAdmin()` - will generate reports showing the student's who are off-track (a rating of 2 or lower). (One for 1-4th grade, one for 5th-8th grade, one for the whole school)
(Additional Directions coming soon!)
