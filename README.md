# School Pulse (for Chicago Public Schools)
Use CSV files from CPS to print student one-pagers (pdf) by Homeroom and report of off-track students

1. Download files from CPS' tools.
 * **Grades** - [Gradebook](gradebook.cps.k12.il.us)>Reports>ES Cummulative Grades Extract>Save as .csv
 * **Attendance** - [Dashboard](https://dashboard.cps.edu/Dashboard)>School Profile>Att Detail (in left side panel)>Click on Bar Graph, current year> Click Options in the top right drop down>Export Detail> to CSV
 * **Tardies** - [Gradebook](gradebook.cps.k12.il.us)>Attendance>Excessive Absence Report>Select the Date, All, T (for Tardy)>Export to Excel
 * **English Learners** *(must have Principal Role)* - [SIM](sim.cps.k12.il.us)>My Report in top Nav Bar, then All Reports>ELL Reports> CPS Students in Program>Output as CSV
 * **Lexia** - [myLexia](mylexia.com)>View Progress Report>Export>Request Export> Check Email for Link
 * **ST Math** - emailed weekly, called the Progress Completion Report (PCR). (Copy and paste to new, non-password protected csv)
 * **NEWSela** - also emailed weekly, called Usage Report. (Copy and paste to new, non-password protected csv)
 * **Administrative Notes** - comes from a [Google Form](http://goo.gl/forms/GPIlJi7KRo) for administrators/clerks to add to
 
2. Save all of those files in the same folder as weekly_report.R

3. Run in RStudio to generate PDFs (Additional Directions coming soon!)
