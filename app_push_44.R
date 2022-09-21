
###### 09/19/2022 ######
#R version 4.2.0 (2022-04-22 ucrt)
setwd("G:/Shared drives/HMCSE-PAM Lab/Jay's Space/2022 Active Projects/EntrytoEnd_shiny")

# libraries 
library(shiny)
library(networkD3)
library(tidyverse)
library(DT)
library(purrr)
library(plotly)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(arules)
library(arulesViz)
library(lubridate)
library(gt)
library(gtsummary)
library(RColorBrewer)
library(janitor)
library(htmlwidgets)
library(pivottabler)
library(rpivotTable)
library(moonBook)
library(caret)
library(visNetwork)
library(echarts4r)
  
### DATA SET ###
## Cohort/Students analysis data in PAM Dashboard
#data script location
#G:/Shared drives/HMCSE-PAM Lab/Jay's Space/_DataShaping/FTIC_201705_202108_enc_Rmd
 
 
# risk-analysis data set
# risk_apr_data_set <- readRDS("risk_apr_data_set2.rds")  
risk_apr_data_set1 <- readRDS("risk_apr_data_w_GPAv1.rds") # used risk analysis 

### HS Program APR Help table
hs_prog_apr_help_table <- readRDS("hs_prog_apr_help_table.rds")

# selected fin codes
#data script
#G:/Shared drives/HMCSE-PAM Lab/Jay's Space/_Dashboards/Shiny/R script-Datasets/ftic_overview_aprandmore.R
fin_ftic_all_data_selected_codes <- readRDS("fin_ftic_selected_codes.rds") # finanacial data 

# updated Spring 2022
#enc_data_ds <- readRDS("ftic_dashboard_data.rds") # enc_script
enc_data_ds1 <- readRDS("ftic_dashboard_datav1.rds") # enc_script, use summary overview tab


# data script location
# G:/Shared drives/HMCSE-PAM Lab/Jay's Space/2021 Active Projects/08/MAJOR SWITCH/Final_destination_graduation.Rmd
new_sankeydata_v1 <- readRDS("sankey_new_data_2015to2021.rds") # program changes


# course data
course_data_v1 <- readRDS("DB_crs_grade_FTIC1521V1.rds")  # coures data

# tier character data
student_character_1stFall <- readRDS("student_character_1stFall_df.rds") # by term (file apriori by Tier.rdm)# tier characteristics


# hs name and apr and stopout data
#location "G:/Shared drives/HMCSE-PAM Lab/Jay's Space/2022 Active Projects/APR&StopoutsBy_HS_name/APR and Stopout By HS Name.Rmd
apr_stopout_hs_name_data <- readRDS("apr_stopout_table_2017_to_2021.rds")

# input helper

maxhsgpa <- max(new_sankeydata_v1$GPA_HIGHSCHOOL)
minhsgpa <- min(new_sankeydata_v1$GPA_HIGHSCHOOL)
 

### start-ui
ui <- dashboardPage( skin = "blue",
                     
                     dashboardHeader(title="From Entry To End Program", titleWidth = 300),
                     dashboardSidebar( width = 300,
                                       
                                       sidebarMenu(id = "Menu...",                 
                                                   menuItem( text= "FTIC Overview", startExpanded = T, icon = icon("fas fa-chart-line"),  
                                                             menuSubItem(text = "APR/Graduation/Summary", tabName = "metricapr"),
                                                             menuSubItem(text = "Course Grades",     tabName = "coursegrades")
                                                             
                                                   ),      
                                                   menuItem( text= "FTIC Migration", startExpanded = T,icon = icon("fas fa-chart-bar"),
                                                             menuSubItem(text = "Entry or Exit Flow Graph",  tabName = "flowchart")
                                                             
                                                   ),# menuitem
                                                   menuItem( text = "FTIC Characteristics", startExpanded = T, icon = icon("list-alt"),
                                                             menuSubItem(text = "Explore Tier Characteristics", tabName = "stu_characteristic")
                                                             
                                                             
                                                   ),
                                                   menuItem( text = "FTIC Financial Aid Data", startExpanded = T, icon = icon("fas fa-hand-holding-usd"), 
                                                             # menuSubItem(text = "Linking Award Types", tabName = "fin_aid_type"),
                                                             menuSubItem(text = "Pivot Table",               tabName = "fin_aid_pivot")
                                                   ),
                                                   menuItem( text = "FTIC Risk-Analysis", startExpanded = T, icon = icon("fas fa-stream"),
                                                             menuSubItem( text = "Predicting Academic Success", tabName = "risk_apr"),
                                                             menuSubItem( text = "New Applicants", tabName = "risk_apr_new_stu"))
                                       )
                     ), # sidebar menu
                     
                     dashboardBody(
                         
                         tags$head(
                             tags$style(HTML(
                                 ".skin-blue .main-sidebar {font-size: 16px; color: White; background-color: #FDFEFE; font-family: Helvetica;}
                                 
                                  .skin-blue .main-sidebar .sidebar .sidebar-menu a{ background-color: #004C97;color: #FDFEFE;}
                                  .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu a {background-color: #009CDE !important;}
                                  .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{ background-color: #C4D600; color: #000000; }
                                  .skin-blue .main-header  .navbar  .sidebar-toggle:hover{ background-color: #ff69b4;}
                                  .skin-blue .main-header  .logo {background-color: #004384; font-size: 20px;}
                                  .skin-blue .main-header  .navbar { background-color: #004C97;}
                                  .content-wrapper, 
                                 .right-side { background-color: #DFE9F2;
                                 }" ))  
                         ),
                         tabItems(
                             
                             # New applicants
                             tabItem(tabName = "risk_apr_new_stu",
                                     fluidRow(
                                         box(title="Upload a File", width =  12, solidHeader = T,
                                             checkboxGroupInput("checkdataname", label=h5(p(span("Checkbox Data", style="font-weight:bold"))), 
                                                                choices = list("Admissions Checklists Data"= 1, "Banner Applicants Data" = 2), selected = 1),
                                             fileInput("new_stu_upload", "Click browse to import a .csv file", buttonLabel = "Browse...", accept = c(".csv")),
                                             tags$hr(),
                                             verbatimTextOutput("rawdatadim"),
                                             h5(p(span("Column names of the data frame:"))),
                                             verbatimTextOutput("dataheaders")),
                                         box(title="Upload Only Selected Headers Data Preview", width = 12, solidHeader = T,
                                             
                                            div(style="overflow-x:scorll;overflow-y:scroll", DT::dataTableOutput("new_stu_data")),
                                            verbatimTextOutput("cleandatadim")
                                                ),
                                         
                                         tags$ul(    
                                            h4(p(span("Possible Predictors", style ="color:blue"))),
                                                 tags$li("HS_Program_index = measures APR by students' high school name and program"),
                                                 tags$li("New HS = if FTIC from the school have enrolled before or not"),
                                                 tags$li("High School GPA = 4.00 scale as calculated by UWF"),
                                                 tags$li("Foreign Language = if required unites have been met or not"),
                                                 tags$li("Gender = M (male) or F (female)"),
                                                 tags$li("Prior Hours =  earned university hours before first fall (or summer) at UWF"),
                                                 tags$li("First Fall GPA = UWF GPA earned at end of first fall")),
                                         tags$ol(
                                             h4(p(span("Prediction Data", style="color:blue"), downloadButton("downloadriskdata", "Download"))),
                                             tags$li(p(strong("Pre-college factors (prefac): HS GPA, New HS, Foreign lang, and hs_program_index (currently used)")))#,
                                             #tags$li(p(strong("... factors (...): HS GPA, New HS, Foreign lang, hs_program_index, and ..."))) 
                                             ),
                                         box(title = "Preprocessed data with Risk-level", width =  12, solidHeader = T,
                                             
                                              div(style="overflow-x:scorll;overflow-y:scroll", DT::dataTableOutput("pred_new_applicants"))
                                                )
                                     )
                                 
                             ),
                             #risk_analysis
                             tabItem(tabName = "risk_apr",
                                     fluidRow(
                                          
                                         column(4, selectInput("selectdependent", "Outcome Variable", choices = "APR", selected = "APR")),
                                         
                                         column(4, selectInput("selectvariables", "Predictors", 
                                                               choices = colnames(risk_apr_data_set1[,c(4:6,13,9:12)]),
                                                               selected = c("HS_GPA","Gender","HS_Program_Index","Prior_Hours","HS_New","Foreign_Lang"), multiple = T), 
                                          p(span("Five factors in combination tend to give best results: HS GPA, Gender, HS_Program Index, Award ACEF, and First_Fall_GPA", style ="color:blue")),
                                          p(span("Six pre-college factors are selected: HS GPA, Gender, HS_Program Index, Prior_Hours, HS_New, Foreign_Lang", style ="color:blue"))
                                         ),
                                         
                                         
                                         column(4, selectInput("selectmethods", "Methods", choices = "Risk Predict #1", selected = "Risk Predict #1"))
                                     ),
                                     fluidRow(
                                         tabBox(title = "Risk Analysis", id = "risk_tab", width = 12,
                                                #tabPanel(title = "Model Fitting", verbatimTextOutput("risk_apr_fitting")),
                                                tabPanel(title = "Accuracy Table", 
                                                         plotOutput("risk_apr_crosstab", width = "100%" ),
                                                         br()   ),
                                                tabPanel(title = "Variable Importance", 
                                                         echarts4rOutput("risk_apr_variimp")
                                                         #plotlyOutput("risk_apr_variimp")
                                                         ),
                                                tabPanel(title = "Risk-Level Students", 
                                                         div(style = 'overflow-x: scroll; overflow-y: scroll;',
                                                             DTOutput("risk_apr_level_ftic"))),
                                                tabPanel(title = "2021 Predicted vs. GPA APR", 
                                                         h3(span(strong("2021 FTIC GPA APR vs. Risk-level"))),
                                                         p(span("... APR is calculated after completion of first spring semester", style="color:blue")),
                                                         br(),
                                                         DTOutput("ftic_2021_apr") ,
                                                         hr(),
                                                         h3(span(strong("Average High School GPA by Risk-level & APR"))),
                                                         DTOutput("risk_apr_hsgpa_all"),
                                                         hr(),
                                                         h3(span(strong("Risk-level by High School & Program Index"))),
                                                         DTOutput("hs_prog_index_table"),
                                                         hr(),
                                                         h3(span(strong("FTIC 2017 Characteristics by Risk-level & APR"))),
                                                         gt_output("ftic2017_risk_all") 
                                                         ),
                                                tabPanel(title =  "High School & Entry Program",
                                                         h3("High School and Applied Program Index"),
                                                         h4("HS and Program Index measures APR that tracks performance by students' high school name and program"),
                                                         h4("Cohorts are from 2017 to 2020"),
                                                         div(style ='overflow-x: scroll', DTOutput("hs_prog_index_unique"))
                                                    
                                                ),
                                                tabPanel(title = "High School Name for APR and Stopout",
                                                        fluidRow( 
                                                            box(width = 2, title = "Select HS Name", solidHeader = T,
                                                             selectInput("hs_name_rank", "Top 10 High School Name", choices = NULL, selected ="Gulf Breeze High School" )),
                                                            box(width = 10, title = "Plot - APR and Stopout by Hs Name", solidHeader = T,
                                                             echarts4rOutput("hsname_enroll_plot"))
                                                         ),
                                                          fluidRow( 
                                                              box(width = 12, title = "Table - Rank of high enrolled and APR ",solidHeader = T,
                                                              div( style="overflow-x:scorll;overflow-y:scroll", DTOutput("apr_hsname_table")))
                                                              )
                                                            )
                                                
                                                
                                         )
                                     )    
                                     
                             ),
                             
                             #fin pivot
                             tabItem(tabName = "fin_aid_pivot",
                                     
                                     column(12, 
                                            div(style ='overflow-x: scroll',
                                                rpivotTableOutput("fin_overallpivot", width = "100%", height = "1000px") ) )
                                     
                                     
                             ),
                             #student characteristics tab
                             tabItem(tabName = "stu_characteristic",
                                     
                                     # tier characteristics
                                     fluidRow(
                                         box(width = 12, title ="Exploring Factors Associated with Student Performance", solidHeader = T, status = "primary",
                                             h4(span(strong("This application:", style="color:black"))),
                                             h4(span("display patterns between factors")),
                                             h4(span("explores pre-college factors")),
                                             h4(span("shows most strongly associated rules by selection (ex: support, confidence, or lift)")),
                                             h4(span("applies rules to break or strengthen association via first term GPA")),
                                             h4(span("Data includes:", style="color:black")),
                                             h4(span("tri-county high school graduates")),
                                             h4(span("2017 to 2021 cohort")),
                                             h4(span("high school name and GPA, application month, entry program, and generation")),
                                             h4(span("See more detailed tabular data under Data Summary"))
                                         )
                                     ),
                                     fluidRow(
                                         column(3,
                                                selectInput(inputId = "selecttiers", 
                                                            label = "Choose Tiers", 
                                                            choices = c(1:5,"NA") , selected = 4 , multiple = T)
                                                
                                         ),
                                         column(3,
                                                sliderInput(
                                                    inputId = "supp",
                                                    label = "Choose a minimum support",
                                                    min = 0.01,
                                                    max = 0.1, # max 10% of dataset
                                                    value = 0.01,
                                                    step = 0.01),
                                                h5("Support: proportion of students information in the data")
                                         ),
                                         column(3,
                                                sliderInput(
                                                    inputId = "conf",
                                                    label = "Choose a minimum confidence",
                                                    min = 0.2,
                                                    max = 0.9,
                                                    value = 0.5,
                                                    step = 0.1),
                                                h5("Confidence: likelihood that RHS (right-hand-side) depends on LHS (left-hand-side)")
                                         ),
                                         column(3,
                                                sliderInput(
                                                    inputId = "lift",
                                                    label = "Choose a minimum lift",
                                                    min = 1.0,
                                                    max = 5.0,
                                                    value = 1.0, 
                                                    step = 0.1),
                                                h5("Lift: strength of association (if iift > 1: strong positive relationship)"))
                                     ),
                                     fluidRow(
                                         tabBox( title = "Associated via characterizaton rules",width = 12,
                                                 tabPanel(title = "Plot - interactive graph",
                                                          h4(strong("Find rules with rhs containing First Term GPA")),
                                                          h5("Only plotting the best 100 using", strong("lift"), "if too many rules supplied. Color intensity reflects rule strength (lift). Dropdown filters factors by id."),
                                                          visNetworkOutput("rulegraph", height = "800px")       
                                                          
                                                 ),
                                                 tabPanel(title = "Table - rule support and strength",
                                                          DTOutput("filteredrules")
                                                 ),
                                                 tabPanel(title = "Table - data summary",
                                                          div(style = 'overflow-x: scroll; overflow-y: scroll;',
                                                              gt_output("gtsummarytable"))
                                                 ))
                                     )  
                                     
                             ), #students tab
                             # courses tab
                             tabItem(tabName = "coursegrades",
                                     fluidRow(
                                         box(width = 2, title = "Filter...", solidHeader = T, status = "primary", background = "black",
                                             selectInput(inputId = "courseprogram", label = "Select a Program",
                                                         choices = sort(unique(course_data_v1$ENTRY_PROGRAM)),
                                                         selected = "Cybersecurity"
                                             ),
                                             selectInput(inputId = "couresecohort", label = "Select a Cohort",
                                                         choices = sort(unique(course_data_v1$Cohort)),
                                                         selected = "2020"
                                             ),
                                             "Choosing the high school GPA thresholds",br(),
                                             "will update passed/failed rates and counts",br(),
                                             "(Replaced NAs with 0.00)",
                                             numericInput(inputId = "crsgparange1",
                                                          label = "Minimum Allowed High School GPA Thresholds (>= 2.00)", step = 0.01, #dragRange = F,
                                                          value = 0.00,  min=minhsgpa, max = maxhsgpa ), 
                                             numericInput(inputId = "crsgparange2",
                                                          label = "Maximum Allowed High School GPA Thresholds (<= 5.04)",step = 0.01, #dragRange = F,
                                                          value = 5.04, min=minhsgpa, max = maxhsgpa ),
                                             selectInput(inputId = "topcrsselectterms", label = "Select a Term",
                                                         choices = NULL,
                                                         selected = NULL) 
                                             
                                         ), 
                                         box(width = 10, title = "DWF With The Most Frequently Enrolled 1st Year Courses by Program", solidHeader = T, 
                                             status = "primary", 
                                             #actionButton("frerqCRS", label = "Show Updated Info."),
                                             plotly::plotlyOutput("bar_course_pass_plot"))), #1st row
                                     
                                     fluidRow(
                                         box(width = 2, title = "Filter...", solidHeader = T, background = "black",status = "primary",
                                             selectInput(inputId = "coursename", label = "Select a Course",
                                                         choices = NULL,
                                                         selected = "ENC1101"),br(),
                                             "Included Grades are A to F range, NF (Non-attending/Fail), U (Unsatisfactory), and W (Withdrawn).",
                                             br(),
                                             selectInput(inputId = "courseterms", label = "Select a Term",
                                                         choices = NULL,
                                                         selected = NULL)
                                         ),
                                         
                                         box(width = 10, title = "APR by 1st Year Course Grades by Program (included multiple attempts)", 
                                             solidHeader = T, status = "primary", 
                                             #actionButton("aprgradeCRS",label = "Show Updated Info."),
                                             plotly::plotlyOutput("bar_course_apr_plot"))
                                     ) # 2nd row
                             ), #course tabname/2nd row
                             # sankey tab
                             tabItem(tabName= "flowchart", 
                                     fluidRow(
                                         column(2,selectInput("movecohort","Choose a Cohort", choices = sort(unique(new_sankeydata_v1$Cohort)), selected = 2017 )),
                                         column(2,selectInput("movelevel","Choose a Category", choices =c("College","Department","Program"), selected = "College", multiple = FALSE )),
                                         column(2,selectInput("choosedirection", label = "Choose a Direction", choices = c("Forward","Backward"), selected = "Forward")),
                                         column(2,selectInput("moveentry","Choose an Entry/Exit", choices = NULL , selected = "College of Sci and Engineering" )),
                                         column(2,numericRangeInput(inputId = "sankeydirectionhsgpa", label = "Choose High School GPA", value = c(0.00,5.04), separator = " to ", step=0.01, width = "100%", min=0.00, max = 5.04 )), 
                                         #column(2,"Click to refresh", br(), actionButton("entrysankey", label = "Start Flow Graph Update", class = "btn-lg btn-danger")),
                                         bsTooltip(id="choosedirection", title = "Forward: entry-to-exit, Backward: exit-to-entry",
                                                   placement = "right",  trigger = "hover",options = list(delay = list(show=1000, hide=3000 ))),
                                         column(2,selectInput("movepath","Year Range", 
                                                              choices = c("from 1 to 2", "from 2 to 3","from 3 to 4","from 4 to end", "from 1 to end"), 
                                                              selected = "from 1 to 2"), multiple =FALSE)
                                     ), #1st flow
                                     #sankey updated: 1st row is filters and description. 2nd row is plot
                                     fluidRow(
                                         tabBox(title = "Move-out or Move-in",  id="sankey_tab", width = 12,
                                                tabPanel(title = "From your Entry or Exit",
                                                         networkD3::sankeyNetworkOutput("sankey_moving", width = "100%")
                                                ),
                                                tabPanel(title="Flow graph Details",
                                                         p("The path from entry to degree time such as direction of switching colleges, departments, or programs by choosing a category"),
                                                         p("The importance of factors including between entry to 2nd Fall such as 1st term GPA, APR, high school GPA, and gender"),
                                                         p("The path from entry to graduation time by choosing the Forward direction"),
                                                         p("Previous paths leading to a final category by choosing the Backward direction"),
                                                         p("How categories are linked with each other by counts and proportion"),
                                                         br(),
                                                         strong("Terms:"),
                                                         p(span("Entry:", style= "color:blue"), "name of college, department, or program for the", strong(" first Fall")),
                                                         p(span("Exit:", style= "color:blue"), "name of college, department, or program at", strong("graduation")),
                                                         p(span("2nd to 4th Fall:", style= "color:blue"), "continuing in each consecutive following Fall"),
                                                         p(span("Degree:", style= "color:blue"), "name of degree for graduating college, department, or program"),
                                                         p(span("Graduation Time:", style= "color:blue"), "graduated within 4-year, 5-years, 6-years, or no degree by Fall 2021"),
                                                         p(span("Nodes:", style= "color:blue"), "starts entry groups and splits into different groups between entry to graduation time"),
                                                         p(span("Links:", style= "color:blue"), "the thinkness of each line is proportional to the number students between nodes"),
                                                         p(span("No Degree:", style= "color:blue"), "no bachelor's degree by Fall 2021"),
                                                         p(span("Stopout:", style= "color:blue"), "did not enroll in that Fall (either of the following)"),
                                                         p(strong("Stopout to Degree name:"), "early graduation"),
                                                         p(strong("Stopout to NoDegree:"), "dropped out without degree"),
                                                         p(span("NA:", style="color:blue"), "no available information after withdrawing from all courses that semester/academic year"),
                                                         br(),
                                                         em("Note: some numbers won't exactly match other sources since the time the data was downloaded")
                                                         
                                                )
                                         )
                                     ), # sanley plot and discription box
                                     #sankey original to replaced  with migration
                                     fluidRow(
                                         strong("Flow Graph Data filterd by Cohort and Entry"),
                                         box( title = "Entry to Exit", solidHeader = T, width = 12,
                                              div(style ='overflow-x: scroll;overflow-y: scorll;',
                                                  DT::DTOutput("flowtable",  width = "100%")))
                                     )
                             ),  #program sankey tab
                             
                             
                             # summary tab
                             tabItem(tabName = "metricapr", 
                                     
                                     fluidRow( 
                                         column(2,  
                                                selectInput(inputId = "metriccohort", 
                                                            label = "Select a Cohort", 
                                                            choices = sort(unique(enc_data_ds1$cohort)), selected = 2017, multiple = F )),
                                         
                                         column(width = 2,  
                                                selectizeInput(inputId = "metriccollege", 
                                                               label = "Select a college", 
                                                               choices =  c("(All)",sort(unique(enc_data_ds1$stu_college))),
                                                               selected ="(All)"
                                                )),
                                         
                                         column(width = 2, 
                                                selectInput(inputId = "metricdepart", 
                                                            label ="Select a department", 
                                                            choices = NULL,  
                                                            selected = NULL#"Biology"  
                                                )),
                                         column(width = 2,
                                                selectInput(inputId = "metricCIP",
                                                            label ="Select a Program",
                                                            choices = NULL, 
                                                            selected = NULL 
                                                )),
                                         
                                         column(width = 2, 
                                                numericRangeInput(inputId = "metricHSGPA",
                                                                  label = "Select HS GPA (Replaced NAs with 0.0)",
                                                                  value = c( 0.00,5.04), separator = "To", step = 0.01, width = NULL,  min=0.00, max = 5.04 )), 
                                         
                                         
                                         column(width = 2,  
                                                selectInput(inputId = "metricdemo", 
                                                            label = "Select demographics", 
                                                            choices = c("None", "Gender","Ethnicity","FirstGen","FeeResidency"),
                                                            selected = "None", multiple= F ))
                                    
                                     ), # 1st row for filter
                                     
                                     
                                     
                                     h4(strong("Note: results display only filtered selection", style="color:black")),
                                     
                                     fluidRow(   
                                         box(width = 6, title ="Enrollment & Retention in First Year (based on current term)", status = "primary",
                                             collapsible=TRUE,
                                             # h3("Enrollments & Retention in 1st Year (based on current term)",
                                             #                   style = 'font-size:20px; background-color: #C4D600;'), 
                                             solidHeader = T,  plotly::plotlyOutput("enrollment_bar") 
                                             # style = "height:400px; overflow-y: scroll;overflow-x: scroll;")
                                         ),
                                         box(width = 6,  collapsible=TRUE, title = "First Fall GPA", status = "primary", solidHeader = T,
                                             plotly::plotlyOutput("firstfallgpa_plot"))
                                         
                                     ), # 2nd row
                                     br(),
                                     #program chart by cohort
                                     fluidRow(
                                         box(width = 6, collapsible=TRUE, title = "Unchanged Students Academic Progress Rate (remained after entering)", status = "primary", solidHeader = T,
                                             plotly::plotlyOutput("metricAPR_plot")),
                                         
                                         box(width = 6, collapsible=TRUE, title = "Graduation Time (based on entry)", status = "primary", solidHeader = T,
                                             plotly::plotlyOutput("metricontime_plot"))
                                         #style = "height:500px; overflow-y: scroll;overflow-x: scroll;")
                                     ), # 3rd row
                                     fluidRow(
                                         tabBox(title = "Summary Table", width = 12,
                                                
                                                tabPanel(title = "Table - retention, APR, and graduation by filtered selection", 
                                                         div(DTOutput("uwf_table_reactive"), style = "overflow-x: scroll; overflow-y: scroll; font-size: 100%;"),
                                                         hr(),
                                                        # h5(p(span("Trend in APR", style="color:blue"))),
                                                         echarts4rOutput("metric_trend")
                                                         ),
                                                tabPanel(title = "Bar Chart - Comparison APR by Program",
                                                         echarts4rOutput("racing_apr_plot", height = "1000px")), #style = "overflow-x: scroll; overflow-y: scroll;"),
                                                # tabPanel(title = "Old Table (2015-2020) - retention, APR, and graduation by filter",  
                                                #          DTOutput("cohortaprtable"), style = "overflow-x: scroll; overflow-y: scroll;"),
                                                tabPanel(title = "Pivot Table - based on Cohort", 
                                                         #solidHeader = T, width = 12,background = "light-blue",
                                                         div(style ='overflow-x: scroll',
                                                             rpivotTableOutput("metric_pivot", width = "100%", height = "1000px"))) 
                                                
                                         )
                                     )
                             )#tabitem
                         ) #tabitems  
                         
                         
                     )# dashboardbody
                     
)
#end-ui



#start-server

server <- function(input, output, session) {
    
    ### New Applications Prediction
    options(shiny.maxRequestSize = 30*1024^2) #30 MB # 1MB = 1024^2
    
    new_applicants_data <- reactive({
        
         req(input$new_stu_upload)
         req(input$checkdataname)
         ext <- read.csv(input$new_stu_upload$datapath, header =TRUE, row.names = NULL)

         
    })
    
    output$rawdatadim <- renderText({
        dim <- dim(new_applicants_data())
        paste("Number of rows in uploaded data: ", dim[1])
    })
    output$dataheaders <- renderText({
       headernames <-  colnames(new_applicants_data())
       paste(headernames, sep=", ")
    })
    
    ### Data process
    #hs_prog_apr_hel_table
   # choose data set and create columns
    selected_applicants_cleaned <- reactive({
        
        variable_risk_factor1 <-  c( "program_desc","program_code","hs_ceeb","hs_name","hs_cnty","hs_foreign_lang",
                                     "hs_offer_gpa","transfer_hours_earned","acef_award",
                                     "addr1","addr2", "city",
                                     "state",
                                     "zip", "cnty","dob")
        
        variable_risk_factor2 <-  c( "program_desc","program_code","hs_ceeb","hs_name","hs_cnty","hs_foreign_lang",
                                     "hs_offer_gpa","transfer_hours_earned",#"acef_award",
                                     "addr1","addr2", "city",
                                     "st",
                                     "zip", "cnty","dob")
 
         #checkboxgroup data
        ### Admissions
        if(input$checkdataname == 1){
            new_applicatns_add_data  <-  new_applicants_data() %>% 
                janitor::clean_names() %>% 
                filter(hs_offer_gpa > 0.00) %>% 
                filter(!is.na(hs_offer_gpa)) %>%
                filter(!is.na(hs_foreign_lang)) %>%
                #mutate(transfer_hours_earned = trans_hours_earned) %>% # wait to fix the header name
                mutate(transfer_hours_earned = ifelse(is.na(transfer_hours_earned), 0, transfer_hours_earned)) %>%
                select(contains("uwf"),gtsummary::all_of(variable_risk_factor1))   
                
          ### Banner  
         }else if(input$checkdataname == 2){
            admtype=c("UR","UD","UI")
            stutype=c("E","B")
            ne_applicatns_add_data  <-  new_applicants_data() %>% 
                janitor::clean_names() %>% 
                filter(admit_type %in% admtype) %>% 
                filter(studtyp %in% stutype) %>% 
                filter(hs_offer_gpa > 0.00) %>% 
                 filter(!is.na(hs_offer_gpa)) %>%
                filter(!is.na(hs_foreign_lang)) %>%
                mutate(transfer_hours_earned = ifelse(is.na(transfer_hours_earned), 0, transfer_hours_earned)) %>% 
                select(contains("uwf"),gtsummary::all_of(variable_risk_factor2))  %>% 
                mutate("state"=st, acef_award = rep(NA))
         }
    })
    
    # uploaded data coding headers
        new_applicants_cleaned <- reactive({
            # from help hs_prog_apr_table
            hs_rate <- hs_prog_apr_help_table %>%   select(hs_ceeb, hs_apr_rate) %>% 
                unique()
            prog_rate <- hs_prog_apr_help_table %>%   select(program_code, prog_apr_rate)  %>% 
                unique() 
    #Banner_Application_Load_2_2_2022 <- read_csv("//argo.uwf.edu/its/datahubprod/satactlists/Admitted Student Files/Banner Application Load 2.2.2022.csv")
    #    test <-   Banner_Application_Load_2_2_2022      
       new_data <-  selected_applicants_cleaned() %>% 
            mutate(hs_ceeb = as.character(hs_ceeb)) %>% 
            mutate(hs_ceeb = ifelse(is.na(hs_ceeb),"Not Available", hs_ceeb)) %>% # apr 0.7
            mutate_at(c("hs_ceeb","hs_name","program_code","program_desc"), replace_na, "Not Available") %>%
            left_join(hs_rate, by="hs_ceeb" ) %>% # from unique valus
            mutate(HS_New  = ifelse(is.na(hs_apr_rate), "Y","N")) %>% # new column
            left_join(prog_rate, by="program_code") %>% # from uniqu values
            mutate_at(c("hs_apr_rate","prog_apr_rate"), replace_na, 0.800) %>% # replace 0.8 for new hs and prog apr rate
            mutate(HS_PROG_APR = (hs_apr_rate + prog_apr_rate)/2) %>% # new column
            mutate(HS_Program_Index  = cut(HS_PROG_APR, breaks=c(0, 0.7499, 0.7999, 0.8499, 1.1 ),
                                        labels = c("<0.75", "[0.75, 0.80)", "[0.80, 0.85)","0.85+")))  %>% # new column
            mutate("HS_GPA" = hs_offer_gpa, "Foreign_Lang"  = hs_foreign_lang ) %>%  # new column
            mutate(across(where(is.character), as.factor))
            
    })
    

    #raw data after filtered undergraduate students FTIC
    output$new_stu_data <- renderDataTable({
        
        my_new_data <- selected_applicants_cleaned()  %>% janitor::clean_names()
        datatable(my_new_data)
      

    })
    
    output$cleandatadim <- renderText({
        
        dim <- dim(selected_applicants_cleaned())
        paste("Number of rows in processed data: ", dim[1])
     
    })
   
  ### Modeling
    model_pre <- reactive({
        #Gender (Male/Female)
        risk_apr_filtered <-  risk_apr_data_set1 %>% 
            filter(Cohort <= 2020)  %>% 
            mutate(APR= as.factor(APR)) %>% 
            mutate(across(where(is.character), as.factor))
          #mm2 <- 
        set.seed(123)
        model_pre <-  glm(APR ~ HS_New  + HS_GPA  + HS_Program_Index  + Foreign_Lang , data= risk_apr_filtered, family = "binomial")
     })
    

   ### Prediction data
    prob_pre_factor <- reactive({
        Prob_pre <-   predict(model_pre(), new_applicants_cleaned(), type="response")

    })
   
    result1 <- reactive({
        
         result1 <- cbind( new_applicants_cleaned() ,"prob_prefac"=prob_pre_factor() )  %>% 
                    mutate(risk_prefac = ifelse(prob_prefac >= 0.9000, "Low",
                                    ifelse(prob_prefac >= 0.8000, "Moderate 1", 
                                           ifelse(prob_prefac >= 0.7000, "Moderate 2", "High")))) %>% 
                    mutate(risk_prefac= factor(risk_prefac,   levels = c("High","Moderate 2","Moderate 1","Low"))) %>%
                    mutate(risk_rank = ifelse(prob_prefac >= 0.9000, 1,
                                         ifelse(prob_prefac >= 0.8000, 2, 
                                                ifelse(prob_prefac >= 0.7000, 3, 4)))) %>% 
                    relocate(risk_prefac, .after = prob_prefac) %>% 
                    relocate(risk_rank, .after = prob_prefac) %>%
                    janitor::clean_names() %>% 
                    mutate(prob_prefac = round(prob_prefac, 4))
    })
    
    predicted_new_app <- reactive({
        #col_names <- names(new_applicants_data()) %>% janitor::make_clean_names() # original data set headers
     output_varis <-  c( "program_desc","program_code","hs_ceeb","hs_name","hs_cnty","hs_foreign_lang",
                                     "hs_offer_gpa","transfer_hours_earned","acef_award", "addr1","addr2", "city","state",
                                     "zip", "cnty","dob") # trans_hours_earned and state
        # change column name
         predicted <- result1() %>% 
            select(1, gtsummary::all_of(output_varis), "soatest_code_aar1"=prob_prefac,"soatest_code_aar2"= risk_rank, risk_prefac)
    })
    
    
         # data table
    output$pred_new_applicants <- DT::renderDataTable({
        
        pred_datatable <-  datatable(predicted_new_app(),
               #extensions = "Buttons",
               caption = tags$caption("Risk-level:Low(1) =0.90+;Moderate1(2)=[0.80,0.90);Moderate2(3)=[0.70,0.80);High(4)=0.70-"),
               filter = "top") %>% 
               #options = list(dom="Blfrtip",
               #               buttons=c("copy","csv","excel","pdf","print"), 
               #               lengthMenu=list(c(10,25,50,-1), c(10,25,50,"All")),pageLength=25)) %>%
         formatRound('soatest_code_aar1', digits = 4) %>% 
         formatStyle('soatest_code_aar1',  
                     # align
                     color = styleInterval(c(0.6999, 0.7999, 0.8999), c('#D117E7','#E76917', '#1795E7','#2DE717')), 
                     backgroundColor = styleInterval(0.5, c('white', 'white')))
            # print("Make sure the data inlcudes uwfid, hs_ceeb, hs_name,hs_cnty, hs_foreign_lang,
            #       hs_offer_gpa, transfer_hours_earned,addr1, addr2, city, zip, cnty, dob")
            # 
     
  
   })
    
   # downloadriskdata
   output$downloadriskdata <- downloadHandler(
       
       filename = function(){paste0( "Pred_risk_new_Applicants ",  Sys.Date(), ".csv", sep="")},
       content = function(file){
           write.csv(predicted_new_app(), file, row.names = FALSE)
       }
   )
   
    ### Predicting academic success 
    ### risk-apr analysis
    risk_apr_fitting_model <- reactive({
        
        #Y <- risk_apr_data_set %>% filter(Cohort <= 2020) %>% select(input$selectdependent) %>% names()
        risk_X <- risk_apr_data_set1 %>%  
            mutate(Award_Merit = ifelse(Award_Merit =="ACEX", "ACEF", Award_Merit)) %>% 
            filter(Cohort <= 2020) %>% select(APR,input$selectvariables) %>% names()
        f <- as.formula(paste("APR ~", paste(risk_X[!risk_X %in% "APR"], collapse = " + ")))
        
        risk_apr_filtered <-  risk_apr_data_set1 %>% filter(Cohort <= 2020)  %>% mutate(APR= as.factor(APR)) %>% 
            #mutate(HS_PROGRAM_Index = relevel(HS_PROGRAM_Index, ref="<0.75")) %>% 
            mutate(across(where(is.character), as.factor))
        
        glm(f, data= risk_apr_filtered, family = "binomial")
        #saveRDS(fit_m1, "fit_m1.rds")
        
    })
    #model risk-apr
    output$risk_apr_fitting <- renderPrint(risk_apr_fitting_model())
    
    output$risk_ORplot <- renderPlot({
        
        ORplot(risk_apr_fitting_model(), type = 2, show.CI = TRUE, main = "Odds Ratio & CI", sig.level = 0.05, pch = 1, cex = 2, lwd = 4, col = c("blue","red"))
    })
    
    #risk summary table
    output$risk_apr_summary <- renderDT({
        
        extractOR(risk_apr_fitting_model())  
        
    })
    
    #risk-apr variable important
    
    output$risk_apr_variimp <- renderEcharts4r({ # renderPlotly({
        
        risk_imp <- caret::varImp(risk_apr_fitting_model(), scale = FALSE)
        #risk_imp <- caret::varImp(m9, scale = FALSE)
        Variable <- rownames(risk_imp)
        vi <- risk_imp[[1]]
        vi <- reshape2::melt(vi)
        
        imp_data <- cbind(Variable, vi) %>% as.data.frame() %>% arrange(vi) %>% mutate(value = round(value,4))
        
     # p <-  ggplot(imp_data, aes(x = value, y = Variable, col = Variable)) + geom_point(size = 4) + theme_bw()
     #fig <- plot_ly(data = imp_data, x = ~value, y = ~Variable,type="scatter", color = ~Variable)
        
     imp_data %>% e_chart(Variable) %>% e_scatter(value , symbol_size=8 ) %>% 
         e_grid(top=150, bottom = "16%") %>% 
         e_tooltip(trigger = c("item","axis")) %>% 
         e_title("Variable Importance", left="center",top=50,  textStyle = list(fontSize = 20, fontFamily='Helvetica')) %>% 
         e_labels(FontSize =9, position = "right",  color="#009CDE",fontWeight = "normal",
                  formatter = htmlwidgets::JS("function(params){
                                                 return(params.value[1] + '')}") )  %>% 
                 e_flip_coords()
         
    })
    
    #confusion matrix for risk apr
    risk_apr_filtered_train <- reactive({
        #train data
        risk_apr_filtered_train <-  risk_apr_data_set1 %>% 
            mutate(Award_Merit = ifelse(Award_Merit =="ACEX", "ACEF", Award_Merit)) %>% 
            filter(Cohort <= 2020)  %>% mutate(APR= as.factor(APR)) %>% 
            mutate(across(where(is.character), as.factor))
    })
    
    Prob_APR_train <- reactive({   
        #predicton on train set
        Prob_APR_train    <- predict(risk_apr_fitting_model(), risk_apr_filtered_train(), type="response")
        
    })
            
     output$risk_apr_crosstab <- renderPlot({     
            #comp set
            risk_ftic_data_train <- cbind(risk_apr_filtered_train(),  "Prob"=Prob_APR_train()) %>% 
                mutate(Actual = APR, Predict = ifelse(Prob >= 0.5, "Yes","No"))
            tab <- table(risk_ftic_data_train$Actual, risk_ftic_data_train$Predict)
     
         tab <- tab/ sum(tab) # perdentage based on overall
         overall_tab <- sum(diag(tab))/sum(tab)
         tab <- as.data.frame(tab, stringAsFactors = TRUE) # data frame default (Var1,Var2,Freq)
         tab$Var2 <- factor(tab$Var2, rev(levels(tab$Var2))) # levels for predict for y axis
         #print(overall_tab)
         p <-    ggplot(tab, aes(Var1, Var2, fill = Freq)) 
         p <- p  +   geom_tile()   +
            geom_text(size= 9, aes(label = scales::percent(Freq))) +
            scale_fill_gradient(low = "white", high = "#3575b5") +
            labs(x = "Actual", y = "Predict", title = "Confusion Matrix of APR", 
                 fill = "Accuracy")  +
            theme(plot.title = element_text(size = 25, hjust = 0.5,
                                            margin = margin(20, 0, 20, 0)),
                  legend.title = element_text(size = 18, margin = margin(0, 20, 10, 0)),
                  axis.title.x = element_text(margin = margin(20, 20, 20, 20), size = 22),
                  axis.title.y = element_text(margin = margin(0, 20, 0, 10), size = 22))
         print(p)
        
    })
    
    
    
    #recall model
    #fit_model <- readRDS("fit_m1.rds")
    #new data set
    risk_apr_new_data <- reactive({ 
        
        new_risk_df <-  risk_apr_data_set1 %>% 
            filter(Cohort >= 2021) %>% 
            mutate(Award_ACEF  = ifelse(is.na(Award_ACEF ), "N", Award_ACEF )) %>% 
            mutate(across(where(is.character), as.factor)) %>% 
            mutate(uwfid = as.factor(uwfid), Cohort = as.factor(Cohort)) 
    })
    
    risk_apr_pred <- reactive({
        
        #prediction
        Prob_APR <- predict(risk_apr_fitting_model(), risk_apr_new_data(), type="response")
    })
    
    #predicted new ftic   
    risk_ftic_data <- reactive({  
        
        #test here
        risk_ftic_data_train <- cbind(risk_apr_filtered_train(),  "Prob"=Prob_APR_train()) %>% 
            mutate(Prob = round(Prob,4)) %>% 
            mutate(Risk_Level = ifelse(Prob >= 0.9000, "Low",
                                       ifelse(Prob >= 0.8000, "Moderate 1", 
                                              ifelse(Prob >= 0.7000, "Moderate 2", "High")))) %>% 
            mutate(Risk_Level=factor(Risk_Level,   levels = c("High","Moderate 2","Moderate 1","Low"))) %>% 
            select( -GPA_APR, -15)
        #data table
        risk_ftic_data <- cbind(risk_apr_new_data(),  "Prob"=risk_apr_pred())  %>% 
            mutate(Prob = round(Prob,4)) %>% 
            mutate(Risk_Level = ifelse(Prob >= 0.9000, "Low",
                                       ifelse(Prob >= 0.8000, "Moderate 1", 
                                              ifelse(Prob >= 0.7000, "Moderate 2", "High")))) %>% 
            mutate(Risk_Level=factor(Risk_Level,   levels = c("High","Moderate 2","Moderate 1","Low"))) %>% 
                       select( -GPA_APR, -15)
        final_dt <- rbind(risk_ftic_data_train, risk_ftic_data)
    })
        #export table
     output$risk_apr_level_ftic <- renderDT({ 
         risk_stu_2021to2022 <- risk_ftic_data() %>% filter(Cohort ==2021) %>% mutate(Cohort = as.factor(Cohort))
        datatable(risk_stu_2021to2022,  extensions = "Buttons",
                  caption = tags$caption("Risk-level:Low=0.90+;Moderate1=[0.80,0.90);Moderate2=[0.70,0.80);High=0.70-"),
                  filter = "top",
                  options = list(dom="Blfrtip",
                                 buttons=c("copy","csv","excel","pdf","print"), 
                                 lengthMenu=list(c(10,25,50,-1), c(10,25,50,"All")),pageLength=25)) %>%
            formatRound('Prob', digits = 4) %>% 
            formatStyle('Prob',  
                        # align
                        color = styleInterval(c(0.6999, 0.7999, 0.8999), c('#D117E7','#E76917', '#1795E7','#2DE717')), 
                        backgroundColor = styleInterval(0.5, c('white', 'white')))
        
        
    })
     
    
    output$ftic_2021_apr <- renderDT({
        
        ftic2021 <-  risk_ftic_data() %>% filter(Cohort == 2021) %>% mutate(APR = ifelse(is.na(APR), "APR_NA", "APR"))
        tab2021 <- table(ftic2021$Risk_Level, ftic2021$APR)
        
        tab2 <- as.data.frame(tab2021, stringAsFactors = TRUE) # data frame default (Var1,Var2,Freq)
        tab2$Var2 <- factor(tab2$Var2, rev(levels(tab2$Var2)))
        tab3 <- tab2 %>% group_by(Var1) %>% pivot_wider(names_from = Var2, values_from = Freq) %>% 
            mutate(SubTotal =  sum(APR,APR_NA), APR_pct = paste0(round(APR/SubTotal*100,2), "%", sep="")) 
       tab_final <-  
           rbind(tab3, data.frame(Var1='Total', APR=sum(tab3$APR), APR_NA  = sum(tab3$APR_NA ), SubTotal =sum(tab3$SubTotal), 
                               APR_pct = paste0(round(sum(tab3$APR)/sum(tab3$SubTotal)*100,2), "%", sep ="")))
       names(tab_final) <- c("Pred_Risk_Level","APR(GPA>=2.00)","Non-APR","SubTotal","Max APR %")
       tab_final_df <- datatable(tab_final, caption = "2021 FTIC GPA APR: at least 2.0 GPA after spring semester") 

    })
    
     ftic_all_prob <- reactive({
        
        risk_ftic_data_train <- cbind(risk_apr_filtered_train(),  "Prob"=Prob_APR_train()) %>% 
        mutate(Prob = round(Prob,4)) %>% 
            mutate(Risk_Level = ifelse(Prob >= 0.9000, "Low",
                                       ifelse(Prob >= 0.8000, "Moderate 1", 
                                              ifelse(Prob >= 0.7000, "Moderate 2", "High")))) %>% 
            mutate(Risk_Level=  factor(Risk_Level, levels = c("High","Moderate 2","Moderate 1","Low")))
        
        ftic2017 <-  risk_ftic_data_train #%>% filter(Cohort == 2017)
     })
        
        output$risk_apr_hsgpa_all <- renderDT({
            
        #     ftic2017 <- ftic_all_prob() %>% filter(Cohort == 2017) %>% 
        #     mutate(APR2 = ifelse(APR=="No" | is.na(APR), "APR_NA", "APR"))
        #  # APR tab   
        # tab2017 <- table(ftic2017$Risk_Level, ftic2017$APR2)
        # tab2 <- as.data.frame(tab2017, stringAsFactors = TRUE) # data frame default (Var1,Var2,Freq)
        # tab2$Var2 <- factor(tab2$Var2, rev(levels(tab2$Var2)))
        # tab3 <- tab2 %>% group_by(Var1) %>% pivot_wider(names_from = Var2, values_from = Freq) %>% 
        #     mutate(SubTotal =  sum(APR,APR_NA), APR_pct = paste0(round(APR/SubTotal*100,2), "%", sep="")) 
        # 
        # tab_final <-  
        #     rbind(tab3, data.frame(Var1='Total', APR=sum(tab3$APR), APR_NA  = sum(tab3$APR_NA ), SubTotal =sum(tab3$SubTotal), 
        #                            APR_pct = paste0(round(sum(tab3$APR)/sum(tab3$SubTotal)*100,2), "%", sep ="")))
        # 
        # # modify colnames
        # names(tab_final) <- c("Pred_Risk_Level","APR","Non-APR","SubTotal","Act APR %")
        # tab_final_df2 <- datatable(tab_final, caption = "2017 FTIC Risk-level vs. Actual APR") 
        
        risk_apr <- ftic_all_prob() %>% mutate(APR2 = ifelse(APR=="No" | is.na(APR), "Non_APR", "APR")) %>% 
            group_by(Cohort,Risk_Level,APR2) %>% summarise(Count=n(), .groups = "drop") %>% 
            pivot_wider(names_from = APR2, values_from = Count) %>% 
            mutate(SubTotal = (APR+Non_APR), Act_APR = paste0(round(APR/SubTotal*100,2), "%",sep=""))
       
        
        risk_apr_hsgpa <- ftic_all_prob() %>% mutate(APR2 = ifelse(APR=="No" | is.na(APR), "Non_APR_HSGPA", "APR_HSGPA")) %>% 
            group_by(Cohort,Risk_Level,APR2) %>% summarise(AVE_HS_GPA = round(mean(HS_GPA, na.rm=T),2), .groups = "drop") %>% 
            pivot_wider(names_from = APR2, values_from = AVE_HS_GPA) 
            # mutate(SubTotal = (APR+Non_APR), Act_APR = paste0(round(APR/SubTotal*100,2), "%",sep=""))
        
        risk_apr_hsgpa_all <- merge(risk_apr_hsgpa, risk_apr, by=c("Cohort","Risk_Level"),all.x=TRUE) %>% 
            mutate(Risk_Level=  factor(Risk_Level, levels = c("High","Moderate 2","Moderate 1","Low")))  
            
        
        datatable(risk_apr_hsgpa_all)
        
        
    })
        

        
        
        output$HSGPA_risklevel <- renderDT({
            
            gpa_risk_tab <-  ftic2017_predicted_act_data() %>% 
                group_by(cohort, Risk_Level, APR) %>% 
                summarise(count=n(), AVE_HS_GPA = round(mean(HS_GPA, na.rm=T),2), .groups = "drop") %>% 
                filter(!is.na(cohort))
            names(gpa_risk_tab) <- c("Cohort","Risk_Level","APR","Count","AVE_HS_GPA")
            datatable(gpa_risk_tab)
            
        })
        
        
        
         ftic2017_predicted_act_data <- reactive({
            # getting demo info from enc_data
            ftic2017_add <- enc_data_ds1 %>% filter( uwf_term_id2 =="Fall_1") %>% 
                select(uwfid,"Entry_Program"=program_desc_abbs2 ,"Graduation_Time"=gradu_time ,"Graduation_Status"=graduation_status,
                        deg_program_desc_abbs2 ,stu_county ,stu_first_gen_ind,stu_residence_hall,stu_state ,"HS_GPA2"=hsgpa,cohort,
                       "Fee_Residency"=fee_residency     )
            #merge with risk apr data set APR (Yes/No)
            summary_ftic2017_demo <- merge(ftic_all_prob(),ftic2017_add, by="uwfid",all.x=TRUE)
            
            tabsum <- summary_ftic2017_demo %>%  
                mutate(Risk_Level =  factor(Risk_Level, levels = c("High","Moderate 2","Moderate 1","Low")),
                       APR = ifelse(APR=="Yes","APR","Non-APR")) %>% 
                mutate(HS_Program_Index =  factor(HS_Program_Index, levels = c("<0.75","[0.75, 0.80)","[0.80, 0.85)","0.85+")))
            
            })
         
         
         output$risk_bar_plot <- renderPlotly({
     
              mycolor <- c("#81898D","#8DC8E8","#8D9BE8")
             # p <- ftic2017_predicted_act_data() %>% filter(cohort == 2017) %>% 
             #     mutate(Risk_Level =factor(Risk_Level), Graduation_Status = factor(Graduation_Status))   
             # pp <-   ggplot(p, aes(x=Risk_Level,  fill = Graduation_Status)) + 
             #     geom_bar() + labs(title = "Garudation Status by Risk-Level",fill="Graduation Status") +
             #     xlab("Risk Level") + scale_fill_manual(values = mycolor)    
             fig <- ftic2017_predicted_act_data() %>% 
                mutate(Graduation_Status = factor(Graduation_Status, levels = c("OnTime","OverTime","NoDegree"))) %>% 
                 filter(cohort == 2017) %>% count(Risk_Level, Graduation_Status)
             fig <- fig %>% plot_ly(x= ~Risk_Level, y =~n,type="bar", color =~Graduation_Status)
            
             
         })
         
         output$ftic2017_gradu_status <- render_gt({
             
             ftic2017_predicted_act_data() %>% filter(cohort == 2017 ) %>% 
                 select(HS_GPA2, APR, Risk_Level, Gender, Graduation_Status  ) %>%
                  tbl_summary( by = Risk_Level,
                              statistic = all_continuous() ~ "{mean} ({sd}) {min} {max}",
                              missing = "no",
                              percent = "col",
                              digits = all_categorical() ~ c(0,1)) %>%  
                 add_n() %>% bold_labels() %>%  as_gt()
             
         })
        
        
        output$ftic2017_risk_all <- render_gt({
            
           high <-  ftic2017_predicted_act_data() %>% filter(cohort == 2017 ) %>% 
               select(HS_GPA, APR,Risk_Level,Graduation_Status, HS_Program_Index,Gender ) %>% 
               filter(Risk_Level =="High") %>% select(-Risk_Level) %>% 
            tbl_summary( by =APR,
                                    statistic = all_continuous() ~ "{mean} ({sd}) {min} {max}",
                                    missing = "no",
                                    percent = "row",
                                    digits = all_categorical() ~ c(0,1)) %>%  
                 add_n() %>% bold_labels() 
       
          m2 <-   ftic2017_predicted_act_data() %>% filter(cohort == 2017 ) %>% 
              select(HS_GPA, APR,Risk_Level,Graduation_Status,HS_Program_Index, Gender) %>%
              filter(Risk_Level =="Moderate 2") %>% select(-Risk_Level) %>% 
                tbl_summary( by =APR,
                             statistic = all_continuous() ~ "{mean} ({sd}) {min} {max}",
                             missing = "no",
                             percent = "row",
                             digits = all_categorical() ~ c(0,1)) %>%  
                add_n() %>% bold_labels() 
          m1 <-   ftic2017_predicted_act_data() %>% filter(cohort == 2017 ) %>% 
              select(HS_GPA,APR,Risk_Level,Graduation_Status,Gender,HS_Program_Index ) %>%
              filter(Risk_Level =="Moderate 1") %>% select(-Risk_Level) %>% 
              tbl_summary( by =APR,
                           statistic = all_continuous() ~ "{mean} ({sd}) {min} {max}",
                           missing = "no",
                           percent = "row",
                           digits = all_categorical() ~ c(0,1)) %>%  
              add_n() %>% bold_labels() 
          low <-   ftic2017_predicted_act_data() %>% filter(cohort == 2017 ) %>% 
              select(HS_GPA,APR,Risk_Level,Graduation_Status,Gender, HS_Program_Index) %>%
              filter(Risk_Level =="Low") %>% select(-Risk_Level) %>% 
              tbl_summary( by =APR,
                           statistic = all_continuous() ~ "{mean} ({sd}) {min} {max}",
                           missing = "no",
                           percent = "row",
                           digits = all_categorical() ~ c(0,1)) %>%  
              add_n() %>% bold_labels() 
                 
          
          tbl_merge(tbls = list(high, m2, m1,low), tab_spanner = c("**High Risk**","**Moderate2**","**Moderate1**","**Low Risk**")) %>% 
              #modify_spanning_header(everything() ~ NA_character_) %>%   
              as_gt()
            
        })
        
        ### high school and entry program index
        
        output$hs_prog_index_table <- renderDT({
            hs_prog_index_tab <- ftic2017_predicted_act_data() %>% group_by(Cohort, Risk_Level,HS_Program_Index, APR) %>% 
                summarise(Count=n(), AVE_HSGPA=round(mean(HS_GPA, na.rm=TRUE),2))
          datatable(hs_prog_index_tab)
        })
        output$hs_prog_index_unique <- renderDT({
           
           tbs2 <- hs_prog_apr_help_table %>% filter(Cohort < 2021) %>% # 2017 to 2020 cohrot
               select(hs_name,HS_APR ,hs_apr_rate,program_desc,Prog_APR,prog_apr_rate, APR,HS_GPA ) %>% 
               filter(!is.na(hs_name)) %>% filter(!is.na(program_desc)) %>% filter(!is.na(hs_apr_rate)) %>% 
              #mutate(hs_name = ifelse(is.na(hs_name), "Missing", hs_name), program_desc = ifelse(is.na(program_desc), "Missing", program_desc))  %>%
               mutate(hs_apr_rate= ifelse(hs_apr_rate <= 0.2, 0.2, hs_apr_rate)) %>% 
               mutate(HS_PROG_APR = (hs_apr_rate + prog_apr_rate)/2) %>%
               mutate(HS_Program_Index2 = cut(HS_PROG_APR, breaks=c(0, 0.7499, 0.7999, 0.8499, 1.1 ), labels = c("<0.75", "[0.75, 0.80)", "[0.80, 0.85)","0.85+"))) %>%
               group_by(hs_name,HS_APR ,hs_apr_rate,program_desc,Prog_APR,prog_apr_rate, HS_Program_Index2,APR ) %>%
               summarise(Count=n(),AVE_HS_GPA = round(mean(HS_GPA,na.rm=TRUE),2), .groups = "drop") #%>% 
               # #unique() %>% 
                #arrange(hs_name ,program_desc, APR )  
               # pivot_wider(names_from = APR, values_from = c(AVE_HS_GPA)) %>% filter(!duplicated(hs_name, program_desc))
           names(tbs2) <- c("HS_Name","HS_APR_C","HS_APR_%", "App_Program","Prog_APR_C","Prog_APR_%" ,"HS_Program_Index", "APR","Count", "AVE_HSGPA")
           datatable(tbs2, extensions = "Buttons",
                     filter = "top",
                     options = list(dom="Blfrtip",
                                    buttons=c("copy","csv","excel","pdf","print"), 
                                    lengthMenu=list(c(10,25,50,-1), c(10,25,50,"All")),pageLength=25)) %>% 
               formatPercentage(c("HS_APR_%","Prog_APR_%"),   digits = 2)
           
            
        })
   
    ### High shcool name and apr and stopouts
        #### Top 10 hs name by apr and stopouts
        hs_name_data_table <- reactive({
            
         hs_name_data_table <-    apr_stopout_hs_name_data %>% 
             group_by(cohort) %>% 
             mutate(cohort_sum = sum(total)) %>% 
             mutate(hs_pct=total/cohort_sum) %>% 
             group_by(cohort) %>% 
             mutate(rank_total = rank(-total)) %>% 
             mutate(rank_apr_y = rank(-apr_y)) %>% 
             arrange(rank_total, rank_apr_y)
        })
                
         output$apr_hsname_table <- renderDT({
             
            datatable(hs_name_data_table(), extensions = "Buttons",
                      filter = "top",
                      options = list(dom="Blfrtip",
                                     buttons=c("copy","csv","excel","pdf","print"), 
                                     lengthMenu=list(c(10,25,50,-1), c(10,25,50,"All")),pageLength=25)) %>% 
                formatPercentage(c("apr_pct","stop2_0_non_apr_pct","hs_pct"),digits = 2)
                  
        })
        
        #### High school name and APR (stop-outs>=2.00) - plot used echarts4r
         top10hsname <- reactive({
             
             hs_name_data_table() %>% filter(rank_apr_y <= 10 | rank_apr_y <= 10)
             
         })
         
         observeEvent(top10hsname(), {
             choice <-  sort(unique(top10hsname()$hs_name))
             shiny::updateSelectInput(inputId ="hs_name_rank", choices = choice)
             #shiny::updateSelectInput(inputId ="movefinal", choices = choice)
         })
         
        output$hsname_enroll_plot <- renderEcharts4r({
            
           hs_name_enroll <-  hs_name_data_table() %>% 
               group_by(cohort) %>% 
               mutate(cohort_sum = sum(total)) %>% 
               mutate(hs_pct = total/cohort_sum)
           
           hs_name_enroll %>%  filter(hs_name == input$hs_name_rank) %>% # "Gulf Breeze High School") %>%
               arrange(cohort) %>% 
                e_charts(cohort) %>% 
                e_bar(total, name = "Enrolled") %>% 
                e_bar(apr_y , name = "APR") %>% 
                e_bar(apr_n, name = "Non-APR") %>% 
                e_bar(stop2_00, name = "Stopout>=2.00") %>% 
                e_bar(serie= stop2_0_non_apr_pct , name = "Stopout>=2.00 (%)",y_index=1,stack="grp1"  ) %>% 
                e_grid(top=150, bottom = "16%") %>% 
                e_tooltip(trigger = c("item","axis")) %>% 
                e_title(paste0( "APR and Stopout"),
                       subtext=paste0("by:",input$hs_name_rank),
                       left="center",top=50,  
                       textStyle = list(fontSize = 20, fontFamily='Helvetica')) %>% 
                e_labels(FontSize =9)
           # plot_ly(x = ~cohort, y =~total, color = ~cohort, type="bar") %>% #,  colors = c("#DE3163","#004C97","#004C97") ) %>%  
           # layout( xaxis = list(title ="Cohort", titlefont = list(size = 12, color="blue")),
           #         yaxis = list(title ="Enrolled (Count)",titlefont = list(size = 12, color="green")),
           #         barmode ="group",
           #         legend=(list(title=list(text='<b> Cohort </b>'))))  
            
            
        })
    
    ### Financial data
    
    fin_pivot_data <- fin_ftic_all_data_selected_codes %>% 
        select(1,2,3,year_amount,selected_codes,Stu_College,Stu_Department,Stu_ProgramCIPDesc,gender,ethnicity,
               firstgen,Graduation_Status, tier,APR,detail_code,detail_code_desc,award_cond_id)
    
    
    output$fin_overallpivot <-  renderRpivotTable({
        
        rpivotTable(
            data = fin_pivot_data,
            rows = c("selected_codes" ),
            cols = c("Cohort","aid_year"),
            aggregatorName = "Sum",
            vals = "year_amount",
            rendererName = "Table"
        )
        
        
    })
    
    
    
    
    
    ### Explore Tier characteristics using apriori algorithm
    
    filtered_df <-  reactive({
        
        tricounty <- c("Escambia", "Santa Rosa", "Okaloosa")
        nonapr=c("No","Withdrawn")
        
        selected_data <- student_character_1stFall  %>%  
            filter(FTIC_FeeResidency == "Florida Resident (USA)" & HS_CNTY %in% tricounty) %>% #filtered data
            mutate(fall1GPA1= ifelse(FTIC_APRCT %in% nonapr, "Below 2.00", "Above 2.00")) %>% 
            mutate(fall1GPA1 = as.character(fall1GPA1)) %>% 
            mutate(HS_NAME2 = as.character(HS_NAME)) %>% 
            mutate(App_Month = format(as.Date(APP_DT), "%m")) %>% 
            mutate(app_time=ifelse(App_Month >= 12 & App_Month <= 10, "Oct. to Dec.",
                                   ifelse(App_Month >= 01 & App_Month <=05, "Jan. to May", "June to Sept."))) %>% 
            mutate(Age = ifelse( FTIC_Age <= 18, "<=18",">18")) %>% 
            mutate(Awared_PELL = ifelse(PELLGrant == 0, "None", ifelse(PELLGrant <= 2000, "($0, $2,000]", "$2,000+"))) %>%
            mutate(TotalAidnotpell = ifelse(TotalFinAid_Term -PELLGrant >= 0, (TotalFinAid_Term -PELLGrant), 0 ),
                   TotalAidnpell = round(TotalAidnotpell,0)) %>% 
            mutate(TotalAidNotPell_Amount = cut(TotalAidnotpell, breaks = c(0, 1000, 2000,3000, 10000), labels = c("< $1,000", "< $2,000", "< $3,000",">= $3,000"))) %>% 
            #mutate(TotalAid_NotPellorMerit = ifelse(AidNotPellMerit <= 0, "No", "Yes")) %>% 
            #mutate(OtherFinAidSupport = ifelse(AidNotPellMerit == 0 , "NoAid", ifelse(AidNotPellMerit <= 3000, "Below$3,000", "Above$3,000"))) %>% 
            mutate(Pell_Amount = cut(PELLGrant, breaks = c(0, 1000, 2000,3000, 50000), labels = c("< $1,000", "< $2,000", "< $3,000",">= $3,000"))) %>%
            mutate(Awarded_Loan = ifelse(LoanTypes == 0, "No", "Yes" )) %>% 
            mutate(HS_NAME3 = ifelse(is.na(HS_NAME2), "Not available", HS_NAME2)) %>% 
            mutate(First_Generation = ifelse(Stu_FirstGenInd =="N", "No","Yes")) %>% 
            mutate(Merit_Scholarships = ifelse(ScholarshipsInstitutionalTypes== 0, "No","Yes")) %>% 
            mutate(BF_Scholarships = ifelse(ScholarshipsStateBFTypes == 0, "No","Yes")) %>%  
            filter(APPLICANT_TIER %in% input$selecttiers  ) %>% #filter tiers
            mutate(Prior_Hours=cut(FTIC_PriorHours, breaks=c(-1,0,2.99,11.99,26.99,999),labels = c("None","(1,3]","(3,12]","(12,27]","27+"))) %>% 
            mutate(HS_GPA = cut(Best_HSGPA,  breaks=c(-1, 3.59, 3.89, 4.19, 999),labels = c("<3.60","[3.60,3.89)","[3.89,4.19)","4.19+"))) %>%
            select("HS_Name"=HS_NAME3, app_time,  
                   HS_ENG_YRS,  
                   HS_MAT_YRS,  
                   #HS_NS_YRS, 
                   #HS_SS_YRS,
                   #HS_FL_YRS,
                   Prior_Hours,
                   HS_GPA,
                   "Entry_Program"=FTIC_ProgramCIPDesc,
                   First_Generation,
                   "FirstFallGPA"=fall1GPA1,
                   #FTIC_UWFGPACode,
                   FTIC_Cohort) %>% # remove cohort for freq.plot
            mutate_if(is.character, as.factor) #  %>% mutate_at(c(3:9), replace_na, "Missing") 
        
    })
    
    
    rules <- reactive({
        
        # import data set
        ruledata_no_cohort <-  filtered_df() %>% select(-FTIC_Cohort) 
        ruledata <-  
            as(ruledata_no_cohort , "transactions")
        
        #myruledata <- read.transactions("writedata1", sep=",", rm.duplicates = TRUE)
        
        rules_t1 <- apriori( data= ruledata,  
                             parameter = list(support = as.numeric(input$supp), confidence = as.numeric(input$conf), minlen=2, maxlen=3),
                             appearance = list(
                                 #rhs = c("FTIC_UWFGPACode=[0,1.99]", "FTIC_UWFGPACode=[2.00,2.76]", "FTIC_UWFGPACode=[2.77,4.00]"),
                                 rhs = c("FirstFallGPA=Below 2.00", "FirstFallGPA=Above 2.00"),
                                 default = "lhs")) 
        no_redundant_rules <- rules_t1[!is.redundant(rules_t1)]
        only_sig_rules <- no_redundant_rules[!is.significant(no_redundant_rules),]
        filterd_rules <- subset(only_sig_rules, subset = lift > input$lift)
        
        
    })
    
    
    
    
    output$filteredrules  <- renderDT({
        
        outrules <- inspectDT(head(sort(rules(), by="lift"),300))
        #rule_DF <-  DATAFRAME(rules(), setStart ='', setEnd ='',separate = TRUE)  # for more control with items
 
        
    })
    #gaph rules
    output$rulegraph <- renderVisNetwork({
        plot(rules(), method = "graph", engine ="htmlwidget")
    })
    
    output$gtsummarytable <- render_gt({
        
        tabsum <- filtered_df() %>% mutate(FirstFallGPA =  factor(FirstFallGPA)) 
        tabsum %>% tbl_summary( by =FirstFallGPA,
                                statistic = all_continuous() ~ "{mean} ({sd}) {min} {max}",
                                missing = "no",
                                percent = "row",
                                digits = all_categorical() ~ c(0,1)) %>%  
            add_n() %>% 
            modify_caption("**Table: FTIC Characteristics by First Fall GPA**") %>% 
            bold_labels() %>% as_gt()  
        
        
    })
    
    ### Sankey category  and entry or final
    movedata <- reactive({
        
        m_long <-  new_sankeydata_v1 %>% select(College, Department, Program, Cohort) %>% #filter(Cohort != 2021) %>% 
            tidyr::pivot_longer(c("College","Department","Program"), names_to = "level", values_to = "entry") %>% 
            filter(level == input$movelevel) %>% 
            data.frame()
        
    })
    
    observeEvent(movedata(), {
        choice <-  sort(unique(movedata()$entry))
        shiny::updateSelectInput(inputId ="moveentry", choices = choice)
        #shiny::updateSelectInput(inputId ="movefinal", choices = choice)
    })
    
    
    move.HSGPA <- reactive({
        sankey.hsgpa.filtered <- new_sankeydata_v1 %>%
            filter(Cohort == input$movecohort) %>% 
            mutate(GPA_HIGHSCHOOL = ifelse(is.na(GPA_HIGHSCHOOL),  0, GPA_HIGHSCHOOL)) %>% 
            mutate(response = ifelse(((College == input$moveentry) |(Department == input$moveentry)|(Program == input$moveentry)), "Yes", "No")) %>% 
            filter(response == "Yes")
    })
    
    observeEvent(move.HSGPA(), {
       no_zerohsgpa <-  new_sankeydata_v1 %>% mutate(GPA_HIGHSCHOOL= ifelse(GPA_HIGHSCHOOL == 0, NA, GPA_HIGHSCHOOL))
        minrange = min(no_zerohsgpa$GPA_HIGHSCHOOL, na.rm = T)
        maxrange = max(no_zerohsgpa$GPA_HIGHSCHOOL, na.rm = T)
        shiny::updateNumericInput(inputId ="sankeydirectionhsgpa", min = minrange, max= maxrange)
    })
    
    
    sankey_1st_data <- reactive({
        # entry data
        move1 <- new_sankeydata_v1  %>%  #filter(Cohort != 2021) %>%
            arrange(Stu_UWFID, Fall_ID) %>%
            dplyr::select(  Stu_UWFID, FALL_UWF,  "level"=input$movelevel,
                            Stu_Gender,Cohort, GPA_HIGHSCHOOL, Gradu_Time, TermGPA,EnteringFallGPA,APR)
        # degree info
        mov1_deg <- new_sankeydata_v1  %>%  #filter(Cohort != 2021) %>%
            dplyr::select( Stu_UWFID,  contains("deg") ) %>% group_by(DEGREECIPTitle) %>% ungroup() %>%
            select(Stu_UWFID, "College"=Deg_College, "Department"=Deg_Department,"Program"=Deg_ProgramCIPDesc) %>%
            filter(!duplicated(Stu_UWFID)) %>% select(Stu_UWFID, input$movelevel)
        #merge two
        two_sankey_data <- merge(move1,mov1_deg, by="Stu_UWFID", all.x=T )
        
        #6854
        move2 <- two_sankey_data %>%   #remove level column
            filter(GPA_HIGHSCHOOL >= input$sankeydirectionhsgpa[1] &  GPA_HIGHSCHOOL <= input$sankeydirectionhsgpa[2]) %>% 
            tidyr::pivot_wider( names_from = FALL_UWF , values_from = c(level ,TermGPA,EnteringFallGPA )) %>%
            mutate(Fall1GPA =  ifelse( TermGPA_UWFFALL1 >= 2.77, "Above2.77 ","Below2.77 "),
                   Year1GPA = ifelse( EnteringFallGPA_UWFFALL2 >= 2.00, "Above2.00","Below2.00") ) %>% filter(!duplicated(Stu_UWFID)) %>%  #6851
            mutate_at(c(8:11), replace_na, "Stopout") %>% filter(!is.na(level_UWFFALL1))
        move3 <-  setNames(move2[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)],
                           c("ID","GEN", "Cohort","HSGPA","GRD","APR", "DEG",
                             "VAR1","VAR2","VAR3","VAR4",
                             "GPA1", "GPA2", "GPA3", "GPA4",
                             "UWF1","UWF2","UWF3","UWF4", "GPA","UWF"
                           ))
        
        #print(move3)
    })
    
    sankey_2nd_data <- reactive({             
        #test_flow_choose <- sankey_filtering_DF()[(sankey_filtering_DF()$Cohort == input$movecohort) & (sankey_filtering_DF()$VAR1 == input$moveentry),]    
        if (input$choosedirection =="Forward") {
            test_flow_choose <- sankey_1st_data()[(sankey_1st_data()$Cohort == input$movecohort & sankey_1st_data()$VAR1 == input$moveentry),] 
        } else{
            test_flow_choose <- sankey_1st_data()[(sankey_1st_data()$Cohort == input$movecohort & sankey_1st_data()$DEG == input$moveentry),] 
        }
    })
    sankey_3rd_data <- reactive({
        
        test_flow_filtered <- sankey_2nd_data() %>% 
            dplyr::mutate(   VAR1 =  paste0(VAR1, "_Y1", sep=""),
                             VAR2 =  paste0(VAR2, "_Y2", sep=""),
                             VAR3 =  paste0(VAR3, "_Y3", sep=""),
                             VAR4 =  paste0(VAR4, "_Y4", sep=""),
                             GPA  =  paste0(GPA,  "Tgp", sep=""),
                             APR  =  paste0(APR,  "apr", sep=""),
                             GEN  =  paste0(GEN,  "GEN", sep=""),
                             UWF  =  paste0(UWF,  "Fgp", sep=""),
                             DEG  =  paste0(DEG,  "DEG", sep=""),
                             GRD  =  paste0(GRD,   "GRD",sep=""))
        
        
        col1 <- test_flow_filtered %>%  
            group_by(VAR1, GEN ) %>%  dplyr::summarise(n =n(), .groups ="drop") %>%  
            ungroup() %>% arrange(-n) %>% select( In= 1, Out= 2, 3)
        
        col1_apr <- test_flow_filtered %>%  
            group_by(GEN, GPA ) %>%  dplyr::summarise(n =n(), .groups ="drop")  %>% arrange(-n) %>% select( In= 1, Out= 2, 3)
        
        col2 <- test_flow_filtered %>%   
            group_by(GPA,  APR ) %>%  dplyr::summarise(n =n(), .groups ="drop")  %>% arrange(-n) %>% select( In= 1, Out= 2, 3)
        
        col3 <- test_flow_filtered %>%  
            group_by(APR, VAR2) %>%  dplyr::summarise(n =n(), .groups ="drop")  %>% na.omit() %>% 
            arrange(-n) %>%  select( In= 1, Out= 2, 3)
        
        col4 <- test_flow_filtered %>%  
            group_by(VAR2, VAR3)  %>% dplyr::summarise(n =n(), .groups ="drop") %>% arrange(-n) %>%  select( In= 1, Out= 2, 3)
        
        # create flow APRto2
        col5 <- test_flow_filtered %>% 
            group_by(VAR3, VAR4) %>%  dplyr::summarise(n =n(), .groups ="drop")  %>% arrange(-n) %>%  select( In= 1, Out= 2, 3)
        
        #create flow2to3
        col6 <- test_flow_filtered %>% 
            group_by(VAR4, DEG ) %>% dplyr::summarise(n =n(), .groups ="drop")  %>% arrange(-n) %>%  select( In= 1, Out= 2, 3)
        
        col7 <- test_flow_filtered %>%
            group_by(DEG, GRD) %>%  dplyr::summarise(n =n(), .groups ="drop")  %>%  arrange(-n) %>%  select( In= 1, Out= 2, 3)
        
        #create data frame 
        
        #print(up_flow_data)
        if(input$movepath == "from 1 to 2"){
            flow_data <- rbind(  col1, col1_apr, col2, col3)
        }else if(input$movepath == "from 2 to 3"){
            flow_data <- rbind(  col4)
        }else if(input$movepath == "from 3 to 4"){
            flow_data <- rbind(   col5) 
        }else if(input$movepath == "from 4 to end"){
            flow_data <- rbind(   col6, col7) 
        }else if(input$movepath =="from 1 to end"){
            flow_data <- rbind(col1, col1_apr, col2, col3, col4, col5, col6, col7 ) 
        }
        
        #selected_flow <- rbind(col1, col1_apr, col2, col3) 
        
    })
    
    
    sankey_plot <- reactive({
        
        # create nodes and links
        #links
        nodes_FD <- sankey_3rd_data() %>%  select(In, Out) %>% 
            pivot_longer(c("In","Out"), names_to = "col_name",
                         values_to = "name_match") %>% select(-1) %>% distinct() %>% 
            mutate(name = str_sub( name_match, end=-4)) %>% as.data.frame()
        nodes_FD$nodegroup <-  as.factor( gsub(" ", "-",nodes_FD$name))
        
        # nodes
        plot_id_FD <- sankey_3rd_data() %>% 
            mutate( IDIn = match( In, nodes_FD$name_match)-1,
                    IDOut = match(Out, nodes_FD$name_match)-1,
                    Freq =n ) %>% as.data.frame()
        plot_id_FD$group <- sub(' .*', '', nodes_FD[plot_id_FD$IDIn + 1, 'name'])
        
        #sankey plot
        upsankey <-  sankeyNetwork(
            Links = plot_id_FD, Nodes = nodes_FD,
            Source = "IDIn",
            Target = "IDOut",
            Value = "Freq",
            NodeID = "name",
            #NodeGroup = "nodegroup",
            LinkGroup = "group",
            sinksRight = F, iterations = 0,
            fontSize =  12, fontFamily = "Helvetica",
            nodeWidth = 20, nodePadding = 20
        )
         
    })
    #sankey plot and col names
    #,"3rd Fall","4th Fall","Degree","Graduation Time
    sankey_plot_cols <- reactive({
        #colnames
        var_labels = if(input$movepath =="from 1 to 2"){
            c("Entry","Gender","First Term GPA", "APR","Year2:Fall")
        }else if(input$movepath == "from 2 to 3"){
            c("Year2:Fall","Year3:Fall")  
        }else if(input$movepath == "from 3 to 4"){
            c("Year3:Fall","Year4:Fall") 
        }else if(input$movepath == "from 4 to end"){
            c("Year4:Fall","End:Degree","Grad Status")  
        }else{
            c("Entry","Gender","First Term GPA","APR","Year2:Fall","Year3:Fall","Year4:Fall","End:Degree","Grad Status")   
        }
        #js form
        var_labels_js <- paste0('["', paste(var_labels, collapse = '","'), '"]')
        render_js <- paste('
        function(el2) {
            var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i);
            var labels = ', var_labels_js,';
            cols_x.forEach((d, i) => {
                d3.select(el2).select("svg")
                .append("text")
                .attr("x", d)
                .attr("y", 12)
                .text(labels[i]);})}
                           ')
 
            htmlwidgets::onRender(x=sankey_plot(), jsCode = render_js)
 
    })
    
    output$sankey_moving <- renderSankeyNetwork(sankey_plot_cols()) # need add error message
    
    #### migration data table
    
    flowdata <- reactive({
        flowdatatable <-  sankey_1st_data()[(sankey_1st_data()$Cohort == input$movecohort & sankey_1st_data()$VAR1 == input$moveentry),] 
        
    })
    # stop here to update the flow table
    output$flowtable <- renderDT({
        
        FlowTable <- flowdata() %>% 
            select(Cohort,"GPA_HIGHSCHOOL"=HSGPA,GPA1,"Entry"=VAR1,  "Year2"=VAR2,"Year3"=VAR3,
                   "Year4"=VAR4,"FinalDegree"=DEG,"GraduationTime"=GRD ) %>% 
            group_by(Cohort,Entry,Year2,Year3,Year4,FinalDegree,GraduationTime) %>% 
            mutate(GPA_HIGHSCHOOL = ifelse(GPA_HIGHSCHOOL == 0 , NA, GPA_HIGHSCHOOL)) %>% 
            mutate(GPA1 = ifelse(GPA1 == 0, NA, GPA1)) %>% 
            dplyr::summarise(  MinHSGPA=min(GPA_HIGHSCHOOL, na.rm=T),
                               MeanHSGPA= round(mean(GPA_HIGHSCHOOL, na.rm=T),2),
                               MaxHSGPA=max(GPA_HIGHSCHOOL, na.rm=T) , 
                               MeanUWFGPATerm1= round(mean(GPA1, na.rm=T),2),
                               Count = n(), .groups= "drop") %>% ungroup() %>% arrange(-Count) 
        FlowTable$TotalCount <-  rep(sum(FlowTable$Count))
        FlowTable$FinalStatus_Rate <- paste0(round(FlowTable$Count/FlowTable$TotalCount,4)*100, "%", sep="") 
        
        datatable(FlowTable,  extensions = "Buttons",
                  filter = "top",
                  options = list(dom="Blfrtip",
                                 buttons=c("copy","csv","excel","pdf","print"), 
                                 lengthMenu=list(c(10,25,50,-1), c(10,25,50,"All")),pageLength=25)) %>% 
            formatStyle('FinalStatus_Rate',
                        color = styleInterval(50, c('gray', 'white')),
                        backgroundColor = styleInterval(50, c('whtie', 'gray')))
        
    })
    
    
    
    ### APR/Graduation/Summary tab ####
    
   

    
    metric_college <- reactive({
        #req(input$metriccohort)
        enc_data_ds1 %>% filter(stu_college == input$metriccollege)
    })
    
    observeEvent(metric_college(), {
        choice1 <-  unique(metric_college()$stu_department)
        choice2 <-  unique(metric_college()$cohort)
        shiny::updateSelectInput(inputId ="metricdepart", choices = c("(All)", choice1))
    })
    metric_department <- reactive({
        
        req(input$metriccollege)
        filter(metric_college(), stu_department == input$metricdepart)
    })
    
    observeEvent(metric_department(), {
        choice1 <- metric_department()$stu_program_cip_desc
        choice2 <- metric_department()$cohort
        shiny::updateSelectInput(inputId = "metricCIP", choices = c("(All)", choice1))
    })
    
    metric_cip <- reactive({
        req(input$metricdepart)
        filter(metric_department(), stu_program_cip_desc == input$metricCIP)
    })
 
    
    groupdata <- reactive({
        
        enc_data_m1 <- enc_data_ds1 %>%  mutate(Gender= gender, Ethnicity =ethnicity, FirstGen=firstgen, FeeResidency=fee_residency)
        if(input$metriccollege == "(All)"){
            enc_data_m1 %>% filter(cohort == input$metriccohort) %>% 
            #enc_cohort() %>% 
                pivot_longer(c("stu_program_cip_desc"), names_to = "group", values_to = "subgroup") %>% unique()
                #distinct(uwfid, uwf_term_id2, .keep_all = T)  
        }else if(input$metricdepart == "(All)" ){
            enc_data_m1 %>% filter(cohort == input$metriccohort) %>%  
            #enc_cohort() %>% 
                pivot_longer(c("stu_college","stu_department","stu_program_cip_desc"), names_to = "group", values_to = "subgroup") %>% 
                filter( subgroup %in% input$metriccollege) 
        }else if(input$metricCIP == "(All)" ){
            enc_data_m1 %>% filter(cohort == input$metriccohort) %>%     
            #enc_cohort() %>% 
                pivot_longer(c("stu_college","stu_department","stu_program_cip_desc"), names_to = "group", values_to = "subgroup") %>% 
                filter( subgroup %in% input$metricdepart)  
        }else{
            enc_data_m1 %>% filter(cohort == input$metriccohort) %>%   
            #enc_cohort() %>% 
                pivot_longer(c("stu_college","stu_department","stu_program_cip_desc"), names_to = "group", values_to = "subgroup") %>% 
                filter(subgroup %in% input$metricCIP)  
        }
    })
    
    
    
    enc_filtered_data <- reactive({
        reten_terms <- c("Fall_1","Spring_1","Fall_2")
        demodata <- groupdata() %>% 
            filter(uwf_term_id2 %in% reten_terms) %>% mutate(hsgpa = ifelse(is.na(hsgpa), 0, hsgpa)) %>% 
            filter(hsgpa >= input$metricHSGPA[1] & hsgpa <= input$metricHSGPA[2]) %>% 
            mutate(uwf_term_id3 = uwf_term_id2) %>% 
            mutate(uwf_term_id2 = factor(uwf_term_id2, levels = c("Fall_1","Spring_1","Fall_2"), labels = c("Fall 1", "Spring 1","Fall 2")))
        
        if(input$metricdemo == "None"){
            demodata %>% 
                select(uwfid,terms, uwf_term_id2, uwf_term_id3,  uwf_gpa, gpa2, graduation_status,cohort, group,subgroup )
            
        }else{
            
            demodata %>% 
                select(uwfid,uwf_term_id2,uwf_term_id3, "selectedfactor"=input$metricdemo,uwf_gpa, gpa2, graduation_status ,cohort,group,subgroup)
        }
    })
    
    # updated apr summary tab
    
    output$enrollment_bar <- renderPlotly({
        

       reten_cc <-  if(input$metricdemo == "None"){
            
            cc <- count(enc_filtered_data(),  uwf_term_id2, cohort) 
            cc2 <- left_join(cc, count(cc, cohort, by = "cohort", wt = n, name ="nn")) %>% 
                mutate(prop = paste(round( n/n[1] *100,2), "%", sep="")) # based on entry cohort size
            N <- length(unique(cc2$uwf_term_id2))
            my_col <-  RColorBrewer::brewer.pal(3, "Dark2")
            reten_xaxis <- list(title= "Terms")
            plotly::plot_ly(cc2, x = ~ uwf_term_id2  ,   color = ~uwf_term_id2 ,colors= my_col,
                            y= ~n , type="bar", text =~prop, textposition = "outside") %>% 
                config(displaylogo = FALSE)  
            
        }else{
            cc <- count(enc_filtered_data(), uwf_term_id2, selectedfactor) 
            cc2 <- left_join(cc, count(cc, uwf_term_id2, wt = n, name ="nn")) %>% 
                mutate(prop = paste(round( n/nn *100,2), "%", sep="")) #based on total gender
            N <- length(unique(cc2$uwf_term_id2))
            my_col <-  RColorBrewer::brewer.pal(3, "Dark2")
            reten_xaxis <- list(title=input$metricdemo)
            plotly::plot_ly(cc2, x = ~selectedfactor  ,   color = ~uwf_term_id2 ,colors= my_col,
                            y= ~n , type="bar", text =~prop, textposition = "outside")  
        }
       
       reten_cc %>% 
           config(displaylogo = FALSE) %>% 
           layout(title = "Enrollment From First Fall to Second Fall",
                  yaxis = list(title ="Count of uwfid (entered and returned)"), 
                  xaxis=  reten_xaxis,  
                  barmode = "group", showlegend = TRUE) 

    })
    
    
    output$firstfallgpa_plot <- renderPlotly({
        
        first_fall_factor <- enc_filtered_data() %>% 
            mutate(FirstFallGPA = ifelse(is.na(uwf_gpa), "Missing",
                                         ifelse(uwf_gpa >= 2.77, ">=2.77",
                                                ifelse(uwf_gpa>= 2.00, "[2.00,2.77)", "<2.00")))) %>% #term gpa
            mutate(FirstFallGPA = factor(FirstFallGPA, levels = c("Missing","<2.00","[2.00,2.77)",">=2.77")))
        
      first_fall_p <-   if(input$metricdemo == "None"){
            
            cc <- count(first_fall_factor, uwf_term_id2, FirstFallGPA, cohort) 
            cc2 <- left_join(cc, count(cc,uwf_term_id2, cohort,  wt = n, name ="nn")) %>%  filter(uwf_term_id2 =="Spring 1") %>%  #1st fall gpa
                mutate(prop = paste(round( n/nn *100,2), "%", sep="")) 
            N <- length(unique(cc2$FirstFallGPA)) #number of col
            mycolor <- RColorBrewer::brewer.pal(4, "Set1")
            firstfallgpa_xaxis <- list(title= "First Fall GPA")
            plotly::plot_ly(cc2, x = ~ FirstFallGPA  ,   color = ~FirstFallGPA ,colors= mycolor,
                            y= ~n , type="bar", text =~prop, textposition = "outside")  
            
        }else{
            
            cc <- count(first_fall_factor, uwf_term_id2, FirstFallGPA, selectedfactor)  #choose vari
            cc3 <- left_join(cc, count(cc, uwf_term_id2,  FirstFallGPA, wt = n, name ="nn")) %>%  filter(uwf_term_id2 =="Spring 1") %>%  #1st fall gpa
                mutate(prop = paste(round( n/nn*100,2), "%", sep=""))
            N <- length(unique(cc3$FirstFallGPA)) #number of col
            mycolor <- RColorBrewer::brewer.pal(4, "Set1")
            firstfallgpa_xaxis <- list(title=input$metricdemo)
            plotly::plot_ly(cc3, x = ~ selectedfactor  ,   color = ~FirstFallGPA ,colors= mycolor,
                            y= ~n , type="bar", text =~prop, textposition ="outside")  
               
        }
      first_fall_p %>% 
          config(displaylogo = FALSE) %>% 
          layout( title="Above a 2.77 GPA or Below",
                  yaxis = list(title ="Count of uwfid (returned 1st spring term)"), 
                  xaxis= firstfallgpa_xaxis, barmode = "group", showlegend = TRUE)
        
    })
    
    output$metricAPR_plot <- renderPlotly({
        
        
        Fall2_apr <- enc_data_ds1 %>% 
            filter(uwf_term_id2 == "Fall_2") %>% 
            pivot_longer(c("stu_college","stu_department","stu_program_cip_desc"), names_to = "group", values_to = "subgroup") %>% 
            unique() %>% 
            group_by(uwfid) %>% select(uwfid, uwf_term_id2,  "group"=group,"fall2_subgroup"=subgroup, uwf_gpa)
        
        apr_p <-  if(input$metricdemo == "None" ){

             mycolor <- c("#B6ADA5","#009CDE","#40B5E6")  
            # based on 2nd fall location
            firstterm <-  enc_filtered_data() %>% filter(  uwf_term_id2 =="Fall 1") %>% 
                select(uwfid, "term1"=uwf_term_id2, cohort,  "group"=group,"entry_subgroup"=subgroup)
            
            secondFall <- merge(firstterm, Fall2_apr, by= c("uwfid","group"), all.x = TRUE) %>%
                mutate(migrate = ifelse(entry_subgroup == fall2_subgroup, "Stay","Out")) %>% 
                mutate(migrate = ifelse(is.na(migrate),"dropout", migrate)) %>% 
                mutate(fall2gpa = ifelse(uwf_gpa >= 2.00, "2.00","not2.00")) %>%
                mutate(APR = ifelse((fall2gpa == "2.00" & migrate == "Stay"), "Stayed APR",
                                    ifelse(fall2gpa == "2.00" & migrate == "Out", "Migrated APR", "Non-APR"))) %>% 
                mutate(APR2 = ifelse(APR == "Non-APR", "Non-APR","APR"))
            secondFall$APR <- factor(secondFall$APR, levels = c("Non-APR","Migrated APR", "Stayed APR"))
           # secondFall$APR2 <- factor(secondFall$APR2, levels = c("Non-APR",  "APR"))
            # calculate % by filter
            cc <- count(secondFall, APR2, APR,  term1, cohort)  #choose vari
            cc4 <- left_join(cc, count(cc,  term1, cohort, wt = n, name ="nn"))  %>% 
                mutate(prop = paste(round( n/nn*100,2), "%", sep=""))
            apr_xaxis <- list(title="Remaining Students APR")
            plotly::plot_ly(cc4, x = ~APR2  ,   color = ~APR , colors = mycolor,  
                            y= ~n , type="bar", text =~prop, textposition ="outside")   
        }else{
            mycolor <- c("#B6ADA5","#009CDE","#40B5E6")
            # based on 2nd fall location
            firstterm <-  enc_filtered_data() %>% filter(  uwf_term_id2 =="Fall 1") %>% 
                select(uwfid, "term1"=uwf_term_id2, cohort, selectedfactor,"group"=group,"entry_subgroup"=subgroup)
             
            Fall2_GPA <- merge(firstterm, Fall2_apr, by= c("uwfid","group"), all.x = TRUE) %>%  
                mutate(migrate = ifelse(entry_subgroup == fall2_subgroup, "Stay","Out")) %>% 
                mutate(migrate = ifelse(is.na(migrate),"dropout", migrate)) %>%
                mutate(fall2gpa = ifelse(uwf_gpa >= 2.00, "2.00","not2.00")) %>% 
                mutate(APR = ifelse((fall2gpa == "2.00" & migrate == "Stay"), "Stayed APR",
                                    ifelse(fall2gpa == "2.00" & migrate == "Out", "Migrated APR", "Non-APR")))
             Fall2_GPA$APR <- factor(Fall2_GPA$APR, levels = c("Non-APR","Migrated APR", "Stayed APR"))
             cc <- count(Fall2_GPA,  APR, selectedfactor)  #choose vari
             cc4 <- left_join(cc, count(cc,  APR,  wt = n, name ="nn"))  %>% 
                mutate(prop = paste(round( n/nn*100,2), "%", sep=""))
             apr_xaxis <- list(title=input$metricdemo)
             plotly::plot_ly(cc4, x = ~ selectedfactor  ,   color = ~APR ,colors= mycolor,  
                            y= ~n , type="bar", text =~prop, textposition ="outside") 
            
        }
        
        apr_p %>%  
            config(displaylogo = FALSE) %>% 
            layout(  title="Remained Second Fall with at least a 2.00 GPA",
                     yaxis = list(title ="Count of uwfid (returned 2nd fall term)"), 
                     xaxis= apr_xaxis,barmode = "group", showlegend = TRUE)
            
    })
    # metric graduation plot
    output$metricontime_plot <- renderPlotly({
        
     ontime_p <-    if(input$metricdemo =="None"){
            
            cc <- count( enc_filtered_data(), uwf_term_id2, graduation_status)
            cc5 <- left_join(cc, count(cc, uwf_term_id2, wt = n, name ="nn")) %>% filter(uwf_term_id2 =="Fall 1") %>%    #1st fall gpa
                mutate(prop = paste(round( n/nn*100,2),"%",spe=""))  # cohort size
             N <- length(unique(cc5$graduation_status)) #number of col
            mycolor <- RColorBrewer::brewer.pal(3, "Set2") 
            ontime_xaxis <- list(title= "Graduation Time")
            plotly::plot_ly(cc5, x = ~ graduation_status  ,   color = ~graduation_status ,colors= mycolor,
                            y= ~n , type="bar", text =~prop, textposition="outside")  
            
        }else{
            
            cc <- count( enc_filtered_data(), uwf_term_id2, graduation_status, selectedfactor)  #choose vari
            cc5 <- left_join(cc, count(cc, graduation_status,uwf_term_id2, wt = n, name ="nn")) %>%  filter(uwf_term_id2 =="Fall 1") %>%   #1st fall gpa
                mutate(prop = paste(round( n/nn*100,2),"%",spe=""))  # cohort size
             N <- length(unique(cc5$selectedfactor)) #number of col
            mycolor <- RColorBrewer::brewer.pal(3, "Set2") 
            ontime_xaxis <- list(title= input$metricdemo)
            plotly::plot_ly(cc5, x = ~ selectedfactor  ,   color = ~graduation_status ,colors= mycolor,
                            y= ~n , type="bar", text =~prop, textposition="outside")  
        } 
     
     ontime_p %>% 
         config(displaylogo = FALSE) %>% 
         layout(title="Four-Year Graduation Rate", yaxis = list(title ="Count of uwfid"), 
                xaxis= ontime_xaxis, barmode = "group", showlegend = TRUE)
        
    })
    
    #pivot table data
    metric_pivot_data <- reactive({
        enc_data_ds_filterhsgpa <- enc_data_ds1 %>% filter(hsgpa >= input$metricHSGPA[1] & hsgpa <= input$metricHSGPA[2])  
        #demo and gradu
        metric_demo <- enc_data_ds1 %>% filter(uwf_term_id2 =="Fall_1") %>% # Fall 1 did not identify APR
            select(uwfid,  terms, stu_college,stu_department,stu_program_cip_desc,
                    gender,  firstgen,  fee_residency,ethnicity,
                   tier,graduation_status, hsgpa) %>% group_by(uwfid) #%>% filter(!duplicated(uwfid))
        #retention
        reten_terms <- c("Fall_1","Spring_1","Fall_2")
        metric_reten <- enc_data_ds1 %>% 
            filter(uwf_term_id2 %in% reten_terms) %>%
            select(uwfid,cohort,terms, uwf_term_id2) %>% arrange(uwfid,terms) %>% 
            group_by(uwfid,uwf_term_id2) %>% 
            mutate(row = row_number()) %>% 
            pivot_wider(names_from = uwf_term_id2, values_from = terms ) %>%
            select(-row) %>%  #filter(!is.na(Fall_1)) %>% # create a unique identifier row amd remove NA at Fall1
            mutate_at(c(3,4,5), replace_na, 0) %>% 
            mutate_at(c(3,4,5), ~ifelse(. != 0, "Returned", "Stopped"))  
        colnames(metric_reten) <- c("uwfid","cohort", "Cohort_Fall1","Returning_Spring1","Returning_Fall2")
        demo_reten <- merge(metric_demo, metric_reten, by="uwfid",all.x = TRUE)
       # addmargins(table(metric_reten$cohort, metric_reten$Returning_Fall2))
       #APR
       metric_apr_wide <- enc_data_ds1 %>% filter(uwf_term_id2 == "Fall_2" ) %>%
           select(uwfid,uwf_gpa, uwf_term_id2,apr_y1) %>% mutate(APR= apr_y1)  
       
       demo_reten_apr <- merge(demo_reten, metric_apr_wide, by="uwfid", all.x=TRUE) %>%
       mutate(APR= ifelse(is.na(APR), "Non-APR",APR)) %>%
       mutate(APR = ifelse(cohort == 2021, "NA", APR)) %>%
       mutate(Returning_Fall2 = ifelse(cohort ==2021, "NA", Returning_Fall2))
        #spring gpa
       Fall1_gpa <- enc_data_ds1 %>% filter(uwf_term_id2 == "Fall_1" ) %>% 
            mutate(fall1gpa= round(stu_gpa_grade_points/stu_gpa_term_hours, 2)) %>% 
            mutate(Fall_1_GPA = ifelse(fall1gpa >= 2.77, ">=2.77", ifelse(fall1gpa>= 2.00, "[2.00,2.77)", "<2.00" ))) %>% 
            select(uwfid, Fall_1_GPA)
        
        # final data
        demo_reten_apr_1stgpa <- merge(demo_reten_apr,Fall1_gpa, by = "uwfid", all.x=TRUE ) %>% 
            mutate(Fall_1_GPA = ifelse(is.na(Fall_1_GPA), "Missing",Fall_1_GPA )) %>% filter(!duplicated(uwfid)) 
        #check
        # ftic_id <- demo_reten_apr_1stgpa %>% select(uwfid, cohort,APR,Returning_Fall2) %>% unique()
        # addmargins(table(ftic_id$cohort, ftic_id$Returning_Fall2))
        
    })
    
    #pivot output
    output$metric_pivot <-  renderRpivotTable({
        
        mertic_pivot_data <-  metric_pivot_data() %>%  
            mutate(Fall_1_GPA = factor(Fall_1_GPA, levels = c("<2.00" ,"[2.00,2.77)", ">=2.77", "Missing"))) %>% 
            select("Cohort"=cohort, 
                   "Entry_College"=stu_college, "Entry_Department"=stu_department,"Entry_Program"=stu_program_cip_desc,
                   7:21, -apr_y1) 
        
        rpivotTable(
            data = mertic_pivot_data,
            rows = c( "Cohort" ),
            cols = c("APR"),
            aggregatorName = "Count as Fraction of Rows",
            #vals = "",
            rendererName = "Table"
        )
        
    })
    
    ## metric summary table
    # ### Table for colleges or uwf
    groupdata_sum_tab <- reactive({
        if(input$metriccollege == "(All)"){
            enc_data_ds1 %>% pivot_longer(c("stu_program_cip_desc"), names_to = "group", values_to = "subgroup") %>% 
                distinct(uwfid, cohort,terms,  .keep_all = T) #Stu_Term
        }else if(input$metricdepart == "(All)"){
            enc_data_ds1 %>% 
                pivot_longer(c("stu_college","stu_department","stu_program_cip_desc"), names_to = "group", values_to = "subgroup") %>% 
                filter( subgroup %in% input$metriccollege)
        }else if(input$metricCIP == "(All)"){
            enc_data_ds1 %>% 
                pivot_longer(c("stu_college","stu_department","stu_program_cip_desc"), names_to = "group", values_to = "subgroup") %>% 
                filter( subgroup %in% input$metricdepart)
        }else{
            enc_data_ds1 %>% 
                pivot_longer(c("stu_college","stu_department","stu_program_cip_desc"), names_to = "group", values_to = "subgroup") %>% 
                filter(subgroup %in% input$metricCIP) 
        }
    })
    
    metric_summary_filtered_data <- reactive({
        
        data1 <- groupdata_sum_tab() %>% 
            filter(uwf_term_id2 == "Fall_1") %>% 
            select(uwfid,"term1"=uwf_term_id2,  cohort, "group"=group,"entry_subgroup"=subgroup)
        data_middle <- merge(data1[,1], enc_data_ds1, by="uwfid", all.x = TRUE) %>% filter(uwf_term_id2 == "Fall_2") %>% 
            pivot_longer(c("stu_college","stu_department","stu_program_cip_desc"), names_to = "group", values_to = "subgroup") %>% 
            unique() %>% 
            select(uwfid,  uwf_term_id2, uwf_gpa, graduation_status,  "group"=group, "fall2_subgroup"=subgroup)
        
        
        sum_data <- merge(data1, data_middle, by= c( "uwfid","group"),all.x=TRUE) %>% 
            unique() %>% # give location with name (3 levels)
            mutate(Retention = ifelse(!is.na(uwf_term_id2),"Yes","No"),
                   APR1=ifelse(uwf_gpa >= 2.00,"APR","Non-APR"), 
                   APR_All=ifelse(is.na(APR1), "Non-APR",APR1)) %>% 
            mutate(Out_Migration = ifelse(entry_subgroup==fall2_subgroup, "Stayed","Out"), # matching subgroup to find migrate
                   APR_Out = ifelse(Out_Migration =="Out" & APR_All == "APR", "APR","Others"),
                   APR_Stay = ifelse(Out_Migration=="Stayed" & APR_All == "APR", "APR","Others")) %>%  # out-migration count
            mutate()
        # retention
        cc <- count( sum_data, Retention, term1, cohort)  #choose vari
        #apr
        cc_apr <- count( sum_data, APR_All,term1, cohort)  #choose vari
        # migration
        cc_Stay  <- count( sum_data, APR_Stay, term1, cohort)
        cc_Out  <- count( sum_data, APR_Out, term1, cohort)
   
        #graduation
        cc_grad <- count( sum_data, graduation_status, term1, cohort) 
        #perdentage
        cc_ren <- left_join(cc, count(cc, term1, cohort, wt = n, name ="nn")) %>%  
            mutate(Retention_Pct = paste(round( n/nn*100,2),"%",spe=""))  %>% filter(Retention =="Yes") %>% 
            select("Cohort"=cohort,"CohortSize"=nn,"Retention"=n, Retention_Pct)
        cc_apr <- left_join(cc_apr, count(cc, term1, cohort, wt = n, name ="nn")) %>%  filter(APR_All =="APR") %>% 
            mutate(APR_Total_Pct = paste(round( n/nn*100,2),"%",spe="")) %>% 
            select("Cohort"=cohort, "APR_Total"=n,APR_Total_Pct)
        
        cc_mig_stay <- left_join(cc_Stay, count(cc, term1, cohort, wt = n, name ="nn")) %>%  filter(APR_Stay =="APR") %>% 
            mutate(APR_Stay_Pct = paste(round( n/nn*100,2),"%",spe="")) %>% 
            select("Cohort"=cohort, "APR_Stay"=n, APR_Stay_Pct)
        cc_mig_out <- left_join(cc_Out, count(cc, term1, cohort, wt = n, name ="nn")) %>%  filter(APR_Out =="APR") %>% 
            mutate(APR_Migrate_Pct = paste(round( n/nn*100,2),"%",spe="")) %>% 
            select("Cohort"=cohort, "APR_Migrate"=n,APR_Migrate_Pct)
        
        
        cc_grad <- left_join(cc_grad, count(cc, term1, cohort, wt = n, name ="nn")) %>%  
            mutate(gradu_Pct = paste(round( n/nn*100,2),"%",spe="")) # OnTime, OverTime, NoDegree
        cc_OnTime <- cc_grad %>% filter(graduation_status=="OnTime") %>%  
            select("Cohort"=cohort, "OnTime"=n,"OnTime_Pct"=gradu_Pct)  
        cc_OverTime <- cc_grad %>% filter(graduation_status=="OverTime") %>%  
            select("Cohort"=cohort, "OverTime"=n,"OverTime_Pct"=gradu_Pct)  
        #merge
        cc_onover <- merge(cc_OnTime, cc_OverTime, by = "Cohort", all.x = TRUE) #on and over time data
        cc_total  <- merge(cc_ren, cc_apr, by= "Cohort", all.x = TRUE)
     
        cc_stayout <- merge(cc_mig_stay, cc_mig_out,  by = "Cohort", all.x = TRUE)
        total <- merge(cc_total, cc_stayout, by ="Cohort", all.x = TRUE)
        total2 <- merge(total, cc_onover, by ="Cohort", all.x = TRUE)
        
    })
    output$uwf_table_reactive <- renderDT({
        #summary table
        datatable(metric_summary_filtered_data(), 
                  caption = tags$caption( "Total APR includes changed and unchanged students", style ="color:blue")
        )
        
    })
    
    
    
    overview_summary_data <- reactive({
        if(input$metriccollege == "(All)"){
          enc_data_ds1 %>% pivot_longer(c("stu_program_cip_desc"), names_to = "group", values_to = "subgroup") %>% 
                distinct(uwfid, cohort, terms,  .keep_all = T)
         # addmargins(table(test$cohort, test$uwf_term_id2))
        }else if(input$metricdepart == "(All)"){
            enc_data_ds1 %>% 
                pivot_longer(c("stu_college","stu_department","stu_program_cip_desc"), names_to = "group", values_to = "subgroup") %>% 
                filter( subgroup %in% input$metriccollege)
        }else if(input$metricCIP == "(All)"){
            enc_data_ds1 %>% 
                pivot_longer(c("stu_college","stu_department","stu_program_cip_desc"), names_to = "group", values_to = "subgroup") %>% 
                filter( subgroup %in% input$metricdepart)
        }else{
            enc_data_ds1 %>% 
                pivot_longer(c("stu_college","stu_department","stu_program_cip_desc"), names_to = "group", values_to = "subgroup") %>% 
                filter(subgroup %in% input$metricCIP) 
        }
    })
    
    UWFtable_all <-  reactive({
        
        # from filter group and subgroup
        # filter fall 1 and 2 separate
        # merge two data set by uwfid
        # count by two factors
        # first fall group merge with second fall group
        
       entry_location <-  overview_summary_data() %>% 
           filter(uwf_term_id2 == "Fall_1") %>%  
           filter((hsgpa >= input$metricHSGPA[1]) & (hsgpa <= input$metricHSGPA[2])) %>% 
           select(uwfid,cohort, "group"=group,"entry_subgroup"=subgroup) 
       #addmargins(table(entry_location$cohort, entry_location$term1))
       fall2_location <- merge(entry_location[,1],  enc_data_ds1, by ="uwfid", all.x=TRUE) %>%  
           filter(uwf_term_id2 == "Fall_2") %>%
           pivot_longer(c("stu_college","stu_department","stu_program_cip_desc"), names_to = "group", values_to = "subgroup") %>% 
           unique() %>% 
           select(uwfid, "term2"=uwf_term_id2,  apr_y1, returned,gradu_time, "group"=group,"fall2_subgroup"=subgroup)
       #check
       #addmargins(table(fall2_location$cohort, fall2_location$term2))
       summary_table_data1 <- merge(entry_location, fall2_location, by =c("uwfid", "group"), all.x =TRUE) %>% 
           unique() %>%  mutate(Retention = ifelse(is.na(returned), "No","Yes"),
                                       APR = ifelse(is.na(apr_y1) | apr_y1 != "APR", "No", "Yes" ),
                                       Graduation = ifelse(is.na(gradu_time),"NoDegree", gradu_time))
       #check
       #addmargins(table(summary_table_data1$cohort, summary_table_data1$APR))
    })
    
    UWF_Table <- reactive({
        
        cohort_size <- UWFtable_all() %>% group_by(cohort) %>%  count()
        names(cohort_size) <- c("cohort","Adj_CohortSize")
        rent_college <- UWFtable_all() %>% 
             group_by( cohort, Retention) %>% count() %>%
            tidyr::pivot_wider( names_from = Retention, values_from = n ) %>%
            replace(is.na(.), 0) %>% mutate(Retention=Yes)  %>%
            select( -Yes,-No)
        
        rapr_college <-   UWFtable_all() %>%
            group_by( cohort, APR) %>% count() %>%
            tidyr::pivot_wider( names_from = APR, values_from = n ) %>%
            replace(is.na(.), 0) %>% mutate(APR = Yes) %>%
            select( -Yes,-No)
        
        apr_rent <- merge(rent_college,rapr_college, by=c("cohort"), all.x = T)
        
        gradu_college <-   UWFtable_all() %>%
            group_by( cohort, Graduation) %>% count() %>%
            tidyr::pivot_wider( names_from = Graduation, values_from = n ) %>%
            replace(is.na(.), 0)  
        gradu_cohort <- merge(gradu_college,cohort_size , by="cohort", all.x=TRUE )
             
        all_df <- merge(apr_rent, gradu_cohort,  by=c("cohort" ), all.x = T)
        
        gradu <-  all_df %>%  mutate(Grad5yrs = (Grad4yrs + Grad5yrs) , Grad6yrs  = (Grad5yrs + Grad6yrs))
        
        #final table
        final_metric_df <-   gradu %>%  rowwise() %>%
            mutate(across(c(2:7), ~round(.x/Adj_CohortSize*100,2), .names = "{col}%")) %>%
            select("Cohort"=cohort,8,2:7,9:14)
        
    })
    
    output$cohortaprtable <- reactive({
        
        datatable(UWF_Table(),
                  caption = tags$caption( "Total APR includes changed and unchanged students", style ="color:blue"))
    })
    

    
    ### Trends in APR & Retention e
    
    output$metric_trend <- renderEcharts4r({
        
        metric_summary_filtered_data() %>% mutate(APR_pct = gsub( " %","",APR_Total_Pct)) %>% 
            mutate(APR_pct = as.numeric(APR_pct)) %>% 
            e_charts(x=Cohort) %>% 
            e_line(CohortSize) %>%
            e_line(Retention) %>% 
            e_line(APR_Total, name = "APR") %>% 
            e_bar(APR_pct,name = "APR%", stack ="grp1", y_index = 1, color="#80CEEE") %>% 
            e_line(OnTime, name = "On-time Grad") %>% 
            #e_legend(show = TRUE,right='5',top='15%', orient = 'vertical') %>% 
            e_labels(position = "insideRight", fontSize= 10 , #color="#674230",fontWeight = "bold",
                     formatter = htmlwidgets::JS("
                                                 function(params){
                                                 return(params.value[1] + '')}") ) %>%
            e_title(paste0("APR & Retention by Filtered Selection (2015-2020)"),
                           subtext=paste0("Selected:",input$metriccollege,"-", input$metricdepart, "-",input$metricCIP),
                    left="center",top=30,  textStyle = list(fontSize = 20, fontFamily='Helvetica')) %>% 
            e_grid(top=100, bottom = "16%") %>% 
            e_tooltip(trigger = c("item","axis"))
        
        
    })
    
    ### Racing bar charts
    
    racing_apr_data <- reactive({
        
       college_data <-  if(input$metriccollege == "(All)"){
            
            apr_data <- enc_data_ds1 %>% 
                filter(uwf_term_id2 == "Fall_1") %>% 
                mutate(apr_id = ifelse(is.na(apr_id),"Non_APR", apr_id)) %>% 
                select(uwfid,cohort, contains("college"), contains("department"),contains("program"),apr_id) %>%
                mutate(cohort =as.numeric(cohort)) 
            
        }else{
            
            apr_data <- enc_data_ds1 %>% filter(stu_college == input$metriccollege) %>%  
                filter(uwf_term_id2 == "Fall_1") %>% 
                mutate(apr_id = ifelse(is.na(apr_id),"Non_APR", apr_id)) %>% 
                select(uwfid,cohort, contains("college"), contains("department"),contains("program"),apr_id) %>%
                mutate(cohort =as.numeric(cohort)) 
            
        }
        
        apr_table <- college_data %>%  
            count(cohort,program_desc_abbs2, apr_id) %>% filter(cohort <= 2020) %>% 
            pivot_wider(names_from = apr_id, values_from = n) %>% replace(is.na(.), 0) %>% 
             mutate(APR_pct= round(APR/(APR+Non_APR)*100,2))  %>% arrange(-cohort, -APR)
        
        
    })
    output$racing_apr_plot <- renderEcharts4r({
        
   
    # racing_bar_data <-  test_racing  %>% 
    #     mutate(depart = str_replace_all(stu_department, 
    #                                     c('Department'='','of'='', ' '='','&'='',','='','and'='','Engineering'='Engr.'))) %>% 
    #     mutate(Used = round( Fall_2/Fall_1*100, 0)) %>% 
    #    # filter(Used <= 1.0) %>% 
    #     mutate(onset_date = make_date(year= cohort, month = term)) %>% 
    #     mutate(Date1 = as.Date(onset_date, format ="%Y/%m")) %>% 
    #     mutate(Date2 = strptime(as.character(Date1), "%Y")) %>% 
    #     mutate(Date_year =  format(as.Date(Date2), "%Y/%m")) %>%  arrange(cohort, Used)
    
    racing_apr_data() %>%  
        group_by( cohort ) %>% 
        e_charts(program_desc_abbs2, timeline = TRUE, reorder = TRUE) %>% 
        e_timeline_opts(autoPlay = TRUE, top = 60) %>% 
        e_bar(APR, name="APR", stack = "grp", 
              itemStyle = list(color ="#009CDE", borderColor = "#3091BB", borderWidth = "1")) %>% 
        e_bar(Non_APR,name="Non-APR", stack = "grp", 
              itemStyle = list(color ="#B6ADA5", borderColor = "#A5AEB6", borderWidth = "1")) %>% 
        e_bar(APR_pct, name="APR %", stack = "grp1", y_index = 1,
              itemStyle = list(color ="#97C800", borderColor = "#40A829", borderWidth = "1")) %>% 
        # e_legend(show = TRUE,right='5',top='15%', orient = 'vertical',
        #          ) %>% 
        e_labels(position = "insideTop", fontSize= 10 # color="#674230",fontWeight = "bold",
        # formatter = htmlwidgets::JS("
        #                             function(params){
        #                             return(params.value[0] + '%')}")
                ) %>%
        e_title("Percentage of FTIC students enrolled in their second fall with at least 2.0 GPA (2015-2020)",
                left="center",top=30,  textStyle = list(fontSize = 20, fontFamily='Helvetica')) %>% 
        e_grid(top=120, bottom = "16%") %>% 
        e_x_axis(axisLabel=list(interval = 0, rotate =45)) %>% 
        e_axis_stagger() %>% 
        e_y_axis(formatter = e_axis_formatter("decimal", digits = 0)) %>% 
        #e_flip_coords() %>% 
        #e_y_axis(inverse = TRUE) %>% #stop here
        e_axis_labels( y= "count" ) %>% 
            e_tooltip(trigger = c("item","axis"))
    
    })
    
    
    ### COURSES TAB  
    
    course_apr <- reactive({
        
        crsapr_df <- course_data_v1[,-1] %>%
            filter(ENTRY_PROGRAM == input$courseprogram) %>%
            filter(Cohort == input$couresecohort)
    })
    
    observeEvent(course_apr(), {
        choice_crsname <-  sort(unique(course_apr()$CRSE_NAME))
        shiny::updateSelectInput(inputId = "coursename", choices = choice_crsname)
        choice_crsterm <-  sort(unique(course_apr()$crs_DEMO_TIME))
        shiny::updateSelectInput(inputId = "courseterms", choices = c("(All)",choice_crsterm))
        choice_topcrsterm <-  sort(unique(course_apr()$crs_DEMO_TIME))
        shiny::updateSelectInput(inputId = "topcrsselectterms", choices = c("(All)",choice_topcrsterm))
    })
    
    course_pass <- reactive({
        
        topcrs_df <- course_data_v1[,-1] %>%
            mutate(topcrsall = (input$topcrsselectterms == "(All)" |  course_data_v1$crs_DEMO_TIME == input$topcrsselectterms))
        
        topcrs_t <-  topcrs_df[topcrs_df$topcrsall,,drop=FALSE] %>%
            filter(Cohort == input$couresecohort)  %>%
            filter(ENTRY_PROGRAM == input$courseprogram) %>%
            filter((GPA_HIGHSCHOOL >= input$crsgparange1) & (GPA_HIGHSCHOOL <= input$crsgparange2))  
        
    })
    
    # output course pass plot
    bar_course_pass <- reactive({# eventReactive(input$frerqCRS,{
        freqcrs_df <-  course_pass()  %>%  
            group_by(CRSE_NAME,CRS_PASS ) %>%  
            count() %>% 
            tidyr::pivot_wider(names_from =  CRS_PASS, values_from =  n) %>% 
            replace(is.na(.), 0) 
        
        freqcrs_df$CRSCount <-  rowSums(freqcrs_df[,2:3])
        
        freqcrs_df1 <-  freqcrs_df  %>% data.frame() %>%  
            arrange(-CRSCount) %>% slice(1:25) %>% 
            mutate_at(vars(2:3), list(Prop =  ~ ./CRSCount*100 )) %>% 
            mutate(across(where(is.numeric), round, 2)) 
        
        crs_y1 <- paste(freqcrs_df1$Passed_Prop, "%", sep= " ")
        crs_y2 <- paste(freqcrs_df1$Failed_Prop, "%", sep= " ")
        
        plot_ly(freqcrs_df1, x = ~reorder(CRSE_NAME,-CRSCount), y = ~Passed , name="Passed"  , 
                text = crs_y1,
                type="bar", marker = list(color="#669900"))  %>%   
            add_trace( y = ~Failed  ,  marker = list(color="#FF3399"), name ="Failed",
                       text = crs_y2 , 
                       textposition ="auto") %>% 
            layout( xaxis = list(title =paste(input$topcrsselectterm,"Courses", sep =""), titlefont = list(size = 12, color="blue")), # add input xaxis
                    yaxis = list(title ="Enrolled (Count)",titlefont = list(size = 12, color="green")), barmode ="group")
        
    })
    
    output$bar_course_pass_plot <- renderPlotly(bar_course_pass())
    
    ### APR by course grades
    course_gradeapr <- reactive({
        crs_df <- course_data_v1 %>% 
            mutate(crsall = (input$courseterms == "(All)" |  course_data_v1$crs_DEMO_TIME == input$courseterms))
        crs_t <-  crs_df[crs_df$crsall,,drop= FALSE] %>%  
            filter(ENTRY_PROGRAM == input$courseprogram) %>% 
            filter(Cohort == input$couresecohort)  %>% 
            filter((GPA_HIGHSCHOOL >= input$crsgparange1) & (GPA_HIGHSCHOOL <= input$crsgparange2)) %>% 
            filter(CRSE_NAME == input$coursename)   
        
    }) 
    
    
    bar_course_apr <- reactive({# eventReactive(input$aprgradeCRS,{
        
        aprcrs_DF <-  course_gradeapr()  %>% mutate(APR = factor(APR)) %>%  
            group_by( GRADE_AWARDED, APR) %>% count() %>% 
            plot_ly(x = ~GRADE_AWARDED, y =~n, color = ~APR, type="bar",  colors = c("#DE3163","#004C97","#004C97") ) %>%  
            layout( xaxis = list(title ="Course Grades", titlefont = list(size = 12, color="blue")),
                    yaxis = list(title ="Enrolled (Count)",titlefont = list(size = 12, color="green")),
                    barmode ="group",
                    legend=(list(title=list(text='<b> APR </b>'))))  
        
        
    })
    
    output$bar_course_apr_plot <- renderPlotly(bar_course_apr())   
    
    
    
    
} #end-sever

shinyApp(ui = ui, server = server)
