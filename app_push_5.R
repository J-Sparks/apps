
###### 06/10/2022 ######

#load libraries 
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

 
# selected fin codes
fin_ftic_all_data_selected_codes <- readRDS("fin_ftic_selected_codes.rds")  

# updated Spring 2022
enc_data_ds <- readRDS("ftic_dashboard_data.rds") 

# sankey data
sankey_data <- readRDS("final_destination_longV0.rds") # no zero hs gpa
new_sankeydata_v1 <- readRDS("sankey_new_data_2015to2021.rds")

# course data
course_data_v1 <- readRDS("DB_crs_grade_FTIC1521V1.rds") 

# tier character data
student_character_1stFall <- readRDS("student_character_1stFall_df.rds") # by term (file apriori by Tier.rdm)

# help for filter
CIP_college <- unique(sankey_data$ENTRY_COLLEGE)   %>% sort()
CIP_depart <- unique(sankey_data$ENTRY_DEPARTMENT) %>% sort()
CIP_choice <- unique(sankey_data$UWFFall1CIPTitle) %>% sort()
cohort_choice <- sort(unique(sankey_data$Cohort), decreasing = F)  
maxhsgpa <- max(new_sankeydata_v1$GPA_HIGHSCHOOL)
minhsgpa <- min(new_sankeydata_v1$GPA_HIGHSCHOOL)
metriccolchoice <- CIP_college 
barchartmetric <- c("APR","RETENTION","GRADUATIONin4YRS")
 

###start-ui
ui <- dashboardPage( skin = "green",
                     dashboardHeader(title="From Entry To End Program", titleWidth = 300),
                     dashboardSidebar( width = 300,
                     sidebarMenu(id = "Menu...",                  
                        menuItem( text= "Overview FTIC", startExpanded = T, icon = icon("fas fa-chart-line"),
                            menuSubItem(text = "APR/Graduation/Summary", tabName = "metricapr"),
                            menuSubItem(text = "Course Grades Data",     tabName = "coursegrades")
                             
                            ),      
                        menuItem( text= "Migration FTIC", startExpanded = T,icon = icon("bar-chart-o"),
                            menuSubItem(text = "Entry or Exit Flow Graph",  tabName = "flowchart")

                        ),# menuitem
                        menuItem( text = "Characteristics FTIC", startExpanded = T, icon = icon("list-alt"),
                            menuSubItem(text = "Tier Characteristics", tabName = "stu_characteristic")
                            
                            
                        ),
                        menuItem( text = "Financial Aid Data", startExpanded = T, icon = icon("fas fa-hand-holding-usd"), 
                                 # menuSubItem(text = "Linking Award Types", tabName = "fin_aid_type"),
                                  menuSubItem(text = "Pivot Table",               tabName = "fin_aid_pivot")
                            )
                     )
                     ), # sidebar menu
                     
        dashboardBody(
                         tags$head( 
                             tags$style(HTML(".main-sidebar { font-size: 18px; }")) #change the font size to 15
                             
                         ),
                         tabItems(
 
                             tabItem(tabName = "fin_aid_pivot",
                                     
                                column(12, 
                                               div(style ='overflow-x: scroll',
                                               rpivotTableOutput("fin_overallpivot", width = "100%", height = "1000px") ) )
                                 
                                 
                             ), #fin pivot
                             #student characteristics tab
                             tabItem(tabName = "stu_characteristic",
                                     fluidRow(
                                         box(width = 2, title = "Filter...", solidHeader = T, status = "info", background = "black",
                                             br("FTIC who were FL residents and graduated tri-county high schools"),
                                             br("Cohort: from 2017 to 2021"),
                                             br("Included application month, entry program, demographic, and familial factors"),
                                             br(),

                                             selectInput(inputId = "selecttiers", 
                                                         label = "Choose Tiers", 
                                                         choices = c(1:5,"NA") , selected = 4 , multiple = T),
                                             br("Support: proportion of students information in the data"),
                                             sliderInput(
                                                 inputId = "supp",
                                                 label = "Choose a minimum support",
                                                 min = 0.01,
                                                 max = 0.1, # max 10% of dataset
                                                 value = 0.01,
                                                 step = 0.01
                                             ),
                                             #amount
                                             br("Confidence: likelihood that rhs (right-hand-side) depends on lhs (left-hand-side)"),
                                              sliderInput(
                                                 inputId = "conf",
                                                 label = "Choose a minimum confidence",
                                                 min = 0.4,
                                                 max = 0.9,
                                                 value = 0.6,
                                                 step = 0.1
                                             ),
                                             br("Lift: strength of association (If Lift > 1: strong positive relationship)"),
                                             sliderInput(
                                                 inputId = "lift",
                                                 label = "Choose a minimum lift",
                                                 min = 1.0,
                                                 max = 5.0,
                                                 value = 1.0, 
                                                 step = 0.1
                                             )
                                             ), #box1
                                         box(width = 10, title ="Explore Student Characteristics by Tier", solidHeader = T, status = "info", 
                                             plotOutput("freqitemplot"), 
                                             h4("Find rules with rhs containing First Term GPA"),
                                             DTOutput("filteredrules"),
                                             h3("Summary Table by Cohort"),
                                             gt_output("gtsummarytable")
                                             ) #box2
                                         
                                         
                                     )
                                     ), #students tab
                             # courses tab
                             tabItem(tabName = "coursegrades",
                                     fluidRow(
                                         box(width = 2, title = "Filter...", solidHeader = T, status = "warning", background = "black",
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
                                             status = "warning", actionButton("frerqCRS", label = "Show Updated Info."),
                                             plotly::plotlyOutput("bar_course_pass_plot"))), #1st row
                                     
                                     fluidRow(
                                         box(width = 2, title = "Filter...", solidHeader = T, background = "black",status = "warning",
                                             selectInput(inputId = "coursename", label = "Select a Course",
                                                         choices = NULL,
                                                         selected = NULL),br(),
                                             "Included Grades are A to F range, NF (Non-attending/Fail), U (Unsatisfactory), and W (Withdrawn).",
                                             br(),
                                             selectInput(inputId = "courseterms", label = "Select a Term",
                                                         choices = NULL,
                                                         selected = NULL)
                                             ),
                                         
                                         box(width = 10, title = "APR by 1st Year Course Grades by Program (included multiple attempts)", 
                                                     solidHeader = T, status = "warning", 
                                                     actionButton("aprgradeCRS",label = "Show Updated Info."),
                                                     plotly::plotlyOutput("bar_course_apr_plot"))
                                                 ) # 2nd row
                                     ), #course tabname/2nd row
                             # sankey tab
                             tabItem(tabName= "flowchart", 
                                     fluidRow(
                                                column(2,selectInput("movecohort","Choose a Cohort", choices = sort(unique(new_sankeydata_v1$Cohort)), selected = NULL )),
                                                column(2,selectInput("movelevel","Choose a Category", choices =c("College","Department","Program"), selected = NULL, multiple = FALSE )),
                                                column(2,selectInput("choosedirection", label = "Choose a Direction", choices = c("Forward","Backward"), selected = NULL)),
                                                column(2,selectInput("moveentry","Choose an Entry/Exit", choices = NULL , selected = NULL )),
                                                column(2,numericRangeInput(inputId = "sankeydirectionhsgpa", label = "Choose High School GPA", value = c(0.00,5.04), separator = " to ", step=0.01, width = "100%", min=0.00, max = 5.04 )), 
                                                column(2,"Click to refresh", br(), actionButton("entrysankey", label = "Start Flow Graph Update", class = "btn-lg btn-danger")),
                                                bsTooltip(id="choosedirection", title = "Forward: entry-to-exit, Backward: exit-to-entry",
                                                placement = "right",  trigger = "hover",options = list(delay = list(show=1000, hide=3000 )))
                                                ), #1st flow
                                      #sankey updated: 1st row is filters and description. 2nd row is plot
                                     fluidRow(
                                         column(8,  box(width = 12,#title = "Moving In/Out", solidHeader = T, status = "success",
                                          networkD3::sankeyNetworkOutput("sankey_moving", width = "100%",  height = "800px")
                                           )),
                                         column(4,box(width = 12, title= "Flow graph Details:",status = "success",solidHeader = T,# background = "black",
                                                      p("The path from entry to degree time such as direction of switching colleges, departments, or programs by choosing a category"),
                                                      p("The importance of factors including between entry to 2nd Fall such as 1st term GPA, APR, high school GPA, and gender", style = "font-family: 'Georgia'; font-si16pt"),
                                                      p("The path from entry to graduation time by choosing the Forward direction", style = "font-family: 'Georgia'; font-si16pt"),
                                                      p("Previous paths leading to a final category by choosing the Backward direction" ),
                                                      p("How categories are linked with each other by counts and proportion" ),
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
                                         box(width = NULL,title = "Entry to Exit", solidHeader = T,  
                                              DT::DTOutput("flowtable",  width = "100%",  height = "800px"))
                                     )
                                     ),  #program sankey tab
                                     
 
                             # summary tab
                             tabItem(tabName = "metricapr", 
                                      
                                  fluidRow( 
                                      column(2,  
                                             selectInput(inputId = "metriccohort", 
                                                         label = "Select a Cohort", 
                                                         choices = sort(unique(enc_data_ds$Cohort)), selected = 2020, multiple = F )),

                                      column(2,  
                                             selectizeInput(inputId = "metriccollege", 
                                                            label = "Select a group", 
                                                            choices =  c("(All)","Stu_College", "Stu_Department","Stu_ProgramCIPDesc"),
                                                            selected ="(All)"
                                                            )),
                                      
                                     column(2, 
                                             selectInput(inputId = "metricdepart", 
                                                  label ="Select a subgroup", 
                                                  choices = NULL,#c("(All)",sort(unique(enc_data_ds$Stu_College)), sort(unique(enc_data_ds$Stu_Department)), sort(unique(enc_data_ds$Stu_ProgramCIPDesc))), 
                                                  selected = "(All)"  
                                                  )),
             
                                     column(2, 
                                         numericRangeInput(inputId = "metricHSGPA",
                                                           label = "Select HS GPA (Replaced NAs with 0.0)",
                                                           value = c( 0.00,5.04), separator = "To", step = 0.01, width = NULL,  min=0.00, max = 5.04 )), 
                                  # # "Choosing the high school GPA thresholds",br(),
                                  # "(Replaced NAs with 0.00)"
                                     # column(2,
                                     #     selectInput(inputId = "metricCIP", 
                                     #                 label ="Select a Program", 
                                     #                 choices = NULL, #  c("All", sort(unique(enc_data_ds$Stu_ProgramCIPDesc))), selected = "All" 
                                     #     )),
                                  
                                     column(4,  
                                         selectizeInput(inputId = "metricdemo", 
                                                        label = "Select demographics", 
                                                        choices = c("Gender","Ethnicity","FirstGen","FeeResidency"),
                                                        selected = "Gender", multiple= F ))
                                  ), # 1st row for filter
                                  
                                 fluidRow(
                                      p("Please", strong("reselect a new Cohort"), "if you have trouble choosing a subgroup")
                                  
                                  ),
                                  fluidRow(   
                                      box(width = 6, title = "Enrollments & Retention in 1st Year", status = "primary", solidHeader = T,
                                              
                                              plotly::plotlyOutput("enrollment_bar") 
                                                 # style = "height:400px; overflow-y: scroll;overflow-x: scroll;")
                                           ),
                                        box(width = 6,   title = "First Fall GPA", status = "primary", solidHeader = T,
                                                plotly::plotlyOutput("firstfallgpa_plot"))
                                             
                                    ), # 2nd row
                                  br(),
                                 #program chart by cohort
                                 fluidRow(
                                     box(width = 6,  title = "Academic Progress Rate", status = "primary", solidHeader = T,
                                         plotly::plotlyOutput("metricAPR_plot")),
                                     
                                     box(width = 6,  title = "Graduation Time", status = "primary", solidHeader = T,
                                           plotly::plotlyOutput("metricontime_plot"))
                                        #style = "height:500px; overflow-y: scroll;overflow-x: scroll;")
                                    ), # 3rd row
                                 fluidRow(
                                     box(title = "Pivot Table - APR, Retention, and Graduation (filter by HS GPA)", 
                                         solidHeader = T, width = 12,background = "light-blue",
                                         div(style ='overflow-x: scroll',
                                             rpivotTableOutput("metric_pivot", width = "100%", height = "1000px") ) 
                                         )
                                 )
                        )#tabitem
                    ) #tabitems  
                             
                         
       )# dashboardbody

)
#end-ui



#start-server

server <- function(input, output, session) {
    
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
    
    # SAVE for multiple columns table for later
    #  output$pvt1 <- renderPivottabler({
    #     if((length(input$selectRows)>0)&&(nchar(input$selectRows)>0)) {
    #     pt <- PivotTable$new()
    #     findata2 <- fin_ftic_all_data %>%
    #         group_by_(input$selectCols, input$selectRows) %>%
    #         summarise(TotalCohort=n(), TotalAid=sum(year_amount, na.rm=TRUE)) %>%
    #         ungroup()
    #     pt$addData(findata2)
    #     pt$addColumnDataGroups(input$selectCols)
    #     pt$addRowDataGroups(input$selectRows)
    #     if(input$selectCalc=="Total Cohort")
    #     pt$defineCalculation(calculationName="TotalCohort", summariseExpression="sum(TotalCohort)")
    #     else if (input$selectCalc=="Total Amounts")
    #         pt$defineCalculation(calculationName="TotalAid",
    #                              summariseExpression="sum(TotalAid, na.rm=TRUE)")
    #     pt$evaluatePivot()
    #     pivottabler(pt)
    #     }
    # })

 

    
### Student characteristics using apriori algorithm

    filtered_df <-  reactive({
        
        tricounty <- c("Escambia", "Santa Rosa", "Okaloosa")
        
        selected_data <- student_character_1stFall  %>%  
            filter(FTIC_FeeResidency == "Florida Resident (USA)" & HS_CNTY %in% tricounty) %>% #filtered data
            mutate(FTIC_APRCT2 = as.character(FTIC_APRCT)) %>% 
            mutate(FTIC_APRCT3 = ifelse(FTIC_APRCT2 == "Withdrawn", "No", FTIC_APRCT2 )) %>% # from withdrawn to Non-APR
            mutate(HS_NAME2 = as.character(HS_NAME)) %>% 
            mutate(App_Month = format(as.Date(APP_DT), "%m")) %>% 
            mutate(Age = ifelse( FTIC_Age <= 20, "Under 20","Over 21")) %>% 
            mutate(Awared_PELL = ifelse(PELLGrant == 0, "None", ifelse(PELLGrant <= 2000, "($0, $2,000]", "$2,000+"))) %>%
            mutate(TotalAidnotpell = ifelse(TotalFinAid_Term -PELLGrant >= 0, (TotalFinAid_Term -PELLGrant), 0 ),
                   TotalAidnotpell = round(TotalAidnotpell,0)) %>% 
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
            mutate_if(is.character, as.factor) %>% 
            select("HS_Name"=HS_NAME3, 
                   App_Month,
                   "Ethnicity"=FTIC_Ethnicity, 
                   "Gender"=FTIC_Gender,
                   #Merit_Scholarships,
                   #BF_Scholarships,
                   "Entry_Program"=FTIC_ProgramCIPDesc,
                   #Pell_Amount,
                   #TotalAidNotPell_Amount,
                   First_Generation,
                   "FirstTemGPA>=2.00"=FTIC_APRCT3,
                   #FTIC_UWFGPACode,
                   FTIC_Cohort) # remove cohort for freq.plot
        
        
        
    })
    
    output$freqitemplot <- renderPlot({
        # import dataset
        ruledata <- as(filtered_df()[,-8], "transactions")
        
        itemFrequencyPlot( ruledata, topN = 20, main="Top 20 Frequency Plot", type="absolute", col = brewer.pal(8, 'Pastel2'))
        
        
    })
    
    rules <- reactive({
        
        # import data set
        ruledata <- as(filtered_df()[, -8], "transactions")
        
        #myruledata <- read.transactions("writedata1", sep=",", rm.duplicates = TRUE)
        
        rules_t1 <- apriori( data= ruledata,  
                             parameter = list(support = as.numeric(input$supp), confidence = as.numeric(input$conf), minlen=3, maxlen=5),
                             appearance = list(
                                     #rhs = c("FTIC_UWFGPACode=[0,1.99]", "FTIC_UWFGPACode=[2.00,2.76]", "FTIC_UWFGPACode=[2.77,4.00]"),
                                     rhs = c("FirstTemGPA>=2.00=Yes", "FirstTemGPA>=2.00=No"),
                                               default = "lhs")) 
        no_redundant_rules <- rules_t1[!is.redundant(rules_t1)]
        only_sig_rules <- no_redundant_rules[!is.significant(no_redundant_rules),]
        filterd_rules <- subset(only_sig_rules, subset = lift > input$lift)
        
        
        
        
    })
    
    
    
    
    output$filteredrules  <- renderDT({
        
        # no_redundant_rules <- rules()[!is.redundant(rules())]
        # only_sig_rules <- no_redundant_rules[!is.significant(no_redundant_rules),]
        # filterd_rules <- subset(only_sig_rules, subset = lift > 1.1)
        outrules <- inspectDT(sort(rules(), by="lift"))
        
        
    })
    
    output$gtsummarytable <- render_gt({
        
        tabsum <-  filtered_df() %>% 
            mutate(FTIC_Cohort = as.factor(FTIC_Cohort)) 
        
        tabsum %>% tbl_summary( by = FTIC_Cohort,
                                statistic = all_continuous() ~ "{mean} ({sd}) {min} {max}",
                                missing = "no") %>%  add_n() %>% bold_labels() %>% as_gt()
        
        
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
                                 mutate(GPA_HIGHSCHOOL = ifelse(is.na(GPA_HIGHSCHOOL), 0, GPA_HIGHSCHOOL)) %>% 
                                 mutate(response = ifelse(((College == input$moveentry) |(Department == input$moveentry)|(Program == input$moveentry)), "Yes", "No")) %>% 
                                    filter(response == "Yes")
         })
     
     observeEvent(move.HSGPA(), {
         minrange = min(move.HSGPA()$GPA_HIGHSCHOOL, na.rm = T)
         maxrange = max(move.HSGPA()$GPA_HIGHSCHOOL, na.rm = T)
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
                    group_by(VAR1, GEN ) %>%  dplyr::summarise(n =n(), .groups ="drop") %>% ungroup() %>% arrange(-n) %>% select( In= 1, Out= 2, 3)
                
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
                up_flow_data <- rbind(col1, col1_apr, col2, col3, col4, col5, col6, col7 ) 
                print(up_flow_data)
   
        })

     sankey_plot <- eventReactive(input$entrysankey,{
         
                # create nodes and links
                #links
                nodes_FD <- sankey_3rd_data() %>%  select(In, Out) %>% 
                        pivot_longer(c("In","Out"), names_to = "col_name",
                        values_to = "name_match") %>% select(-1) %>% distinct() %>% 
                        mutate(name = str_sub( name_match, end=-4)) %>% as.data.frame()
                nodes_FD$group <-  as.factor( gsub(" ", "-",nodes_FD$name))
                # nodes
                plot_id_FD <- sankey_3rd_data() %>% 
                        mutate( IDIn = match( In, nodes_FD$name_match)-1,
                        IDOut = match(Out, nodes_FD$name_match)-1,
                        Freq =n ) %>% as.data.frame()
                
                #sankey chart
                upsankey <-  sankeyNetwork(
                        Links = plot_id_FD, Nodes = nodes_FD,
                        Source = "IDIn",
                        Target = "IDOut",
                        Value = "Freq",
                        NodeID = "name",
                        #colourScale = my_college_color, 
                        NodeGroup = "group",#LinkGroup = "group",
                        sinksRight = F, iterations = 0,
                        fontSize =10, fontFamily = "Arial",
                        nodeWidth = 40, nodePadding = 25
                )

                htmlwidgets::onRender(upsankey, 'function(el2) { 
    var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i);
    var labels = ["Entry","Gender","1st Term GPA", "APR","2nd Fall","3rd Fall","4th Fall","Degree","Graduation Time"];
    cols_x.forEach((d, i) => {
      d3.select(el2).select("svg")
        .append("text")
        .attr("x", d)
        .attr("y", 15)
        .text(labels[i]);})}')

      })
    output$sankey_moving <- renderSankeyNetwork(sankey_plot()) # need add error message
           
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
    
    # xaxis lable
    
    output$metric_label <- renderText({
        input$metricdemo 
    })
    output$metric_label_1 <- renderText({
        input$metriccohort
    })
    
    #metric chort data
    enc_cohort <-  reactive({
        
        enc_data_ds[(enc_data_ds$Cohort == input$metriccohort),] %>% 
            mutate(Gender= gender, Ethnicity =ethnicity, FirstGen=firstgen, FeeResidency=Stu_FeeResidency)  
        
    })
    
  
    groupdata <- reactive({
        if( input$metriccollege == "(All)" | input$metricdepart == "(All)" ){
            enc_cohort()
        }else if(input$metriccollege == "Stu_College" & input$metricdepart == "(All)" ){
            enc_cohort()
            
        }else if(input$metriccollege == "Stu_Department" & input$metricdepart == "(All)" ){
            enc_cohort()
            
        }else if(input$metriccollege == "Stu_ProgramCIPDesc" & input$metricdepart == "(All)" ){
            enc_cohort()
            
        }else if(input$metriccollege == "Stu_College"  & input$metricdepart != "(All)"){
            enc_cohort() %>%  filter(Stu_College == input$metricdepart)
        }else if(input$metriccollege == "Stu_Department" & input$metricdepart != "(All)"){
            enc_cohort() %>%  filter(Stu_Department == input$metricdepart)
        }else if(input$metriccollege == "Stu_ProgramCIPDesc" & input$metricdepart != "(All)" ){
            enc_cohort() %>%  filter(Stu_ProgramCIPDesc == input$metricdepart)
        }
    })
    
    
    
    observeEvent( enc_cohort(), {
        
        if(input$metriccollege == "(All)"){
            updateSelectInput(inputId = "metricdepart",  choices = "(All)")
        }
        
        else if( input$metriccollege == "Stu_College"  ){
            updateSelectInput(inputId =  "metricdepart", choices = c("(All)",unique(enc_cohort()$Stu_College)))
        }
        else if(input$metriccollege == "Stu_Department"){
            updateSelectInput(inputId =  "metricdepart", choices = c("(All)",unique(enc_cohort()$Stu_Department)))
        }
        else if(input$metriccollege == "Stu_ProgramCIPDesc"){
            updateSelectInput(inputId =  "metricdepart", choices = c("(All)",unique(enc_cohort()$Stu_ProgramCIPDesc)))
        }
    })
    

    enc_filtered_data <- reactive({
        
        groupdata() %>% 
            filter(HSGPA3 >= input$metricHSGPA[1] & HSGPA3 <= input$metricHSGPA[2]) %>% 
            mutate(uwf_term_id3 = uwf_term_id2) %>% 
            mutate(uwf_term_id2 = factor(uwf_term_id2, levels = c("Fall_1","Spri_1","Fall_2"), labels = c("Fall 1", "Spring 1","Fall 2"))) %>% 
            select(uwfid,uwf_term_id2,uwf_term_id3, "selectedfactor"=input$metricdemo,uwf_gpa, gpa2,Graduation_Status )
     })
    #COHORT SIZE
    cohort_size <- count(enc_data_ds, Cohort,uwf_term_id2 ) 
    
   # updated apr summary tab
    
    output$enrollment_bar <- renderPlotly({
        
      #enc_count <-   enc_filtered_data() %>% count(uwf_term_id2, selectedfactor) 
          
    cc <- count(enc_filtered_data(), uwf_term_id2, selectedfactor) 
    cc2 <- left_join(cc, count(cc, uwf_term_id2, wt = n, name ="nn")) %>% 
        mutate(prop = paste(round( n/nn *100,2), "%", sep="")) 
    N <- length(unique(cc2$uwf_term_id2))
    plotly::plot_ly(cc2, x = ~ selectedfactor  ,   color = ~uwf_term_id2 ,colors= RColorBrewer::brewer.pal(N, "Dark2"),
                    y= ~n , type="bar", text =~prop, textposition = "outside") %>% 
        config(displaylogo = FALSE) %>% 
        layout(title = "Enrollments From Frist Fall to Second Fall",
               yaxis = list(title ="Count of uwfid"), 
               xaxis=  list(title=input$metricdemo),  
               barmode = "group", showlegend = TRUE)
            
    })
    
    
    output$firstfallgpa_plot <- renderPlotly({
        first_fall_factor <- enc_filtered_data() %>% 
            mutate(FirstFallGPA = ifelse(is.na(uwf_gpa), "Missing",
                                ifelse(uwf_gpa>= 2.77, ">=2.77",
                                ifelse(uwf_gpa>= 2.00, "[2.00,2.77)", "<2.00")))) %>% #term gpa
            mutate(FirstFallGPA = factor(FirstFallGPA, levels = c("Missing","<2.00","[2.00,2.77)",">=2.77")))
        cc <- count(first_fall_factor, uwf_term_id2, FirstFallGPA, selectedfactor)  #choose vari
        cosize=count(first_fall_factor,uwf_term_id2)
        cc3 <- left_join(cc, count(cc, uwf_term_id2,  FirstFallGPA, wt = n, name ="nn")) %>%  filter(uwf_term_id2 =="Spring 1") %>%  #1st fall gpa
            mutate(prop = paste(round( n/nn*100,2), "%", sep=""))
        # cohort size
        N <- length(unique(cc3$FirstFallGPA)) #number of col
        plotly::plot_ly(cc3, x = ~ selectedfactor  ,   color = ~FirstFallGPA ,colors= RColorBrewer::brewer.pal(N, "Set1"),
                        y= ~n , type="bar", text =~prop, textposition ="outside") %>% 
            config(displaylogo = FALSE) %>% 
            layout( title="Above a 2.77 GPA or Below",
                yaxis = list(title ="Count of uwfid"), xaxis= list(title=input$metricdemo),barmode = "group", showlegend = TRUE)

    })
    
    output$metricAPR_plot <- renderPlotly({
        
        firstterm <-  enc_filtered_data() %>% filter(  uwf_term_id2 =="Fall 1") %>% select(uwfid, "term1"=uwf_term_id2, selectedfactor)
        secondterm <- enc_filtered_data() %>% filter(  uwf_term_id2 =="Fall 2") %>% 
            select(uwfid, "term2"=uwf_term_id2, uwf_gpa )  
        Fall2_GPA <- merge(firstterm, secondterm, by= "uwfid", all.x=TRUE) %>% 
            mutate(APR =  ifelse((is.na(uwf_gpa) | uwf_gpa <  2.00 ), "non-APR","APR")) 
        
        cc <- count(Fall2_GPA,  APR, selectedfactor)  #choose vari
        cosize=count(Fall2_GPA, APR)
        cc4 <- left_join(cc, count(cc,  APR, wt = n, name ="nn"))  %>% 
            mutate(prop = paste(round( n/nn*100,2), "%", sep=""))
        # cohort size
        N <- length(unique(cc4$APR)) #number of col
        plotly::plot_ly(cc4, x = ~ selectedfactor  ,   color = ~APR ,colors= c("#009CDE","#B6ADA5"), #RColorBrewer::brewer.pal(N, "Pastel1"),
                        y= ~n , type="bar", text =~prop, textposition ="outside") %>% 
            config(displaylogo = FALSE) %>% 
            layout(  title="Returned Second Fall with at least a 2.00 GPA",
                yaxis = list(title ="Count of uwfid"), xaxis= list(title=input$metricdemo),barmode = "group", showlegend = TRUE)
        
        
    })
     
    output$metricontime_plot <- renderPlotly({
            
            cc <- count( enc_filtered_data(), uwf_term_id2, Graduation_Status, selectedfactor)  #choose vari
            cosize=count( enc_filtered_data(), uwf_term_id2)
            cc5 <- left_join(cc, count(cc, Graduation_Status,uwf_term_id2, wt = n, name ="nn")) %>%  filter(uwf_term_id2 =="Fall 1") %>%   #1st fall gpa
            mutate(prop = paste(round( n/nn*100,2),"%",spe=""))  # cohort size
            N <- length(unique(cc5$selectedfactor)) #number of col
            plotly::plot_ly(cc5, x = ~ selectedfactor  ,   color = ~Graduation_Status ,colors= RColorBrewer::brewer.pal(N, "Set2"),
                       y= ~n , type="bar", text =~prop, textposition="outside") %>% 
            config(displaylogo = FALSE) %>% 
            layout(title="Four-Year Graudation Rate", yaxis = list(title ="Count of uwfid"), 
                   xaxis= list(title= input$metricdemo),barmode = "group", showlegend = TRUE)
             
        })
    #gtsummary for metric table
    
    metric_data <- reactive({
        # 1st fall
        first_term <-  enc_data_ds %>% 
            filter(HSGPA3 >= input$metricHSGPA[1] & HSGPA3 <= input$metricHSGPA[2]) %>% 
            filter(  uwf_term_id2 == "Fall_1") %>% mutate(  term1 = uwf_term_id2 ) %>% select(-uwf_gpa)
        #returning fall gpa
        second_term <- enc_data_ds  %>% filter(  uwf_term_id2 =="Fall_2") %>% select(uwfid, uwf_term_id2, uwf_gpa) %>% 
            mutate( term2 = uwf_term_id2  )  
        Fall2_GPA <- merge(first_term, second_term, by= "uwfid", all.x = TRUE) %>% 
            mutate(APR =  ifelse((is.na(uwf_gpa) | uwf_gpa <  2.00 ), "non-APR","APR")) 
        # by cohort 
        metricsum <-  Fall2_GPA %>% 
            mutate(Cohort = as.factor(Cohort)) %>% 
            select(Cohort,APR,Stu_College,Stu_Department,Stu_ProgramCIPDesc,
                   Stu_FeeResidency,gender,ethnicity,firstgen,Graduation_Status, tier,
                   "HS_GPA"=HSGPA3,"UWFGPA"=uwf_gpa ,"Prior_Hours"=Stu_TotalUniversityHours
            )
        
    })
    
    metric_pivot_data <- reactive({
        enc_data_ds %>% filter(HSGPA3 >= input$metricHSGPA[1] & HSGPA3 <= input$metricHSGPA[2])  
        
        #demo and gradu
        metric_demo <- enc_data_ds %>% select(uwfid, Cohort, terms,Stu_College, Stu_Department, Stu_ProgramCIPDesc,
                                              Gender= gender, FirstGen=firstgen, "FeeResidency"=Stu_FeeResidency,Ethnicity =ethnicity,
                                              "Tier"=tier,Graduation_Status, "HS_GPA"=HSGPA3) %>% group_by(uwfid) %>% filter(!duplicated(uwfid))
        #retention
        metric_reten <- enc_data_ds %>% select(uwfid,terms, uwf_term_id2) %>% 
            pivot_wider(names_from = uwf_term_id2, values_from = terms) %>% 
            mutate_at(c(2,3,4), replace_na, 0) %>% 
            mutate_at(c(2,3,4), ~ifelse(. != 0, "Returned", "Stopped"))  
        colnames(metric_reten) <- c("uwfid", "Fall_1_Cohort","Spring_1_Return","Fall_2_Return")
        demo_reten <- merge(metric_demo, metric_reten, by="uwfid",all.x = TRUE)
        #APR
        metric_apr_wide <- enc_data_ds %>% filter(uwf_term_id2 == "Fall_2" ) %>% 
            select(uwfid,uwf_gpa, uwf_term_id2) %>% mutate(APR= ifelse(uwf_gpa >= 2.00, "APR","Non-APR")) %>% 
            select(uwfid, uwf_gpa, APR)
        demo_reten_apr <- merge(demo_reten, metric_apr_wide, by="uwfid", all.x=TRUE) %>% 
            mutate(APR= ifelse(is.na(APR), "Non-APR",APR)) %>% 
            mutate(APR = ifelse(Cohort == 2021, NA, APR)) %>% 
            mutate(Fall_2_Return = ifelse(Cohort ==2021, NA, Fall_2_Return))
        #spring gpa
        Fall1_gpa <- enc_data_ds %>% filter(uwf_term_id2 == "Spri_1" ) %>% 
            mutate(Fall_1_GPA = ifelse(uwf_gpa >= 2.77, ">=2.77", ifelse(uwf_gpa>= 2.00, "[2.00,2.77)", "<2.00" ))) %>% 
            select(uwfid, Fall_1_GPA)
        
        # final data
        demo_reten_apr_1stgpa <- merge(demo_reten_apr,Fall1_gpa, by = "uwfid", all.x=TRUE ) %>% 
            mutate(Fall_1_GPA = ifelse(is.na(Fall_1_GPA), "Missing",Fall_1_GPA ))
         
         
    })
    
    output$metric_pivot <-  renderRpivotTable({
        
       mertic_pivot_data <-  metric_pivot_data() %>%  mutate(Fall_1_GPA = factor(Fall_1_GPA, levels = c("<2.00" ,"[2.00,2.77)", ">=2.77", "Missing"))) %>% 
           select(Cohort,"Terms"=terms, "College"=Stu_College, "Department"=Stu_Department,"Program"=Stu_ProgramCIPDesc,
                  Gender,"First_Genderation"=FirstGen, 9:16, "UWF_GPA"=uwf_gpa, APR, Fall_1_GPA ) 
             
            rpivotTable(
                data = mertic_pivot_data,
                rows = c("College", "Cohort" ),
                cols = c("APR"),
                aggregatorName = "Count as Fraction of Rows",
                #vals = "",
                rendererName = "Table"
            )



     })
        
        
        
        
    # Bar Chart for CIP Metrics
 #    CipMetricTablebyCohort <-  eventReactive(input$barchartcip,{
 #        
 #        notapr=c("Dropped","WithdrawnFTIC")
 #        m_bar_cip <-  metricbycohort() %>%  
 #            mutate(RETENTION = ifelse(UWFFall2CIPTitle %in% notapr , "No","Yes")) %>%
 #            mutate(GRADUATIONin4YRS = ifelse(Gradu_Time == "Grad4yrs","Yes","No")) %>% 
 #            select("ENTRY_PROGRAM"=UWFFall1CIPTitle,  "Metrics"=input$barchartmetric) %>% 
 #            group_by( ENTRY_PROGRAM,  Metrics) %>%  
 #            dplyr::summarise(Count =n(), .groups = "drop") %>% # count() %>%  #group_by( Cohort,  Metrics) %>% #ENTRY_DEPARTMENT,
 #            tidyr::pivot_wider(names_from = Metrics, values_from = Count, values_fill = 0) %>% 
 #            mutate(MetricCount = Yes) %>% rowwise() %>% 
 #            mutate(CohortSize = sum(No, Yes)) %>%
 #            mutate_at(vars(2:3), ~(round(./CohortSize*100,2))) %>%
 #            mutate( MetricPercent = paste(Yes, "%", sep=" ")) %>% 
 #            select(ENTRY_PROGRAM, MetricCount,CohortSize,MetricPercent,Yes)
 #        #help#
 #        ytext <- m_bar_cip$CohortSize
 #        y1mp <- m_bar_cip$MetricCount
 #        m.ypp <- m_bar_cip$MetricPercent
 #        y1mpp <- round(m_bar_cip$MetricCount/m_bar_cip$CohortSize*100,2)# percentage digit 2
 #        per_y1 <-  list( tickfont = list(color = "red"), overlaying = "y1mpp", side="right", title = "<b> % of performance")
 #        
 #        #bar plot
 #        bar_cip <-  plotly::plot_ly(m_bar_cip, x= ~reorder(ENTRY_PROGRAM, -CohortSize), y = ~CohortSize, type = "bar",name = "Cohort Size", text= ytext,
 #                                       textposition="auto",
 #                                       marker = list(color = "#007A33" ) )
 #           
 #        bar_cip %>%  
 #            add_trace( y = ~MetricCount, text = y1mp, name = input$barchartmetric , marker = list(color = "#004C97")) %>%
 #            add_trace( y = ~y1mpp, text= m.ypp,  name = "Performance (%)", marker = list( color = "#009CDE")) %>% #, yaxis = "y2", mode = "line+markers", type ="bar"
 #            layout(title = "",
 #                   xaxis = list( title="", tickfont = list(size = 12, color = "darkgreen")),
 #                   yaxis = list( title= paste(input$barchartmetric, "(Count)", sep=" "),
 #                                 titlefont = list(size = 12, color="red"), tickfont = list(size= 12, color="blue")),
 #                   yaxis2 = per_y1 ,
 #                   legend = list(x = 1, y= 1, bgcolor = 'rgba(255, 255, 255, 0)', bordercolor = 'rgba(255, 255, 255, 0)'),
 #                   barmode = "group", bargap =0.15) %>% 
 #            config(displaylogo = FALSE)
 #        
 # 
 #      
 #    })
 #    
 #     output$barchartCIPMETRIC <- renderPlotly(CipMetricTablebyCohort())
 #        
 # ######### DOWNLOAD RAW METRIC TABLE ######
 #        SUMMetricTable <- reactive({
 #            
 #            all_temp_df <-  sankey_data  %>% 
 #                filter((GPA_HIGHSCHOOL >= input$barcharthsgpa1) & (GPA_HIGHSCHOOL <= input$barcharthsgpa2))
 #        }) 
 #        
 # 
 #     #output$metrictable <- downloadHandler(
 #        metriciptable1 <-  eventReactive(input$summaryMetric,{
 #            
 #            notapr=c("Dropped","WithdrawnFTIC")
 #            #retention
 #            rent_cip <-  SUMMetricTable() %>%   
 #                mutate(RETENTION = ifelse(UWFFall2CIPTitle %in% notapr , "No","Yes")) %>%
 #                group_by(ENTRY_COLLEGE,ENTRY_DEPARTMENT,"ENTRY_PROGRAM"=UWFFall1CIPTitle,Cohort, RETENTION) %>% count() %>% 
 #                tidyr::pivot_wider( names_from = RETENTION, values_from = n ) %>%
 #                replace(is.na(.), 0) %>% mutate(RETENTION = Yes)  %>% select(-No, -Yes)
 #            #apr
 #            apr_cip <-  SUMMetricTable() %>% 
 #                group_by(ENTRY_COLLEGE,ENTRY_DEPARTMENT,"ENTRY_PROGRAM"=UWFFall1CIPTitle,Cohort, APR) %>% count() %>% 
 #                tidyr::pivot_wider( names_from = APR, values_from = n ) %>% 
 #                replace(is.na(.), 0) %>% mutate(APR = Yes) %>% select(-No,-Yes)
 #            apr_rent_cip <- merge(rent_cip,apr_cip, by=c("ENTRY_COLLEGE","ENTRY_DEPARTMENT","ENTRY_PROGRAM", "Cohort" ), all.x = T)
 #            #graduation 
 #            gradu_cip <-  SUMMetricTable() %>% 
 #                group_by(ENTRY_COLLEGE,ENTRY_DEPARTMENT,"ENTRY_PROGRAM"=UWFFall1CIPTitle,Cohort, Gradu_Time) %>% count() %>% 
 #                tidyr::pivot_wider( names_from = Gradu_Time, values_from = n ) %>% 
 #                replace(is.na(.), 0) %>% 
 #                mutate(CohortSize = sum(Grad4yrs , Grad5yrs ,Grad6yrs ,NoDegree)) 
 #            
 #            all_cip_df <- merge(apr_rent_cip, gradu_cip,  by=c("ENTRY_COLLEGE","ENTRY_DEPARTMENT","ENTRY_PROGRAM", "Cohort" ), all.x = T)
 #            
 #            all_cip_df1 <-   all_cip_df %>% data.frame() %>% 
 #                mutate(across(c(5:10), ~round(.x/CohortSize*100,2), .names = "{col}%")) %>%
 #                select(1:8,10,9,11:17) # check location NoDegree%
 #            
 # 
 #            DT::datatable(all_cip_df1[,c(1:4,11,5:10,12:17)],  extensions = "Buttons",
 #                          filter = "top",
 #                          options = list(dom="Blfrtip",
 #                                         buttons=c("copy","csv","excel","pdf","print"),
 #                                         lengthMenu=list(c(10,25,50,-1), c(10,25,50,"All")),pageLength=25))
 #        })
 #        
 #        output$summetrictable  <- renderDT(metriciptable1())
        
            
        #     filename = function() {
        #         paste("data-", Sys.Date(), ".csv", sep="")
        #     },
        #     content = function(file){
        #         shiny::withProgress(
        #             message = paste0("Downloading"),
        #             value = 0,
        #             {
        #                 shiny::incProgress(1/10)
        #                 Sys.sleep(1)
        #                 shiny::incProgress(5/10)
        #         
        #         write_csv(summary_cip(), file, row.names = F)
        #     }
        #     )
        #     }
        # )
        
        
        # 
        # output$barcharCIPMETRIC <- eventReactive(input$barcharcip,{
        # 
        #     MetricTabledf() %>%
        #         filter(ENTRY_COLLEGE == input$metriccollege[-1] & Cohort == input$cipcohort )
        # 
        # 
        # })


    #     r <- purrr::map_dbl(collegename[, 2:7], ~.x)
    #     nms <- names(r)
    #     
    #     #code to plot the radar
    #     plot_ly(
    #         type = 'scatterpolar',
    #         r = r,
    #         theta = nms,
    #         fill = 'toself', fillcolor="#4169E1",opacity=0.6,
    #         mode = 'markers'
    #         )   %>%
    #         plotly::layout(  
    #             polar = list(
    #                 radialaxis = list(
    #                     visible = T,
    #                     range = c(0,max(r)))), showlegend = F)
    #     
     
 

   #output5
   # Bar chart for department in summary tab
   # bar_college <- reactive({
   #     sankey_data[(sankey_data$ENTRY_COLLEGE == input$metriccollege),] 
   # })
   # 
   # observeEvent(bar_college(), {
   #     choice <-  unique(bar_college()$ENTRY_DEPARTMENT)
   #     shiny::updateSelectInput(inputId ="bardepartment", choices = choice)
   # })
   # 
   # metricdf_bar <- reactive({
   #     
   #     temp_df <- bar_college() %>%  
   #         filter(ENTRY_COLLEGE == input$metriccollege) %>% 
   #        filter(ENTRY_DEPARTMENT == input$bardepartment)  %>% 
   #         filter((GPA_HIGHSCHOOL >= input$barcharthsgpa1) & (GPA_HIGHSCHOOL <= input$barcharthsgpa2))
   # 
   # })
   # 
   #  barchart_METRIC_test <- eventReactive(input$barchartrall,{
   # 
   #      notapr=c("Dropped","WithdrawnFTIC")
   #       bardf <-   metricdf_bar() %>%
   #             mutate(GRADUATIONin4YRS = ifelse(Gradu_Time == "Grad4yrs","Yes","No")) %>% 
        #        mutate(RETENTION = ifelse(UWFFall2CIPTitle %in% notapr, "No","Yes")) %>%
        #        select(Cohort,  "Metrics"=input$barchartmetric) %>% 
        #        group_by( Cohort,  Metrics) %>%  
        #        count() %>%   
        #        tidyr::pivot_wider(names_from = Metrics, values_from = n, values_fill = 0) %>% 
        #        arrange(Cohort) %>% 
        #        mutate(MetricCount = Yes) %>% rowwise() %>% 
        #        mutate(CohortSize = sum(No, Yes)) %>%
        #        mutate_at(vars(2:3), ~(round(./CohortSize*100,2))) %>%
        #        mutate( MetricPercent = paste(Yes, "%", sep=" ")) %>% 
        #        select(Cohort, MetricCount,CohortSize,MetricPercent) #check  
        #  ymp= round(bardf$MetricCount/bardf$CohortSize*100,2)
        #  ycs= bardf$CohortSize
        #  ymc= bardf$MetricCount
        #  ypp= bardf$MetricPercent
        #  per_y <-  list( tickfont = list(color = "red"), overlaying = "ymp", side="right", title = "<b> % of performance")
        #  #bar plot
        # bar_depart <-  plotly::plot_ly(bardf, x= ~Cohort, y = ~CohortSize, type = "bar",name = "Cohort Size", text= ycs,
        #                  textposition="auto",
        #                  marker = list(color = "#007A33" )
        #                  )  
   #      bar_depart %>%  add_trace( y = ~MetricCount, text = ymc, name = input$barchartmetric , marker = list(color = "#004C97")) %>%
   #                      add_trace( y = ~ymp,text=ypp,  name = "Performance (%)", marker = list( color = "#009CDE")) %>% #, yaxis = "y2", mode = "line+markers", type ="bar"
   #                      layout(title = "",
   #                           xaxis = list( title="", tickfont = list(size = 12, color = "darkgreen")),
   #                           yaxis = list( title= paste(input$barchartmetric, "(Count)", sep=" "),
   #                                  titlefont = list(size = 12, color="red"), tickfont = list(size= 12, color="blue")),
   #                           yaxis2 = per_y ,
   #                          legend = list(x = 1, y= 1, bgcolor = 'rgba(255, 255, 255, 0)', bordercolor = 'rgba(255, 255, 255, 0)'),
   #                          barmode = "group", bargap =0.15) %>% 
   #                      config(displaylogo = FALSE)
   #  })
   # 
   # output$barchartMETRIC <- renderPlotly(barchart_METRIC_test())
   # 
   #  
   # 
   # ### Table for colleges or uwf
   # UWFtable_all <-  reactive({
   #    rows <- sankey_data %>% 
   #            mutate(test= (input$metriccollege == "UWF" | sankey_data$ENTRY_COLLEGE == input$metriccollege), .groups="drop" )
   #    apr_table <-  sankey_data[rows$test,,drop= FALSE]  
    #   apr_table_filter <- apr_table %>%  filter((GPA_HIGHSCHOOL >= input$barcharthsgpa1) & (GPA_HIGHSCHOOL <= input$barcharthsgpa2))
    # })
    # 
    # 
    # UWF_Table <- eventReactive(input$uwftable,{
    #     
    #     notapr= c("Dropped","WithdrawnFTIC")
    #    rent_college <-  UWFtable_all() %>%   
    #        mutate(RETENTION = ifelse(UWFFall2CIPTitle %in% notapr , "No","Yes")) %>%
    #        group_by( Cohort, RETENTION) %>% count() %>% 
    #        tidyr::pivot_wider( names_from = RETENTION, values_from = n ) %>%
    #        replace(is.na(.), 0) %>% mutate(RETENTION=Yes)  %>% 
    #        select( -Yes,-No)
    #    
    #    rapr_college <-  UWFtable_all() %>% 
    #        group_by( Cohort, APR) %>% count() %>% 
    #        tidyr::pivot_wider( names_from = APR, values_from = n ) %>% 
    #        replace(is.na(.), 0) %>% mutate(APR = Yes) %>% 
    #        select( -Yes,-No) 
    #    
    #    apr_rent <- merge(rent_college,rapr_college, by=c("Cohort" ), all.x = T)
    #    
    #    gradu_college <-  UWFtable_all() %>% 
    #        group_by( Cohort, Gradu_Time) %>% count() %>% 
    #        tidyr::pivot_wider( names_from = Gradu_Time, values_from = n ) %>% 
    #        replace(is.na(.), 0) %>% rowwise() %>% 
    #        mutate(CohortSize = sum(Grad4yrs , Grad5yrs ,Grad6yrs ,NoDegree)) 
   #     
   #     all_df <- merge(apr_rent, gradu_college,  by=c("Cohort" ), all.x = T)
   #     
   #      gradu <-  all_df %>%  mutate(Grad5yrs = (Grad4yrs + Grad5yrs) , Grad6yrs  = (Grad5yrs + Grad6yrs))
   # 
   #      #final table
   #      final_metric_df <-   gradu %>%  rowwise() %>% 
   #          mutate(across(c(2:7), ~round(.x/CohortSize*100,2), .names = "{col}%")) %>% 
   #          select(1,8,2:7,9:14)
   #  
   #       datatable(final_metric_df) 
   #        
   #  })   
   #  
   # output$cohortaprtable  <- renderDT(UWF_Table()) 
   
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
  bar_course_pass <- eventReactive(input$frerqCRS,{
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
 

bar_course_apr <- eventReactive(input$aprgradeCRS,{
    
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
