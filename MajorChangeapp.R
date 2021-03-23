library(shiny)
library(shinydashboard)
library(DT)
library(readr)
library(dplyr)


ID_ENC_DEG_info_add_CIP <- as.data.frame( read.csv("C:/PAM_Jay/ProgramChanges/ID_ENC_DEG_info_add_CIP1419.csv"))

data <- ID_ENC_DEG_info_add_CIP
xAxisChoice <-  colnames(data)
yAxisChoice <- colnames(data)

CollegeChoice <- unique(data$Begin_col_desc)
DepartChoice <- unique(data$Begin_Deprt_desc)
CIPProgChoice <- unique(data$Begin_CIP_desc)
CohortChoice <- unique(data$Cohort)
metricChoice <- c("Cohort","ChangeInd","Stu_Classification","Begin_col_desc","Begin_Deprt_desc","Begin_CIP_desc","MaxCPTimes", 
                  "Stu_TotalUniversityHours","Stu_TotalInstGradePoints","TermYear")

ui <- dashboardPage(
    dashboardHeader(
        title = "UWFProgramChanges"),
    dashboardSidebar(
        sidebarMenu(
            selectInput("DepartmentSelector", label= "Select Entry Department:", choices = DepartChoice),
            selectInput("ProgramSelector", label = "Select Entry Program:", choices = CIPProgChoice),
            actionButton("refreshPlotly", label  = "Refresh")
            
        )
    ),
    
    dashboardBody(
        
        fluidRow(   
            plotlyOutput("plot1", height = 800)
            
        )
        
    )
)


 server <- function(input, output){
    filterData <- reactive({
        
        filteredData <- data[((data$Begin_Deprt_desc == input$DepartmentSelector) & (data$Begin_CIP_desc == input$ProgramSelector)),] 
        return(filteredData)
        
    })
    
    plot1 <- eventReactive(input$refreshPlotly,{
        
        filterData() %>% 
            filter( ChangeInd != "StayedProg" & ChangedProg == 1) %>%
            #group_by(STU_ID) %>% filter( Stu_DEMO_TIME_FRAME == max(Stu_DEMO_TIME_FRAME)) %>% 
            mutate(Cohort=factor(Cohort)) %>% 
            count(  Stu_ProgramCIPDesc ) %>% #Cohort,Begin_col,Begin_Deprt,
            arrange(-n) %>% 
            as.data.frame() %>% 
            top_n(20) %>% 
            plot_ly( x= ~ reorder(Stu_ProgramCIPDesc, -n), y=~n, color = ~Stu_ProgramCIPDesc ,type="bar") %>% 
            layout( title = "Top 20+ Destination Programs for FTIC Migration", 
                    yaxis = list(title = "2014-2019 Cohort: Counted 1st Migration Only"),
                    xaxis = list(title = "Destination Programs"))
        
    })
    
    output$plot1 <- renderPlotly(plot1())
}

# Run the application 
shinyApp(ui = ui, server = server)
