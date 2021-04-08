library(shiny)
library(DT)
library(readr)
library(dplyr)
library(shinydashboard)
library(plotly)


V2_ID_ENC_DEG_DATA <- as.data.frame( read.csv("C:/PAM_Jay/ProgramChanges/V2_ID_ENC_DEG_DATA.csv"))

data <- V2_ID_ENC_DEG_DATA 
 

CollegeChoice <- unique(data$Begin_col_desc)
DepartChoice <- unique(data$Begin_deprt_desc)
CIPProgChoice <- unique(data$Begin_CIP_desc)
CohortChoice <- unique(data$Cohort)




ui <- dashboardPage(
    dashboardHeader(title="UWFProgramChanges"),
    dashboardSidebar(
        sidebarMenu(
            menuItem(text = "Data", startExpanded = T,  
                     menuSubItem(text = "Plot",tabName = "refreshPlotly", icon = icon("chart-bar")),
                     menuSubItem(text = "Table", tabName = "programtable", icon=icon("table"))),
            selectInput("CollegeSelector", label= "Select Entry College:", choices = CollegeChoice),
            selectInput("DepartmentSelector", label= "Select Entry Department:", choices = NULL),
            selectInput("ProgramSelector", label= "Select Entry Program:", choices = NULL),
            actionButton("refreshPlotly", label  = "Update")
         )
     
),
   
 
        
        
        
     dashboardBody(
         tabItems( 
             tabItem(
               tabName = "refreshPlotly", plotlyOutput("plot1", height = 800)),
        
             tabItem(
               tabName = "programtable", DTOutput(outputId = "something")
             )

        )
     )
  
 
)

 server <- function(input, output, session){
   
     
      college <- reactive({
         
             data[(data$Begin_col_desc == input$CollegeSelector),] 
         })
      
      observeEvent(college(), {
          choice <-  unique(college()$Begin_deprt_desc)
          shiny::updateSelectInput(inputId ="DepartmentSelector", choices = choice)
      })
      
      
      department <- reactive({
          req(input$DepartmentSelector)
          filter(college(), Begin_deprt_desc == input$DepartmentSelector)
          })
      
      observeEvent(department(),{
          choice <- unique(department()$Begin_CIP_desc)
          shiny::updateSelectInput(inputId = "ProgramSelector", choices = choice)
      })
      
     
      PlotData <- reactive({
          
          filteredData1 <- data[ ((data$Begin_col_desc == input$CollegeSelector) & (data$Begin_deprt_desc == input$DepartmentSelector) & (data$Begin_CIP_desc == input$ProgramSelector)), ]                          
          
          return(filteredData1)
          
      })
      
      
      
      output$something <- renderDT({
          
          Tabledata <- PlotData() %>% group_by(Cohort, TermYear, ChangedProg) %>% 
                       dplyr::summarise(TotalCounts=n(), TotalChangeProgram=sum(ChangedProg))
          
          datatable(Tabledata, extensions = "Buttons",caption = "Number of Program changes by Academic Term (Cohort: 2014 - 2018)",
                    filter = "top",
                    options = list(dom="Blfrtip",buttons=c("copy","csv","excel","pdf","print"), lengthMenu=list(c(10,25,50,-1), c(10,25,50,"All")),pageLength=25))
          
      })    
 
   
       
    

    plot1 <- eventReactive(input$refreshPlotly,{
        
        PlotData() %>% 
            filter( ChangeInd != "StayedProg" & ChangedProg == 1) %>%
            filter(Cohort <= 2018) %>% 
            filter(Begin_CIP_desc != Stu_ProgramCIPDesc) %>% 
            count(  Stu_ProgramCIPDesc ) %>% 
            arrange(-n) %>% 
            as.data.frame() %>% 
            top_n(20) %>% 
            plot_ly( x= ~ reorder(Stu_ProgramCIPDesc, -n), y=~n, color = ~Stu_ProgramCIPDesc ,type="bar") %>% 
            layout( title = "Top 20+ Destination Programs for FTIC Migration", 
                    yaxis = list(title = "2014-2018 Cohort: Counted 1st Migration Only"),
                    xaxis = list(title = "Destination Programs"))
        
    })
    

    output$plot1 <- renderPlotly(plot1())
     
   
}

# Run the application 
shinyApp(ui = ui, server = server)
