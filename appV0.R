
library(shiny)
library(networkD3)
library(tidyverse)
library(shinydashboard)
sankey_data <- readRDS("final_destination_long.rds")

CIP_college <- unique(sankey_data$ENTRY_COLLEGE) %>% sort()
CIP_depart <- unique(sankey_data$ENTRY_DEPARTMENT) %>% sort()
CIP_choice <- unique(sankey_data$UWFFall1CIPTitle) %>% sort()
cohort_choice <- sort(unique(sankey_data$Cohort), decreasing = F)  
maxhsgpa <- max(sankey_data$GPA_HIGHSCHOOL)
minhsgpa <- min(sankey_data$GPA_HIGHSCHOOL)

#start-ui

ui <- dashboardPage( skin = "green",
  dashboardHeader(title="From Entry To End Program", titleWidth = 400),
  dashboardSidebar( width = 400,
    sidebarMenu(width = 400,
      selectInput(inputId = "college", 
                   label   = "Select Entry College",choices = CIP_college, selected = "HMCSE" 
                    ),
      selectInput(inputId = "department", 
                    label   = "Select Entry Department", choices = NULL 
        ),
      selectInput(inputId = "CIP",
              label   = "Select Entry Program",
              choices =   NULL
              ),
      selectInput(inputId = "Cohort",
              label   = "Select Cohort",
              choices =  NULL 
              ),
      sliderInput(inputId = "gparange",
                  label   = "HS GPA Range", step = 0.05, dragRange = F,
                  min= 0.00, max= 6.50, value = c(3.0, 4.50) 
      ),
  actionButton("refreshPlotly", label = "Update")
   
)),

dashboardBody(
  
   sankeyNetworkOutput("sankey_FD",width = "100%", height = "1000px")
    
   
)
)
#end-ui



#start-server
server <- function(input, output, session) {
  
  college <- reactive({
        sankey_data[(sankey_data$ENTRY_COLLEGE == input$college),] 
  })
  
  observeEvent(college(), {
    choice <-  unique(college()$ENTRY_DEPARTMENT)
    shiny::updateSelectInput(inputId ="department", choices = choice)
  })

  department <- reactive({
    req(input$department)
    filter(college(), ENTRY_DEPARTMENT == input$department)
  })
  
  observeEvent(department(),{
    choice <- unique(department()$UWFFall1CIPTitle)
    shiny::updateSelectInput(inputId = "CIP", choices = choice)
  })
  
  CIP <- reactive({
    req(input$CIP)
    filter(department(), UWFFall1CIPTitle == input$CIP)
  })
  
  observeEvent(CIP(), {
    choice <-  sort(unique(CIP()$Cohort))
    shiny::updateSelectInput(inputId ="Cohort", choices = choice)
  })
  
  Cohort <- reactive({
      req(input$Cohort)
      filter(CIP(), Cohort == input$Cohort)
  })
  #(GPA_HIGHSCHOOL >= input$gparange[1] & GPA_HIGHSCHOOL <= input$gparange[2])
  observeEvent(Cohort(), {
      minrange = min(Cohort()$GPA_HIGHSCHOOL)
      maxrange = max(Cohort()$GPA_HIGHSCHOOL) 
      shiny::updateSliderInput(inputId ="gparange", min = minrange, max= maxrange)
  })
 
  
  updatedata <- reactive({
    
    final_destination_long1 <- sankey_data[((sankey_data$ENTRY_COLLEGE == input$college) & 
             (sankey_data$ENTRY_DEPARTMENT == input$department) & 
             (sankey_data$UWFFall1CIPTitle == input$CIP) & 
             (sankey_data$Cohort == input$Cohort) &
             (sankey_data$GPA_HIGHSCHOOL >= input$gparange[1] & sankey_data$GPA_HIGHSCHOOL <= input$gparange[2] )
             ) , ]  %>%
      select(-ENTRY_COLLEGE,-ENTRY_DEPARTMENT,-Cohort, -GPA_HIGHSCHOOL) %>% 
      mutate(
        UWFFall1CIPTitle = case_when(UWFFall1CIPTitle != ""~ paste0(UWFFall1CIPTitle, sep="_Y1")),
        UWFFall2CIPTitle = case_when(UWFFall2CIPTitle != ""~ paste0(UWFFall2CIPTitle, sep="_Y2")),
        UWFFall3CIPTitle = case_when(UWFFall3CIPTitle != ""~ paste0(UWFFall3CIPTitle, sep="_Y3")),
        UWFFall4CIPTitle = case_when(UWFFall4CIPTitle != ""~ paste0(UWFFall4CIPTitle, sep="_Y4")),
        DEGREECIPTitle = case_when(DEGREECIPTitle != ""~ paste0(DEGREECIPTitle, sep="END")),
        Gradu_Time = case_when(Gradu_Time != ""~ paste0(Gradu_Time, sep="GRD")))
    # create flow 1to2
    final_destination1 <- final_destination_long1 %>% 
      group_by(UWFFall1CIPTitle,UWFFall2CIPTitle) %>% 
      summarise(n =n()) %>% ungroup() %>% 
      select( In= 1, Out= 2, 3)
    # create flow2to3
    final_destination2 <- final_destination_long1 %>% 
      group_by(UWFFall2CIPTitle, UWFFall3CIPTitle) %>% 
      summarise(n =n()) %>% ungroup() %>% 
      select( In= 1, Out= 2, 3)
    # create flow3to4
    final_destination3 <- final_destination_long1 %>% 
      group_by(UWFFall3CIPTitle, UWFFall4CIPTitle) %>% 
      summarise(n =n()) %>% ungroup() %>% 
      select( In= 1, Out= 2, 3)
    # create flow4to5
    final_destination4 <- final_destination_long1 %>% 
      group_by(UWFFall4CIPTitle, DEGREECIPTitle) %>% 
      summarise(n =n()) %>% ungroup() %>% 
      select( In= 1, Out= 2, 3)
    # create flow5to6
    final_destination5 <- final_destination_long1 %>% 
      group_by(DEGREECIPTitle, Gradu_Time) %>% 
      summarise(n =n()) %>% ungroup() %>% 
      select( In= 1, Out= 2, 3)
    #create data frame 
    flow_data1 <- rbind(final_destination1, final_destination2) 
    flow_data2 <- rbind(final_destination3, final_destination4) 
    flow_data3 <- rbind(flow_data1, flow_data2)
    flow_data4 <- rbind(flow_data3,final_destination5)
    
    print(flow_data4)
  })
  
  sankeychart <- eventReactive(input$refreshPlotly,{
   # create nodes 
  nodes_FD <- updatedata() %>%  select(In, Out) %>% 
    pivot_longer(c("In","Out"), names_to = "col_name",
                 values_to = "name_match") %>% select(-1) %>% distinct() %>% 
    mutate(name = str_sub( name_match, end=-4)) %>% as.data.frame()
   
  #create links
  plot_id_FD <-  updatedata() %>% 
    mutate( IDIn = match( In, nodes_FD$name_match)-1,
            IDOut = match(Out, nodes_FD$name_match)-1,
            Freq =n) %>% as.data.frame() 
    #sankey chart
   sanp <-  sankeyNetwork(
      Links = plot_id_FD, Nodes = nodes_FD,
      Source = "IDIn",
      Target = "IDOut",
      Value = "Freq",
      NodeID = "name",
      sinksRight = F,
      fontSize = 15, fontFamily = "Arial",
      nodeWidth = 30, nodePadding = 20)
    
    htmlwidgets::onRender(sanp, '
  function(el) { 
    var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i);
    var labels = ["Entry", "Year 2", "Year 3","Year 4", "Final Degree","Graduation Time"];
    cols_x.forEach((d, i) => {
      d3.select(el).select("svg")
        .append("text")
        .attr("x", d)
        .attr("y", 15)
        .text(labels[i]);
    })
  }
')
   
})
  output$sankey_FD <- renderSankeyNetwork(sankeychart())
  
} #end-sever

shinyApp(ui = ui, server = server)
 
