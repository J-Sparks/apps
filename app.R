
library(shiny)
library(networkD3)
library(tidyverse)
library(DT)
library(shinydashboard)
sankey_data <- readRDS("final_destination_long.rds") # no zero hs gpa

CIP_college <- unique(sankey_data$ENTRY_COLLEGE) %>% sort()
CIP_depart <- unique(sankey_data$ENTRY_DEPARTMENT) %>% sort()
CIP_choice <- unique(sankey_data$UWFFall1CIPTitle) %>% sort()
cohort_choice <- sort(unique(sankey_data$Cohort), decreasing = F)  
maxhsgpa <- max(sankey_data$GPA_HIGHSCHOOL)
minhsgpa <- min(sankey_data$GPA_HIGHSCHOOL)

#start-ui

ui <- dashboardPage( skin = "green",
                     dashboardHeader(title="From Entry To End Program", titleWidth = 300),
                     dashboardSidebar( width = 300,
                        menuItem( text= "Data", startExpanded = T,
                            menuSubItem(text="Flow Chart", tabName = "refreshPlotly", icon = icon("fas fa-stream") ),
                            menuSubItem(text = "Data Table", tabName = "flowTable", icon= icon("fas fa-table")),
                                       sidebarMenu(width = 300,
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
                                                               min= 0.00, max= 6.50, value = c(0.0, 4.50) 
                                                   ),
                                                   sliderInput(inputId = "uwfgpay1",
                                                               label = "Select UWFGPA Year 1", step = 0.05, dragRange = F,
                                                               min=0.00, max = 4.00, value = c(0.00, 4.00)
                                                               ),
                                                   
                                                   actionButton("refreshPlotly", label = "Update")
                                                   
                                       ))# item
                            ), # sidebarmenu
                     
        dashboardBody(
                         tags$head( 
                             tags$style(HTML(".main-sidebar { font-size: 15px; }")) #change the font size to 20
                         ),
                         tabItems(
                             tabItem(tabName= "refreshPlotly", sankeyNetworkOutput("sankey_FD",
                                                                                   width = "100%", 
                                                                                   height = "800px")),
                             tabItem(tabName = "flowTable", DTOutput("flowtable", 
                                                                     width = "100%", 
                                                                     height = "800px"))
                         )# tabitems
                     )# dashboardbody
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
    
    observeEvent(Cohort(), {
        minrange = min(Cohort()$GPA_HIGHSCHOOL)
        maxrange = max(Cohort()$GPA_HIGHSCHOOL)
        mingparange = min(Cohort()$UWFGPAY1)
        maxgparange = max(Cohort()$UWFGPAY1)
        shiny::updateSliderInput(inputId ="gparange", min = minrange, max= maxrange)
        shiny::updateSliderInput(inputId ="uwfgpay1", min = mingparange, max= maxgparange)
    })
    # 
    # gparange <- reactive({
    #     req(input$gparange)
    #     filter(Cohort(), gparange <= input$gparange[1], gparange >= input$gparange[2])
    # })
    # 
    # observeEvent(gparange(), {
    #     mingparange = min(gparange()$UWFGPAY1)
    #     maxgparange = max(gparange()$UWFGPAY1) 
    #     shiny::updateSliderInput(inputId ="uwfgpay1", min = mingparange, max= maxgparange)
    # })
    
    
    flowdata <- reactive({
        
        flowdatatable <- sankey_data[((sankey_data$ENTRY_COLLEGE == input$college) & 
                                    (sankey_data$ENTRY_DEPARTMENT == input$department) & 
                            (sankey_data$UWFFall1CIPTitle == input$CIP) & 
                            (sankey_data$Cohort == input$Cohort) #&
                            #(sankey_data$GPA_HIGHSCHOOL >= input$gparange[1] & sankey_data$GPA_HIGHSCHOOL <= input$gparange[2] )
        ) , ]
        
        print(flowdatatable)
    })
    
    
    updatedata <- reactive({
        
        final_destination_long1 <- sankey_data[((sankey_data$ENTRY_COLLEGE == input$college) & 
                                  (sankey_data$ENTRY_DEPARTMENT == input$department) & 
                                  (sankey_data$UWFFall1CIPTitle == input$CIP) & 
                                  (sankey_data$Cohort == input$Cohort) &
                                  (sankey_data$GPA_HIGHSCHOOL >= input$gparange[1] & sankey_data$GPA_HIGHSCHOOL <= input$gparange[2]) &
                                  (sankey_data$UWFGPAY1 >= input$uwfgpay1[1] & sankey_data$UWFGPAY1 <= input$uwfgpay1[2])     ) , ]  %>%
            select(-ENTRY_COLLEGE,-ENTRY_DEPARTMENT,-Cohort, -GPA_HIGHSCHOOL, -UWFGPAY1) %>% 
            mutate(
                UWFFall1CIPTitle = case_when(UWFFall1CIPTitle != ""~ paste0(UWFFall1CIPTitle, sep="_Y1")),
                APR = case_when(APR != ""~ paste0(APR, sep="APR")),
                UWFFall2CIPTitle = case_when(UWFFall2CIPTitle != ""~ paste0(UWFFall2CIPTitle, sep="_Y2")),
                UWFFall3CIPTitle = case_when(UWFFall3CIPTitle != ""~ paste0(UWFFall3CIPTitle, sep="_Y3")),
                UWFFall4CIPTitle = case_when(UWFFall4CIPTitle != ""~ paste0(UWFFall4CIPTitle, sep="_Y4")),
                DEGREECIPTitle = case_when(DEGREECIPTitle != ""~ paste0(DEGREECIPTitle, sep="END")),
                Gradu_Time = case_when(Gradu_Time != ""~ paste0(Gradu_Time, sep="GRD")))
        
        # create flow 1toAPR
        final_destination0 <- final_destination_long1 %>% 
            group_by(UWFFall1CIPTitle,APR) %>% 
            summarise(n =n()) %>% ungroup() %>% 
            select( In= 1, Out= 2, 3)
        
        # create flow APRto2
        final_destination1 <- final_destination_long1 %>% 
            group_by(APR,UWFFall2CIPTitle) %>% 
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
        flow_data01 <- rbind(final_destination0, final_destination1) 
        flow_data23 <- rbind(final_destination2, final_destination3) 
        flow_data45 <- rbind(final_destination4, final_destination5) 
        flow_data_A <- rbind(flow_data01, flow_data23) 
        flow_data_B <- rbind(flow_data_A, flow_data45)
        
        print(flow_data_B)
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
            fontSize = 13, fontFamily = "Arial",
            nodeWidth = 30, nodePadding = 20)
        
        htmlwidgets::onRender(sanp, '
  function(el) { 
    var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i);
    var labels = ["Entry", "APR", "Year 2", "Year 3","Year 4", "Final Degree","Graduation Time"];
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
    #output1
    output$sankey_FD <- renderSankeyNetwork(sankeychart())
    #output2
    output$flowtable <- renderDT({
        
        FlowTable <- flowdata() %>% 
            select("Entry"=UWFFall1CIPTitle, APR, "Year2"=UWFFall2CIPTitle,"Year3"=UWFFall3CIPTitle,
                   "Year4"=UWFFall4CIPTitle,"FinalDegree"=DEGREECIPTitle,"GraduationTime"=Gradu_Time,Cohort,GPA_HIGHSCHOOL,UWFGPAY1 ) %>% 
            group_by(Entry,APR,Year2,Year3,Year4,FinalDegree,GraduationTime,Cohort) %>% 
            dplyr::summarise(  MinHSGPA=min(GPA_HIGHSCHOOL, na.rm=T),
                               MaxHSGPA=max(GPA_HIGHSCHOOL, na.rm=T) , 
                               MeanHSGPA= round(mean(GPA_HIGHSCHOOL, na.rm=T),2),
                               MeanUWFGPAY1= round(mean(UWFGPAY1, na.rm=T),2),
                               Count = n(), .groups= "drop") %>% ungroup()  
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
    
} #end-sever

shinyApp(ui = ui, server = server)

