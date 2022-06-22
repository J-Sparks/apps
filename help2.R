# acex table
acex_data[acex_data$FTIC_UWFID=="970375780",]
t1 <- acex_data_year %>%
    mutate(Tier = APPLICANT_TIER) %>%
    group_by(FTIC_Cohort, Tier, REPT_TIME_FRAME ) %>% 
     summarise(ACEX = sum(Year_Amount), Count=n()) %>% 
    tidyr::pivot_wider(names_from = REPT_TIME_FRAME, values_from = c(ACEX ,Count)) %>% 
    dplyr::mutate_at(c(3:7), dollar)


 new_tab <-   t1 %>%  
     mutate(Count_20222023 = NA, Count_20232024 = NA, Count_20242025=NA, Count_20252026=NA) %>% 
     mutate(ACEX_20222023 = NA, ACEX_20232024 = NA, ACEX_20242025=NA, ACEX_20252026= NA)

t1_acex <- t1 %>% select(1:7) %>% mutate(ACEX_20222023 = NA, ACEX_20232024 = NA, ACEX_20242025=NA, ACEX_20252026= NA) 
t1_count <- t1 %>% select(1:2,8:12) %>% mutate(Count_20222023 = NA, Count_20232024 = NA, Count_20242025=NA, Count_20252026=NA) 


table_test <- datatable(t1[1:])

library(tables) ## prior to 0.7.72

df <- data.frame(exposure = sample(LETTERS[1:5], 100, TRUE),
                 Group = sample(c("GroupX","GroupY"), 100, TRUE),
                 disease = as.integer(sample(c(0,1), 100, TRUE)))

num <- function(x) base::sum(x, na.rm=TRUE)
output1 <- tabular(Factor(exposure)+1~
            Factor(Group)*
            (Heading("Group Total")*(1)+num*disease+Percent("row")),
        data=df)

set.seed(100)
library(tables)
df <- data.frame(exposure = sample(LETTERS[1:5], 100, TRUE),
                 Group = sample(c("GroupX","GroupY"), 100, TRUE),
                 disease = as.integer(sample(c(0,1), 100, TRUE)))

myTable <- tabular(Factor(exposure)+1~
                       Factor(Group)*
                       (Heading("Group Total")*(1)+Factor(disease)*((n=1)+Heading("%")*Percent(Equal(exposure,Group)))),
                   data=df) %>%  as.data.frame()

datatable(myTable)


# flter matched columns
sankey.hsgpa.filtered_test <- new_sankeydata %>%
    filter(Cohort == 2019) %>% 
    mutate(GPA_HIGHSCHOOL = ifelse(is.na(GPA_HIGHSCHOOL), 0, GPA_HIGHSCHOOL)) %>% 
    mutate(response = ifelse(((Department == College) |(Department == Department)|(Department == Program)), "Yes", "No"))
data.table::setDT(sankey.hsgpa.filtered_test)
uptest <- sankey.hsgpa.filtered_test[,response := as.numeric(length(unique(c("College","Department","Program"))) ==1), by = seq_len(nrow(sankey.hsgpa.filtered_test))]

res <- lapply(sankey.hsgpa.filtered_test[c("College","Department","Program")], `%in%`, sankey.hsgpa.filtered_test$Program)
as.data.frame(res) 

odd_biome.2017 <- new_sankeydata %>% 
    filter(Cohort == 2017) %>%  filter(Deg_ProgramCIPDesc =="BIO Sci.s Gen")

odd_test <- course_data[course_data$UWFID==970522924,]



# Sankey flow
# write.csv(flow_col1,  "flow_DATA_updated202208V0.csv",row.names = F)
cate <- "College"
new_sankeydata <- sankey_new_data
mov1 <- new_sankeydata  %>%  filter(Cohort != 2021) %>% 
        arrange(Stu_UWFID, Fall_ID) %>% 
     dplyr::select(  Stu_UWFID, FALL_UWF, "level"=Department, 
                      Stu_Gender,Cohort, GPA_HIGHSCHOOL, Gradu_Time, TermGPA,EnteringFallGPA,APR)#,

 
mov1_deg <- new_sankeydata  %>%  filter(Cohort != 2021) %>% 
     dplyr::select( Stu_UWFID,  contains("deg") ) %>% group_by(DEGREECIPTitle) %>% ungroup() %>% 
     select(Stu_UWFID, "College"=Deg_College, "Department"=Deg_Department,"Program"=Deg_ProgramCIPDesc) %>% 
    filter(!duplicated(Stu_UWFID)) %>% select(Stu_UWFID,  "DEG"=Department) #6851
two_sankey_data <- merge(mov1,mov1_deg, by="Stu_UWFID", all.x=T )

  #6854
mov2 <- two_sankey_data %>%   #remove level column
    tidyr::pivot_wider( names_from = FALL_UWF , values_from = c(level ,TermGPA,EnteringFallGPA )) %>%  
    mutate(Fall1GPA =  ifelse( TermGPA_UWFFALL1 >= 2.77, "Above2.77 ","Below2.77 "),  
           Year1GPA = ifelse( EnteringFallGPA_UWFFALL2 >= 2.00, "Above2.00","Below2.00") ) %>% filter(!duplicated(Stu_UWFID)) %>%  #6851
    mutate_at(c(8:11), replace_na, "Stopout") %>% filter(!is.na(level_UWFFALL1))
mov12 <-  setNames(mov2[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)],
                   c("ID","GEN", "Cohort","HSGPA","GRD","APR", "DEG",
                     "VAR1","VAR2","VAR3","VAR4",
                     "GPA1", "GPA2", "GPA3", "GPA4",
                     "UWF1","UWF2","UWF3","UWF4", "GPA","UWF"
                   ))

addmargins(table(mov2$Cohort, mov2$level_UWFFALL1))
which(duplicated(mov2$Stu_UWFID))
glimpse(mov2)
colSums(is.na(mov2))
 
#



flow_col2 <-  read_csv( "sankey_new_data.csv")
stay.out <- c("Grad5yrs","Grad6yrs")
test_flow <- flow_col2 %>% group_by(Stu_UWFID ) %>% 
    select(Stu_UWFID,FALL_UWF,College,Department,Program,"Gender"=Stu_Gender,Cohort,
             GPA_HIGHSCHOOL,Gradu_Time,Deg_College,Deg_Department, Deg_ProgramCIPDesc, TermGPA,EnteringFallGPA   ) %>% 
     tidyr::pivot_wider( names_from = FALL_UWF , values_from = c(College,Department,Program ,GPA_HIGHSCHOOL ,TermGPA,EnteringFallGPA    ) ) %>%  
     mutate(Fall1GPA=ifelse(TermGPA_UWFFALL1 >= 2.77, "Above2.77 ","Below2.77 ")) %>% 
     mutate(Year1GPA= ifelse(EnteringFallGPA_UWFFALL2 >= 3.00, "Above3.00 ","Below3.00 "))  
  

# create flow  

new_sankeydata %>% 
    filter(Cohort != 2021) %>% 
    dplyr::select(  Stu_UWFID, FALL_UWF, College,Stu_Gender,Cohort, GPA_HIGHSCHOOL,
                    Gradu_Time,Deg_College,Deg_Department, Deg_ProgramCIPDesc, TermGPA,EnteringFallGPA,APR ) %>%  
    tidyr::pivot_wider( names_from = FALL_UWF , values_from = c(level ,TermGPA,EnteringFallGPA )) %>% 
    mutate(Fall1GPA = ifelse(TermGPA_UWFFALL1 >= 2.77, "Above2.77 ","Below2.77 ")) %>% 
    mutate(Year1GPA = ifelse(EnteringFallGPA_UWFFALL2 >= 3.00, "Above3.00 ","Below3.00 ")) %>% 
    dplyr::mutate_at(c(10:13), tidyr::replace_na, "Stopout") 












test_flow_filter_temp <- test_flow %>% 
    filter(Cohort == 2015)  %>% 
    select(1:7,  contains("program"), contains("gpa")) %>% #input level and entry or final
    filter_at( c(8), all_vars(.=="Accounting")) %>% 
    mutate_at(c(8:11), tidyr::replace_na, "Stopout") 

colnames(test_flow_filter_temp)[c(8:11,2,4,5,24,25)] <-  c("VAR1","VAR2","VAR3","VAR4","GEN","GRD","DEG","GPA","UWF") #GPA ist term gpa, UWF year1 gpa

addmargins(table(new_sankeydata$Stu_Department, new_sankeydata$Deg_Department))
sort(unique(new_sankeydata$Deg_ProgramCIPDesc)) #56degree program/70 entryprogram
m1 <- new_sankeydata %>% group_by(Stu_UWFID ) %>% 
    select(Stu_UWFID, FALL_UWF,College, "Gender"=Stu_Gender,Cohort,
           GPA_HIGHSCHOOL,Gradu_Time,Deg_College,Deg_Department, Deg_ProgramCIPDesc, TermGPA,EnteringFallGPA , APR ) %>% 
    tidyr::pivot_wider( names_from = FALL_UWF , values_from = c(College,TermGPA,EnteringFallGPA    ) ) %>%  
    mutate(Fall1GPA=ifelse(TermGPA_UWFFALL1 >= 2.77, "Above2.77 ","Below2.77 ")) %>% 
    mutate(Year1GPA= ifelse(EnteringFallGPA_UWFFALL2 >= 3.00, "Above3.00 ","Below3.00 ")) %>% 
    mutate_at(c(9:12), replace_na, "Stopout")  
addmargins(table(m1$Cohort, m1$College_UWFFALL1)) 
glimpse(m1)
colnames(m1)[c(8,9,10,11,2,4,5,24,25)]<-  c( "VAR1","VAR2","VAR3","VAR4","GEN","GRD","DEG","GPA","UWF")
 
move2 <- m1 %>% filter(Cohort == 2017)  %>% filter(DEG=="College of Sci and Engineering" )   



test_flow_filter <- move2 %>% 
    mutate(   
        VAR1 =  paste0(VAR1,  "_Y1", sep=""),
        GPA  =  paste0(GPA,  "Tgp", sep=""),
        GEN  =  paste0(GEN,  "GEN", sep=""),
        UWF  =  paste0(UWF,  "Fgp", sep=""),
        VAR2 =  paste0(VAR2 ,  "_Y2", sep=""),
        VAR3 =  paste0(VAR3 ,  "_Y3", sep=""),
        VAR4 =  paste0(VAR4 ,  "_Y4", sep=""),
        DEG  =  paste0(DEG ,  "DEG", sep=""),
        GRD  =  paste0(GRD, "GRD",sep=""))

col1 <- test_flow_filter %>%  
     group_by(VAR1, GPA )  %>% 
    dplyr::summarise(n =n(), .groups ="drop") %>% ungroup() %>% arrange(-n) %>% 
      select( In= 1, Out= 2, 3)

col2 <- test_flow_filter %>%  
    group_by(GPA, GEN ) %>% 
    dplyr::summarise(n =n(), .groups ="drop") %>% ungroup() %>% 
    select( In= 1, Out= 2, 3)

col3 <- test_flow_filter %>%  
    group_by(GEN, VAR2) %>% 
    dplyr::summarise(n =n(), .groups ="drop") %>% ungroup() %>% na.omit() %>% 
    select( In= 1, Out= 2, 3)

col4 <- test_flow_filter %>%  
    group_by(VAR2, VAR3) %>% 
    dplyr::summarise(n =n(), .groups ="drop") %>% ungroup() %>% 
    select( In= 1, Out= 2, 3)

# create flow APRto2
col5 <- test_flow_filter %>% 
    group_by(VAR3,VAR4)  %>%  
    dplyr::summarise(n =n(), .groups ="drop") %>% ungroup() %>% 
    select( In= 1, Out= 2, 3)

#create flow2to3
col6 <- test_flow_filter %>% 
     group_by(VAR4, DEG) %>%
    dplyr::summarise(n =n(), .groups ="drop") %>% ungroup() %>%
     select( In= 1, Out= 2, 3)
 
col7 <- test_flow_filter %>%
    group_by(DEG ,GRD) %>% 
        dplyr::summarise(n =n(), .groups ="drop") %>% ungroup() %>% 
         select( In= 1, Out= 2, 3)

#create data frame 
flow_data01 <- rbind(col1, col2, col3, col4, col5, col6,col7 ) 

my_college_color <- 'd3.scaleOrdinal(["Col-of-Arts,-Soc-Sci-and-Human", "College-of-Business", "College-of-Health", 
"College-of-Sci-and-Engineering", "No College Designated", "Grad4yrs", "Grad5yrs", "NoDegree","College-of-Ed-and-Prof-Studies","Stopout"]) 
.range(["tomato","yellowgreen","turquoise","palevioletred", "gray","lime","gold","orchid","royalblue","skyblue","lightblue","red"])'
#["#009CDE","#8DC8E8","#40A829","#97C800","#B6ADA5","#00838f","#0097a7","#00acc1","#00bcd4"]


    # create nodes 
    nodes_FD <- flow_data01 %>%  select(In, Out) %>% 
        pivot_longer(c("In","Out"), names_to = "col_name",
                     values_to = "name_match") %>% select(-1) %>% distinct() %>% 
        mutate(name = str_sub( name_match, end=-4)) %>% as.data.frame()
    nodes_FD$group <-  as.factor( gsub(" ", "-",nodes_FD$name))
    
    colors <- paste(sapply(nodes_FD$nodes$colors, function(x) { paste0("d3.rgb(", paste(c(col2rgb(x), 0.5), collapse = "," ), ")") }), collapse = ", ")
    colorJS <- paste0('d3.scaleOrdinal([', colors, '])')
    #create links
    plot_id_FD <- flow_data01 %>% 
        mutate( IDIn = match( In, nodes_FD$name_match)-1,
                IDOut = match(Out, nodes_FD$name_match)-1,
                Freq =n ) %>% as.data.frame()
    plot_id_FD <- sub(' .*', '', nodes_FD[energy$links$source + 1, 'name'])
    

    #sankey chart
    upsankey <-  sankeyNetwork(
        Links = plot_id_FD, Nodes = nodes_FD,
        Source = "IDIn",
        Target = "IDOut",
        Value = "Freq",
        NodeID = "name",
        colourScale = my_college_color, 
        NodeGroup = "group",#LinkGroup = "group",
        sinksRight = F, iterations = 0,
        fontSize =10, fontFamily = "Arial",
        nodeWidth = 30, nodePadding = 5)
    
    htmlwidgets::onRender(upsankey, 'function(el) { 
    var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i);
    var labels = ["Entry","1st Fall GPA", "Gender","2nd Fall","3rd Fall","4th Fall","Degree","Graduation Time"];
    cols_x.forEach((d, i) => {
      d3.select(el).select("svg")
        .append("text")
        .attr("x", d)
        .attr("y", 15)
        .text(labels[i]);})}')

     









drop=c("Dropped","WithdrawnFTIC")
test1 <- sankey_data %>% 
    mutate(Gradu_Time = ifelse(Gradu_Time == "Grad4yrs","Yes","No")) %>% 
    mutate(ENTRY_PROGRAM = UWFFall1CIPTitle, GRADUATIONin4YRS = Gradu_Time) %>% 
    mutate(RETENTION = ifelse(UWFFall2CIPTitle %in% drop, "No","Yes")) %>%
    mutate(ENTRY_PROGRAM = UWFFall1CIPTitle, GRADUATIONin4YRS = Gradu_Time) %>%
    group_by( Cohort, ENTRY_PROGRAM, GRADUATIONin4YRS) %>% #input$barchartmetric) %>%
    dplyr::count() %>%
    tidyr::pivot_wider(names_from = GRADUATIONin4YRS, values_from = n) %>%
    mutate(APRCount  = Yes) %>%
    mutate(CohortSize = sum(No, Yes)) %>%
    mutate_at(vars(3:4), ~(round(./CohortSize*100,2))) %>%
    mutate( APR= paste(Yes, "%", sep=" ")) %>% select(Cohort, APRCount,CohortSize,APR) 

allnames=c("HMCSE","CEPS")
test2 <- sankey_data %>% filter(ENTRY_COLLEGE %in% c("COB", allnames)) %>% 
    filter( ((GPA_HIGHSCHOOL >= 3.5) & 
                 (GPA_HIGHSCHOOL <= 5.0) )) 

 


filter_college <- if(input$metriccollege == "All"){
    sankey_data
}else {
    sankey_data %>% filter(ENTRY_COLLEGE == "HMCSE")
} 

sankey_data %>% 
    mutate(RETENTION = ifelse(UWFFall2CIPTitle %in% drop , "No","Yes")) %>%
    group_by( ENTRY_COLLEGE, Cohort,Gradu_Time) %>% count() %>% 
    tidyr::pivot_wider( names_from = Gradu_Time, values_from = n ) %>% 
    replace(is.na(.), 0) %>% 
    mutate(CohortSize = sum(Grad4yrs , Grad5yrs ,Grad6yrs ,NoDegree)) %>% 
     mutate_at(vars(3:6), ~(round(./CohortSize*100,2))) %>%
    mutate(  APRPercent= paste(Yes, "%", sep=" "))


rent_college <-  sankey_data  %>% 
    mutate(RETENTION = ifelse(UWFFall2CIPTitle %in% drop , "No","Yes")) %>%
     group_by( ENTRY_COLLEGE, RETENTION) %>% count() %>% 
    tidyr::pivot_wider( names_from = RETENTION, values_from = n ) %>%
    replace(is.na(.), 0) %>% mutate(RETENTION = Yes) %>% 
    select(ENTRY_COLLEGE, "RETENCount" = Yes, RETENTION,-No)

rapr_college <- sankey_data %>% 
    group_by( ENTRY_COLLEGE, APR) %>% count() %>% 
    tidyr::pivot_wider( names_from = APR, values_from = n ) %>% 
    replace(is.na(.), 0) %>%   mutate(APR = Yes ) %>% 
    select(ENTRY_COLLEGE, "APRCount" = Yes,APR,-No)

apr_rent <- merge(rent_college,rapr_college, by=c("ENTRY_COLLEGE" ), all.x = T)

gradu_college <-  sankey_data %>% 
    group_by( ENTRY_COLLEGE, Gradu_Time) %>% count() %>% 
    tidyr::pivot_wider( names_from = Gradu_Time, values_from = n ) %>% 
    replace(is.na(.), 0) %>% 
    mutate(CohortSize = sum(Grad4yrs , Grad5yrs ,Grad6yrs ,NoDegree)) 

all_df <- merge(apr_rent, gradu_college,  by=c("ENTRY_COLLEGE" ), all.x = T)

all_df %>% mutate(Grad5yrs = (Grad4yrs  + Grad5yrs) , Grad6yrs  = (Grad5yrs + Grad6yrs)) %>% 
    mutate_at(vars(3,5), ~(round(./CohortSize*100,2))) %>%
    mutate_at(vars(3,5),~paste(. , "%", sep=" ")) 
 
all_short1 <- colnames(all_short) <- paste(colnames(all_short), "Count", sep="_")                    

all_df2 <- cbind(all_df , all_short) %>% 
    mutate_at(vars(3,5), ~(round(./CohortSize*100,2))) %>%
    mutate_at(vars(3,5),~paste(. , "%", sep=" ")) 

gradu_count <-  all_df %>% select(6:9) %>% data.frame() %>% mutate(Grad4yrsCount = Grad4yrs, Grad5yrsCount = Grad5yrs,
                                                                 Grad6yrsCount =Grad6yrs, NoDegreeCount =NoDegree )
colnames(gradu_count) <- paste(gradu_count, "%", sep="")
gradu_count %>% 
mutate(across(c(4:5), ~round(.x/NoDegreeCount*100,2), .names = "{col}%"))

# courses output

  
# pass
  
tesmpdf <- crs_grade_FTIC1521[,-1]  %>% filter(Cohort == 2015 & ENTRY_PROGRAM =="Reg Nursing/Reg Nurse") %>% 
    count(CRSE_NAME,CRS_PASS ) %>%  
    tidyr::pivot_wider(names_from = CRS_PASS, values_from = n) %>% 
    replace(is.na(.), 0)  
    tesmpdf$CRSCount <-  rowSums(tesmpdf[,2:3])
    
 tesmpdf1 <-  tesmpdf  %>% data.frame() %>%  arrange(-CRSCount) %>% slice(1:30000) %>% 
                 mutate_at(vars(2:3 ), list(Prop =  ~ ./CRSCount*100 )) %>% 
                 mutate(across(where(is.numeric), round, 2))  
 
 crs_y1 <- paste(tesmpdf1$Passed_Prop, "%", sep= " ")
 
    #  mutate(across(c("Failed","Passed"), ~round(.x/CRSCount*100,2),  .names = "{col}_Prop")) %>%    
    # mutate(across(c("Failed","Passed"), ~paste(round(.x/CRSCount*100,2), "%", sep=" "), .names = "{col}_"))   


fig1 <- plotly::plot_ly(tesmpdf1, x = ~reorder(CRSE_NAME,-CRSCount), y= ~Passed ,name=  "# of Passed"  , 
                        text =~Passed,
                        type="bar", marker = list(color="#669900"))   
    fig1 <- fig1 %>%  
    add_trace( y = ~Failed  ,  marker = list(color="#FF3399"), name ="# of Failed",
               text = ~Failed, 
               textposition ="auto")
    
    
 # apr
    crsapr <- crs_grade_FTIC1521 %>% filter( CRS_PREFIX == "MAC" | CRS_PREFIX == "MAT" |CRS_PREFIX == "STA" ) %>% 
        filter(CRSE_NAME == "MAC1147") %>% 
         group_by( Cohort, CRS_PASS, APR) %>% 
        count() %>% 
        tidyr::pivot_wider( names_from = c(  "CRS_PASS","APR"), values_from = n ) %>% 
        replace(is.na(.), 0)  
    crsapr$CRSCount <-  rowSums(crsapr[,2:5])
    crsapr2 <-  crsapr %>%  
        mutate_at(vars(2:5 ), list(Prop =  ~ ./CRSCount*100 )) %>%  
      mutate(across(where(is.numeric), round, 2))  
    
    fig2 <- plotly::plot_ly(crsapr2, x = ~ factor(Cohort) , y= ~Passed_Yes  ,name=  "# of Passed & APR"  , 
                            text = ~Passed_Yes_Prop,
                            type="bar", marker = list(color="#1976d2")) 
    fig2 <- fig2 %>%  
        add_trace( y = ~Passed_No  ,  marker = list(color="#eb984e"), name ="# of Passed & NonAPR",
                   text = ~Passed_No_Prop , 
                   textposition ="auto")
     fig2 <- fig2 %>%  
        add_trace( y = ~Failed_No  ,  marker = list(color="#c2185b"), name ="# of Failed & NonAPR",
                   text = ~Failed_No_Prop , 
                   textposition ="auto")
    fig2 <- fig2 %>%  
        add_trace( y = ~Failed_Yes  ,  marker = list(color="#fdd835"), name ="# of Failed & APR",
                   text = ~Failed_Yes_Prop , 
                   textposition ="auto") %>% 
        add_annotations(
            x = 1,
            y = 50, #aprcrs_DF1$CRSCount[which.min(aprcrs_DF1$CRSCount)],
            text = "Counts only the most recent attempt", showarrow =F
        )

    
    
    




 addmargins(table(crs_grade_FTIC1521$crs_DEMO_TIME, crs_grade_FTIC1521$Cohort))    
 
 # stop here
 
 crs_grade_FTIC1521 <- readRDS("DB_crs_grade_FTIC1521V0.rds")

crsapr <- crs_grade_FTIC1521 %>% group_by(CRSE_NAME, Cohort,crs_DEMO_TIME , CRS_PASS,APR) %>% 
    filter(!is.na(APR)) %>% 
    count() %>% 
    tidyr::pivot_wider( names_from = c("CRS_PASS","APR" ), values_from = n ) %>% 
    replace(is.na(.), 0) %>%  
    mutate(APRCount =rowSums(across(where(is.numeric)))) %>% 
    mutate(across(c("Yes","No"), ~round(.x/APRCount*100,2), .names = "{col}%"))  

crs_apr_pass <- merge(crs,crsapr, by= c("Cohort", "CRSE_NAME") , all.x = T)

#logic for select all
rows_test <- crsapr %>%  dplyr::summarise(Log=(Cohort == "all" |  Cohort == 2015), .groups="drop")
apr_table_test <-  crsapr[rows_test$Log , , drop= FALSE]  



colSums(is.na(crs))    

# plot for apr and courses grade
colnames(crs_grade_FTIC1521)
crs_grade_FTIC1521 %>% filter(CRSE_NAME =="MAC1147") %>% filter(ENTRY_PROGRAM == "Cybersecurity") %>% 
    filter(Cohort == 2020) %>% filter(crs_DEMO_TIME == 202008) %>% 
    group_by( CRS_PASS, APR) %>% 
    count( ) %>% 
    plot_ly(x = ~GRADE_AWARDED, y =~n, color = ~APR, type="bar" , colors = "Set2") #c("red","green"))

test <- sankey_data %>% 
    select(Cohort,  APR) %>% 
    group_by( Cohort,  APR) %>%  
    count() %>% arrange(Cohort) %>% 
    tidyr::pivot_wider( names_from = APR, values_from = n )

notapr= c("Dropped","WithdrawnFTIC")
rent_college <-  sankey_data %>%   
    mutate(RETENTION = ifelse(UWFFall2CIPTitle %in% notapr , "No","Yes")) %>%
    group_by( Cohort, RETENTION) %>% count() %>% 
    tidyr::pivot_wider( names_from = RETENTION, values_from = n ) %>%
    replace(is.na(.), 0) %>% mutate(RETENTION=Yes)  %>% 
    select (-Yes,-No)

rapr_college <-  sankey_data %>% 
    group_by( Cohort, APR) %>% count() %>% 
    tidyr::pivot_wider( names_from = APR, values_from = n ) %>% 
    replace(is.na(.), 0) %>% mutate(APR = Yes) %>% 
    select(-Yes,-No) 

apr_rent <- merge(rent_college,rapr_college, by=c("Cohort" ), all.x = T)

gradu_college <-  sankey_data %>% 
    group_by( Cohort, Gradu_Time) %>% count() %>% 
    tidyr::pivot_wider( names_from = Gradu_Time, values_from = n ) %>% 
    replace(is.na(.), 0) %>% rowwise() %>% 
    mutate(CohortSize = sum(Grad4yrs , Grad5yrs ,Grad6yrs ,NoDegree)) 

all_df <- merge(apr_rent, gradu_college,  by=c("Cohort" ), all.x = T)

gradu <-  all_df %>%  mutate(Grad5yrs = (Grad4yrs + Grad5yrs) , Grad6yrs  = (Grad5yrs + Grad6yrs))
gradu_count <- gradu %>% select(6:9) %>% 
    mutate(Grad4yrsCount = Grad4yrs, Grad5yrsCount = Grad5yrs,Grad6yrsCount =Grad6yrs, NoDegreeCount =NoDegree ) %>% 
    select(-1,-2,-3,-4) #remove original columns

#final table
final_metric_df <- gradu %>%  rowwise() %>% 
    mutate(across(c(2:7), ~round(.x/CohortSize*100,2), .names = "{col}%"))  #%>% 
#mutate_at(vars(3,5:9), ~paste(. , "%", sep=" ")) #%>% 
#mutate(across(c(6:10), ~round(.x/CohortSize*100,2), .names = "{col}%"))

datatable(final_metric_df[,c(1,2,4,10:14,3,5,6:9)], caption = "TEST") 

sankey_data
m_rent_cip <-  sankey_data %>%  
    filter(ENTRY_COLLEGE == "CASSH") %>%
    filter(Cohort == 2017 ) %>%
    mutate(RETENTION = ifelse(UWFFall2CIPTitle %in% notapr , "No","Yes")) %>%
    mutate(GRADUATIONin4YRS = ifelse(Gradu_Time == "Grad4yrs","Yes","No")) %>% 
    select("ENTRY_PROGRAM"=UWFFall1CIPTitle,  "Metrics"= APR) %>% 
    group_by( ENTRY_PROGRAM,  Metrics) %>%  
    dplyr::summarise(Count =n(), .groups = "drop") %>% # count() %>%  #group_by( Cohort,  Metrics) %>% #ENTRY_DEPARTMENT,
    tidyr::pivot_wider(names_from = Metrics, values_from = Count, values_fill = 0) %>% 
    #arrange(ENTRY_PROGRAM) %>% 
    mutate(MetricCount = Yes) %>% rowwise() %>% 
    mutate(CohortSize = sum(No, Yes)) %>%
    mutate_at(vars(2:3), ~(round(./CohortSize*100,2))) %>%
    mutate( MetricPercent = paste(Yes, "%", sep=" ")) %>% 
    select(ENTRY_PROGRAM, MetricCount,CohortSize,MetricPercent,Yes)
#help#
y=m_rent_cip$CohortSize
y1mp <- m_rent_cip$MetricCount
m.ypp <- m_rent_cip$MetricPercent
y1mpp <- m_rent_cip$Yes 
per_y1 <-  list( tickfont = list(color = "red"), overlaying = "y1mp", side="right", title = "<b> % of performance")

#bar plot
bar_cip <-  plotly::plot_ly(m_rent_cip, 
                            x= ~ reorder(ENTRY_PROGRAM, CohortSize),
                            y = ~y, type = "bar",
                            name = "Cohort Size", text= ~CohortSize,
                            textposition="auto",
                            marker = list(color = "#007A33" ) )

bar_cip %>%  
    add_trace( y = ~MetricCount, text = y1mp, name = "APR" , marker = list(color = "#004C97")) %>%
    add_trace( y = ~y1mpp, text=m.ypp,  name = "Performance (%)", marker = list( color = "#009CDE")) %>% #, yaxis = "y2", mode = "line+markers", type ="bar"
    layout(title = "",
           xaxis = list( title="", tickfont = list(size = 12, color = "darkgreen")),
           yaxis = list( title= "APR", "(Count)", sep=" ",
                         titlefont = list(size = 12, color="red"), 
                         tickfont = list(size= 12, color="blue")),
           yaxis2 = per_y1 ,
           legend = list(x = 1, y= 1, bgcolor = 'rgba(255, 255, 255, 0)', bordercolor = 'rgba(255, 255, 255, 0)'),
           barmode = "group", bargap =0.15) %>% 
    config(displaylogo = FALSE)

### prerequsites
unique(course_data$GRADE_AWARDED)
course_data$orderGRADE_AWARDED <- ordered(course_data$GRADE_AWARDED, levels=c("A","A-","B+","B","B-","C+","C","C-","D+","D","F","NF","U","W")) 
glimpse(course_data)
MAC1147 <- course_data %>% group_by(UWFID, CRSE_NAME) %>% filter(crs_DEMO_TIME == min(crs_DEMO_TIME)) %>% # first attempt only
    select(UWFID, CRSE_NAME, GRADE_AWARDED,CRS_PASS)  %>%  
    filter(CRSE_NAME == "MAC1105" ) %>% filter(!duplicated(UWFID)) %>%  #3248
    rename(UWFID=1, ENC1101=2,ENC1101Grades=3,ENC1101PASS=4) 

MAC2311 <- course_data %>% group_by(UWFID, CRSE_NAME) %>% filter(crs_DEMO_TIME == min(crs_DEMO_TIME)) %>% 
    select(UWFID, CRSE_NAME, GRADE_AWARDED,CRS_PASS)  %>%  
    filter(CRSE_NAME == "MAC2311" ) %>% filter(!duplicated(UWFID)) %>%  #3247
    rename(UWFID=1, ENC1102=2,ENC1102Grades=3,ENC1102PASS=4)

A_range <- c("A","A-")
B_range <- c("B","B-","B+")
C_range <- c("C","C+","C-")
DWF <- c("D","D+","W","F","NF")
ENC10111012 <- merge(MAC1147,  MAC2311, by="UWFID", all.y = T) %>% 
    replace(is.na(.), "W/OPrerequisite") %>% filter(ENC1101Grades != "W/OPrerequisite") %>%
    mutate(ENC1102ID= ifelse(ENC1102PASS == "Passed", 1, 0)) %>% 
    mutate(ENC1101Grades4 = ifelse(ENC1101Grades %in% A_range,  "A Range",
                                   ifelse(ENC1101Grades %in% B_range,  "B Range",
                                          ifelse(ENC1101Grades %in% C_range, "C Range", "DWF"))))  
    #mutate(ENC1101Grades = ordered( ENC1101Grades, levels=c("A","A-","B+","B","B-","C+","C","C-","D+","D","F","NF","U","W")))

glimpse(ENC10111012)
 fig_prerequisite <-  ENC10111012 %>%  group_by(ENC1101Grades4 , ENC1102PASS) %>% count() %>% 
     tidyr::pivot_wider(names_from = ENC1102PASS, values_from = n) %>% mutate(SUM = sum(Passed, Failed)) %>% 
     mutate(ytext = paste("Chance to Pass ENC1102:", round(Passed/SUM*100,2),  '%', sep=" "), y2text= paste(round(Failed/SUM*100,2),  '%', sep=" ")) %>% 
 plot_ly(  x= ~ENC1101Grades4, y= ~Passed, text= ~ytext, name="Passed", type="bar")  %>% #, colors =c("orange","green")
       layout(xaxis = list(title = "Awarded Grades in ENC1011"),
              yaxis = list(title = "Enrolled (Count)"),
           legend=(list(title=list(text='<b> ENC1102 </b>')))) %>% 
     add_trace( y = ~Failed, text =~y2text, name ="Failed") 
       
 fig_prerequisite 
  
tab1  <- round(100*prop.table(table(ENC10111012$ENC1101Grades4, ENC10111012$ENC1102PASS), 1), digits = 2)  %>% 
    as.matrix() 
V1 <- rep("IF you got an")
Grade <- rownames(tab1)
Chance <- rep("Change to pass")
precrs <- rep("ENC1101")
entercrs <- rep("ENC1102")

ytext <- data.frame(CRSAwarded,Grade, precrs, Chance, paste(entercrs,tab1[,2], "%", sep=" ")  )  
knitr::kable(ytext)


library(readr)
select_crs_vari202108 <- read_csv("select_crs_vari202108V0.csv") 

crs_df_test <- course_data %>%  
    filter(ENTRY_PROGRAM == "Cybersecurity") %>% 
    filter(Cohort == "2020")  %>% 
    filter((GPA_HIGHSCHOOL >= 0.00) & (GPA_HIGHSCHOOL <= 5.04)) %>% 
    filter(CRSE_NAME == "MAC1147") %>% 
    mutate(crsall = ( crs_DEMO_TIME <= 202101 | crs_DEMO_TIME == 202008) )
crs_test <-  crs_df_test[crs_df_test$crsall,,drop= FALSE]

crs_test %>% group_by(CRSE_NAME, GRADE_AWARDED, APR) %>% count()
