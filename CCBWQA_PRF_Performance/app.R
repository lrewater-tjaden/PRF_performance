#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load packages
library(shiny)
library(shinythemes)
library(readxl)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(rsconnect)


###### Deploy App ######


######### Load data #########

    #import paired data
    CCBWQA_PRF_Paired <- read_excel("../CCBWQA_PRF_TPandTSS_20190813.xlsx", 
                                sheet = "Data by Group and Net & % Diff", 
                                col_types = c("numeric", "text", "date", 
                                              "numeric", "text", "numeric", "text", 
                                              "text", "numeric", "text", "text", 
                                              "numeric", "text", "text", "numeric", 
                                              "numeric", "text", "text", "numeric", 
                                              "numeric", "numeric", "numeric"))


    #import unpaired data
    CCBWQA_PRF_TPandTSS_unpaired <- read_excel("../CCBWQA_PRF_TPandTSS_20190813.xlsx", 
                                           sheet = "All Stacked Unique", col_types = c("date", 
                                                                                       "numeric", "text", "text", "numeric", 
                                                                                       "text", "text", "numeric", "numeric", 
                                                                                       "text", "numeric", "text", "numeric"))

######## Simplify Data ########
    
    paired <- CCBWQA_PRF_Paired %>% 
        select(group_name, rdate, flow_conditions, param_abbrev, up_location_name, up_rvalue_converted, down_location_name, down_rvalue_converted, net_change, seasonid) %>% 
        mutate(date = ymd(rdate)) %>% 
        mutate_at(vars(date), funs(year, month, day))
    
    
    
    unpaired <- CCBWQA_PRF_TPandTSS_unpaired %>% 
        select(rdate, param_abbrev, location_name, rvalue_converted, unit_name, flow_conditions, seasonid) %>% 
        mutate(date = ymd(rdate)) %>% 
        mutate_at(vars(date), funs(year, month, day))


######### User interface ########
ui <- fluidPage(

    
    # Update theme:
    theme = shinytheme("slate"),
    
    
    # App title
    titlePanel("Cherry Creek Water Quality Authority: Pollution Reduction Facility Data"),

   
    # Navigatioon bar
    navbarPage("",
               
               
               
    
######### Introduction Tab ##########
    
        tabPanel("Introduction",
            p("The Cherry Creek Basin Water Quality Authority has implemented two Pollution Reduction Facilities (PRFs)
              in the Cottonwood Creek Subwatershed to reduce pollution entering Cherry Creek Reservoir. The two facilites are the Peoria Pond and the Perimeter Wetland System.
              Water quality has been monitored above and below each site before and after restoration. A map of the subwatershed and monitoring points is displayed below."),
            
            #url link
            uiOutput("tab"),
            
            p(""),
            
            p("This app will allow the user to explore water quality at each monitoring sites and to evaluate the effectiveness of the PRFs over time. 
              The 'Explore Data Tab' allows users to compare boxplots of monitoring sites for the entire period of record or at specified time ranges.
              The 'Permmiter Wetlands Tab' takes a deeper look at the Perimeter Wetland System at the various phases of restoration. 
              The 'Statistical Analyses Tab' explains the statistical methods that can be used to evaluate the effectiveness of PRF facilities.  ")
    
           
            
            ), #close intro tab
    

######### Explore Data Tab #########

        tabPanel("Explore Data",
            p("Insert text about data"),
            
                
               #Sidebar with selections for sites
            sidebarLayout(
                sidebarPanel(
                    #sidebar header
                    h3("Select Inputs (Top Graph)"),
                    
                    
                    #input sites
                    selectInput("location_name1",
                                "Select Site 1",
                                choices = c("Above Peoria Pond" = "CT-P1", "Below Peoria Pond" = "CT-P2", "Above Perimeter Wetland" = "CT-1","Below Perimeter Wetland"= "CT-2")
                                ),
                    
                    selectInput("location_name2",
                                "Select Site 2",
                                choices = c("Above Peoria Pond" = "CT-P1", "Below Peoria Pond" = "CT-P2", "Above Perimeter Wetland" = "CT-1","Below Perimeter Wetland"= "CT-2")
                    ),
                    
                    #input parameter
                    radioButtons("param_abbrev",
                                 "Select Parameter",
                                 choices = c("Total Phosphorus" = "TP", "Total Suspended Solids" = "TSS")
                                 ),
                    
                    h3("Select Inputs (Bottom Graph)"),
                    
                    #input parameter
                    radioButtons("param_abbrev2",
                                 "Select Parameter",
                                 choices = c("Total Phosphorus" = "TP", "Total Suspended Solids" = "TSS")
                    ),
                    
                    #input start year
                    sliderInput("start_year",
                                "Start Year",
                                1992,
                                2019, 
                                1992,
                                step = 1
                                ),
                    
                    #input end year
                    sliderInput("end_year",
                                "End Year",
                                1992,
                                2019, 
                                2019,
                                step = 1
                    )
                    
                ),  #close sidebar panel
            
            #outputs
            
            mainPanel(
                fluidRow(
                    
                        plotOutput("boxplot1"),
                        plotOutput("boxplot2")
                         
                         
                         
                         ) #close fluid row
                

                
                
                ) #close main panel
            
            
            ) #close sidebar layout
        ), #close explore data tab






########## Perimeter Wetland Tab ########

tabPanel("Perimeter Wetland",
         p("The perimeter wetland system is made up of 2 monitoring points: above the wetland (CT-1) and below the wetland (CT-2)."),
         p("The difference (D) between the points is calculated as D = [Below] - [Above]. A negative difference is considered a success (the concetration below the wetland is 
           less than the concentration above the wetland). "),
         p("Restoration was done in two phases. The first phase was completed in 2004 and the second phase was completed in 2008. The restoration phases are as follows: 
           1) Pre restoration (1992-2004),
           2) Phase 1 (2004-2008), and
           3) Phase 2 (2008-2019).\n"),
         
         #Sidebar with selections for sites
         sidebarLayout(
             sidebarPanel(
                 #sidebar header

                 #input parameter
                 radioButtons("param_abbrev3",
                              "Select Parameter",
                              choices = c("Total Phosphorus" = "TP", "Total Suspended Solids" = "TSS")
                 )

                 
             ),  #close sidebar panel
             
             #outputs
             
             mainPanel(
                 fluidRow(plotOutput("boxplot_differences")
                    
                     
                     
                     
                 ) #close fluid row
                 
                 
                 
                 
             ) #close main panel
             
             
         ) #close sidebar layout  
         
         
         
         
         
), #close perimeter tab



########## Statistical Analyses Tab ########
tabPanel("Statisical Analyses",
         
        h3("Overview"),
        p("The objective of statistical testing is to determine if the differences between the monitoring points below and above the wetlands
        are significantly different. This will indicate if the PRF has effectively reduced pollution."),
        
        p(" Monitoring data from the Wetland Perimeter System is paired. To determine if a parametric or non-parametric test should be used the normality of the 
          differences was investigated by plotting the differences on a histogram. The differences were found to be normally distributed."),
        
        p(""),
        
        
         #Stats Testing for Perimeter Wetland System 
        h3("Normality of Differences"),
         p("Plot the distribution of the differences to determine if the differences data is parametric or non-parametric. 
           Based on the histogram the difference between the paired upstream and downstream monitoring sites are normally distributed."),
        
       #image of histogram
       
       
       h3("Wilcoxon Signed Rank Test"),
       h4("Method:"),
       p("The Wilcoxon signed rank test is used to determine whether the ranks/median difference between paired observations equals zero."),
       p("D = x-y, where x = below and y = above"),
       p("Null hyp: median[D] = 0"),
       p("Alt hyp: median[D] < 0 (1 sided test, x is expected to be smaller than y)"),
      
        #image of R code
       
       h4("Results:"),
       p("The p-value =  1.456e-09. The p-value is < 0.05 therefore we accept the alternative hypothesis: median [D] <0 meaning the concentrations below the wetland are significantly lower than above the wetlands.")




         
         
         
         
         
         
) #close stats tab



####### Close it out #######



) #close navbar
) #close ui







# Define server logic required to draw a histogram
server <- function(input, output) {
    
    

    
    output$tab <- renderUI ({
        
        url <-a("Monitoring Sites Map", href = "http://ccbwqportal.org/sites/default/files/pictures/annual_report_banners/2018/CottonwoodCreekSubWateshed.jpg")
        
        tagList("URL link:", url)
       
        
    })
    
    
    
 
    output$boxplot1 <- renderPlot({
        
        #plot boxplots by season by site facet wrap
        season_labels <- c('10' = "Mar - Sept", '11' = "Oct - Feb")
        
        #filter data based on parameter and site inputs
        unpaired_filtered <- unpaired %>% 
            filter(location_name== input$location_name1 | location_name == input$location_name2) %>% 
            filter(param_abbrev == input$param_abbrev)
        
        
        ggplot(unpaired_filtered, aes(y= rvalue_converted, fill = location_name))+
            geom_boxplot()+
            scale_x_discrete(name = "")+
            scale_y_continuous(name = "")+
            ggtitle("Boxplots of All Data (1992-2019) for Selected Sites and WQ Parameter")+
            scale_fill_brewer(palette = "Accent") +
            theme_bw()+
            theme(legend.position = "bottom",
                  axis.title = element_text(face = "bold"),
                  legend.title = element_text(face = "bold"))+
            labs(fill = "Site")+
            facet_grid(~seasonid, scales = "free", labeller = labeller(seasonid = season_labels))

        
    })
    
    
    
    output$boxplot2 <- renderPlot({
        
        #filter data based on parameter and site inputs
        unpaired_filtered2 <- unpaired %>% 
            filter(param_abbrev == input$param_abbrev2) %>% 
            filter(between(year, input$start_year, input$end_year))
    
        
        ggplot(unpaired_filtered2, aes(x = factor(year), y= rvalue_converted, fill = location_name))+
            geom_boxplot()+
            scale_x_discrete(name = "")+
            scale_y_continuous(name = "")+
            ggtitle("Yearly Boxplots for Selected WQ Parmaeter")+
            scale_fill_brewer(palette = "Accent") +
            theme_bw()+
            theme(legend.position = "bottom",
                  axis.title = element_text(face = "bold"),
                  legend.title = element_text(face = "bold"))+
            labs(fill = "Site")
        
    })
    

    
    
    
    output$boxplot_differences <- renderPlot({
        
        perimeter_phased <- paired %>%   
            filter(group_name == "Perimeter System Wetland PRF")
        
        perimeter_phased$restoration_phase <- ifelse(perimeter_phased$year < 2004, "pre",
                                                     ifelse(perimeter_phased$year>=2004 & perimeter_phased$year<2008, "phase1",
                                                            ifelse(perimeter_phased$year>=2008,"phase2", 0
                                                            )))
        
        perimeter_phased$restoration_phase <- factor(perimeter_phased$restoration_phase, levels = c("pre", "phase1", "phase2"))
        
        
        flow_labels <- c('2' = "Baseflow", '1' = "Storm flow") 
        
        
        perimeter_filtered <- perimeter_phased %>% 
            filter(param_abbrev == input$param_abbrev3) 
          
        
        ggplot(perimeter_filtered, aes(y= net_change, fill = restoration_phase))+
            geom_boxplot()+
            scale_x_discrete(name = "Difference")+
            scale_y_continuous(name = "")+
            ggtitle("Difference (below-above) in Water Quality\nPerimeter System Wetland PRF")+
            scale_fill_brewer(palette = "Accent") +
            theme_bw()+
            theme(legend.position = "bottom",
                  axis.title = element_text(face = "bold"),
                  legend.title = element_text(face = "bold"),
                  axis.text.x = element_text(angle = 270))+
            labs(fill = "Restoration Phase:")+
            facet_grid(~flow_conditions, labeller = labeller(flow_conditions = flow_labels))
        
    })
    
    
    
} #close server

# Run the application 
shinyApp(ui = ui, server = server)
