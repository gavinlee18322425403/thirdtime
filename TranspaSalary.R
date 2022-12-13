#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

##--load packages----------------------------------------------------------------------------####
#-----------------------------------------------------------------------------------------------#
library(shiny)
library(dplyr)
library(sjmisc)
library(ggplot2)
library(ggthemes)
library(leaflet)


##--define functions-------------------------------------------------------------------------####
#-----------------------------------------------------------------------------------------------#

# define mean_basesalary_plot for base salary plot.
mean_basesalary_plot <- function(company_selected=NULL, title_selected=NULL){
  if(is.null(company_selected)|is.null(title_selected)){   # display an empty plot when either of company_selected or title_selecetd is null.
    ggplot()+
      geom_point(aes(0,0), alpha=0)+
      ggtitle("No Company and JobTitle selected")+
      xlab("experience of work [year]")+
      ylab("mean base salary [$]")+
      theme(plot.title = element_text(size=11, face="bold"),
            # panel.background = element_rect(fill="#a6cae3"),
            # axis.text.x = element_text(angle = 0,hjust=1),
            axis.title.x = element_text(size=10, face="bold"),
            axis.title.y = element_text(size=10, face="bold"),
            axis.text.x = element_text(size=9, face="bold"),
            axis.text.y = element_text(size=9, face="bold"))
    }
  else {
    tem <- select(lfsd.df, company, title, yearsofexperience, basesalary)  
    tem$yearsofexperience <- factor(round(tem$yearsofexperience))   # round yearsofexperience to integer and turn to factor.
    
    tem <- tem[which(tem$company %in% company_selected & tem$title==title_selected),]  # filter company and tilte selected and assign to tem.
   
    # calculate mean of base salary of each company and title, assian to d. 
    d <- aggregate(tem$basesalary, by=list(tem$yearsofexperience,tem$company), FUN=mean) 
    colnames(d) <- c("exper", "company", "meanbasesalary") # rename d.
    
    
    ggplot(data=d, aes(as.numeric(exper), meanbasesalary,color=company))+
      geom_point(alpha = 0.3)+ # plot all points with transparence = 0.3
      geom_smooth(aes(group=company),se=FALSE)+    # plot fitted line without standare error.
      ylab("mean base salary [$]")+
      xlab("experience of work [year]")+   # set x and y label.
      scale_x_continuous(breaks = seq(0,42,2))+   # set x axis interval.
      ggtitle("Mean base salary changes with working experience")+  # set plot title.
      theme(plot.title = element_text(size=11, face="bold"),        # modify text size and style.
            # panel.background = element_rect(fill="#a6cae3"),
            # axis.text.x = element_text(angle = 0,hjust=1),
            axis.title.x = element_text(size=10, face="bold"),
            axis.title.y = element_text(size=10, face="bold"),
            axis.text.x = element_text(size=9, face="bold"),
            axis.text.y = element_text(size=9, face="bold")
      )

  }
  
}

mean_bonus_plot <- function(company_selected=NULL, title_selected=NULL){
  if(is.null(company_selected)|is.null(title_selected)){    # display an empty plot when either of company_selected or title_selecetd is null.
    ggplot()+
      geom_point(aes(0,0), alpha=0)+
      ggtitle("No Company and JobTitle selected")+
      xlab("experience of work [year]")+
      ylab("mean bonus [$]")+
      theme(plot.title = element_text(size=11, face="bold"),
            # panel.background = element_rect(fill="#a6cae3"),
            # axis.text.x = element_text(angle = 0,hjust=1),
            axis.title.x = element_text(size=10, face="bold"),
            axis.title.y = element_text(size=10, face="bold"),
            axis.text.x = element_text(size=9, face="bold"),
            axis.text.y = element_text(size=9, face="bold"))
  }
  
  else {
    tem <- select(lfsd.df, company, title, yearsofexperience, bonus)
    tem$yearsofexperience <- factor(round(tem$yearsofexperience))   # round yearsofexperience to integer and turn to factor.
    
    tem <- tem[which(tem$company %in% company_selected & tem$title==title_selected),] # filter company and tilte selected and assign to tem.
    
    # calculate mean of base salary of each company and title, assian to d. 
    d <- aggregate(tem$bonus, by=list(tem$yearsofexperience,tem$company), FUN=mean)
    colnames(d) <- c("exper", "company", "meanbonus")  # rename d.
    
    ggplot(data=d, aes(as.numeric(exper), meanbonus,color=company))+
      geom_point(alpha = 0.3)+  # plot all points with transparence = 0.3
      geom_smooth(aes(group=company),se=FALSE)+   # plot fitted line without standare error.
      ylab("mean bonus [$]")+
      xlab("experience of work [year]")+  # set x and y label.
      scale_x_continuous(breaks = seq(0,42,2))+   # set x axis interval.
      ggtitle("Mean bonus changes with working experience")+  # set plot title.
      theme(plot.title = element_text(size=11, face="bold"),   # modify text size and style.
            # panel.background = element_rect(fill="#a6cae3"),
            # axis.text.x = element_text(angle = 0,hjust=1),
            axis.title.x = element_text(size=10, face="bold"),
            axis.title.y = element_text(size=10, face="bold"),
            axis.text.x = element_text(size=9, face="bold"),
            axis.text.y = element_text(size=9, face="bold")
      )
    
  }
  
    
}

mean_stock_plot <- function(company_selected=NULL, title_selected=NULL){
  if(is.null(company_selected)|is.null(title_selected)){  # display an empty plot when either of company_selected or title_selecetd is null.
    ggplot()+
      geom_point(aes(0,0), alpha=0)+
      ggtitle("No Company and JobTitle selected")+
      xlab("experience of work [year]")+
      ylab("mean stock grants [$]")+
      theme(plot.title = element_text(size=11, face="bold"),
            # panel.background = element_rect(fill="#a6cae3"),
            # axis.text.x = element_text(angle = 0,hjust=1),
            axis.title.x = element_text(size=10, face="bold"),
            axis.title.y = element_text(size=10, face="bold"),
            axis.text.x = element_text(size=9, face="bold"),
            axis.text.y = element_text(size=9, face="bold"))
  }
  
  else {
    tem <- select(lfsd.df, company, title, yearsofexperience, stockgrantvalue)
    tem$yearsofexperience <- factor(round(tem$yearsofexperience))  # round yearsofexperience to integer and turn to factor.
    
    tem <- tem[which(tem$company %in% company_selected & tem$title==title_selected),]  # filter company and tilte selected and assign to tem.
    # calculate mean of base salary of each company and title, assian to d. 
    d <- aggregate(tem$stockgrantvalue, by=list(tem$yearsofexperience,tem$company), FUN=mean)  
    colnames(d) <- c("exper", "company", "meanstock")  # rename d.
    
    ggplot(data=d, aes(as.numeric(exper), meanstock,color=company))+
      geom_point(alpha = 0.3)+  # plot all points with transparence = 0.3
      geom_smooth(aes(group=company),se=FALSE)+    # plot fitted line without standare error.
      ylab("mean stock grants [$]")+
      xlab("experience of work [year]")+   # set x and y label.
      scale_x_continuous(breaks = seq(0,42,2))+   # set x axis interval.
      ggtitle("Mean stock grants changes with working experience")+  # set plot title.
      theme(plot.title = element_text(size=11, face="bold"),   # modify text size and style.
            # panel.background = element_rect(fill="#a6cae3"),
            # axis.text.x = element_text(angle = 0,hjust=1),
            axis.title.x = element_text(size=10, face="bold"),
            axis.title.y = element_text(size=10, face="bold"),
            axis.text.x = element_text(size=9, face="bold"),
            axis.text.y = element_text(size=9, face="bold")
      )
    
  }
  
}
#-----------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------#



##----Data Wrangling-------------------------------------------------------------------------####
#-----------------------------------------------------------------------------------------------#
lfsd.df<- read.csv("lfsd_df.csv")
company_unique_names <- unique(lfsd.df$company)    # get all unique company names.
try <- unique(lfsd.df$family)
# assign kinds of company to company_family. 
company_family <- c("Software","E-commerce","E-commerce","IT","Software","IT",
                        "Social Media","Transportation","IT","Entertainment","Software",
                        "Social Media","Manufacturing","Transportation",
                        "Software","Social Media","Retail","Manufacturing",
                        "Retail","Manufacturing","E-commerce","Software","Social Media",
                        "Finance","Software","E-commerce","Manufacturing","Finance",
                        "Finance","E-commerce")


company_name <- data.frame(name = company_unique_names, family = paste(company_family))  # assign unique company and family to company_name
job_titles <- unique(lfsd.df$title)  # assign name of job title to job_title.


#-----------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------#






##----Shiny Code-----------------------------------------------------------------------------####
#-----------------------------------------------------------------------------------------------#

# Define UI for application that draws a histogram
ui <- fluidPage(
  navlistPanel(widths = c(2, 10),
              # first tab Panel, showing the location of jobs.
              tabPanel("Income Overview",   # name of tab Panel. 
                   titlePanel("Select job title and companies."), # name of the Panel. 
                   sidebarLayout(
                       sidebarPanel(width=4,
                                    radioButtons("job_title", label="job title", job_titles[1:12],selected = 0),   # set Buttons for job title selection.
                                    
                                    selectInput("companies_family", label = "company family",   # set input selection for company classification 
                                                choices=c("All", unique(company_family)),       
                                                selected = "All",multiple = TRUE),
                                    
                                    selectInput("company_name",     # set input selection for company names.
                                                label = "company name", 
                                                choices=NULL, 
                                                selected = NULL,multiple = TRUE)),
                       # display mean baseslary, mean bonus, and mean stock plot according to the job title and companies selected. 
                       mainPanel(width = 8,   
                                 plotOutput("mean_basesalary",width ="60%",height = "200px"),
                                 plotOutput("mean_bonus", width ="60%",height = "200px"),
                                 plotOutput("mean_stock", width="60%", height="200px"))
                    )
                 ),
          
          # second tab Panel, showing the location of jobs.
          tabPanel("Location",  
                   titlePanel("Company & Job Location"),  # name of tab panel.
                   sidebarLayout(
                    
                     # select slider input for the range of base salary, bonus, stock, and work experience.
                     sidebarPanel("your conditions", width=3,
                                  sliderInput("base_salary_wanted",label="base",min=0,max=2000000,value=c(0,0)),
                                  sliderInput("base_bonus_wanted",label = "bonus",min=0,max=1e+06,value=c(0,0)),
                                  sliderInput("base_stock_wanted",label="stock grants",min=0,max=2800000,value=c(0,0)),
                                  sliderInput("exp_had",label="previous working experience",min=0,max=42,value=c(0,0))
                                  ),
                     
                     mainPanel(width=9,  # Display map created in server function. 
                               leafletOutput("map", height=700, width = "100%") # display leafletOutput build-in "map"
                               )
                   )
    
                  )
        )
      )

#           # input ports
#           ## 选框输入
#           selectizeInput("company_name", label = "company name", company_names[1:3], selected = NULL,multiple = TRUE),
#           radioButtons("job_title", "job title", job_titels[1:3]),
#           
#           
#           ## 数字输入
#           numericInput("num", "Number one", value=0, min=0, max=100),
#           sliderInput("min", "Limit (minimum)", value = 50, min = 0, max = 100),
#           
#           ## 点击输入
#           actionButton("click", "Click me!"),
#           actionButton("drink","Drink me!", icon=icon("person")),
#           
#           #----------------------------------------------------------------------
#           # output ports
#           textOutput("text"),
#           verbatimTextOutput("code"),
#           
#           tableOutput("static"),
#           dataTableOutput("dynamic")
#           
#           
#           
# )

# Define server logic required to draw a histogram
server <- function(input, output) {
          # filter the company name under the family selected-------------------
             family_selected <- reactive({input$companies_family})  
             
              
             observeEvent(family_selected(),{  
               if("All" %in% family_selected())   # if "All" is selected show all company names
               {choices <- company_name[,"name"]}
               
               # else show all company names according to user's choice of company classification.
               else {choices <- company_name[which(company_name$family %in% family_selected()),]}
               
               # return the company names to selectInput("company_name") according to company family selected above.
               updateSelectInput(inputId="company_name", choices=choices)
             })
          #-------------------------------------------------------------------
             
             
          #-------------------------------------------------------------------
             company_selected <- reactive({input$company_name})   # assign company selected to company selected by reactive function.
             title_selected <- reactive({input$job_title})        # assign job title selected to company selected by reactive function.
             
             # return mean base salary plot to plotOutput("mean_basesalary")
             output$mean_basesalary <- renderPlot({mean_basesalary_plot(company_selected(),title_selected())
                                   },res=96)
             
             # return mean bonus plot to sliderInput("base_bonus_wanted")
             output$mean_bonus <- renderPlot({mean_bonus_plot(company_selected(),title_selected())},res=96)
             
             # return mean stock plot to plotOutput("mean_stock")
             output$mean_stock <- renderPlot({mean_stock_plot(company_selected(),title_selected())},res=96)
             
             
             # assign base salary, bonus, stock and work experience to bsr, bonusr,stockr and experiencer by reactive function.
             bsr <- reactive({input$base_salary_wanted})
             bonusr <- reactive({input$base_bonus_wanted})
             stockr <- reactive({input$base_stock_wanted})
             experiencer <- reactive({input$exp_had})

             
             output$map <- renderLeaflet({ 
                                    select(lfsd.df[1:1000,],basesalary, bonus,  # select base salary, bonus, etc.,of first 1000 observations 
                                           stockgrantvalue, yearsofexperience,
                                           lat,lng, company, title,location,family) %>%
                 
                                    filter(basesalary>bsr()[1]& basesalary < bsr()[2] |  # filter set up range for each sliderInput.
                                             bonus>bonusr()[1]& bonus < bonusr()[2] |
                                             stockgrantvalue > stockr()[1] & stockgrantvalue < stockr()[2] |
                                             yearsofexperience > experiencer()[1] & yearsofexperience < experiencer()[2]) %>%
                                             
                                    leaflet() %>%
                                        addTiles() %>%
                                        # setMaxBounds(lng1=0, lat1=-90, lng2=360, lat2=90) %>%
                                        setView(lng=-40.8906866703548,lat=35.1226660865198,zoom=2, options=list(maxZoom=1000)) %>%
                 
                                        # display location of jobs meeting conditions.
                                        addMarkers(lng=~lng, lat=~lat, clusterId = TRUE, popup = ~paste(company, "<br>", 
                                                                                                        title, "<br>",
                                                                                                        "base_sal:", basesalary,"<br>",
                                                                                                        "bonus:", bonus, "<br>",
                                                                                                        "stock:",stockgrantvalue))
    
                                        
                           })
             
}


# Run the application 
shinyApp(ui = ui, server = server)
#-----------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------#
