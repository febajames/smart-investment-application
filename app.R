# Shiny app for smart investment


# install packages if not present --------------------------------------------------------
list.of.packages <- c("shinydashboard", "dplyr","formattable","shinycssloaders","ggplot2","plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#load libraries---------------------------------------------------------------------------
library(shinydashboard)
library(dplyr)
library(formattable)
library(shinycssloaders)
library(plotly)
library(ggplot2)
source("model_call.R")


# Read the input data --------------------------------------------------------------------
#input data for grades
intgrades <- read.csv("data/interestgrades.csv",sep = ",",stringsAsFactors = FALSE)
#input data for model
model_data <- readRDS("data/model_data.rds")
model_data$loan_status<-factor(model_data$loan_status)
# input data for testing model
sample_data <- readRDS("data/sample_data.rds")



header <- dashboardHeader(  title = "Smart Investment",
                           
                            tags$li(a(href = 'https://www.lendingclub.com/',
                                      img(src = 'logo.png',
                                          title = "Company Home", height = "30px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"),
  dropdownMenu(
    type = "messages",
    messageItem(
      from = "Lending Club",
      message = "Build your Portfolio now and start investing",
      href = "https://www.lendingclub.com/lenderg/createaccount"
    ))
  
)
sidebar <- dashboardSidebar(
 sidebarMenu( id = "All",
   menuItem(text = "Welcome",icon = icon("fas fa-home"),tabName = "in_dt"),
   menuItem(text = "Interest Rates Guide",icon = icon("table"), tabName = "grades"),
   menuItem(text = "Shortlist Investment", tabName = "forinvestors",icon = icon("list-ol")),
   menuItem(text = "EDA",icon = icon("bar-chart-o"), tabName = "eda"),
   menuItem("Order Summary", tabName="Results", icon= 
              icon("file-text-o"))
   )
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "in_dt", 
            fluidRow(
              column(width = 7,h2(" Welcome to the Manual Strategy Guide"),
              h4("This is a comprehensive guide to hand pick your investments."),
                 br(),h4(
              tags$ol(
                tags$li("Take a look at how grades are assigned based on the interest rates"),
  tags$li("Scan through profiles based on loan purpose, interest rates and borrower information"),
  tags$li("Choose to lend based on the repayment probability that suit your risk appetite"))
  )
              ),
              column(width = 5, align = "center",
                     img(src='handpick.jpeg'))
              )
            
    ),
    
    tabItem(tabName = "forinvestors",
            fluidRow(
              
              box(title = "General Information"
                  ,status = "primary"
                  ,solidHeader = TRUE 
                  ,collapsible = TRUE, width = 12
                  ,column(width = 4,numericInput(inputId = "int_rt",
                               label = "interest rate (min)",
                               min = 0, max = 30, value = (min(sample_data$int_rate)*100) + 1)),
                  column(width = 4, radioButtons("notev", "Single Note value:",
                               c("$ 25" = 25,
                                 "$ 50" = 50,
                                 "$ 75" = 75,
                                 "$ 100" = 100)))
                  
                  # ,column(width = 4,numericInput(inputId = "dti",
                  #             label = "debt to income ratio(max)",
                  #             min = 0, max = 30, value = max(sample_data$dti) + 1))
                  ,column(width = 4,numericInput(inputId = "loan_amt",
                              label = "Loan Amount (max)",
                              min = 0, max = 500000, value = max(sample_data$loan_amnt) + 1))
                  )),
            fluidRow(
              box(title = "Borrower Information", width = 12, icon = icon("users"),
                  status="primary",
                  color = "olive"
                  ,solidHeader = TRUE 
                  ,collapsible = TRUE
                  ,column(width = 4,uiOutput("purpose"))
                  ,column(width = 4,uiOutput("empdur"))
                  ,column(width = 4,numericInput(inputId = "annualinc",
                              label = "Annual Income(min)",
                            min = 500, max = 500000, value = min(sample_data$annual_inc) + 1))
                  )
              
            )
          ,fluidRow( width = 12,align = "right",
                    box(align="center", icon = icon("fa-filter"),
                    actionButton("model_run", "Shortlist",icon = icon("list-ol")),
                    actionButton("reset", "Reset",icon = icon("refresh")),
                    actionButton("switch_tab", " Buy", icon = icon("credit-card"))
                    )),
            fluidRow(withSpinner(DT::DTOutput("tbl"))
                     ))
            ,
    tabItem(tabName = "grades", fluidRow(
                                      
                                        img(src='grade_risk.jpg', align = "center"),br(),br(),
                                         column(align = "center" ,width = 5,dataTableOutput("sgrade"))
                                         )
            ),
    tabItem(tabName = "eda",
            tabBox(width = "100",tabPanel("Univariate",  
                            plotlyOutput("UPlot1", width = "65%"),
                            plotlyOutput("UPlot2", width = "65%"),
                            plotlyOutput("UPlot3", width = "65%"),
                            plotlyOutput("UPlot4", width = "65%"),
                            plotlyOutput("UPlot5",width = "65%"),
                            plotlyOutput("UPlot6",width = "65%"),
                            plotlyOutput("UPlot7",width = "65%")),
                   
                   
                   tabPanel("Bivariate",
                   plotlyOutput("BPlot1", width = "65%"),
                   plotlyOutput("BPlot2", width = "65%"),
                   plotlyOutput("BPlot3", width = "65%"),
                   plotlyOutput("BPlot4", width = "70%"),
                   plotlyOutput("BPlot5", width = "65%"),
                   plotlyOutput("BPlot6", width = "65%")),
                   tabPanel("Correlation Plot",
                            fluidRow(width = 6,img(src="Corr.png"))),
                   tabPanel("Model Summary",
                            fluidRow(width = 6,img(src="Sum1.png"))))
            ),
    tabItem(tabName = "Results", fluidRow(h3("Order Summary"), hr(),br(),br(),
                                          infoBoxOutput("approvalBox"),
                                          infoBoxOutput("notevalue"),
                                          valueBoxOutput("investedvalue")
                                          
                                          ),
            fluidRow(actionButton("invm", "Invest more",icon = icon("credit-card"))))
)
  
)



# Create the UI using the header, sidebar, and body
ui <- dashboardPage(header, sidebar, body, skin = "red")

server <- function(input, output, session) {
  
   
 
  # Input selection  based on input data.
  output$purpose <- renderUI({
    # if(is.null(input$datafile))
    #   return()
    
    # Get the data set with the appropriate name
    dat <- sample_data
    
    # Create the Select inputt
    checkboxGroupInput(inputId = "purpose", #name of input
                label = "Loan Purpose:", #label displayed in ui
                choices = as.character(unique(dat$purpose))
    )
  })
  
  # Input selection based on input data.
  output$empdur <- renderUI({
    
    
    # Get the data set with the appropriate name
    dat <- sample_data
    
    # Create the Select input
    checkboxGroupInput(inputId = "emp_dur", #name of input
                label = "Duration of employment:", #label displayed in ui
                choices =  as.character(unique(dat$emp_dur)))
    
  })
  
  observe({
    if (input$reset > 0) {
      updateCheckboxGroupInput(session=session, inputId = "purpose", #name of input
                               label = "Loan Purpose:", #label displayed in ui
                               choices = as.character(unique(sample_data$purpose)))
      updateCheckboxGroupInput(session=session,inputId = "emp_dur", #name of input
                               label = "Duration of employment:", #label displayed in ui
                               choices =  as.character(unique(sample_data$emp_dur)))
      updateNumericInput(session = session, inputId = "int_rt",
                         label = "interest rate (min)",
                         min = 0, max = 30, value = (min(sample_data$int_rate)*100) + 1)
      # updateNumericInput(session = session,inputId = "dti",
      #                    label = "debt to income ratio(max)",
      #                    min = 0, max = 30, value = max(sample_data$dti) + 1)
      updateNumericInput(session = session, inputId = "loan_amt",
                         label = "Loan Amount (max)",
                         min = 0, max = 500000, value = max(sample_data$loan_amnt) + 1)
      updateNumericInput(session = session, inputId = "annualinc",
                         label = "Annual Income(min)",
                         min = 500, max = 500000, value = min(sample_data$annual_inc) + 1)
    }
  })
 
  
  
  
  filtered <- reactive({
    
    if(!is.null(input$purpose) | !is.null(input$emp_dur)){
   a <- sample_data[sample_data$purpose %in% input$purpose & 
                      sample_data$emp_dur %in% input$emp_dur &
                      #sample_data$dti <= input$dti &
                      sample_data$annual_inc >= input$annualinc 
                       & sample_data$int_rate >= (input$int_rt) * 0.01 
                      & sample_data$loan_amnt <= input$loan_amt 
                    ,]
    }else{
      a <- sample_data[
                         #sample_data$dti <= input$dti &
                         sample_data$annual_inc >= input$annualinc 
                       & sample_data$int_rate >= (input$int_rt) * 0.01 
                       & sample_data$loan_amnt <= input$loan_amt 
                       ,]
    }
    
    
   return(a)
  })
  
  
  
  output$tbl <-  DT::renderDT({
    
    if(input$model_run > 0 & nrow(filtered()) != 0){
      results <- risk_propensity(model_data = model_data, sample_data = filtered())
    }else{
      results <- filtered()
      
    }
    
    
    return(results)
    
  }) 
  
  output$sgrade <- renderDataTable(intgrades)
  
  
  
  output$approvalBox <- renderInfoBox({
    s = input$tbl_rows_selected
    infoBox("Notes added",
      paste0(length(s)),  icon = icon("list"),
      color = "yellow"
    )
  })
  
  output$notevalue <- renderInfoBox({
    s = input$tbl_rows_selected
    infoBox("Note value",
      paste("$", input$notev),  icon = icon("list"),
      color = "purple"
    )
  })
  
  output$investedvalue <- renderValueBox({
    s = input$tbl_rows_selected
    tot = length(s) * as.numeric(input$notev)
    valueBox(
     paste0("$ ",tot) , "invested", icon = icon("credit-card"),
      color = "yellow"
    )
  })
  
  # to move to final order tab after placing order
  
  observeEvent(input$switch_tab, {
    updateTabItems(session, "All","Results")
  })
  
  # To ask the investor to go back and invest more.
  observeEvent(input$invm, {
    updateTabItems(session, "All","forinvestors")
  })
  
  
  # EDA plots ----------------------------------
  model_data
  output$UPlot1 <- renderPlotly({
    plot.status<-ggplot(model_data,aes(x=model_data$loan_status,y=((..count..)/sum(..count..))*100))
    plot.status<-plot.status+geom_histogram(aes(fill=loan_status),stat="count",show.legend = FALSE)
    plot.status<-plot.status+labs(y="Percent",x="Status")+ggtitle("Loan Status")
    
    p <- ggplotly(plot.status)
  })
  
  output$UPlot2 <- renderPlotly({
    plot.pur<-ggplot(model_data,aes(x=purpose))+geom_bar(width=0.7,position="dodge",fill="turquoise3",show.legend = FALSE)
    plot.pur<-plot.pur+coord_flip()  
    plot.pur<-plot.pur+labs(y="Count",x="Purpose")+ggtitle("LoanPurpose")
    
    p <- ggplotly(plot.pur)
  })
  
  output$UPlot3 <- renderPlotly({
    home<-filter(model_data,home_ownership %in% c("RENT","MORTGAGE","OTHER"))
    plot.home<-ggplot(home,aes(x=home_ownership))+geom_bar(width=0.3,position="dodge",fill="turquoise3",show.legend = FALSE)
    plot.home<-plot.home+labs(y="Count",x="Ownership Type")+ggtitle("Home Ownership Types")
    
    p <- ggplotly(plot.home)
  })
  
  output$UPlot4 <- renderPlotly({
    plot.lgrade<-ggplot(model_data,aes(x=sub_grade))+geom_bar(width=0.7,position="dodge",fill="turquoise3",show.legend = FALSE)
    plot.lgrade<-plot.lgrade+labs(y="Count",x="Sub Grade")+ggtitle("Loan Grades")
    
    p <- ggplotly(plot.lgrade)
  })
  
  output$UPlot5 <- renderPlotly({
    plot.amt<-ggplot(model_data,aes(model_data$loan_amnt))+ geom_histogram(stat = "bin",binwidth = 2500, fill="turquoise3")
    plot.amt<-plot.amt+labs(y="Count",x="Amount")+ggtitle("Loan Amounts")
    
    p <- ggplotly(plot.amt)
  })
  
  output$UPlot6 <- renderPlotly({
    plot.rate<-ggplot(model_data,aes(x=model_data$int_rate))
    plot.rate<-plot.rate+geom_histogram(stat = "bin",binwidth = 0.02, fill="turquoise3")
    plot.rate<-plot.rate+labs(y="Count",x="Rate")+ggtitle("Interest Rates")
    
    p <- ggplotly(plot.rate)
  })
  
  output$UPlot7 <- renderPlotly({
    plot.dti<-ggplot(model_data,aes(x=model_data$dti))
    plot.dti<-plot.dti+geom_histogram(stat = "bin",binwidth = 5, fill="turquoise3")
    plot.dti<-plot.dti+labs(y="Count",x="DTI")+ggtitle("Debt-to-Income Ratios")
    
    p <- ggplotly(plot.dti)
  })
  
  output$BPlot1 <- renderPlotly({
    plot.grade.int<-ggplot(model_data,aes(grade,int_rate,fill=grade))+
      geom_boxplot(outlier.color = "blue",show.legend = FALSE)+
      labs(title="Boxplot of Loan Grade vs. Interest Rates",x="Grade",y="Interest Rate")
    
    p <- ggplotly(plot.grade.int)
  })
  
  output$BPlot2 <- renderPlotly({
    plot.dti.status<-ggplot(model_data,aes(loan_status,dti,fill=loan_status))+
      geom_boxplot(outlier.color = "blue",show.legend = FALSE)+
      labs(title="Boxplot of Loan Status vs. Debt-to-Income Ratio",x="Loan Status",y="DTI")+
      guides(color=guide_legend("my title"))
    
    p <- ggplotly(plot.dti.status)
  })
  
  output$BPlot3 <- renderPlotly({
    plot.amt.status<-ggplot(model_data,aes(loan_status,loan_amnt,fill=loan_status))+
      geom_boxplot(outlier.color = "blue",show.legend = FALSE)+
      labs(title="Boxplot of Loan Status vs. Loan Amount",x="Loan Status",y="Loan Amount")
    
    p <- ggplotly(plot.amt.status)
  })
  
  output$BPlot4 <- renderPlotly({
    plot.sub.status<-ggplot(model_data, aes(sub_grade))+
      geom_bar(aes(fill=loan_status),position = "fill",show.legend = FALSE)+
      labs(y="Percent",x="Sub Grade")+ggtitle("Proportion of Fully Paid and Not Fully Paid by Loan Sub Grades")
    
    p <- ggplotly(plot.sub.status)
  })
  
  output$BPlot5 <- renderPlotly({
    plot.amt.pur<-ggplot(model_data,aes(purpose))+geom_bar(aes(fill=loan_status),show.legend = FALSE)+
      theme(axis.text.x=element_text(size=8, angle = 90))
    plot.amt.pur<-plot.amt.pur+labs(x="Purpose")+ggtitle("Proportion of Fully Paid and Not Fully Paid by Loan Purpose")
    
    p <- ggplotly(plot.amt.pur)
  })
  
  output$BPlot6 <- renderPlotly({
    home=filter(model_data,home_ownership %in% c("RENT","MORTGAGE"))
    plot.type.status<-ggplot(home, aes(home_ownership))+
      geom_bar(aes(fill=loan_status),position = "fill",show.legend = FALSE)+
      labs(y="Percent",x="Ownership Type")+ggtitle("Proportion of Fully Paid and Not Fully Paid by Home Ownership Type")
    
    p <- ggplotly(plot.type.status)
  })
  
}



shinyApp(ui, server)
