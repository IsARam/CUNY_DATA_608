#####################################################################################
#Global
library(shinydashboard)
library(shiny)
library(googleVis)
library(ggplot2)
library(dplyr)
library(leaflet)
library(shinyTime)
library(lubridate)
library(hms)
library(DT)
library(markdown)
library(plotly)


#Get clean data
df <- readr::read_csv("./Performance_DF_Cleaner.csv")
#assoc_df <- readr::read_csv("./assoc_df.csv")

#####################################################################################
#UI
ui <- fluidPage(
  navbarPage(
    "LIRR On Time Performance",
    tabPanel(
      tagList(shiny::icon('atlas'), "Overview"),
      fluid = TRUE,
      fluidRow(column(12,
                      includeMarkdown("overview.rmd")))
    ),
    tabPanel(
      tagList(shiny::icon('bar-chart'), "OTP By Branch"),
      sidebarLayout(
        sidebarPanel(
          
          selectizeInput(
            inputId = "year",
            label = "Select Year",
            unique(df$Period.Year),
            selected = '2018'
          ),
          
          helpText(
            'To view OTP of each branch for a given year.')
    
        ),
        mainPanel(plotOutput("plot1"))
      )
    ),
    tabPanel(
      tagList(shiny::icon('chart-line'), "OTP By Year"),
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
         
          selectizeInput(
            inputId = "Branch1",
            label = "Select Branch",
            unique(df$Indicator.Name),
            selected = 'Babylon'
          ),
          
          helpText(
            'To view branch historic performance.'
          ),
          htmlOutput('text1')
          
        ),
        mainPanel(plotOutput("plot4"))
      )
  
    ),
    
    tabPanel((tagList(
      shiny::icon('table'), "OTP Data Table"
    )),
    DT::dataTableOutput("table1"))
  )
)


#####################################################################################
#Server

server <- function(input, output, session) {
  df1 <- reactive({
    df %>%
      filter(., Indicator.Name != 'On-Time Performance') %>%
      filter(., Period.Year == input$year) %>%
      group_by(Indicator.Name) %>%
      summarise(., Average_OTP = mean(Monthly.Actual)) %>%
      arrange(., desc(Average_OTP))
  })
  
  output$plot1 <- renderPlot({
    ggplot(df1(),
           aes(
             x = reorder(df1()$Indicator.Name, df1()$Average_OTP),
             y = df1()$Average_OTP,
             fill = df1()$Indicator.Name
           )) +
      geom_col() + coord_flip() + scale_fill_brewer(palette = "Paired") +
      xlab("Branch") + ylab("Average OTP (%)") +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle('Annual Average OTP') +
      geom_text(aes(label = format(round(
        df1()$Average_OTP, 2
      ), nsmall = 2)), hjust = 1.5, size =
        3.0) +
      guides(fill = guide_legend(title = "Branch"))
  })
  
  df2 <- reactive({
    df %>%
      filter(., Indicator.Name == input$Branch1 | Indicator.Name == "Overall")%>%
      group_by(., Indicator.Name, Period.Year) %>%
      summarise(., Average_OTP = mean(Monthly.Actual))
  })
  
  output$plot4 <- renderPlot({
    ggplot(df2(), aes(x = df2()$Period.Year, y = df2()$Average_OTP)) +
      geom_line(aes(color = df2()$Indicator.Name)) +
      xlab("Year") + ylab('Average OTP, (%)') +  
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle('OTP By Year') +
      guides(color = guide_legend(title = "Branch")) +
      scale_x_continuous(breaks = unique(df2()$Period.Year))
  })
  
  output$text1 <- renderUI({
    
    perf2 <- df2() %>%
      filter(., Indicator.Name == input$Branch1)%>%
      arrange(., desc(Average_OTP))
    
    top2 <- head(perf2, 1)
    bot2 <- tail(perf2, 1)
    
    str <-
      paste(
        '<br><b>Highest Performance Year:</b>',
        top2$Period.Year,
        '<br><b>Lowest Performance Year:</b>',
        bot2$Period.Year
      )
    HTML(paste(str, sep = '<br/>'))
  })
  
  output$table1 = DT::renderDataTable({
    df
  })
  
  
}
#########################################################################
#App
shinyApp(ui, server)