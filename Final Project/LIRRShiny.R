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
    "On Time Performance",
    tabPanel(
      title = 'Main Page',
      fluid = TRUE,
      fluidRow(column(12,
                      includeMarkdown("main_page.rmd"))),
      fluidRow(br(),
               column(12,
                      br(),
                      column(
                        12,
                        
                      )))
    ),
    tabPanel(
      tagList(shiny::icon('bar-chart'), "Yearly OTP By Branch"),
      sidebarLayout(
        sidebarPanel(
          helpText(
            'To view On-Time Performance of each branch for a given year,
            select a year from the drop down menu.'
          ),
          selectizeInput(
            inputId = "year",
            label = "Select Year",
            unique(df$Period.Year),
            selected = '2018'
          )
        ),
        mainPanel(plotOutput("plot1"))
      )
    ),
    tabPanel(
      tagList(shiny::icon('chart-line'), "Performance Over Time"),
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          helpText(
            'To view branch historic performance,
            select a branch from the drop down menu.'
          ),
          selectizeInput(
            inputId = "Branch1",
            label = "Select Branch",
            unique(df$Indicator.Name),
            selected = 'Babylon'
          ),
          htmlOutput('text1')
          
        ),
        mainPanel(plotOutput("plot3"))
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
      ggtitle('Yearly On-Time Performance') +
      geom_text(aes(label = format(round(
        df1()$Average_OTP, 2
      ), nsmall = 2)), hjust = 1.5, size =
        3.0) +
      guides(fill = guide_legend(title = "Branch"))
  })
  
  df2 <- reactive({
    df %>%
      filter(., Indicator.Name != 'On-Time Performance') %>%
      group_by(Indicator.Name) %>%
      summarise(., Average_OTP = mean(Monthly.Actual)) %>%
      arrange(., (Average_OTP))
  })
  
  output$plot2 <- renderPlot({
    ggplot(df2(),
           aes(
             x = reorder(df2()$Indicator.Name, df2()$Average_OTP),
             y = df2()$Average_OTP,
             fill = df2()$Indicator.Name
           )) +
      geom_col() + coord_flip() + scale_fill_brewer(palette = "Paired") +
      xlab("Branch") + ylab("Average OTP (%)") +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle('Average On-Time Performance Since 2008') +
      geom_text(aes(label = format(round(
        df2()$Average_OTP, 2
      ), nsmall = 2)), hjust = 1.5, size =
        3.0) +
      guides(fill = guide_legend(title = "Branch"))
  })
  
  
  df3 <- reactive({
    df%>%
      filter(., Indicator.Name == input$Branch1 | Indicator.Name == "Overall")%>%
      group_by(., Indicator.Name, Period.Month)%>%
      summarise(., Average_OTP = mean(Monthly.Actual))%>%
      arrange(., desc(Average_OTP))
  })
  
  output$plot3 <- renderPlot({
    ggplot(df3(), aes(x = df3()$Period.Month, y = df3()$Average_OTP)) +
      geom_line(aes(color = df3()$Indicator.Name)) +
      xlab("Month") + ylab('Percentage On-Time, (%)') +  
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle('Branch Average Monthly On-Time Performance Since 2008') +
      scale_x_discrete(limits = month.abb) +
      guides(color = guide_legend(title = "Branch"))
  })
  
  df5 <- reactive({
    df %>%
      filter(., Indicator.Name == input$Branch1 | Indicator.Name == "Overall")%>%
      group_by(., Indicator.Name, Period.Year) %>%
      summarise(., Average_OTP = mean(Monthly.Actual))
  })
  
  output$plot4 <- renderPlot({
    ggplot(df5(), aes(x = df5()$Period.Year, y = df5()$Average_OTP)) +
      geom_line(aes(color = df5()$Indicator.Name)) +
      xlab("Year") + ylab('Percentage On-Time, (%)') +  
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle('Branch On-Time Performance By Year Since 2008') +
      guides(color = guide_legend(title = "Branch")) +
      scale_x_continuous(breaks = unique(df5()$Period.Year))
  })
  
  output$text1 <- renderUI({
    perf1 <- df3() %>%
      filter(., Indicator.Name == input$Branch1)%>%
      arrange(., desc(Average_OTP))
    
    top1 <- head(perf1, 1)
    bot1 <- tail(perf1, 1)
    
    perf2 <- df5() %>%
      filter(., Indicator.Name == input$Branch1)%>%
      arrange(., desc(Average_OTP))
    
    top2 <- head(perf2, 1)
    bot2 <- tail(perf2, 1)
    
    str <-
      paste(
        '<b>Highest Performance Month:</b>',
        month.abb[top1$Period.Month],
        '<br><b>Lowest Performance Month:</b>',
        month.abb[bot1$Period.Month],
        '<br><b>Highest Performance Year:</b>',
        top2$Period.Year,
        '<br><b>Lowest Performance Year:</b>',
        bot2$Period.Year
      )
    HTML(paste(str, sep = '<br/>'))
  })
  
  df6 <- reactive({
    assoc_df
  })
  
  output$plot5 <- renderPlot({
    ggplot(df6(),
           aes(
             x = df6()$distance,
             y = df6()$Average_OTP,
             color = factor(df6()$Indicator.Name)
           )) +
      geom_point(size = 4.0) + scale_fill_brewer(palette = "Paired") +
      geom_smooth(
        method = 'lm',
        formula = y ~ log(x),
        color = 'black',
        se = FALSE,
        size = .1
      ) +
      xlab("Track Distance, (mi)") + ylab("Average OTP (%)") +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle('Average On-Time Performance as a Function of Branch Track Distance') +
      guides(color = guide_legend(title = "Branch"))
  })
  
  output$plot6 <- renderPlot({
    ggplot(df6(),
           aes(
             x = df6()$num_of_stations,
             y = df6()$Average_OTP,
             color = factor(df6()$Indicator.Name)
           )) +
      geom_point(size = 4.0) + scale_fill_brewer(palette = "Paired") +
      geom_smooth(
        method = 'lm',
        color = 'black',
        se = FALSE,
        size = .1,
        formula = y ~ x
      ) +
      xlab("Number of Stations, (n)") + ylab("Average OTP (%)") +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle('Average On-Time Performance as a Function of Number of Stations') +
      guides(color = guide_legend(title = "Branch"))
    
  })
  
  output$table1 = DT::renderDataTable({
    df
  })
  
  
}
#########################################################################
#App
shinyApp(ui, server)