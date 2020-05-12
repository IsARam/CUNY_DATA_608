#####################################################################################
#Global
library(ggplot2)
library(dplyr)
library(plotly)

#Data
df <- readr::read_csv("./TidyPerformance.csv")

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
                ),
                mainPanel(plotOutput("plot4"))
            )
        )))

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
            geom_col() + coord_flip() + scale_fill_brewer(palette = "Set3") +
            xlab("Branch") + ylab("Average OTP (%)") +
            theme(plot.title = element_text(hjust = 0.7)) +
            ggtitle('Annual Average OTP') +
            geom_text(aes(label = format(round(
                df1()$Average_OTP, 1
            ), nsmall = 1)), hjust = 1.7, size =
                5.0) +
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
            theme(plot.title = element_text(hjust = 0.7)) +
            ggtitle('OTP By Year') +
            guides(color = guide_legend(title = "Branch")) +
            scale_x_continuous(breaks = unique(df2()$Period.Year))
    })
    
    output$text1 <- renderUI({
        
        perf2 <- df2() %>%
            filter(., Indicator.Name == input$Branch1)%>%
            arrange(., desc(Average_OTP))
 
    })
    
    
}
#########################################################################
#App
shinyApp(ui, server)