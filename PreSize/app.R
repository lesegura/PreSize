library(rsconnect)
library(tidyverse)
library(shiny)
library(DT)
library(shinyFeedback)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("slate"),
    # Application title
    titlePanel("Calculating sample size based on precision for clustered data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("estimate", "Choose the estimate of interest", c("Risk Difference", 
                                                                         "Risk Ratio", 
                                                                         "Incidence Rate Difference", 
                                                                         "Incidence Rate Ratio", 
                                                                         "Odds Ratio")),
            conditionalPanel(
                        condition = "input.estimate == 'Risk Difference' || input.estimate == 'Risk Ratio' || input.estimate == 'Odds Ratio'" ,
                        sliderInput("p1",
                        "Risk in the exposed",
                        min = 0.01,
                        max = 0.99,
                        value = 0.4)
                        ), 
            conditionalPanel(
                        condition = "input.estimate == 'Incidence Rate Difference' || input.estimate == 'Incidence Rate Ratio'" ,
                        sliderInput("i1",
                        "Incidence Rate in the exposed",
                        min = 0.01,
                        max = 0.99,
                        value = 0.4)
                        ), 
            conditionalPanel(
                        condition = "input.estimate == 'Risk Difference' || input.estimate == 'Risk Ratio' || input.estimate == 'Odds Ratio'" ,
                        sliderInput("p0",
                        "Risk in the unexposed",
                        min = 0.01,
                        max = 0.99,
                        value = 0.3)
                        ),
            conditionalPanel(
                        condition = "input.estimate == 'Incidence Rate Difference' || input.estimate == 'Incidence Rate Ratio'" ,
                        sliderInput("i0",
                        "Incidence Rate in the unexposed",
                        min = 0.01,
                        max = 0.99,
                        value = 0.3)
            ),
            conditionalPanel(
                        condition = "input.estimate != 'Odds Ratio'", 
                        sliderInput("r",
                        "Ratio of the unexposed group size to the exposed group size",
                        min = 1,
                        max = 10.0,
                        value = 3, 
                        step = 1)
                        ), 
            conditionalPanel(
                condition = "input.estimate == 'Odds Ratio'", 
                sliderInput("r_cc",
                            "Ratio of Controls to Cases",
                            min = 1,
                            max = 10.0,
                            value = 3, 
                            step = 1)
            ), 
            sliderInput("cl",
                        "Significance level of the CI",
                        min = 0.01,
                        max = 0.99,
                        value = 0.90, 
                        step = 0.01),
            conditionalPanel(
                        condition = "input.estimate == 'Risk Difference'",
                        sliderInput("f",
                        "Confidence Interval width",
                        min = 0.01,
                        max = 0.99,
                        value = 0.08, 
                        step = 0.01)
                        ),
            conditionalPanel(
                condition = "input.estimate == 'Risk Ratio'",
                sliderInput("f_r",
                            "Ratio of the ULCI / LLCI",
                            min = 1,
                            max = 20,
                            value = 2, 
                            step = 1)
            ),
            sliderInput("deff",
                        "Design Effect",
                        min = 1,
                        max = 99,
                        value = 1, 
                        step = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           h4("Sample Size Calculation"),
           DTOutput("smp_size_tab"), 
           br(),
           textOutput("msg_txt"), 
           tags$head(tags$style("#msg_txt{color: #ffa600}"
           )
        )
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    z <- reactive({
        qnorm(1 - (1 - input$cl) / 2)
    })
    
    mynames <- reactive({
        if(input$estimate == "Odds Ratio") {
        c('<span style="color:#CFCFCF">Cases Group Size</span>', 
          '<span style="color:#CFCFCF">Controls Group Size</span>', 
          '<span style="color:#CFCFCF">Total Sample Size</span>')
    } else if(input$estimate != "Odds Ratio") {
        c('<span style="color:#CFCFCF">Exposed Group Size</span>', 
          '<span style="color:#CFCFCF">Unexposed Group Size</span>', 
          '<span style="color:#CFCFCF">Total Sample Size</span>')
    }
 }
 )
    
    output$smp_size_tab <- renderDT(
        
        if(input$estimate == "Risk Difference"){
        tibble(       
        n1 = 4 * z() ^ 2 * input$deff * (input$r * input$p1 * (1 - input$p1) + input$p0 * (1 - input$p0)) / (input$f ^ 2 * input$r), 
        n0 = n1 * input$r,
        N = n1 + n0) %>%
            round()
        } else if(input$estimate == "Risk Ratio"){
            tibble(       
                n1 = 4 * z() ^ 2 * input$deff * (input$r * input$p0 * (1 - input$p1) + input$p1 * (1 - input$p0)) / (input$r * input$p1 * input$p0 * log(input$f_r) ^ 2), 
                n0 = n1 * input$r,
                N = n1 + n0) %>%
                round()
        } else if(input$estimate == "Odds Ratio"){
            tibble(       
                n1 = 4 * z() ^ 2 * input$deff * (input$r_cc * input$p0 * (1 - input$p0) + input$p1 * (1 - input$p1)) / 
                    ((log(input$f_r) ^ 2) * (input$r_cc * input$p1 * input$p0 * (1 - input$p1) * (1 - input$p0))), 
                n0 = n1 * input$r_cc, 
                N = n1 + n0) %>%
                round()
        } else if(input$estimate == "Incidence Rate Difference"){
            tibble(       
                n1 = 4 * z() ^ 2 * input$deff * (input$r * input$i0 + input$i1) / (input$r * input$f ^ 2), 
                n0 = n1 * input$r_cc, 
                N = n1 + n0) %>%
                round()
        }
        else if(input$estimate == "Incidence Rate Ratio"){
            tibble(       
                n1 = 4 * z() ^ 2 * input$deff * (input$r * input$i0 + input$i1) / (input$r * input$i1 * input$i0 * log(input$f_r) ^ 2), 
                n0 = n1 * input$r_cc, 
                N = n1 + n0) %>%
                round()
        }
        , 
        selection = "none",
        rownames = FALSE,
        options = list(pageLength = 2, dom = "t"),
        class = "hover",
        escape = F,
        colnames = mynames()
    )
    
    output$msg_txt <- renderText({
        if(input$estimate == "Risk Difference" | input$estimate == "Incidence Rate Difference") {
            paste(
                paste(
                    paste(
                        paste(
                            paste(
                                paste(
                                    paste("Sample size based on precision for a CI width of", input$f, sep = " "), 
                                    "a deff of", sep = ", "), 
                                input$deff, sep = " "), 
                            "a confidence level of", sep = ", "), 
                        input$cl, sep = " "), "and an unexposed to exposed group size ratio of", sep = ", "),
                input$r, sep = " ")
        } else if(input$estimate == "Risk Ratio" | input$estimate == "Incidence Rate Ratio") {
            paste(
                paste(
                    paste(
                        paste(
                            paste(
                                paste(
                                    paste("Sample size based on precision for CI ratio (ULCI / LLCI) of", input$f_r, sep = " "), 
                                    "a deff of", sep = ", "), 
                                input$deff, sep = " "), 
                            "a confidence level of", sep = ", "), 
                        input$cl, sep = " "), "and an unexposed to exposed group size ratio of", sep = ", "),
                input$r, sep = " ")
        } else if(input$estimate == "Odds Ratio"){
            paste(
                paste(
                    paste(
                        paste(
                            paste(
                                paste(
                                    paste("Sample size based on precision for a CI ratio (ULCI / LLCI) of", input$f_r, sep = " "), 
                                    "a deff of", sep = ", "), 
                                input$deff, sep = " "), 
                            "a confidence level of", sep = ", "), 
                        input$cl, sep = " "), "and an ratio of Controls to Cases of", sep = ", "),
                input$r_cc, sep = " ")
        }
        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
