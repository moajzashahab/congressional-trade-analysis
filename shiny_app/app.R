library(shiny)
library(DT)
library(dplyr)
library(readxl)
library(skimr)
library(ggplot2)
library(gridExtra)
library(plotly)

file_path = "../data/master_df.xlsx"
df = read_excel(file_path)

numerical_vars = c("trade_price", "price_change_pct", "seniority_years", "excess_return")
categorical_vars = c("transaction_type", "party", "state", "gender")


ui = fluidPage(titlePanel("Congressional Trade Analysis"),
        tabsetPanel(tabPanel("Raw Data Table", DTOutput("data_table")),
                    tabPanel("Summary Statistics", DTOutput("skim_table")),
                    tabPanel("Visualizations", h3("Static Histograms"),
                             plotOutput("Numerical Variables Histograms", height = "600px"),
                             hr(),
                             h3("Categorical Variables Bar Plots"),
                             selectInput("barplot_variable", 
                                         "Select Categorical Variable:", 
                                          choices = categorical_vars),
                             plotlyOutput("interactive_barplot"))))
                           
server = function(input, output, session) {
            output$data_table = renderDT({datatable(df, 
                                                    options = list(scrollX = TRUE, pageLength = 10, autoWidth = TRUE),
                                                    caption = "Congressional Trade Data")})
                             
            output$skim_table = renderDT({skim_df = skim(df)
                                          datatable(skim_df,
                                                    options = list(scrollX = TRUE, pageLength = 10, autoWidth = TRUE),
                                                    caption = "Summary Statistics for Data")})
            
            output$static_histograms = renderPlot({create_histogram = function(var) {
                                          ggplot(df, aes(x = .data[[var]])) +
                                          geom_histogram(aes(y = ..density..), 
                                                         bins = 30, fill = "blue", color = "black", alpha = 0.7) +
                                          geom_density(color = "red", size = 1) +
                                          labs(title = paste("Distribution of", var), x = var, y = "Density") +
                                          theme_minimal() +
                                          theme(plot.title = element_text(hjust = 0.5, size = 14),
                                                axis.text = element_text(size = 10),
                                                axis.title = element_text(size = 12))}
                                          histogram_plots = lapply(numerical_vars, create_histogram)
                                          grid.arrange(grobs = histogram_plots, ncol = 2)})
            
            output$interactive_barplot = renderPlotly({req(input$barplot_variable)
                                            create_interactive_barplot = function(var) {
                                                plot_ly(data = df,
                                                        y = ~reorder(.data[[var]], table(.data[[var]])[.data[[var]]]),
                                                        type = "bar",
                                                        orientation = "h",
                                                        marker = list(color = "steelblue")) %>%
                                                layout(title = paste("Distribution of", var),
                                                       xaxis = list(title = "Count"),
                                                       yaxis = list(title = var))}
                                            create_interactive_barplot(input$barplot_variable)})
            }
                           
shinyApp(ui, server)
                           