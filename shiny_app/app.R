library(shiny)
library(DT)
library(dplyr)
library(readxl)
library(skimr)
library(ggplot2)
library(gridExtra)
library(plotly)

# read in data
file_path = "../data/master_df.xlsx"
df = read_excel(file_path)

# data processing
numerical_vars = c("trade_price", "price_change_pct", "seniority_years", "excess_return")
categorical_vars = c("transaction_type", "party", "state", "gender")

z = qnorm(0.975) 

define_trade_success = function(row) {
  if (row$transaction_type %in% c("Purchase")) {
    return(ifelse(row$price_change_pct > 0, 1, 0))} 
  else if (row$transaction_type %in% c("Sale", "Sale (Partial)", "Sale (Full)")) {
    return(ifelse(row$price_change_pct < 0, 1, 0))}                       
  else {return(NA)}}

define_market_success = function(row) {
  if (row$transaction_type %in% c("Purchase")) {
    return(ifelse(row$excess_return > 0, 1, 0))} 
  else if (row$transaction_type %in% c("Sale", "Sale (Partial)", "Sale (Full)")) {
    return(ifelse(row$excess_return < 0 & row$price_change_pct < 0, 1, 0))} 
  else {return(NA)}}

compute_confidence_interval = function(successes, total) {
  p = successes / total
  se = sqrt(p * (1 - p) / total)
  lower = max(0, p - z * se) 
  upper = min(1, p + z * se)  
  return(list(proportion = p, lower_bound = lower, upper_bound = upper))}

df = df %>%
  rowwise() %>%
  mutate(trade_success = define_trade_success(cur_data())) %>%
  ungroup() %>%
  filter(!is.na(trade_success))  

df = df %>%
  rowwise() %>%
  mutate(market_success = define_market_success(cur_data())) %>%
  ungroup() %>%
  filter(!is.na(market_success)) 

committee_columns = grep("committee_", colnames(df), value = TRUE)
committee_success_rates = data.frame(committee = character(),
                                     success_rate = numeric(),
                                     lower_ci = numeric(),
                                     upper_ci = numeric(),
                                     total_trades = integer(),
                                     stringsAsFactors = FALSE)

for (committee in committee_columns) {committee_name = gsub("committee_", "", committee)  
        committee_data = df %>% filter(.data[[committee]] == 1)
        total_trades = nrow(committee_data)
        successes = nrow(committee_data %>% filter(trade_success == 1))
        
        if (total_trades > 0) {ci = compute_confidence_interval(successes, total_trades)
                               committee_success_rates = rbind(committee_success_rates,
                               data.frame(committee = committee_name,
                                          success_rate = ci$proportion,
                                          lower_ci = ci$lower_bound,
                                          upper_ci = ci$upper_bound,
                                          total_trades = total_trades))} 
        else {committee_success_rates = rbind(committee_success_rates,
                                              data.frame(committee = committee_name,
                                                         success_rate = NA,
                                                         lower_ci = NA,
                                                         upper_ci = NA,
                                                         total_trades = total_trades))}}

committee_success_rates = committee_success_rates %>% filter(!is.na(success_rate))
committee_success_rates = committee_success_rates %>% arrange(desc(success_rate))

max_trades = max(committee_success_rates$total_trades, na.rm = TRUE)
             committee_success_rates = committee_success_rates %>%
             mutate(bubble_size = total_trades / max_trades * 1000)  

# set up UI
ui = fluidPage(titlePanel("Congressional Trade Analysis"),
        tabsetPanel(tabPanel("Raw Data", DTOutput("data_table")),
                    tabPanel("Summary Statistics", DTOutput("skim_table")),
                    tabPanel("Visualizations for Distributions", 
                             h3("Histograms for Numeric Variables"),
                             plotOutput("static_histograms", height = "600px"),
                             hr(),
                             h3("Bar Plots for Categorical Variables"),
                             selectInput("barplot_variable", 
                                         "Select Categorical Variable:", 
                                          choices = categorical_vars),
                             plotlyOutput("interactive_barplot"),
                             hr(),
                             h3("Trade and Market Success Distributions"),
                             plotOutput("success_plots", height = "600px")),
                    tabPanel("Success Rate Visualizations",
                             h3("Committee Success Rate  Bar Chart with Confidence Intervals"),
                             plotOutput("committee_bar_chart", height = "600px"),
                             hr(),
                             h3("Committee Success Rate Bubble Chart"),
                             plotOutput("committee_bubble_chart", height = "600px"))
                    ))

# set up backend 
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
            
            output$success_plots = renderPlot({
                                      success_colors = c("0" = "red", "1" = "green")
                                      
                                      success_plot = ggplot(df, aes(x = factor(trade_success), fill = factor(trade_success))) +
                                                     geom_bar() +
                                                     scale_fill_manual(values = success_colors, 
                                                                       labels = c("0: Unsuccessful", "1: Successful")) +
                                                     labs(title = "Distribution of Trade Success Outcomes", 
                                                          x = "Trade Success", y = "Count", fill = "Outcome") +
                                                     theme(plot.title = element_text(hjust = 0.5, size = 14), 
                                                           axis.text = element_text(size = 10), 
                                                           axis.title = element_text(size = 12), 
                                                           legend.position = "top")
                                      
                                      market_success_plot = ggplot(df, aes(x = factor(market_success), 
                                                                           fill = factor(market_success))) +
                                                            geom_bar() +
                                                            scale_fill_manual(values = success_colors, 
                                                                              labels = c("0: Unsuccessful", "1: Successful")) +
                                                            labs(title = "Distribution of Market Success Outcomes", 
                                                                 x = "Market Success", y = "Count", fill = "Outcome") +
                                                            theme(plot.title = element_text(hjust = 0.5, size = 14), 
                                                                  axis.text = element_text(size = 10), 
                                                                  axis.title = element_text(size = 12), 
                                                                  legend.position = "top")
              
                                                            grid.arrange(success_plot, market_success_plot, ncol = 2)})
            
            output$committee_bar_chart = renderPlot({ggplot(committee_success_rates, 
                                                            aes(x = success_rate, y = reorder(committee, success_rate))) +
                                                     geom_bar(stat = "identity", fill = "skyblue", width = 0.8) +
                                                     geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), 
                                                                   width = 0.2, color = "black") +
                                                     labs(title = "Success Rate by Committee with Confidence Intervals",
                                                          x = "Success Rate", y = "Committee") +
                                                     theme_minimal() +
                                                     theme(plot.title = element_text(hjust = 0.5, size = 16, 
                                                                                     margin = margin(b = 10)),
                                                           axis.text = element_text(size = 10),
                                                           axis.title = element_text(size = 12))})
            
            output$committee_bubble_chart = renderPlot({ggplot(committee_success_rates, 
                                                               aes(x = success_rate, 
                                                                   y = reorder(committee, success_rate), 
                                                                   size = bubble_size)) +
                                                        geom_point(alpha = 0.6, color = "steelblue") +
                                                        scale_size_continuous(range = c(3, 20),
                                                                              name = "Number of Trades",
                                                                              breaks = c(100, 500, 1000),
                                                                              labels = c("100 trades", "500 trades", "1000 trades")) +
                                                        labs(title = "Success Rate by Committee (Bubble Size = Number of Trades)",
                                                             x = "Success Rate", y = "Committee") +
                                                        theme_minimal() +
                                                        theme(plot.title = element_text(hjust = 0.5, size = 16),
                                                              axis.text = element_text(size = 10),
                                                              axis.title = element_text(size = 12),
                                                              legend.position = "right")})
            }
   
                        
shinyApp(ui, server)
                           