library(DT)
library(dplyr)
library(readxl)
library(plotly)
library(skimr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(GGally)
library(ggcorrplot)

# read in data
file_path = "../data/master_df.xlsx"
df = read_excel(file_path)

# set up variables
min_trades = 30
z = qnorm(0.975) 
categorical_vars = c("transaction_type", "party", "state", "gender")
numerical_vars = c("trade_price", "price_change_pct", "seniority_years", "excess_return")
pairplot_vars = c("trade_price", "price_change_pct", "seniority_years", "excess_return", "trade_success")
corr_vars = c("trade_price", "price_change_pct", "seniority_years", "excess_return", "trade_success", "party_encoded", 
              "gender_encoded")

# set up helper functions
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

compute_confidence_interval = function(successes, total) {p = successes / total
                                  se = sqrt(p * (1 - p) / total)
                                  lower = max(0, p - z * se) 
                                  upper = min(1, p + z * se)  
                                  return(list(proportion = p, lower_bound = lower, upper_bound = upper))}

# data processing
df = df %>% rowwise() %>%
    mutate(trade_success = define_trade_success(cur_data())) %>%
    ungroup() %>% filter(!is.na(trade_success))  

df = df %>% rowwise() %>%
     mutate(market_success = define_market_success(cur_data())) %>%
     ungroup() %>% filter(!is.na(market_success)) 

df = df %>% mutate(party_encoded = as.numeric(as.factor(party)), gender_encoded = as.numeric(as.factor(gender)))
df = df %>% mutate(num_committees = rowSums(select(., starts_with("committee_")), na.rm = TRUE))
df = df %>% mutate(trade_date = as.Date(trade_date))  
df = df %>% mutate(trade_year = format(trade_date, "%Y")) 
df = df %>% mutate(trade_date = as.Date(trade_date),        
                   trade_year = format(trade_date, "%Y")) 

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

if (total_trades > 0) {
        ci = compute_confidence_interval(successes, total_trades)
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
committee_success_rates = committee_success_rates %>% mutate(bubble_size = total_trades / max_trades * 1000) 

filtered_committee_df = committee_success_rates %>% filter(total_trades >= min_trades)

sector_success_rates = df %>% filter(!is.na(sector)) %>%
                       group_by(sector) %>% summarise(total_trades = n(),
                                                      successes = sum(trade_success, na.rm = TRUE),
                                                      .groups = "drop") %>%
                       rowwise() %>% mutate(ci = list(compute_confidence_interval(successes, total_trades)),
                                            success_rate = ci$proportion, lower_ci = ci$lower_bound, 
                                            upper_ci = ci$upper_bound) %>%
                       unnest_wider(ci) %>% filter(total_trades >= min_trades) 

party_success = df %>% group_by(party) %>% 
                summarise(average_success_rate = mean(trade_success, na.rm = TRUE), .groups = "drop")

pairplot_data = df %>% select(all_of(pairplot_vars)) %>% mutate(trade_success = as.factor(trade_success)) 

subset_df = df %>% select(all_of(corr_vars))
corr_matrix = cor(subset_df, use = "complete.obs")

committee_columns = grep("committee_", names(df), value = TRUE)
committee_df = df %>% select(all_of(committee_columns))
committee_corr = cor(committee_df, use = "complete.obs")
colnames(committee_corr) = abbreviate(colnames(committee_corr), minlength = 15)
rownames(committee_corr) = abbreviate(rownames(committee_corr), minlength = 15)

success_by_committees = df %>% group_by(num_committees) %>%
                        summarise(average_success_rate = mean(trade_success, na.rm = TRUE),
                                  .groups = "drop")

trades_per_year = df %>% group_by(trade_year) %>% summarise(num_trades = n(), .groups = "drop")
success_per_year = df %>% group_by(trade_year) %>%
                   summarise(avg_success_rate = mean(trade_success, na.rm = TRUE), .groups = "drop")

sector_counts = df %>% group_by(sector) %>% summarise(num_trades = n(), .groups = "drop")
sector_success = df %>% group_by(sector) %>%
                 summarise(avg_success_rate = mean(trade_success, na.rm = TRUE), .groups = "drop")

rank_success = df %>% group_by(is_chair_or_vice) %>%
               summarise(avg_success_rate = mean(trade_success, na.rm = TRUE),  .groups = "drop") %>%
               mutate(Rank = ifelse(is_chair_or_vice == 1, "Chair/Vice Chair", "Member")) 

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
                             h3("Number of Trades Over Time"),
                             plotOutput("trades_over_time", height = "600px"),
                             hr(),
                             h3("Success Rate Over Time"),
                             plotOutput("success_rate_over_time", height = "600px"),
                             hr(),
                             h3("Success Rate by Number of Committee Memberships"),
                             plotOutput("success_by_committee", height = "600px"),
                             hr(),
                             h3("Success Rate by Committee with Confidence Intervals"),
                             plotOutput("committee_bar_chart", height = "600px"),
                             hr(),
                             h3("Committee Success Rate Bubble Chart"),
                             plotOutput("committee_bubble_chart", height = "600px"),
                             hr(),
                             h3("Success Rate by Committee (Minimum 30 Trades)"),
                             plotOutput("filtered_committee_plot", height = "600px"),
                             hr(),
                             h3("Success Rate by Sector with Confidence Intervals"),
                             plotOutput("sector_success_plot", height = "600px"),
                             hr(),
                             h3("Success vs. Seniority Years"),
                             plotOutput("success_vs_seniority_jitter", height = "600px"),
                             hr(),
                             h3("Seniority Years by Success"),
                             plotOutput("success_vs_seniority_boxplot", height = "600px"),
                             hr(),
                             h3("Success Rate by Party"),
                             plotOutput("party_success_plot", height = "600px"),
                             hr(),
                             h3("Number of Trades by Sector"),
                             plotOutput("num_trades_by_sector", height = "600px"),
                             hr(),
                             h3("Success Rate by Sector"),
                             plotOutput("success_by_sector", height = "600px"),
                             hr(),
                             h3("Success Rate by Rank")),
                    tabPanel("Pair and Correlation Visualizations",
                             h3("Pair Plot by Trade Success"),
                             plotOutput("pair_plot", height = "600px"),
                             hr(),
                             h3("Correlation Matrix"),
                             plotOutput("correlation_matrix_plot", height = "600px"),
                             hr(),
                             h3("Correlation Matrix of Committee Memberships"),
                             plotOutput("committee_correlation_plot", height = "600px"))
                    ))

# set up back-end 
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
            
            output$filtered_committee_plot = renderPlot({ggplot(filtered_committee_df, 
                                                                aes(x = success_rate, y = reorder(committee, success_rate))) +
                                                         geom_bar(stat = "identity", fill = "skyblue", width = 0.8) +
                                                         labs(title = paste("Success Rate by Committee (Minimum", min_trades, "Trades)"),
                                                              x = "Success Rate", y = "Committee") +
                                                         theme_minimal() + 
                                                         theme(plot.title = element_text(hjust = 0.5, size = 16, 
                                                                                         margin = margin(b = 10)),
                                                               axis.text = element_text(size = 10),
                                                               axis.title = element_text(size = 12),
                                                               plot.margin = margin(t = 20, r = 20, b = 20, l = 20))})
            
            output$sector_success_plot = renderPlot({ggplot(sector_success_rates, 
                                                            aes(x = success_rate, y = reorder(sector, success_rate))) +
                                                     geom_bar(stat = "identity", fill = "skyblue", width = 0.8) +
                                                     geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), 
                                                                   width = 0.2, color = "black") +
                                                     geom_text(aes(label = paste0(total_trades, " trades"), 
                                                                   x = success_rate + 0.01), hjust = 0, size = 3) +
                                                     labs(title = "Success Rate by Sector with Confidence Intervals",
                                                          x = "Success Rate", y = "Sector") +
                                                     theme_minimal() +
                                                     theme(plot.title = element_text(hjust = 0.5, size = 16),
                                                           axis.text = element_text(size = 10),
                                                           axis.title = element_text(size = 12),
                                                           plot.margin = margin(t = 20, r = 20, b = 20, l = 20))})
            
            output$success_vs_seniority_jitter = renderPlot({ggplot(df, aes(x = seniority_years, y = trade_success)) +
                                                             geom_jitter(width = 0.2, height = 0.05, 
                                                                         color = "blue", alpha = 0.6) +
                                                             labs(title = "Success vs. Seniority Years", x = "Seniority (Years)", 
                                                                  y = "Success (1 = Successful Trade)") + theme_minimal() +
                                                             theme(plot.title = element_text(hjust = 0.5, size = 14),
                                                                   axis.text = element_text(size = 10),
                                                                   axis.title = element_text(size = 12))})
            
            output$success_vs_seniority_boxplot = renderPlot({ggplot(df, aes(x = factor(trade_success), y = seniority_years)) +
                                                              geom_boxplot(fill = "skyblue", color = "black", width = 0.5) +
                                                              labs(title = "Seniority Years by Success",
                                                                   x = "Success (0 = Unsuccessful, 1 = Successful)",
                                                                   y = "Seniority (Years)") + theme_minimal() +
                                                              theme(plot.title = element_text(hjust = 0.5, size = 14),
                                                                    axis.text = element_text(size = 10),
                                                                    axis.title = element_text(size = 12))})
            
            output$party_success_plot = renderPlot({ggplot(party_success, aes(x = party, y = average_success_rate)) +
                                                    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
                                                    labs(title = "Success Rate by Party", x = "Party", 
                                                         y = "Average Success Rate") + theme_minimal() +
                                                    theme(plot.title = element_text(hjust = 0.5, size = 14),
                                                          axis.text = element_text(size = 10),
                                                          axis.title = element_text(size = 12))})
            
            output$pair_plot = renderPlot({ggpairs(pairplot_data, aes(color = trade_success, alpha = 0.6),
                                                   lower = list(continuous = wrap("points", alpha = 0.4)),
                                                   upper = list(continuous = wrap("cor", size = 4)),
                                                   diag = list(continuous = wrap("densityDiag", alpha = 0.4))) +
                                           theme_minimal() + theme(legend.position = "bottom",
                                                                   plot.title = element_text(hjust = 0.5, size = 14)) +
                                           labs(title = "Pair Plot of Selected Variables by Trade Success")})
            
            output$correlation_matrix_plot = renderPlot({ggcorrplot(corr_matrix, method = "square", type = "full",
                                                                    lab = TRUE, lab_size = 4, 
                                                                    colors = c("red", "white", "blue"),
                                                                    title = "Correlation Matrix",
                                                                    legend.title = "Correlation") +
                                                         theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 16),
                                                                                 legend.position = "right",
                                                                                 axis.text.x = element_text(angle = 45, hjust = 1))})
            
            output$committee_correlation_plot = renderPlot({ggcorrplot(committee_corr, method = "square", type = "full",
                                                                       lab = FALSE, colors = c("red", "white", "blue"),
                                                                       title = "Correlation Matrix of Committee Memberships",
                                                                       legend.title = "Correlation") +
                                                            theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 16),
                                                                                    legend.position = "right",
                                                                                    axis.text.x = element_text(size = 8, 
                                                                                                               angle = 90, 
                                                                                                               hjust = 1),
                                                                                    axis.text.y = element_text(size = 8))})
            output$success_by_committee = renderPlot({ggplot(success_by_committees, 
                                                             aes(x = num_committees, y = average_success_rate)) +
                                                      geom_bar(stat = "identity", fill = "skyblue", color = "black") +
                                                      labs(title = "Success Rate by Number of Committee Memberships",
                                                           x = "Number of Committees", y = "Average Success Rate") +
                                                      theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 16),
                                                                              axis.text = element_text(size = 10),
                                                                              axis.title = element_text(size = 12))})
            
            output$trades_over_time = renderPlot({ggplot(trades_per_year, 
                                                         aes(x = as.numeric(trade_year), y = num_trades)) +
                                                  geom_line(group = 1, color = "blue", size = 1) +
                                                  geom_point(color = "red", size = 2) +
                                                  labs(title = "Number of Trades Over Time",
                                                       x = "Year", y = "Number of Trades") +
                                                  theme_minimal() +
                                                  theme(plot.title = element_text(hjust = 0.5, size = 16),
                                                        axis.text = element_text(size = 10),
                                                        axis.title = element_text(size = 12))})
            
            output$success_rate_over_time = renderPlot({ggplot(success_per_year, 
                                                               aes(x = as.numeric(trade_year), y = avg_success_rate)) +
                                                        geom_line(group = 1, color = "blue", size = 1) +
                                                        geom_point(color = "red", size = 2) +
                                                        labs(title = "Success Rate Over Time", x = "Year",
                                                             y = "Average Success Rate") +
                                                        theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 16),
                                                                                axis.text = element_text(size = 10),
                                                                                axis.title = element_text(size = 12))})
            
            output$success_by_sector = renderPlot({ggplot(sector_success, 
                                                          aes(x = avg_success_rate, y = reorder(sector, avg_success_rate))) +
                                                   geom_bar(stat = "identity", fill = "darkorange", color = "black") +
                                                   labs(title = "Success Rate by Sector", x = "Average Success Rate",
                                                        y = "Sector") + theme_minimal() +
                                                   theme(plot.title = element_text(hjust = 0.5, size = 16),
                                                         axis.text = element_text(size = 10),
                                                         axis.title = element_text(size = 12))})
            
            output$num_trades_by_sector = renderPlot({ggplot(sector_counts, 
                                                             aes(x = num_trades, y = reorder(sector, num_trades))) +
                                                      geom_bar(stat = "identity", fill = "purple", color = "black") +
                                                      labs(title = "Number of Trades by Sector", x = "Number of Trades",
                                                           y = "Sector") + theme_minimal() +
                                                      theme(plot.title = element_text(hjust = 0.5, size = 16),
                                                            axis.text = element_text(size = 10),
                                                            axis.title = element_text(size = 12))})
            
            output$success_by_rank = renderPlot({ggplot(rank_success, aes(x = Rank, y = avg_success_rate)) +
                                                 geom_bar(stat = "identity", fill = "skyblue", color = "black") +
                                                 labs(title = "Success Rate by Rank", x = "Rank", 
                                                      y = "Average Success Rate") + theme_minimal() +
                                                 theme(plot.title = element_text(hjust = 0.5, size = 16),
                                                       axis.text = element_text(size = 8),
                                                       axis.title = element_text(size = 8))})
            }

# run app                        
shinyApp(ui, server)
                           