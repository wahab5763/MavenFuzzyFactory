library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(lubridate)
library(sf)
library(fuzzyjoin)
library(leaflet)
library(shinybusy)
library(shinyauthr)
library(ggplot2)
library(dplyr)
source("ga_dashboard/R/get_data_module.R", local=TRUE)
source("ga_dashboard/R/sidebar.R", local=TRUE)



# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c("user1", "user2", "wahab"),
  password = purrr::map_chr(c("pass1", "pass2", "wahab"), sodium::password_store),
  permissions = c("admin", "standard", "admin"),
  name = c("User One", "User Two", "Goat")
)

df_sources_cvr <- readr::read_rds(here::here("ga_dashboard/data/df_sources_cvr_rate"))
df_order_details <- readr::read_rds(here::here("ga_dashboard/data/df_order_details"))
df_ovr_sources_cvr <- readr::read_rds(here::here("ga_dashboard/data/overall_cvr_sources"))
df_sources_cvr_month <- readr::read_rds(here::here("ga_dashboard/data/df_sources_cvr_rate_monthly"))
user_data <- readr::read_rds(here::here("ga_dashboard/data/user_rfm_profit"))
dt_cvr_source <- readr::read_rds(here::here("ga_dashboard/data/dt_cvr_source"))
dt_cvr_content <- readr::read_rds(here::here("ga_dashboard/data/conversion_rate/dt_cvr_content"))
df_content_cvr_monthly <- readr::read_rds(here::here("ga_dashboard/data/conversion_rate/content_monthly_cvr_rate"))
df_ovr_content_cvr <- readr::read_rds(here::here("ga_dashboard/data/conversion_rate/overall_cvr_content"))
df_cvr_content_yearly <- readr::read_rds(here::here("ga_dashboard/data/conversion_rate/df_content_cvr_yearly"))
#Pages
dt_cvr_pages <- readr::read_rds(here::here("ga_dashboard/data/conversion_rate/pages/dt_cvr_pages"))
df_pages_cvr_monthly <- readr::read_rds(here::here("ga_dashboard/data/conversion_rate/pages/df_pages_cvr_monthly"))
df_ovr_pages_cvr <- readr::read_rds(here::here("ga_dashboard/data/conversion_rate/pages/overall_cvr_pages"))
df_cvr_pages_yearly <- readr::read_rds(here::here("ga_dashboard/data/conversion_rate/pages/df_pages_cvr_yearly"))
#referer
dt_cvr_referer <- readr::read_rds(here::here("ga_dashboard/data/conversion_rate/http_referer/dt_cvr_http_referer"))
df_referer_cvr_monthly <- readr::read_rds(here::here("ga_dashboard/data/conversion_rate/http_referer/http_referer_monthly_cvr_rate"))
df_ovr_referer_cvr <- readr::read_rds(here::here("ga_dashboard/data/conversion_rate/http_referer/overall_cvr_http_referer"))
df_cvr_referer_yearly <- readr::read_rds(here::here("ga_dashboard/data/conversion_rate/http_referer/df_http_referer_cvr_yearly"))
#device_type
dt_cvr_device <- readr::read_rds(here::here("ga_dashboard/data/conversion_rate/device_type/dt_cvr_device_type"))
df_device_cvr_monthly <- readr::read_rds(here::here("ga_dashboard/data/conversion_rate/device_type/device_type_monthly_cvr_rate"))
df_ovr_device_cvr <- readr::read_rds(here::here("ga_dashboard/data/conversion_rate/device_type/overall_cvr_device_type"))
df_cvr_device_yearly <- readr::read_rds(here::here("ga_dashboard/data/conversion_rate/device_type/df_device_type_cvr_yearly"))
#campaigns
dt_cvr_camp <- readr::read_rds(here::here("ga_dashboard/data/conversion_rate/campaign/dt_cvr_utm_campaign"))
df_camp_cvr_monthly <- readr::read_rds(here::here("ga_dashboard/data/conversion_rate/campaign/utm_campaign_monthly_cvr_rate"))
df_ovr_camp_cvr <- readr::read_rds(here::here("ga_dashboard/data/conversion_rate/campaign/overall_cvr_utm_campaign"))
df_cvr_camp_yearly <- readr::read_rds(here::here("ga_dashboard/data/conversion_rate/campaign/df_utm_campaign_cvr_yearly"))
#dashboard
df_dash_sale_month <- readr::read_rds(here::here("ga_dashboard/data/df_dash_month_sale"))
#camp-analysis
df_campaign_analysis <- readr::read_rds(here::here("ga_dashboard/data/campaig_analysis/df_campaign_analysis"))
df_cvr_desktop_pre <- readr::read_rds(here::here("ga_dashboard/data/campaig_analysis/cvr_desktop_targeted_campaign_pre_launch"))
df_cvr_desktop_post <- readr::read_rds(here::here("ga_dashboard/data/campaig_analysis/cvr_desktop_targeted_campaign_post_launch"))
df_cvr_pilot_pre <- readr::read_rds(here::here("ga_dashboard/data/campaig_analysis/cvr_pilot_campaign_pre_launch"))
df_cvr_pilot_post <- readr::read_rds(here::here("ga_dashboard/data/campaig_analysis/cvr_pilot_post_launch"))
#Bounce Rate
df_bounce_rate <- readr::read_rds(here::here("ga_dashboard/data/df_bounce_rate"))
df_bounce_rate_sources <- readr::read_rds(here::here("ga_dashboard/data/bounce_rate_sources"))
#Page Views
df_page_views <- readr::read_rds(here::here("ga_dashboard/data/page_views/df_page_views"))
df_page_views_device <- readr::read_rds(here::here("ga_dashboard/data/page_views/page_views_device"))
df_page_views_month <- readr::read_rds(here::here("ga_dashboard/data/page_views/page_views_month"))
#Products
df_overal_products <- readr::read_rds(here::here("ga_dashboard/data/products/df_overal_products"))
df_products_month <- readr::read_rds(here::here("ga_dashboard/data/products/df_overal_products_month"))
df_products_year <- readr::read_rds(here::here("ga_dashboard/data/products/df_overal_products_year"))



#web_data_c <- readr::read_csv(here::here("ga_dashboard/data/search_console.csv"))


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(script = here::here("ga_dashboard/www/scripts.js"), functions = c("remove_plots_date_change")),
  shiny::includeCSS(here::here("ga_dashboard/www/styles.css")),
  
  loginUI(id = "login", user_title = "User Name (wahab)",
          pass_title = "Password (wahab)"),
  br(),
  br(),
  div(
    id = "display_content",
    div(class="full-page",
    data_ui(id = "get-data")),
    br(),
    br(),
    
    sidebar_ui(id = 'sidebar'),
    
    br(),
    br()
  ) %>% shinyjs::hidden(),
  
  shiny::includeScript(here::here("ga_dashboard/www/scripts.js"))
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  updateSelectizeInput(session = session,
    inputId = "customer",
    choices = as.character(unique(user_data$user_id)),
    server = TRUE,
    options= list(maxOptions = 1000)
    #options= list(maxOptions = length(user_data$user_id))
  )

  
#Dashboard value boxes
  
    #creating the valueBoxOutput content
  
  output$n_users <- renderValueBox({
    valueBox(
      formatC(df_order_details$total_users, format="d", big.mark=',')
      ,paste('Total Customers',df_order_details$total_users)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")
    
    
  })
  
  
  
  output$n_orders <- renderValueBox({
    
    valueBox(
      formatC(df_order_details$n_orders, format="d", big.mark=',')
      ,'Total Orders'
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "purple")
    
  })
  
  
  
  output$aov <- renderValueBox({
    
    valueBox(
      formatC(df_order_details$aov, format="d", big.mark=',')
      ,paste('Average Order Value:',round(df_order_details$aov,3))
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "orange")
    
  })
  
  output$repeat_rate <- renderValueBox({
    
    valueBox(
      formatC(df_order_details$repeat_users, format="d", big.mark=',')
      ,paste('Repeat rate:',df_order_details$repeat_percentage)
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "red")
    
  })
  
  output$revenue <- renderValueBox({
    valueBox(
      formatC(df_order_details$net_revenue, format="d", big.mark=',')
      ,paste('Total Net revenue:',df_order_details$net_revenue)
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "aqua")
    
  })
  
  output$refund_orders <- renderValueBox({
    Refund_Rate <- (df_order_details$n_refund_orders/df_order_details$n_orders)*100
    
    valueBox(
      formatC(df_order_details$n_refund_orders, format="d", big.mark=',')
      ,paste('Refund Orders (Refund Rate: ', round(Refund_Rate,2),")")
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "navy")
    
  })
  
  # Create the bar chart
  output$revenue_month <- renderPlotly({
    filteredData <- df_dash_sale_month[df_dash_sale_month$Year == input$yearInput_dash, ]
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    
    plot_ly(filteredData, x = ~Month, y = ~total_sale, type = 'bar',
            text = ~total_sale, textposition = 'auto') %>%
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(title = "Revenue",yaxis_categoryorder = "total descending")
      )
  })
  
  
  
  #For User analysis valueBox
  
  # Reactive function to filter user_data based on the selected user
  selected_user_data <- reactive({
    user_data[user_data$user_id == input$customer, ]
  })
  

  output$t_spent <- renderValueBox({
    selected_value <- selected_user_data()$total_spent
    valueBox(selected_value, "Total Spent",
             color = "green")
  })

  
  
  output$orders_placed <- renderValueBox({
    
    selected_value <- selected_user_data()$n_orders
    valueBox(selected_value, "Total Orders",
             color = "purple")
    
  })
  
  
  
  output$items_purchased <- renderValueBox({
    
    selected_value <- selected_user_data()$c_order_items
    valueBox(selected_value, "Total Items Purchased"
      ,color = "orange")
    
  })
  
  output$date_fp <- renderValueBox({
    
    selected_value <- date(selected_user_data()$date_first_purchase)
    valueBox(selected_value, "Date First Purchase"
      ,color = "red")
    
  })
  
  output$date_lp <- renderValueBox({
    selected_value <- date(selected_user_data()$date_last_purchase)
    valueBox(selected_value, "Date Last Purchase"
      ,color = "aqua")
    
  })
  
  output$median_orders <- renderValueBox({
 
      selected_value <- selected_user_data()$median_days_bw_transactions
      valueBox(selected_value, "Median Days b/w Orders"
      ,color = "navy")
    
  })
  
  
  output$rfm_score <- renderValueBox({
    
    selected_value <- selected_user_data()$RFMScore
    valueBox(selected_value, "RFM Score"
             ,color = "teal")
    
  })
  
  output$rfm_segment <- renderValueBox({
    selected_value <- selected_user_data()$rfm_segment
    valueBox(selected_value, "RFM Segment"
             ,color = "olive")
    
  })
  
  output$profit_type <- renderValueBox({
    
    selected_value <- selected_user_data()$customer_Type
    valueBox(selected_value, paste("Profit generated: ", selected_user_data()$c_profit_generated)
             ,color = "light-blue")
    
  })
  
  
  #Bounce rate
  #creating the valueBoxOutput content
  output$total_sessions <- renderValueBox({
    valueBox(
      formatC(df_bounce_rate$total_sessions, format="d", big.mark=',')
      ,paste('Total Sessions:',df_bounce_rate$total_sessions)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
    
    
  })
  
  
  
  output$single_page_sessions <- renderValueBox({
    
    valueBox(
      formatC(df_bounce_rate$single_page_sessions, format="d", big.mark=',')
      ,paste('Single Page Sessions',df_bounce_rate$single_page_sessions)
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "green")
    
  })
  
  
  
  output$bounce_rate_card <- renderValueBox({
    
    valueBox(
      formatC(df_bounce_rate$bounce_rate+1, format="d", big.mark=',')
      ,paste('Bounce Rate:',df_bounce_rate$bounce_rate)
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")
    
  })
  
  #Chart
  output$bounce_rate_chart <- renderPlotly({
    
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    # Create the bar graph
    plot_ly(data = df_bounce_rate_sources %>% 
              arrange(desc(bounce_rate)),
            x = ~reorder(utm_source, -bounce_rate),
            y = ~bounce_rate,
            type = "bar",color = ~utm_source,
            colors = category_colors) %>%
      layout(
        xaxis = list(title = "Source"),
        yaxis = list(title = "Bounce Rate",
                     yaxis_categoryorder = "total descending")
      ) 
  })
  
  #-------
  
  #Page Views 
  #N times page view
  output$ntime_pv <- renderPlotly({
    df_page_views <- df_page_views[order(df_page_views$n_times_page_views, decreasing = TRUE), ]
    plot_ly(df_page_views, x = ~reorder(pageview_url, -n_times_page_views), y = ~n_times_page_views, type = "bar",
            text = ~n_times_page_views, textposition = 'auto') %>%
      layout(
        title = ~paste("Average Page View per Session",max(round(average_pageview_session,2))),
        xaxis = list(title = "Page View URL"),
        yaxis = list(title = "Count Views")
      )
  })
  #page view by device
  output$pv_device <- renderPlotly({
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    plot_ly(df_page_views_device, x = ~Year, y = ~page_views,  type = "bar",
            text = ~page_views, textposition = 'auto',
            color = ~device_type,
            colors = category_colors) %>%
      layout(
        xaxis = list(title = "Device Type"),
        yaxis = list(title = "Count Page Views", yaxis_categoryorder = "total descending")
      )
  })
  
  #page view by Month
  output$pv_month <- renderPlotly({
    filteredData <- df_page_views_month[df_page_views_month$Year == input$yearInput_pv, ]
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    
    plot_ly(filteredData, x = ~Month, y = ~page_views, type = 'bar',
            color = ~reorder(pageview_url, -page_views),
            colors = category_colors) %>%
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(title = "Page Views", yaxis_categoryorder = "total descending")
      )
  })
  #------
  #---Product Analysis---
  output$over_product <- renderPlotly({
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    
    plot_ly(df_overal_products, x = ~reorder(product_name, -revenue), y = ~revenue, type = "bar",
            text = ~paste("Units",units_sold), textposition = 'auto',
            color = ~reorder(product_name, -revenue),
            colors = category_colors) %>%
      layout(
        xaxis = list(title = "Product Name"),
        yaxis = list(title = "Revenue",
                     range = c(0, 47000))
      )
  })

  output$year_products <- renderPlotly({
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    plot_ly(df_products_year, x = ~Year, y = ~revenue,  type = "bar",
            text = ~paste("Units",units_sold), textposition = 'auto',
            color = ~reorder(product_name, -revenue),
            colors = category_colors) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Revenue",
                     range = c(0, 25000))
      )
  })
  
  #products by Month
  output$month_products <- renderPlotly({
    filteredData <- df_products_month[df_products_month$Year == input$yearInput_product, ]
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    
    plot_ly(filteredData, x = ~Month, y = ~revenue, type = 'bar',
            text = ~paste("Units",units_sold), textposition = 'auto',
            color = ~reorder(product_name, -revenue),
            colors = category_colors) %>%
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(title = "Revenue", yaxis_categoryorder = "total descending",
                     yaxis = list(range = c(40,10000 )))
      )
  })
  
  #-----------
  
  
  
  
  #Campaign_analysis
  selected_camp_data <- reactive({
    df_campaign_analysis[df_campaign_analysis$utm_campaign == input$campaign, ]
  })
  
  output$table <- DT::renderDataTable({
    filtered_data <- selected_camp_data()
    DT::datatable(
      filtered_data,
      options = list(
        dom = 't',
        ordering = FALSE,
        pageLength = 5,
        lengthMenu = c(5, 10, 15),
        language = list(
          info = 'Showing _START_ to _END_ of _TOTAL_ entries',
          paginate = list(
            first = '&laquo;',
            last = '&raquo;',
            previous = '&lsaquo;'
          )
        ),
        rowCallback = JS(
          "function(row, data) {
            $('td', row).css('font-weight', 'bold');
            $('td:eq(0)', row).css('color', '#FF5722');
            $('td:eq(1)', row).css('color', '#3F51B5');
            $('td:eq(2)', row).css('color', '#4CAF50');
          }"
        )
      )
    )
  })
  
  #Chart
  output$chart <- renderPlotly({
    filtered_data <- selected_camp_data()
    
    plot_ly(filtered_data, x = ~utm_source, type = "bar") %>%
      add_trace(y = ~orders, name = "Orders") %>%
      add_trace(y = ~sessions, name = "Sessions") %>%
      layout(
        title = ~paste("Conversion Rate",round(conversion_rate,3)),
        barmode = "group",
        xaxis = list(title = "UTM Source"),
        yaxis = list(title = "Count")
      )
  })
  
  #Pilot pre and post launch
  
  output$pilot_comparison <- renderPlot({
    names(df_cvr_pilot_pre)[names(df_cvr_pilot_pre) == "reve_generated_pre"] <- "revenue"
    names(df_cvr_pilot_post)[names(df_cvr_pilot_post) == "reve_generated_post"] <- "revenue"
    
    combined_data <- rbind(
      data.frame(Campaign = "Pre-Launch", df_cvr_pilot_pre),
      data.frame(Campaign = "Post-Launch", df_cvr_pilot_post)
    )
    ggplot(combined_data, aes(x = Campaign, y = conversion_rate, fill = Campaign)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Campaign", y = "Conversion Rate") +
      ggtitle("Comparison of Conversion Rate") +
      theme_minimal() +
      theme(legend.position = "top")
  })
  # Conversion rate text
  output$conversionRateText <- renderPrint({
    conversion_rate_pre <- df_cvr_pilot_pre$conversion_rate
    conversion_rate_post <- df_cvr_pilot_post$conversion_rate
    
    cat("Pre-Pilot Conversion Rate:", conversion_rate_pre, "\n")
    cat("Post-Pilot Conversion Rate:", conversion_rate_post)
  })
  
  #Desktop_targeted comparison pre and post launch
  output$desktop_comparison <- renderPlot({
    names(df_cvr_desktop_pre)[names(df_cvr_desktop_pre) == "reve_generated_pre"] <- "revenue"
    names(df_cvr_desktop_post)[names(df_cvr_desktop_post) == "reve_generated_post"] <- "revenue"
    
    combined_data <- rbind(
      data.frame(Campaign = "Pre-Launch", df_cvr_desktop_pre),
      data.frame(Campaign = "Post-Launch", df_cvr_desktop_post)
    )
    ggplot(combined_data, aes(x = Campaign, y = conversion_rate, fill = Campaign)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Campaign", y = "Conversion Rate") +
      ggtitle("Comparison of Conversion Rate") +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
  # Conversion rate text
  output$conversionRateText_desk <- renderPrint({
    conversion_rate_pre <- df_cvr_desktop_pre$conversion_rate
    conversion_rate_post <- df_cvr_desktop_post$conversion_rate
    
    cat("Pre-Desktop_Targeted Conversion Rate:", conversion_rate_pre, "\n")
    cat("Post-Desktop_Targeted Conversion Rate:", conversion_rate_post)
  })
  
  
  
#Conversion rates
  
  #Sources Conversion rate
  # Create the bar chart
  output$cr_year <- renderPlotly({
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    # Create the bar graph
    plot_ly(data = df_sources_cvr %>% 
            arrange(desc(conversion_rate)),
            x = ~Year,
            y = ~conversion_rate,
            type = "bar",
            color = ~reorder(utm_source, -conversion_rate),
            colors = category_colors) %>%
      layout(
        xaxis = list(title = "Years"),
        yaxis = list(title = "Conversion Rate")
      ) 
  })
  # Create the bar chart
  output$cr_month <- renderPlotly({
    filteredData <- df_sources_cvr_month[df_sources_cvr_month$Year == input$yearInput, ]
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    
    plot_ly(filteredData, x = ~Month, y = ~conversion_rate, type = 'bar',
            color = ~reorder(utm_source, -conversion_rate),
            colors = category_colors) %>%
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(title = "Conversion Rate",yaxis_categoryorder = "total descending")
      )
  })
  
  
  #Date wise conversion rate
  output$conversionPlot <- renderPlotly({
    selected_date <- input$date  # Get the selected date from the input
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    
    # Arrange the dataframe by date_session_order in ascending order
    arranged_df <- dt_cvr_source[order(dt_cvr_source$date_session_order), ]
    
    # Filter the dataframe based on the selected date
    filtered_df <- arranged_df[arranged_df$date_session_order == selected_date, ]
    
    # Create the Plotly plot
    plot_ly(data = filtered_df, x = ~date_session_order, y = ~conversion_rate,
            color = ~utm_source,
            colors = category_colors,
            type = "scatter",
            mode = "lines+markers") %>%
      layout(title = "Conversion Rate by Date",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Conversion Rate"))
  })
  
  #Average conversion rate sources, pie chart
  # Create plotly pie chart
  
  # Create the bar chart
  output$cr_ovrall <- renderPlotly({
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    cvr_rate <- round(df_ovr_sources_cvr$conversion_rate,2)
    plot_ly(df_ovr_sources_cvr, x = ~reorder(utm_source, -conversion_rate),
            y = ~cvr_rate, type = 'bar',
            text = ~cvr_rate, textposition = 'auto',
            marker = list(color = category_colors,
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) |> 
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(title = "Conversion Rate",yaxis_categoryorder = "total descending")
      )
  })
  #--------------
  
#Content Conversion Rate
  # Create the bar chart
  output$cr_year_content <- renderPlotly({
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    # Create the bar graph
    plot_ly(data = df_cvr_content_yearly %>% 
              arrange(desc(conversion_rate)),
            x = ~Year,
            y = ~conversion_rate,
            type = "bar",color = ~utm_content,
            colors = category_colors) %>%
      layout(
        xaxis = list(title = "Years"),
        yaxis = list(title = "Conversion Rate",yaxis_categoryorder = "total descending")
      ) 
  })
  # Create the bar chart
  output$cr_month_content <- renderPlotly({
    filteredData <- df_content_cvr_monthly[df_content_cvr_monthly$Year == input$yearInput_content, ]
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    
    plot_ly(filteredData, x = ~Month, y = ~conversion_rate, type = 'bar',
            color = ~utm_content,
            colors = category_colors) %>%
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(title = "Conversion Rate",yaxis_categoryorder = "total descending")
      )
  })
  
  
  #Date wise conversion rate
  output$date_plot_content <- renderPlotly({
    selected_date <- input$date_content  # Get the selected date from the input
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    
    # Arrange the dataframe by date_session_order in ascending order
    arranged_df <- dt_cvr_content[order(dt_cvr_content$date_session_order), ]
    
    # Filter the dataframe based on the selected date
    filtered_df <- arranged_df[arranged_df$date_session_order == selected_date, ]
    
    # Create the Plotly plot
    plot_ly(data = filtered_df, x = ~date_session_order, y = ~conversion_rate,
            color = ~utm_content,
            colors = category_colors,
            type = "scatter",
            mode = "lines+markers") %>%
      layout(title = "Conversion Rate by Date",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Conversion Rate"))
  })
  
  #Average conversion rate sources, pie chart
  # Create plotly pie chart
  
  output$cr_ovrall_content <- renderPlotly({
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    # Create the bar graph
    plot_ly(data = df_ovr_content_cvr %>% 
              arrange(desc(conversion_rate)),
            x = ~reorder(utm_content, -conversion_rate),
            y = ~conversion_rate,
            type = "bar",color = ~utm_content,
            colors = category_colors) %>%
      layout(
        xaxis = list(title = "Years"),
        yaxis = list(title = "Conversion Rate",yaxis_categoryorder = "total descending")
      ) 
  })
  
  #----------------
  #pages conversion rate
  # Create the bar chart
  output$cr_year_pages <- renderPlotly({
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    # Create the bar graph
    plot_ly(data = df_cvr_pages_yearly %>% 
              arrange(desc(conversion_rate)),
            x = ~Year,
            y = ~conversion_rate,
            type = "bar",color = ~pageview_url,
            colors = category_colors) %>%
      layout(
        xaxis = list(title = "Years"),
        yaxis = list(title = "Conversion Rate",yaxis_categoryorder = "total descending")
      ) 
  })
  # Create the bar chart
  output$pages_cr_month <- renderPlotly({
    filteredData <- df_pages_cvr_monthly[df_pages_cvr_monthly$Year == input$yearInputPages, ]
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    
    plot_ly(filteredData, x = ~Month, y = ~conversion_rate, type = 'bar',
            color = ~pageview_url,
            colors = category_colors) %>%
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(title = "Conversion Rate",yaxis_categoryorder = "total descending")
      )
  })
  
  
  #Date wise conversion rate
  output$date_plot_pages <- renderPlotly({
    selected_date <- input$date_pages  # Get the selected date from the input
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    
    # Arrange the dataframe by date_session_order in ascending order
    arranged_df <- dt_cvr_pages[order(dt_cvr_pages$date_session_pageview), ]
    
    # Filter the dataframe based on the selected date
    filtered_df <- arranged_df[arranged_df$date_session_pageview == selected_date, ]
    
    # Create the Plotly plot
    plot_ly(data = filtered_df, x = ~date_session_pageview, y = ~conversion_rate,
            color = ~pageview_url,
            colors = category_colors,
            type = "scatter",
            mode = "lines+markers") %>%
      layout(title = "Conversion Rate by Date",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Conversion Rate"))
  })
  

  # Create plotly pie chart
  
  output$cr_ovrall_pages <- renderPlotly({
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    # Create the bar graph
    plot_ly(data = df_ovr_pages_cvr %>% 
              arrange(desc(conversion_rate)),
            x = ~reorder(pageview_url, -conversion_rate),
            y = ~conversion_rate,
            type = "bar",color = ~pageview_url,
            colors = category_colors) %>%
      layout(
        xaxis = list(title = "Years"),
        yaxis = list(title = "Conversion Rate",yaxis_categoryorder = "total descending")
      ) 
  })
  #------------------------pages_end
  #---------referer------
  # Create the bar chart
  output$cr_year_referer <- renderPlotly({
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    # Create the bar graph
    plot_ly(data = df_cvr_referer_yearly %>% 
              arrange(desc(conversion_rate)),
            x = ~Year,
            y = ~conversion_rate,
            type = "bar",color = ~http_referer,
            colors = category_colors) %>%
      layout(
        xaxis = list(title = "Years"),
        yaxis = list(title = "Conversion Rate",yaxis_categoryorder = "total descending")
      ) 
  })
  # Create the bar chart
  output$cr_month_referer <- renderPlotly({
    filteredData <- df_referer_cvr_monthly[df_referer_cvr_monthly$Year == input$yearInput_referer, ]
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    
    plot_ly(filteredData, x = ~Month, y = ~conversion_rate, type = 'bar',
            color = ~http_referer,
            colors = category_colors) %>%
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(title = "Conversion Rate",yaxis_categoryorder = "total descending")
      )
  })
  
  
  #Date wise conversion rate
  output$date_plot_referer <- renderPlotly({
    selected_date <- input$date_referer  # Get the selected date from the input
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    
    # Arrange the dataframe by date_session_order in ascending order
    arranged_df <- dt_cvr_referer[order(dt_cvr_referer$date_session_order), ]
    
    # Filter the dataframe based on the selected date
    filtered_df <- arranged_df[arranged_df$date_session_order == selected_date, ]
    
    # Create the Plotly plot
    plot_ly(data = filtered_df, x = ~date_session_order, y = ~conversion_rate,
            color = ~http_referer,
            colors = category_colors,
            type = "scatter",
            mode = "lines+markers") %>%
      layout(title = "Conversion Rate by Date",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Conversion Rate"))
  })
  
  
  # Create plotly pie chart
  
  output$cr_ovrall_referer <- renderPlotly({
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    # Create the bar graph
    plot_ly(data = df_ovr_referer_cvr %>% 
              arrange(desc(conversion_rate)),
            x = ~reorder(http_referer, -conversion_rate),
            y = ~conversion_rate,
            type = "bar",color = ~http_referer,
            colors = category_colors) %>%
      layout(
        xaxis = list(title = "Years"),
        yaxis = list(title = "Conversion Rate",yaxis_categoryorder = "total descending")
      ) 
  })
  #----------------------
  #---------Device------
  # Create the bar chart
  output$cr_year_device <- renderPlotly({
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    # Create the bar graph
    plot_ly(data = df_cvr_device_yearly %>% 
              arrange(desc(conversion_rate)),
            x = ~Year,
            y = ~conversion_rate,
            type = "bar",color = ~device_type,
            colors = category_colors) %>%
      layout(
        xaxis = list(title = "Years"),
        yaxis = list(title = "Conversion Rate",yaxis_categoryorder = "total descending")
      ) 
  })
  # Create the bar chart
  output$cr_month_device <- renderPlotly({
    filteredData <- df_device_cvr_monthly[df_device_cvr_monthly$Year == input$yearInput_device, ]
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    
    plot_ly(filteredData, x = ~Month, y = ~conversion_rate, type = 'bar',
            color = ~device_type,
            colors = category_colors) %>%
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(title = "Conversion Rate",yaxis_categoryorder = "total descending")
      )
  })
  
  
  #Date wise conversion rate
  output$date_plot_device <- renderPlotly({
    selected_date <- input$date_device  # Get the selected date from the input
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    
    # Arrange the dataframe by date_session_order in ascending order
    arranged_df <- dt_cvr_device[order(dt_cvr_device$date_session_order), ]
    
    # Filter the dataframe based on the selected date
    filtered_df <- arranged_df[arranged_df$date_session_order == selected_date, ]
    
    # Create the Plotly plot
    plot_ly(data = filtered_df, x = ~date_session_order, y = ~conversion_rate,
            color = ~device_type,
            colors = category_colors,
            type = "scatter",
            mode = "lines+markers") %>%
      layout(title = "Conversion Rate by Date",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Conversion Rate"))
  })
  
  
  # Create plotly pie chart
  
  output$cr_ovrall_device <- renderPlotly({
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    # Create the bar graph
    plot_ly(data = df_ovr_device_cvr %>% 
              arrange(desc(conversion_rate)),
            x = ~reorder(device_type, -conversion_rate),
            y = ~conversion_rate,
            type = "bar",color = ~device_type,
            colors = category_colors) %>%
      layout(
        xaxis = list(title = "Years"),
        yaxis = list(title = "Conversion Rate",yaxis_categoryorder = "total descending")
      ) 
  })
  #----------------------
  #---------Campaign------
  # Create the bar chart
  output$cr_year_camp <- renderPlotly({
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    # Create the bar graph
    plot_ly(data = df_cvr_camp_yearly %>% 
              arrange(desc(conversion_rate)),
            x = ~Year,
            y = ~conversion_rate,
            type = "bar",color = ~utm_campaign,
            colors = category_colors) %>%
      layout(
        xaxis = list(title = "Years"),
        yaxis = list(title = "Conversion Rate",yaxis_categoryorder = "total descending")
      ) 
  })
  # Create the bar chart
  output$cr_month_camp <- renderPlotly({
    filteredData <- df_camp_cvr_monthly[df_camp_cvr_monthly$Year == input$yearInput_camp, ]
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    
    plot_ly(filteredData, x = ~Month, y = ~conversion_rate, type = 'bar',
            color = ~utm_campaign,
            colors = category_colors) %>%
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(title = "Conversion Rate",yaxis_categoryorder = "total descending")
      )
  })
  
  
  #Date wise conversion rate
  output$date_plot_camp <- renderPlotly({
    selected_date <- input$date_camp  # Get the selected date from the input
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    
    # Arrange the dataframe by date_session_order in ascending order
    arranged_df <- dt_cvr_camp[order(dt_cvr_camp$date_session_order), ]
    
    # Filter the dataframe based on the selected date
    filtered_df <- arranged_df[arranged_df$date_session_order == selected_date, ]
    
    # Create the Plotly plot
    plot_ly(data = filtered_df, x = ~date_session_order, y = ~conversion_rate,
            color = ~utm_campaign,
            colors = category_colors,
            type = "scatter",
            mode = "lines+markers") %>%
      layout(title = "Conversion Rate by Date",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Conversion Rate"))
  })
  
  
  # Create plotly pie chart
  
  output$cr_ovrall_camp <- renderPlotly({
    category_colors <- c("#1f77b4", "#fd4580", "#97b858", "#ffd700", "#00ced1")
    # Create the bar graph
    plot_ly(data = df_ovr_camp_cvr %>% 
              arrange(desc(conversion_rate)),
            x = ~reorder(utm_campaign, -conversion_rate),
            y = ~conversion_rate,
            type = "bar",color = ~utm_campaign,
            colors = category_colors) %>%
      layout(
        xaxis = list(title = "Years"),
        yaxis = list(title = "Conversion Rate",yaxis_categoryorder = "total descending")
      ) 
  })
  #----------------------
  
  
  
  
  # call login module supplying data frame,
  # user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  shiny::observe({
    req(credentials()$user_auth)
    shinyjs::show(id = "display_content")
  })
  
  shiny::observe({
    req(!credentials()$user_auth)
    shinyjs::hide(id = "display_content")
  })
  

  df <- data_server(
    id          = 'get-data',
    auth        = shiny::reactive(credentials()$user_auth)
  )
  

  sidebar_server(id = "sidebar")
  
}

# Run the application
shinyApp(ui = ui, server = server)