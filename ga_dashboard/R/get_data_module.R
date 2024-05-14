# start ui module
data_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    div(class="full-page",
      ui <- dashboardPage(title = 'Maven Fuzzy Factory', header, sidebar, condition_click, skin='red')
    )
  )
  
}
available_dates <- unique(dt_cvr_source$date_session_order)


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Maven Fuzzy Factory")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dash", icon = icon("dashboard")),
    menuItem("User Details", tabName = "ua", icon = icon("user")),
    menuItem("Bounce Rate", tabName = "br", icon = icon("backward")),
    menuItem("Conversion Rate",tabName ="conversion", icon = icon("chart-line"),
             selected = TRUE,
             menuSubItem("Sources", tabName = "cr_sources", icon = icon("ship")),
             menuSubItem("Campaigns", tabName = "cr_camp", icon = icon("message")),
             menuSubItem("Content", tabName = "cr_content", icon = icon("book")),
             menuSubItem("Device Type", tabName = "cr_device", icon = icon("laptop")),
             menuSubItem("Referer", tabName = "cr_referer", icon = icon("bus")),
             menuSubItem("Product Web-Pages", tabName = "cr_pages", icon = icon("computer"))
    ),
    menuItem("Page Views", tabName = "pv", icon = icon("eye")),
    menuItem("Campaign Analysis", tabName = "ca", icon = icon("message")),
    menuItem("Products Overview", tabName = "pa", icon = icon("box")),
    menuItem(shinyauthr::logoutUI(id = "logout"))
  )
)

frow_dash <- fluidRow(
  valueBoxOutput("n_orders")
  ,valueBoxOutput("n_users")
  ,valueBoxOutput("repeat_rate")
  ,valueBoxOutput("aov")
  ,valueBoxOutput("revenue")
  ,valueBoxOutput("refund_orders")
  ,  box(
    title = "Revenue Per Month"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = 12
    ,selectInput("yearInput_dash", "Select Year", choices = unique(df_dash_sale_month$Year),
                 selected = max(df_dash_sale_month$Year))
    ,plotlyOutput("revenue_month", width = "100%"))
  
)

frow_user <- fluidRow(
  valueBoxOutput("t_spent")
  ,valueBoxOutput("orders_placed")
  ,valueBoxOutput("items_purchased")
  ,valueBoxOutput("date_fp")
  ,valueBoxOutput("date_lp")
  ,valueBoxOutput("median_orders")
  ,valueBoxOutput("rfm_score")
  ,valueBoxOutput("rfm_segment")
  ,valueBoxOutput("profit_type")
)

#Bounce Rate
frow_br <- fluidRow(
  valueBoxOutput("total_sessions")
  ,valueBoxOutput("single_page_sessions")
  ,valueBoxOutput("bounce_rate_card")
  ,  box(
    title = "Bounce rate based on different Sources"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = 12
    ,plotlyOutput("bounce_rate_chart", width = "100%"))
)

#page Views
frow_pv <- fluidRow(
  box(
    title = "Number of Page views"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = 12
    ,plotlyOutput("ntime_pv", height = "300px")
  )
  
  ,box(
    title = "Page Views by Device"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,width = 12
    ,plotlyOutput("pv_device", height = "300px"))

  ,  box(
    title = "Page Views per Month"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = 12
    ,selectInput("yearInput_pv", "Select Year", choices = unique(df_page_views_month$Year),
                 selected = max(df_page_views_month$Year))
    ,plotlyOutput("pv_month", width = "100%"))
  
)

#products analysis
frow_products <- fluidRow(
  box(
    title = "Revenue by Product"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = 12
    ,plotlyOutput("over_product")
  )
  
  ,box(
    title = "Revenue by Product per Year"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,width = 12
    ,plotlyOutput("year_products"))
  
  ,  box(
    title = "Revenue by Product Per Month"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = 12
    ,selectInput("yearInput_product", "Select Year", choices = unique(df_products_month$Year),
                 selected = max(df_products_month$Year))
    ,plotlyOutput("month_products", width = "100%"))
  
)

frow_camp_analysis <- fluidRow(
  column(width = 12,
         box(
           title = "UTM_Campaign by UTM_Source"
           ,status = "primary"
           ,solidHeader = TRUE 
           ,collapsible = TRUE
           , width = "100%"
           ,DT::dataTableOutput("table", width = "100%"),  # Output plotly graph
         )),
  
  
  column(width = 12,
         box(
           title = "Sessions and Orders by UTM_Source"
           ,status = "primary"
           ,solidHeader = TRUE 
           ,collapsible = TRUE
           , width = "100%"
           ,plotlyOutput("chart")))
  
  
)

#For pilot
frow_pilot <- fluidRow( 
  column(width = 12,
  box(
    title = "Pre and post Launch pilot Campaign"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,width = 12
    ,plotOutput("pilot_comparison", height = "400px")
    ,verbatimTextOutput("conversionRateText")))
)  

frow_desktop <- fluidRow( 
  column(width = 12,
  box(
    title = "Pre and post Launch Desktop Campaign"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,width = 12
    ,plotOutput("desktop_comparison", height = "400px")
    ,verbatimTextOutput("conversionRateText_desk"))) 
)



frow_cr_sources <- fluidRow(  
  fluidRow( 
           box(
             title = "Date wise Conversion Rate"
             ,status = "primary"
             ,solidHeader = TRUE 
             ,collapsible = TRUE
             , width = 6
           ,plotlyOutput("conversionPlot"),  # Output plotly graph
    ),
  box(
  title = "Sources: Overall Conversion Rate"
  ,status = "primary"
  ,solidHeader = TRUE 
  ,collapsible = TRUE
  , width = 6
  ,plotlyOutput("cr_ovrall"))),

  box(
    title = "Sources: Conversion Rate per Month"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = 12
    ,selectInput("yearInput", "Select Year", choices = unique(df_sources_cvr_month$Year),
                 selected = max(df_sources_cvr_month$Year))
    ,plotlyOutput("cr_month", width = "100%")),
  
  box(
    title = "Sources: Conversion Rate per Year"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = 12
    ,plotlyOutput("cr_year", width = "100%"))
)

#Conversion rate for Campaigns
frow_cr_camp <- fluidRow(  
  fluidRow( 
    
    column(width = 6,
           box(
             title = "Date wise Conversion Rate"
             ,status = "primary"
             ,solidHeader = TRUE 
             ,collapsible = TRUE
             , width = "100%"
             ,plotlyOutput("date_plot_camp"),  # Output plotly graph
           )),
    
    column(width = 6,
           box(
             title = "Campaigns: Overall Conversion Rate"
             ,status = "primary"
             ,solidHeader = TRUE 
             ,collapsible = TRUE
             , width = "100%"
             ,plotlyOutput("cr_ovrall_camp")))),
  
  box(
    title = "Campaigns: Conversion Rate per Month"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = 12
    ,selectInput("yearInput_camp", "Select Year", choices = unique(df_camp_cvr_monthly$Year),
                 selected = max(df_camp_cvr_monthly$Year))
    ,plotlyOutput("cr_month_camp", width = "100%")),
  
  box(
    title = "Campaigns: Conversion Rate per Year"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = 12
    ,plotlyOutput("cr_year_camp", width = "100%"))
)

#conversion rate for Content
frow_cr_content <- fluidRow(  
  fluidRow( 
    
    column(width = 6,
           box(
             title = "Date wise Conversion Rate"
             ,status = "primary"
             ,solidHeader = TRUE 
             ,collapsible = TRUE
             , width = "100%"
             ,plotlyOutput("date_plot_content"),  # Output plotly graph
           )),
    
    column(width = 6,
           box(
             title = "Content: Overall Conversion Rate"
             ,status = "primary"
             ,solidHeader = TRUE 
             ,collapsible = TRUE
             , width = "100%"
             ,plotlyOutput("cr_ovrall_content")))),
  
  box(
    title = "Content: Conversion Rate per Month"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = 12
    ,selectInput("yearInput_content", "Select Year", choices = unique(df_content_cvr_monthly$Year),
                 selected = max(df_content_cvr_monthly$Year))
    ,plotlyOutput("cr_month_content", width = "100%")),
  
  box(
    title = "Content: Conversion Rate per Year"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = 12
    ,plotlyOutput("cr_year_content", width = "100%"))
)


#conversion rate for Device Type
frow_cr_device <- fluidRow(  
  fluidRow( 
    
    column(width = 6,
           box(
             title = "Date wise Conversion Rate"
             ,status = "primary"
             ,solidHeader = TRUE 
             ,collapsible = TRUE
             , width = "100%"
             ,plotlyOutput("date_plot_device"),  # Output plotly graph
           )),
    
    column(width = 6,
           box(
             title = "Devices: Overall Conversion Rate"
             ,status = "primary"
             ,solidHeader = TRUE 
             ,collapsible = TRUE
             , width = "100%"
             ,plotlyOutput("cr_ovrall_device")))),
  
  box(
    title = "Devices: Conversion Rate per Month"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = 12
    ,selectInput("yearInput_device", "Select Year", choices = unique(df_device_cvr_monthly$Year),
                 selected = max(df_device_cvr_monthly$Year))
    ,plotlyOutput("cr_month_device", width = "100%")),
  
  box(
    title = "Devices: Conversion Rate per Year"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = 12
    ,plotlyOutput("cr_year_device", width = "100%"))
)
#Conversion Rate for HTTP referer
frow_cr_referer <- fluidRow(  
  fluidRow( 
    
    column(width = 6,
           box(
             title = "Referer: Date wise Conversion Rate"
             ,status = "primary"
             ,solidHeader = TRUE 
             ,collapsible = TRUE
             , width = "100%"
             ,plotlyOutput("date_plot_referer"),  # Output plotly graph
           )),
    
    column(width = 6,
           box(
             title = "Referer: Overall Conversion Rate"
             ,status = "primary"
             ,solidHeader = TRUE 
             ,collapsible = TRUE
             , width = "100%"
             ,plotlyOutput("cr_ovrall_referer")))),
  
  box(
    title = "Referer: Conversion Rate per Month"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = 12
    ,selectInput("yearInput_referer", "Select Year", choices = unique(df_referer_cvr_monthly$Year),
                 selected = max(df_referer_cvr_monthly$Year))
    ,plotlyOutput("cr_month_referer", width = "100%")),
  
  box(
    title = "Referer: Conversion Rate per Year"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = 12
    ,plotlyOutput("cr_year_referer", width = "100%"))
)
#Conversion rate for pages
frow_cr_pages <- fluidRow(  
  fluidRow( 
    
    column(width = 6,
           box(
             title = "Date wise Conversion Rate"
             ,status = "primary"
             ,solidHeader = TRUE 
             ,collapsible = TRUE
             , width = "100%"
             ,plotlyOutput("date_plot_pages"),  # Output plotly graph
           )),
    
    column(width = 6,
           box(
             title = "Pages: Overall Conversion Rate"
             ,status = "primary"
             ,solidHeader = TRUE 
             ,collapsible = TRUE
             , width = "100%"
             ,plotlyOutput("cr_ovrall_pages")))),
  
  box(
    title = "Pages: Conversion Rate per Month"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = 12
    ,selectInput("yearInputPages", "Select Year", choices = unique(df_pages_cvr_monthly$Year),
                 selected = max(df_pages_cvr_monthly$Year))
    ,plotlyOutput("pages_cr_month", width = "100%")),
  
  box(
    title = "Pages: Conversion Rate per Year"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = 12
    ,plotlyOutput("cr_year_pages", width = "100%"))
)


#conditional body of dashboard

condition_click <- dashboardBody(
  
  tabItems(
    tabItem(tabName = "dash",
            fluidRow(
              column(width = 12,
                     frow_dash
              )
            )
    ),
    tabItem(tabName = "ua",
            fluidRow(
              column(width = 12,
                     selectizeInput(inputId = "customer", label = "Select User ID: ", choices = NULL),
                     frow_user
              )
            )
    ),
    tabItem(tabName = "br",
            fluidRow(
              column(width = 12,
                     frow_br
              )
            )
    ),
    tabItem(tabName = "pv",
            fluidRow(
              column(width = 12,
                     frow_pv
              )
            )
    ),
    tabItem(tabName = "ca",
            fluidRow(
              column(width = 12,
                     selectInput("campaign", "Select Campaign: ", choices = unique(df_campaign_analysis$utm_campaign),
                                 selected = df_campaign_analysis$utm_campaign[2]),
                     frow_camp_analysis,
                     conditionalPanel(
                       condition = "input.campaign == 'pilot'",
                         fluidRow(
                           column(width = 12,
                         frow_pilot
                       ))
                     ),
                     conditionalPanel(
                       condition = "input.campaign == 'desktop_targeted'",
                         fluidRow(
                           column(width = 12,
                         frow_desktop
                       ))
                     )
              )
            )
    ),
    tabItem(tabName = "pa",
            fluidRow(
              column(width = 12,
                     frow_products
              )
            )
    ),

      tabItem(tabName = "cr_sources",
              fluidRow(
                column(width = 12,
                       h2("Sources Conversion Rate"),
                       dateInput("date", "Select Date:", value = min(dt_cvr_source$date_session_order),min = min(dt_cvr_source$date_session_order), max = max(dt_cvr_source$date_session_order)),
                       frow_cr_sources
                       
                       
                )
              )
      ),
      tabItem(tabName = "cr_camp",
              fluidRow(
                column(width = 12,
                       h2("Campaign Conversion Rate"),
                       dateInput("date_camp", "Select Date:", value = min(dt_cvr_camp$date_session_order),min = min(dt_cvr_camp$date_session_order),
                                 max = max(dt_cvr_camp$date_session_order)),
                       frow_cr_camp
                ))
      ),
      tabItem(tabName = "cr_content",
              
              fluidRow(
                
                column(width = 12,
                       h2("Content Conversion Rate"),
                       dateInput("date_content", "Select Date:", value = min(dt_cvr_content$date_session_order),min = min(dt_cvr_content$date_session_order),
                                 max = max(dt_cvr_content$date_session_order)),
                       frow_cr_content
                       
                       
                )
                
              )
      ),
      tabItem(tabName = "cr_device",
              fluidRow(
                column(width = 12,
                       h2("Devices Conversion Rate"),
                       dateInput("date_device", "Select Date:", value = min(dt_cvr_device$date_session_order),min = min(dt_cvr_device$date_session_order), max = max(dt_cvr_device$date_session_order)),
                       frow_cr_device
                ))
      ),
    tabItem(tabName = "cr_pages",
            fluidRow(
              column(width = 12,
                     h2("Web Pages Conversion Rate"),
            dateInput("date_pages", "Select Date:", value = min(dt_cvr_pages$date_session_pageview),min = min(dt_cvr_pages$date_session_pageview), max = max(dt_cvr_pages$date_session_pageview)),
              frow_cr_pages
              ))
    ),
      tabItem(tabName = "cr_referer",
              h2("Referer Conversion Rate"),
              fluidRow(
                column(width = 12,
                       dateInput("date_referer", "Select Date:", value = min(dt_cvr_referer$date_session_order),min = min(dt_cvr_referer$date_session_order), max = max(dt_cvr_referer$date_session_order)),
                       frow_cr_referer
                ))
      )
    
    
    )
  )
  #---
  







#  start server module
data_server <- function(id, auth) {
  shiny::moduleServer(
    id,
    
    function(input, output, session) {

    
      shinyjs::click(id  = "go")

      return(
        list(
          filter_btn = shiny::reactive(input$go)
        )
      )
    }
  )
}
