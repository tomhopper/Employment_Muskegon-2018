
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
#library(rvest)
#library(readxl)
library(blscrapeR)
library(dplyr)
library(purrr)
library(tidyr)

library(lubridate)
library(scales)
library(ggvis)

# Read the data from the website
# LAUMT263474000000003
# unem_df <- read_html("http://data.bls.gov/timeseries/LAUMT263474000000003?data_tool=XGtable") %>%
#   html_node(".regular-data") %>%
#   html_table(trim = TRUE)
unem_df <- bls_api(seriesid = "LAUMT263474000000003", startyear = 1980, endyear = 2000, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  bind_rows(bls_api(seriesid = "LAUMT263474000000003", startyear = 2000, endyear = 2018, registrationKey = Sys.getenv("BLS_KEY"))) %>% 
  dateCast() %>% 
  unnest() %>% 
  arrange(date) %>% 
  mutate(value = value / 100)

# Read in the downloaded data
# TODO: download the data directly from the website
# unem_df <- read_excel("data-raw/SeriesReport-20170220100534_f21a6f.xlsx", skip = 9)

# Strip the "P : Preliminary" line from the table
# unem_df <- as.data.frame(sapply(unem_df, function(x) gsub("P : Preliminary", NA, x)))
# # Delete rows containing all NA
# unem_df <- unem_df[rowSums(is.na(unem_df)) != ncol(unem_df), ]
# unem_df <- as.data.frame(sapply(unem_df, function(x) gsub("(P)", "", x, fixed = TRUE)))
# unem_df <- as.data.frame(sapply(unem_df, function(x) gsub("(R)", "", x, fixed = TRUE)))
# for(i in c(1,3,4,5,6))
#   unem_df[,i] <- as.numeric(as.character(unem_df[,i]))
# 
# # Tidy data
# unem_rate_df <- unem_df %>% 
#   select(Year, month = Period, rate = ends_with("rate")) %>% 
#   mutate(rate = rate / 100)
# 
# # Prep months look-up
# months <- list(jan=1,feb=2,mar=3,apr=4,may=5,jun=6,jul=7,aug=8,sep=9,oct=10,nov=11,dec=12)
# Months <- list(January=1,February=2,March=3,April=4,May=5,June=6,July=7,August=8,September=9,October=10,November=11,December=12)
# 
# # Convert month abbreviation & year columns to a date column. Key off of 
# # TODO: key off of the end of the month
# unem_rate_df <- unem_rate_df %>% mutate(date = ymd(paste0(as.character(unem_rate_df$Year), "-", as.character( sapply(unem_rate_df$month, function(x) months[[tolower(x)]])), "-", "1"))) %>% 
#   dplyr::arrange(date)

# Convert unemployment rate to time series and decompose into components
rate_ts <- ts(unem_df$value, frequency = 12) #,start = 0, end = 10, frequency = 12
rate_decomp <- decompose(rate_ts)

# Save time series decomposition to a data frame and make tidy
ts_df <- data_frame(date = unem_df$date, 
                    observed = rate_decomp$x,
                    adjusted = rate_decomp$trend,
                    seasonal = rate_decomp$seasonal,
                    random = rate_decomp$random,
                    date_dec = decimal_date(date)) %>% 
  gather(component, rate, -date, -date_dec)

# Convert character "component" to a factor and order the levels for plotting
ts_df$component <- factor(ts_df$component, levels = c("observed", "adjusted", "seasonal", "random"))

# Pull out just recent data from the trend to use for prediction 
# (we want seasonally-adjusted value matching today's date)
recent_date <- last(ts_df$date[ts_df$component == "observed"]) - months(6)
ts_recent_df <- ts_df %>% 
  filter(date >= recent_date) %>% 
  na.omit()
# Check by plotting
# ggplot(ts_recent_df %>% filter(component == "adjusted") %>% na.omit(), aes(x = date, y = rate)) +
#   geom_line() +
#   geom_smooth(method = "lm")

# Get the trend over the recent data
ts_lm <- lm(data = (ts_recent_df %>% filter(component == "adjusted") %>% na.omit()), formula = rate ~ date_dec)#, weights = 0.1^(seq(10, 1, by = -1)))
# Create a data frame with the trend line
ts_newdata_df <- data_frame(date = seq(max(ts_recent_df$date[ts_recent_df$component == "adjusted"]), max(ts_recent_df$date[ts_recent_df$component == "observed"]), by = "month"),
                            date_dec = decimal_date(date),
                            component = "adjusted",
                            rate = date_dec * coef(ts_lm)[2] + coef(ts_lm)[1])
last_six_date <- last(ts_df$date[ts_df$component == "observed"]) - months(9)
ts_newdata_df <- bind_rows(ts_df, ts_newdata_df %>% 
                             filter(date > as.Date(last_six_date))) %>%
  na.omit()
ts_newdata_df$component <- factor(ts_newdata_df$component, levels = c("observed","adjusted","seasonal","random"))

ann_text_df <- data_frame(date = max(ts_newdata_df$date[ts_newdata_df$component == "adjusted"]),
                          rate = last(ts_newdata_df$rate[ts_newdata_df$component == "adjusted"]),
                          component = factor("adjusted", levels = c("observed","adjusted","seasonal","random")),
                          year = year(date),
                          month = month(date))

shinyServer(function(input, output) {
  output$employmentValue <- renderText({
    
    paste0("<table style=\"border-collapse: separate; border-spacing: 10px; font-size: 125%\"><tr><td>Unemployment rate, ", unem_df$periodName[nrow(unem_df)]," ", unem_df$year[nrow(unem_df)],":</td><td>", percent(round(last(ts_df$rate[ts_df$component == "observed"]), 3)),"</td></tr>",
           "<tr><td>Estimated seasonally-adjusted rate:</td><td>", percent(round(ann_text_df$rate[1], 3)), "</td></tr></table>")
    
  })
  
  output$em_plot_ui <- renderUI({
    ggvisOutput("em_plot")
  })
  
  # output$daterange <- renderUI({
  #     dateRangeInput(inputId = "daterange",
  #                    label = "Select data range",
  #                    start = min(ts_newdata_df$date),
  #                    end = max(ts_newdata_df$date),
  #                    min = min(ts_newdata_df$date),
  #                    max = max(ts_newdata_df$date)
  #     )
  # })

  plot_data <- reactive({
    my_df <- ts_newdata_df %>% 
      filter(component == input$component) %>% 
      na.omit()
  })
  values_tt <- reactive({
    function(x) {
      if(is.null(x)) return(NULL)
      else {
        the_date <- as.Date(x$date/86400000, origin = "1970-01-01")
        return(paste0(month.abb[month(the_date)], " ", as.character(year(the_date)), ": ", percent(round(x$rate, 3))))
      }
    }
  })
  ggv_title <- reactive({
    switch (input$component,
            observed = "Observed Unemployment",
            adjusted = "Seasonally Adjusted Unemployment",
            seasonal = "Seasonal Variation in Unemployment",
            random = "Residual Variation in Unemployment"
    )
  }) 
  
  output$graph_title <- renderUI({
    h3(ggv_title())
    
  })
  plot_data %>% 
    ggvis(x = ~date, y = ~rate, key:= ~date) %>% 
    layer_lines() %>% 
    layer_points(fillOpacity := 0.5) %>% 
    scale_datetime("x") %>% 
    add_axis(type = "y", title_offset = 50, format = ".1%", title = "Unemployment rate") %>% 
    add_tooltip(values_tt(), "hover") %>% 
    bind_shiny("em_plot")
  
  
})
