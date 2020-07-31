#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(rvest)
library(lubridate)
library(plotly)
library(formattable)
library(bootstraplib)

bs_theme_new(version = "4+3", bootswatch = "sketchy")

pops <- read_csv("countypop.csv") %>%
    mutate(county = gsub("\\.", "", county),
           county = gsub(" County, Minnesota", "", county),
           county = gsub("St Louis", "St. Louis", county))

casedata <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>%
    filter(state == "Minnesota") %>%
    select(date, county, cases) %>%
    left_join(pops, by = "county") %>%
    group_by(county) %>%
    mutate(cases_lag14 = lag(cases, 14),
           totalcases_last14 = cases - cases_lag14) %>%
    ungroup() %>%
    mutate(rate_last14 = totalcases_last14 / pop * 10000,
           school_opening_status = cut(rate_last14,
                                       c(-Inf, 10, 20, 30, 50, Inf),
                                       labels = c("All In-Person",
                                                  "Elem In-Person / HS Hybrid",
                                                  "All Hybrid",
                                                  "Elem Hybrid / HS Distance",
                                                  "All Distance")))

opening_status <- data.frame(x = rep(c(ymd("2020-01-01"), ymd("2030-01-01")), 5),
                             ymin = rep(c(0, 10, 20, 30, 50), 2),
                             ymax = rep(c(10, 20, 30, 50, Inf), 2),
                             school_opening_status = rep(c("All In-Person",
                                                           "Elem In-Person / HS Hybrid",
                                                           "All Hybrid",
                                                           "Elem Hybrid / HS Distance",
                                                           "All Distance"), 2)) %>%
    mutate(                  school_opening_status = factor(school_opening_status,
                                                            levels = c("All In-Person",
                                                                       "Elem In-Person / HS Hybrid",
                                                                       "All Hybrid",
                                                                       "Elem Hybrid / HS Distance",
                                                                       "All Distance")))

latest.day <- max(casedata$date)
last.accurate.day <- latest.day - 7

# Define UI for application that draws a histogram
ui <- fluidPage(
    bootstrap(),
    # Application title
    titlePanel("MN School Opening Statistics"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("county",
                        "Select a Minnesota county",
                        choices = pops$county,
                        selected = "Hennepin"),
            uiOutput("fineprint"),
    ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                column(6,
                       uiOutput("county_info_1")),
                column(6, align = "right",
                       uiOutput("county_info_2"))
            ),
            fluidRow(
           plotOutput("opening")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$opening <- renderPlot({
        casedata %>% filter(county == input$county,
                            date >= "2020-05-01",
                            date <= last.accurate.day) %>%
            ggplot(aes(x = date, y = rate_last14)) + 
            geom_line(show.legend = FALSE) +
            geom_point(show.legend = FALSE, size = 2) +
            scale_color_brewer(palette = "RdYlGn", direction = -1, drop = FALSE, ) +
            geom_ribbon(data = opening_status, aes(x = x,
                                                   ymin = ymin,
                                                   ymax = ymax,
                                                   fill = school_opening_status), 
                        alpha = 0.3,
                        inherit.aes = FALSE) +
            scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
            coord_cartesian(xlim = c(ymd("2020-05-01") - 3, ymd(last.accurate.day) + 3), 
                            ylim = c(0,80),
                            expand = 0) +
            scale_fill_brewer(name = "", palette = "RdYlGn", direction = -1) +
            ylab("Total cases in past 14 days\nper 10,000 population") +
            xlab("Date") +
        theme_minimal(base_size = 16) +
            theme(legend.position = "bottom")

    })
    
    output$county_info_1 <- renderUI({
        countypop <- comma(pops$pop[which(pops$county == input$county)], digits = 0)
        caserate <- casedata$rate_last14[which(casedata$county == input$county &
                                                   casedata$date == last.accurate.day)]
        status <- casedata$school_opening_status[which(casedata$county == input$county & 
                                                           casedata$date == last.accurate.day)]
        
        tagList(
            h1(paste(input$county, "County")),
            h4(paste("Population (2019 estimated):", countypop),
               style = "color:#999999")
        )
    })

    output$county_info_2 <- renderUI({
        countypop <- comma(pops$pop[which(pops$county == input$county)], digits = 0)
        caserate <- casedata$rate_last14[which(casedata$county == input$county &
                                                   casedata$date == last.accurate.day)]
        status <- casedata$school_opening_status[which(casedata$county == input$county & 
                                                           casedata$date == last.accurate.day)]
        
        tagList(
            h3(paste0("As of ", strftime(last.accurate.day, "%b %d, %Y"))),
            h4(paste0("14-day case rate per 10,000: ", round(caserate,1)),
               style = "color:#99CC99"),
            h4(paste0("Guideline Status: ", status),
               style = "color:#9999FF")
            
        )
    })
    
    output$fineprint <- renderUI({
        tagList(
        p(
            HTML("County populations are based on <a href='https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html'>2019 estimates from the US Census</a>." )),
        p(
            HTML("Case numbers are obtained from the <a href='https://github.com/nytimes/covid-19-data'>
            New York Times Coronavirus GitHub data repository</a>." )),
        p(
            paste("While case numbers are available through", strftime(max(casedata$date), "%b %d, %Y"),
                  "we report 14-day totals through", strftime(last.accurate.day, "%b %d, %Y"), "as recent case numbers are more likely to be inaccurate.")
        ),
        p(
            HTML("Source code for this app is available <a href='https://github.com/jwolfson/school_openings_mn'>on my GitHub site</a>.")
        )
    )
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
