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
library(forcats)
library(shinycssloaders)
library(shinyWidgets)

bs_theme_new(version = "4+3", bootswatch = "sketchy")

LAG_DAYS <- 14
POP_DENOM <- 10000
STATUS_CUTOFFS<- c(10, 20, 30, 50)
STATUS_LABELS <- c("All In-Person",
                   "Elem In-Person / HS Hybrid",
                   "All Hybrid",
                   "Elem Hybrid / HS Distance",
                   "All Distance")
MAX_Y <- 80
STATE <- "Minnesota"

pops <- read_csv("countypop_us.csv")

casedata <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>%
    select(date, state, county, cases) %>%
    left_join(pops, by = c("state", "county")) %>%
    group_by(state, county) %>%
    mutate(cases_lag = lag(cases, LAG_DAYS),
           totalcases_last = cases - cases_lag) %>%
    ungroup() %>%
    mutate(rate_last = totalcases_last / pop * POP_DENOM,
           rate_last_ci_upper = POP_DENOM / pop * (totalcases_last + 1.96*sqrt(totalcases_last)),
           rate_last_ci_lower = pmax(0, POP_DENOM / pop * (totalcases_last - 1.96*sqrt(totalcases_last))),
           school_opening_status = cut(rate_last,
                                       c(-Inf, STATUS_CUTOFFS, Inf),
                                       labels = STATUS_LABELS))

casedata_district <- read_csv("district_cases.csv")

opening_status <- data.frame(x = rep(c(ymd("2020-01-01"), ymd("2030-01-01")), each = length(STATUS_LABELS)),
                             ymin = rep(c(0, STATUS_CUTOFFS), 2),
                             ymax = rep(c(STATUS_CUTOFFS, Inf), 2),
                             school_opening_status = rep(STATUS_LABELS, 2)) %>%
    mutate(                  school_opening_status = factor(school_opening_status,
                                                            levels = STATUS_LABELS))


latest.day <- max(casedata$date)
first.day <- latest.day - 90
last.accurate.day <- latest.day

casedata <- casedata %>% mutate(unreliable_data = (date > last.accurate.day))
casedata_district <- casedata_district %>% mutate(unreliable_data = FALSE)

makeCountyPlot <- function(countydata) {
    countydata %>%
        ggplot(aes(x = date)) + 
        geom_line(show.legend = FALSE, aes(y = rate_last, linetype = unreliable_data)) +
#        geom_point(aes(y = rate_last_ci_upper), shape = 18) +
        geom_ribbon(aes(ymin = rate_last_ci_lower, ymax = rate_last_ci_upper), fill = "black", alpha = 0.1) +
        geom_point(show.legend = FALSE, aes(y = rate_last, shape = unreliable_data), size = 2) +
        scale_color_brewer(palette = "RdYlGn", direction = -1, drop = FALSE ) +
        geom_ribbon(data = opening_status, aes(x = x,
                                               ymin = ymin,
                                               ymax = ymax,
                                               fill = school_opening_status), 
                    alpha = 0.3,
                    inherit.aes = FALSE) +
        scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
        scale_shape_manual(values = c(16, NA)) +
        scale_linetype_manual(values = c("solid", "dashed")) +
        coord_cartesian(xlim = c(first.day - 3, latest.day + 3), 
                        ylim = c(0,MAX_Y),
                        expand = 0) +
        scale_fill_brewer(name = "", palette = "RdYlGn", direction = -1) +
        ylab(sprintf("Total cases in past %d days\nper %s population", LAG_DAYS, comma(POP_DENOM, digits = 0))) +
        xlab("Date") +
        theme_minimal(base_size = 16) +
        theme(legend.position = "bottom")
}

makeDistrictPlot <- function(districtdata) {
    districtdata %>%
        mutate(
            school_opening_status = factor(school_opening_status,
                                           levels = STATUS_LABELS)
        ) %>%
        ggplot(aes(x = date,
                   fill = school_opening_status)) +
        # geom_bar(aes(y = rate_last), 
        #          stat = "identity",
        #          alpha = 0.7) +
        #geom_point(aes(y = (rate_last + rate_last_max)/2), size = 1) +
        geom_errorbar(aes(ymin = rate_last,
                          ymax = rate_last_max),
                      width = 0,
                      size = 2) +
        geom_errorbar(aes(ymin = rate_last_ci_lower,
                          ymax = rate_last_ci_upper), 
                      width = 0.5) +
        geom_ribbon(data = opening_status, aes(x = x,
                                               ymin = ymin,
                                               ymax = ymax,
                                               fill = school_opening_status), 
                    alpha = 0.3,
                    inherit.aes = FALSE) +
        scale_fill_brewer(palette = "RdYlGn", 
                          direction = -1, 
                          drop = FALSE,
                          name = "") +
        scale_x_date(breaks = districtdata$date, date_labels = "Week ending\n%b %d") +
        coord_cartesian(xlim = c(min(districtdata$date) - 7, max(districtdata$date) + 7),
                        ylim = c(0, MAX_Y)) +
        ylab(sprintf("Total cases in past %d days\nper %s population", LAG_DAYS, comma(POP_DENOM, digits = 0))) +
        xlab(NULL) +
        theme_minimal(base_size = 16) +
        theme(legend.position = "bottom")
}

makeCountyComparisonPlot <- function(allcounties) {
    allcounties %>% 
        filter(!is.na(rate_last)) %>%
        ggplot(aes(x = county, y = rate_last, fill = school_opening_status)) +
        geom_bar(stat = "identity",  
                 aes(color = highlighted,
                     alpha = highlighted),
                 size = 1) +
        scale_fill_brewer(name = "", palette = "RdYlGn", direction = -1, drop = FALSE, 
                          guide = guide_legend(override.aes = list(alpha = 0.3))) +
        scale_color_manual(values = c(NA, "black"), guide = NULL) +
        scale_alpha_manual(values = c(0.3, 0.7), guide = NULL) +
        ylab(sprintf("Total cases in past %d days\nper %s population", LAG_DAYS, comma(POP_DENOM, digits = 0))) +
        xlab(NULL) +
        theme_minimal(base_size = 16) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,
                                         size = 10),
              panel.grid.major.x = element_blank(),
              legend.position = "bottom") +
        ggtitle("")
}

makeCountryPlot <- function(alldata, bypop) {
    

    if(bypop) {
        alldata %>% filter(!is.na(rate_last)) %>%
        group_by(state, school_opening_status) %>%
        summarise(n_status = sum(pop)) %>%
        ungroup() %>%
        group_by(state) %>%
        mutate(pct_status = n_status / sum(n_status)) %>%
        ungroup() %>%
        mutate(state = fct_reorder(state, pct_status, .fun = first),
               school_opening_status = fct_rev(school_opening_status)) %>% 
        ggplot(aes(x = state, y = pct_status, fill = school_opening_status)) +
        geom_bar(stat = "identity", position = "stack", aes(alpha = (state == STATE)), show.legend = FALSE) + 
        scale_fill_brewer(palette = "RdYlGn", drop = FALSE ) +
        scale_alpha_manual(values = c(0.3, 0.7)) +
        scale_y_continuous(labels = scales::percent) +
        ylab("Percentage of population") +
        xlab(NULL) +
        coord_flip(expand = FALSE, ylim = c(0,1.01)) +
        theme_minimal(base_size = 16) +
        theme(legend.position = "none",
              axis.text.y = element_text(vjust = 0.2),
              panel.grid.major.y = element_blank())
    } else {
        alldata %>% filter(!is.na(rate_last)) %>%
            mutate(state = fct_reorder(state, school_opening_status, function(x) mean(x=="All In-Person", na.rm = TRUE)),
                   school_opening_status = fct_rev(school_opening_status)) %>%
            ggplot(aes(x = state, fill = school_opening_status)) +
            geom_bar(position = "fill", aes(alpha = (state == STATE)), show.legend = FALSE) + 
            scale_fill_brewer(palette = "RdYlGn", drop = FALSE ) +
            scale_alpha_manual(values = c(0.3, 0.7)) +
            scale_y_continuous(labels = scales::percent) +
            ylab("Percentage of counties") +
            xlab(NULL) +
            coord_flip(expand = FALSE, ylim = c(0,1.01)) +
            theme_minimal(base_size = 16) +
            theme(legend.position = "none",
                  axis.text.y = element_text(vjust = 0.2),
                  panel.grid.major.y = element_blank())
    }
    
}

# Define UI for application that draws a histogram
ui <- function(request) {
    fluidPage(
    bootstrap(),
    tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}
                         .fineprint {font-size: 9pt}
                        #sidebar {background-color: #EFF4FF;}"))
    ),
    
    # Application title
    titlePanel(sprintf("%s School Opening Statistics", STATE)),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            id = "sidebar",
            uiOutput("countyselect"),
            #uiOutput("districtselect"),
            div(
            uiOutput("multicheck"),
                style = "color: #333399; font-size: 12pt"
            ),
            uiOutput("countyweights"),
            uiOutput("rescale_message"),
            uiOutput("bookmark"),
            hr(),
            uiOutput("background"),
            uiOutput("fineprint"),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(id = "tab", selected = "county",
                        
                tabPanel(title = "By County", value = "county",
                    fluidRow(
                        column(6,
                               uiOutput("county_info_1")),
                        column(6, align = "right",
                               uiOutput("county_info_2"))
                    ),
                    fluidRow(
                        column(12,
                               withSpinner(plotOutput("opening"), type = 5, color = "#DAE6FF")
                        )
                    ),
                    fluidRow(
                        column(12,
                               h3("County Comparison"),
                               h6("How Minnesota's counties compare"),
                               withSpinner(plotOutput("compare_counties"), type = 5, color = "#DAE6FF")
                        )
                    ),
                    fluidRow(
                        column(12,
                               h2("U.S. Comparison"),
                               h5("How each state's counties stack up using Minnesota's school opening guidance"),
                               withSpinner(plotOutput("compare_us", height = "700px"), type = 5, color = "#DAE6FF")
                        )
                    ),
                    fluidRow(
                        column(12, align = "center",
                               checkboxInput("bypop", span("View by population", style = "font-size:20px"))
                        )
                    )
                ),
                
                tabPanel(title = "By School District", value = "district",
                         fluidRow(
                             column(6,
                                    uiOutput("district_info_1")),
                             column(6, align = "right",
                                    uiOutput("district_info_2"))
                         ),
                         fluidRow(
                             column(12,
                                    withSpinner(plotOutput("opening_district"), type = 5, color = "#DAE6FF"),
                                    p("Inner (thick) bar gives the range of 14-day per 10,000 case totals that are consistent with reported case data (MDH does not report exact case numbers in ZIP codes with <= 5 cases). Outer bars give 95% confidence intervals for this range. For larger school districts where no ZIP codes have <= 5 cases, only the outer bars are visible."),
                             )
                         ))
            )
        )
    )
)
}

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    county_data <- reactive({
        req(input$county)
        
        if(length(input$county) == 1) {
        dat <- casedata %>% filter(state == STATE,
                            county %in% input$county,
                            date >= first.day,
                            date <= latest.day)
        } else { #Multiple counties selected
            orig.weights <- get_weights()
            weights <- orig.weights / sum(orig.weights) # Rescale the weights
           dat <- suppressWarnings(casedata %>% 
                filter(state == STATE, 
                       county %in% input$county,
                       date >= first.day,
                       date <= latest.day) %>%
                group_by(date) %>%
                summarise(
                    rate_last = ifelse(length(rate_last)!= length(weights), 
                                     NA, 
                                     weighted.mean(rate_last, w = weights)),
                    rate_last_SE = ifelse(length(totalcases_last) != length(weights),
                                          NA,
                                          POP_DENOM * sqrt(sum(totalcases_last*weights^2/pop^2)))) %>%
                mutate(rate_last_ci_lower = rate_last - 1.96*rate_last_SE,
                       rate_last_ci_upper = rate_last + 1.96*rate_last_SE,
                       school_opening_status = cut(rate_last,
                                                   c(-Inf, STATUS_CUTOFFS, Inf),
                                                   labels = STATUS_LABELS),
                       county = sprintf("Custom (%s)", paste(input$county, collapse = "/")),
                       pop = NA,
                       unreliable_data = (date > last.accurate.day)))
        }
        dat
    })
    
    district_data <- reactive({
        req(input$district)
        casedata_district %>% filter(name == input$district,
                                     !is.na(rate_last))
    })
    
    get_weights <- reactive({
        req(input$weight_1)
        as.numeric(unlist(lapply(1:length(input$county),
                                 function(i) input[[paste0("weight_", i)]]
                                 )))
    })
    
    current_county_rates <- reactive({
        casedata %>% 
            filter(state == STATE,
                   date == last.accurate.day) %>%
            mutate(county = fct_reorder(county, rate_last),
                   highlighted = (county %in% input$county))
    })
    
    
    output$opening <- renderPlot({
        req(input$county)
        suppressWarnings(county_data() %>% makeCountyPlot())
    })
    
    output$opening_district <- renderPlot({
        req(input$district)
        suppressWarnings(district_data() %>% makeDistrictPlot())
    })
    
    output$compare_counties <- renderPlot({
        req(input$county)
            current_county_rates() %>% makeCountyComparisonPlot()
    })
    
    output$compare_us <- renderPlot({
        req(input$county)
            casedata %>% makeCountryPlot(bypop = input$bypop)
    })
    
    output$countyselect <- renderUI( {
        
        if(is.null(input$tab) | input$tab == "county") {
            if(is.null(input$multicounty)) {
                single <- TRUE
            } else {
                single <- !input$multicounty
            }
            
            multi <- !single
            
            selectInput("county",
                        "Select a county",
                        choices = pops %>% filter(state == STATE) %>% pull(county),
                        selected = pops %>% filter(state == STATE) %>%  arrange(desc(pop)) %>%  slice(1:(1+multi)) %>% pull(county),
                        multiple = multi)
            
        } else {
            selectInput("district",
                        "Select a school district",
                        choices = casedata_district %>% pull(name) %>% unique(),
                        multiple = FALSE)
        }
    })
    
    output$multicheck <- renderUI({
        if(is.null(input$tab) | input$tab == "county") {
            prettySwitch(inputId = "multicounty",
                     label = "Select/combine multiple counties", 
                     value = FALSE, 
                     status = "primary", 
                     fill = TRUE)
        } 
    })
    
    
    output$countyweights <- renderUI( {
        req(input$county)
        if(length(input$county) == 1) {
            return(NULL)
        } else {
            L <- lapply(1:length(input$county),
                   function(i) {
                     numericInput(paste0("weight_", i),
                                  label = paste0("% ", input$county[i]),
                                  value = round(1/length(input$county)*100), 
                                  width = "100px")  
                   })
            return(L)
        }
    })
    
    output$rescale_message <- renderUI( {
        req(input$county)
        
        if(abs(sum(get_weights()) - 100) > 1 & length(input$county) > 1) {
            return(p("Sums of percentages not adding to 100% will be rescaled proportionally"))
        }
    })
    
    output$bookmark <- renderUI( {
        req(input$county)
#        if(length(input$county) > 1) {
            bookmarkButton(title = "Generate a custom URL to return to these inputs at a later time.",
                           style = "background-color: #333399; border-width: 0px")
#        }
    })
    
    output$county_info_1 <- renderUI({
        
        req(input$county)
        
        if(length(input$county) == 1) {
            countypop <- comma(pops %>% filter(state == STATE, county %in% input$county) %>%
                                   pull(pop) %>% first(), digits = 0)
            
            return(tagList(
                h1(paste(input$county, "County")),
                h4(paste("Population (2019 estimated):", countypop),
                   style = "color:#999999")
            ))
            
        } else {
            # orig.weights <- get_weights()
            # weights <- orig.weights / sum(orig.weights)
            # pct <- round(100*weights)
            
            return(tagList(
                h1(sprintf("Custom County (%s)",
                           paste(input$county, collapse = " / "))
                )))
        }
    })
    
    output$district_info_1 <- renderUI({
        req(input$district)
        
        zips <- district_data() %>% pull(zips) %>% first()
        
            return(tagList(
                h1(input$district),
                h4(sprintf("ZIP codes: %s", zips))
            ))
            
    })
    
    output$county_info_2 <- renderUI({
        
        caserate <- county_data() %>% filter(date == last.accurate.day) %>% pull(rate_last)
        status <- county_data() %>% filter(date == last.accurate.day) %>% pull(school_opening_status)

        tagList(
            h3(paste0("As of ", strftime(last.accurate.day, "%B %d, %Y"), ":"),
               style = "color:#999999"),
            h4(paste0(sprintf("%d-day case rate per %s: ", LAG_DAYS, comma(POP_DENOM, digits = 0)), round(caserate,1)),
               style = "color:#99CC99"),
            h4(paste0("Guideline Status: ", status),
               style = "color:#9999FF"),
            
        )
    })
    
    output$district_info_2 <- renderUI({
        
        dd <- district_data()
        
        caserate <- dd %>% filter(date == max(dd$date)) %>% pull(rate_last)
        caserate_max <- dd %>% filter(date == max(dd$date)) %>% pull(rate_last_max)
        status <- dd %>% filter(date == max(dd$date)) %>% pull(school_opening_status)
        
        tagList(
            h3(paste0("As of ", strftime(max(dd$date), "%B %d, %Y"), ":"),
               style = "color:#999999"),
            h4(sprintf("%d-day case rate per %s: %.1f-%.1f", 
                              LAG_DAYS, 
                              comma(POP_DENOM, digits = 0), 
                      round(caserate,1),
                      round(caserate_max, 1)),
               style = "color:#99CC99")
        )
    })
    
    output$background <- renderUI({

    if(input$tab == "county") {
        tagList(
            h4("Background"),
            p("On July 30, 2020, the State of Minnesota released its",
              a(href = "https://education.mn.gov/mdeprod/idcplg?IdcService=GET_FILE&dDocName=MDE033418&RevisionSelectionMethod=latestReleased&Rendition=primary",
                "Safe Learning Plan"),
              "for the 2020-2021 school year. One of the key components of the plan was a set of guidelines for the mode in which schools should operate (in-person, hybrid, distance learning) based on the level of COVID-19 transmission in the local community. The key metric recommended for
              quantifying community transmission for each county is the",
              
              tags$i("total number of cases over the past 14 days per 10,000 residents"),
              
              ". The purpose of this app is to display this key metric for each county and school district in Minnesota.")
        )
        
    } else {
        tagList(
            h4("Background"),
            p("On July 30, 2020, the State of Minnesota released its",
              a(href = "https://education.mn.gov/mdeprod/idcplg?IdcService=GET_FILE&dDocName=MDE033418&RevisionSelectionMethod=latestReleased&Rendition=primary",
                "Safe Learning Plan"),
              "for the 2020-2021 school year. One of the key components of the plan was a set of guidelines for the mode in which schools should operate (in-person, hybrid, distance learning) based on the level of COVID-19 transmission in the local community. The key metric recommended for
              quantifying community transmission for each county is the",
              
              tags$i("total number of cases over the past 14 days per 10,000 residents"),
              
              ". The purpose of this app is to display this key metric for each county and school district in Minnesota."),
            p(HTML("<b>NOTE:</b> The state has proposed using <i>county</i>-level metrics to make decisions on school status.
                School district level estimates are less accurate, and are provided for information purposes only."),
              style = "font-size: 16px; color: #000099")
            
        )
        
    }        
    })
    
    output$fineprint <- renderUI({
        if(input$tab == "county") {
            tagList(
                hr(),
                h6("Fine print"),
                div(class = "fineprint",
                    p(
                        HTML("County populations are based on <a href='https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html'>2019 estimates from the US Census</a>." )),
                    p(
                        HTML("Case numbers are obtained from the <a href='https://github.com/nytimes/covid-19-data'>
            New York Times Coronavirus GitHub data repository</a>, and will update whenever that data source does (at least daily)." )),
                    p("Shaded areas in the top plot give 95% confidence intervals for the 14-day case total per 10,000 people."),
                    p(
                        HTML("Source code for this app is available <a href='https://github.com/jwolfson/school_openings_mn'>on my GitHub site</a>.")
                    )
                )
            )
            
        } else {
            tagList(
                hr(),
                h6("Fine print"),
                div(class = "fineprint",
                    p(
                        HTML("ZIP-code specific case numbers are obtained on a weekly basis from the Minesota Department of Health's
                             <a href='https://www.health.state.mn.us/diseases/coronavirus/stats/index.html#wmapcz1'>COVID-19 Weekly Report</a>")
                    ),
                    p(
                        HTML("ZIP code populations are obtained from <a href='https://www.unitedstateszipcodes.org/zip-code-database/'>UnitedStatesZipCodes.org</a>")
                    ),
                    p("Mappings between ZIP codes and school districts are obtained from Minnesota Department of Education data."),
                    p(
                        HTML("Source code for this app is available <a href='https://github.com/jwolfson/school_openings_mn'>on my GitHub site</a>.")
                    )
                )
            )
            
        }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
