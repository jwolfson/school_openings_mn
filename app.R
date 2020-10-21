#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(showtext)

font_add_google("Cabin Sketch", "Cabin Sketch")
font_add_google("Neucha", "Neucha")

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
library(thematic)
library(RColorBrewer)
library(data.table)

shinyOptions(bootstraplib = TRUE)
#thematic_on(font = "auto")
#shiny::onStop(thematic_off)

bs_theme_new(bootswatch = "sketchy")
#bs_theme_new()
#bs_theme_fonts(base = "Nanum Pen Script", heading = "Cabin Sketch")
thematic::thematic_shiny(font = "auto")
shiny::onStop(thematic_off)

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

pops <- fread("countypop_us.csv") 
mdh_data <- read_csv("current_mdh.csv") %>%
  pivot_longer(-county, names_to = "date", values_to = "rate_mdh") %>%
  mutate(date = mdy(gsub("_","-", date)))
mults <- fread("current_mults.csv")

casedata <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", ) %>% 
    select(date, state, county, cases) %>%
    mutate(date = ymd(date)) %>%
    left_join(pops, by = c("state", "county")) %>%
    left_join(mults, by = "county") %>%
    left_join(mdh_data, by = c("county", "date")) %>%
    group_by(state, county) %>%
    mutate(cases_lag = lag(cases, LAG_DAYS),
           totalcases_last = cases - cases_lag) %>%
    ungroup() %>%
    mutate(rate_last = totalcases_last / pop * POP_DENOM,
           rate_last_ci_upper = POP_DENOM / pop * (totalcases_last + 1.96*sqrt(totalcases_last)),
           rate_last_ci_lower = pmax(0, POP_DENOM / pop * (totalcases_last - 1.96*sqrt(totalcases_last))),
           school_opening_status = cut(rate_last,
                                       c(-Inf, STATUS_CUTOFFS, Inf),
                                       labels = STATUS_LABELS),
           pred_mdh = rate_last * mult_factor,
           pred_mdh_lower = rate_last * (mult_factor - 1.96*mult_factor_sd),
           pred_mdh_upper = rate_last * (mult_factor + 1.96*mult_factor_sd))

casedata_district <- fread("district_cases.csv") %>%
  mutate(date = ymd(date),
         school_opening_status = cut(rate_last,
                              c(-Inf, STATUS_CUTOFFS, Inf),
                              labels = STATUS_LABELS))

opening_status <- data.frame(x = rep(c(ymd("2020-01-01"), ymd("2030-01-01")), each = length(STATUS_LABELS)),
                             ymin = rep(c(0, STATUS_CUTOFFS), 2),
                             ymax = rep(c(STATUS_CUTOFFS, 5000), 2),
                             school_opening_status = rep(STATUS_LABELS, 2)) %>%
    mutate(                  school_opening_status = factor(school_opening_status,
                                                            levels = STATUS_LABELS))


latest.day <- max(casedata$date)
first.day <- latest.day - 90
last.accurate.day <- latest.day

casedata <- casedata %>% mutate(unreliable_data = (date > last.accurate.day),
                                mdh_release_day = date %in% seq(min(mdh_data$date), max(mdh_data$date) + 14, by = 7),
                                post_mdh = date > max(mdh_data$date),
                                true_pred_mdh = ifelse(post_mdh, pred_mdh, rate_mdh))
casedata_district <- casedata_district %>% mutate(unreliable_data = FALSE)

# testPlot <- function() {
#   high_districts <- casedata_district %>% group_by(name) %>% arrange(date) %>% summarize(current_rate = last(rate_last)) %>%
#     ungroup() %>% arrange(desc(current_rate)) %>% slice(1:25)
#   
#   casedata_district %>% filter(name %in% high_districts$name) %>%
#     ggplot(aes(x = date, y = rate_last, group = name)) + geom_line()
#   
# }

makeCountyPlot <- function(countydata, show_mdh = FALSE) {
  
  if(show_mdh) {
    countydata_mdh <- countydata %>% filter(mdh_release_day == TRUE) %>%
      mutate(label = sprintf("%s County\n%s\n%s 14-day total per 10k: %.1f",
                             county,
                             strftime(date, "%b %d, %Y"),
                             ifelse(post_mdh, "Projected MDH", "Official MDH"),
                             true_pred_mdh))
  }
  
  g <- countydata %>%
    mutate(label = sprintf("%s County\n%s\n14-day total per 10k: %.1f\n(95%% CI: %.1f-%.1f)",
                           county,
                           strftime(date, "%b %d, %Y"),
                           rate_last,
                           rate_last_ci_lower,
                           rate_last_ci_upper)) %>%
        ggplot(aes(x = date)) + 
        geom_line(show.legend = FALSE, aes(y = rate_last, text = label)) +
#        geom_point(aes(y = rate_last_ci_upper), shape = 18) +
        geom_ribbon(aes(ymin = rate_last_ci_lower, ymax = rate_last_ci_upper), fill = "black", alpha = 0.1, show.legend = FALSE) +
        geom_point(aes(y = rate_last, text = label), size = 1, alpha = 0.7, show.legend = FALSE) +
        #scale_color_brewer(palette = "RdYlGn", direction = -1, drop = FALSE ) +
        geom_ribbon(data = opening_status, aes(x = x,
                                               ymin = ymin,
                                               ymax = ymax,
                                               fill = school_opening_status), 
                    alpha = 0.3,
                    inherit.aes = FALSE)
  
    if(show_mdh) {
      g <- g +
        geom_point(data = countydata_mdh %>% filter(!post_mdh), aes(y = true_pred_mdh, text = label), size = 2, shape = 15, show.legend = FALSE) +
        #geom_line(data = countydata_mdh %>% filter(!post_mdh), aes(y = true_pred_mdh), size = 1, show.legend = FALSE) +
        geom_point(data = countydata_mdh %>% filter(post_mdh), 
                   aes(y = true_pred_mdh, text = label), 
                   size = 3, 
                   shape = 17, 
                   color = "blue", 
                   show.legend = FALSE)
    }
      #geom_line(data = countydata_mdh, aes(y = true_pred_mdh, linetype = post_mdh)) +
     g + scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
        #scale_shape_manual(values = c(16, NA)) +
        #scale_linetype_manual(values = c("solid", "dashed")) +
        scale_y_continuous(breaks = c(0, 10, 20, 30, 50)) +
        coord_cartesian(xlim = c(first.day - 3, latest.day + 3), 
                        ylim = c(0,max(c(max(countydata$rate_last_ci_upper), max(countydata$true_pred_mdh, na.rm = TRUE), MAX_Y))),
                        expand = 0) +
        scale_fill_brewer(name = "", palette = "RdYlGn", direction = -1) +
        guides(x = "none", ymin = "none", ymax = "none", color = "none", shape = "none") +
        ylab(sprintf("Total cases in past %d days\nper %s population", LAG_DAYS, comma(POP_DENOM, digits = 0))) +
        xlab(NULL) +
        #theme_minimal() +
        theme(legend.position = "bottom",
              axis.title = element_text(family = "Cabin Sketch", size = 14),
              axis.text = element_text(family = "Neucha", size = 12),
              legend.text = element_text(family = "Neucha", size = 12))
}

makeDistrictPlot <- function(districtdata) {
    districtdata %>%
        mutate(
            school_opening_status = factor(school_opening_status,
                                           levels = STATUS_LABELS)
        ) %>%
        mutate(label = sprintf("%s\n%s\n14-day total per 10k: %.1f\n(95%% CI: %.1f-%.1f)",
                           name,
                           strftime(date, "%b %d, %Y"),
                           rate_last,
                           rate_last_ci_lower,
                           rate_last_ci_upper)) %>%
        ggplot(aes(x = date)) +
        # geom_bar(aes(y = rate_last), 
        #          stat = "identity",
        #          alpha = 0.7) +
    geom_linerange(aes(ymin = rate_last,
                      ymax = rate_last_max), 
                  #width = 0,
                  size = 1.5, 
                  alpha = 0.5,
                  show.legend = FALSE) +
    geom_ribbon(aes(ymin = rate_last_ci_lower, ymax = rate_last_ci_upper), 
                fill = "black", 
                alpha = 0.2, 
                show.legend = FALSE) +
    geom_point(aes(y = rate_last, text = label), 
               size = 1, shape = 15, alpha = 0.7, show.legend = FALSE) +
        #geom_errorbar(aes(ymin = rate_last_ci_lower,
        #                  ymax = rate_last_ci_upper), 
        #              width = 0.5) +
        geom_ribbon(data = opening_status, aes(x = x,
                                               ymin = ymin,
                                               ymax = ymax,
                                               fill = school_opening_status), 
                    alpha = 0.3,
                    inherit.aes = FALSE) +
        scale_x_date(breaks = districtdata$date, date_labels = "%b %d") +
    scale_y_continuous(breaks = c(0, 10, 20, 30, 50)) +
    coord_cartesian(xlim = range(districtdata$date) + c(-7, 7), 
                    ylim = c(0,max(max(districtdata$rate_last_ci_upper), MAX_Y)),
                    expand = 0) +
    scale_fill_brewer(name = "", palette = "RdYlGn", direction = -1) +
    guides(x = "none", ymin = "none", ymax = "none", color = "none") +
    ylab(sprintf("Total cases in past %d days\nper %s population", LAG_DAYS, comma(POP_DENOM, digits = 0))) +
    xlab(NULL) +
    #theme_minimal() +
    theme(legend.position = "bottom",
          axis.title = element_text(family = "Cabin Sketch", size = 14),
          axis.text = element_text(family = "Neucha", size = 12),
          legend.text = element_text(family = "Neucha", size = 12))
}

makeDistrictGauge <- function(districtdata) {
  status_colors <- rev(paste0(brewer.pal(length(STATUS_LABELS), "RdYlGn"), "99"))
  delta <- last(diff(districtdata$rate_last))
  
  plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = round(last(districtdata$rate_last), 1),
    title = list(text = "14-day case rate per 10,000",
                 font = list(size = 18)),
    type = "indicator",
    mode = "gauge+number+delta",
    delta = list(reference = round(nth(districtdata$rate_last, -2), 1),
                 increasing = list(color = substr(status_colors[5],1,7)),
                 decreasing = list(color = substr(status_colors[1],1,7))),
    gauge = list(
      axis =list(range = list(NULL, 80),
                 tickvals = list(0, 10, 20, 30, 50)),
      bar = list(color = "#00000099"),
      steps = list(
        list(range = c(0, STATUS_CUTOFFS[1]), color = status_colors[1]),
        list(range = c(STATUS_CUTOFFS[1], STATUS_CUTOFFS[2]), color = status_colors[2]),
        list(range = c(STATUS_CUTOFFS[2], STATUS_CUTOFFS[3]), color = status_colors[3]),
        list(range = c(STATUS_CUTOFFS[3], STATUS_CUTOFFS[4]), color = status_colors[4]),
        list(range = c(STATUS_CUTOFFS[4], 80), color = status_colors[5]))),
    width = 350,
    height = 200,
    ) 
}

makeCountyComparisonPlot <- function(allcounties) {
    allcounties %>% 
        filter(!is.na(rate_last)) %>%
        mutate(label = sprintf("%s County\n%.1f\n%s\n(Click for details)",
                               county,
                               rate_last,
                               school_opening_status)) %>%
        ggplot(aes(x = county, y = rate_last, fill = school_opening_status, text = label)) +
        geom_bar(stat = "identity",  
                 aes(color = highlighted,
                     alpha = highlighted),
                 size = 1) +
        scale_fill_brewer(name = "", palette = "RdYlGn", direction = -1, drop = FALSE, 
                          guide = guide_legend(override.aes = list(alpha = 0.3))) +
        scale_color_manual(values = c(NA, "black"), guide = NULL) +
        scale_alpha_manual(values = c(0.3, 0.7), guide = NULL) +
        scale_y_continuous(breaks = c(0, 10, 20, 30, 50)) +
        #coord_cartesian(ylim = c(0,MAX_Y)) +
        ylab(sprintf("Total cases in past %d days\nper %s population", LAG_DAYS, comma(POP_DENOM, digits = 0))) +
        xlab(NULL) +
        #theme_minimal(base_size = 16) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,
                                         size = 10,
                                         family = "Neucha"),
              axis.text.y = element_text(size = 12,
                                         family = "Neucha"),
              panel.grid.major.x = element_blank(),
#              legend.position = "bottom",
              legend.position = "none",
              axis.title = element_text(size = 14, 
                                        family = "Cabin Sketch"),
              #legend.text = element_text(size = 14),
              panel.background = element_blank(),
              panel.grid.major.y = element_line(color = gray(0.95))
              ) +
        ggtitle("")
}

makeDistrictComparisonPlot <- function(alldistricts) {
  ad <- alldistricts %>% 
    filter(!is.na(rate_last)) %>%
    mutate(label = sprintf("%s\nPop: %s\n14-day case rate: %.1f\n%s\n(Click for details)",
                           name,
                           format(pop, big.mark = ","),
                           rate_last,
                           school_opening_status))
  
  ad.highlighted <- ad %>% filter(highlighted == TRUE)
  
   g <- ad %>%
    ggplot(aes(x = name, y = rate_last, color = school_opening_status,
               text = label)) +
    geom_point(aes(size = pop), shape = 16, alpha = 0.7) +
    geom_linerange(aes(ymin = rate_last_ci_lower, ymax = rate_last_ci_upper), alpha = 0.5) +
    scale_color_brewer(name = "", palette = "RdYlGn", direction = -1, drop = FALSE, 
                      guide = guide_legend(override.aes = list(alpha = 0.3))) +
    scale_y_continuous(breaks = c(0, 10, 20, 30, 50)) +
    scale_size_continuous(range = c(0.1,5)) +
    geom_point(data = ad.highlighted, size = 3, shape = 15, 
               color = "black") +
    geom_linerange(data = ad.highlighted, aes(ymin = rate_last_ci_lower, ymax = rate_last_ci_upper), 
                   color = "black") +
    #coord_cartesian(ylim = c(0,MAX_Y)) +
    ylab(sprintf("Total cases in past %d days\nper %s population", LAG_DAYS, comma(POP_DENOM, digits = 0))) +
    xlab(NULL) +
    #theme_minimal(base_size = 16) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12,
                                     family = "Neucha"),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(),
          #              legend.position = "bottom",
          legend.position = "none",
          axis.title = element_text(size = 14, 
                                    family = "Cabin Sketch"),
          #legend.text = element_text(size = 14),
          panel.background = element_rect(fill = gray(0.85)),
          panel.grid.major.y = element_line(color = gray(0.75))
    ) +
    ggtitle("") 
   
}

makeCountryPlot <- function(alldata, bypop) {
    

    if(bypop) {
        alldata %>% filter(!is.na(rate_last), date == latest.day) %>%
        group_by(state, school_opening_status) %>%
        summarise(n_status = sum(pop)) %>%
        ungroup() %>%
        group_by(state) %>%
        mutate(pct_status = n_status / sum(n_status)) %>%
        ungroup() %>%
        mutate(state = fct_reorder(state, pct_status, .fun = first),
               school_opening_status = fct_rev(school_opening_status),
               label = sprintf("%s\n%s: %d%%", state, school_opening_status, round(100*pct_status))) %>% 
        ggplot(aes(x = state, y = pct_status, fill = school_opening_status, text = label)) +
        geom_bar(stat = "identity", position = "stack", aes(alpha = (state == STATE)), show.legend = FALSE) + 
        scale_fill_brewer(palette = "RdYlGn", drop = FALSE ) +
        scale_alpha_manual(values = c(0.3, 0.7)) +
        scale_y_continuous(labels = scales::percent) +
        ylab("Percentage of population") +
        xlab(NULL) +
        coord_flip(expand = FALSE, ylim = c(0,1.01)) +
        #theme_minimal(base_size = 16) +
        theme(legend.position = "none",
              axis.title = element_text(family = "Neucha", size = 14),
              axis.text.x = element_text(family = "Neucha", size = 14),
              axis.text.y = element_text(vjust = 0.2, family = "Neucha", size = 10),
              panel.background = element_blank(),
              panel.grid.major.y = element_blank())
    } else {
        alldata %>% filter(!is.na(rate_last), date == latest.day) %>%
            group_by(state, school_opening_status) %>%
            summarise(n_status = n()) %>%
            ungroup() %>%
            group_by(state) %>%
            mutate(pct_status = n_status / sum(n_status)) %>%
            ungroup() %>%
            mutate(state = fct_reorder(state, pct_status, .fun = first),
                   school_opening_status = fct_rev(school_opening_status),
                   label = sprintf("%s\n%s: %d%%", state, school_opening_status, round(100*pct_status))) %>% 
            ggplot(aes(x = state, y = pct_status, fill = school_opening_status, text = label)) +
            geom_bar(stat = "identity", position = "stack",
                     aes(alpha = (state == STATE)), show.legend = FALSE) + 
            scale_fill_brewer(palette = "RdYlGn", drop = FALSE ) +
            scale_alpha_manual(values = c(0.3, 0.7)) +
            scale_y_continuous(labels = scales::percent) +
            ylab("Percentage of counties") +
            xlab(NULL) +
            coord_flip(expand = FALSE, ylim = c(0,1.01)) +
            #theme_minimal(base_size = 16) +
            theme(legend.position = "none",
                  axis.text.y = element_text(vjust = 0.2, family = "Neucha", size = 10),
                  axis.text.x = element_text(family = "Neucha", size = 14),
                  axis.title = element_text(family = "Cabin Sketch", size = 14),
                  panel.grid.major.y = element_blank(),
                  panel.background = element_blank())
    }
    
}

# Define UI for application that draws a histogram
ui <- function(request) {
  
    fluidPage(
    bootstrap(),
    tags$link(href="https://fonts.googleapis.com/css?family=Cabin+Sketch|Neucha&display=swap", rel="stylesheet"),
    #tags$link(href="https://fonts.googleapis.com/css2?family=Nanum+Pen+Script&display=swap", rel="stylesheet"),
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
            #div(
            #uiOutput("multicheck"),
            #    style = "color: #333399; font-size: 12pt"
            #),
            uiOutput("countyweights"),
            uiOutput("rescale_message"),
            uiOutput("bookmark"),
            hr(),
            uiOutput("background"),
            uiOutput("fineprint"),
            uiOutput("about")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(id = "tab", selected = "county",
                        
                tabPanel(title = "By County", value = "county",
                    fluidRow(
                        column(6,
                               uiOutput("county_info_1"),
                               uiOutput("show_mdh_checkbox")),
                        column(6, align = "right",
                               uiOutput("county_info_2"))
                    ),
                    fluidRow(
                        column(12,
                               uiOutput("opening_info"),
                               withSpinner(plotlyOutput("opening"), type = 5, color = "#DAE6FF")
                        )
                    ),
                    fluidRow(
                        column(12,
                               h3("County Comparison"),
                               h6("How Minnesota's counties compare"),
                               withSpinner(plotlyOutput("compare_counties"), type = 5, color = "#DAE6FF")
                        )
                    ),
                    fluidRow(
                        column(12,
                               h2("U.S. Comparison"),
                               h5("How each state's counties stack up using Minnesota's school opening guidance"),
                               withSpinner(plotlyOutput("compare_us", height = "700px"), type = 5, color = "#DAE6FF")
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
                                    uiOutput("district_info_1"),
                                    uiOutput("district_info_2")),
                             column(6, align = "right",
                                      plotlyOutput("districtgauge", height = "225px", inline = TRUE)
                                      )#,
                                    #fluidRow(
                                    #  uiOutput("district_info_2")
                                    #)
                                    #)
                         ),
                         fluidRow(
                             column(12,
                                      withSpinner(plotlyOutput("opening_district"), type = 5, color = "#DAE6FF"),
                                      p("For districts containing ZIP codes with <= 5 cases, vertical bars give the range of 14-day per 10,000 case totals that are consistent with reported case data. Outer shaded region indicates 95% confidence intervals for the (range of) case rates."),
                             )
                         ),
                         fluidRow(
                           column(12,
                                  h3("District Comparison"),
                                  h6("How Minnesota school districts compare"),
                                  withSpinner(plotlyOutput("compare_districts"), type = 5, color = "#DAE6FF")
                           )
                         )
                )
            )
        )
    )
)
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
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
        dat %>% filter(rate_last >= 0 & !is.nan(rate_last_ci_lower) & !is.nan(rate_last_ci_upper))
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
        req(input$county)
      casedata %>% 
            filter(state == STATE,
                   date == last.accurate.day) %>%
            mutate(county = fct_reorder(county, rate_last),
                   highlighted = (county %in% input$county))
    })
    
    current_district_rates <- reactive({
      casedata_district %>%
        filter(date == max(date)) %>%
        mutate(name = fct_reorder(name, rate_last),
               highlighted = (name %in% input$district))
    })
    
    output$show_mdh_checkbox <- renderUI({
      req(input$county)
      div(checkboxInput("show_mdh",
                        span("Display MDH official & projected rates",
                             style = "color:#666666; font-size:16px; font-family:Cabin Sketch")),
          style = "padding-left:20px")
    })

    # output$show_mdh_checkbox <- renderUI({
    #   materialSwitch(inputId = "show_mdh", 
    #                  label = "Display MDH official & projected rates", 
    #                  value = FALSE,
    #                  right = TRUE,
    #                  status = "info")
    # })
    
    output$opening <- renderPlotly({
        req(input$county, !is.null(input$show_mdh))
        suppressWarnings(county_data() %>% makeCountyPlot(show_mdh = input$show_mdh)) %>% ggplotly(tooltip = "text") %>%
          layout(legend = list(orientation = "h", x = 0.5, y = -0.2, xanchor="center",
                               font = list(family = "Neucha"))) %>%
          plotly::style(hoverlabel = list(font = list(family = "Neucha", size = 12),
                                          bgcolor = "#000099"))
    })
    
    output$opening_info <- renderUI({
      req(input$show_mdh)
      
      if(!input$show_mdh) return(NULL)
      p(HTML("<span style='color:#000000'>Black squares</span> in the plot below show official rates provided on a two-week lag by the Minnesota Department of Health. 
             <br><span style='color:#0000FF'>Blue triangles</span> are <i>projections</i> of what MDH will report in upcoming data releases, based on daily new case numbers."), 
        style = "color:#666666; font-family:Cabin Sketch;padding-left:100px; padding-bottom:0px; margin-bottom:0px")
    })
    
    output$opening_district <- renderPlotly({
        req(input$district)
        suppressWarnings(district_data() %>% makeDistrictPlot()) %>% ggplotly(tooltip = "text") %>%
          layout(margin = list(t=0),
                 legend = list(orientation = "h", x = 0, y = -0.2,
                               font = list(family = "Neucha"))) %>%
          plotly::style(hoverlabel = list(font = list(family = "Neucha", size = 12),
                                          bgcolor = "#000099"))
    })
    
    output$districtgauge <- renderPlotly({
      req(input$district)
      district_data() %>% makeDistrictGauge() %>%
        layout(
          margin = list(l=50,r=50,b=0,t=0, pad = 0, autosize = FALSE),
               font = list(family = "Neucha"))
    })
    
    output$compare_counties <- renderPlotly({
        
        req(input$county)
            current_county_rates() %>% makeCountyComparisonPlot() %>% ggplotly(tooltip = "text", source = "countycompare") %>%
                plotly::style(hoverlabel = list(font = list(family = "Neucha", size = 12)))
    })

    output$compare_districts <- renderPlotly({
      req(input$district)
      current_district_rates() %>% makeDistrictComparisonPlot() %>% 
        ggplotly(tooltip = "text", source = "districtcompare") %>%
        plotly::style(hoverlabel = list(font = list(family = "Neucha", size = 12)))
    })
    
    output$compare_us <- renderPlotly({
        #req(input$county)
            casedata %>% makeCountryPlot(bypop = input$bypop) %>% ggplotly(tooltip = "text") %>%
            plotly::style(hoverlabel = list(font = list(family = "Neucha", size = 12)))
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
                h4(sprintf("ZIP codes: %s", zips)),
                h6(sprintf("(excludes ZIP codes where less than 0.25 square miles of land area is within district boundaries)"))
            ))
            
    })
    
    output$county_info_2 <- renderUI({
        
        caserate <- county_data() %>% filter(date == last.accurate.day) %>% pull(rate_last)
        status <- county_data() %>% filter(date == last.accurate.day) %>% pull(school_opening_status)

        tagList(
            h3(paste0("As of ", strftime(last.accurate.day, "%B %d, %Y")),
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
        
        if(caserate != caserate_max) {
          tagList(
            h3(paste0("Data as of ", strftime(max(dd$date), "%B %d, %Y"), ""),
               style = "color:#999999"),
            # h4(sprintf("%d-day case rate per %s: %.1f-%.1f", 
            #            LAG_DAYS, 
            #            comma(POP_DENOM, digits = 0), 
            #            round(caserate,1),
            #            round(caserate_max, 1)),
            #    style = "color:#99CC99"),
            #h4(paste0("Guideline Status: ", status),
            #   style = "color:#9999FF")
            
          )
        } else {
          tagList(
            h3(paste0("Data as of ", strftime(max(dd$date), "%B %d, %Y"), ""),
               style = "color:#999999"),
            # h4(sprintf("%d-day case rate per %s: %.1f", 
            #            LAG_DAYS, 
            #            comma(POP_DENOM, digits = 0), 
            #            round(caserate,1)),
            #    style = "color:#99CC99"),
            #h4(paste0("Guideline Status: ", status),
            #   style = "color:#9999FF")
          )
            
        }
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
              
              ". The purpose of this app is to display this key metric for each county and school district in Minnesota."),
            hr(),
            p(tags$b("NOTE:"), "The reported rates in this app may differ from the ", a("14-day COVID-19 Case Rate by County Report (PDF)", href = "https://www.health.state.mn.us/diseases/coronavirus/stats/wschool.pdf"),
              "provided by Minnesota Department of Health. In all cases, MDH data should be viewed as official.",
              style = "font-size: 16px; color: #000099"),
            actionLink("diffrates", "Click for more information", icon = icon("info"))
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
            hr(),
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
                    p("Shaded areas in the top plot give 95% confidence intervals for the 14-day case total per 10,000 people.")
                    
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
                )
            )
            
        }
        
    })
    
    output$about <- renderUI({
      tagList(
        hr(),
        h6("About"),
        div(class="fineprint",
        p(HTML("I am a faculty member in the <a href='https://www.sph.umn.edu/academics/divisions/biostatistics/'>Division of Biostatistics</a> within the <a href='http://www.sph.umn.edu/'>University of Minnesota School of Public Health</a>. <a href='http://z.umn.edu/julianw'>More about me</a>.")),
        p(HTML("For questions, contact <b>julianw@umn.edu</b>.")),
        p(HTML("This app is built using <a href='https://shiny.rstudio.com/'>R Shiny</a>. Source code is available <a href='https://github.com/jwolfson/school_openings_mn'>on my GitHub site</a>.")
        ))
      )
    })
    
    observeEvent(input$diffrates,
                 showModal(modalDialog(
                     size = "l",
                     title = "Why do these numbers not match MDH's?",
                     HTML("There are two main differences between the official MDH rates and those reported on the dashboard:</p>
<ol>
<li> MDH reports case counts by the date of specimen collection, so their official case counts are delayed by 2 weeks to ensure that all test results for specimens collected during a given date range have been included. This dashboard uses the daily data on new cases, regardless of date of specimen collection. Daily case data are more sensitive to emerging trends but do not fully reflect the state of the epidemic on a given date.
<li>This dashboard uses projected 2019 county populations (provided by the U.S. Census), while MDH are using 2018 projections.
                     </ol>")
)))
    
    observeEvent(event_data('plotly_click', source = "countycompare"), {
        
        ed <- event_data('plotly_click', source = "countycompare", session = session)
        ordered.counties <- current_county_rates() %>% arrange(county) %>% pull(county)
        updateSelectInput(session, "county", selected = ordered.counties[ed$x])
        
    })
    
    observeEvent(event_data('plotly_click', source = "districtcompare"), {
      
      ed <- event_data('plotly_click', source = "districtcompare", session = session)
      ordered.districts <- current_district_rates() %>% arrange(name) %>% pull(name)
      updateSelectInput(session, "district", selected = ordered.districts[ed$x])
      
    })
    
    
    
}

#thematic_shiny()
# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
