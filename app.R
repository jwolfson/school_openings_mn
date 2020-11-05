#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(showtext)

#font_add_google("Cabin Sketch", "Cabin Sketch")
#font_add_google("Neucha", "Neucha")

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
library(leaflet)
#library(maps)
#library(mapdata)
library(tigris)
library(sf)
library(sp)
library(forecast)
library(shinybusy)

shinyOptions(bootstraplib = TRUE)
#thematic_on(font = "auto")
#shiny::onStop(thematic_off)

bs_theme_new(bootswatch = "sketchy")
#bs_theme_new()
#bs_theme_fonts(base = "Nanum Pen Script", heading = "Cabin Sketch")
#thematic_shiny(font = "auto")
shiny::onStop(thematic_off)

options(tigris_use_cache = TRUE)

LAG_DAYS <- 14
POP_DENOM <- 10000
STATUS_CUTOFFS<- c(10, 20, 30, 50)
STATUS_LABELS <- c("All In-Person",
                   "Elem In-Person / HS Hybrid",
                   "All Hybrid",
                   "Elem Hybrid / HS Distance",
                   "All Distance")
MAX_Y <- 100
STATE <- "Minnesota"

pops <- fread("countypop_us.csv") 
mdh_data <- read_csv("current_mdh.csv") %>%
  pivot_longer(-county, names_to = "date", values_to = "rate_mdh") %>%
  mutate(date = mdy(gsub("_","-", date)))
mults <- fread("current_mults.csv")

casedata <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
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
first.day <- min(casedata$date)
last.accurate.day <- latest.day

casedata <- casedata %>% mutate(unreliable_data = (date > last.accurate.day),
                                mdh_release_day = date %in% seq(min(mdh_data$date), max(mdh_data$date) + 14, by = 7),
                                post_mdh = date > max(mdh_data$date),
                                true_pred_mdh = ifelse(post_mdh, pred_mdh, rate_mdh))
casedata_district <- casedata_district %>% mutate(unreliable_data = FALSE)

mypal <- rev(brewer.pal(length(STATUS_LABELS), "RdYlGn"))
df_pal <- data.frame(school_opening_status = STATUS_LABELS,
                     sos_color = mypal)

#mn_counties <- counties(state = "Minnesota", cb = TRUE)

#save(mn_counties, file = "county_boundaries.geo")

# zip_rates <- read_csv("current_zip_rates.csv") %>%
#   mutate(zip = as.character(zip)) %>%
#   left_join(df_pal, by = "school_opening_status") %>%
#   mutate(label = sprintf("<span style='font-family:Cabin Sketch; font-size:12pt'>Zip %s<br><span style='font-size:10pt'>Pop: %s</span></span><br><span style='font-family:Neucha'><span style='font-size:12pt'>%s</span><br>Guideline status:<br><span style='background-color:%s; padding:2px; margin-top:3px'>%s</span></span>", 
#                          zip,
#                          format(pop, big.mark=","),
#                          ifelse(rate_last == rate_last_max, 
#                                 sprintf("%.1f", rate_last),
#                                 sprintf("%.1f-%.1f", rate_last, rate_last_max)),
#                          sos_color,
#                          school_opening_status))

zip_rates_all <- read_csv("zip_rates.csv") %>%
  mutate(zip = as.character(zip)) %>%
  left_join(df_pal, by = "school_opening_status") %>%
  mutate(label = sprintf("<span style='font-family:Cabin Sketch; font-size:12pt'>Zip %s<br><span style='font-size:10pt'>Pop: %s</span></span><br><span style='font-family:Neucha'><span style='font-size:12pt'>%s</span><br>Guideline status:<br><span style='background-color:%s; padding:2px; margin-top:3px'>%s</span></span>", 
                         zip,
                         format(pop, big.mark=","),
                         ifelse(rate_last == rate_last_max, 
                                sprintf("%.1f", rate_last),
                                sprintf("%.1f-%.1f", rate_last, rate_last_max)),
                         sos_color,
                         school_opening_status))

zip_rates <- zip_rates_all %>% filter(date == max(date))

load("county_boundaries.geo")
load("zip_boundaries.geo")


addProjection <- function(countydata, days_ahead = 7) {
  county_name <- countydata %>% pull(county) %>% first()
  county_pop <- countydata %>% pull(pop) %>% first()
  max_date <- max(countydata$date)
  current_cases <- countydata %>% pull(cases) %>% last()
  
  model_cases <- suppressWarnings(forecast::auto.arima(countydata %>% pull(cases) %>% diff,
                                                       seasonal = TRUE))
  #allow.multiplicative.trend = TRUE))
  
  countydata_forecast <- 
    bind_cols(
      county = county_name,
      pop = county_pop,
      date = seq(max_date, max_date+(days_ahead-1), by = 1) + 1,
      data.frame(forecast::forecast(model_cases, h = days_ahead))
    ) %>%
    rename(daily_cases = Point.Forecast,
           daily_cases_min_80 = Lo.80,
           daily_cases_max_80 = Hi.80) %>%
    mutate(
      cases = current_cases + cumsum(daily_cases),
      cases_min_80 = current_cases + cumsum(daily_cases_min_80),
      cases_max_80 = current_cases + cumsum(daily_cases_max_80)) %>%
    bind_rows(countydata) %>%
    arrange(date) %>%
    mutate(cases_lag = lag(cases, LAG_DAYS),
           totalcases_last = cases - cases_lag,
           totalcases_last_min = cases_min_80 - cases_lag,
           totalcases_last_max = cases_max_80 - cases_lag,
           rate_last_pred = totalcases_last / pop * POP_DENOM,
           rate_last_pred_upper = POP_DENOM / pop * (totalcases_last_max + 1.96*sqrt(totalcases_last_max)),
           rate_last_pred_lower = pmax(0, POP_DENOM / pop * (totalcases_last_min - 1.96*sqrt(totalcases_last_min))),
           school_opening_status = cut(rate_last,
                                       c(-Inf, STATUS_CUTOFFS, Inf),
                                       labels = STATUS_LABELS)) %>%
    filter(date > max_date)
  
  countydata_forecast
  
}

makeCountyPlot <- function(countydata, show_mdh = FALSE, show_forecast = FALSE) {
  
  if(show_mdh) {
    countydata_mdh <- countydata %>% filter(mdh_release_day == TRUE) %>%
      mutate(label = sprintf("%s 14-day total per 10k: %.1f",
                             ifelse(post_mdh, "Projected MDH", "Official MDH"),
                             true_pred_mdh))
  }
  
  max_forecast <- 0
  
  if(show_forecast) {
    
    countydata_forecast <- addProjection(countydata) %>%
      mutate(label = sprintf("Forecast for %s\n14-day total per 10k: %.1f\nLikely range: %.1f-%.1f",
                             strftime(date, "%b %d, %Y"),
                             rate_last_pred, 
                             rate_last_pred_lower,
                             rate_last_pred_upper))
    
    max_forecast <- max(countydata_forecast$rate_last_pred_upper, na.rm = TRUE)
  }
  
  g <- countydata %>%
    mutate(label = sprintf("%s\n14-day total per 10k: %.1f\n(95%% CI: %.1f-%.1f)",
                           strftime(date, "%b %d, %Y"),
                           rate_last,
                           rate_last_ci_lower,
                           rate_last_ci_upper)) %>%
    ggplot(aes(x = date)) + 
    #geom_line(show.legend = FALSE, aes(y = rate_last, text = label)) +
    #        geom_point(aes(y = rate_last_ci_upper), shape = 18) +
    geom_ribbon(aes(ymin = rate_last_ci_lower, ymax = rate_last_ci_upper), 
                fill = "black", alpha = 0.1, show.legend = FALSE) +
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
  
  if(show_forecast) {
    g <- g + 
      geom_ribbon(data = countydata_forecast, aes(ymin = rate_last_pred_lower, 
                                                  ymax = rate_last_pred_upper),
                  fill = "#800080", alpha = 0.1, show.legend = FALSE) +
      geom_point(data = countydata_forecast, aes(y = rate_last_pred, text = label), size = 2, 
                 color = "#800080", shape = 18, show.legend = FALSE)
  }
  
  #geom_line(data = countydata_mdh, aes(y = true_pred_mdh, linetype = post_mdh)) +
  h <- g + scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
    #scale_shape_manual(values = c(16, NA)) +
    #scale_linetype_manual(values = c("solid", "dashed")) +
    scale_y_continuous(breaks = c(0, 10, 20, 30, 50)) +
    coord_cartesian(expand = 0, 
                    xlim = c(latest.day - ifelse(show_forecast, 30, 60), latest.day + 3 + ifelse(show_forecast,7,0)),
                    ylim = c(0,max(
                      c(max(countydata$rate_last_ci_upper, na.rm = TRUE), 
                        max(countydata$true_pred_mdh, na.rm = TRUE),
                        max_forecast,
                        MAX_Y)))) +
    scale_fill_brewer(name = "", palette = "RdYlGn", direction = -1) +
    guides(x = "none", ymin = "none", ymax = "none", color = "none", shape = "none") +
    ylab(sprintf("Total cases in past %d days\nper %s population", LAG_DAYS, comma(POP_DENOM, digits = 0))) +
    xlab(NULL) +
    #theme_minimal() +
    theme(legend.position = "bottom",
          axis.title = element_text(family = "Cabin Sketch", size = 14),
          axis.text = element_text(family = "Neucha", size = 12),
          legend.text = element_text(family = "Neucha", size = 10))
  
  h
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
          legend.text = element_text(family = "Neucha", size = 10))
}

makeCountyGauge <- function(countydata) {
  status_colors <- rev(paste0(brewer.pal(length(STATUS_LABELS), "RdYlGn"), "99"))
  delta <- last(diff(countydata$rate_last))
  
  plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = round(last(countydata$rate_last), 1),
    title = list(text = "14-day case rate per 10,000",
                 font = list(size = 18)),
    type = "indicator",
    mode = "gauge+number+delta",
    delta = list(reference = round(nth(countydata$rate_last, -2), 1),
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
    #width = 350,
    height = 175
  )
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
        list(range = c(STATUS_CUTOFFS[4], 80), color = status_colors[5])))
    #    width = 350,
    #    height = 200,
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
    scale_size_continuous(range = c(0.1,5))
  
  if(nrow(ad.highlighted) > 0) {
    g <- g +
      geom_point(data = ad.highlighted, size = 3, shape = 15, 
                 color = "black") +
      geom_linerange(data = ad.highlighted, aes(ymin = rate_last_ci_lower, ymax = rate_last_ci_upper), 
                     color = "black")
  }
  
  #coord_cartesian(ylim = c(0,MAX_Y)) +
  g + ylab(sprintf("Total cases in past %d days\nper %s population", LAG_DAYS, comma(POP_DENOM, digits = 0))) +
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
      mutate(pct_status = n_status / sum(n_status),
             weight_status = sum(pct_status*as.integer(school_opening_status))) %>%
      ungroup() %>%
      mutate(wts = as.integer(school_opening_status), 
             state = fct_reorder(state, weight_status, .fun = first, .desc = TRUE),
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
      mutate(pct_status = n_status / sum(n_status),
             weight_status = sum(pct_status*as.integer(school_opening_status))) %>%
      ungroup() %>%
      mutate(wts = as.integer(school_opening_status), 
             state = fct_reorder(state, weight_status, .fun = first, .desc = TRUE),
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

makeStateMap <- function(mapcounty) {
  
  df_map <- casedata %>% 
    filter(state == "Minnesota", date == max(date)) %>%
    left_join(df_pal, by = "school_opening_status") %>%
    filter(county != "Unknown") %>%
    mutate(label = sprintf("<span style='font-family:Cabin Sketch; font-size:12pt'>%s County<br><span style='font-size:10pt'>Pop: %s</span></span><br><span style='font-family:Neucha'><span style='font-size:12pt'>%.1f</span><br>Guideline status:<br><span style='background-color:%s; padding:2px; margin-top:3px'>%s</span></span>", 
                           county, 
                           format(pop, big.mark=","),
                           rate_last,
                           sos_color,
                           school_opening_status),
           weight = ifelse(county %in% mapcounty, 5, 1),
           opacity = ifelse(county %in% mapcounty, 1, 0.6))
  
  mn_county_map <- geo_join(mn_counties,
                            df_map,
                            by_sp = "NAME",
                            by_df = "county",
                            how = "inner")
  
  county_shape <- filter(mn_county_map, NAME %in% mapcounty) %>% pull(geometry)
  
  bbox <- as.numeric(sp::bbox(sf::as_Spatial(county_shape)))
  
  lngc <- mean(bbox[c(1,3)])
  lngr <- diff(bbox[c(1,3)])
  latc <- mean(bbox[c(2,4)])
  latr <- diff(bbox[c(2,4)])
  
  m <- leaflet(data = mn_county_map) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(color = "black",
                fillColor = ~sos_color,
                fillOpacity = 0.5,
                opacity = ~opacity,
                weight = ~weight,
                popup = ~label) %>%
    fitBounds(lng1 = bbox[1] - lngr*0.3, 
              lat1 = bbox[2] - latr*0.3, 
              lng2 = bbox[3] + lngr*0.3, 
              lat2 = bbox[4] + latr*0.3)
  m
}

makeZipMap <- function(zr) {
  
  zip_merged <- geo_join(char_zips,
                         zr,
                         by_sp = "ZCTA5CE10",
                         by_df = "zip",
                         how = "inner")
  
  zip_merged %>%
    leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(fillColor = ~sos_color,
                weight = 1,
                opacity = 1,
                color = gray(0.5),
                dashArray = "1",
                fillOpacity = 0.5,
                popup = ~label)
  
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
                        #sidebar {background-color: #FFFFFF;padding:0px;border-width:0px}
                        .inline label{ display: table-cell; text-align: left; vertical-align: middle; } 
                 .inline .form-group{display: table-row;}"))
    ),
    busy_start_up(
      loader = spin_epic("atom", color = "#FFF"),
      text = h3("Loading..."),
      timeout = 2500,
      color = "#FFF",
      background = "#333333"
    ),
    
    add_busy_spinner(spin = "atom", onstart = FALSE,
                     color = "#333333"),
    
    
    # Application title
    titlePanel(sprintf("%s School Opening Statistics", STATE)),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        width = 4,
        id = "sidebar",
        div(class = "card border-info mb-3",
            h3(class = "card-header",
               uiOutput("selectlabel")),
            div(class = "card-body",
                style = "margin-top:0px; padding-top:0px",
                         uiOutput("selectmenu"),
                         div(style = "padding-left:10px",
                              uiOutput("multicheck")
                              ),
        #uiOutput("districtselect"),
        uiOutput("zipselect"),
        uiOutput("weightinputs"),
        uiOutput("rescale_message"),
        div(style = "margin-top:20px",
          uiOutput("bookmark")))),
        div(class = "card border-info mb-3",
            h3(class = "card-header",
               "Background"),
            div(class = "card-body",
                uiOutput("background"))),
        #uiOutput("fineprint"),
        div(class="alert alert-dismissible alert-info",
            uiOutput("faq")
            ),
        div(class = "card border-info mb-3",
            h5(class = "card-header",
               "About"),
            div(class = "card-body",
                uiOutput("about")))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        width = 8,
        tabsetPanel(id = "tab", selected = "county", type = "tabs",
                    
                    tabPanel(title = span("By County", style = "font-size:16pt"), value = "county",
                             fluidRow(
                                 column(12, style = "padding-left:30px; padding-right:30px",
                                        fluidRow(

                                                 
                               column(8,  style = "padding:10px; background-color:#444444; margin-bottom:0px; border-radius: 0px 0px 0px 30px",
                                      div(
                                        uiOutput("county_info_1"),
                                        style = "color:#FFFFFF"
                                        ),
                                      fluidRow(
                                               div(uiOutput("show_mdh_checkbox", style = "padding-left:30px; color: #EEEEEE; width: 200px")),
                                               div(uiOutput("show_forecast_checkbox"), style = "padding-left:20px; color: #EEEEEE; width: 250px"))
                               ),
                               column(4, align = "right", style = "padding:10px; background-color:#444444; margin-bottom:0px; border-radius: 0px 0px 30px",
                                      plotlyOutput("countygauge", height = "175px", inline = FALSE)
                               )
                             ))),
                             
                             fluidRow(
                               column(12,
                                      style = "padding-left:30px; margin-top:0px",
                                      fluidRow(
                                        column(4,
                                               style = "padding: 20px",
                                               leafletOutput("statemap", height = 400, width = "auto")
                                        ),
                                        column(8,
                                               style = "margin-top:20px",
                                               uiOutput("opening_info"),
                                               plotlyOutput("opening", height = 400, inline = TRUE))
                                      )
                                      )),
                             #   fluidRow(
                             #     column(6,
                             #            uiOutput("county_info_1"),
                             #            uiOutput("show_mdh_checkbox"),
                             #            style = "padding-top:5px"),
                             #     column(6, align = "right",
                             #            plotlyOutput("countygauge", height = "225px", inline = TRUE))
                             # ),
                             # fluidRow(
                             #   column(4,
                             #         withSpinner(leafletOutput("statemap", height = 325), type = 5, color = "#DAE6FF"),
                             #         style = "padding-top:25px"),
                             #     column(8,
                             #            uiOutput("opening_info"),
                             #            withSpinner(plotlyOutput("opening", height = 400), type = 5, color = "#DAE6FF")
                             #     )
                             # ),
                             fluidRow(
                               column(12,
                                      div(class = "card mb-3",
                                          h2("County Comparison", class = "card-header"),
                                          div(class = "card-body",
                                              h5("How Minnesota's counties compare", class = "card-title"),
                                              plotlyOutput("compare_counties")
                                          )
                                      )
                               )
                             ),
                             fluidRow(
                               column(12,
                                      div(class = "card mb-3",
                                          h2("U.S. Comparison", class = "card-header"),
                                          div(class = "card-body",
                                              h5("How each state's counties stack up using Minnesota's school opening guidance"),
                                              plotlyOutput("compare_us", height = "700px")
                                          )
                                      )
                               )
                             ),
                             fluidRow(
                               column(12, align = "center",
                                      checkboxInput("bypop", span("View by population", style = "font-size:20px"))
                               )
                             )
                    ),
                    
                    tabPanel(title = span("By School District", style="font-size:16pt"), value = "district",
                             fluidRow(
                               column(12, style = "padding-left:30px; padding-right:30px",
                                      fluidRow(
                               column(8, style = "padding:10px; background-color:#444444; margin-bottom:15px; border-radius: 0px 0px 0px 30px",
                                      div(
                                        uiOutput("district_info_1"),
                                        style = "color:#FFFFFF"
                                        ),
                                      div(
                                        uiOutput("district_info_2"),
                                        style = "color:#CCCCCC"
                                      )
                                      
                               ),
                               column(4, align = "right", 
                                      style = "padding:10px; background-color:#444444; margin-bottom:15px; border-radius: 0px 0px 30px 0px",
                                      plotlyOutput("districtgauge", height = "175px", inline = FALSE)
                               )#,
                               #fluidRow(
                               #  uiOutput("district_info_2")
                               #)
                               #)
                             ))),
                             fluidRow(
                               column(4,
                                      leafletOutput("zipmap")
                               ),
                               column(8,
                                      plotlyOutput("opening_district"), 
                                      p("For districts containing ZIP codes with <= 5 cases, vertical bars give the range of 14-day per 10,000 case totals that are consistent with reported case data. Outer shaded region indicates 95% confidence intervals for the (range of) case rates.")
                               )
                             ),
                             fluidRow(
                               column(12,
                                      div(class = "card mb-3",
                                      h3("District Comparison", class = "card-header"),
                                      div(class = "card-body",
                                      h5("How Minnesota school districts compare"),
                                      plotlyOutput("compare_districts")
                                      )
                                      )
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
  
  output$multicheck <- renderUI({
    if(is.null(input$tab) | input$tab == "county") {
      prettySwitch(
        inputId = "multicounty",
        label = "Select/combine counties", 
        value = FALSE,
        slim = TRUE,
        status = "info"
      )
      # switchInput(inputId = "multicounty",
      #              label = "Select/combine counties", 
      #              value = FALSE, 
      #              status = "primary", 
      #              fill = TRUE)
    } else {
      prettySwitch(
        inputId = "multizip",
        label = "Select/combine zips", 
        value = FALSE,
        slim = TRUE,
        status = "primary"
      )
      
    }
  })
  
  output$selectlabel <- renderUI( {
    paste("Select a", input$tab)
  })
  
  output$selectmenu <- renderUI( {
    
    if(is.null(input$tab) | input$tab == "county") {
      if(is.null(input$multicounty)) {
        single <- TRUE
      } else {
        single <- !input$multicounty
      }
      
      multi <- !single
      
      pickerInput("county",
                  "",
                       #style="font-family: Cabin Sketch; font-size:16pt"),
                  choices = pops %>% filter(state == STATE) %>% pull(county),
                  selected = pops %>% filter(state == STATE) %>%  arrange(desc(pop)) %>%  slice(1:(1+multi)) %>% pull(county),
                  multiple = multi,
                  options = list(
                    style = "btn-info"
                  ))
      
    } else {
      pickerInput("district",
                  "",
                  #span("Select a school district", class = "card-header"),
                       #style="font-family: Cabin Sketch; font-size:16pt"),
                  choices = casedata_district %>% pull(name) %>% unique(),
                  multiple = FALSE,
                  options = list(
                    style = "btn-info"
                  ))
    }
  })
  
  output$zipselect <- renderUI({
    req(input$district)
    
    if(input$tab == "district" & input$multizip == TRUE) {
      selectInput("zips",
                  span("Select zips", style="font-family: Cabin Sketch; font-size:12pt"),
                  choices = zip_rates %>% pull(zip) %>% unique(),
                  selected = get_district_zips(input$district),
                  multiple = TRUE)
    }
  })
  
  
  output$weightinputs <- renderUI( {
    #      req(input$multicounty | input$multizip)
    L <- NULL
    
    req(!is.null(input$county) | !is.null(input$district), !is.null(input$multicounty) | !is.null(input$multizip))
    
    if(input$tab == "county") {
      req(input$county, input$multicounty)
      L <- lapply(1:length(input$county),
                  function(i) {
                    numericInput(paste0("weight_", i),
                                 label = paste0("% ", input$county[i]),
                                 value = round(1/length(input$county)*100), 
                                 width = "100px")  
                  })
    }
    
    if(input$tab == "district") {
      req(input$district, input$multizip)
      
      dz <- sort(input$zips)
      pw <- zip_rates_all %>% filter(zip %in% dz,
                                     date == max(date)) %>%
        mutate(pop_weights = pop / sum(pop)) %>%
        arrange(zip) %>%
        pull(pop_weights)
      
      L <- lapply(1:length(dz),
                  function(i) {
                    div(class = "inline",
                        style = "width:120px; margin-left:10px",
                        numericInput(paste0("weight_", dz[i]),
                                     label = HTML(sprintf("%%&nbsp;%s:&nbsp;&nbsp;", dz[i])), #style = "margin-right:5px"),
                                     value = round(pw[i]*100)  )
                    )
                  })
    }
    
    L
    
  })
  
  get_county_weights <- reactive({
    
    #req(!is.null(input$multicounty) | (!is.null(input$multizip) & length(input$zips) > 1))
    req(input$multicounty, input$weight_1)
    
    w <- as.numeric(unlist(lapply(1:length(input$county),
                                  function(i) input[[paste0("weight_", i)]])))
    w
  }) 
  
  county_data <- reactive({
    req(input$county)
    
    if(length(input$county) == 1) {
      dat <- casedata %>% filter(state == STATE,
                                 county %in% input$county,
                                 date >= first.day,
                                 date <= latest.day)
    } else { #Multiple counties selected
      orig.weights <- get_county_weights()
      weights <- orig.weights / sum(orig.weights) # Rescale the weights
      dat <- suppressWarnings(casedata %>% 
                                filter(state == STATE, 
                                       county %in% input$county,
                                       date >= first.day,
                                       date <= latest.day) %>%
                                group_by(date) %>%
                                summarise(
                                  cases = sum(weights*cases),
                                  pop = sum(weights*pop),
                                  mdh_release_day = first(mdh_release_day),
                                  post_mdh = first(post_mdh),
                                  rate_mdh = ifelse(length(rate_mdh) != length(weights),
                                                    NA,
                                                    weighted.mean(rate_mdh, w = weights)),
                                  pred_mdh = ifelse(length(rate_last) != length(weights),
                                                    NA,
                                                    weighted.mean(rate_last * mult_factor, w = weights)),
                                  pred_mdh_lower = ifelse(length(rate_last) != length(weights),
                                                          NA,
                                                          weighted.mean(rate_last * (mult_factor - 1.96*mult_factor_sd),
                                                                        w = weights)),
                                  pred_mdh_upper = ifelse(length(rate_last) != length(weights),
                                                          NA,
                                                          weighted.mean(rate_last * (mult_factor + 1.96*mult_factor_sd),
                                                                        w = weights)),
                                  rate_last = ifelse(length(rate_last)!= length(weights), 
                                                     NA, 
                                                     weighted.mean(rate_last, w = weights)),
                                  rate_last_SE = ifelse(length(totalcases_last) != length(weights),
                                                        NA,
                                                        POP_DENOM * sqrt(sum(totalcases_last*weights^2/pop^2)))
                                  
                                ) %>%
                                filter(rate_last >= 0) %>%
                                mutate(true_pred_mdh = ifelse(post_mdh, pred_mdh, rate_mdh),
                                       rate_last_ci_lower = rate_last - 1.96*rate_last_SE,
                                       rate_last_ci_upper = rate_last + 1.96*rate_last_SE,
                                       school_opening_status = cut(rate_last,
                                                                   c(-Inf, STATUS_CUTOFFS, Inf),
                                                                   labels = STATUS_LABELS),
                                       county = sprintf("Custom (%s)", paste(input$county, collapse = "/")),
                                       unreliable_data = (date > last.accurate.day)))
    }
    dat %>% filter(!is.nan(rate_last_ci_lower) & !is.nan(rate_last_ci_upper))
  })
  
  get_district_weights <- reactive({
    req(input$zips)
    dz <- input$zips
    
    #print(paste("Zips in get_district_weights:", paste(input$zips, collapse=", ")))
    
    req(input[[paste0("weight_", dz[1])]])
    
    w <- as.numeric(unlist(lapply(1:length(dz),
                                  function(i) input[[paste0("weight_", dz[i])]])))
    
    #print(paste("Weight in get_district_weights:", paste(w, collapse = " ")))
    
    w
  })
  
  
  district_data <- reactive({
    req(input$district)
    
    if(input$tab == "district" & input$multizip == TRUE & length(input$zips) > 1) {
      #req(length(input$zips) > 1)
      
      orig.weights <- get_district_weights()
      weights <- orig.weights / sum(orig.weights) # Rescale the weights
      
      dz <- input$zips
      
      df_weights <- data.frame(zip = dz, weights = weights)
      
      zip_dat <- zip_rates_all %>% 
        filter(zip %in% dz) %>%
        left_join(df_weights, by = "zip") %>%
        group_by(date) %>%
        summarise(rate_last_w = weighted.mean(rate_last, w = weights),
                  rate_last_ci_upper_w = weighted.mean(rate_last_ci_upper, w = weights),
                  rate_last_ci_lower_w = weighted.mean(rate_last_ci_lower, w = weights),
                  rate_last_max_w = weighted.mean(rate_last_max, w = weights)) %>%
        ungroup() %>%
        mutate(rate_last = rate_last_w,
               rate_last_ci_upper = rate_last_ci_upper_w,
               rate_last_ci_lower = rate_last_ci_lower_w,
               rate_last_max = rate_last_max_w,
               school_opening_status = cut(rate_last,
                                           c(-Inf, STATUS_CUTOFFS, Inf),
                                           labels = STATUS_LABELS),
               name = "Custom District") %>%
        filter(!is.na(rate_last))
      
      dat <- zip_dat
      # casedata_district %>% filter(name == input$district) %>%
      # left_join(zip_dat, by = "date") %>%
      # mutate(rate_last = rate_last_w,
      #        rate_last_ci_upper = rate_last_ci_upper_w,
      #        rate_last_ci_lower = rate_last_ci_lower_w,
      #        rate_last_max = rate_last_max_w,
      #        school_opening_status = cut(rate_last,
      #                                    c(-Inf, STATUS_CUTOFFS, Inf),
      #                                    labels = STATUS_LABELS),
      #        name = sprintf("%s (Zip-reweighted)", name)) %>%
      # filter(!is.na(rate_last))
      
    } else {
      dat <- casedata_district %>% filter(name == input$district,
                                          !is.na(rate_last))
    }
    
    dat
    
  })
  
  get_district_zips <- function(district_name) {
    district_zips <- casedata_district %>% 
      filter(name == district_name) %>% 
      pull(zips) %>% 
      first()
    
    strsplit(district_zips, split = ", ")[[1]]
  }
  
  
  
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
      mutate(name = fct_reorder(name, rate_last)) %>%
      rowwise() %>%
      mutate(highlighted = ifelse(input$multizip, # Only highlight if we're not combining zips
                                  FALSE,
                                  (name %in% input$district)))
  })
  
  
  
  output$rescale_message <- renderUI( {
    req(input$multicounty | input$multizip)
    
    if((input$tab == "county" & input$multicounty) | (input$tab == "district" & input$multizip)) {
      #if(abs(sum(get_county_weights()) - 100) > 1 & (input$multicounty | input$multizip)) {
      return(p(sprintf("%sSums of percentages not adding to 100%% will be rescaled proportionally.",
                       ifelse(input$multizip, 
                              "By default, weights are proportional to Zip code population. ",
                              ""))))
    }
    #}
  })
  
  output$show_mdh_checkbox <- renderUI({
    #req(input$county)
    #div(
    awesomeCheckbox(
      inputId = "show_mdh",
      label = "Display MDH rates", 
      value = FALSE,
      status = "info"
    )
    # HTML('
    # <label class="form-check-label">
    #    <input class="form-check-input" type="checkbox" name = "show_mdh" value = "">
    #      Display MDH rates
    #    </label>')
      #checkboxInput("show_mdh", "Display MDH rates", class = "form-check-input")
    #span("Display MDH rates",
    #style = "color:#666666; font-size:12px; font-family:Cabin Sketch")),
    #style = "padding-left:20px")
  })
  
  output$show_forecast_checkbox <- renderUI({
    req(!is.null(input$multicounty))
    
    if(!input$multicounty) {
      #checkboxInput("show_forecast", "Display one-week forecast")
      awesomeCheckbox(
        inputId = "show_forecast",
        label = "Display one-week forecast", 
        value = FALSE,
        status = "info"
      )
    } 
    
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
    
    if(is.null(input$show_forecast)) {
      sf <- FALSE
    } else {
      sf <- input$show_forecast
    } 
    
    g <- suppressWarnings(county_data() %>% makeCountyPlot(show_mdh = input$show_mdh, show_forecast = sf)) %>%
      ggplotly(tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.5, y = -0.2, xanchor="center",
                           font = list(family = "Neucha")),
             margin = list(t=0),
             hovermode = "x unified") %>%
      plotly::style(hovermode = "skip", traces = 1) %>%
      plotly::style(hoverlabel = list(font = list(family = "Neucha", size = 14),
                                      bgcolor = "#666666"), traces = 2)
    if(input$show_mdh) {
      g <- g %>% plotly::style(hoverlabel = list(font = list(family = "Cabin Sketch", size = 14)), 
                               traces = c(8:12))
    } 
    
    if(sf) {
      g <- g %>% plotly::style(hoverlabel = list(font = list(family = "Cabin Sketch", size = 14)), 
                               traces = c(8,9,10))
    }
    
    g
  })
  
  
  output$opening_info <- renderUI({
    req(!is.null(input$show_mdh), !is.null(input$show_forecast))
    
    T1 <- T2 <- NULL
    
    if(input$show_mdh) {
      T1 <- p(HTML("<span style='color:#000000'>Black squares</span> show official rates provided by the Minnesota Department of Health. 
             <br><span style='color:#0000FF'>Blue triangles</span> are <i>projections</i> of what MDH will report in upcoming data releases, based on daily new case numbers."), 
              style = "color:#666666; font-family:Cabin Sketch;padding-bottom:0px; margin-bottom:0px")
    }
    
    if(input$show_forecast) {
      T2 <- p(HTML("<span style='color:#800080'>Purple diamonds</span> show forecasted rates for the next 7 days."), 
              style = "color:#666666; font-family:Cabin Sketch;padding-bottom:0px; margin-bottom:0px")
    }
    
    if(is.null(T1) & is.null(T2)) {
      out <- ""
    } else {
      out <- div(class = "alert alert-dismissible alert-primary",
        tagList(T1,T2))
    }
    
    out
      
  })
  
  output$countygauge <- renderPlotly({
    cd <- county_data()
    
    makeCountyGauge(cd) %>%
      layout(
        margin = list(l=20,r=20,b=10,t=40, autosize = FALSE),
        font = list(family = "Neucha", color = "#FFFFFF"),
        paper_bgcolor="#444444") %>%
      config(displayModeBar = FALSE)
    
  })
  
  output$districtgauge <- renderPlotly({
    #req(input$district)
    dd <- district_data()
    
    makeDistrictGauge(dd) %>%
      layout(
        margin = list(l=20,r=20,b=10,t=40, autosize = FALSE),
        font = list(family = "Neucha", color = "#FFFFFF"),
        paper_bgcolor="#444444") %>%
      config(displayModeBar = FALSE)
  })
  
  output$opening_district <- renderPlotly({
    req(input$district)
    suppressWarnings(district_data() %>% makeDistrictPlot()) %>% ggplotly(tooltip = "text") %>%
      layout(margin = list(t=0),
             legend = list(orientation = "h", x = 0.5, y = -0.2, xanchor="center",
                           font = list(family = "Neucha"))) %>%
      plotly::style(hoverlabel = list(font = list(family = "Neucha", size = 12),
                                      bgcolor = "#000099"))
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
  
  output$statemap <- renderLeaflet({
    req(input$county)
    makeStateMap(input$county)
  })
  
  output$zipmap <- renderLeaflet({
    req(input$district, !is.null(input$multizip))
    
    if(input$multizip == TRUE) {
      district_zips <- input$zips
    } else {
      district_zips <- get_district_zips(input$district)
    }
    
    zr <- zip_rates %>% filter(zip %in% district_zips)
    #mutate(weight = ifelse(zip %in% district_zips, 5, 1))
    
    makeZipMap(zr)
  })
  
  
  
  output$bookmark <- renderUI( {
    req(input$county)
    #        if(length(input$county) > 1) {
    bookmarkButton(title = "Generate a custom URL to return to these inputs at a later time.",
                   class = "btn btn-info")
    #        }
  })
  
  output$county_info_1 <- renderUI({
    
    req(input$county)
    
    if(length(input$county) == 1) {
      countypop <- comma(pops %>% filter(state == STATE, county %in% input$county) %>%
                           pull(pop) %>% first(), digits = 0)
      
      return(tagList(
        h1(paste(input$county, "County")),
        h4(paste("Population (2019 estimated):", countypop)),
        h3(paste0("Data as of ", strftime(last.accurate.day, "%B %d, %Y")),
           style = "color:#CCCCCC")
      ))
      
    } else {
      # orig.weights <- get_weights()
      # weights <- orig.weights / sum(orig.weights)
      # pct <- round(100*weights)
      
      return(tagList(
        h1(sprintf("Custom County (%s)",
                   paste(input$county, collapse = " / "))
        ),
        h3(paste0("Data as of ", strftime(last.accurate.day, "%B %d, %Y")),
           style = "color:#CCCCCC")
      ))
    }
  })
  
  output$district_info_1 <- renderUI({
    req(!is.null(input$district) & !is.null(input$multizip))
    
    if(input$multizip == TRUE) {
      district_name <- district_data() %>% pull(name) %>% first()
      zips <- paste(input$zips, collapse=", ")
    } else {
      district_name <- input$district
      zips <- paste(get_district_zips(input$district), collapse = ", ")
    }
    
    problem_zip <- "55033"
    
    if(grepl(problem_zip, zips)) {
      warning_zip <- HTML("
                          <div class='alert alert-dismissible alert-danger'>
  <h4 class='alert-header'>Note</h4>
  <strong>A correction was made to data from zip code 55033 in the 10/29 MDH data release, so that rate statistics 
             for this district covering the period of 10/08 - 10/29 are not reliable. Updated 14-day rates will be available on 11/12.
             </strong>
                          </div>")
                          
             #              h4("NOTE: A correction was made to data from zip code 55033 in the 10/29 MDH data release, so that rate statistics 
             # for this district covering the period of 10/08 - 10/29 are not reliable. Updated 14-day rates will be available on 11/12.", 
             #            style = "font-family: Neucha; font-face: bold; background:#CC0000; color:#FFFFFF; padding: 8px")
    } else {
      warning_zip <- NULL
    }
    
    if(input$multizip == TRUE) {
      return(             
        tagList(
          h1(district_name),
          h4(sprintf("ZIP codes: %s", zips)),
          warning_zip
        )
      )
    } else {
      return(             
        tagList(
          h1(district_name),
          h4(sprintf("ZIP codes: %s", zips)),
          h6("(excludes ZIP codes where less than 0.25 square miles of land area is within district boundaries)"),
          warning_zip
        )
      )
    }
    
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
        h3(paste0("Data as of ", strftime(max(dd$date), "%B %d, %Y"), "")),
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
        h3(paste0("Data as of ", strftime(max(dd$date), "%B %d, %Y"), ""))
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

  output$faq <- renderUI({
    h4(icon("question"), 
       tags$a(href = "https://docs.google.com/document/d/1KMajq4HEGn2rwGhxCDInHHUYzZwROZ5jTdXIbOM-T0o/edit?usp=sharing",
              "Frequently Asked Questions",
              target = "_blank"),
       style = "padding: 8px; text-align: center")
    
  })  
  
  output$background <- renderUI({
    
    if(input$tab == "county") {
      tagList(
        p("On July 30, 2020, the State of Minnesota released its",
          a(href = "https://education.mn.gov/mdeprod/idcplg?IdcService=GET_FILE&dDocName=MDE033418&RevisionSelectionMethod=latestReleased&Rendition=primary",
            "Safe Learning Plan"),
          "for the 2020-2021 school year. One of the key components of the plan was a set of guidelines for the mode in which schools should operate (in-person, hybrid, distance learning) based on the level of COVID-19 transmission in the local community. The key metric recommended for
              quantifying community transmission for each county is the",
          
          tags$i("total number of cases over the past 14 days per 10,000 residents"),
          
          ". The purpose of this app is to display this key metric for each county and school district in Minnesota."),
      )
      
    } else {
      tagList(
        p("On July 30, 2020, the State of Minnesota released its",
          a(href = "https://education.mn.gov/mdeprod/idcplg?IdcService=GET_FILE&dDocName=MDE033418&RevisionSelectionMethod=latestReleased&Rendition=primary",
            "Safe Learning Plan"),
          "for the 2020-2021 school year. One of the key components of the plan was a set of guidelines for the mode in which schools should operate (in-person, hybrid, distance learning) based on the level of COVID-19 transmission in the local community. The key metric recommended for
              quantifying community transmission for each county is the",
          
          tags$i("total number of cases over the past 14 days per 10,000 residents"),
          
          ". The purpose of this app is to display this key metric for each county and school district in Minnesota."),

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
      div(class="fineprint",
          p(HTML("I am a faculty member in the <a href='https://www.sph.umn.edu/academics/divisions/biostatistics/'>Division of Biostatistics</a> within the <a href='http://www.sph.umn.edu/'>University of Minnesota School of Public Health</a>. <a href='http://z.umn.edu/julianw'>More about me</a>.")),
          p(HTML("For questions, contact <b>julianw@umn.edu</b>.")),
          p(HTML("This app is built using <a href='https://shiny.rstudio.com/'>R Shiny</a>. Source code is available <a href='https://github.com/jwolfson/school_openings_mn'>on my GitHub site</a>."),
            p(HTML("Lastly, I am tremendously grateful to <a href='http://www.rstudio.com'>RStudio</a> for providing me with a free service tier upgrade so that this app can handle the traffic it's receiving."))
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
    updatePickerInput(session, "county", selected = ordered.counties[ed$x])
    
  })
  
  observeEvent(event_data('plotly_click', source = "districtcompare"), {
    
    ed <- event_data('plotly_click', source = "districtcompare", session = session)
    ordered.districts <- current_district_rates() %>% arrange(name) %>% pull(name)
    updatePickerInput(session, "district", selected = ordered.districts[ed$x])
    
  })
  
  
  
}

#thematic_shiny()
# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
