install.packages("flexdashboard")
install.packages("devtools")
devtools::install_github("RamiKrispin/coronavirus", force = TRUE)

library(flexdashboard)
library(coronavirus)
library(leaflet)
library(leafpop)
library(purrr)


data(coronavirus)
update_datasets()
View(coronavirus)
max(coronavirus$date)

`%>%` <- magrittr::`%>%`
confirmed_color <- "purple"
active_color <- "#1f77b4"
recovered_color <- "forestgreen"
death_color <- "red"


df <- coronavirus %>%
  # dplyr::filter(date == max(date)) %>%
  dplyr::filter(Country.Region == "India") %>%
  dplyr::group_by(Country.Region, type) %>%
  dplyr::summarise(total = sum(cases)) %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = total
  ) %>%
  # dplyr::mutate(unrecovered = confirmed - ifelse(is.na(recovered), 0, recovered) - ifelse(is.na(death), 0, death)) %>%
  dplyr::mutate(unrecovered = confirmed - ifelse(is.na(death), 0, death)) %>%
  dplyr::arrange(-confirmed) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(country = dplyr::if_else(Country.Region == "Saudi Arabia", "Saudi Arabia", Country.Region)) %>%
  dplyr::mutate(country = dplyr::if_else(country == "Malaysia", "Malaysia", country)) %>%
  dplyr::mutate(country = dplyr::if_else(country == "Austria", "Austria", country)) %>%
  dplyr::mutate(country = trimws(country)) %>%
  dplyr::mutate(country = factor(country, levels = country))

df_daily <- coronavirus %>%
  dplyr::filter(Country.Region == "India") %>%
  dplyr::group_by(date, type) %>%
  dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = total
  ) %>%
  dplyr::arrange(date) %>%
  dplyr::ungroup() %>%
  #dplyr::mutate(active = confirmed - death - recovered) %>%
  dplyr::mutate(active = confirmed - death) %>%
  dplyr::mutate(
    confirmed_cum = cumsum(confirmed),
    death_cum = cumsum(death),
    # recovered_cum = cumsum(recovered),
    active_cum = cumsum(active)
  )


df1 <- coronavirus %>% dplyr::filter(date == max(date))



#Confirmed 



valueBox(
  value = paste(format(sum(df$confirmed), big.mark = ","), "", sep = " "),
  caption = "Total confirmed cases",
  icon = "fas fa-user-md",
  color = confirmed_color
)



#Death



valueBox(
  value = paste(format(sum(df$death, na.rm = TRUE), big.mark = ","), " (",
                round(100 * sum(df$death, na.rm = TRUE) / sum(df$confirmed), 1),
                "%)",
                sep = ""
  ),
  caption = "Death cases (death rate)",
  icon = "fas fa-heart-broken",
  color = death_color
)


#Daily cumulative cases


plotly::plot_ly(data = df_daily) %>%
  plotly::add_trace(
    x = ~date,
    # y = ~active_cum,
    y = ~confirmed_cum,
    type = "scatter",
    mode = "lines+markers",
    # name = "Active",
    name = "Confirmed",
    line = list(color = active_color),
    marker = list(color = active_color)
  ) %>%
  plotly::add_trace(
    x = ~date,
    y = ~death_cum,
    type = "scatter",
    mode = "lines+markers",
    name = "Death",
    line = list(color = death_color),
    marker = list(color = death_color)
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-01-30"),
    y = 1,
    text = paste("First case"),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = -10,
    ay = -90
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-03-12"),
    y = 3,
    text = paste("First death"),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = -90,
    ay = -90
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-03-22"),
    y = 14,
    text = paste(
      "Lockdown"
    ),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = -10,
    ay = -90
  ) %>%
  plotly::layout(
    title = "",
    yaxis = list(title = "Cumulative number of cases"),
    xaxis = list(title = "Date"),
    legend = list(x = 0.1, y = 0.9),
    hovermode = "compare"
  )



#Daily confirmed cases


daily_confirmed <- coronavirus %>%
  dplyr::filter(type == "confirmed") %>%
  dplyr::filter(date >= "2020-03-01") %>%
  dplyr::mutate(country = Country.Region) %>%
  dplyr::group_by(date, country) %>%
  dplyr::summarise(total = sum(cases)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = country, values_from = total)



# Plots

daily_confirmed %>%
  plotly::plot_ly() %>%
  plotly::add_trace(
    x = ~date,
    y = ~France,
    type = "scatter",
    mode = "lines+markers",
    name = "France"
  ) %>%
  plotly::add_trace(
    x = ~date,
    y = ~India,
    type = "scatter",
    mode = "lines+markers",
    name = "India"
  ) %>%
  plotly::add_trace(
    x = ~date,
    y = ~Japan,
    type = "scatter",
    mode = "lines+markers",
    name = "Japan"
  ) %>%
  plotly::add_trace(
    x = ~date,
    y = ~Singapore,
    type = "scatter",
    mode = "lines+markers",
    name = "Singapore"
  ) %>%
  
  plotly::layout(
    title = "",
    legend = list(x = 0.1, y = 0.9),
    yaxis = list(title = "Number of new confirmed cases"),
    xaxis = list(title = "Date"),
    # paper_bgcolor = "black",
    # plot_bgcolor = "black",
    # font = list(color = 'white'),
    hovermode = "compare",
    margin = list(
      # l = 60,
      # r = 40,
      b = 10,
      t = 10,
      pad = 2
    )
  )


# Cases distribution


df_COMP <- coronavirus %>%
  # dplyr::filter(date == max(date)) %>%
  dplyr::filter(Country.Region == "France" |
                  Country.Region == "India" |
                  Country.Region == "Japan" |
                  Country.Region == "Singapore") %>%
  dplyr::group_by(Country.Region, type) %>%
  dplyr::summarise(total = sum(cases)) %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = total
  ) %>%
  # dplyr::mutate(unrecovered = confirmed - ifelse(is.na(recovered), 0, recovered) - ifelse(is.na(death), 0, death)) %>%
  dplyr::mutate(unrecovered = confirmed - ifelse(is.na(death), 0, death)) %>%
  dplyr::arrange(confirmed) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(country = dplyr::if_else(Country.Region == "United Arab Emirates", "UAE", Country.Region)) %>%
  dplyr::mutate(country = dplyr::if_else(country == "Mainland China", "China", country)) %>%
  dplyr::mutate(country = dplyr::if_else(country == "North Macedonia", "N.Macedonia", country)) %>%
  dplyr::mutate(country = trimws(country)) %>%
  dplyr::mutate(country = factor(country, levels = country))

plotly::plot_ly(
  data = df_COMP,
  x = ~country,
  # y = ~unrecovered,
  y = ~ confirmed,
  # text =  ~ confirmed,
  # textposition = 'auto',
  type = "bar",
  name = "Confirmed",
  marker = list(color = active_color)
) %>%
  plotly::add_trace(
    y = ~death,
    # text =  ~ death,
    # textposition = 'auto',
    name = "Death",
    marker = list(color = death_color)
  ) %>%
  plotly::layout(
    barmode = "stack",
    yaxis = list(title = "Total cases"),
    xaxis = list(title = ""),
    hovermode = "compare",
    margin = list(
      # l = 60,
      # r = 40,
      b = 10,
      t = 10,
      pad = 2
    )
  )


#ratio

View(df_COMP)

r_death <- df_COMP$death/df_COMP$confirmed

r_recov <- df_COMP$recovered/df_COMP$confirmed

r_treat <- (df_COMP$unrecovered/df_COMP$confirmed) - (df_COMP$death/df_COMP$confirmed)

r_nrecov <- df_COMP$unrecovered/df_COMP$confirmed



# World Map

cv_data_for_plot <- coronavirus %>%
  # dplyr::filter(Country.Region == "India") %>%
  dplyr::filter(cases > 0) %>%
  dplyr::group_by(Country.Region, Province.State, Lat, Long, type) %>%
  dplyr::summarise(cases = sum(cases)) %>%
  dplyr::mutate(log_cases = 2 * log(cases)) %>%
  dplyr::ungroup()
cv_data_for_plot.split <- cv_data_for_plot %>% split(cv_data_for_plot$type)
pal <- colorFactor(c("orange", "red", "green"), domain = c("confirmed", "death", "recovered"))
map_object <- leaflet() %>% addProviderTiles(providers$Stamen.Toner)
names(cv_data_for_plot.split) %>%
  purrr::walk(function(df) {
    map_object <<- map_object %>%
      addCircleMarkers(
        data = cv_data_for_plot.split[[df]],
        lng = ~Long, lat = ~Lat,
        #                 label=~as.character(cases),
        color = ~ pal(type),
        stroke = FALSE,
        fillOpacity = 0.8,
        radius = ~log_cases,
        popup = leafpop::popupTable(cv_data_for_plot.split[[df]],
                                    feature.id = FALSE,
                                    row.numbers = FALSE,
                                    zcol = c("type", "cases", "Country.Region", "Province.State")
        ),
        group = df,
        #                 clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
        labelOptions = labelOptions(
          noHide = F,
          direction = "auto"
        )
      )
  })

map_object %>%
  addLayersControl(
    overlayGroups = names(cv_data_for_plot.split),
    options = layersControlOptions(collapsed = FALSE)
  )


