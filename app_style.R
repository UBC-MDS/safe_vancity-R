library(dash)
library(dashBootstrapComponents)
library(dashHtmlComponents)
library(ggplot2)
library(plotly)
library(tidyverse)

app <-
  Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

#----------------------------------------------------------------------------#
crime <-
  readr::read_csv(here::here('data', 'processed', 'crime_clean.csv'))
crime <- crime[-c(27517, 30974, 30976, 30979, 30991, 27284),]
crime <-
  crime %>% mutate(CRIME_CATEGORY = crime_category) %>% select(-crime_category)

#
crimetypes_l <- crime %>% select(TYPE) %>% unique()
neigh_l <- crime %>% select(NEIGHBOURHOOD) %>% unique()
month_l <- crime %>% select(month_name) %>% unique()
#----------------------to be moved (if we decide not to write a cleaning script for R)

# crime <- crime  %>%
#   mutate(
#     CRIME_CATEGORY = case_when(
#       TYPE %in% c("Offence Against a Person",
#                   "Mischief",
#                   "Homicide") ~ "Violent crimes",
#       TYPE %in% c(
#         "Theft from Vehicle",
#         "Break and Enter Commercial",
#         "Break and Enter Residential/Other",
#         "Theft of Bicycle",
#         "Theft of Vehicle",
#         "Other Theft"
#       ) ~ "Property crimes",
#       TYPE %in% c(
#         "Vehicle Collision or Pedestrian Struck (with Injury)",
#         "Vehicle Collision or Pedestrian Struck (with Fatality)"
#       ) ~ "Vehicle collision"
#     )
#   )  %>%
#   mutate(
#     TYPE = case_when(
#       TYPE == "Vehicle Collision or Pedestrian Struck (with Fatality)" ~ "Vehicle collision, Fatal",
#       TYPE == "Vehicle Collision or Pedestrian Struck (with Injury)" ~ "Vehicle collision, Injured",
#       TRUE ~ TYPE
#     )
#   )
#----------------------to be moved

tab_style = list(
  "borderBottom" = "1px solid #d6d6d6",
  "color" = "white",
  "padding" = "6px",
  "backgroundColor" = "#010915"
  # 'fontWeight' = 'bold'
)
tab_selected_style = list(
  "borderTop" = "4px solid #d6d6d6",
  "borderBottom" = "4px solid #d6d6d6",
  "borderLeft" = "4px solid #d6d6d6",
  "borderRight" = "4px solid #d6d6d6",
  "backgroundColor" = "#010915",
  "color" = "white",
  "padding" = "6px",
  "fontWeight" = "bold",
  "fontSize" = 20
)


app$layout(htmlDiv(
  list(
    htmlDiv(
      list(
        htmlDiv(list(
          htmlImg(
            src = app$get_asset_url("logo-1.jpg"),
            id = "logo_image",
            style = list(
              height = "80px",
              width = "auto",
              `margin-bottom` = "10px",
              `padding-left` = 0
            ),
          )
        ),
        className = "one column"),
        htmlDiv(list(htmlDiv(
          list(
            htmlH2(
              "Vancouver Crime Incidence Dashboard",
              style = list(
                `margin-bottom` = "0px",
                color = "white",
                textalign = "right"
              ),
            ),
            htmlH3(
              "Incidence for 2021",
              style = list(
                `margin-top` = "0px",
                color = "white",
                textalign = "right"
              )
            )
          )
        )),
        className = "six column",
        id = "title"),
        htmlDiv(list(
          htmlH6("Last Updated: ",
                 # + str(
                 #   pd.to_datetime("now", utc=True)
                 #   .tz_convert("US/Pacific")
                 #   .strftime("%m/%d/%Y, %H:%M:%S")),
                 style = list(color = "orange")
          )),
          className = "one-third column",
          id = "title1"
        )
      ),
      id = "header",
      className = "row flex-display",
      style = list(`margin-bottom` = "25px")
    ),
    htmlDiv(list(
      htmlDiv(dccGraph(id = 'van_map'),
              className = "create_container ten columns"),
      htmlDiv(# list(
        #   html.Iframe(
        #     id="hist",
        #     style=list(
        #       border-width = "0",
        #       width = "400px",  # "100%",
        #       height = "475px"
        #     ),
        #   )
        # ),
        className = "create_container five columns")
    ),
    className = "row flex-display"),
    htmlDiv(
      list(htmlDiv(
        list(
          htmlLabel(
            "FILTERS",
            className = "fix_label",
            style = list(
              color = "orange",
              textAlign = "center",
              fontSize = 20
            )
          ),
          htmlLabel(
            list(
              "Select the crime type",
              dccDropdown(
                id = 'crimetype_selection',
                options = crimetypes_l$TYPE %>% purrr::map(function(col)
                  list(label = col, value = col)),
                placeholder = "Select crime type",
                value = "Break and Enter Commercial",
                style = list('color' = 'white')
              )
            ),
            className = "fix_label",
            style = list(color = "white"),
          ),
          htmlLabel(
            list(
              "Select the neighborhood",
              dccDropdown(
                id = 'neigh_selection',
                options = levels(as.factor(crime$NEIGHBOURHOOD))  %>%
                  purrr::map(function(col)
                    list(label = col, value = col)),
                placeholder = "Select neighbourhood",
                value = "West End",
                style = list('color' = 'white'))
            ),
            className = "fix_label",
            style = list(color = "white")
          ),
          htmlLabel(
            list(
              "Select the month",
              dccDropdown(
                id = 'month_selection',
                options = month_l$month_name %>% purrr::map(function(col)
                  list(label = col, value = col)),
                placeholder = "Select Month",
                value = "Jan",
                style = list('color' = 'white')
              )
            ),
            className = "fix_label",
            style = list(color = "white")
          ),
          htmlLabel(
            list("Select the weekday",
                 # dccDropdown(
                 #   id = 'weekday_selection',
                 #   options = month_l$month_name %>% purrr::map(function(col) list(label = col, value = col)),
                 #   placeholder = "Select Month",
                 #   value = "Jan",
                 #   style = list('color' = 'white'))),
                 className = "fix_label",
                 style = list(color = "white")
            ),
            htmlLabel(
              list("Data Source: "),
              className = "fix_label",
              style = list(
                color = "orange",
                textAlign = "center",
                `margin-top` = "80px"
              )
            ),
            htmlLabel(
              list(
                htmlA("VPD Open Source",
                       href = "https://geodash.vpd.ca/opendata/#",)
              ),
              className = "fix_label",
              style = list(
                color = "white",
                textAlign = "center",
                `margin-top` = "0px"
              ),
            ),
          ),
          className = "create_container three columns"
        )),
        htmlDiv(list(
          dccTabs(
            id = "crime_category-widget",
            value = "All",
            children = list(
              dccTab(
                label = "All",
                value = "All",
                style = tab_style,
                selected_style = tab_selected_style
              ),
              dccTab(
                label = "Violent crimes",
                value = "Violent crimes",
                style = tab_style,
                selected_style = tab_selected_style
              ),
              dccTab(
                label = "Property crimes",
                value = "Property crimes",
                style = tab_style,
                selected_style = tab_selected_style
              ),
              dccTab(
                label = "Vehicle collision",
                value = "Vehicle collision",
                style = tab_style,
                selected_style = tab_selected_style
              )
            )
          ),
          dccGraph(id = 'bar-plot-1')
        ),
        className = "create_container nine columns")
      ),
      className = "row flex-display"
    )),
  id = "mainContainer",
  style = list(display = "flex", `flex-direction` = "column")
))


app$callback(output('bar-plot-1', 'figure'),
             list(
               input("crime_category-widget", "value"),
               input('neigh_selection', 'value')
             ),
             function(crime_category, neighbourhood) {
               if (crime_category == "All") {
                 p <- crime %>%
                   filter(NEIGHBOURHOOD == neighbourhood)  %>%
                   add_count(TYPE)  %>%
                   ggplot(aes(y = reorder(TYPE, n), text = n)) +
                   geom_bar(fill = '#aec7e8') +
                   ggtitle(paste("Total Reported Cases by Crime Types in", neighbourhood)) +
                   labs(x = "Number of crime cases", y = "Type of crime") +
                   theme_classic() +
                   theme(
                     plot.background = element_rect(fill = "#010915"),
                     panel.background = element_rect(fill = "#010915"),
                     # panel.grid.major = element_blank(),
                     # panel.grid.minor = element_blank(),
                     axis.text.x = element_text(color = "#FFFFFF"),
                     axis.text.y = element_text(color = "#FFFFFF"),
                     axis.title.x = element_text(face = "bold", color = "#FFFFFF"),
                     axis.title.y = element_text(face = "bold", color = "#FFFFFF"),
                     title = element_text(face = "bold", color = "#FFFFFF")
                   )
               }
               else {
                 p <- crime  %>%
                   filter(NEIGHBOURHOOD == neighbourhood &
                            CRIME_CATEGORY == crime_category)  %>%
                   add_count(TYPE)  %>%
                   ggplot(aes(y = reorder(TYPE, n), text = n)) +
                   geom_bar(fill = '#aec7e8') +
                   ggtitle(paste(
                     crime_category,
                     ": Total Reported Cases by Crime Types in",
                     neighbourhood
                   )) +
                   labs(x = "Number of crime cases", y = "Type of crime") +
                   theme_classic() +
                   theme(
                     plot.background = element_rect(fill = "#010915"),
                     panel.background = element_rect(fill = "#010915"),
                     # panel.grid.major = element_blank(),
                     # panel.grid.minor = element_blank(),
                     axis.text.x = element_text(color = "#FFFFFF"),
                     axis.text.y = element_text(color = "#FFFFFF"),
                     axis.title.x = element_text(face = "bold", color = "#FFFFFF"),
                     axis.title.y = element_text(face = "bold", color = "#FFFFFF"),
                     title = element_text(face = "bold", color = "#FFFFFF")
                   )
               }
               ggplotly(p, tooltip = 'text')
             })

#----------------------------------------------------------------------------#
#map
make_map <-
  function(crime,
           crimetype_selection = "Break and Enter Commercial",
           neigh_selection = "West End",
           month_selection = "Jan") {
    crime <- dplyr::filter(
      crime,
      TYPE == crimetype_selection &
        NEIGHBOURHOOD == neigh_selection &
        month_name == month_selection
    )
    
    fig <- plot_ly(
      crime,
      lat = ~ LAT,
      lon = ~ LONG,
      marker = list(color = crimetype_selection),
      type = 'scattermapbox',
      hovertext = crime[, "NEIGHBOURHOOD"]
    )
    fig %>%
      layout(
        title = 'Crime Density in Vancouver Neighbourhoods',
        font = list(color = 'white'),
        plot_bgcolor = '#010915',
        paper_bgcolor = '#010915',
        mapbox = list(
          style = 'carto-darkmatter',
          zoom = 10,
          center = list(lon = -123.116226, lat = 49.246292),
          margin = list(
            'r' = 25,
            't' = 25,
            'l' = 25,
            'b' = 25
          )
        )
      )
  }

app$callback(list(output('van_map', 'figure')),
             list(
               input('crimetype_selection', 'value'),
               input('neigh_selection', 'value'),
               input('month_selection', 'value')
             ),
             function(crimetype_selection,
                      neigh_selection,
                      month_selection) {
               van_map <-
                 make_map(crime,
                          crimetype_selection,
                          neigh_selection,
                          month_selection)
               
               list(van_map)
               
             })

#----------------------------------------------------------------------------#

app$run_server(debug = T)