library(dash)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(readr)

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
    htmlImg(
      src = app$get_asset_url("logo-1.jpg"),
      id = "logo_image",
      style = list(
        height = "80px",
        width = "auto",
        `margin-bottom` = "10px",
        `padding-left` = 0
      ),
      
    ),
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
    ),
    
    htmlH6(paste("Last Updated: ", Sys.time()),
           style = list(color = "orange")),
      
#### summary stats
      

    htmlH6('Total crimes',
        style=list(textalign = 'center',
                    color = 'white',
                    fontSize = 33)),
    htmlP("32,007",
        style=list(textalign = 'center',
                    color = '#4C78A8',
                    fontSize = 30)),
      
        htmlH6('Total property crimes',
        style=list(textalign = 'center',
                    color = 'white',
                    fontSize = 33)),
    htmlP("21,853",
        style=list(textalign = 'center',
                    color = '#4C78A8',
                    fontSize = 30)),
      
        htmlH6('Total violent crimes',
        style=list(textalign = 'center',
                    color = 'white',
                    fontSize = 33)),
    htmlP("9,114",
        style=list(textalign = 'center',
                    color = '#4C78A8',
                    fontSize =30)),  
      
        htmlH6('Total vehicle collisions',
        style=list(textalign = 'center',
                    color = 'white',
                    fontSize = 33)),
    htmlP("1,040",
        style=list(textalign = 'center',
                    color = '#4C78A8',
                    fontSize = 30)),
      
      ######
      
    toast <- div(
  list(
    dbcButton(
      "About",
      id = "simple-toast-toggle",
      color = "light",
      n_clicks = 0,
      className = "mb-3",
    ),
    dbcToast(
      list(htmlA(
            "GitHub",
            href = "https://github.com/UBC-MDS/safe_vancity",
            style = list(color = "white", "text-decoration" = "underline"),
          ),
          htmlP(
            "The dashboard was created by Arlin Cherian, Victor Francis, Wanying Ye. It is licensed under MIT license. Please visit GitHub for more information."
          ),
          htmlA(
            "Dashboard description",
            style = list(color = "white", "text-decoration" = "underline"),
          ),
          htmlP(
            "This dashboard allows you to see crime incidence in 2021 in Vancouver neighbourhoods. By selecting a neighbourhood from the drop down menu, all the plots in the app will display metrics related to that neighbourhood. The map will display crime density by 'neighbourhood', 'crime type' and by 'month'. You can zoom into the neighbourhood to see specific streets where the crimes have happened. You can use the toggle options on the top right corner of the map to zoom in or out, pan the map and reset axes. The top-right bar plot shows the total reported crimes in a selected neighbourhood by 'day of the week' (default all days). This plot can be filtered using the 'neighbourhood' and 'day of the week' options. Finally, the bottom bar plot shows total reported crimes by crime category in each neighbourhood. Here crime types are grouped by crime categories (Violent, Property and Vehicle Collision). Default view shows the total cases for all crime categories. You can toggle through the tab options. From this plot you can see the top crimes in each neighbourhood in 2021. Some summary stats of overall reported crimes in Vancouver in 2021, total property, violent and vehicle collision crimes are reported at the very top"
          )
          ),
      id = "simple-toast",
      header = "About",
      color = "light",
      icon = "primary",
      dismissable = TRUE,
      is_open = FALSE
    )
  )
),
      
      
      ####
    
    htmlDiv(dccGraph(id = 'van_map')),
      htmlBr(),
    
    htmlDiv(dccGraph(id = 'plot-area')),
    htmlBr(),
    
    htmlLabel(
      "FILTERS",
      className = "fix_label",
      style = list(
        color = "orange",
        textAlign = "center",
        fontSize = 20
      )
    ),
    
    
    htmlLabel('Select crime type', style = list('color' = 'white')),
    dccDropdown(
      id = 'crimetype_selection',
      options = crimetypes_l$TYPE %>% purrr::map(function(col)
        list(label = col, value = col)),
      placeholder = "Select crime type",
      value = "Break and Enter Commercial",
      style = list('color' = 'white')
    ),
        
    htmlLabel('Select neighbourhood', style = list('color' = 'white')),
    dccDropdown(
      id = 'neigh_selection',
      options = levels(as.factor(crime$NEIGHBOURHOOD))  %>%
        purrr::map(function(col)
          list(label = col, value = col)),
      placeholder = "Select neighbourhood",
      value = "West End",
      style = list('color' = 'white')
    ),
        
    htmlLabel('Select month', style = list('color' = 'white')),
    dccDropdown(
      id = 'month_selection',
      options = month_l$month_name %>% purrr::map(function(col)
        list(label = col, value = col)),
      placeholder = "Select Month",
      value = "Jan",
      style = list('color' = 'white')
    ),
        
    htmlLabel('Select Weekday', style = list('color' = 'white')),
    dccDropdown(
      id = "weekday_dropdown",
      value = "All",
      options = list(
        list(label = "Sunday", value = "Sunday"),
        list(label = "Monday", value = "Monday"),
        list(label = "Tuesday", value = "Tuesday"),
        list(label = "Wednesday", value = "Wednesday"),
        list(label = "Thursday", value = "Thursday"),
        list(label = "Friday", value = "Friday"),
        list(label = "Saturday", value = "Saturday")
      )
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
              href = "https://geodash.vpd.ca/opendata/#")
      ),
      className = "fix_label",
      style = list(
        color = "white",
        textAlign = "center",
        `margin-top` = "0px"
      )
    ),
    
    
    
    dccTabs(
      id = "crime_category-widget",
      value = "All",
      children = list(
        dccTab(
          label = "All",
          value = "All",
          style = tab_style,
          selected_style = tab_selected_style,
        ),
        dccTab(
          label = "Violent crimes",
          value = "Violent crimes",
          style = tab_style,
          selected_style = tab_selected_style,
        ),
        dccTab(
          label = "Property crimes",
          value = "Property crimes",
          style = tab_style,
          selected_style = tab_selected_style,
        ),
        dccTab(
          label = "Vehicle collision",
          value = "Vehicle collision",
          style = tab_style,
          selected_style = tab_selected_style,
        )
      )
    ),
    dccGraph(id = 'bar-plot-1')
  )
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


app$callback(output('plot-area', 'figure'),
             list(
               input('weekday_dropdown', 'value'),
               input('neigh_selection', 'value')
             ),
             function(weekday, neighbourhood) {
               if (weekday == "All") {
                 crime_c <- crime %>%
                   add_count(CRIME_CATEGORY)
                 crime_c_new = crime_c %>%
                   filter(NEIGHBOURHOOD == neighbourhood)
                 p <- ggplot(crime_c_new) +
                   aes(x = reorder(CRIME_CATEGORY, n)) +
                   geom_bar(stat = "count", fill = "skyblue3") +
                   labs(
                     title = paste("Total Reported Crimes in", neighbourhood),
                     x = "Crime Category",
                     y = "Number of crime cases"
                   ) +
                   theme(
                     plot.title = element_text(
                       size = 9,
                       face = "bold",
                       color = "#FFFFFF"
                     ),
                     axis.text = element_text(size = 12, color = "#FFFFFF"),
                     axis.title = element_text(size = 12, color = "#FFFFFF"),
                     axis.text.x = element_text(angle = 45),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(fill = "#010915", colour = "#010915"),
                     plot.background = element_rect(fill = "#010915")
                   )
                 
               } else {
                 crime_c <- crime %>%
                   add_count(CRIME_CATEGORY)
                 crime_c_new = crime_c %>%
                   filter(day_of_week == weekday &
                            NEIGHBOURHOOD == neighbourhood)
                 p <- ggplot(crime_c_new) +
                   aes(x = reorder(CRIME_CATEGORY, n)) +
                   geom_bar(stat = "count", fill = "skyblue3") +
                   labs(
                     title = paste("Total Reported Crimes in", neighbourhood, "on", weekday),
                     x = "Crime Category",
                     y = "Number of crime cases"
                   ) +
                   theme(
                     plot.title = element_text(
                       size = 9,
                       face = "bold",
                       color = "#FFFFFF"
                     ),
                     axis.text = element_text(size = 12, color = "#FFFFFF"),
                     axis.title = element_text(size = 12, color = "#FFFFFF"),
                     axis.text.x = element_text(angle = 45),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(fill = "#010915", colour = "#010915"),
                     plot.background = element_rect(fill = "#010915")
                   )
                 
               }
               
               ggplotly(p, height = 625, width = 475) %>% layout(dragmode = 'select')
               
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
        title = paste("Crime density in", neigh_selection),
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
app$callback(
  output("simple-toast", "is_open"),
  list(input("simple-toast-toggle", "n_clicks")),
  function(n) {
    if (n > 0) {
      return(TRUE)
    }
    return(dashNoUpdate())
  }
)

#----------------------------------------------------------------------------#

app$run_server(host = '0.0.0.0')
