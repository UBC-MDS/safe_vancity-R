library(dash)
library(dashBootstrapComponents)
library(dashHtmlComponents)
library(ggplot2)
library(plotly)
library(readr)

app < -
Dash$new(external_stylesheets=dbcThemes$BOOTSTRAP)

#----------------------------------------------------------------------------#
crime < -
    readr: : read_csv(here: : here('data', 'processed', 'crime_clean.csv'))
crime < - crime[-c(27517, 30974, 30976, 30979, 30991, 27284), ]
crime < -
crime % > % mutate(CRIME_CATEGORY=crime_category) % > % select(-crime_category)

#
crimetypes_l < - crime % > % select(TYPE) % > % unique()
neigh_l < - crime % > % select(NEIGHBOURHOOD) % > % unique()
month_l < - crime % > % select(month_name) % > % unique()

#----------------------------------------------------------------------------#

tab_style = list(
    "borderBottom"="1px solid #d6d6d6",
    "color"="white",
    "padding"="6px",
    "backgroundColor"="#010915"
    # 'fontWeight' = 'bold'
)
tab_selected_style = list(
    "borderTop"="4px solid #d6d6d6",
    "borderBottom"="4px solid #d6d6d6",
    "borderLeft"="4px solid #d6d6d6",
    "borderRight"="4px solid #d6d6d6",
    "backgroundColor"="#010915",
    "color"="white",
    "padding"="6px",
    "fontWeight"="bold",
    "fontSize"=20
)

app$layout(htmlDiv(
    list(
        toast < - div(list(
            dbcButton(
                "About",
                id="simple-toast-toggle",
                color="light",
                n_clicks=0,
                className="mb-3",
                style=list(position="fixed", right=90),
            ),
            dbcToast(
                list(
                    htmlA(
                        "GitHub",
                        href="https://github.com/UBC-MDS/safe_vancity",
                        style=list(color="white", "text-decoration"="underline"),
                    ),
                    htmlP(
                        "The dashboard was created by Arlin Cherian, Victor Francis, Wanying Ye. It is licensed under MIT license. Please visit GitHub for more information."
                    ),
                    htmlA(
                        "Dashboard description",
                        style=list(color="white", "text-decoration"="underline"),
                    ),
                    htmlP(
                        "This dashboard allows you to see crime incidence in 2021 in Vancouver neighbourhoods. By selecting a neighbourhood from the drop down menu, all the plots in the app will display metrics related to that neighbourhood. The map will display crime density by 'neighbourhood', 'crime type' and by 'month'. You can zoom into the neighbourhood to see specific streets where the crimes have happened. You can use the toggle options on the top right corner of the map to zoom in or out, pan the map and reset axes. The top-right bar plot shows the total reported crimes in a selected neighbourhood by 'day of the week' (default all days). This plot can be filtered using the 'neighbourhood' and 'day of the week' options. Finally, the bottom bar plot shows total reported crimes by crime category in each neighbourhood. Here crime types are grouped by crime categories (Violent, Property and Vehicle Collision). Default view shows the total cases for all crime categories. You can toggle through the tab options. From this plot you can see the top crimes in each neighbourhood in 2021. Some summary stats of overall reported crimes in Vancouver in 2021, total property, violent and vehicle collision crimes are reported at the very top"
                    )
                ),
                id="simple-toast",
                header="About",
                color="light",
                icon="primary",
                dismissable=TRUE,
                is_open=FALSE
            )
        )),





        htmlDiv(
            list(
                htmlDiv(list(
                    htmlImg(
                        src=app$get_asset_url("logo-1.jpg"),
                        id="logo_image",
                        style=list(
                            height="80px",
                            width="auto",
                            `margin-bottom`="10px",
                            `padding-left`=0
                        ),

                    )
                ), className="one column"),
                htmlDiv(list(htmlDiv(
                    list(
                        htmlH2(
                            "Vancouver Crime Incidence Dashboard",
                            style=list(`margin-bottom`="0px",
                                       color="white",
                                       textalign="right"
                                       ),
                        ),
                        htmlH3(
                            "Incidence for 2021",
                            style=list(`margin-top`="0px",
                                       color="white",
                                       textalign="right"
                                       )
                        )
                    )
                )), className="six column",
                    id="title")
            ),
            id="header",
            className="row flex-display",
            style=list(`margin-bottom`="25px")
        ),



        # summary stats


        htmlDiv(list(
            htmlDiv(list(
                htmlH6(
                    'Total crimes',
                    style=list(
                        textAlign='center',
                        color='white',
                        fontSize=27
                    )
                ),
                htmlP(
                    "32,007",
                    style=list(
                        textAlign='center',
                        color='#4C78A8',
                        fontSize=25
                    )
                )
            ), className="card_container two columns"),


            htmlDiv(list(
                htmlH6(
                    'Total property crimes',
                    style=list(
                        textAlign='center',
                        color='white',
                        fontSize=27
                    )
                ),
                htmlP(
                    "21,853",
                    style=list(
                        textAlign='center',
                        color='#4C78A8',
                        fontSize=25
                    )
                )
            ), className="card_container three columns"),


            htmlDiv(list(
                htmlH6(
                    'Total violent crimes',
                    style=list(
                        textAlign='center',
                        color='white',
                        fontSize=27
                    )
                ),
                htmlP(
                    " 9,114",
                    style=list(
                        textAlign='center',
                        color='#4C78A8',
                        fontSize=25
                    )
                )
            ), className="card_container three columns"),

            htmlDiv(list(
                htmlH6(
                    'Total vehical collision',
                    style=list(
                        textAlign='center',
                        color='white',
                        fontSize=27
                    )
                ),
                htmlP(
                    " 1,040",
                    style=list(
                        textAlign='center',
                        color='#4C78A8',
                        fontSize=25
                    )
                )
            ), className="card_container three columns")

        )),

        ######


        htmlDiv(list(
            htmlDiv(list(htmlDiv(
                dccGraph(
                    id='van_map',
                    style=list(`border-width`="0",
                               width="100%",
                               height="475px"
                               )
                )
            )), className="create_container seven columns"),
            htmlDiv(list(htmlDiv(
                dccGraph(
                    id='plot-area',
                    style=list(`border-width`="0",
                               width="400px",
                               height="475px"
                               )
                )
            )), className="create_container four columns")
        ), className="row flex-display"),


        htmlDiv(list(
            htmlDiv(
                list(
                    htmlLabel(
                        "FILTERS",
                        className="fix_label",
                        style=list(
                            color="orange",
                            textAlign="center",
                            fontSize=20
                        )
                    ),


                    htmlLabel('Select crime type', style=list('color'='white')),
                    dccDropdown(
                        id='crimetype_selection',
                        options = crimetypes_l$TYPE % > % purrr: : map(function(col)
                                                                      list(label = col, value = col)),
                        placeholder="Select crime type",
                        value="Break and Enter Commercial",
                        style=list('color'='white')
                    ),

                    htmlLabel('Select neighbourhood', style=list('color'='white')),
                    dccDropdown(
                        id='neigh_selection',
                        options=levels(as.factor(crime$NEIGHBOURHOOD)) % > %
                        purrr: : map(function(col)
                                    list(label = col, value = col)),
                        placeholder="Select neighbourhood",
                        value="West End",
                        style=list('color'='white')
                    ),

                    htmlLabel('Select month', style=list('color'='white')),
                    dccDropdown(
                        id='month_selection',
                        options = month_l$month_name % > % purrr: : map(function(col)
                                                                       list(label = col, value = col)),
                        placeholder="Select Month",
                        value="Jan",
                        style=list('color'='white')
                    ),

                    htmlLabel('Select Weekday', style=list('color'='white')),
                    dccDropdown(
                        id="weekday_dropdown",
                        value="All",
                        options=list(
                            list(label="Sunday", value="Sunday"),
                            list(label="Monday", value="Monday"),
                            list(label="Tuesday", value="Tuesday"),
                            list(label="Wednesday", value="Wednesday"),
                            list(label="Thursday", value="Thursday"),
                            list(label="Friday", value="Friday"),
                            list(label="Saturday", value="Saturday")
                        )
                    ),


                    htmlLabel(
                        list("Data Source: "),
                        className="fix_label",
                        style=list(
                            color="orange",
                            textAlign="center",
                            `margin-top`="80px"
                        )
                    ),
                    htmlLabel(
                        list(
                            htmlA("VPD Open Source",
                                  href="https://geodash.vpd.ca/opendata/#")
                        ),
                        className="fix_label",
                        style=list(
                            color="white",
                            textAlign="center",
                            `margin-top`="0px"
                        )
                    ),

                    htmlLabel(paste("Last Updated: ", Sys.time()),
                              style=list(color="orange", `margin-top`="60px"))
                ),
                className="create_container three columns"
            ),



            htmlDiv(list(
                dccTabs(
                    id="crime_category-widget",
                    value="All",
                    children=list(
                        dccTab(
                            label="All",
                            value="All",
                            style=tab_style,
                            selected_style=tab_selected_style,
                        ),
                        dccTab(
                            label="Violent crimes",
                            value="Violent crimes",
                            style=tab_style,
                            selected_style=tab_selected_style,
                        ),
                        dccTab(
                            label="Property crimes",
                            value="Property crimes",
                            style=tab_style,
                            selected_style=tab_selected_style,
                        ),
                        dccTab(
                            label="Vehicle collision",
                            value="Vehicle collision",
                            style=tab_style,
                            selected_style=tab_selected_style,
                        )
                    )
                ),
                dccGraph(id='bar-plot-1')
            ), className="create_container eight columns")
        ), className="row flex-display")



    ),
    id="mainContainer",
    style=list(display="flex", `flex-direction`="column")
))

#----------------------------------------------------------------------------#
#frontend/backend
        
app$callback(output('bar-plot-1', 'figure'),
             list(
    input("crime_category-widget", "value"),
    input('neigh_selection', 'value'),
    input('crimetype_selection', 'value')
),
    function(crime_category,
             neighbourhood,
             crime_type) {
    crime < - crime % > %
                 mutate(highlight_col = dplyr: : case_when(TYPE == crime_type ~ TRUE,
                                                          TYPE != crime_type ~ FALSE))
    if (crime_category == "All") {
                     p < - crime % > %
                     filter(NEIGHBOURHOOD == neighbourhood) % > %
                     dplyr:: add_count(TYPE)  % > %
                     ggplot(aes(
                         y=reorder(TYPE, n),
                         fill=highlight_col,
                         text=n
                     )) +
                     geom_bar() +
                     scale_fill_manual("legend", values=c("TRUE"="orange", "FALSE"='#4C78A8')) +
                     ggtitle(paste("Total Reported Cases by Crime Types in", neighbourhood)) +
                     labs(x="Number of crime cases", y="Type of crime") +
                     theme_classic() +
                     theme(
                         legend.position='none',
                         plot.background=element_rect(fill="#010915"),
                         panel.background=element_rect(fill="#010915"),
                         # panel.grid.major = element_blank(),
                         # panel.grid.minor = element_blank(),
                         axis.text.x=element_text(color="#FFFFFF"),
                         axis.text.y=element_text(color="#FFFFFF"),
                         axis.title.x=element_text(
                             face="bold", color="#FFFFFF"),
                         axis.title.y=element_text(
                             face="bold", color="#FFFFFF"),
                         title=element_text(face="bold", color="#FFFFFF")
                     )
                 }
    else {
                     p < - crime % > %
                     filter(NEIGHBOURHOOD == neighbourhood &
                            CRIME_CATEGORY == crime_category) % > %
                     dplyr:: add_count(TYPE)  % > %
                     ggplot(aes(
                         y=reorder(TYPE, n),
                         fill=highlight_col,
                         text=n
                     )) +
                     geom_bar() +
                     scale_fill_manual("legend", values=c("TRUE"="orange", "FALSE"='#4C78A8')) +
                     ggtitle(paste(
                         crime_category,
                         ": Total Reported Cases by Crime Types in",
                         neighbourhood
                     )) +
                     labs(x="Number of crime cases", y="Type of crime") +
                     theme_classic() +
                     theme(
                         legend.position='none',
                         plot.background=element_rect(fill="#010915"),
                         panel.background=element_rect(fill="#010915"),
                         # panel.grid.major = element_blank(),
                         # panel.grid.minor = element_blank(),
                         axis.text.x=element_text(color="#FFFFFF"),
                         axis.text.y=element_text(color="#FFFFFF"),
                         axis.title.x=element_text(
                             face="bold", color="#FFFFFF"),
                         axis.title.y=element_text(
                             face="bold", color="#FFFFFF"),
                         title=element_text(face="bold", color="#FFFFFF")
                     )
                 }
    ggplotly(p, tooltip='text')
})


app$callback(output('plot-area', 'figure'),
             list(
    input('weekday_dropdown', 'value'),
    input('neigh_selection', 'value')
),
    function(weekday, neighbourhood) {
    if (weekday == "All") {
        crime_c < - crime % > %
        dplyr:: add_count(CRIME_CATEGORY)
        crime_c_new=crime_c % > %
        filter(NEIGHBOURHOOD == neighbourhood)
        p < - ggplot(crime_c_new) +
        aes(x=reorder(CRIME_CATEGORY, n)) +
        # , width=0.4, position = position_dodge(width=0.1)
        geom_bar(stat="count", fill='#4C78A8') +
        labs(
            title=paste("Total Reported Crimes in", neighbourhood),
            x="Crime Category",
            y="Number of crime cases"
        ) +
        theme(
            plot.title=element_text(
                size=9,
                face="bold",
                color="#FFFFFF"
            ),
            axis.text=element_text(size=8, color="#FFFFFF"),
            axis.title=element_text(size=8, color="#FFFFFF"),
            axis.text.x=element_text(angle=45),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.background=element_rect(fill="#010915", colour="#010915"),
            plot.background=element_rect(fill="#010915")
        )

    } else {
        crime_c < - crime % > %
        dplyr:: add_count(CRIME_CATEGORY)
        crime_c_new=crime_c % > %
        filter(day_of_week == weekday &
               NEIGHBOURHOOD == neighbourhood)
        p < - ggplot(crime_c_new) +
        aes(x=reorder(CRIME_CATEGORY, n)) +
        geom_bar(stat="count", fill='#4C78A8') +
        labs(
            title=paste("Total Reported Crimes in",
                        neighbourhood, "on", weekday),
            x="Crime Category",
            y="Number of crime cases"
        ) +
        theme(
            plot.title=element_text(
                size=9,
                face="bold",
                color="#FFFFFF"
            ),
            axis.text=element_text(size=12, color="#FFFFFF"),
            axis.title=element_text(size=12, color="#FFFFFF"),
            axis.text.x=element_text(angle=45),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.background=element_rect(fill="#010915", colour="#010915"),
            plot.background=element_rect(fill="#010915")
        )

    }

    ggplotly(p, height=475, width=400) % > % layout(dragmode='select')

})


# map
make_map < -
function(crime,
         crimetype_selection="Break and Enter Commercial",
         neigh_selection="West End",
         month_selection="Jan") {
    crime < - dplyr:: filter(
        crime,
        TYPE == crimetype_selection &
        NEIGHBOURHOOD == neigh_selection &
        month_name == month_selection
    )

    fig < - plot_ly(
        crime,
        lat=~ LAT,
        lon=~ LONG,
        marker=list(color=crimetype_selection),
        type='scattermapbox',
        hovertext=crime[, "NEIGHBOURHOOD"]
    )
    fig % > %
    layout(
        title=paste("Crime density in", neigh_selection),
        font=list(color='white'),
        plot_bgcolor='#010915',
        paper_bgcolor='#010915',
        showlegend=FALSE,
        mapbox=list(
            style='carto-darkmatter',
            zoom=11,
            center=list(lon=-123.116226, lat=49.246292),
            margin=list(
                l=25,
                t=25,
                r=25,
                b=25
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
    van_map < -
                 make_map(crime,
                          crimetype_selection,
                          neigh_selection,
                          month_selection)

    list(van_map)

})
app$callback(output("simple-toast", "is_open"),
             list(input("simple-toast-toggle", "n_clicks")),
             function(n) {
    if (n > 0) {
        return(TRUE)
    }
    return(dashNoUpdate())
})

#----------------------------------------------------------------------------#

app$run_server(host='0.0.0.0')
