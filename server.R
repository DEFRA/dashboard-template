
#-------------------------------------------------------------------#
#                           S E R V E R                             #
#-------------------------------------------------------------------#


# This is the server logic of this Shiny Dashboard. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram

shinyServer(function(input, output, session) {
    
    # KEY STAT LINKS ################################################
    # Links from key stats to dashboard page (page order number gives the correct page)
    
    # Observes the button press for each key stat then navigates to the appropriate page
    
    # Biopesticides Button
    observeEvent(input$btn_page2, {
        updatebs4TabSetPanel(session, inputId = "sidebar", selected = 2)
    })
    
    # Button Page 3 | 1
    observeEvent(input$btn_page3_1, {
        updatebs4TabSetPanel(session, inputId = "sidebar", selected = 4)
    })
    
    
    # Button Page 3 | 2
    observeEvent(input$btn_page3_2, {
        updatebs4TabSetPanel(session, inputId = "sidebar", selected = 5)
    })
    
    # Button Page 3 | 3
    observeEvent(input$btn_page3_3, {
        updatebs4TabSetPanel(session, inputId = "sidebar", selected = 6)
    })
    
    
    # DASHBOARD THEME ###############################################
    # Determines whether light or dark theme is used based on toggle in UI.R
    observeEvent(input$DarkLight, {
        if(input$DarkLight == TRUE){
            output$theme_ui <- renderUI({
                use_theme(Defra_Theme_Dark)
            })
        }else{
            output$theme_ui <- renderUI({
                use_theme(Defra_Theme_Light)
            })
            NULL
        }
    })
    
    # KEY STAT ARROW ICON DIRECTION #############################################
    iconArrowDirect <- function(var){
        if(var > 0){"arrow-up"} else if (var == 0){"equals"} else {"arrow-down"}
    }
    
    # PAGE STYLING ##############################################################
    # Determines styling for Card Colours, Graph Colours and GGiraph colours

    ### DEFRA LOGO ###
    # This is the logo that shows on the Dashboard sidebar. It varies between light and dark dependendent on the theme.
    
    output$imgDefraSidebarLogo <- renderImage({
        if (input$DarkLight == TRUE){filename <- "www/DefraLogoWhite.png"}else{filename <- "www/DefraLogo.png"}
        list(src = filename,width = "150")
    }, deleteFile = FALSE)
    
    
    ### GRAPH STYLING ###
    # GGPlot Theme Code
    # This selects between the two graph themes - dark and light - that are defined in theme.R
    defra_style <- reactive({
        if (input$DarkLight == TRUE){dark()}else{light()}})
    
    # GGPlot Colour Scheme
    CSGraph <- reactive({
        if (input$DarkLight == TRUE){CSDarkGraph}else{CSLightGraph}})
    
    # GGPlot Colour theme reverse order (provides some variety)
    CSGraph2 <- reactive({
        if (input$DarkLight == TRUE){CSDarkGraph[10:1]}else{CSLightGraph[10:1]}})
    
    # GGPlot static line colour (light or dark)
    CSGraphLine <- reactive({
        if (input$DarkLight == TRUE){CSDarkGraphLine}else{CSLightGraphLine}})
    
    # GGiraph is the layer that makes graphs interactive
    
    # GGiraph Theme
    themeOptions <- reactive({
        if (input$DarkLight == TRUE){themeOptions_dark}else{themeOptions_light}})
    
    # GGiraph Line plot Theme
    themeOptions_line <- reactive({
        if (input$DarkLight == TRUE){themeOptions_line_dark}else{themeOptions_line_light}})
    
    
    ### DASHBOARD CARDS ###
    # Card background colour - switches between light and dark theme
    
    # Background Colour Scheme for dashboard cards
    CSColour <- reactive({
        if (input$DarkLight == TRUE){CSCardDarkColour}else{CSCardLightColour}})
    
    # Text Colour Scheme
    CSTextColour <- reactive({
        if (input$DarkLight == TRUE){CSCardLightColour}else{CSCardDarkColour}})
    
    # RENDERING THE DASHBOARD ########################################################
    
    # The observeEvent checks whether the dashboard is in a light or dark theme. Everything that the 
    # user sees is rendered in the correct theme dependent on this condition.
    
    observeEvent(input$DarkLight, {
        if(input$DarkLight == TRUE){
            ColorBox <- "dark"
            ColorTitle <- "transparent"
        }else{
            ColorBox <- "light"
            ColorTitle <- "light"
        }
        
        # WELCOME PAGE ################################
        
        output$Welcome <- renderUI({
            fluidPage(
                # Here we see the "card" functions being called from the theme.R files.
                # Fluid rows create flexible rows that scale with the browser size. They are split into columns of 12.
                fluidRow(
                    cardTitle(dashboardTitle, ColorTitle),
                    cardAbout("Welcome Page", About_dash, ColorBox, collapsed = TRUE, width = 4),
                    cardAbout("Instructions", Instructions_dash, ColorBox, collapsed = TRUE, width = 4),
                    cardAbout("Suggestions", suggest_dash, ColorBox, collapsed = TRUE, width = 4)
                ),
                
                fluidRow(
                    column(width = 3,
                           cardKeyStat(P2_KS1[1], P2_KS1[2], "sun-o", keyStat_colour[3], width = NULL, footer = actionLink(inputId = "btn_page2",icon = icon("arrow-circle-right"),
                                                                                                                               label = "Page 2", style="color: #fff"))
                    ),
                    column(width = 3,
                           cardKeyStat(P3_1_KS1[1], P3_1_KS1[2], "apple-alt", keyStat_colour[4], width = NULL, footer = actionLink(inputId = "btn_page3_1",icon = icon("arrow-circle-right"),
                                                                                                                                   label = "Page 3 | 1", style="color: #fff"))
                    ),
                    column(width = 3,
                           cardKeyStat(P3_2_KS1[1], P3_2_KS1[2], "eye-dropper", keyStat_colour[5], width = NULL, footer = actionLink(inputId = "btn_page3_2",icon = icon("arrow-circle-right"),
                                                                                                                                     label = "Page 3 | 2", style="color: #fff"))
                    ),
                    column(width = 3,
                           cardKeyStat(P3_3_KS1[1], P3_3_KS1[2], "leaf", keyStat_colour[6], width = NULL, footer = actionLink(inputId = "btn_page3_3",icon = icon("arrow-circle-right"),
                                                                                                                              label = "Page 3 | 3", style="color: #fff"))
                    )
                )
            )
        })
        
        # PAGE 2 ################################
        output$Page_2 <- renderUI({
            fluidPage(
                fluidRow(
                    cardTitle("Page 2", ColorTitle),
                    cardKeyStat(P2_KS1[1],P2_KS1[2],"tint", keyStat_colour[1], width = 4),
                    cardKeyStat(P2_KS2[1],P2_KS2[2],"tint", keyStat_colour[2], width = 4),
                    cardKeyStat(P2_KS3[1],P2_KS3[2],"tint", keyStat_colour[3], width = 4),
                ),
                fluidRow(
                    cardAbout("About Page 2", summary_page2, ColorBox),
                    cardTable("table_page2", ColorBox),
                    cardGraph("NSTS Number of tests", "nsts_data_graph", ColorBox, width = 4),
                    cardGraph("NSTS Percentage of sprayed area covered", "nsts_percent_graph", ColorBox, width = 4),
                    cardGraph("NSTS Number of tests by region", "nsts_region_graph", ColorBox, width = 4),
                    cardAboutSource(id = "AS_page2", about_page2, source_page2, ColorBox)
                )
            )
        })
        
        # PAGE 3 | SUB PAGE 1 ##############################
        output$Sub_Page_1 <- renderUI({
            fluidPage(
                fluidRow(
                    cardTitle("Page 3 | Sub-page 1", ColorTitle),
                    cardKeyStat(P3_1_KS1[1],P3_1_KS1[2],"apple-alt", keyStat_colour[1], width = 4),
                    cardKeyStat(P3_1_KS2[1],P3_1_KS2[2],"apple-alt", keyStat_colour[2], width = 4),
                    cardKeyStat(P3_1_KS3[1],P3_1_KS3[2],"apple-alt", keyStat_colour[3], width = 4)
                ),
                fluidRow(
                    cardAbout("About Page 3 | 1", summary_page3_1, ColorBox, width = 6),
                    cardAbout("More Info on Page 3 | 1", more_info_page3_1, "fuchsia", width = 6),
                    cardTable("table_page3_1", ColorBox, width = 12),
                    cardDisplayOptions2(
                        airYearpickerInput(
                            inputId = "p3_1_datePicker",
                            label = "Years to Display",
                            range = TRUE,
                            multiple = 2,
                            value = c(first(p3_1_data_final$Year),last(p3_1_data_final$Year)),
                            dateFormat = "yyyy",
                            minDate = first(p3_1_data_final$Year),
                            maxDate = last(p3_1_data_final$Year),
                            placeholder = "Enter a date range",
                            clearButton = TRUE
                        ),
                        pickerInput(
                            inputId = "p3_1_picker",
                            label = "A multiple select list", 
                            multiple = TRUE,
                            selected = first(sort(unique(p3_1_data_final$Area_Usage))),
                            choices = sort(unique(p3_1_data_final$Area_Usage)),
                            options = list(
                                `live-search` = TRUE 
                            )
                        ),
                        ColorBox),
                    cardSubTitle(textOutput("p3_1_title"), ColorTitle),
                    cardGraph("Area Chart Sample", "p3_1_graph", ColorBox),
                    cardGraph(textOutput("p3_1_title_graph"), "p3_1_graph_2", ColorBox),
                    cardAboutSource(id = "AS_organics", about_page3_1, source_page3_1, ColorBox)
                )
            )

        })
        
        # PAGE 3 | SUB PAGE 2 ##############################
        output$Sub_Page_2 <- renderUI({
            fluidPage(
                fluidRow(
                    cardTitle("Page 3 | Sub-page 2", ColorTitle),
                    cardKeyStat(P3_2_KS1[1],P3_2_KS1[2],"plus", keyStat_colour[6]),
                    cardKeyStat(P3_2_KS2[1],P3_2_KS2[2],"plus", keyStat_colour[7])
                ),
                fluidRow(
                    cardAbout("About Page 3 | 2", summary_page3_2, ColorBox),
                    cardTable("table_page3_2", ColorBox),
                    cardGraph("Bar Plot", "p3_2_graph", ColorBox),
                    cardGraph("Stacked Bar Chart", "p3_2_graph_2", ColorBox),
                    cardAboutSource(id = "AS_page3_2", about_page3_2, source_page3_2, ColorBox)
                    
                )
            )
        })
        
        # PAGE 3 | SUB PAGE 3 ############################################
        output$Sub_Page_3 <- renderUI({
            fluidPage(
                fluidRow(
                    cardTitle("Page 3 | Sub-page 3", ColorTitle),
                    cardKeyStat(P3_3_KS1[1],P3_3_KS1[2],iconArrowDirect(p3_3_data_latest_1), keyStat_colour[1]),
                    cardKeyStat(P3_3_KS2[1],P3_3_KS2[2],iconArrowDirect(p3_3_data_latest_2), keyStat_colour[2]),
                    cardKeyStat(P3_3_KS3[1],P3_3_KS3[2],iconArrowDirect(p3_3_data_latest_3), keyStat_colour[3]),
                    cardKeyStat(P3_3_KS4[1],P3_3_KS4[2],iconArrowDirect(p3_3_data_latest_4), keyStat_colour[4])
                ),
                fluidRow(
                    cardAbout("About Description", summary_page3_3, ColorBox, width = 4),
                    cardTable("table_page3_3", ColorBox),
                    cardGraph("Ribbon Chart showing errors", "p3_3_graph", ColorBox),
                    cardGraph("Set of 4 grouped line charts", "p3_3_graph_2", ColorBox),
                    cardAboutSource(id = "AS_page3",about_page3_3,source_page3_3, ColorBox) # <- The "id" must be unique for each cardAboutSource
                )
            )
        })
        
    }
    )
    # PAGE 2 | DATA #######################################################################
    output$nsts_data_graph <- renderGirafe({
        girafe(ggobj = ggplot(p2_1_data_tests) 
               + geom_point_interactive(alpha = 0.01, size=16, aes(x = Year, y = Value, colour = Tests_Spray, group = Tests_Spray,
                                                                   tooltip = c(paste0("Year: ", Year, "\n Number of Tests: ", format(round(Value, 2), nsmall = 2))), 
                                                                   data_id = Year))
               + geom_line_interactive(size = 1.5, aes(x = Year, y = Value, group = Tests_Spray, colour = Tests_Spray, data_id = Tests_Spray))
               + defra_style() + scale_colour_manual(values=CSGraph()) 
               + xlab("Year") + ylab("Number of tests")
               + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)), width_svg = widthSVG, height_svg = heightSVG, options = themeOptions_line())
    })
    
    output$nsts_percent_graph <- renderGirafe({
        girafe(ggobj = ggplot(p2_1_data_percent) 
               + geom_point_interactive(alpha = 0.01, size=16, aes(x = Year, y = Value, colour = Tests_Spray, group = Tests_Spray, 
                                                                   tooltip = c(paste0("Year: ", Year, "\n Number of Tests: ", format(round(Value, 2), nsmall = 2))), 
                                                                   data_id = Year))
               + geom_line_interactive(size = 1.5, aes(x = Year, y = Value, group = Tests_Spray, colour = Tests_Spray, data_id = Tests_Spray))
               + defra_style() + scale_colour_manual(values=CSGraph()[7]) 
               + xlab("Year") + ylab("Number of tests")
               + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)), width_svg = widthSVG, height_svg = heightSVG, options = themeOptions_line())
    })
    
    output$nsts_region_graph <- renderGirafe({
        girafe(ggobj = ggplot(p2_1_region_data) 
               + geom_bar_interactive(position="stack", stat="identity", aes(x = Year, y = Value, group = Region, fill = Region, 
                                                                             tooltip = c(paste0("Year: ", year(Year), "\n Region: ", Region, "\n Number of tests: ", format(round(Value, 2), nsmall = 2))), 
                                                                             data_id = Year))
               + defra_style() + scale_fill_manual(values=CSGraph()) 
               + xlab("Year") + ylab("Number of tests")
               + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)), width_svg = widthSVG, height_svg = heightSVG, options = themeOptions_line())
    })
    
    # PAGE 3 | SUB_PAGE 1 | DATA ########################################################
    # TITLE ###
    output$p3_1_title <- renderText({paste0("Land-use & Crop Area |", paste(" ", year(input$dateRange_org_la), collapse = " to "))})
    output$p3_1_title_graph <- renderText({paste("Land Area for ", input$detail_org_la)})
    
    # Selects regions clicked in the overview stacked area plot
    observeEvent(input$land_use_org_map_selected, {
        updatePickerInput(session, "detail_org_la", selected = input$land_use_org_map_selected)
    })
    
    
    # DATA ###
    
    data_p3_1_detailed <- reactive({
        req(input$p3_1_datePicker)
        req(input$detail_org_la)
        filter(p3_1_data_final, Area_Usage %in% input$p3_1_picker)
    })
    
    data_p3_1_overview <- reactive({
        req(input$p3_1_datePicker)
        p3_1_data_final
    })
    
    lim_min_p3_1_datePicker <- reactive({
        min(input$p3_1_datePicker)
    })
    
    lim_max_p3_1_datePicker <- reactive({
        max(input$p3_1_datePicker)
    })
    
    
    output$p3_1_graph <- renderGirafe({
        girafe(ggobj = ggplot(data_p3_1_overview())
               + geom_area_interactive(aes(x = Year, y = Area, group = Area_Usage, fill = Area_Usage, 
                                           data_id = Area_Usage, tooltip = Area_Usage))
               + defra_style() + scale_colour_manual(values=CSGraph()) + scale_fill_manual(values=alpha(CSGraph())) 
               + xlab("Year") + ylab("Area (Million Hectares)")
               + guides(colour=FALSE)
               + scale_x_date(date_labels = "%Y", date_breaks = "2 year", limits = c(lim_min_p3_1_datePicker(), lim_max_p3_1_datePicker())), 
               width_svg = widthSVG, height_svg = heightSVG, options = themeOptions_line())
    })
    
    output$p3_1_graph_2 <- renderGirafe({
        
        girafe(ggobj = ggplot(data_p3_1_detailed()) 
               + geom_point_interactive(alpha = 0.01, size=16, aes(x = Year, y = Area, group = Area_Usage, colour = Area_Usage, 
                                                                   tooltip = c(paste0("Year: ", year(Year), "\n Type: ", Area_Usage, "\n Area (million Hectares): ", format(round(Area, 3), nsmall = 3))), 
                                                                   data_id = Year))
               + geom_line_interactive(size = 1.5, aes(x = Year, y = Area, group = Area_Usage, colour = Area_Usage,data_id = Area_Usage))
               + defra_style() + scale_colour_manual(values=CSGraph()) + scale_fill_manual(values=alpha(CSGraph()))
               + scale_x_date(date_labels = "%Y", date_breaks = "2 year", limits = c(lim_min_p3_1_datePicker(), lim_max_p3_1_datePicker()))
               + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
               + xlab("Year") + ylab("Area (Million Hectares)"),
               width_svg = widthSVG, height_svg = heightSVG, options = themeOptions_line())
    })
    
    
    # PAGE 3 | SUB_PAGE 2 | DATA ########################################################
    
    output$p3_2_graph <- renderGirafe({
        girafe(ggobj = ggplot(p3_2_data) 
               + geom_bar_interactive(position="dodge", stat="identity", aes(x = Year, y = Value, group = NULL, fill = Type, 
                                                                             tooltip = c(paste0("Year: ", year(Year), "\n Number: ", format(round(Value, 2), nsmall = 2))), 
                                                                             data_id = Year))
               + defra_style() + scale_fill_manual(values=CSGraph2()) 
               + xlab("Year") + ylab("Number")
               + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)), width_svg = widthSVG, height_svg = heightSVG, options = themeOptions_line())
    })
    
    output$p3_2_graph_2 <- renderGirafe({
        girafe(ggobj = ggplot(p3_2_data_2) 
               + geom_bar_interactive(position="stack", stat="identity", aes(x = Member_State, y = Value, group = Type, fill = Type, 
                                                                             tooltip = c(paste0("Member State: ", Member_State, "\n Number: ", format(round(Value, 2), nsmall = 2))), 
                                                                             data_id = Member_State))
               + defra_style() + scale_fill_manual(values=CSGraph2()) 
               + xlab("Age of Patient") + ylab("Cases")
               + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)), width_svg = widthSVG, height_svg = heightSVG, options = themeOptions_line())
    })
    
    # PAGE 3 | SUB_PAGE 3 | DATA ########################################################
    
    output$p3_3_graph <- renderGirafe({
        girafe(ggobj = ggplot(df_master_p3_3_1_data)
               + geom_ribbon_interactive(aes(x = Year, ymax = UpperConfidence, ymin = LowerConfidence, fill = Species, colour = NA))
               + geom_point_interactive(alpha = 0.01, size=16, aes(x = Year, y = Indicator, group = Species, colour = Species, 
                                                                   tooltip = c(paste0("Year: ", Year, "\n Species: ", Species, "\n Indicator: ", format(round(Indicator, 2), nsmall = 2))), 
                                                                   data_id = Year)) 
               + geom_line_interactive(size = 1.5, aes(x = Year, y = Indicator, group = Species, colour = Species, data_id = Species))
               + defra_style() 
               + scale_colour_manual(values=CSGraph2()) 
               + scale_fill_manual(values=alpha(CSGraph2(), 0.2))
               + xlab("Year") + ylab("Data y axis") 
               + geom_hline(aes(yintercept=100), colour=CSGraphLine()[1], linetype="dotted") 
               + geom_vline(aes(xintercept=2013), colour = CSGraphLine()[1])
               + guides(colour=FALSE)
               + scale_x_date(date_labels = "%Y", date_breaks = "2 year")
               + expand_limits(y=c(30,110))
               + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)), 
               width_svg = widthSVG, height_svg = heightSVG, options = themeOptions_line())
    })
    
    output$p3_3_graph_2 <- renderGirafe({
        girafe(ggobj = ggplot(p3_3_2_data) + geom_point_interactive(alpha = 0.01, size=16, aes(x = Year, y = Indicator, group = Species, colour = Species, 
                                                                                      tooltip = c(paste0("Year: ", Year, "\n Species: ", Species, "\n CFI: ", format(round(Indicator, 2), nsmall = 2))), 
                                                                                      graph3_3_1data_id = Year)) 
               + geom_line_interactive(size = 1.5, aes(x = Year, y = Indicator, group = Species, colour = Species, data_id = Species))
               + defra_style() + scale_colour_manual(values=CSGraph2()) 
               + xlab("Year") + ylab("Data y axis") 
               + scale_x_date(date_labels = "%Y", date_breaks = "5 year")
               + geom_hline(aes(yintercept=1.00), colour=CSGraphLine()[1], linetype="dotted") 
               + facet_wrap(~Species) 
               + theme(legend.position="none"), width_svg = widthSVG, height_svg = heightSVG, options = themeOptions_line())
        
    })
    
    
    
  

    # EVALUATION OF DATA TABLES #################################################################
    # This table stores information about the data shown on a specific dashboard page
    # The data information is stored in an excel file "Data Instructions.xlsx"
    table_options <- list(lengthChange = FALSE,
                          bInfo = FALSE,
                          searching = FALSE,
                          paging = FALSE,
                          headerCallback = JS(
                              "function(thead, data, start, end, display){",
                              "  $(thead).remove();",
                              "}"))
    
    # This function iterates over all the sheet names in the Data Instructions Excel. It then creates table outputs for each sheet
    lapply(sheets_eval, function(i) {
        outputId <- paste0("table_", i)
        output[[outputId]] <- renderDT({datatable(get(paste0("eval_data_", i)), rownames = FALSE, options = table_options, selection = "none") %>%
                formatStyle(c("Cat","Text"), background = CSColour(), color = CSTextColour(), stripe = "#ffffff00")
        })
    })
    
    
}
)
