
#-------------------------------------------------------------------#
#                            T H E M E                              #
#-------------------------------------------------------------------#

lightDark <- "light"
lightDarkBg <-"#ffffff"

# DASHBOARD STARTING THEME -----------------------------------------------------------------------------------------
# ThemeVar = TRUE sets the dashboard in darkmode
ThemeVar <- TRUE

# COLOUR PALETTES ################################################################################################

# see pesticide PDF documentation for details of the colour schemes and colour blind tests. All colours used should be suitable in grayscale too.

# -------------------------------------- LIGHT COLOUR SCHEME --------------------------------------------------------- #
# Primary, Dark Grey, Yellow, Orange, Red, Pink, Purple, Blue, Light Green

CSLight <- c("#00A33B", "#272C30", "#FFCC00", "#FF9E16", "#D9262E", "#D51067", "#6D3075", "#007CBA", "#77BC1F")
CSLightGraph <- c("#FFDB36", "#FF817B", "#D7222B", "#D13693", "#461E4B", "#6E2F75", "#0273AC", "#268AFC", "#18D8AC", "#75BD1D")
CSLightGraphLine <- c(CSLight[2])
CSCardLightColour <- "#ffffff"

# -------------------------------------- DARK COLOUR SCHEME --------------------------------------------------------- #
# Colours are desaturated to increase contrast on dark backgrounds
# Primary, Grey, Yellow, Orange, Red, Pink, Purple, Blue, Light Green

CSDark <- c("#00A33B", "#576069", "#DEBB1F", "#FF9F21", "#DE3039", "#D84482", "#77487C", "#017DBB", "#7DBA2D")
CSDarkGraph <- c("#DEBB1F", "#FF817B", "#DE3039", "#D84482", "#5A2760", "#77487C", "#0273AC", "#268AFC", "#18D8AC", "#7DBB2D")

# This is for vertical or horizontal line markers on graphs
CSDarkGraphLine <- c("#ffffff")

# Colour of cards in dark mode
CSCardDarkColour <- "#353c42"

keyStat_colour <- c("yellow", "orange", "red", "fuchsia", "purple", "blue", "green")

# CARD DESIGNS #######################################################################################################
# Creates BS4Dash Cards for various purposes. This keeps consistency across the dashboard

# Graph Cards
# Each "card" on the dashboard that contains elements like a title, graph or text is defined here as a template. These functions
# can then be used to easily create new cards for the Dashboard.

# CARD | GRAPH | GGIRAPH (interactive ggplot2 graph)
cardGraph <- function(titleCard, graph, backColour = NULL, width = 6){
  bs4Card(
    title = titleCard, # Title of card displayed
    closable = FALSE, # Is there an "x" to close the card
    maximizable = TRUE, # Can the card be maximised into full screen
    width = width, # Width of card
    solidHeader = FALSE, # A solid coloured header
    gradientColor = backColour, # Sets the background colour to NULL be default - and then set by the theme in the server
    collapsible = TRUE, # Can you collapse and minimise the card
    girafeOutput(graph, width = "100%", height = "100%") # Graph output. In this case takes a girafe object and outputs it in the server
  )
}

# CARD | GRAPH | PLOTLY (interactive ggplot2 using plotly wrapper)
cardGraphPlotly <- function(titleCard, graph, backColour = NULL){
  bs4Card(
    title = titleCard,
    closable = FALSE,
    maximizable = TRUE,
    width = 6,
    solidHeader = FALSE,
    gradientColor = backColour,
    collapsible = TRUE,
    plotlyOutput(graph, height = "100%", width = "100%") # Graph output. In this case a plotly object and outputs it to the server
  )
}


# CARD | KEY STATISTIC (for welcome page and for coloured boxes with key stats on each dashboard page)
cardKeyStat <- function(value, subtitle, icon, colour, width = 3, footer = NULL){
  bs4ValueBox(value = value, # Key statistic value - this should be as short as possible
              subtitle = subtitle, # Text that accompanies key stat
              icon = icon, # Icon on the key statistic box
              status = colour, # Colour of the status box - These options are defined in the theme file and set in the server
              width = width, # Width of the value box - default is 3
              footer = footer # Footer of the box - typically where a link might go (NULL by default)
  )
}

# CARD | PAGE HEADING (for the main page titles)
cardTitle <- function(titleCard, backColour = NULL){
  bs4Card(
    elevation = 0,
    width = 12,
    collapsed = FALSE,
    collapsible = FALSE,
    gradientColor = backColour,
    closable = FALSE,
    h1(titleCard)
  )
}

# CARD | PAGE SUBTITLE HEADING (for subtitles that stretch full page)
cardSubTitle <- function(titleCard, backColour = NULL){
  bs4Card(
    elevation = 0,
    width = 12,
    collapsed = FALSE,
    collapsible = FALSE,
    gradientColor = backColour,
    closable = FALSE,
    h3(titleCard)
  )
}

# CARD | ABOUT (for showing text based information)
cardAbout <- function(title, text, backColour = NULL, object = NULL, collapsed = TRUE, width = 4){
  bs4Card(
    title = title,
    elevation = 1,
    width = width,
    status = colour,
    gradientColor = backColour,
    collapsed = collapsed,
    closable = FALSE,
    h6(text),
    object
  )
}

# CARD | TABLE (for showing information in a table)
cardTable <- function(table, backColour = NULL, collapsed = TRUE, width = 8){
  bs4Card(
    title = "Information on Data",
    elevation = 2,
    width = width,
    status = colour,
    gradientColor = backColour,
    collapsed = collapsed,
    closable = FALSE,
    DTOutput(table)
  )
}

# CARD | SOURCE (displays text information on a source)
cardSource <- function(Source, backColour = NULL){
  bs4Card(
    title = "Source",
    elevation = 1,
    width = 12,
    collapsed = TRUE,
    collapsible = TRUE,
    gradientColor = backColour,
    closable = FALSE,
    h6(Source)
  )
}

# TABBED CARD | ABOUT + SOURCE (shows tabbed about and source text information)
cardAboutSource <- function(id = "AboutSourceCard", About, Source, backColour = NULL, title = ""){
  bs4TabCard(
    id = id,
    title = title,
    elevation = 1,
    width = 12,
    status = "light",
    solidHeader = TRUE,
    gradientColor = backColour,
    collapsed = FALSE,
    collapsible = FALSE,
    closable = FALSE,
    bs4TabPanel(
      tabName = "About", 
      active = TRUE,
      h6(About)
    ),
    bs4TabPanel(
      tabName = "Source", 
      active = FALSE,
      h6(Source)
    )
  )
}

# CARD | DISPLAY OPTIONS (provides the option for up to 5 data selectors, e.g. checkbox, radio buttons or date picker)
cardDisplayOptions <- function(optionsObject = NULL, optionsObject2 = NULL, optionsObject3 = NULL, optionsObject4 = NULL, optionsObject5 = NULL, backColour = NULL){
  bs4Card(
    width = 12,
    title = "What would you like to display?",
    collapsed = TRUE,
    closable = FALSE,
    gradientColor = backColour,
    solidHeader = FALSE,
    headerBorder = TRUE,
    fluidRow(
      bs4Card(
        title = "Key Options",
        width = 6,
        elevation = 3,
        status = "dark",
        gradientColor = "primary",
        collapsible = FALSE,
        closable = FALSE,
        optionsObject, optionsObject2
      ),
      bs4Card(
        title = "Other Display Options",
        width = 6,
        elevation = 0,
        gradientColor = backColour,
        collapsible = FALSE,
        closable = FALSE,
        optionsObject3, optionsObject4, optionsObject5
      )
    )
  )
}

# CARD | DISPLAY OPTIONS 2 (designed for only 2 data selectors. Use Display Options 1 for more than 2 data selectors)
cardDisplayOptions2 <- function(optionsObject = NULL, optionsObject2 = NULL, backColour = NULL){
  bs4Card(
    width = 12,
    title = "What would you like to display?",
    collapsed = TRUE,
    closables = FALSE,
    gradientColor = backColour,
    solidHeader = FALSE,
    headerBorder = TRUE,
    fluidRow(
      bs4Card(
        title = "Key Options",
        width = 6,
        elevation = 3,
        status = "dark",
        gradientColor = "primary",
        collapsible = FALSE,
        closable = FALSE,
        optionsObject
      ),
      bs4Card(
        title = "Other Display Options",
        width = 6,
        elevation = 0,
        gradientColor = backColour,
        collapsible = FALSE,
        closable = FALSE,
        optionsObject2
      )
    )
  )
}

# CARD | UNDER DEVELOPMENT (for pages that are under development)
cardUnderDevelopment <- function(){
  bs4Card(
    elevation = 1,
    width = 12,
    status = "warning",
    gradientColor = "warning",
    collapsed = FALSE,
    collapsible = FALSE,
    closable = FALSE,
    h3("This Page is Under Development")
  )
}

# DASHBOARD THEMES ##############################

# LIGHT THEME
# Fresh themes for modifying the default BS4Dash colours

light_file <- file.path("defra-light.css")

Defra_Theme_Light <- create_theme(
  bs4dash_vars(
    navbar_light_color = CSLight[2],
    navbar_light_active_color = CSLight[2],
    navbar_light_hover_color = CSLight[1]
  ),
  bs4dash_yiq(
    contrasted_threshold = 200,
    text_dark = CSLight[2],
    text_light = "#ffffff"
  ),
  bs4dash_layout(
    main_bg = lightDarkBg
  ),
  bs4dash_sidebar_light(
    bg = "#FFFFFF",
    color = CSLight[2],
    hover_color = CSLight[1],
    submenu_bg = "#FFFFFF",
    submenu_color = CSLight[2],
    submenu_hover_color = CSLight[1]
  ),
  bs4dash_status(
    primary = CSLight[1], 
    danger = CSLight[5], 
    light = "#ffffff",
    #dark = "#25322A",
    warning = CSLight[4], 
  ),
  bs4dash_color(
    # Modification of default colours. Spare colours are commented out (left as default)
    gray_900 = CSLight[2], 
    blue = CSLight[8], 
    #light_blue = NULL,
    red = CSLight[5], 
    green = CSLight[9],
    yellow = CSLight[3], 
    # navy = NULL,
    # teal = NULL,
    # olive = NULL,
    # lime = NULL,
    orange = CSLight[4], 
    fuchsia = CSLight[6], 
    purple = CSLight[7], 
    # maroon = NULL,
    # black = NULL,
  )
)

# DARK THEME
# Second Fresh theme in Dark

dark_file <- file.path("defra-dark.css")

Defra_Theme_Dark <- create_theme(
  bs4dash_vars(
    navbar_light_color = "#fff",
    navbar_light_active_color = "#00A33B",
    navbar_light_hover_color = "#00A33B"
  ),
  # Changes at what point text changes from light to dark on a colour background
  bs4dash_yiq(
    contrasted_threshold = 100,
    text_dark = "#ffffff", 
    text_light = "#ffffff"
  ),
  # Background Colour for the dashboard
  bs4dash_layout(
    main_bg = "#272c30"
  ),
  # Change to yellow to test what things do
  bs4dash_sidebar_light(
    bg = "#272c30", 
    color = "#bec5cb",
    hover_color = "#FFF",
    submenu_bg = "#272c30", 
    submenu_color = "#FFF", 
    submenu_hover_color = "#FFF"
  ),
  bs4dash_status(
    primary = "#00A33B", 
    danger = "#D9262E", 
    warning = "#FF9816",
    light = "#272c30", 
    # dark = "#353c42"
    dark = "#ffffff"
  ),
  bs4dash_color(
    gray_900 = CSDark[2], 
    blue = CSDark[8],
    red = CSDark[5], 
    green = CSDark[9],
    yellow = CSDark[3], 
    # navy = NULL,
    # teal = NULL,
    # olive = NULL,
    # lime = NULL,
    orange = CSDark[4], 
    fuchsia = CSDark[6], 
    purple = CSDark[7], 
    # maroon = NULL,
    # black = NULL,
  )
)





# GRAPH STYLING #########################
#Graph GGiraph interactive plot specific theme options. Again delivered in light and dark variations

# Dimensions in inches - ratio between the two is important
widthSVG <- 12
heightSVG <- 8


tooltip_css <- "background-color:white; opacity:0.5; color:#25322A; font-style:bold; padding:10px; z-index: 5; border-radius:5px; top:relative !important;"
hover_css = "fill:#576069; cursor: crosshair;"
hoverLine_css = "stroke:#272C30; stroke-width:2; cursor: crosshair;"
click_css = "fill:#272C30; cursor: crosshair;"
click_css_line = "stroke:#272C30; stroke-width:2; cursor: crosshair;"

tooltip_css_dark <- "background-color:#272C30; opacity:0.5; color:#ffffff; font-style:bold; padding:10px; z-index: 5; border-radius:5px; top:relative !important;"
hover_css_dark = "fill:#ffffff; cursor: crosshair;"
hoverLine_css_dark = "stroke:#ffffff; stroke-width:2; cursor: crosshair;"
click_css_dark = "fill:#ffffff; cursor: crosshair;"
click_css_line_dark = "stroke:#ffffff; stroke-width:2; cursor: crosshair;"


themeOptions_light <- list(opts_tooltip(css = tooltip_css), opts_toolbar(position = "topright"), opts_hover(css = hover_css), opts_sizing(rescale = TRUE, width = 1), opts_selection(css = click_css))
themeOptions_line_light <- list(opts_tooltip(css = tooltip_css), opts_toolbar(position = "topright"), opts_hover(css = hoverLine_css), opts_sizing(rescale = TRUE, width = 1), opts_selection(css = click_css_line))

themeOptions_dark <- list(opts_tooltip(css = tooltip_css_dark), opts_toolbar(position = "topright"), opts_hover(css = hover_css_dark), opts_sizing(rescale = TRUE, width = 1), opts_selection(css = click_css_dark))
themeOptions_line_dark <- list(opts_tooltip(css = tooltip_css_dark), opts_toolbar(position = "topright"), opts_hover(css = hoverLine_css_dark), opts_sizing(rescale = TRUE, width = 1), opts_selection(css = click_css_line_dark))

# GGPLOT THEMING ###

#ggplot styling
#Deprecated theme

# my_theme <- theme_minimal()+ 
#   theme(text = element_text(colour = "black"),
#         axis.title = element_text(size = rel(1.1)),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         legend.position = "bottom")

# LIGHT THEME ###

light <- function() {
  font <- "Arial"
  
  ggplot2::theme(
    
    #Text format:
    #This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(family=font,
                                       size=18,
                                       face="bold",
                                       color="#25322A"),
    #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=14,
                                          margin=ggplot2::margin(9,0,9,0)),
    plot.caption = ggplot2::element_blank(),
    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    
    #Legend format
    #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "bottom",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=12,
                                        color="#25322A"),
    
    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    axis.title = ggplot2::element_text(family=font,
                                       size=12,
                                       color="#25322A"),
    axis.text = ggplot2::element_text(family=font,
                                      size=12,
                                      color="#25322A"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
    
    rect = element_rect(fill = "transparent"), # all rectangles
    
    #panel.background = ggplot2::element_blank(),
    
    #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "#ffffff",colour = NA),
    
    strip.background = element_rect(fill = "transparent",colour = NA),
    strip.text = element_text(family=font, size=12, colour = CSLight[2], margin = margin(4, 9, 4, 9))
  )
}

# DARK THEME ###

dark <- function(){
  
  font <- "Arial"
  ggplot2::theme(  
    #Text format:
    #This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(family=font,
                                       size=18,
                                       face="bold",
                                       color="#ffffff"),
    #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=14,
                                          margin=ggplot2::margin(9,0,9,0)),
    plot.caption = ggplot2::element_blank(),
    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    
    #Legend format
    #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "bottom",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=12,
                                        color="#ffffff"),
    
    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    axis.title = ggplot2::element_text(family=font,
                                       size=12,
                                       color="#ffffff"),
    axis.text = ggplot2::element_text(family=font,
                                      size=12,
                                      color="#ffffff"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#ffffff"),
    panel.grid.major.x = ggplot2::element_blank(),
    
    
    panel.background = element_rect(fill = "#343a40",colour = NA),
    plot.background = element_rect(fill = "#343a40",colour = NA),
    strip.background = element_rect(fill = "#343a40",colour = NA),
    strip.text = element_text(family=font, size=12, colour = "#ffffff", margin = margin(4, 9, 4, 9))
  )
  
}
