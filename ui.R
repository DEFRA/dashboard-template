
#-------------------------------------------------------------------#
#                              U I                                  #
#-------------------------------------------------------------------#


# TEMPLATE BS4 DASHBOARD ##############################################
# Author: Alex Agnew
# To run locally "Run App" in RStudio on either server.R or ui.R files scripts

# Libraries ########################################################
# Add libraries as required to the below. The below are required for the functionality of the dashboard

# REQUIRED LIBRARIES ------------------------------------------------
library(shiny)
library(bs4Dash)
library(fresh)
library(tidyverse)
library(readxl)
library(DT)
library(plotly)
library(gcookbook)
library(readODS)
library(doBy)
library(ggiraph)
library(shinyWidgets)
library(lubridate)


# OPTIONAL LIBRARIES ------------------------------------------------
NULL


# REQUIRED LINKED R SCRIPTS -----------------------------------------
# Dashboard Metadata
source("MetaData.R") # <- Stores Identifying Data in a seperate file in order for dev dashboard to be developed with different title

# KEY EDITS SCRIPT ###
# Key Edits is a useful script for source links or definitions that may need to change frequently
source("keyEdits.R")

# DATA SCRIPT ###
# This is where data acquisition and cleaning should take place
source("data.R") # <- "data.R" is the name of the file in the dashboard folder

# TEXT SCRIPT ###
# Text is where all text variables should be stored for ease of access
source("text.R")

# THEME SCRIPT ###
# This theme script defines the "Defra Brand" following the brand guidelines. Modifications should not be necessary
source("theme.R")


# SUGGESTED LINKED R SCRIPTS ----------------------------------------

# KEY STATS SCRIPT ###
# Key stats is useful for drawing statistics from the data cleaning that you may want to highlight
source("keyStats.R")

# OTHER SCRIPTS -----------------------------------------------------
NULL # <- If additional files are created source them here


# WEB APP -----------------------------------------------------------
# Defines the UI of the Web App
shinyUI(
    
    bs4DashPage(
        # THIS SECTION SHOULD NOT BE MODIFIED
        enable_preloader = FALSE, # <- Pre-loading screen not required
        navbar = bs4DashNavbar(
            title = "dashboardTitle", # <- Dashboard Title in Browser
            status = "light",
            
            # Light | Dark mode toggle switch
            leftUi = div(
                prettySwitch(
                    inputId = "DarkLight",
                    value = ThemeVar, # <- ThemeVar is the theme set be default - e.g. dark mode
                    label = "Dark Theme", 
                    fill = FALSE,
                    status = "success",
                    slim = TRUE
                ))
        ),
        
        # Footer ################################################
        footer =  bs4DashFooter(
            copyrights = "Sample Dashboard", # <- Dashboard Text in Footer
            right_text = div(img(src="DefraLogoLong.png", width = "450")) # <- Defra Logo in footer
        ),
        
        #Sidebar ###################################
        sidebar = bs4DashSidebar(
            # Elevation - depth of shadow (between 1 and 5)
            elevation = "2",
            
            # Defra Logo - calls light or dark theme logo
            title = div(imageOutput("imgDefraSidebarLogo")),
            
            # Takes the primary (green) colour values
            status = "primary",
            
            # Dashboard Light or Dark theme
            skin = lightDark,
            
            # Dashboard Sidebar Menu
            bs4SidebarMenu(
                id = "sidebar",
                title = "Menu",
                bs4SidebarMenuItem(
                    text = "Welcome Page",
                    tabName = "Welcome",
                    icon = "hand-paper"
                ),
                bs4SidebarMenuItem(
                    text = "Page 2",
                    tabName = "Page_2",
                    icon = "cloud-sun-rain"
                ),
                bs4SidebarMenuItem(
                    text = "Page 3",
                    tabname = NULL,
                    icon = "apple-alt",
                    bs4SidebarMenuItem(
                        text = "Sub-Page 1",
                        tabName = "Sub_Page_1",
                        icon = "mountain"
                    ),
                    bs4SidebarMenuItem(
                        text = "Sub-Page 2",
                        tabName = "Sub_Page_2",
                        icon = "eye-dropper"
                    ),
                    bs4SidebarMenuItem(
                        text = "Sub-Page 3",
                        tabName = "Sub_Page_3",
                        icon = "leaf"
                    )
                )
            ),
            
            # EXTERNAL LINNKS ON SIDEBAR (use if you want to link to external websites)
            
            # LINK ###
            tags$div(
                hr(style = "border-top: 1px solid; padding-left: 20px; padding-right: 20px;"), # <- Straight line
                a(href= sidebarLink, "Google",  
                  style = "padding-left: 20px; padding-right: 0px;"),
                hr(style = "border-top: 1px solid; padding-left: 20px; padding-right: 20px;") # <- Straight line
            )
        ),
        #Body ------------------------------------------------------------
        body = bs4DashBody(
            #Theme
            #use_theme(DefraThemed), #Grabs the theme file from theme.R
            tagList(
                # Outputs the UI in either dark or light theme
                uiOutput(outputId = "theme_ui"),
                
                # Additional CSS required to override some style for the dashboard layout
                includeCSS("www/styles.css")
            ),
            
            # Calls body items for individual pages (Pages.R)
            bs4TabItems(
                bs4TabItem(
                    tabName = "Welcome",
                    uiOutput("Welcome")
                ),
                bs4TabItem(
                    tabName = "Page_2",
                    uiOutput("Page_2")
                ),
                bs4TabItem(
                    tabName = "Sub_Page_1",
                    uiOutput("Sub_Page_1")
                ),
                bs4TabItem(
                    tabName = "Sub_Page_2",
                    uiOutput("Sub_Page_2")
                ),
                bs4TabItem(
                    tabName = "Sub_Page_3",
                    uiOutput("Sub_Page_3")
                )
            )
        )
    ))




