
#-------------------------------------------------------------------#
#                            T E X T                                 #
#-------------------------------------------------------------------#

# TEXT OPTIONS #
# var <- div() for each variable
# tags$b() - Bold text
# a() - Link, a(href="LINK", "LINK TEXT")
# p() - New paragraph



# Welcome Page ########################

About_dash <- "The contents of this dashboard are currently for internal use only. If you would like to use 
the contents for PQs, Ministerial briefs or any other external use please contact the CPHW Evidence Team to discuss."

Instructions_dash <- "This is a template dashboard. Please test its functionality and behaviour and modify a copy of the code to understand its functionality."

suggest_dash <- tags$div("We welcome any and all feedback. If you have any suggestions or problems please contact Ellie Martell at ", 
                         a(href="mailto:eleanor.martell@defra.gov.uk","eleanor.martell@defra.gov.uk"), "or another member of the CPHW evidence team.")


b <- function(x){
  tags$b(x)
}



# Page 3 | 2 ####################################
summary_page3_2 <- "Summary page 3 | 2"
about_page3_2 <- "About page 3 | 2"
source_page3_2 <- "Source page 3 | 2"

# Page 3 | 1 #########################################
summary_page3_1 <- tags$div(
p("Paragraph one"),

p("Paragraph two"),

p("Paragraph three")
)

about_page3_1 <- tags$div(
p("Some text",
  a(href="https://www.google.com/", 
    "https://www.google.com/"))
)


more_info_page3_1 <- tags$div(
  p("More info available here ",  a(href="https://www.google.com/",
                                 "Stats"))
)

source_page3_1 <- tags$div("A Source (2018)",a(href="https://www.google.com/", "https://www.google.com/"))                     


# Page 3 | 3 #################################

summary_page3_3 <- tags$div(
  p("Descriptive Text"), 
  
  tags$h5("Topic 1"),
  
  p("More Text"),
  
  tags$h5("Topic 2"),
  
  p("Some Extra Text")
)

about_page3_3 <- tags$div(
  p("Some About"),
  
  tags$h5("Another Headline"),
  
  p("A paragraph of text"),
  
  p("Some more text"),
  
  p("For more information please see the ", 
         a(href = "https://www.google.com/", 
                "A Link To google"),"."),
)

source_page3_3 <- tags$div(
  p("A source link, ",
         a(href="https://www.gov.uk/", 
                "https://www.gov.uk/"))
)

# Page 2 #############################################################
summary_page2 <- "Summary Page 2"

about_page2 <- "About Page 2"
source_page2 <- "Source Page 2"