
#-------------------------------------------------------------------#
#                       K E Y  S T A T S                            #
#-------------------------------------------------------------------#


# KS1 will be displayed on the Dashboard Welcome Page
#-------------------------------------------------------------------#

# WELCOME NATIONAL ACTION PLAN KEY STAT #############################

NapKS <- list(h3("NAP"),"National Action Plan Framework")

# WELCOME PRICES KEY STAT ###########################################
PricesKS <- list(h3("Prices"),"Prices Dashboard")

# PAGE 2 KEY STATS ###################################################
P2_KS1 <- list(h3(p2_1_number_final[3]),"Key statistic 1")
P2_KS2 <- list(h3(p2_1_percent_final[3]),"Key statistic 2")
P2_KS3 <- list(h3(p2_1_region_final[3]),"Key statistic 3")

# PAGE 3 | SUB-PAGE 1 KEY STATS ##################################
P3_1_KS1 <- list(h3(p3_1_data_KS1), paste0("Key statistic 1 in ", year(last(p3_1_data$Year))))
P3_1_KS2 <- list(h3(p3_1_data_KS2), paste0("Key statistic 2 in ", year(last(p3_1_data$Year))))
P3_1_KS3 <- list(h3(p3_1_data_KS3), paste0("Key statistic 3 in ", year(last(p3_1_data$Year))))

# PAGE 3 | SUB-PAGE 2 KEY STATS #######################################
P3_2_KS1 <- list(h3(p3_2_data_latest), "Key Stat 1")
P3_2_KS2 <- list(h3(p3_2_data_latest), "Key Stat 2")

# PAGE 3 | SUB-PAGE 3 KEY STATS ######################################
P3_3_KS1 <- list(h3(p3_3_data_latest_1[3]), "Key statistic 1")
P3_3_KS2 <- list(h3(p3_3_data_latest_2[2]), "Key statistic 2")
P3_3_KS3 <- list(h3(p3_3_data_latest_3[3]), "Key statistic 3")
P3_3_KS4 <- list(h3(p3_3_data_latest_4[3]), "Key statistic 4")
