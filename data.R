
#-------------------------------------------------------------------#
#                             D A T A                               #
#-------------------------------------------------------------------#

# PAGE 2 ##########################################################################################

clean_p2_1 <- function(dataset, pivot_val, colnames){
  dataset %>%
    pivot_longer(c(-1)) %>%
    `colnames<-`(c("Year", "Tests_Spray", "Value"))
}

clean_p2_2 <- function(dataset){
  dataset %>%
    pivot_longer(c(-1)) %>%
    `colnames<-`(c("Region", "Year", "Value"))
}

p2_1_data_raw <- read_excel(p2_1_data_source, sheet = "p2_1")

# TOTAL TESTS AND % SPRAYED AREA
p2_1_data <- p2_1_data_raw[c(1:3), c(1:18)]
names(p2_1_data) <- p2_1_data_raw[c(1),c(1:18)]
p2_1_data <- data.frame(t(p2_1_data))
names(p2_1_data) <- c("Year", "Tests", "Percentage Sprayed Area")
p2_1_data <- clean_p2_1(p2_1_data)[-c(1:3),]
p2_1_data$Value <- as.numeric(p2_1_data$Value)
p2_1_data_tests <- subset(p2_1_data, Tests_Spray %in% unique(Tests_Spray)[2])
p2_1_data_percent <- subset(p2_1_data, Tests_Spray %in% unique(Tests_Spray)[1])

# TESTS BY REGION
p2_1_region_data <- p2_1_data_raw[c(20:40), c(1:5)]
names(p2_1_region_data) <- c("Region", p2_1_data_raw[c(18), c(2:5)])
p2_1_region_data <- p2_1_region_data[c(3,6:8),]
p2_1_region_data <- clean_p2_2(p2_1_region_data)
p2_1_region_data$Year <- as.Date(paste(p2_1_region_data$Year, 1, 1, sep = "-"))
p2_1_region_data$Value <- as.numeric(p2_1_region_data$Value)

# KEY STATS
p2_1_number_final <- tail(subset(p2_1_data, Tests_Spray %in% unique(Tests_Spray)[2]),1)
p2_1_percent_final <- tail(subset(p2_1_data, !is.na(Value) & Tests_Spray %in% unique(Tests_Spray)[1]),1)
p2_1_percent_final$Value <- paste0(p2_1_percent_final$Value, "%")
p2_1_region_final <- tail(subset(p2_1_region_data, Region %in% unique(Region)[1]),1)


# PAGE 3 | SUB PAGE 1 ##############################################################################

clean_p3_1 <- function(dataset){
  dataset %>%
    pivot_longer(-1) %>%
    `colnames<-`(c("Area_Usage", "Year", "Area"))
}

p3_1_data_raw <- read_excel(p3_1_data_source, sheet = "p3_1")

p3_1_data <- p3_1_data_raw[c(36:46),]
names(p3_1_data) <- c("Area_Usage", p3_1_data_raw[9,2:18])
p3_1_data[,2:18] <- sapply(p3_1_data[,2:18], as.numeric)

p3_1_data <- clean_p3_1(p3_1_data)
p3_1_data$Year <- as.Date(paste(p3_1_data$Year, 1, 1, sep = "-"))
p3_1_data$Area <- p3_1_data$Area / 1000

p3_1_cats <- unique(p3_1_data$Area_Usage)
p3_1_data_final <- subset(p3_1_data, !(Area_Usage %in% tail(p3_1_cats,1)))

p3_1_total_current <- subset(p3_1_data, Year %in% last(Year)& Area_Usage == tail(p3_1_cats,1))
p3_1_total_current$Area <- format(round(p3_1_total_current$Area, 1), nsmall = 1)


# AGRICULTURE | ORGANICS | MARKET 
p3_1_data_KS1 <- paste0(p3_1_total_current," Unit")[3]
p3_1_data_KS2 <- "30"
p3_1_data_KS3 <- "25%"


# PAGE 3 | SUB PAGE 2 #############################################################################

# First Set of Data #

p3_2_data_raw <- read_excel(p3_2_data_source, sheet = "p3_2")
p3_2_data_raw_2 <- read_excel(p3_2_data_source, sheet = "p3_2_2")

sample_fcn_clean <- function(dataset){
  dataset %>%
    pivot_longer(-1) %>%
    `colnames<-`(c("Year", "Type", "Value"))
}


p3_2_data <- p3_2_data_raw[c(2:4), c(1:24)]
p3_2_data <- as.data.frame(t(p3_2_data))[-1,]
names(p3_2_data) <- c("Year", "Category 1", "Category 2")
p3_2_data$Year <- as.Date(paste(p3_2_data$Year, 1, 1, sep = "-"))
p3_2_data <- sample_fcn_clean(p3_2_data)
p3_2_data$Value <- as.numeric(p3_2_data$Value)


# Second Set of Data #

sample_fcn_clean_2 <- function(dataset){
  dataset %>%
    pivot_longer(c(-1, -2)) %>%
    `colnames<-`(c("Country_Code", "Member_State", "Type", "Value"))
}

p3_2_data_2 <- p3_2_data_raw_2[c(3:30),c(1:5)]
names(p3_2_data_2) <- c("Country_Code", p3_2_data_raw_2[2,c(2:5)])
p3_2_data_2 <- sample_fcn_clean_2(p3_2_data_2)
p3_2_data_2$Value <- as.numeric(p3_2_data_2$Value)

# KEY STATS #
p3_2_data_latest <- paste0(subset(p3_2_data, Year %in% last(Year) & Type == "Category 1")[3])
p3_2_data_latest_2 <- paste0(subset(p3_2_data, Year %in% last(Year) & Type == "Category 2")[3])

# PAGE 3 | SUB PAGE 3 ############################################################################


# CHICK FEED INDEX (CFI) #
sample_fcn_clean_3 <- function(dataset){
  dataset %>%
    pivot_longer(-1) %>%
    `colnames<-`(c("Year", "Species", "Indicator")) %>%
    mutate(Year = as.Date(paste(Year, 1, 1, sep = "-")))
}

p3_3_1_data_raw <- read_ods(p3_3_1_data_source)
p3_3_2_data_raw <- read_excel(p3_3_2_data_source)

p3_3_2_data <- p3_3_2_data_raw
names(p3_3_2_data) <- c("Year", "A", "B", "C", "D")
p3_3_2_data <- sample_fcn_clean_3(p3_3_2_data)
p3_3_2_data <- mutate(p3_3_2_data, Indicator = Indicator * 100)
p3_3_2_data$Year <- as.Date(paste(p3_3_2_data$Year, 1, 1, sep = "-"))


# Selects desired columns
p3_3_1_data <- p3_3_1_data_raw[c(16:63), c(1:17)]

# Names individual columns - necessary due to messy spreadsheet structure
colnames(p3_3_1_data) <- c("Year",
                          "All Species (unsmoothed)",
                          "All Species (smoothed)",
                          "All Species (smoothed) lower 97.5% confidence interval",
                          "All Species (smoothed) upper 97.5% confidence interval",
                          "All Farmland Birds (unsmoothed)",
                          "All Farmland Birds (smoothed)",
                          "All Farmland Birds (smoothed) lower 97.5% confidence interval",
                          "All Farmland Birds (smoothed) upper 97.5% confidence interval",
                          "Generalist Farmland Birds (unsmoothed)",
                          "Generalist Farmland Birds (smoothed)",
                          "Generalist Farmland Birds (smoothed) lower 97.5% confidence interval",
                          "Generalist Farmland Birds (smoothed) upper 97.5% confidence interval",
                          "Specialist Farmland Birds (unsmoothed)",
                          "Specialist Farmland Birds (smoothed)",
                          "Specialist Farmland Birds (smoothed) lower 97.5% confidence interval",
                          "Specialist Farmland Birds (smoothed) upper 97.5% confidence interval")

df_1 <- data.frame(Year=as.numeric(p3_3_1_data$Year),
                          Species="A",
                          Indicator = as.numeric(p3_3_1_data$`All Species (smoothed)`),
                          LowerConfidence = as.numeric(p3_3_1_data$`All Species (smoothed) lower 97.5% confidence interval`),
                          UpperConfidence = as.numeric(p3_3_1_data$`All Species (smoothed) upper 97.5% confidence interval`)
)

df_2 <- data.frame(Year=as.numeric(p3_3_1_data$Year),
                          Species="B",
                          Indicator = as.numeric(p3_3_1_data$`All Farmland Birds (smoothed)`),
                          LowerConfidence = as.numeric(p3_3_1_data$`All Farmland Birds (smoothed) lower 97.5% confidence interval`),
                          UpperConfidence = as.numeric(p3_3_1_data$`All Farmland Birds (smoothed) upper 97.5% confidence interval`)
)

df_3 <- data.frame(Year=as.numeric(p3_3_1_data$Year),
                          Species="C",
                          Indicator = as.numeric(p3_3_1_data$`Generalist Farmland Birds (smoothed)`),
                          LowerConfidence = as.numeric(p3_3_1_data$`Generalist Farmland Birds (smoothed) lower 97.5% confidence interval`),
                          UpperConfidence = as.numeric(p3_3_1_data$`Generalist Farmland Birds (smoothed) upper 97.5% confidence interval`)
)

df_4 <- data.frame(Year=as.numeric(p3_3_1_data$Year),
                          Species="D",
                          Indicator = as.numeric(p3_3_1_data$`Specialist Farmland Birds (smoothed)`),
                          LowerConfidence = as.numeric(p3_3_1_data$`Specialist Farmland Birds (smoothed) lower 97.5% confidence interval`),
                          UpperConfidence = as.numeric(p3_3_1_data$`Specialist Farmland Birds (smoothed) upper 97.5% confidence interval`)
)

df_master_p3_3_1_data <- rbind(df_1, df_2, df_3, df_4)
df_master_p3_3_1_data$Year <- as.Date(paste(df_master_p3_3_1_data$Year, 1, 1, sep = "-"))
df_master_p3_3_1_data <- mutate(df_master_p3_3_1_data, Indicator = Indicator * 100)
df_master_p3_3_1_data <- mutate(df_master_p3_3_1_data, LowerConfidence = LowerConfidence * 100)
df_master_p3_3_1_data <- mutate(df_master_p3_3_1_data, UpperConfidence = UpperConfidence * 100)

# KEY STATS

indexlatest <- subset(df_master_p3_3_1_data, Year %in% last(Year)& Species=="A")$Indicator
indexchange <- indexlatest - 100
indexchange <-round(indexchange, 0)
p3_3_data_latest_1 <- paste(indexchange,"%")

indexlatest <- subset(df_master_p3_3_1_data, Year %in% last(Year) & Species=="B")$Indicator
indexchange <- indexlatest - 100
indexchange <-round(indexchange, 0)
p3_3_data_latest_2 <- paste(indexchange,"%")

indexlatest <- subset(df_master_p3_3_1_data, Year %in% last(Year) & Species=="C")$Indicator
indexchange <- indexlatest - 100
indexchange <-round(indexchange, 0)
p3_3_data_latest_3 <- paste(indexchange,"%")

indexlatest <- subset(df_master_p3_3_1_data, Year %in% last(Year) & Species=="D")$Indicator
indexchange <- indexlatest - 100
indexchange <-round(indexchange, 0)
p3_3_data_latest_4 <- paste(indexchange,"%")



# EVALUATION OF DATA TABLES #####################################
sheets_eval <- excel_sheets(eval_source)

for(i in sheets_eval){
  data <- read_excel(eval_source, i)
  data <- data[-3,]
  names(data) <- c("Cat", "Text")
  data <- data[-1,]
  assign(paste0("eval_data_", i), data)
}

