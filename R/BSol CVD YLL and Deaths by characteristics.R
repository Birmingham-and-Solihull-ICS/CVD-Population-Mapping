library(readxl)
library(dplyr)
library(ggplot2)
library(ggforce)
library(viridis)
library(rlang)

## Load Birmingham and Solihull data
file_path_bham = file.path("//svwvap1126.addm.ads.brm.pri",
                           "PHSensitive$",
                           "Intelligence",
                           "2. Requests",
                           "REQ3289 - BSol ICB CVD Project",
                           "REQ3289_BSOL_CVD_Deaths_10years_2014_to_2023.xlsx",
                           fsep="/")

file_path_solihull = file.path("//svwvap1126.addm.ads.brm.pri",
                               "PHSensitive$",
                               "Intelligence",
                               "2. Requests",
                               "REQ3289 - BSol ICB CVD Project",
                               "20240812_Solihull_CVD_Deaths.xlsx",
                               fsep="/")

BhamData <- read_excel(file_path_bham)

SolihullData <- read_excel(file_path_solihull)

## remove Solihull deaths from Birmingham data ####
BhamData <- BhamData %>%
  filter(`LOCAL AUTHORITY CODE OF USUAL RESIDENCE OF DECEASED` == 'E08000025')

## remove unneeded columns from Birmingham dataframe
colnames(BhamData)
colnames(SolihullData)

BhamData <- subset(BhamData, select = -c(MonthText, ICB, Analysis, `DATE OF DEATH OF DECEASED`))

## remove unneeded columns from Birmingham dataframe
SolihullData <- subset(SolihullData, select = -c(CCG_OF_REGISTRATION_CODE, DATE_OF_DEATH))

# rename solihull columns to match Birmingham
SolihullData <- SolihullData %>%
  rename(`LOCAL AUTHORITY CODE OF USUAL RESIDENCE OF DECEASED` = `ULA_OF_RESIDENCE_CODE`,
         `UNDERLYING CAUSE OF DEATH CODE` = UNDERLYING_CAUSE_OF_DEATH,
         `No of Deaths` = CVD,
         `SEX` = sex,
         `AgeInYears` = Age,
         `LOWER SOA CODE OF USUAL RESIDENCE OF DECEASED` = LSOA_OF_RESIDENCE_CODE)

# join data frames to create BSol data frame
BSol <- rbind(BhamData, SolihullData)

## Create columns for ICD10 groups and subgroups
BSol <- BSol %>%
  mutate(
    group = case_when(
      grepl("^I0[0-2]",`UNDERLYING CAUSE OF DEATH CODE`) ~ "Acute rheumatic fever",
      grepl("^I0[5-9]",`UNDERLYING CAUSE OF DEATH CODE`) ~ "Chronic rheumatic heart diseases",
      grepl("^I1[0-5]",`UNDERLYING CAUSE OF DEATH CODE`) ~ "Hypertensive diseases",
      grepl("^I2[0-5]",`UNDERLYING CAUSE OF DEATH CODE`) ~ "Ischaemic heart diseases",
      grepl("^I2[6-8]",`UNDERLYING CAUSE OF DEATH CODE`) ~ "Pulmonary heart disease and diseases of pulmonary circulation",
      grepl("^I[3-4]\\d|I5[0-2]",`UNDERLYING CAUSE OF DEATH CODE`) ~ "Other forms of heart disease",
      grepl("^I6[0-9]",`UNDERLYING CAUSE OF DEATH CODE`) ~ "Cerebrovascular diseases",
      grepl("^I7[0-9]",`UNDERLYING CAUSE OF DEATH CODE`) ~ "Diseases of arteries, arterioles and capillaries",
      grepl("^I8[0-9]",`UNDERLYING CAUSE OF DEATH CODE`) ~ "Diseases of veins, lymphatic vessels and lymph nodes, not elsewhere classified",
      grepl("^I9[5-9]",`UNDERLYING CAUSE OF DEATH CODE`) ~ "Chronic rheumatic heart diseases"),
    icd_subgroup = stringr::str_extract(`UNDERLYING CAUSE OF DEATH CODE`, "(I\\d{2})"))

#check if any groups missing
any(is.na(BSol$group))

#### subgroup definitions####

#load icd10 definitions
ICD10Listing <- read_excel("../data/tlkp_ICD10_DiagnosisCodes_FullListing.xlsx") %>%
  select(`ICD10 Code`, `ICD10 Short Title`)

# Extract first 3 characters to get subgroup
ICD10Listing <- ICD10Listing %>%
  mutate(
    icd_subgroup = stringr::str_extract(`ICD10 Code`, "(I\\d{2})")
  ) %>%
  distinct(icd_subgroup, .keep_all = TRUE)

# join
BSol <- left_join(BSol, ICD10Listing, by= "icd_subgroup")

# check if any subgroups missing
any(is.na(BSol$`ICD10 Short Title`))

# create Local Authority Name column #

BSol <- BSol %>%
  mutate(LocalAuthority = case_when (`LOCAL AUTHORITY CODE OF USUAL RESIDENCE OF DECEASED` == 'E08000025' ~ 'Birmingham',
                                     `LOCAL AUTHORITY CODE OF USUAL RESIDENCE OF DECEASED` == 'E08000029' ~ 'Solihull'))

#load LSOA lookup
LSOALookup <- read.csv("../data/LSOA-lookup.csv")

# join to BSol data
BSol <- left_join(BSol, LSOALookup, by = c('LOWER SOA CODE OF USUAL RESIDENCE OF DECEASED' = 'LSOA'))

# create age groups #

BSol <- BSol %>%
  mutate(
    AgeGroup = case_when(
      AgeInYears < 16 ~ "0 - 15",
      AgeInYears < 31 ~ "16 - 30",
      AgeInYears < 46 ~ "31 - 45",
      AgeInYears < 61 ~ "46 - 60",
      AgeInYears < 76 ~ "61 - 75",
      AgeInYears < 91 ~ "76 - 90",
      AgeInYears >=91 ~ ">= 91"),
    AgeGroup = factor(AgeGroup, levels = c("0 - 15", "16 - 30", "31 - 45", "46 - 60", "61 - 75", "76 - 90", ">= 91"))
  )

## calculate YLL

BSol <- BSol %>%
  mutate(
    YLL = case_when(
      AgeInYears < 75 ~ (74.5 - AgeInYears) * `No of Deaths`,
      AgeInYears >= 75 ~ 0
    )
  )

##################               ####################
##### deaths plot by age
################              ###############

# Average Yearly Total Per ICD10 subgroup
AgeDeathsSubGroup <- BSol %>%
  group_by(`ICD10 Short Title`, `group`, `AgeGroup`) %>%
  summarise(`Deaths` = sum(`No of Deaths`)/10)

#plot
AgeDeathsSubGroupPlot <- ggplot(data= AgeDeathsSubGroup, aes(x = `ICD10 Short Title`, y = Deaths, fill = AgeGroup)) +
  geom_bar(position = "stack", stat= "identity") +
  labs( y = "Average Yearly Deaths, 2014 to 2023, BSol", x = "") +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 750)) +
  facet_col(facets = vars(group),
            scales = "free_y",
            space = "free") +
  labs(fill = "Age Group") +
  scale_fill_viridis(discrete = TRUE, option = "magma")

AgeDeathsSubGroupPlot

ggsave("../output/BSOLAgeDeathsSubGroupPlot.png", AgeDeathsSubGroupPlot, width = 10, height= 12)

##################               ####################
##### deaths and YLL plots by IMD
################              ###############

BSol$IMD_quintile <- as.factor(BSol$IMD_quintile)

# Average Yearly Total YLL and Deaths Per ICD10 subgroup by IMD quintile
IMDDeathsSubGroup <- BSol %>%
  group_by(`ICD10 Short Title`, `group`, `IMD_quintile`) %>%
  summarise(`Deaths` = sum(`No of Deaths`)/10,
            `YLL` = sum(YLL)/10)
#plot deaths
IMDDeathsPlot <- ggplot(data= IMDDeathsSubGroup, aes(x = `ICD10 Short Title`, y = Deaths, fill = IMD_quintile)) +
  geom_bar(position = "stack", stat= "identity") +
  labs( y = "Average Yearly Deaths, 2014 to 2023, BSol", x = "") +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 750)) +
  facet_col(facets = vars(group),
            scales = "free_y",
            space = "free") +
  labs(fill = "IMD Quintile") +
  scale_fill_viridis(discrete = TRUE, option = "magma")

IMDDeathsPlot

ggsave("../output/IMDDeathsSubGroupPlot.png", IMDDeathsPlot, width = 10, height= 12)

#plot YLL
IMDYLLPlot <- ggplot(data= IMDDeathsSubGroup, aes(x = `ICD10 Short Title`, y = YLL, fill = IMD_quintile)) +
  geom_bar(position = "stack", stat= "identity") +
  labs( y = "Average Yearly YLL, 2014 to 2023, BSol", x = "") +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 3500)) +
  facet_col(facets = vars(group),
            scales = "free_y",
            space = "free") +
  labs(fill = "IMD Quintile") +
  scale_fill_viridis(discrete = TRUE, option = "magma")

IMDYLLPlot

ggsave("../output/IMDYLLSubGroupPlot.png", IMDYLLPlot, width = 10, height= 12)

##################               ####################
##### Locality deaths and YLL plots
################              ###############

# Average Yearly Total Per ICD10 subgroup
LocalityDeathsSubGroup <- BSol %>%
  group_by(`ICD10 Short Title`, `group`, `Locality`) %>%
  summarise(`Deaths` = sum(`No of Deaths`)/10,
            `YLL` = sum(YLL)/10)
#plot
LocalityDeathsPlot <- ggplot(data= LocalityDeathsSubGroup, aes(x = `ICD10 Short Title`, y = Deaths, fill = Locality)) +
  geom_bar(position = "stack", stat= "identity") +
  labs( y = "Average Yearly Deaths, 2014 to 2023, BSol", x = "") +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 750)) +
  facet_col(facets = vars(group),
            scales = "free_y",
            space = "free") +
  labs(fill = "Locality") +
  scale_fill_viridis(discrete = TRUE, option = "magma")

LocalityDeathsPlot

ggsave("../output/LocalityDeathsSubGroupPlot.png", LocalityDeathsPlot, width = 10, height= 12)


#plot YLL
LocalityYLLPlot <- ggplot(data= LocalityDeathsSubGroup, aes(x = `ICD10 Short Title`, y = YLL, fill = Locality)) +
  geom_bar(position = "stack", stat= "identity") +
  labs( y = "Average Yearly YLL, 2014 to 2023, BSol", x = "") +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 3500)) +
  facet_col(facets = vars(group),
            scales = "free_y",
            space = "free") +
  labs(fill = "Locality") +
  scale_fill_viridis(discrete = TRUE, option = "magma")

LocalityYLLPlot

ggsave("../output/LocalityYLLSubGroupPlot.png", LocalityYLLPlot, width = 10, height= 12)


##################               ####################
##### Sex YLL and deaths plots
################              ###############
BSol$SEX <- factor(BSol$SEX, levels = c(1, 2), labels = c("Male", "Female"))

# Average Yearly Total Per ICD10 subgroup
SexDeathsSubGroup <- BSol %>%
  group_by(`ICD10 Short Title`, `group`, `SEX`) %>%
  summarise(`Deaths` = sum(`No of Deaths`)/10,
            `YLL` = sum(YLL)/10)

#plot
SexDeathsPlot <- ggplot(data= SexDeathsSubGroup, aes(x = `ICD10 Short Title`, y = Deaths, fill = SEX)) +
  geom_bar(position = "stack", stat= "identity") +
  labs( y = "Average Yearly Deaths, 2014 to 2023, BSol", x = "") +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 750)) +
  facet_col(facets = vars(group),
            scales = "free_y",
            space = "free") +
  labs(fill = "Sex") +
  scale_fill_viridis(discrete = TRUE, option = "viridis")


SexDeathsPlot

ggsave("../output/SexDeathsSubGroupsPlot.png", SexDeathsPlot, width = 10, height= 12)

#plot YLL
SexYLLPlot <- ggplot(data= SexDeathsSubGroup, aes(x = `ICD10 Short Title`, y = YLL, fill = SEX)) +
  geom_bar(position = "stack", stat= "identity") +
  labs( y = "Average Yearly YLL, 2014 to 2023, BSol", x = "") +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 3500)) +
  facet_col(facets = vars(group),
            scales = "free_y",
            space = "free") +
  labs(fill = "Sex") +
  scale_fill_viridis(discrete = TRUE, option = "viridis")

SexYLLPlot

ggsave("../output/SexYLLSubGroupPlot.png", SexYLLPlot, width = 10, height= 12)

######### Loop to simplify making of above plots ########################################

characteristics <- c("SEX", "Locality", 'IMD_quintile')
outcomes <- c("Deaths", "YLL")

for(outcome in outcomes) {
  for (characteristic in characteristics) {

    column_name <- sym(characteristic)

    data_i <- BSol %>%
      mutate(
        YLL = case_when(
          AgeInYears < 75 ~ (74.5 - AgeInYears) * `No of Deaths`,
          AgeInYears >= 75 ~ 0
        )
      ) %>%
      group_by(`ICD10 Short Title`, `group`, !!column_name) %>%
      summarise(`Deaths` = sum(`No of Deaths`)/10,
                `YLL` = sum(YLL)/10)

    max_value <- data_i %>%
      group_by(`ICD10 Short Title`, `group`) %>%
      summarize(total = sum(.data[[outcome]])) %>%
      pull(total) %>%
      max()

    plot_i <- ggplot(data= data_i, aes(x = `ICD10 Short Title`, y = .data[[outcome]], fill = .data[[characteristic]])) +
      geom_bar(position = "stack", stat= "identity") +
      labs( y = paste("Average Yearly", outcome, "2014 to 2023,", "by", characteristic), x = "") +
      coord_flip() +
      theme_bw() +
      scale_y_continuous(
        expand = c(0, 0), limits = c(0, 1.1*max_value)) +
      facet_col(facets = vars(group),
                scales = "free_y",
                space = "free") +
      scale_fill_viridis(discrete = TRUE, option = "magma")

    ggsave(paste("../output/", outcome, "_", characteristic, ".png", sep = ""), plot_i, width = 9, height= 12) }
}
