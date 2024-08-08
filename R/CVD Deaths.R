library(dplyr)
library(ggplot2)
library(ggforce)
library(viridis)

## Load data
file_path = file.path("//svwvap1126.addm.ads.brm.pri",
                      "PHSensitive$",
                      "Intelligence",
                      "2. Requests",
                      "REQ3289 - BSol ICB CVD Project",
                      "REQ3289_BSOL_CVD_Deaths_10years_2014_to_2023.xlsx",
                      fsep="/")

data <- readxl::read_excel(file_path)

colnames(data)

## Create columns for ICD10 groups and subgroups
data <- data %>%
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
any(is.na(data$group))

#### subgroup definitions####

#load icd10 definitions
ICD10Listing <- readxl::read_excel("../Data/tlkp_ICD10_DiagnosisCodes_FullListing.xlsx") %>%
  select(`ICD10 Code`, `ICD10 Short Title`)

# Extract first 3 characters to get subgroup
ICD10Listing <- ICD10Listing %>%
  mutate(
    icd_subgroup = stringr::str_extract(`ICD10 Code`, "(I\\d{2})")
  ) %>%
  distinct(icd_subgroup, .keep_all = TRUE)

# join
data <- left_join(data, ICD10Listing, by= "icd_subgroup")

# check if any subgroups missing
any(is.na(data$`ICD10 Short Title`))

########################### ###############
                      ######### totals #########
#####################

# Average Yearly Total Per ICD10 group
GroupedTotalDeaths <- data %>%
  group_by(`group`) %>%
  summarise(Deaths = sum(`No of Deaths`)) %>%
  mutate(AverageYearlyDeaths = Deaths / 10)

# Average Yearly Total Per ICD10 subgroup
SubGroupTotalDeaths <- data %>%
  group_by(`ICD10 Short Title`, `group`) %>%
  summarise(Deaths = sum(`No of Deaths`)) %>%
  mutate(AverageYearlyDeaths = Deaths / 10)
#plot
AverageYearlyDeathsPlot <- ggplot(data= SubGroupTotalDeaths, aes(x = `ICD10 Short Title`, y = AverageYearlyDeaths)) +
  geom_col(fill="#3488a6") +
  labs( y = "Average Yearly Deaths, 2014 to 2023, BSol", x = "") +
  coord_flip() +
  theme_bw() +
  geom_text(aes(label = AverageYearlyDeaths), colour = "black", size = 3, hjust = -0.2) +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 700)) +
  facet_col(facets = vars(group),
            scales = "free_y",
            space = "free")

AverageYearlyDeathsPlot

ggsave("../Figures/AverageYearlyDeathsPlot.PNG", AverageYearlyDeathsPlot, width = 9, height= 12)

####################                        #################
                           ### Early deaths (under 75) #######
#####################      #####################
# create early death column (age <75)
data <- data %>%
  mutate(
    EarlyDeath = case_when(
      AgeInYears < 75 ~ "Under 75",
      AgeInYears >=75 ~ "75 and over"))

# Average Yearly Total Per ICD10 subgroup
EarlyDeathsSubGroupTotal <- data %>%
  filter(EarlyDeath == "Under 75") %>%
  group_by(`ICD10 Short Title`, `group`) %>%
  summarise(Deaths = sum(`No of Deaths`)) %>%
  mutate(AverageYearlyDeaths = Deaths / 10)
#plot
EarlyDeathsSubGroupTotalPlot <- ggplot(data= EarlyDeathsSubGroupTotal, aes(x = `ICD10 Short Title`, y = AverageYearlyDeaths)) +
  geom_col(fill="#3488a6") +
  labs( y = "Average Yearly Deaths under 75 years old, 2014 to 2023, BSol", x = "") +
  coord_flip() +
  theme_bw() +
  geom_text(aes(label = AverageYearlyDeaths), colour = "black", size = 3, hjust = -0.2) +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 700)) +
  facet_col(facets = vars(group),
            scales = "free_y",
            space = "free")

EarlyDeathsSubGroupTotalPlot

ggsave("../Figures/EarlyDeathsSubGroupTotalPlot.PNG", EarlyDeathsSubGroupTotalPlot, width = 9, height= 12)


##################               ####################
  ##### Age groups stacked bar
################              ###############

# create age groups
data <- data %>%
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

# Average Yearly Total Per ICD10 subgroup
AgeDeathsSubGroup <- data %>%
  group_by(`ICD10 Short Title`, `group`, `AgeGroup`) %>%
  summarise(Deaths = sum(`No of Deaths`)) %>%
  mutate(AverageYearlyDeaths = Deaths / 10)
#plot
AgeDeathsSubGroupPlot <- ggplot(data= AgeDeathsSubGroup, aes(x = `ICD10 Short Title`, y = AverageYearlyDeaths, fill = AgeGroup)) +
  geom_bar(position = "stack", stat= "identity") +
  labs( y = "Average Yearly Deaths, 2014 to 2023, BSol", x = "") +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 700)) +
  facet_col(facets = vars(group),
            scales = "free_y",
            space = "free") +
  labs(fill = "Age Group") +
  scale_fill_viridis(discrete = TRUE, option = "magma")

AgeDeathsSubGroupPlot

ggsave("../Figures/AgeDeathsSubGroupPlot.PNG", AgeDeathsSubGroupPlot, width = 10, height= 12)






