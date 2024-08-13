library(readxl)
library(dplyr)
library(ggplot2)
library(ggforce)
library(viridis)

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

### join Birmingham and Solihull to create BSol data
colnames(BhamData)
colnames(SolihullData)

## remove unneeded columns from Birmingham dataframe
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

########### Locality and Deprivation ######

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

# create Local Authority Name column #

BSol <- BSol %>%
  mutate(LocalAuthority = case_when (`LOCAL AUTHORITY CODE OF USUAL RESIDENCE OF DECEASED` == 'E08000025' ~ 'Birmingham',
                                     `LOCAL AUTHORITY CODE OF USUAL RESIDENCE OF DECEASED` == 'E08000029' ~ 'Solihull'))


####### Loop to calculate CVD death count & YLL by Birmingham, Solihull, and BSol ######

LA_filters <- list(
  c("Birmingham"),
  c("Solihull"),
  c("Birmingham", "Solihull")
)

outcomes <- c("Deaths", "YLL")

for (outcome in outcomes) {
  for (LA_filter in LA_filters) {

  data_i <- BSol %>%
    filter(
      LocalAuthority %in% LA_filter
    ) %>%
    mutate(
      YLL = case_when(
        AgeInYears < 75 ~ (74.5 - AgeInYears) * `No of Deaths`,
        AgeInYears >= 75 ~ 0
        )
    ) %>%
    group_by(`ICD10 Short Title`, `group`) %>%
    summarise(`Deaths` = sum(`No of Deaths`)/10,
              `YLL` = sum(YLL)/10)
  if(length(LA_filter) == 2) {
    area = "BSol"
  } else {
    area = LA_filter[[1]]
  }

 plot_i <- ggplot(data= data_i, aes(x = `ICD10 Short Title`, y = .data[[outcome]])) +
    geom_col(fill="#3488a6") +
    labs( y = paste("Average Yearly", outcome, "2014 to 2023,", area), x = "") +
    coord_flip() +
    theme_bw() +
    geom_text(aes(label = .data[[outcome]]), colour = "black", size = 3, hjust = -0.2) +
    scale_y_continuous(
      expand = c(0, 0), limits = c(0, 1.1*max(data_i[[outcome]]))) +
    facet_col(facets = vars(group),
              scales = "free_y",
              space = "free")

ggsave(paste("../output/", outcome, "_", area, ".png", sep = ""), plot_i, width = 9, height= 12)

}
}






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
    expand = c(0, 0), limits = c(0, 3000)) +
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

##########################
### Years life lost #######
############################

#Calculate YLL for each row
BhamData <- BhamData %>%
  filter(`AgeInYears` <= 74) %>%
  mutate(YLL = (74.5 - `AgeInYears`) * `No of Deaths`)

# Average Yearly Total Per ICD10 subgroup
GroupedYLL <- BhamData %>%
  group_by(`ICD10 Short Title`, `group`) %>%
  summarise(YLL = sum(`YLL`)) %>%
  mutate(AverageYLL = YLL / 10)
#plot
AverageYLLPlot <- ggplot(data= GroupedYLL, aes(x = `ICD10 Short Title`, y = AverageYLL)) +
  geom_col(fill="#3488a6") +
  labs( y = "Average Years of Life Lost, 2014 to 2023, Birmingham", x = "") +
  coord_flip() +
  theme_bw() +
  geom_text(aes(label = AverageYLL), colour = "black", size = 3, hjust = -0.2) +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 3000)) +
  facet_col(facets = vars(group),
            scales = "free_y",
            space = "free")

AverageYLLPlot

## YLL by Sex
Sex <- BhamData %>%
  group_by(`UNDERLYING CAUSE OF DEATH CODE`, `SEX`) %>%
  summarise(YLL = sum(`YLL`)) %>%
  mutate(AverageYLL = YLL / 10) # divide by 10 to get yearly

## YLL by Locality

## YLL by Deprivation