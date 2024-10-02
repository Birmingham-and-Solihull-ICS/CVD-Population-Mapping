library(readxl)
library(dplyr)
library(ggplot2)
library(ggforce)
library(viridis)
library(rlang)
library(writexl)

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

## remove unneeded columns from Solihull dataframe
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

## Create column for ICD10 subgroups (3 digit ICD10 code)
BSol <- BSol %>%
  mutate(
    icd_subgroup = stringr::str_extract(`UNDERLYING CAUSE OF DEATH CODE`, "(I\\d{2})"))

#### subgroup definitions ####

#load icd10 definitions
ICD10Listing <- read_excel("../../data/tlkp_ICD10_DiagnosisCodes_FullListing.xlsx") %>%
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

# create Local Authority name column #

BSol <- BSol %>%
  mutate(LocalAuthority = case_when (`LOCAL AUTHORITY CODE OF USUAL RESIDENCE OF DECEASED` == 'E08000025' ~ 'Birmingham',
                                     `LOCAL AUTHORITY CODE OF USUAL RESIDENCE OF DECEASED` == 'E08000029' ~ 'Solihull'))

#load LSOA lookup
LSOALookup <- read.csv("../../data/LSOA-lookup.csv")

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


BSol$IMD_quintile <- as.factor(BSol$IMD_quintile)

BSol$SEX <- factor(BSol$SEX, levels = c(1, 2), labels = c("Male", "Female"))


## Join to clinical groups
ClinicalGroup <- read_excel("../../data/ICD10_CVD_ShortTitles.xlsx")

BSol <- left_join(BSol, ClinicalGroup, by= c("icd_subgroup" = "Code"))



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
      group_by(`Clinical Subgroup`, `Clinical Group`) %>%
      summarise(`Deaths` = sum(`No of Deaths`)/10,
                `YLL` = sum(YLL)/10)
    if(length(LA_filter) == 2) {
      area = "BSol"
    } else {
      area = LA_filter[[1]]
    }

    write_xlsx(data_i, paste("../../output/Clinical_Groupings/Primary_Cause/Deaths/Tables/PrimaryCause_DeathsYLL_", area, ".xlsx", sep = ""))


    plot_i <- ggplot(data= data_i, aes(x = `Clinical Subgroup`, y = .data[[outcome]])) +
      geom_col(fill="#3488a6") +
      labs( y = paste("Average Yearly", outcome, "(Primary Cause), 2014 to 2023,", area), x = "") +
      coord_flip() +
      theme_bw() +
      geom_text(aes(label = .data[[outcome]]), colour = "black", size = 3, hjust = -0.2) +
      scale_y_continuous(
        expand = c(0, 0), limits = c(0, 1.1*max(data_i[[outcome]]))) +
      facet_col(facets = vars(`Clinical Group`),
                scales = "free_y",
                space = "free")

    ggsave(paste("../../output/Clinical_Groupings/Primary_Cause/Deaths/PrimaryCause_", outcome, "_", area, ".png", sep = ""), plot_i, width = 9, height= 12)

  }
}


##################               ####################
##### deaths plot by age
################              ###############

# Average Yearly Total Per ICD10 subgroup
AgeDeathsSubGroup <- BSol %>%
  group_by(`Clinical Subgroup`, `Clinical Group`, `AgeGroup`) %>%
  summarise(`Deaths` = sum(`No of Deaths`)/10)

write_xlsx(AgeDeathsSubGroup, "../../output/Clinical_Groupings/Primary_Cause/Deaths/Tables/PrimaryCause_Deaths_BSol_by_Age.xlsx")

#plot
AgeDeathsSubGroupPlot <- ggplot(data= AgeDeathsSubGroup, aes(x = `Clinical Subgroup`, y = Deaths, fill = AgeGroup)) +
  geom_bar(position = "stack", stat= "identity") +
  labs( y = "Average Yearly Deaths (Primary Cause), 2014 to 2023, BSol", x = "", fill = "Age Group") +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 1250)) +
  facet_col(facets = vars(`Clinical Group`),
            scales = "free_y",
            space = "free") +
  scale_fill_viridis(discrete = TRUE, option = "magma", begin = 0.2, end = 0.9)

AgeDeathsSubGroupPlot

ggsave("../../output/Clinical_Groupings/Primary_Cause/Deaths/PrimaryCause_Deaths_BSol_Age.png", AgeDeathsSubGroupPlot, width = 10, height= 12)


######### Loop to plot YLL and deaths by sex, imd and locality ########################################

characteristics <- c("SEX", "Locality", 'IMD_quintile')
outcomes <- c("Deaths", "YLL")

legend_titles = list(
  "SEX" = "Sex",
  "IMD_quintile" = "IMD Quintile",
  "Locality" = "Locality"
)

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
      group_by(`Clinical Subgroup`, `Clinical Group`, !!column_name) %>%
      summarise(`Deaths` = sum(`No of Deaths`)/10,
                `YLL` = sum(YLL)/10)

    max_value <- data_i %>%
      group_by(`Clinical Subgroup`, `Clinical Group`) %>%
      summarize(total = sum(.data[[outcome]])) %>%
      pull(total) %>%
      max()

    write_xlsx(data_i, paste("../../output/Clinical_Groupings/Primary_Cause/Deaths/Tables/PrimaryCause_Deaths_YLL_BSol_by_", characteristic, ".xlsx", sep = ""))

    plot_i <- ggplot(data= data_i, aes(x = `Clinical Subgroup`, y = .data[[outcome]], fill = .data[[characteristic]])) +
      geom_bar(position = "stack", stat= "identity") +
      labs(y = paste("Average Yearly", outcome, "(Primary Cause), 2014 to 2023, BSol,", "by", legend_titles[[characteristic]]), x = "", fill = legend_titles[[characteristic]]) +
      coord_flip() +
      theme_bw() +
      scale_y_continuous(
        expand = c(0, 0), limits = c(0, 1.1*max_value)) +
      facet_col(facets = vars(`Clinical Group`),
                scales = "free_y",
                space = "free") +
      scale_fill_viridis(discrete = TRUE, option = "magma", begin = 0.25, end = 0.85)

    ggsave(paste("../../output/Clinical_Groupings/Primary_Cause/Deaths/PrimaryCause_", outcome, "_", characteristic, "_BSol.png", sep = ""), plot_i, width = 10, height= 12) }

}

## Percentage of deaths by clinical subgroup
PercentageDeaths <- BSol %>%
  group_by(`Clinical Subgroup`) %>%
  summarise(`Deaths` = sum(`No of Deaths`)) %>%
  mutate(TotalDeaths = sum(Deaths),
         PercentageDeaths = Deaths/TotalDeaths *100)

write_xlsx(PercentageDeaths, "../../output/Clinical_Groupings/Primary_Cause/Deaths/Tables/PrimaryCause_PercentageDeaths_ClinicalSubgroup_BSol.xlsx")
