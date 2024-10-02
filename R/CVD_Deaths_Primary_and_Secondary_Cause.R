library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)
library(ggplot2)
library(ggforce)
library(viridis)

## Load Birmingham data
file_path_bham = file.path("//svwvap1126.addm.ads.brm.pri",
                           "PHSensitive$",
                           "Intelligence",
                           "2. Requests",
                           "REQ3289 - BSol ICB CVD Project",
                           "REQ3289_BIRM_CVD_Deaths_AllDiagnoses_10years_2014_to_2023.xlsx",
                           fsep="/")

BhamData <- read_excel(file_path_bham)

# Load Solihull data
file_path_solihull = file.path("//svwvap1126.addm.ads.brm.pri",
                               "PHSensitive$",
                               "Intelligence",
                               "2. Requests",
                               "REQ3289 - BSol ICB CVD Project",
                               "20240925_Solihull_CVD_Deaths_AllCause.xlsx",
                               fsep="/")

SolihullData <- read_excel(file_path_solihull)

## remove unneeded columns from Birmingham dataframe
colnames(BhamData)
colnames(SolihullData)

BhamData <- subset(BhamData, select = -c(MonthText, Analysis, `DATE OF DEATH OF DECEASED`))

## remove unneeded columns from Solihull dataframe
SolihullData <- subset(SolihullData, select = -c(CCG_OF_REGISTRATION_CODE, DATE_OF_DEATH))

# rename solihull columns to match Birmingham
SolihullData <- SolihullData %>%
  rename(`LOCAL AUTHORITY CODE OF USUAL RESIDENCE OF DECEASED` = `ULA_OF_RESIDENCE_CODE`,
         `UNDERLYING CAUSE OF DEATH CODE` = UNDERLYING_CAUSE_OF_DEATH,
         `SEX` = sex,
         `AgeInYears` = Age,
         `LOWER SOA CODE OF USUAL RESIDENCE OF DECEASED` = LSOA_OF_RESIDENCE_CODE,
         `CAUSES1 OF DEATH MENTIONED ON DEATH CERTIFICATE` = `CAUSE_OF_DEATH_ICD_CODE_1`,
         `CAUSES2 OF DEATH MENTIONED ON DEATH CERTIFICATE` = `CAUSE_OF_DEATH_ICD_CODE_2`,
         `CAUSES3 OF DEATH MENTIONED ON DEATH CERTIFICATE` = `CAUSE_OF_DEATH_ICD_CODE_3`,
         `CAUSES4 OF DEATH MENTIONED ON DEATH CERTIFICATE` = `CAUSE_OF_DEATH_ICD_CODE_4`,
         `CAUSES5 OF DEATH MENTIONED ON DEATH CERTIFICATE` = `CAUSE_OF_DEATH_ICD_CODE_5`,
         `CAUSES6 OF DEATH MENTIONED ON DEATH CERTIFICATE` = `CAUSE_OF_DEATH_ICD_CODE_6`,
         `CAUSES7 OF DEATH MENTIONED ON DEATH CERTIFICATE` = `CAUSE_OF_DEATH_ICD_CODE_7`,
         `CAUSES8 OF DEATH MENTIONED ON DEATH CERTIFICATE` = `CAUSE_OF_DEATH_ICD_CODE_8`)

# join data frames to create BSol data frame
BSol <- rbind(BhamData, SolihullData)


# create Local Authority name column #
BSol <- BSol %>%
  mutate(LocalAuthority = case_when (`LOCAL AUTHORITY CODE OF USUAL RESIDENCE OF DECEASED` == 'E08000025' ~ 'Birmingham',
                                     `LOCAL AUTHORITY CODE OF USUAL RESIDENCE OF DECEASED` == 'E08000029' ~ 'Solihull'))

# create an ID column (needed to remove duplicate ICD10 subgroups for one individual)
BSol$index <- 1:nrow(BSol)

# pivot longer so each cause code is one row
death_pivot <- BSol %>%
  pivot_longer(
    cols = contains("CAUSE"),
    names_to='Cause Number',
    values_to='ICD10 Code')

#remove rows from data frame with NA values in ICD10 Code column
death_pivot <- death_pivot %>% drop_na('ICD10 Code')

# extract first 3 digits of ICD10 code to get ICD10 sub-group (minimises number of missing condition names and simplifies number of conditions)
death_pivot <- death_pivot %>%
  mutate(
    icd_subgroup = str_extract(`ICD10 Code`, "^.{3}")
  )

# add CVD ICD10 Group names
death_pivot <- death_pivot %>%
  mutate(
    CVD_group = case_when(
      grepl("^I0[0-2]",`ICD10 Code`) ~ "Acute rheumatic fever",
      grepl("^I0[5-9]",`ICD10 Code`) ~ "Chronic rheumatic heart diseases",
      grepl("^I1[0-5]",`ICD10 Code`) ~ "Hypertensive diseases",
      grepl("^I2[0-5]",`ICD10 Code`) ~ "Ischaemic heart diseases",
      grepl("^I2[6-8]",`ICD10 Code`) ~ "Pulmonary heart disease and diseases of pulmonary circulation",
      grepl("^I[3-4]\\d|I5[0-2]",`ICD10 Code`) ~ "Other forms of heart disease",
      grepl("^I6[0-9]",`ICD10 Code`) ~ "Cerebrovascular diseases",
      grepl("^I7[0-9]",`ICD10 Code`) ~ "Diseases of arteries, arterioles and capillaries",
      grepl("^I8[0-9]",`ICD10 Code`) ~ "Diseases of veins, lymphatic vessels and lymph nodes, not elsewhere classified",
      grepl("^I9[5-9]",`ICD10 Code`) ~ "Chronic rheumatic heart diseases")
  )

# remove duplicate ICD10 subgroups for each individual
death_pivot <- death_pivot %>% select(-one_of('Cause Number', 'ICD10 Code')) # remove cause number column and ICD10 code column
death_pivot <- distinct(death_pivot)

#load LSOA lookup
LSOALookup <- read.csv("../data/LSOA-lookup.csv")
# join to data
death_pivot <- left_join(death_pivot, LSOALookup, by = c('LOWER SOA CODE OF USUAL RESIDENCE OF DECEASED' = 'LSOA'))

# create age groups #
death_pivot <- death_pivot %>%
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

# IMD
death_pivot$IMD_quintile <- as.factor(death_pivot$IMD_quintile)
# Sex
death_pivot$SEX <- factor(death_pivot$SEX, levels = c(1, 2), labels = c("Male", "Female"))

# load ICD10 definitions spreadsheet
ICD10Listing <- read_excel("../data/tlkp_ICD10_DiagnosisCodes_FullListing.xlsx") %>%
  select(`ICD10 Code`, `ICD10 Short Title`, `Simplified Cause`)

# Extract first 3 characters to get subgroup
ICD10Listing <- ICD10Listing %>%
  mutate(
    icd_subgroup = str_extract(`ICD10 Code`, "^.{3}")
  ) %>%
  distinct(icd_subgroup, .keep_all = TRUE) %>% # select unique icd_subgroups so only have one of each
  select(-('ICD10 Code'))  # remove ICD10 code column as no longer correct (as using subgroup only)

# join subgroup look up with deaths data
death_pivot <- left_join(death_pivot, ICD10Listing, by="icd_subgroup")

# count number of subgroups missing
sum(is.na(death_pivot$`ICD10 Short Title`))

#count number of CVD sub groups missing
cvd <- filter(death_pivot, grepl("^I", `icd_subgroup`)) # create data frame with just cvd ICD10 subgroups (i.e, code ICD10 code starts with I)
sum(is.na(cvd$`ICD10 Short Title`)) # 0 missing values

#################################           ####################################
####### Loop to calculate average yearly CVD death count & YLL for Birmingham, Solihull and BSol ############
##########################          ####################################

LA_filters <- list(
  c("Birmingham"),
  c("Solihull"),
  c("Birmingham", "Solihull")
)

outcomes <- c("Deaths", "YLL")

for (outcome in outcomes) {
  for (LA_filter in LA_filters) {

    data_i <- cvd %>%
      filter(
        LocalAuthority %in% LA_filter
      ) %>%
      mutate(
        YLL = case_when(
          AgeInYears < 75 ~ (74.5 - AgeInYears),
          AgeInYears >= 75 ~ 0
        )
      ) %>%
      group_by(`ICD10 Short Title`, `CVD_group`) %>%
      summarise(`Deaths` = n()/10,
                `YLL` = sum(YLL)/10)

    if(length(LA_filter) == 2) {
      area = "BSol"
    } else {
      area = LA_filter[[1]]
    }

    write_xlsx(data_i, paste("../output/AllCause/Deaths/Tables/Deaths_YLL_AllCause_", area, ".xlsx", sep = ""))


    plot_i <- ggplot(data= data_i, aes(x = `ICD10 Short Title`, y = .data[[outcome]])) +
      geom_col(fill="#3488a6") +
      labs( y = paste("Average Yearly", outcome, "(All Cause), 2014 to 2023", area), x = "") +
      coord_flip() +
      theme_bw() +
      geom_text(aes(label = .data[[outcome]]), colour = "black", size = 3, hjust = -0.2) +
      scale_y_continuous(
        expand = c(0, 0), limits = c(0, 1.1*max(data_i[[outcome]]))) +
      facet_col(facets = vars(CVD_group),
                scales = "free_y",
                space = "free")

    ggsave(paste("../output/AllCause/Deaths/AllCause_", outcome, "_", area, ".png", sep = ""), plot_i, width = 9, height= 12)

  }
  }


##### deaths plot by age ###########################


# Average Yearly Total Per ICD10 subgroup
AgeDeathsSubGroup <- cvd %>%
  group_by(`ICD10 Short Title`, `CVD_group`, `AgeGroup`) %>%
  summarise(`Deaths` = n()/10)

write_xlsx(AgeDeathsSubGroup, "../output/AllCause/Deaths/Tables/BSol_Deaths_AllCause_by_Age.xlsx")

#plot
AgeDeathsSubGroupPlot <- ggplot(data= AgeDeathsSubGroup, aes(x = `ICD10 Short Title`, y = Deaths, fill = AgeGroup)) +
  geom_bar(position = "stack", stat= "identity") +
  labs( y = "Average Yearly Deaths (All Cause), 2014 to 2023, BSol, by Age", x = "", fill = "Age Group") +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 1500)) +
  facet_col(facets = vars(CVD_group),
            scales = "free_y",
            space = "free") +
  scale_fill_viridis(discrete = TRUE, option = "magma", begin = 0.2, end = 0.9)

AgeDeathsSubGroupPlot

ggsave("../output/AllCause/Deaths/BSol_AllCause_Deaths_Age.png", AgeDeathsSubGroupPlot, width = 10, height= 12)


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

    data_i <- cvd %>%
      mutate(
        YLL = case_when(
          AgeInYears < 75 ~ (74.5 - AgeInYears),
          AgeInYears >= 75 ~ 0
        )
      ) %>%
      group_by(`ICD10 Short Title`, `CVD_group`, !!column_name) %>%
      summarise(`Deaths` = n()/10,
                `YLL` = sum(YLL)/10)

    max_value <- data_i %>%
      group_by(`ICD10 Short Title`, `CVD_group`) %>%
      summarize(total = sum(.data[[outcome]])) %>%
      pull(total) %>%
      max()

    write_xlsx(data_i, paste("../output/AllCause/Deaths/Tables/BSol_AllCause_Deaths_YLL_by_", characteristic, ".xlsx", sep = ""))

    plot_i <- ggplot(data= data_i, aes(x = `ICD10 Short Title`, y = .data[[outcome]], fill = .data[[characteristic]])) +
      geom_bar(position = "stack", stat= "identity") +
      labs(y = paste("Average Yearly", outcome, "(All Cause), 2014 to 2023, BSol,", "by", legend_titles[[characteristic]]), x = "", fill = legend_titles[[characteristic]]) +
      coord_flip() +
      theme_bw() +
      scale_y_continuous(
        expand = c(0, 0), limits = c(0, 1.1*max_value)) +
      facet_col(facets = vars(CVD_group),
                scales = "free_y",
                space = "free") +
      scale_fill_viridis(discrete = TRUE, option = "magma", begin = 0.25, end = 0.85)

    ggsave(paste("../output/AllCause/Deaths/BSol_AllCause_", outcome, "_", characteristic, ".png", sep = ""), plot_i, width = 10, height= 12) }

}




