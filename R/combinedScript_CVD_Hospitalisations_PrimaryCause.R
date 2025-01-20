library(readxl)
library(dplyr)
library(writexl)
library(ggplot2)
library(ggforce)
library(rlang)
library(viridis)
library(stringr)
library(plotly)
library(htmlwidgets)

# load BSol hospitalization data

file_path = file.path("//svwvap1126.addm.ads.brm.pri",
                      "PHSensitive$",
                      "Intelligence",
                      "2. Requests",
                      "REQ3289 - BSol ICB CVD Project",
                      "BSol_CVD_inpatients_1819to2324.xlsx",
                      fsep="/")

BSol <- read_excel(file_path)

## Create columns for ICD10 groups and subgroups
BSol <- BSol %>%
  mutate(
    group = case_when(
      grepl("^I0[0-2]",`DiagnosisGroup`) ~ "Acute rheumatic fever",
      grepl("^I0[5-9]",`DiagnosisGroup`) ~ "Chronic rheumatic heart diseases",
      grepl("^I1[0-5]",`DiagnosisGroup`) ~ "Hypertensive diseases",
      grepl("^I2[0-5]",`DiagnosisGroup`) ~ "Ischaemic heart diseases",
      grepl("^I2[6-8]",`DiagnosisGroup`) ~ "Pulmonary heart disease and diseases of pulmonary circulation",
      grepl("^I[3-4]\\d|I5[0-2]",`DiagnosisGroup`) ~ "Other forms of heart disease",
      grepl("^I6[0-9]",`DiagnosisGroup`) ~ "Cerebrovascular diseases",
      grepl("^I7[0-9]",`DiagnosisGroup`) ~ "Diseases of arteries, arterioles and capillaries",
      grepl("^I8[0-9]",`DiagnosisGroup`) ~ "Diseases of veins, lymphatic vessels and lymph nodes, not elsewhere classified",
      grepl("^I9[5-9]",`DiagnosisGroup`) ~ "Chronic rheumatic heart diseases")) %>%
  rename(icd_subgroup = DiagnosisGroup,
         ConstituencyCode = Constituency)

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
BSol <- left_join(BSol, ICD10Listing, by="icd_subgroup")

# check if any subgroups missing
any(is.na(BSol$`ICD10 Short Title`))

# create Local Authority Name column #
BSol <- BSol %>%
  mutate(LocalAuthority = case_when (LocalAuthority == 'E08000025' ~ 'Birmingham',
                                     LocalAuthority == 'E08000029' ~ 'Solihull'))

#load LSOA lookup
LSOALookup <- read.csv("../data/LSOA-lookup.csv")

# join to BSol data
BSol <- left_join(BSol, LSOALookup, by = c('LSOA_2011' = 'LSOA')) #LSOA11 as LSOA21 results in NAs

# create age groups #
BSol <- BSol %>%
  mutate(
    AgeGroup = case_when(
      AgeOnAdmission < 16 ~ "0 - 15",
      AgeOnAdmission < 31 ~ "16 - 30",
      AgeOnAdmission < 46 ~ "31 - 45",
      AgeOnAdmission < 61 ~ "46 - 60",
      AgeOnAdmission < 76 ~ "61 - 75",
      AgeOnAdmission < 91 ~ "76 - 90",
      AgeOnAdmission >=91 ~ ">= 91"),
    AgeGroup = factor(AgeGroup, levels = c("0 - 15", "16 - 30", "31 - 45", "46 - 60", "61 - 75", "76 - 90", ">= 91"))
  )

BSol$IMD_quintile <- as.factor(BSol$IMD_quintile)

# load ethnicity coding
EthnicityCoding <- read_excel("../data/NHS-ethnicity-coding.xlsx")

BSol <- left_join(BSol, EthnicityCoding, by = c("Ethnic_Code" = "National code"))
#rename national code definition column to ethnicity
BSol <- BSol %>%
  rename(Ethnicity = `National code definition`)
# replace missing Ethnicity with unknown
BSol <- BSol %>%
  mutate(Ethnicity = ifelse(is.na(Ethnicity), "Unknown", Ethnicity))

## Join to clinical groups
ClinicalGroup <- read_excel("../data/ICD10_CVD_ShortTitles.xlsx") %>%
  select(- `ICD10 Short Title`)

BSol <- left_join(BSol, ClinicalGroup, by= c("icd_subgroup" = "Code"))

# remove cases where clinical subgroup has been coded as 'not cvd'
ClinicalGroup_CVD <- BSol %>%
  filter(`Clinical Subgroup` != 'Not CVD')


###############################################################################################################################################
                                               ## Primary Cause Clinical Groupings
##############################################################################################################################################

####### Loop to calculate CVD hospitalization count by Birmingham, Solihull, and BSol ######
LA_filters <- list(
  c("Birmingham"),
  c("Solihull"),
  c("Birmingham", "Solihull")
)

for (LA_filter in LA_filters) {

  data_i <- ClinicalGroup_CVD %>%
    filter(
      LocalAuthority %in% LA_filter
    ) %>%
    group_by(`Clinical Subgroup`, `Clinical Group`) %>%
    summarise(`Hospitalisations` = n()/6)
  if(length(LA_filter) == 2) {
    area = "BSol"
  } else {
    area = LA_filter[[1]]
  }

  write_xlsx(data_i, paste("../output/Hospitalisations/PrimaryCause/ClinicalGroups/Tables/PrimaryCause_Hospitalisations_", area, ".xlsx", sep = ""))

  plot_i <- ggplot(data= data_i, aes(x = `Clinical Subgroup`, y = Hospitalisations)) +
    geom_col(fill="#3488a6") +
    labs( y = paste("Average Yearly Hospitalisations (Primary Cause) 2018 to 2024,", area), x = "") +
    coord_flip() +
    theme_bw() +
    geom_text(aes(label = sprintf("%.2f", Hospitalisations)), colour = "black", size = 4.5, hjust = -0.2) +
    scale_y_continuous(
      expand = c(0, 0), limits = c(0, 1.11*max(data_i$Hospitalisations))) +
    facet_col(facets = vars(`Clinical Group`),
              scales = "free_y",
              space = "free") +
    theme(text = element_text(size=19)) #change font size of  text

  ggsave(paste("../output/Hospitalisations/PrimaryCause/ClinicalGroups/PrimaryCause_Hospitalisations_", area, ".svg", sep = ""), plot_i, width = 12, height= 10)
}

######### Loop to plot hospitalizations by age, sex, imd, locality and ethnicity ########################################

characteristics <- c('Locality', 'IMD_quintile', 'AgeGroup', 'Gender', 'Ethnicity')

legend_titles = list(
  "Gender" = "Sex",
  "IMD_quintile" = "IMD Quintile",
  "Locality" = "Locality",
  "AgeGroup" = "Age",
  "Ethnicity" = "Ethnicity"
)

for(characteristic in characteristics) {

  column_name <- sym(characteristic)

  data_i <- ClinicalGroup_CVD %>%
    group_by(`Clinical Subgroup`, `Clinical Group`, !!column_name) %>%
    summarise(`Hospitalisations` = n()/6)

  max_value <- data_i %>%
    group_by(`Clinical Subgroup`, `Clinical Group`) %>%
    summarize(total = sum(Hospitalisations)) %>%
    pull(total) %>%
    max()

  write_xlsx(data_i, paste("../output/Hospitalisations/PrimaryCause/ClinicalGroups/Tables/PrimaryCause_Hospitalisations_BSol_by_", characteristic, ".xlsx", sep = ""))

  plot_i <- ggplot(data= data_i, aes(x = `Clinical Subgroup`, y = Hospitalisations, fill = .data[[characteristic]])) +
    geom_bar(position = "stack", stat= "identity") +
    labs(y = paste("Average Yearly Hospitalisations (Primary Cause) 2018 to 2024, BSol, by", legend_titles[[characteristic]]), x = "", fill = legend_titles[[characteristic]]) +
    coord_flip() +
    theme_bw() +
    scale_y_continuous(
      expand = c(0, 0), limits = c(0, 1.1*max_value)) +
    facet_col(facets = vars(`Clinical Group`),
              scales = "free_y",
              space = "free") +
    scale_fill_viridis(discrete = TRUE, option = "magma", begin = 0.15, end = 0.95)

  ggsave(paste("../output/Hospitalisations/PrimaryCause/ClinicalGroups/PrimaryCause_Hospitalisations_BSol_by_", characteristic, ".png", sep = ""), plot_i, width = 12, height= 14)
}

###############################
########### comparison to deaths  (primary cause) ##################
#######################################

# load percentage deaths
PercentageDeaths <- read_excel("../output/Deaths/PrimaryCause/ClinicalGroups/Tables/PrimaryCausePercentageDeaths10Years.xlsx")

PercentageHospitalisations <- ClinicalGroup_CVD %>%
  group_by(`Clinical Subgroup`) %>%
  summarise(`Hospitalisations` = n()) %>%
  mutate(TotalHospitalisations = sum(Hospitalisations),
         PercentageHospitalisations = Hospitalisations/TotalHospitalisations *100)

#join
DeathsHospitalisation <- left_join(PercentageHospitalisations, PercentageDeaths, by="Clinical Subgroup")

#replace  NA percentage value with zero
DeathsHospitalisation <- DeathsHospitalisation %>% mutate(PercentageDeaths = ifelse(is.na(PercentageDeaths), 0, PercentageDeaths))

# Reshape the data for plotting
df_long <- DeathsHospitalisation %>%
  tidyr::pivot_longer(cols = c(PercentageHospitalisations, PercentageDeaths),
                      names_to = "Outcome",
                      values_to = "Percentage")

# Rename the Metric levels
df_long$Outcome <- recode(df_long$Outcome, "PercentageHospitalisations" = "Hospitalisations", "PercentageDeaths" = "Deaths")

# save output
write_xlsx(df_long, "../output/Hospitalisations_vs_deaths/PrimaryCause/ClinicalGroups/Tables/PrimaryCause_PercentageHospitalisationsAndDeaths_BSol.xlsx")

# Wrap the text of the Condition labels
df_long$`Clinical Subgroup` <- str_wrap(df_long$`Clinical Subgroup`, width = 40)

# Create the plot
HospVsDeaths <- ggplot(df_long, aes(x = `Clinical Subgroup`, y = Percentage, fill = Outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 50)) +
  facet_col(facets = vars(Outcome),
            scales = "free_y",
            space = "free") +
  labs(title = "Percentage of Hospitalisations (2018 to 2024) and Deaths (2014 to 2023), Primary Cause, BSol",
       x = "Clinical Subgroup",
       y = "Percentage",
       fill = "Outcome")  +
  geom_text(aes(label = sprintf("%.2f", Percentage)), colour = "black", size = 4, hjust = -0.2) +
  theme(
    plot.title.position = "plot", # Make the title span the full plot width including axis labels
    plot.title = element_text(hjust = 0.5)) + # Center-align the title
  theme(text = element_text(size=16)) #change font size of  text

HospVsDeaths

ggsave("../output/Hospitalisations_vs_deaths/PrimaryCause/ClinicalGroups/PrimaryCause_Hospitalisations_vs_Deaths_BSol.svg", HospVsDeaths, width = 12, height= 10)

#### Yearly number of deaths vs yearly number of hospitalizations scatter plot #####################################

# load data
YearlyHospitalisations <- read_excel("../output/Hospitalisations/PrimaryCause/ClinicalGroups/Tables/PrimaryCause_Hospitalisations_BSol.xlsx")
YearlyDeaths <- read_excel("../output/Deaths/PrimaryCause/ClinicalGroups/Tables/PrimaryCause_Deaths_YLL_BSol.xlsx")

YearlyDeaths <- YearlyDeaths %>% select(-`Clinical Group`)

#join
YearlyDeathsHospitalisation <- left_join(YearlyHospitalisations, YearlyDeaths, by="Clinical Subgroup")

# scatter plot
plot <- ggplot(YearlyDeathsHospitalisation,
               aes(x = `Hospitalisations`,
                   y = `Deaths`,
                   text = `Clinical Subgroup`,
                   color = `Clinical Group`
               ),
) +
  geom_point() +
  labs(title = "Average Yearly Deaths (2014 to 2023) and Hospitalisations (2018 to 2024) with CVD as a Primary Cause, BSol") +
  theme_bw()

plot <- ggplotly(plot)

saveWidget(plot, file="../output/Hospitalisations_vs_deaths/PrimaryCause/HospitalisationVsDeaths_ClinicalGroups_scatter.html")


###############################################################################################################################################
                                                   ## Primary Cause ICD10 Groupings
##############################################################################################################################################

####### Loop to calculate CVD hospitalization count by Birmingham, Solihull, and BSol ######

LA_filters <- list(
  c("Birmingham"),
  c("Solihull"),
  c("Birmingham", "Solihull")
)

for (LA_filter in LA_filters) {

    data_i <- BSol %>%
      filter(
        LocalAuthority %in% LA_filter
      ) %>%
      group_by(`ICD10 Short Title`, `group`) %>%
      summarise(`Hospitalisations` = n()/6)
    if(length(LA_filter) == 2) {
      area = "BSol"
    } else {
      area = LA_filter[[1]]
    }

    write_xlsx(data_i, paste("../output/Hospitalisations/PrimaryCause/ICD10Groups/Tables/PrimaryCause_Hospitalisations_", area, ".xlsx", sep = ""))

    plot_i <- ggplot(data= data_i, aes(x = `ICD10 Short Title`, y = Hospitalisations)) +
      geom_col(fill="#3488a6") +
      labs( y = paste("Average Yearly Hospitalisations (Primary Cause) 2018 to 2024,", area), x = "") +
      coord_flip() +
      theme_bw() +
      geom_text(aes(label = sprintf("%.2f", Hospitalisations)), colour = "black", size = 3, hjust = -0.2) +
      scale_y_continuous(
        expand = c(0, 0), limits = c(0, 1.11*max(data_i$Hospitalisations))) +
      facet_col(facets = vars(group),
                scales = "free_y",
                space = "free")

    ggsave(paste("../output/Hospitalisations/PrimaryCause/ICD10Groups/PrimaryCause_Hospitalisations_", area, ".png", sep = ""), plot_i, width = 9, height= 12)
}

######### Loop to plot hospitalizations by age, sex, imd, locality, Ethnicity ########################################

characteristics <- c('Locality', 'IMD_quintile', 'AgeGroup', 'Gender', 'Ethnicity')

legend_titles = list(
  "Gender" = "Sex",
  "IMD_quintile" = "IMD Quintile",
  "Locality" = "Locality",
  "AgeGroup" = "Age",
  "Ethnicity" = "Ethnicity"
)

for(characteristic in characteristics) {

  column_name <- sym(characteristic)

  data_i <- BSol %>%
    group_by(`ICD10 Short Title`, `group`, !!column_name) %>%
    summarise(`Hospitalisations` = n()/6)

  max_value <- data_i %>%
    group_by(`ICD10 Short Title`, `group`) %>%
    summarize(total = sum(Hospitalisations)) %>%
    pull(total) %>%
    max()

    write_xlsx(data_i, paste("../output/Hospitalisations/PrimaryCause/ICD10Groups/Tables/PrimaryCause_Hospitalisations_BSol_by_", characteristic, ".xlsx", sep = ""))

    plot_i <- ggplot(data= data_i, aes(x = `ICD10 Short Title`, y = Hospitalisations, fill = .data[[characteristic]])) +
      geom_bar(position = "stack", stat= "identity") +
      labs(y = paste("Average Yearly Hospitalisations (Primary Cause) 2018 to 2024, BSol, by", legend_titles[[characteristic]]), x = "", fill = legend_titles[[characteristic]]) +
      coord_flip() +
      theme_bw() +
      scale_y_continuous(
        expand = c(0, 0), limits = c(0, 1.1*max_value)) +
      facet_col(facets = vars(group),
                scales = "free_y",
                space = "free") +
      scale_fill_viridis(discrete = TRUE, option = "magma", begin = 0.15, end = 0.95)

    ggsave(paste("../output/Hospitalisations/PrimaryCause/ICD10Groups/PrimaryCause_Hospitalisations_BSol_by_", characteristic, ".png", sep = ""), plot_i, width = 12, height= 14)
    }

##########################################################
############ comparison to deaths - primary cause, ICD10 groups ##################
#############################################

# load percentage deaths
PercentageDeaths <- read_excel("../output/Deaths/PrimaryCause/ICD10Groups/Tables/PrimaryCausePercentageDeaths10Years.xlsx")

PercentageHospitalisations <- BSol %>%
  group_by(group) %>%
  summarise(`Hospitalisations` = n()) %>%
  mutate(TotalHospitalisations = sum(Hospitalisations),
         PercentageHospitalisations = Hospitalisations/TotalHospitalisations *100)

#join
DeathsHospitalisation <- left_join(PercentageHospitalisations, PercentageDeaths, by=c("group" = "CVD_group"))

#replace  NA percentage value with zero
DeathsHospitalisation <- DeathsHospitalisation %>% mutate(PercentageDeaths = ifelse(is.na(PercentageDeaths), 0, PercentageDeaths))

# Reshape the data for plotting
df_long <- DeathsHospitalisation %>%
  tidyr::pivot_longer(cols = c(PercentageHospitalisations, PercentageDeaths),
                      names_to = "Outcome",
                      values_to = "Percentage")

# Rename the Metric levels
df_long$Outcome <- recode(df_long$Outcome, "PercentageHospitalisations" = "Hospitalisations", "PercentageDeaths" = "Deaths")

# save output
write_xlsx(df_long, "../output/Hospitalisations_vs_deaths/PrimaryCause/ICD10Groups/Tables/PrimaryCause_PercentageHospitalisationsAndDeaths.xlsx")

# Wrap the text of the Condition labels
df_long$group <- str_wrap(df_long$group, width = 25)

# Create the plot
HospVsDeaths <- ggplot(df_long, aes(x = group, y = Percentage, fill = Outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 50)) +
  facet_col(facets = vars(Outcome),
            scales = "free_y",
            space = "free") +
  labs(title = "Percentage of Hospitalisations (2018 to 2024) and Deaths (2014 to 2023) by ICD10 Condition Group, Primary Cause, BSol",
       x = "ICD10 Group",
       y = "Percentage",
       fill = "Outcome")  +
  geom_text(aes(label = sprintf("%.2f", Percentage)), colour = "black", size = 4, hjust = -0.2) +
  theme(
    plot.title.position = "plot", # Make the title span the full plot width including axis labels
    plot.title = element_text(hjust = 0.5)) + # Center-align the title
  theme(text = element_text(size=16))

HospVsDeaths

ggsave("../output/Hospitalisations_vs_deaths/PrimaryCause/ICD10Groups/PrimaryCause_Hospitalisations_vs_Deaths_BSol.svg", HospVsDeaths, width = 10, height= 12)

#### Yearly number of deaths vs yearly number of hospitalizations scatter plot #####################################

# load data
YearlyHospitalisations <- read_excel("../output/Hospitalisations/PrimaryCause/ICD10Groups/Tables/PrimaryCause_Hospitalisations_BSol.xlsx")
YearlyDeaths <- read_excel("../output/Deaths/PrimaryCause/ICD10Groups/Tables/PrimaryCause_Deaths_YLL_BSol.xlsx")

YearlyDeaths <- YearlyDeaths %>% select(-CVD_group)

#join
YearlyDeathsHospitalisation <- left_join(YearlyHospitalisations, YearlyDeaths, by="ICD10 Short Title")

# Rename group column
YearlyDeathsHospitalisation <- YearlyDeathsHospitalisation %>%
  rename('ICD10 Group' = group)

# scatter plot
plot <- ggplot(YearlyDeathsHospitalisation,
       aes(x = `Hospitalisations`,
           y = `Deaths`,
           text = `ICD10 Short Title`,
           color = `ICD10 Group`
       ),
) +
  geom_point() +
  labs(title = "Average Yearly Deaths (2014 to 2023) and Hospitalisations (2018 to 2024) with CVD as a Primary Cause, BSol") +
  theme_bw()

plot <- ggplotly(plot)

saveWidget(plot, file="../output/Hospitalisations_vs_deaths/PrimaryCause/HospitalisationVsDeaths_ICD10Groups_scatter.html")


########### admission method
AdmissionMethod <- BSol %>%
  group_by(`AdmissionMethodDescription`) %>%
  summarise(`AverageAdmissions` = n()/6)

write_xlsx(AdmissionMethod, "../output/AdmissionMethod.xlsx")
