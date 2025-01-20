library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggforce)
library(viridis)
library(writexl)
library(stringr)

## Load Birmingham and Solihull data
file_path = file.path("//svwvap1126.addm.ads.brm.pri",
                           "PHSensitive$",
                           "Intelligence",
                           "2. Requests",
                           "REQ3289 - BSol ICB CVD Project",
                           "BSol_CVD_Deaths_with_Ethnicity_2014to2023.xlsx",
                           fsep="/")

BSol <- read_excel(file_path)

# create Local Authority name column
BSol <- BSol %>%
  mutate(LocalAuthority = case_when (`ULA_OF_RESIDENCE_CODE` == 'E08000025' ~ 'Birmingham',
                                     `ULA_OF_RESIDENCE_CODE` == 'E08000029' ~ 'Solihull'))

# pivot longer so each cause code is one row
death_pivot <- BSol %>%
  pivot_longer(
    cols = contains("_COD_"),
    names_to='Cause Number',
    values_to='ICD10 Code')

#remove rows from data frame with NULL in ICD10 Code column
death_pivot <- subset(death_pivot, `ICD10 Code` != "NULL")

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

#load LSOA lookup
LSOALookup <- read.csv("../data/LSOA-lookup.csv")
# join to data
death_pivot <- left_join(death_pivot, LSOALookup, by = "LSOA")

# create age groups #
death_pivot <- death_pivot %>%
  mutate(
    AgeGroup = case_when(
      Age < 16 ~ "0 - 15",
      Age < 31 ~ "16 - 30",
      Age < 46 ~ "31 - 45",
      Age < 61 ~ "46 - 60",
      Age < 76 ~ "61 - 75",
      Age < 91 ~ "76 - 90",
      Age >=91 ~ ">= 91"),
    AgeGroup = factor(AgeGroup, levels = c("0 - 15", "16 - 30", "31 - 45", "46 - 60", "61 - 75", "76 - 90", ">= 91"))
  )

# IMD
death_pivot$IMD_quintile <- as.factor(death_pivot$IMD_quintile)
# Sex
death_pivot$Sex <- factor(death_pivot$Sex, levels = c(1, 2), labels = c("Male", "Female"))

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

#count number of CVD sub groups missing
cvd <- filter(death_pivot, grepl("^I", `icd_subgroup`)) # create data frame with just cvd ICD10 subgroups (i.e, code ICD10 code starts with I)
sum(is.na(cvd$`ICD10 Short Title`)) # 0 missing values

## add clinical groups
ClinicalGroup <- read_excel("../data/ICD10_CVD_ShortTitles.xlsx") %>%
  select(-`ICD10 Short Title`)

cvd <- left_join(cvd, ClinicalGroup, by= c("icd_subgroup" = "Code"))

# load ethnicity coding
EthnicityCoding <- read_excel("../data/NHS-ethnicity-coding.xlsx")

cvd <- left_join(cvd, EthnicityCoding, by = c("Ethnic_Code" = "National code"))
#rename national code definition column to ethnicity
cvd <- cvd %>%
  rename(Ethnicity = `National code definition`)

# replace missing Ethnicity with unknown
cvd <- cvd %>%
 mutate(Ethnicity = ifelse(is.na(Ethnicity), "Unknown", Ethnicity))

# remove cases where clinical subgroup has been coded as 'not cvd'
ClinicalGroup_CVD <- cvd %>%
  filter(`Clinical Subgroup` != 'Not CVD')

# clinical group data frames (taking into account removal of `not cvd` groups)
ClinicalGroup_BSolPrimaryCVD <- ClinicalGroup_CVD %>% # Primary cause of death data frame
  filter(`Cause Number` == "S_UNDERLYING_COD_ICD10")
ClinicalGroup_BSolAllCauseCVD <- ClinicalGroup_CVD %>% select(-one_of('Cause Number', 'ICD10 Code')) # remove cause number column and ICD10 code column
ClinicalGroup_BSolAllCauseCVD <- distinct(ClinicalGroup_BSolAllCauseCVD)# all cause data frame

# Primary cause of death data frame
BSolPrimaryCVD <- cvd %>%
  filter(`Cause Number` == "S_UNDERLYING_COD_ICD10")

# all cause data frame
BSolAllCauseCVD <- cvd %>% select(-one_of('Cause Number', 'ICD10 Code')) # remove cause number column and ICD10 code column
BSolAllCauseCVD <- distinct(BSolAllCauseCVD)

###############################################################################################################################################
                                                              ## Primary Cause Clinical Groupings
##############################################################################################################################################

#####
####### Loop to calculate CVD death count & YLL by Birmingham, Solihull, and BSol Clinical Groupings #################
###########

LA_filters <- list(
  c("Birmingham"),
  c("Solihull"),
  c("Birmingham", "Solihull")
)

outcomes <- c("Deaths", "YLL")

for (outcome in outcomes) {
  for (LA_filter in LA_filters) {

    data_i <- ClinicalGroup_BSolPrimaryCVD %>%
      filter(
        LocalAuthority %in% LA_filter
      ) %>%
      mutate(
        YLL = case_when(
          Age < 75 ~ (74.5 - Age),
          Age >= 75 ~ 0
        )
      ) %>%
      group_by(`Clinical Subgroup`, `Clinical Group`) %>%
      summarise(`Deaths` = n()/10,
                `YLL` = sum(YLL)/10)
    if(length(LA_filter) == 2) {
      area = "BSol"
    } else {
      area = LA_filter[[1]]
    }

    write_xlsx(data_i, paste("../output/Deaths/PrimaryCause/ClinicalGroups/Tables/PrimaryCause_Deaths_YLL_", area, ".xlsx", sep = ""))


    plot_i <- ggplot(data= data_i, aes(x = `Clinical Subgroup`, y = .data[[outcome]])) +
      geom_col(fill="#3488a6") +
      labs( y = paste("Average Yearly", outcome, "(Primary Cause), 2014 to 2023,", area), x = "") +
      coord_flip() +
      theme_bw() +
      geom_text(aes(label = .data[[outcome]]), colour = "black", size = 6, hjust = -0.2) +
      scale_y_continuous(
        expand = c(0, 0), limits = c(0, 1.11*max(data_i[[outcome]]))) +
      facet_col(facets = vars(`Clinical Group`),
                scales = "free_y",
                space = "free") +
      theme(text = element_text(size=22)) #change font size of  text

    ggsave(paste("../output/Deaths/PrimaryCause/ClinicalGroups/PrimaryCause_", outcome, "_", area, ".svg", sep = ""), plot_i, width = 15, height= 12)

  }
}

######### Loop to plot YLL and deaths by sex, imd and locality ########################################

characteristics <- c("Sex", "Locality", 'IMD_quintile', 'Ethnicity')
outcomes <- c("Deaths", "YLL")

legend_titles = list(
  "Sex" = "Sex",
  "IMD_quintile" = "IMD Quintile",
  "Locality" = "Locality",
  "Ethnicity" = "Ethnicity"
)

for(outcome in outcomes) {
  for (characteristic in characteristics) {

    column_name <- sym(characteristic)

    data_i <- ClinicalGroup_BSolPrimaryCVD %>%
      mutate(
        YLL = case_when(
          Age < 75 ~ (74.5 - Age),
          Age >= 75 ~ 0
        )
      ) %>%
      group_by(`Clinical Subgroup`, `Clinical Group`, !!column_name) %>%
      summarise(`Deaths` = n()/10,
                `YLL` = sum(YLL)/10)

    max_value <- data_i %>%
      group_by(`Clinical Subgroup`, `Clinical Group`) %>%
      summarize(total = sum(.data[[outcome]])) %>%
      pull(total) %>%
      max()

    write_xlsx(data_i, paste("../output/Deaths/PrimaryCause/ClinicalGroups/Tables/PrimaryCause_Deaths_YLL_BSol_by_", characteristic, ".xlsx", sep = ""))

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
      scale_fill_viridis(discrete = TRUE, option = "magma", begin = 0.15, end = 0.95) +
      theme(text = element_text(size=20)) #change font size of  text

    ggsave(paste("../output/Deaths/PrimaryCause/ClinicalGroups/PrimaryCause_", outcome, "_BSol_", characteristic, ".png", sep = ""), plot_i, width = 12, height= 12) }

}

#################################### deaths by age #######################

# Average Yearly Total Per ICD10 subgroup
AgeDeathsSubGroup <- ClinicalGroup_BSolPrimaryCVD %>%
  group_by(`Clinical Subgroup`, `Clinical Group`, `AgeGroup`) %>%
  summarise(`Deaths` = n()/10)

write_xlsx(AgeDeathsSubGroup, "../output/Deaths/PrimaryCause/ClinicalGroups/Tables/PrimaryCause_Deaths_BSol_by_Age.xlsx")

#plot
AgeDeathsSubGroupPlot <- ggplot(data= AgeDeathsSubGroup, aes(x = `Clinical Subgroup`, y = Deaths, fill = AgeGroup)) +
  geom_bar(position = "stack", stat= "identity") +
  labs( y = "Average Yearly Deaths (Primary Cause), 2014 to 2023, BSol, by Age", x = "", fill = "Age Group") +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 1250)) +
  facet_col(facets = vars(`Clinical Group`),
            scales = "free_y",
            space = "free") +
  scale_fill_viridis(discrete = TRUE, option = "magma", begin = 0.15, end = 0.95)

AgeDeathsSubGroupPlot

ggsave("../output/Deaths/PrimaryCause/ClinicalGroups/PrimaryCause_Deaths_BSol_Age.png", AgeDeathsSubGroupPlot, width = 10, height= 12)

## Percentage of deaths by clinical group ###################
PercentageDeaths <- ClinicalGroup_BSolPrimaryCVD %>%
  group_by(`Clinical Subgroup`) %>%
  summarise(`Deaths` = n()) %>%
  mutate(TotalDeaths = sum(Deaths),
         PercentageDeaths = Deaths/TotalDeaths *100)

write_xlsx(PercentageDeaths, "../output/Deaths/PrimaryCause/ClinicalGroups/Tables/PrimaryCausePercentageDeaths10Years.xlsx")


###############################################################################################################################################
                                                ## All Cause Clinical Groupings
##############################################################################################################################################

#####
####### Loop to calculate CVD death count & YLL by Birmingham, Solihull, and BSol  #################
###########

LA_filters <- list(
  c("Birmingham"),
  c("Solihull"),
  c("Birmingham", "Solihull")
)

outcomes <- c("Deaths", "YLL")

for (outcome in outcomes) {
  for (LA_filter in LA_filters) {

    data_i <- ClinicalGroup_BSolAllCauseCVD %>%
      filter(
        LocalAuthority %in% LA_filter
      ) %>%
      mutate(
        YLL = case_when(
          Age < 75 ~ (74.5 - Age),
          Age >= 75 ~ 0
        )
      ) %>%
      group_by(`Clinical Subgroup`, `Clinical Group`) %>%
      summarise(`Deaths` = n()/10,
                `YLL` = sum(YLL)/10)
    if(length(LA_filter) == 2) {
      area = "BSol"
    } else {
      area = LA_filter[[1]]
    }

    write_xlsx(data_i, paste("../output/Deaths/AllCause/ClinicalGroups/Tables/AllCause_Deaths_YLL_", area, ".xlsx", sep = ""))


    plot_i <- ggplot(data= data_i, aes(x = `Clinical Subgroup`, y = .data[[outcome]])) +
      geom_col(fill="#3488a6") +
      labs( y = paste("Average Yearly", outcome, "(All Cause), 2014 to 2023,", area), x = "") +
      coord_flip() +
      theme_bw() +
      geom_text(aes(label = .data[[outcome]]), colour = "black", size = 6, hjust = -0.2) +
      scale_y_continuous(
        expand = c(0, 0), limits = c(0, 1.11*max(data_i[[outcome]]))) +
      facet_col(facets = vars(`Clinical Group`),
                scales = "free_y",
                space = "free") +
      theme(text = element_text(size=19.2)) #change font size of  text

    ggsave(paste("../output/Deaths/AllCause/ClinicalGroups/AllCause_", outcome, "_", area, ".svg", sep = ""), plot_i, width = 15, height= 12)

  }
}

######### Loop to plot YLL and deaths by sex, imd and locality ########################################

characteristics <- c("Sex", "Locality", 'IMD_quintile', 'Ethnicity')
outcomes <- c("Deaths", "YLL")

legend_titles = list(
  "Sex" = "Sex",
  "IMD_quintile" = "IMD Quintile",
  "Locality" = "Locality",
  "Ethnicity" = "Ethnicity"
)

for(outcome in outcomes) {
  for (characteristic in characteristics) {

    column_name <- sym(characteristic)

    data_i <- ClinicalGroup_BSolAllCauseCVD %>%
      mutate(
        YLL = case_when(
          Age < 75 ~ (74.5 - Age),
          Age >= 75 ~ 0
        )
      ) %>%
      group_by(`Clinical Subgroup`, `Clinical Group`, !!column_name) %>%
      summarise(`Deaths` = n()/10,
                `YLL` = sum(YLL)/10)

    max_value <- data_i %>%
      group_by(`Clinical Subgroup`, `Clinical Group`) %>%
      summarize(total = sum(.data[[outcome]])) %>%
      pull(total) %>%
      max()

    write_xlsx(data_i, paste("../output/Deaths/AllCause/ClinicalGroups/Tables/AllCause_Deaths_YLL_BSol_by_", characteristic, ".xlsx", sep = ""))

    plot_i <- ggplot(data= data_i, aes(x = `Clinical Subgroup`, y = .data[[outcome]], fill = .data[[characteristic]])) +
      geom_bar(position = "stack", stat= "identity") +
      labs(y = paste("Average Yearly", outcome, "(All Cause), 2014 to 2023, BSol,", "by", legend_titles[[characteristic]]), x = "", fill = legend_titles[[characteristic]]) +
      coord_flip() +
      theme_bw() +
      scale_y_continuous(
        expand = c(0, 0), limits = c(0, 1.1*max_value)) +
      facet_col(facets = vars(`Clinical Group`),
                scales = "free_y",
                space = "free") +
      scale_fill_viridis(discrete = TRUE, option = "magma", begin = 0.15, end = 0.95)

    ggsave(paste("../output/Deaths/AllCause/ClinicalGroups/AllCause_", outcome, "_BSol_", characteristic, ".png", sep = ""), plot_i, width = 10, height= 12) }

}



#################################### deaths by age #######################

# Average Yearly Total
AgeDeathsSubGroup <- ClinicalGroup_BSolAllCauseCVD %>%
  group_by(`Clinical Subgroup`, `Clinical Group`, `AgeGroup`) %>%
  summarise(`Deaths` = n()/10)

write_xlsx(AgeDeathsSubGroup, "../output/Deaths/AllCause/ClinicalGroups/Tables/AllCause_Deaths_BSol_by_Age.xlsx")

#plot
AgeDeathsSubGroupPlot <- ggplot(data= AgeDeathsSubGroup, aes(x = `Clinical Subgroup`, y = Deaths, fill = AgeGroup)) +
  geom_bar(position = "stack", stat= "identity") +
  labs( y = "Average Yearly Deaths (All Cause), 2014 to 2023, BSol, by Age", x = "", fill = "Age Group") +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 2500)) +
  facet_col(facets = vars(`Clinical Group`),
            scales = "free_y",
            space = "free") +
  scale_fill_viridis(discrete = TRUE, option = "magma", begin = 0.15, end = 0.95)

AgeDeathsSubGroupPlot

ggsave("../output/Deaths/AllCause/ClinicalGroups/AllCause_Deaths_BSol_Age.png", AgeDeathsSubGroupPlot, width = 10, height= 12)

## Percentage of deaths by clinical group ###################
PercentageDeaths <- ClinicalGroup_BSolAllCauseCVD %>%
  group_by(`Clinical Subgroup`) %>%
  summarise(`Deaths` = n()) %>%
  mutate(TotalDeaths = sum(Deaths),
         PercentageDeaths = Deaths/TotalDeaths *100)

write_xlsx(PercentageDeaths, "../output/Deaths/AllCause/ClinicalGroups/Tables/AllCausePercentageDeaths10Years.xlsx")



###############################################################################################################################################
                                                  ## Primary Cause ICD10 Groupings
##############################################################################################################################################

##########################
                 ####### Loop to calculate CVD death count & YLL by Birmingham, Solihull, and BSol for ICD10 groups #################
#########################

LA_filters <- list(
  c("Birmingham"),
  c("Solihull"),
  c("Birmingham", "Solihull")
)

outcomes <- c("Deaths", "YLL")

for (outcome in outcomes) {
  for (LA_filter in LA_filters) {

    data_i <- BSolPrimaryCVD %>%
      filter(
        LocalAuthority %in% LA_filter
      ) %>%
      mutate(
        YLL = case_when(
          Age < 75 ~ (74.5 - Age),
          Age >= 75 ~ 0
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

    write_xlsx(data_i, paste("../output/Deaths/PrimaryCause/ICD10Groups/Tables/PrimaryCause_Deaths_YLL_", area, ".xlsx", sep = ""))


    plot_i <- ggplot(data= data_i, aes(x = `ICD10 Short Title`, y = .data[[outcome]])) +
      geom_col(fill="#3488a6") +
      labs( y = paste("Average Yearly", outcome, "(Primary Cause), 2014 to 2023,", area), x = "") +
      coord_flip() +
      theme_bw() +
      geom_text(aes(label = .data[[outcome]]), colour = "black", size = 3, hjust = -0.2) +
      scale_y_continuous(
        expand = c(0, 0), limits = c(0, 1.11*max(data_i[[outcome]]))) +
      facet_col(facets = vars(`CVD_group`),
                scales = "free_y",
                space = "free")

    ggsave(paste("../output/Deaths/PrimaryCause/ICD10Groups/PrimaryCause_", outcome, "_", area, ".png", sep = ""), plot_i, width = 9, height= 12)

  }
}

######### Loop to plot YLL and deaths by sex, imd and locality ########################################

characteristics <- c("Sex", "Locality", 'IMD_quintile', 'Ethnicity')
outcomes <- c("Deaths", "YLL")

legend_titles = list(
  "Sex" = "Sex",
  "IMD_quintile" = "IMD Quintile",
  "Locality" = "Locality",
  "Ethnicity" = "Ethnicity"
)

for(outcome in outcomes) {
  for (characteristic in characteristics) {

    column_name <- sym(characteristic)

    data_i <- BSolPrimaryCVD %>%
      mutate(
        YLL = case_when(
          Age < 75 ~ (74.5 - Age),
          Age >= 75 ~ 0
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

    write_xlsx(data_i, paste("../output/Deaths/PrimaryCause/ICD10Groups/Tables/PrimaryCause_Deaths_YLL_BSol_by_", characteristic, ".xlsx", sep = ""))

    plot_i <- ggplot(data= data_i, aes(x = `ICD10 Short Title`, y = .data[[outcome]], fill = .data[[characteristic]])) +
      geom_bar(position = "stack", stat= "identity") +
      labs(y = paste("Average Yearly", outcome, "(Primary Cause), 2014 to 2023, BSol,", "by", legend_titles[[characteristic]]), x = "", fill = legend_titles[[characteristic]]) +
      coord_flip() +
      theme_bw() +
      scale_y_continuous(
        expand = c(0, 0), limits = c(0, 1.1*max_value)) +
      facet_col(facets = vars(`CVD_group`),
                scales = "free_y",
                space = "free") +
      scale_fill_viridis(discrete = TRUE, option = "magma", begin = 0.15, end = 0.95)

    ggsave(paste("../output/Deaths/PrimaryCause/ICD10Groups/PrimaryCause_", outcome, "_BSol_", characteristic, ".png", sep = ""), plot_i, width = 10, height= 12) }

}

#################################### deaths by age #######################

# Average Yearly Total
AgeDeathsSubGroup <- BSolPrimaryCVD %>%
  group_by(`ICD10 Short Title`, `CVD_group`, `AgeGroup`) %>%
  summarise(`Deaths` = n()/10)

write_xlsx(AgeDeathsSubGroup, "../output/Deaths/PrimaryCause/ICD10Groups/Tables/PrimaryCause_Deaths_BSol_by_Age.xlsx")

#plot
AgeDeathsSubGroupPlot <- ggplot(data= AgeDeathsSubGroup, aes(x = `ICD10 Short Title`, y = Deaths, fill = AgeGroup)) +
  geom_bar(position = "stack", stat= "identity") +
  labs( y = "Average Yearly Deaths (Primary Cause), 2014 to 2023, BSol, by Age", x = "", fill = "Age Group") +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 1250)) +
  facet_col(facets = vars(`CVD_group`),
            scales = "free_y",
            space = "free") +
  scale_fill_viridis(discrete = TRUE, option = "magma", begin = 0.15, end = 0.95)

AgeDeathsSubGroupPlot

ggsave("../output/Deaths/PrimaryCause/ICD10Groups/PrimaryCause_Deaths_BSol_Age.png", AgeDeathsSubGroupPlot, width = 10, height= 12)

## Percentage of deaths by ICD10 group ###################
PercentageDeaths <- BSolPrimaryCVD %>%
  group_by(`CVD_group`) %>%
  summarise(`Deaths` = n()) %>%
  mutate(TotalDeaths = sum(Deaths),
         PercentageDeaths = Deaths/TotalDeaths *100)

write_xlsx(PercentageDeaths, "../output/Deaths/PrimaryCause/ICD10Groups/Tables/PrimaryCausePercentageDeaths10Years.xlsx")


###############################################################################################################################################
                                               ## All Cause ICD10 Groupings
##############################################################################################################################################

#####
####### Loop to calculate CVD death count & YLL by Birmingham, Solihull, and BSol  #################
###########

LA_filters <- list(
  c("Birmingham"),
  c("Solihull"),
  c("Birmingham", "Solihull")
)

outcomes <- c("Deaths", "YLL")

for (outcome in outcomes) {
  for (LA_filter in LA_filters) {

    data_i <- BSolAllCauseCVD %>%
      filter(
        LocalAuthority %in% LA_filter
      ) %>%
      mutate(
        YLL = case_when(
          Age < 75 ~ (74.5 - Age),
          Age >= 75 ~ 0
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

    write_xlsx(data_i, paste("../output/Deaths/AllCause/ICD10Groups/Tables/AllCause_Deaths_YLL_", area, ".xlsx", sep = ""))


    plot_i <- ggplot(data= data_i, aes(x = `ICD10 Short Title`, y = .data[[outcome]])) +
      geom_col(fill="#3488a6") +
      labs( y = paste("Average Yearly", outcome, "(All Cause), 2014 to 2023,", area), x = "") +
      coord_flip() +
      theme_bw() +
      geom_text(aes(label = .data[[outcome]]), colour = "black", size = 3, hjust = -0.2) +
      scale_y_continuous(
        expand = c(0, 0), limits = c(0, 1.11*max(data_i[[outcome]]))) +
      facet_col(facets = vars(`CVD_group`),
                scales = "free_y",
                space = "free")

    ggsave(paste("../output/Deaths/AllCause/ICD10Groups/AllCause_", outcome, "_", area, ".png", sep = ""), plot_i, width = 9, height= 12)

  }
}

######### Loop to plot YLL and deaths by sex, imd and locality ########################################

characteristics <- c("Sex", "Locality", 'IMD_quintile', 'Ethnicity')
outcomes <- c("Deaths", "YLL")

legend_titles = list(
  "Sex" = "Sex",
  "IMD_quintile" = "IMD Quintile",
  "Locality" = "Locality",
  "Ethnicity" = "Ethnicity"
)

for(outcome in outcomes) {
  for (characteristic in characteristics) {

    column_name <- sym(characteristic)

    data_i <- BSolAllCauseCVD %>%
      mutate(
        YLL = case_when(
          Age < 75 ~ (74.5 - Age),
          Age >= 75 ~ 0
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

    write_xlsx(data_i, paste("../output/Deaths/AllCause/ICD10Groups/Tables/AllCause_Deaths_YLL_BSol_by_", characteristic, ".xlsx", sep = ""))

    plot_i <- ggplot(data= data_i, aes(x = `ICD10 Short Title`, y = .data[[outcome]], fill = .data[[characteristic]])) +
      geom_bar(position = "stack", stat= "identity") +
      labs(y = paste("Average Yearly", outcome, "(All Cause), 2014 to 2023, BSol,", "by", legend_titles[[characteristic]]), x = "", fill = legend_titles[[characteristic]]) +
      coord_flip() +
      theme_bw() +
      scale_y_continuous(
        expand = c(0, 0), limits = c(0, 1.1*max_value)) +
      facet_col(facets = vars(`CVD_group`),
                scales = "free_y",
                space = "free") +
      scale_fill_viridis(discrete = TRUE, option = "magma", begin = 0.15, end = 0.95)

    ggsave(paste("../output/Deaths/AllCause/ICD10Groups/AllCause_", outcome, "_BSol_", characteristic, ".png", sep = ""), plot_i, width = 10, height= 12) }

}

#################################### deaths by age #######################

# Average Yearly Total Per ICD10 subgroup
AgeDeathsSubGroup <- BSolAllCauseCVD %>%
  group_by(`ICD10 Short Title`, `CVD_group`, `AgeGroup`) %>%
  summarise(`Deaths` = n()/10)

write_xlsx(AgeDeathsSubGroup, "../output/Deaths/AllCause/ICD10Groups/Tables/AllCause_Deaths_BSol_by_Age.xlsx")

#plot
AgeDeathsSubGroupPlot <- ggplot(data= AgeDeathsSubGroup, aes(x = `ICD10 Short Title`, y = Deaths, fill = AgeGroup)) +
  geom_bar(position = "stack", stat= "identity") +
  labs( y = "Average Yearly Deaths (All Cause), 2014 to 2023, BSol, by Age", x = "", fill = "Age Group") +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 2500)) +
  facet_col(facets = vars(`CVD_group`),
            scales = "free_y",
            space = "free") +
  scale_fill_viridis(discrete = TRUE, option = "magma", begin = 0.15, end = 0.95)

AgeDeathsSubGroupPlot

ggsave("../output/Deaths/AllCause/ICD10Groups/AllCause_Deaths_BSol_Age.png", AgeDeathsSubGroupPlot, width = 10, height= 12)

## Percentage of deaths by ICD10 group ###################
PercentageDeaths <- BSolAllCauseCVD %>%
  group_by(`CVD_group`) %>%
  summarise(`Deaths` = n()) %>%
  mutate(TotalDeaths = sum(Deaths),
         PercentageDeaths = Deaths/TotalDeaths *100)

write_xlsx(PercentageDeaths, "../output/Deaths/AllCause/ICD10Groups/Tables/AllCausePercentageDeaths10Years.xlsx")


