library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(writexl)
library(PHEindicatormethods)
library(viridis)
library(tidytext)

## create European standard population data frame
# Define the age_band vector
age_band <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
              "30-34", "35-39", "40-44","45-49", "50-54", "55-59",
              "60-64", "65-69", "70-74","75-79", "80-84", "85-89", "90+")

# Create the data frame
standard_pop <- data.frame(age_band = age_band, standard_population = esp2013)


## Load hospitalization data
file_path = file.path("//svwvap1126.addm.ads.brm.pri",
                      "PHSensitive$",
                      "Intelligence",
                      "2. Requests",
                      "REQ3289 - BSol ICB CVD Project",
                      "BSol_CVD_inpatients_1819to2324.xlsx",
                      fsep="/")

raw_data <- read_excel(file_path)


## add age bands to hospitalization data ##

# define a function to create age bands to match stdpop
create_age_band <- function(df, age_column,
                            breaks = c(seq(0, 90, by = 5), Inf),
                            labels) {

  copy_df <- data.frame(df)

  # add new column
  copy_df$age_band <- cut(
    copy_df[[age_column]],
    breaks = breaks,
    labels = age_band, # age band vector previouisly defined
    right = FALSE)

  return(copy_df)

}

# apply create_age_band function to hospitalization data
BSol_AgeBand <- create_age_band(raw_data, age_column = "AgeOnAdmission")


######### age standardized rate hospitalization for whole population ########

# load census data with age only (values for small numbers not exact so try to minimise variables)
Age_Census <- read_excel("../data/Bsol_Census_Age.xlsx") %>%
  select(`Age (91 categories) Code`,`Integrated care boards`, `Age (91 categories)`, Observation)

#rename Age (91 categories) Code column to Age_code
Age_Census <- Age_Census %>%
  rename(Age_code = `Age (91 categories) Code`)

# apply function to create new age_band column
Age_Census <- create_age_band(Age_Census, age_column = "Age_code")

# calculate population totals in each age group
Population_Totals_Age <- Age_Census %>%
  group_by(age_band) %>%
  summarise(
    population_count = sum(Observation)
  )

## save data frame for use for death statandisation
write_xlsx(Population_Totals_Age, "../data/Census_Population_Totals_Age.xlsx")

# calculate average yearly hospitalization in each age band
pop_hosp_count_data <- BSol_AgeBand %>%
  group_by(age_band) %>%
  summarise(n = n()/6)

#join to population totals
pop_hosp_count_data <- pop_hosp_count_data %>%
  full_join(Population_Totals_Age, by= "age_band")

# calculate population rate
pop_rate <- pop_hosp_count_data %>%
  # join standard population
  left_join(standard_pop, by = "age_band") %>%
  calculate_dsr(x = n,
                n = population_count,
                stdpop = standard_population,
                type = "full")

################################################

            ###### Age standardized Rate Hospitalization by Ethnicity ###############

#################################################

# load and join ethnicity coding
EthnicityCoding <- read_excel("../data/NHS-ethnicity-coding.xlsx")
BSol_Ethnicity <- left_join(BSol_AgeBand, EthnicityCoding, by = c("Ethnic_Code" = "National code"))

#rename national code definition column to ethnicity
BSol_Ethnicity <- BSol_Ethnicity %>%
  rename(Ethnicity = `National code definition`)

# Remove rows with ethnicity missing
BSol_Ethnicity_AgeBand_Cleaned <- BSol_Ethnicity %>%
  drop_na(Ethnicity)

# Average yearly hospitalizations by Ethnicity and Age band
Ethnicity_age_hosp_count_data <- BSol_Ethnicity_AgeBand_Cleaned %>%
  group_by(Ethnicity, age_band) %>%
  summarise(n = n()/6)


###
## Population totals by Ethnicity and Age band ####
###

# load census data with age and ethnicity
Ethnicity_Age_Census <- read_excel("../data/Bsol_Census_Ethnicity_Age.xlsx") %>%
  select(`Age (91 categories) Code`,`Integrated care boards`, `Age (91 categories)`, `Ethnic group (20 categories)`, Observation)

# define a function to create a new Ethnicity column which changes census ethnicity names to match hospitalization data
create_nhs_ethnicity <-function(df) {
  df %>%
    mutate(
      Ethnicity = case_when(
        `Ethnic group (20 categories)` == "Asian, Asian British or Asian Welsh: Bangladeshi" ~ "Asian or Asian British - Bangladeshi",
        `Ethnic group (20 categories)` == "Asian, Asian British or Asian Welsh: Chinese" ~ "Other Ethnic Groups - Chinese",
        `Ethnic group (20 categories)` == "Asian, Asian British or Asian Welsh: Indian" ~ "Asian or Asian British - Indian",
        `Ethnic group (20 categories)` == "Asian, Asian British or Asian Welsh: Pakistani" ~ "Asian or Asian British - Pakistani",
        `Ethnic group (20 categories)` == "Asian, Asian British or Asian Welsh: Other Asian" ~ "Asian or Asian British - Any other Asian background",
        `Ethnic group (20 categories)` == "Black, Black British, Black Welsh, Caribbean or African: African" ~ "Black or Black British - African",
        `Ethnic group (20 categories)` == "Black, Black British, Black Welsh, Caribbean or African: Caribbean" ~ "Black or Black British - Caribbean",
        `Ethnic group (20 categories)` == "Black, Black British, Black Welsh, Caribbean or African: Other Black" ~ "Black or Black British - Any other Black background",
        `Ethnic group (20 categories)` == "Mixed or Multiple ethnic groups: White and Asian" ~ "Mixed - White and Asian",
        `Ethnic group (20 categories)` == "Mixed or Multiple ethnic groups: White and Black African" ~ "Mixed - White and Black African",
        `Ethnic group (20 categories)` == "Mixed or Multiple ethnic groups: White and Black Caribbean" ~ "Mixed - White and Black Caribbean",
        `Ethnic group (20 categories)` == "Mixed or Multiple ethnic groups: Other Mixed or Multiple ethnic groups" ~ "Mixed - Any other mixed background",
        `Ethnic group (20 categories)` == "White: English, Welsh, Scottish, Northern Irish or British" ~ "White - British",
        `Ethnic group (20 categories)` == "White: Irish" ~ "White - Irish",
        `Ethnic group (20 categories)` == "White: Gypsy or Irish Traveller" ~ "White - Any other White background",
        `Ethnic group (20 categories)` == "White: Roma" ~ "White - Any other White background",
        `Ethnic group (20 categories)` == "White: Other White" ~ "White - Any other White background",
        `Ethnic group (20 categories)` == "Other ethnic group: Arab" ~ "Other Ethnic Groups - Any other ethnic group",
        `Ethnic group (20 categories)` == "Other ethnic group: Any other ethnic group" ~ "Other Ethnic Groups - Any other ethnic group"
      )
    )
}

# apply create_nhs_ethnicity function to census data frame
EthnicityCleaned_Age_Census <- create_nhs_ethnicity(Ethnicity_Age_Census)

# remove NA ethnicity
EthnicityCleaned_Age_Census <- EthnicityCleaned_Age_Census %>%
  drop_na(Ethnicity)

# check census age code is equal to the age
census_check <- EthnicityCleaned_Age_Census %>%
  group_by(`Age (91 categories) Code`, `Age (91 categories)`) %>%
  summarise(n = sum(Observation))

#rename Age (91 categories) Code column to Age_code
EthnicityCleaned_Age_Census <- EthnicityCleaned_Age_Census %>%
  rename(Age_code = `Age (91 categories) Code`)

# apply create_age_band function
Cleaned_Ethnicty_Age_Census <- create_age_band(EthnicityCleaned_Age_Census, age_column = "Age_code")

# calculate population totals in each age and ethnicity group
Population_Totals_Ethnicity_Age <- Cleaned_Ethnicty_Age_Census %>%
  group_by(Ethnicity, age_band) %>%
  summarize(population_count = sum(Observation))

## save data frame for use for death standardization
write_xlsx(Population_Totals_Ethnicity_Age, "../data/Census_Population_Totals_Ethnicity_Age.xlsx")


##### Prep data frame for standardization #

#join hospitalization counts and population totals
Ethnicity_age_hosp_count_data <- Ethnicity_age_hosp_count_data %>%
  full_join(Population_Totals_Ethnicity_Age, by= c("Ethnicity", "age_band"))

# replace na for number of admissions with 0
Ethnicity_age_hosp_count_data <- Ethnicity_age_hosp_count_data %>%
  mutate(n = ifelse(is.na(n), 0, n))

# create broad ethnicity column
Ethnicity_age_hosp_count_data$ethnicity_broad <- sub("-.*", "", Ethnicity_age_hosp_count_data$Ethnicity)
# change ethnicity to not have broad group in the name
Ethnicity_age_hosp_count_data$Ethnicity <- sub(".*?-", "", Ethnicity_age_hosp_count_data$Ethnicity)
# Replace British with White British
Ethnicity_age_hosp_count_data <- Ethnicity_age_hosp_count_data %>%
  mutate(Ethnicity = case_when(
  Ethnicity == " British" ~ " White British",
  TRUE ~ Ethnicity
))

#######################################################

######## age standardized rate of hospitalization by Ethnicity and fill colour by broad ethnicity

standardised_rates <- Ethnicity_age_hosp_count_data %>%
  group_by(
    Ethnicity, ethnicity_broad
  ) %>%
  left_join(standard_pop, by = "age_band") %>%
  calculate_dsr(x = n,
                n = population_count,
                stdpop = standard_population,
                type = "full")

#### pop standardization with unknown ethnicity removed

UnkownRemoved_pop_hosp_count_data <- Ethnicity_age_hosp_count_data %>%
  group_by(age_band) %>%
  summarise(n = sum(n))

#join to population totals
UnkownRemoved_pop_hosp_count_data <- UnkownRemoved_pop_hosp_count_data %>%
  full_join(Population_Totals_Age, by= "age_band")

UnkownRemoved_pop_rate <- UnkownRemoved_pop_hosp_count_data %>%
  left_join(standard_pop, by = "age_band") %>%
  calculate_dsr(x = n,
                n = population_count,
                stdpop = standard_population,
                type = "full")
## not that different from whole population so dont need to present


# plot the rates
ggplot(standardised_rates,
    aes(x = value,
        y = reorder(Ethnicity, value),
        fill = ethnicity_broad)) +
  geom_col() +
  geom_errorbar(aes(xmin = lowercl,
                    xmax = uppercl),
                width = 0.2,
                color = "grey28") +
  geom_vline(xintercept = 1641.44) +
  annotate("text", x=2700, y=1, label="BSol age standardised rate", angle=0, size=3) +
  theme_bw() +
  labs(y = "Ethnicity",
       x = "Age standardised rate per 100,000",
       caption = "European Standard Population 2013
       95% Confidence Intervals
       Census 2021 used for population totals",
       title = "Age standardised average yearly rate of hospitalisations due to CVD,\nPrimary Cause, 2018 to 2024, BSol",
       fill = "") +
  scale_x_continuous(
    expand = c(0, 0), limits = c(0, 8500)) +
  scale_fill_viridis(discrete = TRUE, option = "viridis")

ggsave("../output/Standardisation/Hospitalisation/PrimaryCause/PrimaryCause_AgeStandardisedRate_Hospitalisations_Ethnicity.svg", width = 10, height=6)


##############################################################################################

                                ######################## By Sex and Ethnicity #########

##############################################################################################

## calculate average yearly hospitalizations by Sex and ethnicity

# replace not known sex with na and remove individuals with sex missing
BSol_Sex_Ethnicity_AgeBand_Cleaned <- BSol_Ethnicity_AgeBand_Cleaned %>%
  mutate(Gender = ifelse(Gender == "Not known", NA, Gender)) %>%
  drop_na(Gender)

# average yearly hospitalization by ethnicity, sex and age
Sex_Ethnicity_Age_hosp_count_data <- BSol_Sex_Ethnicity_AgeBand_Cleaned %>%
  group_by(Ethnicity, Gender, age_band) %>%
  summarise(n = n()/6)

## calculate population totals by Sex and Ethnicity
# load census data
Sex_Ethnicity_Age_Census <- read_excel("../data/Bsol_Census_Ethnicity_Sex_Age.xlsx") %>%
  select(`Age (91 categories) Code`,`Integrated care boards`, `Age (91 categories)`, `Ethnic group (20 categories)`, Observation, `Sex (2 categories)`)

# apply function to create NHS ethnicity in census data frame
Sex_EthnicityCleaned_Age_Census <- create_nhs_ethnicity(Sex_Ethnicity_Age_Census) %>%
  drop_na(Ethnicity) # remove NA ethnicity

#rename Age (91 categories) Code column to Age_code
Sex_EthnicityCleaned_Age_Census <- Sex_EthnicityCleaned_Age_Census %>%
  rename(Age_code = `Age (91 categories) Code`)

# apply function to create new age_band column
Cleaned_Sex_Ethnicity_Age_Census <- create_age_band(Sex_EthnicityCleaned_Age_Census, age_column = "Age_code")

# population totals for each group
Population_Totals_Sex_Ethnicity_Age <- Cleaned_Sex_Ethnicity_Age_Census %>%
  group_by(Ethnicity, age_band, `Sex..2.categories.`) %>%
  summarize(population_count = sum(Observation))

# save data frame for use for death statandisation
write_xlsx(Population_Totals_Sex_Ethnicity_Age, "../data/Census_Population_Totals_Sex_Ethnicity_Age.xlsx")

## prepare df for standardisation ##

#join to hospitalization count data frame
Sex_Ethnicity_Age_hosp_count_data <- Sex_Ethnicity_Age_hosp_count_data %>%
  full_join(Population_Totals_Sex_Ethnicity_Age, by= c("Ethnicity", "age_band", "Gender" = "Sex..2.categories."))

# replace na for number of admissions with 0
Sex_Ethnicity_Age_hosp_count_data <- Sex_Ethnicity_Age_hosp_count_data %>%
  mutate(n = ifelse(is.na(n), 0, n))

# create broad ethnicity column
Sex_Ethnicity_Age_hosp_count_data$ethnicity_broad <- sub("-.*", "", Sex_Ethnicity_Age_hosp_count_data$Ethnicity)
# change ethnicity to not have broad group in the name
Sex_Ethnicity_Age_hosp_count_data$Ethnicity <- sub(".*?-", "", Sex_Ethnicity_Age_hosp_count_data$Ethnicity)
# Replace British with White British
Sex_Ethnicity_Age_hosp_count_data <- Sex_Ethnicity_Age_hosp_count_data %>%
  mutate(Ethnicity = case_when(
    Ethnicity == " British" ~ " White British",
    TRUE ~ Ethnicity
  ))

# aggregate mixed ethnicity due to denominator of 0
Sex_Ethnicity_Age_hosp_count_data <- Sex_Ethnicity_Age_hosp_count_data %>%
  mutate(
    Ethnicity = case_when(
      ethnicity_broad == "Mixed "  ~ " Mixed",
      TRUE ~ Ethnicity
    )) %>%
  group_by(Ethnicity, ethnicity_broad, age_band, Gender) %>%
  summarise(n = sum(n),
            population_count = sum(population_count))


### rate calculation

standardised_rates <- Sex_Ethnicity_Age_hosp_count_data %>%
  group_by(
    Ethnicity, ethnicity_broad, Gender
  ) %>%
  left_join(standard_pop, by = "age_band") %>%
  calculate_dsr(x = n,
                n = population_count,
                stdpop = standard_population,
                type = "full")

## plot

ggplot(standardised_rates,
       aes(x = value,
           y = reorder_within(Ethnicity, value, Gender),
           fill = ethnicity_broad)) +
  facet_wrap(~Gender, ncol=1, scales="free_y") +
  geom_col() +
  geom_errorbar(aes(xmin = lowercl,
                    xmax = uppercl),
                width = 0.2,
                color = "grey28") +
  geom_vline(xintercept = 1641.44) +
  annotate("text", x=2240, y=1, label="BSol age standardised rate", angle=0, size=3) +
  theme_bw() +
  labs(x = "Age standardised rate per 100,000",
       y = "Ethnicity",
       caption = "European Standard Population 2013
       95% Confidence Intervals
       Census 2021 used for population totals
       Mixed ethnicites aggregated due to small numbers",
       title = "Age standardised average yearly rate of hospitalisations due to CVD,\nPrimary Cause, 2018 to 2024, BSol",
       fill = "") +
  scale_x_continuous(
    expand = c(0, 0), limits = c(0, 4500)) +
  scale_fill_viridis(discrete = TRUE, option = "viridis") +
  scale_y_reordered() # correct y axis for reordering

ggsave("../output/Standardisation/Hospitalisation/PrimaryCause/PrimaryCause_AgeStandardisedRate_Hospitalisations_Ethnicity_Sex.svg", width = 10, height=6)



###########################################################################

#####################################  IMD  ###########

##################################################################

# load lsoa imd lookup for hospitalization data
Hosp_LSOA_lookup <- read.csv("../data/LSOA-lookup.csv")

# join to hospitalisation data
BSol_IMD_AgeBand <- left_join(BSol_AgeBand, Hosp_LSOA_lookup, by = c('LSOA_2011' = 'LSOA')) #LSOA11 as LSOA21 results in NAs

# hospitalization counts by imd decile and quintile and age group
IMDDecile_Age_hosp_count_data <- BSol_IMD_AgeBand %>%
  group_by(IMD_decile, age_band) %>%
  summarise(n = n()/6)

IMDQuintile_Age_hosp_count_data <- BSol_IMD_AgeBand %>%
  group_by(IMD_quintile, age_band) %>%
  summarise(n = n()/6)

## load census data
BSol_LSOA_Age_Census <- read_excel("../data/BSol_LSOA_Census_Age.xlsx") %>%
  rename(Age_code = `Age (91 categories) Code`,
         LSOA = `Lower layer Super Output Areas Code`)

# apply create_age_band function
Cleaned_BSol_LSOA_Age_Census <- create_age_band(BSol_LSOA_Age_Census, age_column = "Age_code")

# load lsoa imd lookup for census data
Census_LSOA_lookup <- read_excel("../data/WestMids_LSOA21_IMD_lookup_v2.xlsx") %>%
  rename( IMD_decile = IMD_Decile,
          IMD_quintile = IMD_Quintile)

#join
BSol_IMD_Age_Census <- left_join(Cleaned_BSol_LSOA_Age_Census, Census_LSOA_lookup, by = c('LSOA' = 'LSOA21'))

# population totals for each group
Population_Totals_IMDDecile_Age <- BSol_IMD_Age_Census %>%
  group_by(IMD_decile, age_band) %>%
  summarize(population_count = sum(Observation))

Population_Totals_IMDQuntile_Age <- BSol_IMD_Age_Census %>%
  group_by(IMD_quintile, age_band) %>%
  summarize(population_count = sum(Observation))

## save data frames for use for death standardization
write_xlsx(Population_Totals_IMDQuntile_Age, "../data/Census_Population_Totals_IMDQuntile_Age.xlsx")
write_xlsx(Population_Totals_IMDDecile_Age, "../data/Census_Population_Totals_IMDQDecile_Age.xlsx")

## join to hospitalization count data
IMDDecile_Age_hosp_count_data <- IMDDecile_Age_hosp_count_data %>%
  full_join(Population_Totals_IMDDecile_Age, by= c("IMD_decile", "age_band"))

IMDQuintile_Age_hosp_count_data <- IMDQuintile_Age_hosp_count_data %>%
  full_join(Population_Totals_IMDQuntile_Age, by= c("IMD_quintile", "age_band"))

IMDDecile_Age_hosp_count_data$IMD_decile <- factor(IMDDecile_Age_hosp_count_data$IMD_decile, levels = 1:10, ordered = TRUE)
IMDQuintile_Age_hosp_count_data$IMD_quintile <- factor(IMDQuintile_Age_hosp_count_data$IMD_quintile, levels = 1:5, ordered = TRUE)


#### rate

standardised_rates <- IMDDecile_Age_hosp_count_data %>%
  group_by(
   IMD_decile
  ) %>%
  left_join(standard_pop, by = "age_band") %>%
  calculate_dsr(x = n,
                n = population_count,
                stdpop = standard_population,
                type = "full")


## plot

ggplot(standardised_rates,
       aes(x = value,
           y = IMD_decile,
           fill = IMD_decile)) +
  geom_col() +
  geom_errorbar(aes(xmin = lowercl,
                    xmax = uppercl),
                width = 0.2,
                color = "grey28") +
  geom_vline(xintercept = 1641.44) +
  annotate("text", x=1830, y=10, label="BSol age standardised rate", angle=0, size=3) +
  theme_bw() +
  labs(x = "Age standardised rate per 100,000",
       y = "IMD Decile",
       caption = "European Standard Population 2013
       95% Confidence Intervals
       Census 2021 used for population totals",
       title = "Age standardised average yearly rate of hospitalisations due to CVD,\nPrimary Cause, 2018 to 2024, BSol") +
  scale_x_continuous(
    expand = c(0, 0), limits = c(0, 2250)) +
  scale_fill_viridis(discrete = TRUE, option = "viridis") +
  theme(legend.position="none")

ggsave("../output/Standardisation/Hospitalisation/PrimaryCause/PrimaryCause_AgeStandardisedRate_Hospitalisations_IMDDecile.svg", width = 10, height=6)

#### rate

standardised_rates <- IMDQuintile_Age_hosp_count_data %>%
  group_by(
    IMD_quintile
  ) %>%
  left_join(standard_pop, by = "age_band") %>%
  calculate_dsr(x = n,
                n = population_count,
                stdpop = standard_population,
                type = "full")


## plot

ggplot(standardised_rates,
       aes(x = value,
           y = IMD_quintile,
           fill = IMD_quintile)) +
  geom_col() +
  geom_errorbar(aes(xmin = lowercl,
                    xmax = uppercl),
                width = 0.2,
                color = "grey28") +
  geom_vline(xintercept = 1641.44) +
  annotate("text", x=1830, y=5, label="BSol age standardised rate", angle=0, size=3) +
  theme_bw() +
  labs(x = "Age standardised rate per 100,000",
       y = "IMD Decile",
       caption = "European Standard Population 2013
       95% Confidence Intervals
       Census 2021 used for population totals",
       title = "Age standardised average yearly rate of hospitalisations due to CVD,\nPrimary Cause, 2018 to 2024, BSol",
       fill = "") +
  scale_x_continuous(
    expand = c(0, 0), limits = c(0, 2250)) +
  scale_fill_viridis(discrete = TRUE, option = "viridis") +
  theme(legend.position="none")

ggsave("../output/Standardisation/Hospitalisation/PrimaryCause/PrimaryCause_AgeStandardisedRate_Hospitalisations_IMDquintile.svg", width = 10, height=6)
