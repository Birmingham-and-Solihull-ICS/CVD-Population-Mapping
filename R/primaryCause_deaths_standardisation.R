library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
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

## Load Birmingham and Solihull deaths data
file_path = file.path("//svwvap1126.addm.ads.brm.pri",
                      "PHSensitive$",
                      "Intelligence",
                      "2. Requests",
                      "REQ3289 - BSol ICB CVD Project",
                      "BSol_CVD_Deaths_with_Ethnicity_2014to2023.xlsx",
                      fsep="/")

BSolPrimaryCause <- read_excel(file_path) %>%
  select(Sex, Age, S_UNDERLYING_COD_ICD10, LSOA, LA_Code, PatientId, Ethnic_Code)

# filter so primary cause is cvd ICD10 subgroups (i.e, code ICD10 code starts with I)
BSolPrimaryCVD <- filter(BSolPrimaryCause, grepl("^I", `S_UNDERLYING_COD_ICD10`))

# load ethnicity coding
EthnicityCoding <- read_excel("../data/NHS-ethnicity-coding.xlsx")
#join
BSolPrimaryCVD <- left_join(BSolPrimaryCVD, EthnicityCoding, by = c("Ethnic_Code" = "National code"))

#rename national code definition column to ethnicity
BSolPrimaryCVD <- BSolPrimaryCVD %>%
  rename(Ethnicity = `National code definition`)

# define a function to create age bands to match stdpop
create_age_band <- function(df, age_column,
                            breaks = c(seq(0, 90, by = 5), Inf),
                            labels) {

  copy_df <- data.frame(df)

  # add new column
  copy_df$age_band <- cut(
    copy_df[[age_column]],
    breaks = breaks,
    labels = age_band, # age band vector previously defined
    right = FALSE)

  return(copy_df)

}

# apply function to create new age_band column
BSolPrimaryCVD <- create_age_band(BSolPrimaryCVD, age_column = "Age")

#################### age standardized rate for whole population ###########

# load census population totals by age group - calculated from census in hospitalisation script
Population_Totals_Age <- read_excel("../data/Census_Population_Totals_Age.xlsx")

# average deaths in each age band
pop_deaths_count_data <- BSolPrimaryCVD %>%
  group_by(age_band) %>%
  summarise(Deaths = n()/10)
#join to population totals
pop_deaths_count_data <- pop_deaths_count_data %>%
  full_join(Population_Totals_Age, by= "age_band")

# calculate population rate
pop_rate <- pop_deaths_count_data %>%
  # join standard population
  left_join(standard_pop, by = "age_band") %>%
  calculate_dsr(x = Deaths,
                n = population_count,
                stdpop = standard_population,
                type = "full")


#######################################

############# Ethnicity  ######

################################

# Remove those whose ethnicity missing
BSolPrimaryCVDUnkownEthnicityRemoved <- BSolPrimaryCVD %>%
  drop_na(Ethnicity)

### average yearly deaths by Ethnicity and age
Ethnicity_Age_PrimaryDeathCount <- BSolPrimaryCVDUnkownEthnicityRemoved %>%
  group_by(Ethnicity, age_band) %>%
  summarise(Deaths = n()/10)

## load population totals calculated for hospitalisation standardisation
Population_Totals_Ethnicity_Age <- read_excel("../data/Census_Population_Totals_Ethnicity_Age.xlsx")

#join
Ethnicity_Age_PrimaryDeathCount <- Ethnicity_Age_PrimaryDeathCount %>%
  full_join(Population_Totals_Ethnicity_Age, by= c("Ethnicity", "age_band"))

# replace na for number of deaths with 0
Ethnicity_Age_PrimaryDeathCount <- Ethnicity_Age_PrimaryDeathCount %>%
  mutate(Deaths = ifelse(is.na(Deaths), 0, Deaths))

# create broad ethnicity column
Ethnicity_Age_PrimaryDeathCount$ethnicity_broad <- sub("-.*", "", Ethnicity_Age_PrimaryDeathCount$Ethnicity)

# change ethnicity to not have broad group in the name
Ethnicity_Age_PrimaryDeathCount$Ethnicity <- sub(".*?-", "", Ethnicity_Age_PrimaryDeathCount$Ethnicity)

# Replace British with White British
Ethnicity_Age_PrimaryDeathCount <- Ethnicity_Age_PrimaryDeathCount %>%
  mutate(Ethnicity = case_when(
  Ethnicity == " British" ~ " White British",
  TRUE ~ Ethnicity
))

# aggregate mixed and other due to small numbers
Ethnicity_Age_PrimaryDeathCount <- Ethnicity_Age_PrimaryDeathCount %>%
  mutate(
    Ethnicity = case_when(
      ethnicity_broad == "Mixed "  ~ " Mixed",
      ethnicity_broad == "Other Ethnic Groups " ~ " Other Ethnic Groups",
      TRUE ~ Ethnicity
    )) %>%
  group_by(Ethnicity, age_band, ethnicity_broad) %>%
  summarise(Deaths = sum(Deaths),
            population_count = sum(population_count))


######## age standardized rate of deaths by Ethnicity and fill colour by broad ethnicity

standardised_rates <- Ethnicity_Age_PrimaryDeathCount %>%
  group_by(
    Ethnicity, ethnicity_broad
  ) %>%
  left_join(standard_pop, by = "age_band") %>%
  calculate_dsr(x = Deaths,
                n = population_count,
                stdpop = standard_population,
                type = "full")


## age standardized rate for population excluding unknown ethnicity

pop_deaths_count_data_UnkownRemoved <- Ethnicity_Age_PrimaryDeathCount %>%
  group_by(age_band) %>%
  summarise(Deaths = sum(Deaths))

pop_deaths_count_data_UnkownRemoved <- pop_deaths_count_data_UnkownRemoved %>%
  full_join(Population_Totals_Age, by= "age_band")

pop_rate_unkownRemoved <- pop_deaths_count_data_UnkownRemoved %>%
  left_join(standard_pop, by = "age_band") %>%
  calculate_dsr(x = Deaths,
                n = population_count,
                stdpop = standard_population,
                type = "full")

## plot the rates
ggplot(standardised_rates,
    aes(x = value,
        y = reorder(Ethnicity, value),
        fill = ethnicity_broad)) +
  geom_col() +
  geom_errorbar(aes(xmin = lowercl,
                    xmax = uppercl),
                width = 0.2,
                color = "grey28") +
  geom_vline(aes(xintercept = 202.3733, linetype = "Unknown ethnicity removed")) +
  geom_vline(aes(xintercept = 258.3754, linetype = "All population")) +
  theme_bw() +
  labs(y = "Ethnicity",
       x = "Age standardised rate per 100,000",
       caption = "European Standard Population 2013
       Mixed and Other Ethnic Groups aggregated due to small numbers
       95% Confidence Intervals
       Census 2021 used for population totals",
       title = "Age standardised average yearly rate of deaths due to CVD,\nPrimary Cause, 2014 to 2024, BSol",
       fill = "",
       linetype = "BSol age standardised rate") +
  scale_x_continuous(
    expand = c(0, 0), limits = c(0, 500)) +
  scale_fill_viridis(begin = 0, end = 0.95, discrete = TRUE, option = "plasma") +
  scale_linetype_manual(values = c("All population" = "dotted", "Unknown ethnicity removed" = "solid"))

ggsave("../output/Standardisation/Deaths/PrimaryCause/PrimaryCause_AgeStandardisedRate_Deaths_Ethnicity.svg", width = 10, height=6)



##############################################################################################

######################## By Sex and Ethnicity #########

##############################################################################################

# replace Sex as Male and Female
BSolPrimaryCVD_Sex_Ethnicity <- BSolPrimaryCVDUnkownEthnicityRemoved %>%
  mutate(
    Sex = case_when(
      Sex == 1 ~ "Male",
      TRUE ~ "Female"
    )
  )

### average yearly death by ethnicity, sex and age
Sex_Ethnicity_Age_PrimaryDeathCount <- BSolPrimaryCVD_Sex_Ethnicity %>%
  group_by(Ethnicity, Sex, age_band) %>%
  summarise(Deaths = n()/10)

## load population totals calculated for hospitalisation standardisation
Population_Totals_Sex_Ethnicity_Age <- read_excel("../data/Census_Population_Totals_Sex_Ethnicity_Age.xlsx")

#join
Sex_Ethnicity_Age_PrimaryDeathCount <- Sex_Ethnicity_Age_PrimaryDeathCount %>%
  full_join(Population_Totals_Sex_Ethnicity_Age, by= c("Ethnicity", "age_band", "Sex" = "Sex..2.categories."))


# replace na for number of deaths with 0
Sex_Ethnicity_Age_PrimaryDeathCount <- Sex_Ethnicity_Age_PrimaryDeathCount %>%
  mutate(Deaths = ifelse(is.na(Deaths), 0, Deaths))

# create broad ethnicity column
Sex_Ethnicity_Age_PrimaryDeathCount$ethnicity_broad <- sub("-.*", "", Sex_Ethnicity_Age_PrimaryDeathCount$Ethnicity)

# change ethnicity to not have broad group in the name
Sex_Ethnicity_Age_PrimaryDeathCount$Ethnicity <- sub(".*?-", "", Sex_Ethnicity_Age_PrimaryDeathCount$Ethnicity)

## small numbers mean have to use broad ethnicity
Sex_GroupedEthnicity_Age_PrimaryDeathCount <- Sex_Ethnicity_Age_PrimaryDeathCount %>%
  group_by(ethnicity_broad, age_band, Sex) %>%
  summarise(Deaths = sum(Deaths),
            population_count = sum(population_count))


### rate
standardised_rates <- Sex_GroupedEthnicity_Age_PrimaryDeathCount %>%
  group_by(
    ethnicity_broad, Sex
  ) %>%
  left_join(standard_pop, by = "age_band") %>%
  calculate_dsr(x = Deaths,
                n = population_count,
                stdpop = standard_population,
                type = "full")
#### plot

ggplot(standardised_rates,
       aes(x = value,
           y = reorder_within(ethnicity_broad, value, Sex),
           fill = ethnicity_broad)) +
  facet_wrap(~Sex, ncol=1, scales="free_y") +
  geom_col() +
  geom_errorbar(aes(xmin = lowercl,
                    xmax = uppercl),
                width = 0.2,
                color = "grey28") +
  geom_vline(aes(xintercept = 202.3733, linetype = "Unknown ethnicity removed")) +
  geom_vline(aes(xintercept = 258.3754, linetype = "All population")) +
  theme_bw() +
  labs(x = "Age standardised rate per 100,000",
       y = "Ethnicity",
       caption = "European Standard Population 2013
       Mixed Ethnicity rate not presented as dsr NA for total count < 10
       95% Confidence Intervals
       Census 2021 used for population totals
       Ethnicites aggregated due to small numbers",
       title = "Age standardised average yearly rate of deaths due to CVD,\nPrimary Cause, 2014 to 2023, BSol",
       fill = "",
       linetype = "BSol age standardised rate") +
  scale_x_continuous(
    expand = c(0, 0), limits = c(0, 350)) +
  scale_fill_viridis(begin = 0, end = 0.95, discrete = TRUE, option = "plasma") +
  scale_y_reordered() + # correct y axis for reordering
  scale_linetype_manual(values = c("All population" = "dotted", "Unknown ethnicity removed" = "solid"))

ggsave("../output/Standardisation/Deaths/PrimaryCause/PrimaryCause_AgeStandardisedRate_Deaths_Ethnicity_Sex.svg", width = 10, height=6)


#########################################

############ IMD #####################

####################################

# load LSOA IMD lookup for Death data
DeathsLSOALookup <- read.csv("../data/LSOA-lookup.csv")

# join to data
BSol_IMD_AgeBand <- left_join(BSolPrimaryCVD, DeathsLSOALookup, by = "LSOA")

### average yearly death by IMD and age
IMDDecile_Age_PrimaryDeathCount <- BSol_IMD_AgeBand %>%
  group_by(IMD_decile, age_band) %>%
  summarise(Deaths = n()/10)

IMDQuintile_Age_PrimaryDeathCount <- BSol_IMD_AgeBand %>%
  group_by(IMD_quintile, age_band) %>%
  summarise(Deaths = n()/10)

## load population totals calculated for hospitalization standardization
Population_Totals_IMDDecile_Age <- read_excel("../data/Census_Population_Totals_IMDQDecile_Age.xlsx")
Population_Totals_IMDQuntile_Age <- read_excel("../data/Census_Population_Totals_IMDQuntile_Age.xlsx")


#join
IMDDecile_Age_PrimaryDeathCount <- IMDDecile_Age_PrimaryDeathCount %>%
  full_join(Population_Totals_IMDDecile_Age, by= c("IMD_decile", "age_band"))

IMDQuintile_Age_PrimaryDeathCount <- IMDQuintile_Age_PrimaryDeathCount %>%
  full_join(Population_Totals_IMDQuntile_Age, by= c("IMD_quintile", "age_band"))


# replace na for number of deaths with 0
IMDDecile_Age_PrimaryDeathCount <- IMDDecile_Age_PrimaryDeathCount %>%
  mutate(Deaths = ifelse(is.na(Deaths), 0, Deaths))

IMDQuintile_Age_PrimaryDeathCount <- IMDQuintile_Age_PrimaryDeathCount %>%
  mutate(Deaths = ifelse(is.na(Deaths), 0, Deaths))


IMDDecile_Age_PrimaryDeathCount$IMD_decile <- factor(IMDDecile_Age_PrimaryDeathCount$IMD_decile, levels = 1:10, ordered = TRUE)
IMDQuintile_Age_PrimaryDeathCount$IMD_quintile <- factor(IMDQuintile_Age_PrimaryDeathCount$IMD_quintile, levels = 1:5, ordered = TRUE)


### rate by IMD Decile
standardised_rates <- IMDDecile_Age_PrimaryDeathCount %>%
  group_by(
    IMD_decile
  ) %>%
  left_join(standard_pop, by = "age_band") %>%
  calculate_dsr(x = Deaths,
                n = population_count,
                stdpop = standard_population,
                type = "full")
# plot

ggplot(standardised_rates,
       aes(x = value,
           y = IMD_decile,
           fill = IMD_decile)) +
  geom_col() +
  geom_errorbar(aes(xmin = lowercl,
                    xmax = uppercl),
                width = 0.2,
                color = "grey28") +
  geom_vline(xintercept = 258.3754) +
  annotate("text", x=290, y=10, label="BSol age standardised rate", angle=0, size=3) +
  theme_bw() +
  labs(x = "Age standardised rate per 100,000",
       y = "IMD Decile",
       caption = "European Standard Population 2013
       95% Confidence Intervals
       Census 2021 used for population totals",
       title = "Age standardised average yearly rate of deaths due to CVD,\nPrimary Cause, 2014 to 2023, BSol") +
  scale_x_continuous(
    expand = c(0, 0), limits = c(0, 350)) +
  scale_fill_viridis(begin = 0, end = 0.95, discrete = TRUE, option = "plasma") +
  theme(legend.position="none")


ggsave("../output/Standardisation/Deaths/PrimaryCause/PrimaryCause_AgeStandardisedRate_Deaths_IMDdecile.svg", width = 10, height=6)


# rate by IMD Quntile
standardised_rates <- IMDQuintile_Age_PrimaryDeathCount %>%
  group_by(
    IMD_quintile
  ) %>%
  left_join(standard_pop, by = "age_band") %>%
  calculate_dsr(x = Deaths,
                n = population_count,
                stdpop = standard_population,
                type = "full")


# plot

ggplot(standardised_rates,
       aes(x = value,
           y = IMD_quintile,
           fill = IMD_quintile)) +
  geom_col() +
  geom_errorbar(aes(xmin = lowercl,
                    xmax = uppercl),
                width = 0.2,
                color = "grey28") +
  geom_vline(xintercept = 258.3754) +
  annotate("text", x=290, y=5, label="BSol age standardised rate", angle=0, size=3) +
  theme_bw() +
  labs(x = "Age standardised rate per 100,000",
       y = "IMD Quintile",
       caption = "European Standard Population 2013
       95% Confidence Intervals
       Census 2021 used for population totals",
       title = "Age standardised average yearly rate of deaths due to CVD,\nPrimary Cause, 2014 to 2023, BSol") +
  scale_x_continuous(
    expand = c(0, 0), limits = c(0, 350)) +
  scale_fill_viridis(begin = 0, end = 0.95, discrete = TRUE, option = "plasma") +
  theme(legend.position="none")


ggsave("../output/Standardisation/Deaths/PrimaryCause/PrimaryCause_AgeStandardisedRate_Deaths_IMDquintile.svg", width = 10, height=6)

