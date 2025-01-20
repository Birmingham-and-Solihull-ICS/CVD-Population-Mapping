library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(writexl)
library(ggforce)
library(viridis)

# load BSol hospitalization data

file_path = file.path("//svwvap1126.addm.ads.brm.pri",
                      "PHSensitive$",
                      "Intelligence",
                      "2. Requests",
                      "REQ3289 - BSol ICB CVD Project",
                      "BSol_CVD_inpatients_1819to2324.xlsx",
                      fsep="/")

BSol <- read_excel(file_path)

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

BSol$Ethnicity <- as.factor(BSol$Ethnicity)

## Admission Group column
# load lookup
AdmissionMethodLookUp <- read_excel("../data/AdmissionMethodLookUp.xlsx")

BSol <- left_join(BSol, AdmissionMethodLookUp, by= "AdmissionMethodDescription")

admissionsCheck <- BSol %>%
  group_by(AdmissionMethodDescription, AdmissionGroup) %>%
  summarise(n= n())


### Emergency admission logical
BSol <- BSol %>%
  mutate(
    Emergency = AdmissionGroup == "Emergency"
    )

LogicalCheck <- BSol %>%
  group_by(Emergency) %>%
  summarise(n= n())

########################## Loop to calculate percentage of admissions considered emergency by characteristics ##############

characteristics <- c('Locality', 'IMD_quintile', 'AgeGroup', 'Gender', 'Ethnicity', 'GP_Code')

for(characteristic in characteristics) {

  result <- BSol %>%
    group_by(across(all_of(characteristic))) %>%
    summarise(
      total_admissions = n(),
      emergency_admissions = sum(Emergency == TRUE, na.rm = TRUE),
      emergency_percentage = (emergency_admissions / total_admissions) * 100
    ) %>%
    mutate(
      AreaCode = "E38000258",
      p_hat = emergency_admissions / total_admissions,
      Value = 100 * p_hat,
      # Calculate errors
      Z = qnorm(0.975),
      LowerCI95 = 100 * (p_hat + Z^2/(2*total_admissions) - Z * sqrt((p_hat*(1-p_hat)/total_admissions) + Z^2/(4*total_admissions^2))) / (1 + Z^2/total_admissions),
      UpperCI95 = 100 * (p_hat + Z^2/(2*total_admissions) + Z * sqrt((p_hat*(1-p_hat)/total_admissions) + Z^2/(4*total_admissions^2))) / (1 + Z^2/total_admissions)
      )


  # Save the result as a separate data frame using assign()
  assign(paste0("result_", characteristic), result)

}

############### plot results ####################

######## Sex #######

Gender <- ggplot(
  result_Gender,
  aes(x = Gender,
      y = emergency_percentage)
) +
  geom_col(fill = "#3488a6") +
  geom_errorbar(aes(ymin = LowerCI95,
                    ymax = UpperCI95),
                width = 0.1) +
  labs(title ="Percentage of Total CVD Hospital Admissions Considered as Emergency,\n Primary Cause, BSol (2018 to 2024)" ,
       x = "Sex",
       y = "Percentage",
       caption = "95% Confidence Intervals") +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 100)) +
  theme_bw() +
  theme(text = element_text(size=16))

Gender

ggsave("../output/Hospitalisations/PrimaryCause/EmergencyPercentage/PrimaryCause_PercentageEmergency_Gender.svg", Gender, width = 10, height= 10)

####### IMD ############

IMD <- ggplot(
  result_IMD_quintile,
  aes(x = IMD_quintile,
      y = emergency_percentage)) +
  geom_col(fill="#3488a6") +
  geom_errorbar(aes(ymin=LowerCI95,
                    ymax=UpperCI95),
                width=0.1) +
  labs(title ="Percentage of Total CVD Hospital Admissions Considered as Emergency,\n Primary Cause, BSol (2018 to 2024)",
       x = "IMD Quintile",
       y = "Percentage",
       caption = "95% Confidence Intervals") +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 100)) +
  theme_bw()

IMD

ggsave("../output/Hospitalisations/PrimaryCause/EmergencyPercentage/PrimaryCause_PercentageEmergency_IMD2.svg", IMD, width = 7, height= 7)


######### Locality ################

Locality <- ggplot(
  result_Locality,
  aes(x = Locality,
      y = emergency_percentage)) +
  geom_col(fill="#3488a6") +
  geom_errorbar(aes(ymin=LowerCI95,
                    ymax=UpperCI95),
                width= 0.1) +
  labs(title ="Percentage of Total CVD Hospital Admissions Considered as Emergency,\n Primary Cause, BSol (2018 to 2024)" ,
       x = "Locality",
       y = "Percentage",
       caption = "95% Confidence Intervals") +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 100)) +
  theme_bw()

ggsave("../output/Hospitalisations/PrimaryCause/EmergencyPercentage/PrimaryCause_PercentageEmergency_Locality.svg", Locality, width = 7, height= 7)

Locality

############# Age group ############

AgeGroup <- ggplot(
  result_AgeGroup,
  aes(x = AgeGroup,
      y = emergency_percentage)) +
  geom_col(fill="#3488a6") +
  geom_errorbar(aes(ymin=LowerCI95,
                    ymax=UpperCI95),
                width=0.1) +
  labs(title ="Percentage of Total CVD Hospital Admissions Considered as Emergency,\n Primary Cause, BSol (2018 to 2024)",
       x = "Age Group",
       y = "Percentage",
       caption = "95% Confidence Intervals") +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 100))+
  theme_bw() +
  theme(
    plot.title.position = "plot", # Make the title span the full plot width including axis labels
    plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=14.5))

ggsave("../output/Hospitalisations/PrimaryCause/EmergencyPercentage/PrimaryCause_PercentageEmergency_Age.svg", AgeGroup, width = 8, height= 7)

AgeGroup

##### Ethnicity ##############

result_Ethnicity$Ethnicity <- str_wrap(result_Ethnicity$Ethnicity, width = 35)

Ethnicity <-  ggplot(
  result_Ethnicity,
  aes(x = reorder(Ethnicity, emergency_percentage),
      y = emergency_percentage)) +
  geom_col(fill="#3488a6") +
  geom_errorbar(aes(ymin=LowerCI95,
                    ymax=UpperCI95),
                width= 0.1) +
  labs(title ="Percentage of Total CVD Hospital Admissions Considered as Emergency,\n Primary Cause, BSol (2018 to 2024)" ,
       x = "Ethnicity",
       y = "Percentage",
       caption = "95% Confidence Intervals") +
  theme_bw() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 100)) +
  geom_hline(yintercept = mean(result_Ethnicity$emergency_percentage, na.rm=TRUE)) +
  annotate("text", x=9, y=76.5, label="Mean", size = 4) +
  coord_flip() +
  theme(
    plot.title.position = "plot", # Make the title span the full plot width including axis labels
    plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=15)) +
  theme(plot.margin = margin(t = 20,  # Top margin
                             r = 50,  # Right margin
                             b = 40,  # Bottom margin
                             l = 10))

Ethnicity

ggsave("../output/Hospitalisations/PrimaryCause/EmergencyPercentage/PrimaryCause_PercentageEmergency_Ethnicity.svg", Ethnicity, width = 10, height= 8)


############  Ethnicity plot split by age - under and over 75 #############################

## Create age less than 75 column
BSol <- BSol %>%
  mutate(
    Age2Cat = case_when(
      AgeOnAdmission < 75 ~ "Under 75",
      TRUE ~ "75 and over"))

# values for plotting
EthnicityAge <- BSol %>%
  group_by(Ethnicity, Age2Cat) %>%
  summarise(
    total_admissions = n(),
    emergency_admissions = sum(Emergency == TRUE, na.rm = TRUE),
    emergency_percentage = (emergency_admissions / total_admissions) * 100
  ) %>%
  mutate(
    AreaCode = "E38000258",
    p_hat = emergency_admissions / total_admissions,
    Value = 100 * p_hat,
    # Calculate errors
    Z = qnorm(0.975),
    LowerCI95 = 100 * (p_hat + Z^2/(2*total_admissions) - Z * sqrt((p_hat*(1-p_hat)/total_admissions) + Z^2/(4*total_admissions^2))) / (1 + Z^2/total_admissions),
    UpperCI95 = 100 * (p_hat + Z^2/(2*total_admissions) + Z * sqrt((p_hat*(1-p_hat)/total_admissions) + Z^2/(4*total_admissions^2))) / (1 + Z^2/total_admissions)
  ) %>%
  group_by(Age2Cat) %>%
  mutate(
    AgeGroupMean = mean(emergency_percentage)
  )

EthnicityAge$Ethnicity <- str_wrap(EthnicityAge$Ethnicity, width = 30)

EthnicityAgePlot <-  ggplot(
  EthnicityAge,
  aes(x = Ethnicity,
      y = emergency_percentage)) +
  geom_col(fill="#3488a6") +
  facet_wrap(~Age2Cat, ncol =1) +
  geom_errorbar(aes(ymin=LowerCI95,
                    ymax=UpperCI95),
                width=0.1) +
  labs(title ="Percentage of Total CVD Hospital Admissions Considered as Emergency,\n Primary Cause, BSol (2018 to 2024)" ,
       x = "Ethnicity",
       y = "Percentage",
       caption = "95% Confidence Intervals",
       linetype = "Mean Percentage") +
  theme_bw() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 100)) +
  geom_hline(aes(yintercept = AgeGroupMean, linetype = Age2Cat),
             data = EthnicityAge) +
  coord_flip() +
  theme(
    plot.title.position = "plot", # Make the title span the full plot width including axis labels
    plot.title = element_text(hjust = 0.5)) # Center-align the title

EthnicityAgePlot

ggsave("../output/Hospitalisations/PrimaryCause/EmergencyPercentage/PrimaryCause_PercentageEmergency_EthnicityAge.svg", EthnicityAgePlot, width = 12, height= 12)



####### IMD and Sex ##########

# values for plotting
IMDGender <- BSol %>%
  group_by(IMD_quintile, Gender) %>%
  summarise(
    total_admissions = n(),
    emergency_admissions = sum(Emergency == TRUE, na.rm = TRUE),
    emergency_percentage = (emergency_admissions / total_admissions) * 100
  ) %>%
  mutate(
    p_hat = emergency_admissions / total_admissions,
    Value = 100 * p_hat,
    # Calculate errors
    Z = qnorm(0.975),
    LowerCI95 = 100 * (p_hat + Z^2/(2*total_admissions) - Z * sqrt((p_hat*(1-p_hat)/total_admissions) + Z^2/(4*total_admissions^2))) / (1 + Z^2/total_admissions),
    UpperCI95 = 100 * (p_hat + Z^2/(2*total_admissions) + Z * sqrt((p_hat*(1-p_hat)/total_admissions) + Z^2/(4*total_admissions^2))) / (1 + Z^2/total_admissions)
  ) %>%
  filter(Gender != "Not known") %>%
  group_by(Gender) %>%
  mutate(
    SexGroupMean = mean(emergency_percentage)
  )

IMDGenderPlot <-  ggplot(
  IMDGender,
  aes(x = IMD_quintile,
      y = emergency_percentage)) +
  geom_col(fill="#3488a6") +
  facet_wrap(~Gender, ncol =1) +
  geom_errorbar(aes(ymin=LowerCI95,
                    ymax=UpperCI95),
                width=0.1) +
  labs(title ="Percentage of Total CVD Hospital Admissions Considered as Emergency,\n Primary Cause, BSol (2018 to 2024)" ,
       x = "IMD Quntile",
       y = "Percentage",
       caption = "95% Confidence Intervals\n
       exluding when gender not known due to small numbers",
       linetype = "Mean Percentage") +
  theme_bw() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 100)) +
  geom_hline(aes(yintercept = SexGroupMean, linetype = Gender),
             data = IMDGender) +
  coord_flip() +
  theme(
    plot.title.position = "plot", # Make the title span the full plot width including axis labels
    plot.title = element_text(hjust = 0.5)) # Center-align the title +
theme(text = element_text(size=14))

IMDGenderPlot


ggsave("../output/Hospitalisations/PrimaryCause/EmergencyPercentage/PrimaryCause_PercentageEmergency_IMDGender.svg", IMDGenderPlot, width = 9, height= 7)


########################### IMD and Ethnicity ################

# create broad ethnicity column
BSol$ethnicity_broad <- sub("-.*", "", BSol$Ethnicity)

# create data for ploting
IMDEthnicity <- BSol %>%
  group_by(IMD_decile, ethnicity_broad) %>%
  summarise(
    total_admissions = n(),
    emergency_admissions = sum(Emergency == TRUE, na.rm = TRUE),
    emergency_percentage = (emergency_admissions / total_admissions) * 100
  ) %>%
  mutate(
    p_hat = emergency_admissions / total_admissions,
    # Calculate errors
    Z = qnorm(0.975),
    LowerCI95 = 100 * (p_hat + Z^2/(2*total_admissions) - Z * sqrt((p_hat*(1-p_hat)/total_admissions) + Z^2/(4*total_admissions^2))) / (1 + Z^2/total_admissions),
    UpperCI95 = 100 * (p_hat + Z^2/(2*total_admissions) + Z * sqrt((p_hat*(1-p_hat)/total_admissions) + Z^2/(4*total_admissions^2))) / (1 + Z^2/total_admissions)
  )

# write excel
write_xlsx(IMDEthnicity, "../output/Hospitalisations/PrimaryCause/EmergencyPercentage/Tables/PrimaryCause_PercentageEmergency_IMDEthnicity.xlsx")


#ploted with equipy

######### Age and IMD ################

# values for plotting
AgeIMD <- BSol %>%
  group_by(IMD_quintile, AgeGroup) %>%
  summarise(
    total_admissions = n(),
    emergency_admissions = sum(Emergency == TRUE, na.rm = TRUE),
    emergency_percentage = (emergency_admissions / total_admissions) * 100
  ) %>%
  mutate(
    AreaCode = "E38000258",
    p_hat = emergency_admissions / total_admissions,
    # Calculate errors
    Z = qnorm(0.975),
    LowerCI95 = 100 * (p_hat + Z^2/(2*total_admissions) - Z * sqrt((p_hat*(1-p_hat)/total_admissions) + Z^2/(4*total_admissions^2))) / (1 + Z^2/total_admissions),
    UpperCI95 = 100 * (p_hat + Z^2/(2*total_admissions) + Z * sqrt((p_hat*(1-p_hat)/total_admissions) + Z^2/(4*total_admissions^2))) / (1 + Z^2/total_admissions)
  )


AgeIMDPlot <-  ggplot(
  AgeIMD,
  aes(x = AgeGroup,
      y = emergency_percentage,
      fill = IMD_quintile)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin=LowerCI95,
                    ymax=UpperCI95),
                width=0.1,
                position = position_dodge(0.9)) +
  labs(title ="Percentage of Total CVD Hospital Admissions Considered as Emergency,\n Primary Cause, BSol (2018 to 2024)" ,
       x = "Age Group",
       y = "Percentage",
       caption = "95% Confidence Intervals",
       fill = "IMD quintile") +
  theme_bw() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 100)) +
  scale_fill_viridis(discrete = TRUE, option = "magma", begin = 0.35, end = 0.95) +
  theme(
    plot.title.position = "plot", # Make the title span the full plot width including axis labels
    plot.title = element_text(hjust = 0.5)) # Center-align the title

AgeIMDPlot

ggsave("../output/Hospitalisations/PrimaryCause/EmergencyPercentage/PrimaryCause_PercentageEmergency_AgeIMD.svg", AgeIMDPlot, width = 10, height= 7)


################################################    Analysis by primary condition group ###############

### Add clinical groups column

## Join to clinical groups
ClinicalGroup <- read_excel("../data/ICD10_CVD_ShortTitles.xlsx") %>%
  select(- `ICD10 Short Title`)

BSol <- left_join(BSol, ClinicalGroup, by= c("DiagnosisGroup" = "Code"))

# remove cases where clinical subgroup has been coded as 'not cvd'
ClinicalGroup_CVD <- BSol %>%
  filter(`Clinical Subgroup` != 'Not CVD')


# values for plotting
ClinicalSubgroup <- ClinicalGroup_CVD %>%
  group_by(`Clinical Subgroup`) %>%
  summarise(
    total_admissions = n(),
    emergency_admissions = sum(Emergency == TRUE, na.rm = TRUE),
    emergency_percentage = (emergency_admissions / total_admissions) * 100
  ) %>%
  mutate(
    AreaCode = "E38000258",
    p_hat = emergency_admissions / total_admissions,
    # Calculate errors
    Z = qnorm(0.975),
    LowerCI95 = 100 * (p_hat + Z^2/(2*total_admissions) - Z * sqrt((p_hat*(1-p_hat)/total_admissions) + Z^2/(4*total_admissions^2))) / (1 + Z^2/total_admissions),
    UpperCI95 = 100 * (p_hat + Z^2/(2*total_admissions) + Z * sqrt((p_hat*(1-p_hat)/total_admissions) + Z^2/(4*total_admissions^2))) / (1 + Z^2/total_admissions)
  )


ClinicalSubgroupPlot <-  ggplot(
  ClinicalSubgroup,
  aes(x = reorder(`Clinical Subgroup`, emergency_percentage),
      y = emergency_percentage)) +
  geom_col(fill="#3488a6") +
  geom_errorbar(aes(ymin=LowerCI95,
                    ymax=UpperCI95),
                width=0.1) +
  labs(title ="Percentage of Total CVD Hospital Admissions Considered as Emergency,\n Primary Cause, BSol (2018 to 2024)" ,
       x = "Condition Group",
       y = "Percentage",
       caption = "95% Confidence Intervals") +
  theme_bw() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 100)) +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, option = "magma", begin = 0.35, end = 0.95) +
  theme(
    plot.title.position = "plot", # Make the title span the full plot width including axis labels
    plot.title = element_text(hjust = 0.5)) # Center-align the title

ClinicalSubgroupPlot

ggsave("../output/Hospitalisations/PrimaryCause/EmergencyPercentage/PrimaryCause_PercentageEmergency_ClinicalSubgroup.svg", ClinicalSubgroupPlot, width = 12, height= 7)

##################

### Age under 75
#################

under75 <- BSol %>%
  filter(Age2Cat == 'Under 75')

characteristics <- c('Locality', 'IMD_quintile', 'AgeGroup', 'Gender', 'Ethnicity', 'GP_Code')

for(characteristic in characteristics) {

  result <- under75 %>%
    group_by(across(all_of(characteristic))) %>%
    summarise(
      total_admissions = n(),
      emergency_admissions = sum(Emergency == TRUE, na.rm = TRUE),
      emergency_percentage = (emergency_admissions / total_admissions) * 100
    ) %>%
    mutate(
      AreaCode = "E38000258",
      p_hat = emergency_admissions / total_admissions,
      Value = 100 * p_hat,
      # Calculate errors
      Z = qnorm(0.975),
      LowerCI95 = 100 * (p_hat + Z^2/(2*total_admissions) - Z * sqrt((p_hat*(1-p_hat)/total_admissions) + Z^2/(4*total_admissions^2))) / (1 + Z^2/total_admissions),
      UpperCI95 = 100 * (p_hat + Z^2/(2*total_admissions) + Z * sqrt((p_hat*(1-p_hat)/total_admissions) + Z^2/(4*total_admissions^2))) / (1 + Z^2/total_admissions)
    )


  # Save the result as a separate data frame using assign()
  assign(paste0("under75_result_", characteristic), result)

}


######## under 75 by Sex #######

under75_Gender <- ggplot(
  under75_result_Gender,
  aes(x = Gender,
      y = emergency_percentage)
) +
  geom_col(fill = "#3488a6") +
  geom_errorbar(aes(ymin = LowerCI95,
                    ymax = UpperCI95),
                width = 0.1) +
  labs(title ="Percentage of Total CVD Hospital Admissions in Individuals Under 75 Considered as Emergency,\n Primary Cause, BSol (2018 to 2024)" ,
       x = "Sex",
       y = "Percentage",
       caption = "95% Confidence Intervals") +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 100)) +
  theme_bw() +
  theme(text = element_text(size=15))

under75_Gender

ggsave("../output/Hospitalisations/PrimaryCause/EmergencyPercentage/PrimaryCause_PercentageEmergency_under75_Gender.svg", under75_Gender, width = 10, height= 10)

############## under 75 by ethnicity ##############

under75_result_Ethnicity$Ethnicity <- str_wrap(under75_result_Ethnicity$Ethnicity, width = 35)

under75_Ethnicity <-  ggplot(
  under75_result_Ethnicity,
  aes(x = reorder(Ethnicity, emergency_percentage),
      y = emergency_percentage)) +
  geom_col(fill="#3488a6") +
  geom_errorbar(aes(ymin=LowerCI95,
                    ymax=UpperCI95),
                width= 0.1) +
  labs(title ="Percentage of Total CVD Hospital Admissions in Individuals Under 75 Considered as Emergency,\n Primary Cause, BSol (2018 to 2024)" ,
       x = "Ethnicity",
       y = "Percentage",
       caption = "95% Confidence Intervals") +
  theme_bw() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 100)) +
  geom_hline(yintercept = mean(under75_result_Ethnicity$emergency_percentage, na.rm=TRUE)) +
  annotate("text", x=9, y=72.5, label="Mean", size = 4) +
  coord_flip() +
  theme(
    plot.title.position = "plot", # Make the title span the full plot width including axis labels
    plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=15)) +
  theme(plot.margin = margin(t= 10, r = 50))# edit top and right margins

under75_Ethnicity

ggsave("../output/Hospitalisations/PrimaryCause/EmergencyPercentage/PrimaryCause_PercentageEmergency_under75_Ethnicity.svg", under75_Ethnicity, width = 12, height= 10)

##### under 75 by IMD ############

under75_IMD <- ggplot(
  under75_result_IMD_quintile,
  aes(x = IMD_quintile,
      y = emergency_percentage)) +
  geom_col(fill="#3488a6") +
  geom_errorbar(aes(ymin=LowerCI95,
                    ymax=UpperCI95),
                width=0.1) +
  labs(title ="Percentage of Total CVD Hospital Admissions Considered as Emergency,\n Primary Cause, BSol (2018 to 2024)",
       x = "IMD Quintile",
       y = "Percentage",
       caption = "95% Confidence Intervals") +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 100)) +
  theme_bw()

under75_IMD


########### under 75 by IMD and Sex ########


# values for plotting
under75_IMDGender <- under75 %>%
  group_by(IMD_quintile, Gender) %>%
  summarise(
    total_admissions = n(),
    emergency_admissions = sum(Emergency == TRUE, na.rm = TRUE),
    emergency_percentage = (emergency_admissions / total_admissions) * 100
  ) %>%
  mutate(
    p_hat = emergency_admissions / total_admissions,
    Value = 100 * p_hat,
    # Calculate errors
    Z = qnorm(0.975),
    LowerCI95 = 100 * (p_hat + Z^2/(2*total_admissions) - Z * sqrt((p_hat*(1-p_hat)/total_admissions) + Z^2/(4*total_admissions^2))) / (1 + Z^2/total_admissions),
    UpperCI95 = 100 * (p_hat + Z^2/(2*total_admissions) + Z * sqrt((p_hat*(1-p_hat)/total_admissions) + Z^2/(4*total_admissions^2))) / (1 + Z^2/total_admissions)
  ) %>%
  filter(Gender != "Not known") %>%
  group_by(Gender) %>%
  mutate(
    SexGroupMean = mean(emergency_percentage)
  )

under75_IMDGenderPlot <-  ggplot(
  under75_IMDGender,
  aes(x = IMD_quintile,
      y = emergency_percentage)) +
  geom_col(fill="#3488a6") +
  facet_wrap(~Gender, ncol =1) +
  geom_errorbar(aes(ymin=LowerCI95,
                    ymax=UpperCI95),
                width=0.1) +
  labs(title ="Percentage of Total CVD Hospital Admissions in Individuals Under 75 Considered as Emergency,\n Primary Cause, BSol (2018 to 2024)" ,
       x = "IMD Quntile",
       y = "Percentage",
       caption = "95% Confidence Intervals\n
       exluding when gender not known due to small numbers",
       linetype = "Mean Percentage") +
  theme_bw() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 100)) +
  geom_hline(aes(yintercept = SexGroupMean, linetype = Gender),
             data = under75_IMDGender) +
  coord_flip() +
  theme(
    plot.title.position = "plot", # Make the title span the full plot width including axis labels
    plot.title = element_text(hjust = 0.5)) # Center-align the title


under75_IMDGenderPlot


ggsave("../output/Hospitalisations/PrimaryCause/EmergencyPercentage/PrimaryCause_PercentageEmergency_under75_IMDGender.svg", under75_IMDGenderPlot, width = 9, height= 7)

