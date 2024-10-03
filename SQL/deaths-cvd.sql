--DROP TABLE #deaths
--DROP TABLE #cvd

--DROP TABLE #deaths
--DROP TABLE #suicides
--DROP TABLE #MHSDS

WITH duplicates AS (
	SELECT 
		[Pseudo_NHSNumber], 
		EthnicCategory AS Ethnic_Code,
		NHSDEthnicity_Category, 
		ROW_NUMBER() OVER (ORDER BY [Pseudo_NHSNumber]) AS row_num
	FROM [EAT_Reporting_BSOL].[MentalHealth].[Vw_MHSDS_001MasterPatientIndex]
	WHERE 
		NHSDEthnicity_Category IS NOT NULL AND
		NHSDEthnicity_Category NOT LIKE '' AND
		EthnicCategory != 'Z' AND 
		[Pseudo_NHSNumber] IS NOT NULL
  ),
max_row_num AS (
	SELECT [Pseudo_NHSNumber], max(row_num) as max_row_num
	FROM duplicates
	GROUP BY [Pseudo_NHSNumber]
	),
MH as (
	SELECT M.[Pseudo_NHSNumber], D.NHSDEthnicity_Category, D.Ethnic_Code
	FROM max_row_num AS M
	LEFT JOIN duplicates AS D
	ON M.max_row_num = D.[row_num]
)

SELECT * 
INTO #MHSDS
FROM MH;

SELECT
	   [DEC_SEX] AS Sex
      ,[DEC_MARITAL_STATUS] AS Marital_Status
      ,[DEC_AGEC] AS Age
      ,[S_UNDERLYING_COD_ICD10]
      ,[UnderlyingCauseOfDeathChapterDescription]
      ,[LSOA_OF_RESIDENCE_CODE] AS LSOA
      ,[PCO_OF_RESIDENCE_CODE] AS LA_Code
      ,[PLACE_OF_DEATH_ULA_CODE]
      ,[ULA_OF_RESIDENCE_CODE]
      ,[PatientId]
	  ,S_COD_CODE_1
	  ,S_COD_CODE_2
	  ,S_COD_CODE_3
	  ,S_COD_CODE_4
	  ,S_COD_CODE_5
	  ,S_COD_CODE_6
	  ,S_COD_CODE_7
	  ,S_COD_CODE_8
	  ,S_COD_CODE_9
	  ,S_COD_CODE_10
	  ,S_COD_CODE_11
	  ,S_COD_CODE_12
	  ,S_COD_CODE_13
	  ,S_COD_CODE_14
	  ,S_COD_CODE_15
	  ,E.[Ethnic_Code]
	  ,E.[Ethnic_Grouping_PH]
	  ,E.[Is_Deceased]
  INTO #deaths
  FROM [EAT_Reporting_BSOL].[Other].[VwDeathsRegister]
  LEFT JOIN [EAT_Reporting_BSOL].[Demographic].Ethnicity AS E
  ON [PatientId] = E.[Pseudo_NHS_Number]
  WHERE 
	REG_DATE >= '2014-01-01' AND
	REG_DATE <= '2023-12-31' AND
	ULA_OF_RESIDENCE_CODE IN ('E08000025', 'E08000029')

SELECT * 
INTO #cvd
FROM #deaths
WHERE 
	[S_UNDERLYING_COD_ICD10] LIKE 'I%' OR
	S_COD_CODE_1 LIKE 'I%' OR
	S_COD_CODE_2 LIKE 'I%' OR
	S_COD_CODE_3 LIKE 'I%' OR
	S_COD_CODE_4 LIKE 'I%' OR
	S_COD_CODE_5 LIKE 'I%' OR
	S_COD_CODE_6 LIKE 'I%' OR
	S_COD_CODE_7 LIKE 'I%' OR
	S_COD_CODE_8 LIKE 'I%' OR
	S_COD_CODE_9 LIKE 'I%' OR
	S_COD_CODE_10 LIKE 'I%' OR
	S_COD_CODE_11 LIKE 'I%' OR
	S_COD_CODE_12 LIKE 'I%' OR
	S_COD_CODE_13 LIKE 'I%' OR
	S_COD_CODE_14 LIKE 'I%' OR
	S_COD_CODE_15 LIKE 'I%'

SELECT *
FROM #cvd