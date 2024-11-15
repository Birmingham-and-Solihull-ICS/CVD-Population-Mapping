-- Counting all CVD admissions within 6 months
WITH AllData AS (
	SELECT
		NHSNumber,
		SUBSTRING(DiagnosisCode, 1, 3) AS DiagnosisGroup,
		ProviderCode,
		ProviderName,
		OSLAUA AS LocalAuthority,
		WestminsterParliamentaryConstituency AS Constituency,
		LowerlayerSuperOutputArea2011 AS LSOA_2011, 
		LowerLayerSuperOutputArea AS LSOA_2021, 
		GMPOrganisationCode AS GP_Code,
		E.[Ethnic_Code],
	    E.[Ethnic_Grouping_PH],
		GenderDescription AS Gender,
		AgeOnAdmission,
		AdmissionDate,
		AdmissionSourceCode,
		AdmissionSourceDescription,
		AdmissionMethodCode,
		AdmissionMethodDescription
	  FROM EAT_Reporting_BSOL.SUS.VwInpatientEpisodesDiagnosisRelational AS A
	  LEFT JOIN EAT_Reporting_BSOL.SUS.VwInpatientEpisodesPatientGeography AS B
	  ON A.EpisodeId = B.EpisodeId
	  LEFT JOIN [EAT_Reporting_BSOL].[Demographic].Ethnicity AS E
	  ON [NHSNumber] = E.[Pseudo_NHS_Number]
	  WHERE 
		-- CVD ICD-10 Code
	    DiagnosisCode LIKE 'I%' AND
	    -- Primary diagnosis
		DiagnosisOrder = 1 AND
		-- Only get one row per spell
		OrderInSpell = 1 AND
		-- Last 6 years --
		AdmissionDate >= '2018-04-01' AND
		AdmissionDate < '2024-04-01' AND
		-- Within BSol --
		OSLAUA IN ('E08000025','E08000029') AND
		NHSNumber IS NOT NULL 
		),

DistinctAdmissions AS (
    SELECT
        NHSNumber,
		DiagnosisGroup,
		ProviderCode,
		ProviderName,
		LocalAuthority,
		Constituency,
		LSOA_2011,
		LSOA_2021,
		GP_Code,
		Ethnic_Code,
	    Ethnic_Grouping_PH,
		Gender,
		AgeOnAdmission,
		AdmissionDate,
		AdmissionSourceCode,
		AdmissionSourceDescription,
		AdmissionMethodCode,
		AdmissionMethodDescription,
        ROW_NUMBER() OVER (PARTITION BY NHSNumber, DiagnosisGroup ORDER BY AdmissionDate) AS RN
    FROM
        AllData
),

FilteredAdmissions AS (
    SELECT
        A.NHSNumber,
		A.DiagnosisGroup,
		A.ProviderCode,
		A.ProviderName,
		A.LocalAuthority,
		A.Constituency,
		A.LSOA_2011,
		A.LSOA_2021,
		A.GP_Code,
		A.Ethnic_Code,
	    A.Ethnic_Grouping_PH,
		A.Gender,
		A.AgeOnAdmission,
		A.AdmissionDate,
		A.RN,
		A.AdmissionSourceCode,
		A.AdmissionSourceDescription,
		A.AdmissionMethodCode,
		A.AdmissionMethodDescription
    FROM
        DistinctAdmissions A
    LEFT JOIN DistinctAdmissions B
	-- Join to next date with same person and admission code with within 90 days
    ON A.NHSNumber = B.NHSNumber
    AND A.DiagnosisGroup = B.DiagnosisGroup
    AND A.RN = B.RN + 1
    AND A.AdmissionDate <= DATEADD(DAY, 90, B.AdmissionDate)
	-- And then get rid of any rows that successfully joined
    WHERE B.NHSNumber IS NULL
)

SELECT *
FROM FilteredAdmissions
ORDER BY NHSNumber