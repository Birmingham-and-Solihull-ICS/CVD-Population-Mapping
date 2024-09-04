-- Counting all CVD admissions within 6 months
WITH AllData AS (
	SELECT
		NHSNumber,
		SUBSTRING(DiagnosisCode, 1, 3) AS DiagnosisGroup,
		OSLAUA AS LocalAuthority,
		WestminsterParliamentaryConstituency AS Constituency,
		EthnicCategoryCodeDescription AS Ethnicity,
		GenderDescription AS Gender,
		AgeOnAdmission,
		AdmissionDate,
		LowerlayerSuperOutputArea2011 AS LSOA11,
	    LowerLayerSuperOutputArea AS LSOA21
	  FROM EAT_Reporting_BSOL.SUS.VwInpatientEpisodesDiagnosisRelational AS A
	  LEFT JOIN EAT_Reporting_BSOL.SUS.VwInpatientEpisodesPatientGeography AS B
	  ON A.EpisodeId = B.EpisodeId
	  WHERE 
	    -- CVD ICD-10 Code
	    DiagnosisCode LIKE 'I%' AND
		-- Only get one row per spell
		OrderInSpell = 1 AND
	    -- Primary diagnosis
		DiagnosisOrder = 1 AND
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
		LocalAuthority,
		Constituency,
		Ethnicity,
		Gender,
		AgeOnAdmission,
		LSOA11,
		LSOA21,
		AdmissionDate,
        ROW_NUMBER() OVER (PARTITION BY NHSNumber, DiagnosisGroup ORDER BY AdmissionDate) AS RN
    FROM
        AllData
),

FilteredAdmissions AS (
    SELECT
        A.NHSNumber,
		A.DiagnosisGroup,
		A.LocalAuthority,
		A.Constituency,
		A.Ethnicity,
		A.Gender,
		A.AgeOnAdmission,
		A.LSOA11,
		A.LSOA21,
		A.AdmissionDate
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