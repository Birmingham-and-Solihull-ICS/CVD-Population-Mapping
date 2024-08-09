USE PH_Requests


/*
   Program : REQ3289_BSOL_CVD_Deaths_10years_2014_to_2023.sql
 
==========
05-08-2024
==========
  PURPOSE : This sql program is used to extract 10 years CVD deaths data for Birmingham Local Authority and Solihull Local Authority residents (ie BSOL) for the period "01/01/2014 to 31/12/2023".


    Note :  1. In this routine we are extracting ONS monthly deaths data for Birmingham and Solihull residents for the period "01/01/2014 to 31/12/2023".

            2. The CVD ICD10 codes are :-

                   CVD (Circulatory Diseases)   - ICD10 codes "I00 to I99"  
    
                   Note, CVD stands for "Cardiovascular Disease".
                                                                                                                                      
               Note, the definition is based on fingertips indicator "Under 75 mortality rate from all circulatory diseases", indicator id=40401, the link to the definition is here :-
                         https://fingertips.phe.org.uk/search/Circulatory#page/6/gid/1/pat/15/par/E92000001/ati/502/are/E08000025/iid/40401/age/163/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1/page-options/car-do-0

            3. In our extraction we are extracting the data using the [UNDERLYING CAUSE OF DEATH CODE] field.

            4. MS Excel workbook created is called 'REQ3289_BSOL_CVD_Deaths_10years_2014_to_2023.xlsx'.
               The data from this routine are written to the worksheet called 'CVD_DeathsData'. 

                                                                                                   

    Timing   :  This .SQL program takes approximately 3 minutes and 1 second to run.
*/
   
  /* NB  In stored procedures we can pass a parameter as a value but not as a table name      
          - ie due to SQL compiling the stored procedure
  */


/***************************************************************************************************************/


/* Step 0 : set the Required System options (NB This Stored Procedure cannot be created without setting these options)
*/

/*
SET ANSI_NULLS ON
SET ANSI_WARNINGS ON
*/

/***************************************************************************************************************/


/* Step 1 : Delete any Temporary tables in the database. These tables begin with tblTemp etc .
*/

IF EXISTS (SELECT name FROM sysobjects
             WHERE name = 'tblTempCursorValues')
    BEGIN
      DROP TABLE tblTempCursorValues
    END

--  IF @@ERROR <> 0              
--       GOTO Error_Handler      
 

 CREATE TABLE tblTempCursorValues (TableName NVarChar(20))

  INSERT INTO tblTempCursorValues (TableName)
      VALUES ('tblTemp') 
  INSERT INTO tblTempCursorValues (TableName)
      VALUES ('tblTemp1')

/*   Create the Cursor variable and populate it
*/

DECLARE curs_variable CURSOR      -- ie declare cursor and its values
  FORWARD_ONLY
  STATIC
  READ_ONLY
  FOR
    SELECT *
      FROM tblTempCursorValues 

--  IF @@ERROR <> 0      
--       GOTO Error_Handler      

OPEN curs_variable                                                -- ie open the Cursor

--  IF @@ERROR <> 0      
--       GOTO Error_Handler      

DECLARE @strTableName NVarChar(20)        -- ie declare local variable to store Cursor values
DECLARE @strTSQL      NVarChar(300)       -- ie declare local variable for the T-SQL string, note this variable
                                          --     has to be of type NVarChar() or VarChar()
--  IF @@ERROR <> 0      
--       GOTO Error_Handler      

 
FETCH NEXT                                             -- ie obtain the first row
   FROM curs_variable                                  --    in the Cursor
      INTO @strTableName                               -- ie read first record into these variables                        
   
--  IF @@ERROR <> 0      
--       GOTO Error_Handler      


WHILE @@FETCH_STATUS = 0                                     -- ie loop through the remainder rows in the Cursor
    BEGIN                                                                                    
        SET @strTSQL = ''                                                     -- ie initialise T-SQL string
        SET @strTSQL = 'IF EXISTS (SELECT name FROM sysobjects
                                     WHERE name = ' + CHAR(39) + @strTableName + CHAR(39) + ')
                           BEGIN
                             DROP TABLE ' + @strTableName + CHAR(13) + 
                         ' END'                  

             PRINT CONVERT(Char(200),@strTSQL)  + CONVERT(Char(5),@@FETCH_STATUS)
             EXECUTE(@strTSQL)
                 FETCH NEXT                                                   -- ie read the next record
                      FROM curs_variable                                           --    from the Cursor
                          INTO @strTableName                             -- ie read the next record into these variables      
  END

-- IF @@ERROR <> 0      
--       GOTO Error_Handler 


CLOSE curs_variable                                                                                       -- ie close the Cursor            
DEALLOCATE curs_variable                                                                           -- ie remove the Cursor from memory                 

--  IF @@ERROR <> 0      
--       GOTO Error_Handler      


GO

/***************************************************************************************************************/

/* Step 2 : Extract the data of interest for the relevant period.

            Table : tblTemp1  (ie 23,355 records)
*/

SELECT '01/01/2014 to 31/12/2023' As [Period],
       T1.[Year],
       T1.[Month],
       T1.[MonthText],
       'BSOL' AS [ICB],
       T1.[LOCAL AUTHORITY CODE OF USUAL RESIDENCE OF DECEASED],
       'CVD Deaths' As [Analysis],
       T1.[UNDERLYING CAUSE OF DEATH CODE],
       T1.[SEX],
       T1.[AgeInYears],
       T1.[DATE OF DEATH OF DECEASED],
       T1.[LOWER SOA CODE OF USUAL RESIDENCE OF DECEASED]
  INTO tblTemp1
  FROM PH_Deaths.dbo.[tblMonthlyDeathsData_BCC] T1
     WHERE ((T1.[LOCAL AUTHORITY CODE OF USUAL RESIDENCE OF DECEASED] IN ('E08000025', 'E08000029')) AND
            (T1.[UNDERLYING CAUSE OF DEATH CODE] LIKE 'I%') AND
            ((CONVERT(DATETIME, CONVERT(CHAR(2),SUBSTRING(T1.[DATE OF DEATH OF DECEASED],1,2)) + CONVERT(CHAR(1),'/') + CONVERT(CHAR(2),SUBSTRING(T1.[DATE OF DEATH OF DECEASED],3,2)) + CONVERT(CHAR(1),'/') + CONVERT(CHAR(4),SUBSTRING(T1.[DATE OF DEATH OF DECEASED],5,4)), 103) >= CONVERT(DATETIME, '01/01/2014', 103)) AND
             (CONVERT(DATETIME, CONVERT(CHAR(2),SUBSTRING(T1.[DATE OF DEATH OF DECEASED],1,2)) + CONVERT(CHAR(1),'/') + CONVERT(CHAR(2),SUBSTRING(T1.[DATE OF DEATH OF DECEASED],3,2)) + CONVERT(CHAR(1),'/') + CONVERT(CHAR(4),SUBSTRING(T1.[DATE OF DEATH OF DECEASED],5,4)), 103) <= CONVERT(DATETIME, '31/12/2023', 103))))

GO

/***************************************************************************************************************/

/* Step 3 : Aggregate the stats to get the counts.

            Table : tblTemp1
*/

-- Copy and paste method to write data to MS Excel.
-- This data has been written to worksheet CVD_DeathsData' in MS Excel workbook "REQ3289_BSOL_CVD_Deaths_10years_2014_to_2023.xlsx"

SELECT T1.[Period],
       T1.[Year],
       T1.[Month],
       T1.[MonthText],
       T1.[ICB],
       T1.[LOCAL AUTHORITY CODE OF USUAL RESIDENCE OF DECEASED],
       T1.[Analysis],
       T1.[UNDERLYING CAUSE OF DEATH CODE],
       T1.[SEX],
       T1.[AgeInYears],
       T1.[DATE OF DEATH OF DECEASED],
       T1.[LOWER SOA CODE OF USUAL RESIDENCE OF DECEASED],
       COUNT(*) AS [No of Deaths]
  FROM tblTemp1 T1
    GROUP BY T1.[Period],
             T1.[Year],
             T1.[Month],
             T1.[MonthText],
             T1.[ICB],
             T1.[LOCAL AUTHORITY CODE OF USUAL RESIDENCE OF DECEASED],
             T1.[Analysis],
             T1.[UNDERLYING CAUSE OF DEATH CODE],
             T1.[SEX],
             T1.[AgeInYears],
             T1.[DATE OF DEATH OF DECEASED],
             T1.[LOWER SOA CODE OF USUAL RESIDENCE OF DECEASED]
      ORDER BY T1.[Year],
               T1.[Month],
               T1.[MonthText],
               T1.[LOCAL AUTHORITY CODE OF USUAL RESIDENCE OF DECEASED],
               T1.[UNDERLYING CAUSE OF DEATH CODE],
               T1.[SEX],
               T1.[AgeInYears],
               T1.[DATE OF DEATH OF DECEASED],
               T1.[LOWER SOA CODE OF USUAL RESIDENCE OF DECEASED]

GO

/***************************************************************************************************************/

/* Step 4 : Delete all the Temporary tables created in the routine.
*/

DECLARE curs_variable CURSOR      -- ie declare cursor and its values
  FORWARD_ONLY
  STATIC
  READ_ONLY
  FOR
    SELECT *
      FROM tblTempCursorValues 

--  IF @@ERROR <> 0      
--       GOTO Error_Handler      

OPEN curs_variable                                                -- ie open the Cursor

--  IF @@ERROR <> 0      
--       GOTO Error_Handler      

DECLARE @strTableName NVarChar(20)                 -- ie declare local variable to store Cursor values
DECLARE @strTSQL      NVarChar(300)       -- ie declare local variable for the T-SQL string, note this variable
                                                                         --     has to be of type NVarChar() or VarChar()
--  IF @@ERROR <> 0      
--       GOTO Error_Handler      

 
FETCH NEXT                                                  -- ie obtain the first row
   FROM curs_variable                                            --     in the Cursor
      INTO @strTableName                               -- ie read first record into these variables                        
   
--  IF @@ERROR <> 0      
--       GOTO Error_Handler      


WHILE @@FETCH_STATUS = 0                                     -- ie loop through the remainder rows in the Cursor
    BEGIN                                                                                    
        SET @strTSQL = ''                                                     -- ie initialise T-SQL string
        SET @strTSQL = 'IF EXISTS (SELECT name FROM sysobjects
                                     WHERE name = ' + CHAR(39) + @strTableName + CHAR(39) + ')
                           BEGIN
                             DROP TABLE ' + @strTableName + CHAR(13) + 
                         ' END'                  

             PRINT CONVERT(Char(200),@strTSQL)  + CONVERT(Char(5),@@FETCH_STATUS)
             EXECUTE(@strTSQL)
                 FETCH NEXT                                                   -- ie read the next record
                      FROM curs_variable                                           --    from the Cursor
                          INTO @strTableName                             -- ie read the next record into these variables      
  END

-- IF @@ERROR <> 0      
--       GOTO Error_Handler 


CLOSE curs_variable                                                                                       -- ie close the Cursor            
DEALLOCATE curs_variable                                                                           -- ie remove the Cursor from memory                 

--  IF @@ERROR <> 0      
--       GOTO Error_Handler      

IF EXISTS (SELECT name FROM sysobjects
             WHERE name = 'tblTempCursorValues')
    BEGIN
      DROP TABLE tblTempCursorValues
    END

--  IF @@ERROR <> 0      
--       GOTO Error_Handler      


GO

/***************************************************************************************************************/

/* 
RETURN     -- ie exit Stored Procedure 

Error_Handler:
     BEGIN        
       RAISERROR('Error, Transaction not completed - Error occured within the Stored Procedure', 16, 1)          
       ROLLBACK TRANSACTION
       RETURN -100
     END
*/
GO