 ******************************************************************
      * Author: Hamza El Iraqy
      * Date:  17/04/2022
      * Purpose: CALCULATE GRADE
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP_PROJECT3_CALL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 CALC-FILEDS.
           05 STUDENT-TOTAL PIC 9(3).
           05 STUDENT-AVERAGE PIC 9(3).
       LINKAGE SECTION.
       COPY ".\Callbook.DAT".
       PROCEDURE DIVISION USING COURSE-AVG1 COURSE-AVG2
       COURSE-AVG3 COURSE-AVG4 COURSE-AVG5.
       MAIN-PROCEDURE.
            ADD COURSE-AVG1 COURSE-AVG2 COURSE-AVG3
            COURSE-AVG4 COURSE-AVG5 GIVING STUDENT-TOTAL ROUNDED.
            DIVIDE STUDENT-TOTAL BY 5 GIVING STUDENT-AVERAGE.
            DISPLAY "STUDENT GRADE : " STUDENT-AVERAGE UPON CONSOLE.
            EXIT PROGRAM.
       END PROGRAM TP_PROJECT3_CALL.
