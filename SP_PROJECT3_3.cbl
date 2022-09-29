  ******************************************************************
      * Author: Hamza El Iraqy
      * Date:  17/04/2022
      * Purpose: MODIFY TUITION OF STUDENT RECORD BASED ON STUDENT NUMBER
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SP-PROJECT3.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-RECORD ASSIGN TO
           "C:\Users\hamza\STUFILE-C.txt"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS STUDENT-NUMBER
           FILE STATUS IS STATUS-FIELD.

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-RECORD.
       01 STUDENTS.
           05 STUDENT-NUMBER PIC 9(6).
           05 TUITION-OWED PIC 9(6).
           05 STUDENT-NAME PIC X(40).
           05 PROGRAM-OF-STUDY PIC X(5).
           05 COURSE-CODE1 PIC X(7).
           05 COURSE-AVERAGE1 PIC 9(3).
           05 COURSE-CODE2 PIC X(7).
           05 COURSE-AVERAGE2 PIC 9(3).
           05 COURSE-CODE3 PIC X(7).
           05 COURSE-AVERAGE3 PIC 9(3).
           05 COURSE-CODE4 PIC X(7).
           05 COURSE-AVERAGE4 PIC 9(3).
           05 COURSE-CODE5 PIC X(7).
           05 COURSE-AVERAGE5 PIC 9(3).

       WORKING-STORAGE SECTION.
       01 CONTROL-FIELDS.
           05 STUDENT-INPUT PIC 9(6).
           05 TUITION-INPUT PIC 9(6).
           05 STUDENT-OUTPUT PIC 9(6).
           05 TUITION-OUTPUT PIC 9(6).
           05 EOF-FLAG PIC X(3) VALUE "NO".
           05 STATUS-FIELD PIC X(2).
           05 NEW-FLAG PIC X(3) VALUE "NO".

       SCREEN SECTION.
       01 INPUT-SCREEN.
           05 VALUE "STUDENT NUMBER" LINE 5 COL 5.
           05 IN-STUDENT LINE 5 COL 25
               PIC 9(6) TO STUDENT-INPUT.

       01 OUTPUT-SCREEN.
           05 VALUE "TUITION-OWED" LINE 7 COL 5.
           05 OUT-STUDENT LINE 7 COL 25
               PIC 9(6) TO TUITION-INPUT.

       01 UPDATE-SCREEN.
           05 VALUE "STUD NUMBER" LINE 9 COL 5.
           05 UP-STUDENT LINE 7 COL 25
               PIC 9(6) TO STUDENT-OUTPUT.
           05 VALUE "UPDATED TUITION" LINE 11 COL 5.
           05 UP-TUITION LINE 11 COL 25
               PIC 9(6) TO TUITION-OUTPUT.

       PROCEDURE DIVISION.
       100-INTIALIZE-PROCEDURE.
           PERFORM 101-OPEN-FILE.
           PERFORM 102-TAKE-USER-INPUT.
           PERFORM 104-SEARCH UNTIL EOF-FLAG = "YES".
           PERFORM 105-FIND.
           PERFORM 106-TERMINATE-PROCEDURE.

       101-OPEN-FILE.
           OPEN I-O STUDENT-RECORD.

       102-TAKE-USER-INPUT.
           DISPLAY INPUT-SCREEN.
           ACCEPT INPUT-SCREEN.

       104-SEARCH.
           READ STUDENT-RECORD
               AT END MOVE "YES" TO EOF-FLAG
           END-READ.

       105-FIND.
           IF STUDENT-NUMBER = STUDENT-INPUT THEN
               DISPLAY "FOUND" UPON CONSOLE
               PERFORM 201-UPDATE-RECORD
           ELSE
               DISPLAY "NOT FOUND" UPON CONSOLE
           END-IF.

       201-UPDATE-RECORD.
           DISPLAY OUTPUT-SCREEN.
           ACCEPT OUTPUT-SCREEN.
           MOVE TUITION-INPUT TO TUITION-OWED.
           REWRITE STUDENTS.
           DISPLAY "UPDATED" UPON CONSOLE.


       106-TERMINATE-PROCEDURE.
           CLOSE STUDENT-RECORD.
           STOP RUN.

       END PROGRAM SP-PROJECT3.
