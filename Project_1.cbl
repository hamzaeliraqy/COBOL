      ******************************************************************
      * Author:     Hamza Eliraqy
      * Student No: 40976488
      * Date:       11/02/2022
      * Purpose:    Create student record from user input
      * Tectonics:  cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-RECORDS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-REC ASSIGN TO "C:\Users\hamza\TEST_DATA.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-REC.
       01  FIELDS.
           05 UINPUT   PIC X(5).
       01  STUDENT-RECORD.
           05 STUDENT-NUMBER PIC 9(7).
           05 TUITION PIC 9(5).
           05 STUDENT-NAME PIC X(40).

       WORKING-STORAGE SECTION.
       01 CONTROL-FIELDS.
           05 RECORD-NUMBER PIC 9(1).

       PROCEDURE DIVISION.
       101-STUDENT-DATA.
           PERFORM 201-APP-STARTUP.
           PERFORM 301-END-APP.

       102-ADD-STUDENT-DATA.
           PERFORM 205-OPEN-FILE.
           PERFORM 207-WRITE-STUDENT-DATA.
           PERFORM 209-CLOSE-FILE.

       201-APP-STARTUP.
           DISPLAY "Student Records." UPON CONSOLE.
           DISPLAY "Do you want to start ?" UPON CONSOLE.
           ACCEPT UINPUT FROM CONSOLE.
           IF UINPUT="YES" OR "yes"
               PERFORM 203-TAKE-IN-RECORD
           ELSE
               PERFORM 301-END-APP
           END-IF.

       203-TAKE-IN-RECORD.
           DISPLAY "Enter student number : " UPON CONSOLE.
           ACCEPT STUDENT-NUMBER FROM CONSOLE.
           DISPLAY "Enter Tuition owed : " UPON CONSOLE.
           ACCEPT TUITION FROM CONSOLE.
           DISPLAY "Enter Student Name " UPON CONSOLE.
           ACCEPT STUDENT-NAME FROM CONSOLE.
           PERFORM 102-ADD-STUDENT-DATA.

       205-OPEN-FILE.
           IF RECORD-NUMBER=1
               DISPLAY "Adding second record" UPON CONSOLE
           ELSE
               OPEN OUTPUT STUDENT-REC
               MOVE 1 TO RECORD-NUMBER
           END-IF.

       207-WRITE-STUDENT-DATA.
           WRITE STUDENT-RECORD.
           DISPLAY "Student added." UPON CONSOLE.

       209-CLOSE-FILE.
           PERFORM 201-APP-STARTUP.
           CLOSE STUDENT-REC.

       301-END-APP.
           DISPLAY "Goodbye." UPON CONSOLE.
           CLOSE STUDENT-REC.
               STOP RUN.

       END PROGRAM STUDENT-RECORDS.
