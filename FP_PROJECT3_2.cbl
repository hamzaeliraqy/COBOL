  ******************************************************************
      * Author: Hamza El Iraqy/40976488
      * Date:  17/04/2022
      * Purpose: CONVERT AN SUQUENTIAL FILE TO INDEXED FILE
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FP_PROGRAM.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT I-STUDENT-RECORDS ASSIGN TO
           "C:\Users\hamza\STUFILE.TXT"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT O-STUDENT-RECORDS ASSIGN TO
           "C:\Users\hamza\STUFILE-C.TXT"
           ORGANIZATION IS INDEXED
           RECORD KEY IS STUDENT-NUMBER
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS STATUS-FIELD.

       DATA DIVISION.
       FILE SECTION.
       FD I-STUDENT-RECORDS.
       01 STUDENTS-IN.
           05 I-STUDENT-NUMBER PIC 9(6).
           05 I-TUITION-OWED PIC 9(6).
           05 I-STUDENT-NAME PIC X(40).
           05 I-PROGRAM-OF-STUDY PIC X(5).
           05 I-COURSE-CODE1 PIC X(7).
           05 I-COURSE-AVERAGE1 PIC 9(3).
           05 I-COURSE-CODE2 PIC X(7).
           05 I-COURSE-AVERAGE2 PIC 9(3).
           05 I-COURSE-CODE3 PIC X(7).
           05 I-COURSE-AVERAGE3 PIC 9(3).
           05 I-COURSE-CODE4 PIC X(7).
           05 I-COURSE-AVERAGE4 PIC 9(3).
           05 I-COURSE-CODE5 PIC X(7).
           05 I-COURSE-AVERAGE5 PIC 9(3).

       FD O-STUDENT-RECORDS.
       01 STUDENTS-OUT.
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
           05 EOF-FLAG PIC X(3) VALUE "NO".
           05 STATUS-FIELD PIC X(2).
           05 NEW-FLAG PIC X(3) VALUE "NO".


       PROCEDURE DIVISION.
       100-CREATE-STUDENT-FILE.
           PERFORM 101-INTIALIZE-CREATE-FILE.
           PERFORM 102-CREATE-STUDENT-RECORDS UNTIL
                   EOF-FLAG = "YES".
           PERFORM 103-TERMINATE-PROGRAM.


       101-INTIALIZE-CREATE-FILE.
           PERFORM 201-OPEN-FILES.
           PERFORM 202-READ-FILE.


       102-CREATE-STUDENT-RECORDS.
           PERFORM 203-WRITE-FILES.
           PERFORM 202-READ-FILE.

       201-OPEN-FILES.
           OPEN INPUT I-STUDENT-RECORDS.
           OPEN OUTPUT O-STUDENT-RECORDS.

       203-WRITE-FILES.
           WRITE STUDENTS-OUT
               INVALID KEY MOVE "NO" TO NEW-FLAG
               NOT INVALID KEY MOVE "YES" TO NEW-FLAG.
           DISPLAY STUDENTS-OUT UPON CONSOLE.


       202-READ-FILE.
           READ I-STUDENT-RECORDS
               AT END MOVE "YES" TO EOF-FLAG
               NOT AT END
               DISPLAY STUDENTS-IN UPON CONSOLE
               MOVE STUDENTS-IN TO STUDENTS-OUT
               WRITE STUDENTS-OUT
           END-READ.



       103-TERMINATE-PROGRAM.
           CLOSE I-STUDENT-RECORDS O-STUDENT-RECORDS.
           STOP RUN.
       END PROGRAM FP_PROGRAM.
