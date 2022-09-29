      ******************************************************************
      * Author: Hamza El Iraqy
      * Date:  17/04/2022
      * Purpose: Write student record based on read of student and program file
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-RECORDER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PROGRAM-RECORDS ASSIGN TO "C:\Users\hamza\PROGRAM.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT STUDENT-RECORD ASSIGN TO
           "C:\Users\hamza\STUFILE-C.txt"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS STUDENT-NUMBER
           FILE STATUS IS STATUS-FIELD.
           SELECT OUTPUT-RECORD ASSIGN TO "C:\Users\hamza\OUTPUT.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

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

       FD OUTPUT-RECORD.
       01 OUTPUTS.
           05 STUDENT-NM PIC X(40).
           05 STUDENT-AVG PIC 9(3).
           05 PROGRAM-NM PIC X(20).
           05 TUITION-OWD PIC 9(4)V99.

           COPY "C:\Users\hamza\Copybook.DAT".

       WORKING-STORAGE SECTION.
       01 PROGRAM-VALUES.
           05 FILLER PIC X(6) VALUE 'COMPR'.
           05 FILLER PIC X(20) VALUE 'COMPUTER PRGRAMMER'.
           05 FILLER PIC X(6) VALUE 'COMTC'.
           05 FILLER PIC X(20) VALUE 'COMPUTER TECHNICIAN'.
           05 FILLER PIC X(6) VALUE 'COMSC'.
           05 FILLER PIC X(20) VALUE 'COMPUTER SIENCE'.
           05 FILLER PIC X(6) VALUE 'BUSSX'.
           05 FILLER PIC X(20) VALUE 'BUSINESS'.
           05 FILLER PIC X(6) VALUE 'MARKX'.
           05 FILLER PIC X(20) VALUE 'MARKETING'.
           05 FILLER PIC X(6) VALUE 'ACCTC'.
           05 FILLER PIC X(20) VALUE 'ACCOUNTING'.

       01 PRGM REDEFINES PROGRAM-VALUES.
           05 PROGRAM-TABLE OCCURS 20 TIMES.
               10 PROGRAM-CD PIC X(6).
               10 PROGRAM-NME PIC X(20).

       01 CONTROL-FIELDS.
           05 RECORD-NUMBER PIC 9(2).
           05 END-OF-FILE PIC X(1).
           05 END-OF-FILE2 PIC X(1).
           05 ITER PIC 9(2) VALUE 1.
           05 STUDENT-TOTAL PIC 9(3).
           05 STUDENT-AVERAGE PIC 9(3).
           05 COMPR PIC X(6) VALUE "COMPR".
           05 COMTC PIC X(6) VALUE "COMTC".
           05 COMSC PIC X(6) VALUE "COMSC".
           05 I PIC 9(2) VALUE 1.
           05 FOUND-FLAG PIC X(1).
           05 STATUS-FIELD PIC X(2).





       PROCEDURE DIVISION.
       101-STUDENT-DATA.
           PERFORM 201-APP-STARTUP.

       103-READ-STUDENT-DATA.
           PERFORM 203-OPEN-FILES.
           PERFORM 205-READ-FILES.
           PERFORM 209-CLOSE-FILES.

       201-APP-STARTUP.
           DISPLAY "STUDENT RECORDER NOW OPERATIONAL" UPON CONSOLE.

       203-OPEN-FILES.
           PERFORM 301-OPEN-STUDENT.
           PERFORM 303-OPEN-PROGRAM.
           PERFORM 305-OPEN-OUTPUTS.

       301-OPEN-STUDENT.
           OPEN INPUT STUDENT-RECORD.

       303-OPEN-PROGRAM.
           OPEN INPUT PROGRAM-RECORDS.

       305-OPEN-OUTPUTS.
           OPEN OUTPUT OUTPUT-RECORD.

       205-READ-FILES.
           PERFORM 307-READ-PROGRAM.
           PERFORM 309-READ-STUDENT.

       307-READ-PROGRAM.
           PERFORM UNTIL END-OF-FILE2 = 'Y' OR ITER = 7
           READ PROGRAM-RECORDS
           AT END MOVE 'Y' TO END-OF-FILE2
           END-READ
           END-PERFORM.

       309-READ-STUDENT.
           PERFORM UNTIL END-OF-FILE = 'Y'
           READ STUDENT-RECORD
           AT END MOVE 'Y' TO END-OF-FILE
           NOT AT END
               PERFORM 311-ADD-STUDENT
           END-READ
           END-PERFORM.

       311-ADD-STUDENT.
           MOVE STUDENT-NAME TO STUDENT-NM.
           MOVE TUITION-OWED TO TUITION-OWD.

           MOVE 1 TO I.
           MOVE 'N' TO FOUND-FLAG.

           PERFORM SEARCH-TABLE
               VARYING I FROM 1 BY 1
               UNTIL FOUND-FLAG = 'Y'
               OR I > 7.

           MOVE STUDENT-AVERAGE TO STUDENT-AVG.
           WRITE OUTPUTS.
           PERFORM DISPLAY-RECORDS.

       SEARCH-TABLE.
           MOVE 'N' TO FOUND-FLAG.
           IF PROGRAM-OF-STUDY = PROGRAM-CD(I)
                   MOVE 'Y' TO FOUND-FLAG
                   MOVE PROGRAM-NME(I) TO PROGRAM-NM.

       DISPLAY-RECORDS.
           DISPLAY "NAME : " STUDENT-NM UPON CONSOLE.
           CALL 'TP_PROJECT3_CALL'
           USING BY CONTENT COURSE-AVERAGE1
           COURSE-AVERAGE2 COURSE-AVERAGE3 COURSE-AVERAGE4
           COURSE-AVERAGE5.
           DISPLAY "PROGRAM : " PROGRAM-NM UPON CONSOLE.
           DISPLAY "TUITION OWED : " TUITION-OWD UPON CONSOLE.

       209-CLOSE-FILES.
           PERFORM 313-CLOSE-STUDENT.
           PERFORM 315-CLOSE-PROGRAM.
           PERFORM 317-CLOSE-OUTPUTS.

       313-CLOSE-STUDENT.
           CLOSE STUDENT-RECORD.

       315-CLOSE-PROGRAM.
           CLOSE PROGRAM-RECORDS.

       317-CLOSE-OUTPUTS.
           CLOSE OUTPUT-RECORD.

           STOP RUN.

       END PROGRAM STUDENT-RECORDER.
