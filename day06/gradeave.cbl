       IDENTIFICATION DIVISION.
       PROGRAM-ID. gradeave.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 30-04-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 STRING_AVERAGE       PIC X(7) VALUE "AVERAGE".
       01 STRING_HIGH          PIC X(4) VALUE "HIGH".
       01 STRING_LOW           PIC X(3) VALUE "LOW".
       01 STRING_QUIT          PIC X(4) VALUE "QUIT".
       01 INPUT1               PIC X(50) VALUE ZEROS.

       01 GRADE0               PIC 9(2) VALUE 7.
       01 GRADE1               PIC 9(2) VALUE 3.
       01 GRADE2               PIC 9(2) VALUE 19.
       01 TEMP                 PIC 9(2).
       PROCEDURE DIVISION.
           PERFORM UNTIL INPUT1 EQUAL STRING_QUIT
               DISPLAY "Type AVERAGE / HIGH / LOW / QUIT."
               ACCEPT INPUT1
               EVALUATE TRUE
                   WHEN INPUT1 EQUAL STRING_HIGH
                       MOVE GRADE0 TO TEMP
                       IF GRADE1 > TEMP THEN
                           MOVE GRADE1 TO TEMP
                       END-IF
                       IF GRADE2 > TEMP THEN
                           MOVE GRADE2 TO TEMP
                       END-IF
                       DISPLAY "The highest grade is " TEMP "."

                   WHEN INPUT1 EQUAL STRING_LOW
                       MOVE GRADE0 TO TEMP
                       IF GRADE1 < TEMP THEN
                           MOVE GRADE1 TO TEMP
                       END-IF
                       IF GRADE2 < TEMP THEN
                           MOVE GRADE2 TO TEMP
                       END-IF
                       DISPLAY "The lowest grade is " TEMP "."

                   WHEN INPUT1 EQUAL STRING_AVERAGE
                       MOVE GRADE0 TO TEMP
                       ADD GRADE1 TO TEMP
                       ADD GRADE2 TO TEMP
                       DIVIDE 3 INTO TEMP
                       DISPLAY "The average grade is " TEMP "."
                       
               END-EVALUATE
           END-PERFORM.
           STOP RUN.
           