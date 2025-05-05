       IDENTIFICATION DIVISION.
       PROGRAM-ID. note5ave.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 05-05-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NOTES-TABLE.
           05 NOTES            PIC 9(2) OCCURS 5 TIMES.

       01 TOTAL                PIC 9(3).
       01 IDX                  PIC 9.
       PROCEDURE DIVISION.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 5
               DISPLAY "Enter the " IDX " note : " WITH NO ADVANCING
               ACCEPT NOTES(IDX)
           END-PERFORM.
           MOVE 0 TO TOTAL.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 5
               ADD NOTES(IDX) TO TOTAL
           END-PERFORM.
           DIVIDE 5 INTO TOTAL
           DISPLAY "The average is " TOTAL.
           STOP RUN.
