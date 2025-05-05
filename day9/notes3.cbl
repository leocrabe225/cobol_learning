       IDENTIFICATION DIVISION.
       PROGRAM-ID. notes3.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 05-05-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NOTES-TABLE.
           05 NOTES            PIC 9(2) OCCURS 3 TIMES.

       01 IDX                  PIC 9.
       PROCEDURE DIVISION.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 3
               DISPLAY "Enter the " IDX " note : " WITH NO ADVANCING
               ACCEPT NOTES(IDX)
           END-PERFORM.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 3
               DISPLAY "Note " IDX " " NOTES(IDX)
           END-PERFORM.
           STOP RUN.
