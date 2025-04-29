       IDENTIFICATION DIVISION.
       PROGRAM-ID. mult3.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 29-04-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ITERATOR             PIC 9(3).
       01 TRASH                PIC 9(3).
       01 REMAINDER1           PIC 9(1).
       PROCEDURE DIVISION.
           PERFORM VARYING ITERATOR FROM 1 BY 1 UNTIL ITERATOR > 100
               DIVIDE ITERATOR BY 3 GIVING TRASH REMAINDER REMAINDER1
               IF REMAINDER1 EQUAL ZERO THEN
                   DISPLAY ITERATOR
               END-IF
           END-PERFORM.
           STOP RUN.
           