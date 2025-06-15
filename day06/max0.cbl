       IDENTIFICATION DIVISION.
       PROGRAM-ID. max0.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 29-04-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 INPUT1               PIC 9(2) VALUE 1.
       01 MAX1                 PIC 9(2) VALUE 0.
       PROCEDURE DIVISION.
           PERFORM UNTIL INPUT1 EQUAL 0
               DISPLAY "Enter a number from 0 to 99."
               ACCEPT INPUT1
               IF MAX1 < INPUT1 THEN
                   MOVE INPUT1 TO MAX1
               END-IF
           END-PERFORM.
           DISPLAY "The max was " MAX1 ".".
           STOP RUN.
           