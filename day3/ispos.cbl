       IDENTIFICATION DIVISION.
       PROGRAM-ID. ispos.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 24-04-2025 (fr)
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
       01 INPUT-NBR-1              PIC S9(3).
       PROCEDURE DIVISION.
           DISPLAY "Enter a 2 digits number".
           ACCEPT INPUT-NBR-1.
           IF INPUT-NBR-1 LESS THAN 0 THEN
               DISPLAY "Number " INPUT-NBR-1 " is negative"
           ELSE
               DISPLAY "Number " INPUT-NBR-1 " is positive"
           END-IF.
           STOP RUN.
