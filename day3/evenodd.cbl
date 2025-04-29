       IDENTIFICATION DIVISION.
       PROGRAM-ID. evenodd.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 24-04-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
       01 INPUT-NBR-1              PIC S9(3).
       01 TRASH                    PIC 9(1).
       01 REMAINDER1               PIC 9(1).
       PROCEDURE DIVISION.
           DISPLAY "Enter a number".
           ACCEPT INPUT-NBR-1.
           DIVIDE INPUT-NBR-1 BY 2 GIVING TRASH REMAINDER REMAINDER1.
           IF REMAINDER1 EQUAL 0 THEN
               DISPLAY INPUT-NBR-1 " IS EVEN."
           ELSE
               DISPLAY INPUT-NBR-1 " IS ODD."
           END-IF.
           STOP RUN.
