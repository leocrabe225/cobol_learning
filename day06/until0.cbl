       IDENTIFICATION DIVISION.
       PROGRAM-ID. until0.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 29-04-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 INPUT1               PIC 9(3) VALUE 1.
       PROCEDURE DIVISION.
           PERFORM UNTIL INPUT1 EQUAL 0
               DISPLAY "Enter 0 to stop, or anything else to continue"
               ACCEPT INPUT1
           END-PERFORM.
           STOP RUN.
           