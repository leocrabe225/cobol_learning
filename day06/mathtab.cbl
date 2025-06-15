       IDENTIFICATION DIVISION.
       PROGRAM-ID. mathtab.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 29-04-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ITERATOR             PIC 9(2).
       01 INPUT1               PIC 9(2).
       01 RESULT1              PIC 9(3).
       PROCEDURE DIVISION.
           DISPLAY "Enter a number from 1 to 10.".
           ACCEPT INPUT1.
           PERFORM VARYING ITERATOR FROM 1 BY 1 UNTIL ITERATOR > 10
               MULTIPLY ITERATOR BY INPUT1 GIVING RESULT1
               DISPLAY INPUT1 "*" ITERATOR "=" RESULT1
           END-PERFORM.
           STOP RUN.
           