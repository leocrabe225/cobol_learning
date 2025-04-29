       IDENTIFICATION DIVISION.
       PROGRAM-ID. hello.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 23-04-2025 (fr)
       DATE-COMPILED. null
       ENVIRONMENT DIVISION.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
       01 DATE1      PIC 9(8).
       01 BIRTH-DATE PIC 9(8).
       01 AGE        PIC 9(4).
       PROCEDURE DIVISION.
           SET DATE1 TO FUNCTION CURRENT-DATE(1:8).
           DISPLAY "Enter your birth date (format : YYYYMMDD)".
           ACCEPT BIRTH-DATE.
           COMPUTE AGE = (DATE1 - BIRTH-DATE)/10000.
           DISPLAY "You are " AGE " years old.".
           DISPLAY AGE.
           STOP RUN.
