       IDENTIFICATION DIVISION.
       PROGRAM-ID. hello.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 25-04-2025 (fr)
       DATE-COMPILED. null
       ENVIRONMENT DIVISION.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
       01 NUM1                     PIC S9(2).
       01 NUM2                     PIC S9(2).
       01 RESULT1                  PIC S9(2).
       01 RESULT2                  PIC S9(2).
       01 RESULT3                  PIC S9(2).
       PROCEDURE DIVISION.
           DISPLAY "Input one number.".
           ACCEPT NUM1.
           DISPLAY "Another one.".
           ACCEPT NUM2.
           ADD NUM1 TO NUM2 GIVING RESULT1.
           SUBTRACT NUM2 FROM NUM1 GIVING RESULT2.
           SUBTRACT NUM1 FROM NUM2 GIVING RESULT3.
           DISPLAY NUM1 " + " NUM2 " = " RESULT1.
           DISPLAY NUM1 " - " NUM2 " = " RESULT2.
           DISPLAY NUM2 " - " NUM1 " = " RESULT3.
           