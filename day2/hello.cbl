       IDENTIFICATION DIVISION.
       PROGRAM-ID. hello.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 23-04-2025 (fr)
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
       01 STR        PIC X(12) VALUE 'Hello World?'.
       01 SURNAME    PIC X(20).
       01 NUM1       PIC 9(2).
       01 NUM2       PIC 9(2).
       01 NUM3       PIC 9(2).
       01 RESULT1    PIC 9(4).
       PROCEDURE DIVISION.
           DISPLAY 'Type in you name :'.
           ACCEPT SURNAME.
           DISPLAY "Ok, can you give me two numbers?".
           ACCEPT NUM1.
           ACCEPT NUM2.
           MULTIPLY NUM1 BY NUM2 GIVING RESULT1.
           DISPLAY NUM1 " times " 
               NUM2 " equals "
               RESULT1 ", isn't it amazing?".
           DISPLAY "Can you give me a third one now?".
           ACCEPT NUM3.
           COMPUTE RESULT1 = (NUM1 + NUM2 + NUM3) / 3.
           DISPLAY "The average of the 3 numbers is " RESULT1 ".".
           STOP RUN.
       
