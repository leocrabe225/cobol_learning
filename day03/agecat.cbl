       IDENTIFICATION DIVISION.
       PROGRAM-ID. agecat.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 24-04-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
       01 AGE PIC 9(3).
       PROCEDURE DIVISION.
           DISPLAY "Enter your age if that's not intrusive of course!".
           ACCEPT AGE 
               ON EXCEPTION 
                   DISPLAY "You entered something invalid"
           END-ACCEPT.
           IF AGE GREATER THAN 120 THEN
               DISPLAY "I think you are lying"
           ELSE IF AGE GREATER THAN 60 THEN
               DISPLAY "Hello senior type people"
           ELSE IF AGE GREATER THAN 18 THEN
               DISPLAY "Hello adult type people"
           ELSE IF AGE GREATER THAN 12 THEN
               DISPLAY "Hello teenager type people"
           ELSE 
               DISPLAY "Hello youngling"
           END-IF
           END-IF
           END-IF
           END-IF.
           STOP RUN.
