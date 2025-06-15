       IDENTIFICATION DIVISION.
       PROGRAM-ID. fizzbuzz.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 24-04-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
       01 ITERATOR                 PIC 9(3) VALUE 0.
       01 FIZZ                     PIC X(4) VALUE "FIZZ".
       01 BUZZ                     PIC X(4) VALUE "BUZZ".
       01 OUTPUT1                  PIC X(8).
       01 TRASH                    PIC 9(3).
       01 REMAINDER1               PIC 9(1).
       01 COUNT1                    PIC 9(1).
       PROCEDURE DIVISION.
       LOOP-START.
           MOVE 1 to COUNT1.
           MOVE " " TO OUTPUT1.
           DIVIDE ITERATOR BY 3 GIVING TRASH REMAINDER REMAINDER1.
           IF REMAINDER1 EQUAL 0 THEN
               STRING FIZZ DELIMITED BY SPACE 
                   INTO OUTPUT1 
                   WITH POINTER COUNT1
               END-STRING
           END-IF.
           DIVIDE ITERATOR BY 5 GIVING TRASH REMAINDER REMAINDER1.
           IF REMAINDER1 EQUAL 0 THEN
               STRING BUZZ DELIMITED BY SPACE 
                   INTO OUTPUT1 
                   WITH POINTER COUNT1
               END-STRING
           END-IF.
           DISPLAY ITERATOR SPACE OUTPUT1.
           ADD 1 TO ITERATOR.
           IF ITERATOR <= 100 THEN
               GO TO LOOP-START
           END-IF
           STOP RUN.

       