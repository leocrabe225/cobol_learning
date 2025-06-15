       IDENTIFICATION DIVISION.
       PROGRAM-ID. ti83prce.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 30-04-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Constants
       01 STRING-QUIT          PIC X(4) VALUE "QUIT".
       01 STRING-CLEAR         PIC X(5) VALUE "CLEAR".
       01 STRING-PLUS          PIC X(1) VALUE "+".
       01 STRING-MINUS         PIC X(1) VALUE "-".
       01 STRING-DIVIDE        PIC X(1) VALUE "/".
       01 STRING-MULTIPLY      PIC X(1) VALUE "*".
      
       01 INPUT1               PIC X(50) VALUE ZEROS.
       01 NUM-INPUT1           PIC S9(10).
       
       01 TOTAL                PIC S9(10) VALUE ZERO.
       PROCEDURE DIVISION.
      * Loop unless input is "QUIT"
           PERFORM UNTIL INPUT1 EQUAL STRING-QUIT
      * Display possible instructions, then total
               DISPLAY "QUIT CLEAR + - / *" 
                       X'0A' TOTAL
               ACCEPT INPUT1
               EVALUATE TRUE
                   WHEN INPUT1 EQUAL STRING-CLEAR
                       MOVE 0 TO TOTAL
                   
      * If there is a valid math instruction, ask for a number
                   WHEN INPUT1 EQUAL STRING-PLUS
                     OR INPUT1 EQUAL STRING-MINUS
                     OR INPUT1 EQUAL STRING-DIVIDE
                     OR INPUT1 EQUAL STRING-MULTIPLY
                       ACCEPT NUM-INPUT1
                       IF INPUT1 EQUAL STRING-PLUS THEN
                           ADD NUM-INPUT1 TO TOTAL
                       END-IF
                       IF INPUT1 EQUAL STRING-MINUS THEN
                           SUBTRACT NUM-INPUT1 FROM TOTAL
                       END-IF
                       IF INPUT1 EQUAL STRING-DIVIDE THEN
                           DIVIDE NUM-INPUT1 INTO TOTAL
                       END-IF
                       IF INPUT1 EQUAL STRING-MULTIPLY THEN
                           MULTIPLY NUM-INPUT1 BY TOTAL
                       END-IF
               END-EVALUATE
           END-PERFORM.
           STOP RUN.
           