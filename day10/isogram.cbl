       IDENTIFICATION DIVISION.
       PROGRAM-ID. isogram.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 06-05-2025.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 OCCURENCES-TABLE.
           05 OCCURENCE        PIC 9(03) OCCURS 26 TIMES.

       01 WORD-1               PIC X(20).
       01 IDX                  PIC 9(02).
       01 NUMBER-1             PIC 9(02) VALUE 0.

       PROCEDURE DIVISION.
           DISPLAY "Type a word : "
               WITH NO ADVANCING.
           ACCEPT WORD-1.
           MOVE FUNCTION UPPER-CASE(WORD-1) TO WORD-1.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > LENGTH OF WORD-1
               IF WORD-1(IDX:1) >= "A" AND
                  WORD-1(IDX:1) <= "Z" THEN
                  ADD 1 TO OCCURENCE(FUNCTION ORD(WORD-1(IDX:1)) - 65)
               END-IF
           END-PERFORM.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 26
               IF OCCURENCE(IDX) NOT EQUAL 0 THEN
                  IF NUMBER-1 EQUAL 0 THEN
                      MOVE OCCURENCE(IDX) TO NUMBER-1
                  ELSE 
                     IF NUMBER-1 NOT EQUAL OCCURENCE(IDX) THEN
                         DISPLAY "Not isogram."
                         STOP RUN
                     END-IF
                  END-IF
               END-IF
           END-PERFORM.
           DISPLAY "Isogram"
           STOP RUN.