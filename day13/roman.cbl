       IDENTIFICATION DIVISION.
       PROGRAM-ID. roman.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 13-05-2025.
       DATE-COMPILED. null.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-KEY-VALUE-TABLE VALUE
           "1000M 0900CM0500D 0400CD0100C 0090XC0050L 0040XL0010X 0009IX
      -    "0005V 0004IV0001I ".
           05 WS-KEY-VALUE           OCCURS 13 TIMES.
               10 WS-KEY       PIC 9(04).
               10 WS-VALUE     PIC X(02).

       
       77 WS-INPUT             PIC 9(04) VALUE 0.
       77 WS-IDX               PIC 9(02) VALUE 1.
       PROCEDURE DIVISION.
           PERFORM UNTIL WS-INPUT > 0 AND
                         WS-INPUT < 4000
               DISPLAY "Enter a number between 1 and 3999 : "
                   WITH NO ADVANCING
               ACCEPT WS-INPUT
               IF NOT (WS-INPUT > 0 AND WS-INPUT < 4000) THEN
                   DISPLAY WS-INPUT " is not a valid input."
               END-IF
           END-PERFORM.
           
           PERFORM UNTIL WS-INPUT EQUAL 0
               IF WS-KEY(WS-IDX) <= WS-INPUT THEN
                   SUBTRACT WS-KEY(WS-IDX) FROM WS-INPUT
                   IF WS-VALUE(WS-IDX)(2:1) EQUAL SPACE THEN
                       DISPLAY WS-VALUE(WS-IDX)(1:1)
                           WITH NO ADVANCING
                   ELSE
                       DISPLAY WS-VALUE(WS-IDX)
                           WITH NO ADVANCING
                   END-IF
               ELSE
                   ADD 1 TO WS-IDX
               END-IF
           END-PERFORM.
           DISPLAY SPACE.
           STOP RUN.
           