       IDENTIFICATION DIVISION.
       PROGRAM-ID. twoqueen.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 13-05-2025.
       DATE-COMPILED. null.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CHESS-BOARD.
           05 FILLER           PIC X(10) VALUE " abcdefgh ".
           05 FILLER           PIC X(01) VALUE  X"0A".
           05 WS-LINES OCCURS 8 TIMES.
               10 WS-NUM-BEGIN PIC 9(01).
               10 WS-CELL      PIC X(01) VALUE "-" OCCURS 8 TIMES.
               10 WS-NUM-END   PIC 9(01).
               10 FILLER       PIC X(01) VALUE X"0A".
           05 FILLER           PIC X(10) VALUE " abcdefgh ".
       
       01 WS-ERROR             PIC 9(01) VALUE 0.
           88 WS-ERROR-TRUE              VALUE 1.
           88 WS-ERROR-FALSE             VALUE 0.

       77 WS-START-X           PIC 9(01).
       77 WS-START-Y           PIC 9(01).
       77 WS-IDX               PIC 9(01).
       77 WS-IDX-2             PIC 9(01).
       77 WS-INC-X             PIC S9(01).
       77 WS-INC-Y             PIC S9(01).
       77 WS-INPUT             PIC X(02).
       77 WS-MATH-BUFFER       PIC 9(03).
       PROCEDURE DIVISION.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 8
               COMPUTE WS-NUM-BEGIN(WS-IDX) EQUAL 9 - WS-IDX
               COMPUTE WS-NUM-END(WS-IDX) EQUAL 9 - WS-IDX
           END-PERFORM.
           DISPLAY WS-CHESS-BOARD.
           DISPLAY "Enter the first queen position (a-h, 1-8) : "
               WITH NO ADVANCING.
           PERFORM 0100-ACCEPT-INPUT-BEGIN
              THRU 0100-ACCEPT-INPUT-END.
           MOVE "Q" TO WS-CELL(WS-IDX-2, WS-IDX).
           DISPLAY "Enter the second queen position (a-h, 1-8) : "
               WITH NO ADVANCING.
           PERFORM 0100-ACCEPT-INPUT-BEGIN
              THRU 0100-ACCEPT-INPUT-END.
           MOVE "q" TO WS-CELL(WS-IDX-2, WS-IDX).
           DISPLAY WS-CHESS-BOARD.
      
           MOVE WS-IDX-2 TO WS-START-Y.
           MOVE WS-IDX   TO WS-START-X.

           PERFORM VARYING WS-INC-Y FROM -1 BY 1 UNTIL WS-INC-Y > 1
               PERFORM VARYING WS-INC-X FROM -1 BY 1 UNTIL WS-INC-X > 1
                   IF WS-INC-X NOT EQUAL 0 OR WS-INC-Y NOT EQUAL 0 THEN
                       MOVE WS-START-Y TO WS-IDX-2
                       MOVE WS-START-X TO WS-IDX
                       PERFORM UNTIL WS-IDX-2 > 8 OR WS-IDX-2 EQUAL 0
                                  OR WS-IDX > 8 OR WS-IDX EQUAL 0
                           IF WS-CELL(WS-IDX-2,WS-IDX) EQUAL "Q"
                               DISPLAY "Can hit!"
                               STOP RUN
                           END-IF
                           ADD WS-INC-X TO WS-IDX
                           ADD WS-INC-Y TO WS-IDX-2
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-PERFORM.
           DISPLAY "Cannot hit".
           STOP RUN.


       0100-ACCEPT-INPUT-BEGIN.
           SET WS-ERROR-TRUE TO TRUE.
           PERFORM UNTIL WS-ERROR-FALSE
               ACCEPT WS-INPUT
               PERFORM 0200-READ-INPUT-BEGIN
                  THRU 0200-READ-INPUT-END
               IF WS-ERROR-TRUE THEN
                   DISPLAY "This is not a valid position : "
                       WITH NO ADVANCING
               END-IF
           END-PERFORM.
       0100-ACCEPT-INPUT-END.

       0200-READ-INPUT-BEGIN.
           SET WS-ERROR-FALSE TO TRUE.

           IF WS-INPUT(1:1) >= "a" AND WS-INPUT(1:1) <= "h" THEN
               MOVE FUNCTION ORD(WS-INPUT(1:1)) TO WS-MATH-BUFFER
               SUBTRACT 97 FROM WS-MATH-BUFFER
               MOVE WS-MATH-BUFFER TO WS-IDX
           ELSE
               SET WS-ERROR-TRUE TO TRUE
           END-IF.

           IF WS-INPUT(2:1) >= "1" AND WS-INPUT(2:1) <= "8" THEN
               MOVE WS-INPUT(2:1) TO WS-IDX-2
               SUBTRACT WS-IDX-2 FROM 9 GIVING WS-IDX-2
           ELSE
               SET WS-ERROR-TRUE TO TRUE
           END-IF.
       0200-READ-INPUT-END.
