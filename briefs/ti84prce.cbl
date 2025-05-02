       IDENTIFICATION DIVISION.
       PROGRAM-ID. ti84prce.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 02-05-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 STRING-PLUS          PIC X(1) VALUE "+".
       01 STRING-MINUS         PIC X(1) VALUE "-".
       01 STRING-DIVIDE        PIC X(1) VALUE "/".
       01 STRING-MULTIPLY      PIC X(1) VALUE "*".

       01 TEMP1                PIC 9(2).
       01 INPUT-SIZE-MINUS-1   PIC 9(2).
       01 TOTAL                PIC S9(10).
       01 MATH-BUFFER          PIC S9(10).
       01 NUMBER-BUFFER        PIC S9(10).
       01 INPUT1               PIC X(50).
       01 OPERATION            PIC X(1).
       01 ITERATOR             PIC 9(2).
       01 RETURN-ERROR         PIC X VALUE 'N'.
           88 ERROR-FOUND            VALUE 'N'.
           88 NO-ERROR-FOUND         VALUE 'Y'.
       PROCEDURE DIVISION.
      * Setting up INPUT-SIZE-MINUS-1 variable.
           MOVE LENGTH OF INPUT1 TO INPUT-SIZE-MINUS-1.
           SUBTRACT 1 FROM INPUT-SIZE-MINUS-1.

           PERFORM UNTIL 1 EQUAL 0
               ACCEPT INPUT1
               PERFORM 0100-GET-OPERATION-START
                  THRU 0100-GET-OPERATION-END
               IF ERROR-FOUND THEN
                   PERFORM 0200-GET-NUMBER-START
                      THRU 0200-GET-NUMBER-END
                   IF ERROR-FOUND THEN
                      DISPLAY "A number or operation is expected here."
                   ELSE
                       MOVE NUMBER-BUFFER TO MATH-BUFFER
                       PERFORM 0100-GET-OPERATION-START
                          THRU 0100-GET-OPERATION-END
                       IF ERROR-FOUND THEN
                           DISPLAY 
                      "An operation is expected after the first number."
                       ELSE
                           PERFORM 0200-GET-NUMBER-START
                              THRU 0200-GET-NUMBER-END
                           IF ERROR-FOUND THEN
                               DISPLAY
                             "A number is expected after the operation."
                           ELSE
                             EVALUATE OPERATION
                               WHEN STRING-PLUS
                                 ADD NUMBER-BUFFER TO MATH-BUFFER
                               WHEN STRING-MINUS
                                 SUBTRACT NUMBER-BUFFER FROM MATH-BUFFER
                               WHEN STRING-DIVIDE
                                 DIVIDE NUMBER-BUFFER INTO MATH-BUFFER
                               WHEN STRING-MULTIPLY
                                 MULTIPLY NUMBER-BUFFER BY MATH-BUFFER
                             END-EVALUATE
                             DISPLAY "Result : " MATH-BUFFER
                           END-IF
                       END-IF
                   END-IF
               ELSE
                   MOVE TOTAL TO MATH-BUFFER
                   PERFORM 0200-GET-NUMBER-START
                      THRU 0200-GET-NUMBER-END
               END-IF
           END-PERFORM.
           STOP RUN.

       0100-GET-OPERATION-START.
           SET NO-ERROR-FOUND TO TRUE.
           MOVE FUNCTION TRIM(INPUT1) TO INPUT1
           MOVE INPUT1(1:1) TO OPERATION.
           DISPLAY "Operation start " INPUT1 " / " OPERATION
           IF NOT (OPERATION EQUAL STRING-PLUS
                OR OPERATION EQUAL STRING-MINUS
                OR OPERATION EQUAL STRING-DIVIDE
                OR OPERATION EQUAL STRING-MULTIPLY) THEN
               SET ERROR-FOUND TO TRUE
           ELSE
               MOVE INPUT1(2:INPUT-SIZE-MINUS-1) TO INPUT1
           END-IF.
       0100-GET-OPERATION-END.

       0200-GET-NUMBER-START.
           SET ERROR-FOUND TO TRUE.
           MOVE FUNCTION TRIM(INPUT1) TO INPUT1.
           MOVE 1 TO ITERATOR.
           IF (INPUT1(ITERATOR:1) EQUAL "+" OR 
               INPUT1(ITERATOR:1) EQUAL "-")
               MOVE 2 TO ITERATOR
           END-IF.
           DISPLAY "Number start " INPUT1 " / " ITERATOR
           PERFORM UNTIL (NOT
              (INPUT1(ITERATOR:1) >= "0" AND INPUT1(ITERATOR:1) <= "9"))
              OR ITERATOR > LENGTH OF INPUT1
               ADD 1 TO ITERATOR
               SET NO-ERROR-FOUND TO TRUE 
           END-PERFORM.
           SUBTRACT 1 FROM ITERATOR.
           IF NO-ERROR-FOUND THEN
               DISPLAY  "End number : TRUE / " ITERATOR
           ELSE
               DISPLAY  "End number : FALSE / " ITERATOR " / " 
           END-IF
           
           IF NO-ERROR-FOUND THEN
               MOVE INPUT1(1:ITERATOR) TO NUMBER-BUFFER
               MOVE ITERATOR TO TEMP1
               ADD 1 TO TEMP1
               MOVE INPUT1(TEMP1:(LENGTH OF INPUT1) - ITERATOR) TO
                    INPUT1
           END-IF.
       0200-GET-NUMBER-END.