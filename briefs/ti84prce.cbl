       IDENTIFICATION DIVISION.
       PROGRAM-ID. ti84prce.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 02-05-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 STRING-QUIT          PIC X(4) VALUE "QUIT".
       01 STRING-CLEAR         PIC X(5) VALUE "CLEAR".
       01 STRING-PLUS          PIC X(1) VALUE "+".
       01 STRING-MINUS         PIC X(1) VALUE "-".
       01 STRING-DIVIDE        PIC X(1) VALUE "/".
       01 STRING-MULTIPLY      PIC X(1) VALUE "*".
       01 STRING-POWER         PIC X(1) VALUE "^".

       01 TEMP1                PIC 9(2).
       01 INPUT-SIZE-MINUS-2   PIC 9(2).
       01 TOTAL                PIC S9(10).
       01 MATH-BUFFER          PIC S9(10).
       01 NUMBER-BUFFER        PIC S9(10).
       01 INPUT1               PIC X(50).
       01 OPERATION            PIC X(1).
       01 ITERATOR             PIC 9(2).
       01 RETURN-ERROR         PIC X VALUE 'N'.
           88 ERROR-FOUND            VALUE 'N'.
           88 NO-ERROR-FOUND         VALUE 'Y'.
       01 CALCULATION-STATUS   PIC X VALUE SPACE.
           88 CALCULATION-IN-PROGRESS VALUE SPACE.
           88 CALCULATION-ERROR       VALUE 'E'.
           88 CALCULATION-SUCCESS     VALUE 'S'.

       01 OUTPUT-NUMBER-BUFFER-SIGNED PIC S9(10).
       01 OUTPUT-NUMBER-BUFFER-ALPHAN PIC Z(9)9.
       PROCEDURE DIVISION.
      * Setting up INPUT-SIZE-MINUS-2 variable.
           MOVE LENGTH OF INPUT1 TO INPUT-SIZE-MINUS-2.
           SUBTRACT 2 FROM INPUT-SIZE-MINUS-2.

           PERFORM UNTIL 1 EQUAL 0
             MOVE TOTAL TO OUTPUT-NUMBER-BUFFER-SIGNED
             DISPLAY "TOTAL : " WITH NO ADVANCING
             PERFORM 0300-DISPLAY-SHORT-NUMBER-START
                THRU 0300-DISPLAY-SHORT-NUMBER-END
             DISPLAY SPACE
             ACCEPT INPUT1
             EVALUATE INPUT1
               WHEN STRING-CLEAR
                 MOVE 0 TO TOTAL

               WHEN STRING-QUIT
                 STOP RUN

               WHEN OTHER
                 PERFORM 0200-GET-NUMBER-START
                    THRU 0200-GET-NUMBER-END
                 IF ERROR-FOUND THEN
                   MOVE TOTAL TO MATH-BUFFER
                 ELSE
                   MOVE NUMBER-BUFFER TO MATH-BUFFER
                 END-IF

                 SET CALCULATION-IN-PROGRESS TO TRUE
                 PERFORM UNTIL NOT CALCULATION-IN-PROGRESS
                   PERFORM 0100-GET-OPERATION-START
                      THRU 0100-GET-OPERATION-END
                   IF NO-ERROR-FOUND THEN
                     PERFORM 0200-GET-NUMBER-START
                        THRU 0200-GET-NUMBER-END
                     IF NO-ERROR-FOUND THEN
                       MOVE MATH-BUFFER TO OUTPUT-NUMBER-BUFFER-SIGNED
                       PERFORM 0300-DISPLAY-SHORT-NUMBER-START
                          THRU 0300-DISPLAY-SHORT-NUMBER-END
                       DISPLAY SPACE OPERATION SPACE WITH NO ADVANCING
                       MOVE NUMBER-BUFFER TO OUTPUT-NUMBER-BUFFER-SIGNED
                       PERFORM 0300-DISPLAY-SHORT-NUMBER-START
                          THRU 0300-DISPLAY-SHORT-NUMBER-END
                       DISPLAY " = " WITH NO ADVANCING
                       EVALUATE OPERATION
                         WHEN STRING-PLUS
                           ADD NUMBER-BUFFER TO MATH-BUFFER
                         WHEN STRING-MINUS
                           SUBTRACT NUMBER-BUFFER FROM MATH-BUFFER
                         WHEN STRING-DIVIDE
                           DIVIDE NUMBER-BUFFER INTO MATH-BUFFER
                         WHEN STRING-MULTIPLY
                           MULTIPLY NUMBER-BUFFER BY MATH-BUFFER
                         WHEN STRING-POWER
                  COMPUTE MATH-BUFFER EQUAL MATH-BUFFER ** NUMBER-BUFFER
                       END-EVALUATE
                       MOVE MATH-BUFFER TO OUTPUT-NUMBER-BUFFER-SIGNED
                       PERFORM 0300-DISPLAY-SHORT-NUMBER-START
                          THRU 0300-DISPLAY-SHORT-NUMBER-END
                       DISPLAY SPACE
                       IF INPUT1 EQUAL SPACES
                         SET CALCULATION-SUCCESS TO TRUE
                       END-IF
                     ELSE
                       SET CALCULATION-ERROR TO TRUE
                       DISPLAY "A number is expected after an operation"
                     END-IF
                   ELSE
                     SET CALCULATION-ERROR TO TRUE
                     DISPLAY "An operation is expected here."
                   END-IF
                 END-PERFORM
                 IF CALCULATION-SUCCESS THEN
                   MOVE MATH-BUFFER TO TOTAL
                 END-IF
             END-EVALUATE
           END-PERFORM.
           STOP RUN.

       0100-GET-OPERATION-START.
           SET NO-ERROR-FOUND TO TRUE.
           MOVE FUNCTION TRIM(INPUT1) TO INPUT1
           MOVE INPUT1(1:1) TO OPERATION.
           IF (NOT (OPERATION EQUAL STRING-PLUS
                OR OPERATION EQUAL STRING-MINUS
                OR OPERATION EQUAL STRING-DIVIDE
                OR OPERATION EQUAL STRING-MULTIPLY
                OR OPERATION EQUAL STRING-POWER))
                OR (INPUT1(2:1) NOT EQUAL SPACE)  THEN
               SET ERROR-FOUND TO TRUE
           ELSE
               MOVE INPUT1(3:INPUT-SIZE-MINUS-2) TO INPUT1
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
           PERFORM UNTIL (NOT
              (INPUT1(ITERATOR:1) >= "0" AND INPUT1(ITERATOR:1) <= "9"))
              OR ITERATOR > LENGTH OF INPUT1
               ADD 1 TO ITERATOR
               SET NO-ERROR-FOUND TO TRUE 
           END-PERFORM.
           SUBTRACT 1 FROM ITERATOR.
           
           IF NO-ERROR-FOUND THEN
               MOVE INPUT1(1:ITERATOR) TO NUMBER-BUFFER
               MOVE ITERATOR TO TEMP1
               ADD 1 TO TEMP1
               MOVE INPUT1(TEMP1:(LENGTH OF INPUT1) - ITERATOR) TO
                    INPUT1
           END-IF.
       0200-GET-NUMBER-END.

       0300-DISPLAY-SHORT-NUMBER-START.
           MOVE OUTPUT-NUMBER-BUFFER-SIGNED TO
                OUTPUT-NUMBER-BUFFER-ALPHAN
           PERFORM VARYING ITERATOR FROM 1 BY 1 UNTIL
               OUTPUT-NUMBER-BUFFER-ALPHAN(ITERATOR:1) NOT EQUAL SPACE
               CONTINUE
           END-PERFORM.
           IF OUTPUT-NUMBER-BUFFER-SIGNED < 0 THEN
           DISPLAY "-" WITH NO ADVANCING
           END-IF.
           DISPLAY OUTPUT-NUMBER-BUFFER-ALPHAN(ITERATOR:
             (LENGTH OF OUTPUT-NUMBER-BUFFER-ALPHAN) - ITERATOR + 1)
             WITH NO ADVANCING.
       0300-DISPLAY-SHORT-NUMBER-END.
       