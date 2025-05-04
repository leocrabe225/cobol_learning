       IDENTIFICATION DIVISION.
       PROGRAM-ID. ti84prce.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 02-05-2025 (fr).
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
       01 STRING-POWER         PIC X(1) VALUE "^".
       01 INPUT-SIZE-MINUS-2   PIC 9(2).

       01 TEMP1                PIC 9(2).
      * Stores to total between calculations
       01 TOTAL                PIC S9(10).
      * Stores the current state of the calculation
       01 MATH-BUFFER          PIC S9(10).
      * Stores the next number to use for an operation
       01 NUMBER-BUFFER        PIC S9(10).
       01 INPUT1               PIC X(50).
      * Stores the next operation to perform
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

      * Infinite loop
           PERFORM UNTIL 1 EQUAL 0
      * Displaying previous total
             MOVE TOTAL TO OUTPUT-NUMBER-BUFFER-SIGNED
             DISPLAY "TOTAL : " WITH NO ADVANCING
             PERFORM 0300-DISPLAY-SHORT-NUMBER-START
                THRU 0300-DISPLAY-SHORT-NUMBER-END
      * Line break
             DISPLAY SPACE

             ACCEPT INPUT1
             EVALUATE INPUT1
      * Clears total on CLEAR
               WHEN STRING-CLEAR
                 MOVE 0 TO TOTAL
      * Quit the program on QUIT
               WHEN STRING-QUIT
                 STOP RUN

      * Tries to read a calculation if no command is used
               WHEN OTHER
      * If no number at the start, we use the previous total
                 PERFORM 0200-GET-NUMBER-START
                    THRU 0200-GET-NUMBER-END
                 IF ERROR-FOUND THEN
                   MOVE TOTAL TO MATH-BUFFER
                 ELSE
                   MOVE NUMBER-BUFFER TO MATH-BUFFER
                 END-IF

                 SET CALCULATION-IN-PROGRESS TO TRUE
      * Once we have the first number, we loop until the input is empty
      * or something goes wrong
                 PERFORM UNTIL NOT CALCULATION-IN-PROGRESS
      * Gets the next operation
                   PERFORM 0100-GET-OPERATION-START
                      THRU 0100-GET-OPERATION-END
                   IF NO-ERROR-FOUND THEN
      * Gets the next number
                     PERFORM 0200-GET-NUMBER-START
                        THRU 0200-GET-NUMBER-END
                     IF NO-ERROR-FOUND THEN
      * Displays each step of the calculation properly formatted
                       MOVE MATH-BUFFER TO OUTPUT-NUMBER-BUFFER-SIGNED
                       PERFORM 0300-DISPLAY-SHORT-NUMBER-START
                          THRU 0300-DISPLAY-SHORT-NUMBER-END
                       DISPLAY SPACE OPERATION SPACE WITH NO ADVANCING
                       MOVE NUMBER-BUFFER TO OUTPUT-NUMBER-BUFFER-SIGNED
                       PERFORM 0300-DISPLAY-SHORT-NUMBER-START
                          THRU 0300-DISPLAY-SHORT-NUMBER-END
                       DISPLAY " = " WITH NO ADVANCING

      * Executes the step depending on the operation
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

      * Finishes to display the step
                       MOVE MATH-BUFFER TO OUTPUT-NUMBER-BUFFER-SIGNED
                       PERFORM 0300-DISPLAY-SHORT-NUMBER-START
                          THRU 0300-DISPLAY-SHORT-NUMBER-END
      * Line break
                       DISPLAY SPACE
      * Checks if the input is empty
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
      * Total is only updated if everything went smoothly
                 IF CALCULATION-SUCCESS THEN
                   MOVE MATH-BUFFER TO TOTAL
                 END-IF
             END-EVALUATE
           END-PERFORM.
           STOP RUN.

      * Trimms the leading spaces of the input, then gets the next
      * operation, removes the used part of the string if found
      * sets error-found to true if not found
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

      * Trimms the leading spaces of the input, then gets the next
      * number, can be "T " if the user uses the previous total,
      * removes the used part of the string if found, sets error-found
      * to true if not found.
       0200-GET-NUMBER-START.
           SET ERROR-FOUND TO TRUE.
           MOVE FUNCTION TRIM(INPUT1) TO INPUT1.
           MOVE 1 TO ITERATOR.
           IF (INPUT1(1:2) EQUAL "T ") THEN
               MOVE TOTAL TO NUMBER-BUFFER
               SET NO-ERROR-FOUND TO TRUE
               MOVE INPUT1(2:(LENGTH OF INPUT1) - 2) TO INPUT1
           ELSE
               IF (INPUT1(ITERATOR:1) EQUAL "+" OR 
                   INPUT1(ITERATOR:1) EQUAL "-")
                   MOVE 2 TO ITERATOR
               END-IF
               PERFORM UNTIL (NOT
                  (INPUT1(ITERATOR:1) >= "0"   AND 
                   INPUT1(ITERATOR:1) <= "9")) OR
                   ITERATOR > LENGTH OF INPUT1
                   ADD 1 TO ITERATOR
                   SET NO-ERROR-FOUND TO TRUE 
               END-PERFORM
               SUBTRACT 1 FROM ITERATOR
               
               IF NO-ERROR-FOUND THEN
                   MOVE INPUT1(1:ITERATOR) TO NUMBER-BUFFER
                   MOVE ITERATOR TO TEMP1
                   ADD 1 TO TEMP1
                   MOVE INPUT1(TEMP1:(LENGTH OF INPUT1) - ITERATOR) TO
                        INPUT1
               END-IF
           END-IF.
       0200-GET-NUMBER-END.

      * Display the number stored in OUTPUT-NUMBER-BUFFER-SIGNED
      * without leading zeros, and without a line break
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
       