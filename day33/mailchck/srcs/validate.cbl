       IDENTIFICATION DIVISION.
       PROGRAM-ID. validate.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 14-06-2025 (fr).

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-AMOUNT             PIC 9(02).
       LINKAGE SECTION.
       01 LK-USER-ID            PIC X(10).
       01 LK-USER-NAME          PIC X(50).
       01 LK-USER-EMAIL         PIC X(50).
       COPY retstatu REPLACING ==:PREFIX:== BY ==LK==.
       
       PROCEDURE DIVISION USING 
                                LK-USER-ID,
                                LK-USER-NAME,
                                LK-USER-EMAIL,
                                LK-RETURN-VALUE.

           SET LK-RETURN-OK TO TRUE.

           PERFORM 0100-VALIDATE-ID-BEGIN
              THRU 0100-VALIDATE-ID-END.

           PERFORM 0200-VALIDATE-EMAIL-BEGIN
              THRU 0200-VALIDATE-EMAIL-END.

           EXIT PROGRAM.

       0100-VALIDATE-ID-BEGIN.
           IF NOT (LK-USER-ID IS NUMERIC) THEN
               SET LK-RETURN-ID-FORMAT-ERROR TO TRUE
               EXIT PROGRAM
           END-IF.
       0100-VALIDATE-ID-END.

       0200-VALIDATE-EMAIL-BEGIN.
           PERFORM 0300-CHECK-FOR-AT-BEGIN
              THRU 0300-CHECK-FOR-AT-END.

           PERFORM 0400-CHECK-FOR-DOT-BEGIN
              THRU 0400-CHECK-FOR-DOT-END.
       0200-VALIDATE-EMAIL-END.

       0300-CHECK-FOR-AT-BEGIN.
           MOVE 0 TO WS-AMOUNT.
           INSPECT LK-USER-EMAIL TALLYING
               WS-AMOUNT FOR ALL "@".

           EVALUATE WS-AMOUNT
               WHEN 0
                   SET LK-RETURN-EMAIL-NO-AT TO TRUE
                   EXIT PROGRAM
               
               WHEN 1
                   SET LK-RETURN-OK TO TRUE

               WHEN OTHER
                   SET LK-RETURN-EMAIL-MANY-AT TO TRUE
                   EXIT PROGRAM
           END-EVALUATE.

       0300-CHECK-FOR-AT-END.

       0400-CHECK-FOR-DOT-BEGIN.
           MOVE 0 TO WS-AMOUNT.
           INSPECT LK-USER-EMAIL TALLYING
               WS-AMOUNT FOR ALL ".".

           IF WS-AMOUNT EQUAL 0 THEN
               SET LK-RETURN-EMAIL-NO-DOT TO TRUE
               EXIT PROGRAM
           END-IF.
       0400-CHECK-FOR-DOT-END.
