       IDENTIFICATION DIVISION.
       PROGRAM-ID. mailchck.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 14-06-2025 (fr).

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-USER-INPUT
               ASSIGN TO "input/users.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-F-STATUS.

           SELECT F-LOG-OUTPUT
               ASSIGN TO "output/errors.log"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-F-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD F-USER-INPUT.
       COPY usrrcd.

       FD F-LOG-OUTPUT.
       01 F-LOG-RCD            PIC X(120).

       WORKING-STORAGE SECTION.
       77 WS-USER-TBL-SIZE     PIC 9(02).
       01 WS-USER-TBL OCCURS 10 TIMES.
           05 WS-USER-ID       PIC X(10).
           05 WS-USER-NAME     PIC X(50).
           05 WS-USER-EMAIL    PIC X(50).
           05 WS-ERROR         PIC 9(02).

       77 WS-IDX               PIC 9(02).

       COPY retstatu REPLACING ==:PREFIX:== BY ==WS==.
       
       01 WS-F-STATUS          PIC X(02).
           88 WS-F-STATUS-OK             VALUE "00".
           88 WS-F-STATUS-EOF            VALUE "10".

       PROCEDURE DIVISION.

           PERFORM 0100-READ-FILE-BEGIN
              THRU 0100-READ-FILE-END.

           PERFORM 0300-VALIDATE-USERS-BEGIN
              THRU 0300-VALIDATE-USERS-END.

           PERFORM 0400-LOG-ERRORS-BEGIN
              THRU 0400-LOG-ERRORS-END.

           STOP RUN.

       0100-READ-FILE-BEGIN.
           MOVE 0 TO WS-IDX.

           OPEN INPUT F-USER-INPUT.
           PERFORM UNTIL WS-F-STATUS-EOF
               READ F-USER-INPUT
                   NOT AT END
                       ADD 1 TO WS-IDX
                       PERFORM 0200-RCD-TO-TBL-BEGIN
                          THRU 0200-RCD-TO-TBL-END
               END-READ
           END-PERFORM.
           CLOSE F-USER-INPUT.

           MOVE WS-IDX TO WS-USER-TBL-SIZE.
       0100-READ-FILE-END.

       0200-RCD-TO-TBL-BEGIN.
           MOVE F-USER-ID    TO WS-USER-ID(WS-IDX).
           MOVE F-USER-NAME  TO WS-USER-NAME(WS-IDX).
           MOVE F-USER-EMAIL TO WS-USER-EMAIL(WS-IDX).
       0200-RCD-TO-TBL-END.

       0300-VALIDATE-USERS-BEGIN.
           PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > WS-USER-TBL-SIZE
               CALL "validate" USING
                                 WS-USER-ID(WS-IDX)
                                 WS-USER-NAME(WS-IDX)
                                 WS-USER-EMAIL(WS-IDX)
                                 WS-RETURN-VALUE
               END-CALL
               MOVE WS-RETURN-VALUE TO WS-ERROR(WS-IDX)
           END-PERFORM.
       0300-VALIDATE-USERS-END.

       0400-LOG-ERRORS-BEGIN.
           OPEN OUTPUT F-LOG-OUTPUT.
           
           PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > WS-USER-TBL-SIZE
               PERFORM 0500-LOG-EVALUATE-BEGIN
                  THRU 0500-LOG-EVALUATE-END
           END-PERFORM.

           CLOSE F-LOG-OUTPUT.
       0400-LOG-ERRORS-END.

       0500-LOG-EVALUATE-BEGIN.
           MOVE WS-ERROR(WS-IDX) TO WS-RETURN-VALUE.
           MOVE SPACE TO F-LOG-RCD

           EVALUATE TRUE
               WHEN WS-RETURN-ID-FORMAT-ERROR
                   STRING
                       "[Line " WS-IDX "] Error, invalid ID : " QUOTES
                       WS-USER-ID(WS-IDX) QUOTES
                       DELIMITED BY SIZE
                       INTO F-LOG-RCD
                   END-STRING
                   WRITE F-LOG-RCD
               WHEN WS-RETURN-EMAIL-NO-AT
                   STRING
                       "[Line " WS-IDX 
                       "] Error, invalid email, no " QUOTES "@" QUOTES
                       " found : " QUOTES
                       FUNCTION TRIM(WS-USER-EMAIL(WS-IDX)) QUOTES
                       DELIMITED BY SIZE
                       INTO F-LOG-RCD
                   END-STRING
                   WRITE F-LOG-RCD
               WHEN WS-RETURN-EMAIL-MANY-AT
                   STRING
                       "[Line " WS-IDX 
                       "] Error, invalid email, too many "
                       QUOTES "@" QUOTES " found : " QUOTES
                       FUNCTION TRIM(WS-USER-EMAIL(WS-IDX)) QUOTES
                       DELIMITED BY SIZE
                       INTO F-LOG-RCD
                   END-STRING
                   WRITE F-LOG-RCD
               WHEN WS-RETURN-EMAIL-NO-DOT
                  STRING
                       "[Line " WS-IDX 
                       "] Error, invalid email, no " QUOTES "." QUOTES
                       " found : " QUOTES
                       FUNCTION TRIM(WS-USER-EMAIL(WS-IDX)) QUOTES
                       DELIMITED BY SIZE
                       INTO F-LOG-RCD
                   END-STRING
                   WRITE F-LOG-RCD
           END-EVALUATE.
       0500-LOG-EVALUATE-END.