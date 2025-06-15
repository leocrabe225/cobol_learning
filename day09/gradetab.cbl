       IDENTIFICATION DIVISION.
       PROGRAM-ID. gradetab.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 05-05-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 STRING-INPUT         PIC X(5) VALUE "INPUT".
       01 STRING-AVERAGE       PIC X(7) VALUE "AVERAGE".
       01 STRING-MAX           PIC X(3) VALUE "MAX".
       01 STRING-MIN           PIC X(3) VALUE "MIN".
       01 STRING-QUIT          PIC X(4) VALUE "QUIT".

       01 GRADE-TABLE.
          05 GRADE             PIC 9(2) OCCURS 10 TIMES.

       01 INPUT-1              PIC X(20) VALUE SPACE.
       01 SUM-BUFFER           PIC 9(3).
       01 NUMBER-BUFFER        PIC 9(2).
       01 AVERAGE              PIC 99V99.
       01 MAX-1                PIC 9(2).
       01 MIN-1                PIC 9(2).
       01 IDX-1                PIC 9(2).
       PROCEDURE DIVISION.
           PERFORM UNTIL INPUT-1 EQUAL STRING-QUIT
               DISPLAY STRING-INPUT " / "
                       STRING-AVERAGE " / "
                       STRING-MAX " / "
                       STRING-MIN " / "
                       STRING-QUIT "."
               ACCEPT INPUT-1 
               EVALUATE INPUT-1
                   WHEN STRING-INPUT
                      MOVE 0 TO SUM-BUFFER
                      MOVE 20 TO MIN-1
                      MOVE 0 TO MAX-1
                      PERFORM VARYING IDX-1 FROM 1 BY 1 UNTIL IDX-1 > 10
                          MOVE 99 TO NUMBER-BUFFER
                          PERFORM UNTIL NUMBER-BUFFER <= 20
                              DISPLAY "Enter grade " IDX-1 " : "
                              WITH NO ADVANCING
                              ACCEPT NUMBER-BUFFER
                              IF NUMBER-BUFFER > 20 
                                  DISPLAY
                                      "The grade must be between 0 & 20"
                              END-IF
                          END-PERFORM
                          MOVE NUMBER-BUFFER TO GRADE(IDX-1)
                          ADD NUMBER-BUFFER TO SUM-BUFFER
                          IF NUMBER-BUFFER > MAX-1 THEN
                              MOVE NUMBER-BUFFER TO MAX-1
                          END-IF
                          IF NUMBER-BUFFER < MIN-1 THEN
                              MOVE NUMBER-BUFFER TO MIN-1
                          END-IF
                      END-PERFORM
                      DIVIDE SUM-BUFFER BY 10 GIVING AVERAGE

                   WHEN STRING-AVERAGE
                       DISPLAY "Average : " AVERAGE

                   WHEN STRING-MAX
                       DISPLAY "Maximum grade " MAX-1

                   WHEN STRING-MIN
                       DISPLAY "Minimum grade " MIN-1
               END-EVALUATE
           END-PERFORM.
           STOP RUN.
