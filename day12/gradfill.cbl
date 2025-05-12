       IDENTIFICATION DIVISION.
       PROGRAM-ID. gradfill.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 12-05-2025 (fr).
       DATE-COMPILED. null.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GRADE-DATE-OUTPUT OCCURS 100 TIMES.
           05 FILLER           PIC X(07) VALUE "Note : ".
           05 WS-GRADE         PIC 9(02).
           05 FILLER           PIC X(04) VALUE " le ".
           05 WS-DAY           PIC X(02).
           05 FILLER           PIC X(01) VALUE "/".
           05 WS-MONTH         PIC X(02).
           05 FILLER           PIC X(01) VALUE "/".
           05 WS-YEAR          PIC X(04).

       01 WS-GRADE-DATE-INPUT.
           05 WS-GRADE         PIC 9(02).
           05 FILLER           PIC X(01).
           05 WS-DAY           PIC X(02).
           05 WS-MONTH         PIC X(02).
           05 WS-YEAR          PIC X(04).

       01 WS-IDX               PIC 9(03).
       01 WS-TABLE-SIZE        PIC 9(03).

       01 WS-EXIT              PIC X(01).
           88 WS-EXIT-YES                VALUE "Y".
           88 WS-EXIT-NO                 VALUE "n".
       PROCEDURE DIVISION.
           DISPLAY
                "Enter any grade above 20 to stop."
                X"0A""Enter your grades (0-20), and the dates,"
                X"0A""under the following format : 99 DDMMYYYY"
           
           MOVE 0 TO WS-IDX.
           PERFORM UNTIL WS-IDX EQUAL 100
                   OR WS-EXIT-YES
               DISPLAY "                             "
                   WITH NO ADVANCING

               ACCEPT WS-GRADE-DATE-INPUT

               IF WS-GRADE IN WS-GRADE-DATE-INPUT > 20 THEN
                   SET WS-EXIT-YES TO TRUE
               ELSE
                   ADD 1 TO WS-IDX
                   MOVE CORRESPONDING WS-GRADE-DATE-INPUT 
                     TO WS-GRADE-DATE-OUTPUT(WS-IDX)
               END-IF

           END-PERFORM.

           MOVE WS-IDX TO WS-TABLE-SIZE.

           PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > WS-TABLE-SIZE
               DISPLAY WS-GRADE-DATE-OUTPUT(WS-IDX)
           END-PERFORM.
           STOP RUN.
