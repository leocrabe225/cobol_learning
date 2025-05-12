       IDENTIFICATION DIVISION.
       PROGRAM-ID. gradfill.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 12-05-2025 (fr).
       DATE-COMPILED. null.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GRADE-DATE-OUTPUT OCCURS 5 TIMES.
           05 FILLER           PIC X(07) VALUE "Note : ".
           05 WS-GRADE         PIC 9(02).
           05 FILLER           PIC X(04) VALUE " le ".
           05 WS-DAY       PIC X(02).
           05 FILLER       PIC X(01) VALUE "/".
           05 WS-MONTH     PIC X(02).
           05 FILLER       PIC X(01) VALUE "/".
           05 WS-YEAR      PIC X(04).

       01 WS-GRADE-DATE-INPUT.
           05 WS-GRADE         PIC 9(02).
           05 FILLER           PIC X(01).
           05 WS-DAY           PIC X(02).
           05 WS-MONTH         PIC X(02).
           05 WS-YEAR          PIC X(04).

       01 WS-IDX               PIC 9(01).
       PROCEDURE DIVISION.
           DISPLAY
                "Enter your grades (0-99), and the dates,"
                X"0A""under the following format : 99 DDMMYYYY"
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 5
               DISPLAY "                             "
                   WITH NO ADVANCING
               ACCEPT WS-GRADE-DATE-INPUT
               MOVE CORRESPONDING WS-GRADE-DATE-INPUT 
                 TO WS-GRADE-DATE-OUTPUT(WS-IDX)
           END-PERFORM.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 5
               DISPLAY WS-GRADE-DATE-OUTPUT(WS-IDX)
           END-PERFORM.
           STOP RUN.
