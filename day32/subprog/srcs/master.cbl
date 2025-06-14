       IDENTIFICATION DIVISION.
       PROGRAM-ID. master.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 12-06-2025 (fr).

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME              PIC X(20) VALUE "Leo".
       01 WS-RESULT            PIC X(30).
       01 WS-COUNT             PIC 9(02).
       01 WS-DISPLAY-COUNT     PIC Z9.

       PROCEDURE DIVISION.

           CALL "subprog" USING
                                WS-NAME
                                WS-RESULT
           END-CALL.

           CALL "cntchr" USING
                                WS-NAME
                                WS-COUNT
           END-CALL.

           MOVE WS-COUNT TO WS-DISPLAY-COUNT.

           DISPLAY "The name is " FUNCTION TRIM(WS-DISPLAY-COUNT)
                   " characters long."

           DISPLAY WS-RESULT.

           STOP RUN.
