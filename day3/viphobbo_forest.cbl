       IDENTIFICATION DIVISION.
       PROGRAM-ID. viphobbo.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 24-04-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
       01 CUSTOMER-TYPE            PIC X(10).
       01 ACCOUNT                  PIC 9(10).
       01 VIP                      PIC X(10) VALUE "VIP".
       01 STANDARDM                PIC X(10) VALUE "Standard".
       PROCEDURE DIVISION.
       CLIENT-TYPE-QUESTION.
           DISPLAY "What type of client are you? (VIP or Standard)".
           ACCEPT CUSTOMER-TYPE.
           IF (NOT CUSTOMER-TYPE EQUAL VIP) AND
              (NOT CUSTOMER-TYPE EQUAL STANDARDM) THEN
               DISPLAY QUOTE CUSTOMER-TYPE QUOTE " is not a valid type"
               GO TO CLIENT-TYPE-QUESTION
           END-IF.
           DISPLAY "How much is there in your customer account?".
           ACCEPT ACCOUNT.
           IF CUSTOMER-TYPE EQUAL VIP THEN
               IF ACCOUNT > 10000 THEN
                   DISPLAY "You are a premium member."
               ELSE
                   DISPLAY "You are a privileged member."
               END-IF
           ELSE
               IF ACCOUNT > 5000 THEN
                   DISPLAY "You are a recurring member."
               ELSE
                   DISPLAY "You are a standard member."
               END-IF 
           END-IF.
           STOP RUN.
           