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
           EVALUATE TRUE
               WHEN CUSTOMER-TYPE EQUAL VIP AND ACCOUNT > 10000
                   DISPLAY "You are a premium member."
               WHEN CUSTOMER-TYPE EQUAL VIP AND ACCOUNT <= 10000
                   DISPLAY "You are a privileged member."
               WHEN CUSTOMER-TYPE EQUAL STANDARDM AND ACCOUNT > 5000
                   DISPLAY "You are a recurring member."
               WHEN CUSTOMER-TYPE EQUAL STANDARDM AND ACCOUNT <= 5000
                   DISPLAY "You are a standard member."
           END-EVALUATE.
           STOP RUN.
           