       IDENTIFICATION DIVISION.
       PROGRAM-ID. readfile.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 13-05-2025.
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PEOPLE-FILE
               ASSIGN TO "gens.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD PEOPLE-FILE.
       01 F-PEOPLE-RECORD.
           05 F-PEOPLE-NAME           PIC X(12).
           05 F-PEOPLE-FIRST-NAME     PIC X(12).
       WORKING-STORAGE SECTION.
       01 WS-EOF                      PIC X(01) VALUE 'N'.
           88 WS-EOF-TRUE                       VALUE 'Y'.
           88 WS-EOF-FALSE                      VALUE 'N'.
       PROCEDURE DIVISION.
           OPEN INPUT PEOPLE-FILE.
           DISPLAY "*-------------*-------------*".
           DISPLAY "| Prénom      | Nom         |".
           DISPLAY "*-------------*-------------*".
           PERFORM UNTIL WS-EOF-TRUE
               READ PEOPLE-FILE
                   AT END
                       SET WS-EOF-TRUE TO TRUE
                   NOT AT END
                       DISPLAY "| " F-PEOPLE-FIRST-NAME "| " 
                                   F-PEOPLE-NAME "|"
               END-READ
           END-PERFORM.
           DISPLAY "*-------------*-------------*".
           CLOSE PEOPLE-FILE.
           STOP RUN.
