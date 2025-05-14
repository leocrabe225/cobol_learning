       IDENTIFICATION DIVISION.
       PROGRAM-ID. cliorder.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 14-05-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTS-INPUT
               ASSIGN TO "data/clients.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ORDERS-INPUT
               ASSIGN TO "data/num-commandes.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CLIENTS-INPUT.
       01 CLI-IN-RECORD.
           05 FILLER               PIC X(08).
           05 CLI-IN-NAME          PIC X(10).
           05 FILLER               PIC X(01).
           05 CLI-IN-FNAME         PIC X(10).

       FD ORDERS-INPUT.
       01 ORD-IN-RECORD.
           05 ORD-IN-CLI-ID        PIC 9(02).
           05 FILLER               PIC X(01).
           05 ORD-IN-ID            PIC 9(03).
           05 FILLER               PIC X(01).
           05 ORD-IN-AMT           PIC 9(04).

       WORKING-STORAGE SECTION.
       01 WS-CLI-TBL.
           05 WS-CLI OCCURS 10 TIMES.
               10 WS-CLI-NAME         PIC X(10).
               10 WS-CLI-FNAME        PIC X(10).
       
       01 WS-ORD-TBL.
           05 WS-ORD OCCURS 99 TIMES.
               10 WS-ORD-CLI-ID       PIC 9(02).
               10 WS-ORD-ID           PIC 9(03).
               10 WS-ORD-AMT          PIC 9(04).
       
       01 WS-IDX-1                    PIC 9(02).
       01 WS-IDX-2                    PIC 9(02).

       01 WS-CLI-TBL-SIZE             PIC 9(02).
       01 WS-ORD-TBL-SIZE             PIC 9(02).

       01 WS-EOF                      PIC 9(01).
           88 WS-EOF-TRUE                       VALUE 1.
           88 WS-EOF-FALSE                      VALUE 0.

       PROCEDURE DIVISION.
           MOVE 0 TO WS-IDX-1.
           SET WS-EOF-FALSE TO TRUE.
           OPEN INPUT CLIENTS-INPUT.
           PERFORM UNTIL WS-EOF-TRUE
               READ CLIENTS-INPUT
                   AT END
                       SET WS-EOF-TRUE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-IDX-1
                       MOVE CLI-IN-NAME  TO WS-CLI-NAME(WS-IDX-1)
                       MOVE CLI-IN-FNAME TO WS-CLI-FNAME(WS-IDX-1)
               END-READ
           END-PERFORM.
           CLOSE CLIENTS-INPUT.
           MOVE WS-IDX-1 TO WS-CLI-TBL-SIZE.

           MOVE 0 TO WS-IDX-2.
           SET WS-EOF-FALSE TO TRUE.
           OPEN INPUT ORDERS-INPUT.
           PERFORM UNTIL WS-EOF-TRUE
               READ ORDERS-INPUT
                   AT END
                       SET WS-EOF-TRUE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-IDX-2
                       MOVE ORD-IN-CLI-ID TO WS-ORD-CLI-ID(WS-IDX-2)
                       MOVE ORD-IN-ID     TO WS-ORD-ID(WS-IDX-2)
                       MOVE ORD-IN-AMT    TO WS-ORD-AMT(WS-IDX-2)
               END-READ
           END-PERFORM.
           CLOSE ORDERS-INPUT.
           MOVE WS-IDX-2 TO WS-ORD-TBL-SIZE.

           MOVE 1 TO WS-IDX-1.
           MOVE 1 TO WS-IDX-2.
           PERFORM UNTIL WS-IDX-1 > WS-CLI-TBL-SIZE
               DISPLAY WS-CLI-FNAME(WS-IDX-1) WS-CLI-NAME(WS-IDX-1)
               PERFORM UNTIL WS-ORD-CLI-ID(WS-IDX-2) NOT EQUAL WS-IDX-1
                   DISPLAY "    " WS-ORD-ID(WS-IDX-2) SPACE
                                  WS-ORD-AMT(WS-IDX-2)
                   ADD 1 TO WS-IDX-2
               END-PERFORM
               DISPLAY SPACE
               ADD 1 TO WS-IDX-1
           END-PERFORM.
           
           STOP RUN.
