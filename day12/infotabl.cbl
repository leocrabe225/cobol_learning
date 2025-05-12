       IDENTIFICATION DIVISION.
       PROGRAM-ID. infotabl.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 12-05-2025 (fr).
       DATE-COMPILED. null.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PEOPLE-TABLE.
           05 WS-PEOPLE        OCCURS 10 TIMES.
               10 WS-PEOPLE-NAME   PIC X(10).
               10 WS-PEOPLE-AGE    PIC 9(03).
               10 WS-PEOPLE-HEIGHT PIC 9(03).
       
       01 WS-PEOPLE-OUTPUT.
           05 WS-OUTPUT-NAME          PIC X(10).
           05 FILLER                  PIC X(04) VALUE " is ".
           05 WS-OUTPUT-AGE           PIC 9(03).
           05 FILLER                  PIC X(05) VALUE " and ".
           05 WS-OUTPUT-HEIGHT        PIC 9(03).
           05 FILLER                  PIC X(07) VALUE "cm high.".

       01 WS-IDX               PIC 9(02).
       PROCEDURE DIVISION.
           MOVE "Leo       023183" TO WS-PEOPLE(1).
           MOVE "Anais     033168" TO WS-PEOPLE(2).
           MOVE "Terry     029178" TO WS-PEOPLE(3).
           MOVE "William   057180" TO WS-PEOPLE(4).
           MOVE "Alexandre 030180" TO WS-PEOPLE(5).
           MOVE "Bernadette047163" TO WS-PEOPLE(6).
           MOVE "Yassine   035174" TO WS-PEOPLE(7).
           MOVE "Lucas     030179" TO WS-PEOPLE(8).
           MOVE "Benoit    055176" TO WS-PEOPLE(9).
           MOVE "Vincent   032184" TO WS-PEOPLE(10).
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 10
               MOVE WS-PEOPLE-NAME(WS-IDX) TO WS-OUTPUT-NAME
               MOVE WS-PEOPLE-AGE(WS-IDX) TO WS-OUTPUT-AGE
               MOVE WS-PEOPLE-HEIGHT(WS-IDX) TO WS-OUTPUT-HEIGHT
               DISPLAY WS-PEOPLE-OUTPUT
           END-PERFORM.
           STOP RUN.
