       IDENTIFICATION DIVISION.
       PROGRAM-ID. isonlynb.
       DATA DIVISION. 
       WORKING-STORAGE SECTION.
       01 WS-IDX             PIC 9(02).
       01 WS-FND-NB-BOOL     PIC 9(01).
           88 WS-FND-NB-TRUE           VALUE 1.
           88 WS-FND-NB-FALSE          VALUE 0.
       LINKAGE SECTION.
       01  LK-INPUT          PIC X(50).
       01  LK-IS-NUM-BOOL    PIC 9(01).
           88 LK-IS-NUM-TRUE           VALUE 1.
           88 LK-IS-NUM-FALSE          VALUE 0.
       PROCEDURE DIVISION USING LK-INPUT, LK-IS-NUM-BOOL.
           SET LK-IS-NUM-TRUE TO TRUE.
           SET WS-FND-NB-FALSE TO TRUE.
           PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > LENGTH OF LK-INPUT
               IF LK-INPUT(WS-IDX: 1) NOT EQUAL SPACE AND
                  NOT (LK-INPUT(WS-IDX: 1) >= "0" AND
                  LK-INPUT(WS-IDX: 1) <= "9") THEN
                   SET LK-IS-NUM-FALSE TO TRUE
               END-IF
               IF LK-INPUT(WS-IDX: 1) >= "0" AND
                  LK-INPUT(WS-IDX: 1) <= "9"  THEN
                   SET WS-FND-NB-TRUE TO TRUE
               END-IF
           END-PERFORM.
           IF WS-FND-NB-FALSE THEN
               SET LK-IS-NUM-FALSE TO TRUE
           END-IF
           EXIT PROGRAM.
