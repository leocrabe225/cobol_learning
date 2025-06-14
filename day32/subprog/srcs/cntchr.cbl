       IDENTIFICATION DIVISION.
       PROGRAM-ID. cntchr.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 14-06-2025 (fr).

       DATA DIVISION.

       LINKAGE SECTION.
       01 LK-NOM       PIC X(20).
       01 LK-COUNT     PIC 9(02).

       PROCEDURE DIVISION USING
                                LK-NOM,
                                LK-COUNT.

           MOVE FUNCTION LENGTH(FUNCTION TRIM(LK-NOM)) TO LK-COUNT.

           EXIT PROGRAM.