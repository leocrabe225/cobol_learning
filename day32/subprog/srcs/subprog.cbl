       IDENTIFICATION DIVISION.
       PROGRAM-ID. subprog.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 12-06-2025 (fr).

       DATA DIVISION.

       LINKAGE SECTION.
       01 LK-NOM       PIC X(20).
       01 LK-RESULT    PIC X(30).

       PROCEDURE DIVISION USING
                                LK-NOM,
                                LK-RESULT.

           STRING "Bonjour, " LK-NOM
               DELIMITED BY SIZE
               INTO LK-RESULT
           END-STRING.

           EXIT PROGRAM.
