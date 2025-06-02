       IDENTIFICATION DIVISION.
       PROGRAM-ID. cobcrud.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  GRANGER                PIC X(50).
       01  PEOPLE-ID              PIC 9(10).
       01  PEOPLE-FNAME           PIC X(50).
       01  PEOPLE-NAME            PIC X(50).
       01  PEOPLE-PHONE-NUMBER    PIC X(10).
       01  USERNAME               PIC X(30) VALUE "cobol".
       01  PASSWD                 PIC X(30) VALUE "mdp".
       01  DBNAME                 PIC X(10) VALUE "testdb".
       EXEC SQL END DECLARE SECTION END-EXEC.
       
       01 WS-INPUT                PIC X(10).
       01 WS-INPUT-2              PIC X(50).

       01 WS-IDX                  PIC 9(02).
       01 WS-IS-NUM-BOOL          PIC 9(01).
           88 WS-IS-NUM-TRUE                VALUE 1.
           88 WS-IS-NUM-FALSE               VALUE 0.
       
       01 WS-STRING-CREATE        PIC X(10) VALUE "CREATE".
       01 WS-STRING-READ          PIC X(10) VALUE "READ".
       01 WS-STRING-UPDATE        PIC X(10) VALUE "UPDATE".
       01 WS-STRING-DELETE        PIC X(10) VALUE "DELETE".
       01 WS-STRING-QUIT          PIC X(10) VALUE "QUIT".

       01 WS-OUT-HEADER.
           05 FILLER              PIC X(10) VALUE "id".
           05 FILLER              PIC X(03) VALUE " | ".
           05 FILLER              PIC X(50) VALUE "nom".
           05 FILLER              PIC X(03) VALUE " | ".
           05 FILLER              PIC X(50) VALUE "prenom".
           05 FILLER              PIC X(03) VALUE " | ".
           05 FILLER              PIC X(10) VALUE "telephone".

       01 WS-OUT-BODY.
           05 WS-OUT-ID           PIC X(10).
           05 FILLER              PIC X(03) VALUE " | ".
           05 WS-OUT-NAME         PIC X(50).
           05 FILLER              PIC X(03) VALUE " | ".
           05 WS-OUT-FNAME        PIC X(50).
           05 FILLER              PIC X(03) VALUE " | ".
           05 WS-OUT-PHONE-NUMBER PIC X(10).

       EXEC SQL INCLUDE SQLCA END-EXEC.
       
       PROCEDURE DIVISION.
           DISPLAY "Connecting to PostgreSQL...".
           
           EXEC SQL
            CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
           END-EXEC.
           
           IF SQLCODE NOT = 0
               DISPLAY "Connection error SQLCODE: " SQLCODE
               STOP RUN
           END-IF.
           
           DISPLAY "Successful connection!".

           
           MOVE SPACE TO WS-INPUT.
           PERFORM UNTIL WS-INPUT EQUAL WS-STRING-QUIT
               DISPLAY WS-STRING-CREATE " / "
                       WS-STRING-READ " / "
                       WS-STRING-UPDATE " / "
                       WS-STRING-DELETE " / "
                       WS-STRING-QUIT "."
               ACCEPT WS-INPUT
               EVALUATE WS-INPUT
                   WHEN WS-STRING-CREATE
                       
                   WHEN WS-STRING-READ
                       PERFORM 0200-CRUD-READ-BEGIN
                          THRU 0200-CRUD-READ-END
                   WHEN WS-STRING-UPDATE
            
                   WHEN WS-STRING-DELETE

               END-EVALUATE
           END-PERFORM.
           
           

      *    MOVE "MAMA" TO PEOPLE-NAME.
      *    MOVE "MASSAR" TO PEOPLE-FNAME.
      *    MOVE "0777777777" TO PEOPLE-PHONE-NUMBER.
      *    EXEC SQL 
      *        INSERT INTO individus (nom, prenom, telephone)
      *        VALUES 
      *            (:PEOPLE-NAME, :PEOPLE-FNAME, :PEOPLE-PHONE-NUMBER);
      *    END-EXEC.
      *
      *    IF SQLCODE NOT = 0
      *        DISPLAY "Connection error SQLCODE: " SQLCODE
      *        STOP RUN
      *    END-IF.
       
      *    EXEC SQL COMMIT WORK END-EXEC.

           EXEC SQL
               DISCONNECT ALL
           END-EXEC.

           
           
           DISPLAY "Successful disconnect."
           
           STOP RUN.

       0100-CRUD-CREATE-BEGIN.

       0100-CRUD-CREATE-END.

       0200-CRUD-READ-BEGIN.
           DISPLAY
               "Enter either an ID or a name to select from the table".
           ACCEPT WS-INPUT-2.
           CALL "isonlynb" USING 
               BY REFERENCE WS-INPUT-2
               BY REFERENCE WS-IS-NUM-BOOL
           END-CALL.
           
           IF WS-IS-NUM-TRUE
               DISPLAY "Numeric"
               MOVE FUNCTION NUMVAL(WS-INPUT-2) TO PEOPLE-ID
       EXEC SQL
               SELECT nom, prenom, telephone
               INTO :PEOPLE-NAME, :PEOPLE-FNAME,
                    :PEOPLE-PHONE-NUMBER
               FROM individus
               WHERE id = :PEOPLE-ID;
       END-EXEC
           ELSE
               DISPLAY "Alpha"
       EXEC SQL
               SELECT id, nom, prenom, telephone
               INTO :PEOPLE-ID, :PEOPLE-NAME, :PEOPLE-FNAME,
                    :PEOPLE-PHONE-NUMBER
               FROM individus
               WHERE nom = :WS-INPUT-2;
       END-EXEC
           END-IF.
           
           IF SQLCODE NOT = 0
               DISPLAY "Connection error SQLCODE: " SQLCODE
           END-IF.

           MOVE PEOPLE-ID TO WS-OUT-ID.
           MOVE PEOPLE-NAME TO WS-OUT-NAME.
           MOVE PEOPLE-FNAME TO WS-OUT-FNAME.
           MOVE PEOPLE-PHONE-NUMBER TO WS-OUT-PHONE-NUMBER.

           DISPLAY WS-OUT-HEADER.
           DISPLAY WS-OUT-BODY.
       0200-CRUD-READ-END.

       0300-CRUD-UPDATE-BEGIN.

       0300-CRUD-UPDATE-END.

       0400-CRUD-DELETE-BEGIN.

       0400-CRUD-DELETE-END.