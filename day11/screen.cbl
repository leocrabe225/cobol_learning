       IDENTIFICATION DIVISION.
       PROGRAM-ID. pizza.
             
       DATA DIVISION.
       
         WORKING-STORAGE SECTION.

       01  WS-NOM          PIC X(20) VALUE SPACES.
       01  WS-PRENOM       PIC X(20) VALUE SPACES.
       01  WS-NB-INVITE    PIC 9(02).   
       01  FIN-SAISIE      PIC X     VALUE 'N'.
       01  WS-INTRO-STOP   PIC X(01).

       01  WS-NB-PIZZA     PIC 9(2)V99 VALUE ZERO.


       
       SCREEN SECTION.
       01 ECRAN-INTRO.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1  PIC 9 TO WS-INTRO-STOP.
           05 LINE 3 COLUMN 10 PIC X(40) 
               VALUE "    P P P  IIIII  ZZZZZ  ZZZZZ    A   "
               HIGHLIGHT BLINK FOREGROUND-COLOR 4.
           05 LINE 4 COLUMN 10 PIC X(40) 
               VALUE "    P   P    I       Z      Z    A A  " 
               HIGHLIGHT BLINK FOREGROUND-COLOR 4.
           05 LINE 5 COLUMN 10 PIC X(40) 
               VALUE "    PPPP     I      Z      Z    A   A " 
               HIGHLIGHT BLINK FOREGROUND-COLOR 4.
           05 LINE 6 COLUMN 10 PIC X(40) 
               VALUE "    P        I     Z      Z    AAAAAAA" 
               HIGHLIGHT BLINK FOREGROUND-COLOR 4.
           05 LINE 7 COLUMN 10 PIC X(40) 
               VALUE "    P      IIIII  ZZZZZ  ZZZZZA       A" 
               HIGHLIGHT BLINK FOREGROUND-COLOR 4.

           05 LINE 9 COLUMN 27 PIC X(40) 
               VALUE "COMME A" HIGHLIGHT BLINK.

           05 LINE 11 COLUMN 15 PIC X(40) 
               VALUE "    ____   ____  ______  ______ " 
               HIGHLIGHT BLINK FOREGROUND-COLOR 2.
           05 LINE 11 COLUMN 15 PIC X(40) 
               VALUE "   / __ \ / __ \/_  __ \/ ____/ " 
               HIGHLIGHT BLINK FOREGROUND-COLOR 2.
           05 LINE 12 COLUMN 15 PIC X(40) 
               VALUE "  / /_/ // / / / / / / / __/    " 
               HIGHLIGHT BLINK FOREGROUND-COLOR 2.
           05 LINE 13 COLUMN 15 PIC X(40) 
               VALUE " / _, _// /_/ / / / / / /___    " 
               HIGHLIGHT BLINK FOREGROUND-COLOR 2.
           05 LINE 14 COLUMN 15 PIC X(40) 
               VALUE "/_/ |_| \____/ /_/ /_/_____/    " 
               HIGHLIGHT BLINK FOREGROUND-COLOR 2.
           
           05 LINE 18 COLUMN 18 PIC X(40) 
               VALUE "BIENVENUE CHEZ COBOLEZZA" HIGHLIGHT.

           05 LINE 20 COLUMN 22 PIC X(40) 
               VALUE "APPUYEZ SUR ENTREE" LOWLIGHT.

       01 ECRAN-SAISI.
           05 BLANK SCREEN.
           05 LINE 3 COLUMN 10 
               VALUE "Veuillez commander votre pizza"
                    HIGHLIGHT FOREGROUND-COLOR 4.
           05 LINE 5 COLUMN 10 VALUE "Entrez votre prenom :".
           05 LINE 5 COLUMN 50 PIC X(20) TO WS-PRENOM.
           05 LINE 7 COLUMN 10 VALUE "Entrez votre nom :".
           05 LINE 7 COLUMN 50 PIC X(20) TO WS-NOM.
           05 LINE 9 COLUMN 10 
               VALUE "Entrez le nombre de vos invites :".
           05 LINE 9 COLUMN 50 PIC X(20) TO WS-NB-INVITE.
       
       01 BLANKING.
           05 BLANK SCREEN.

       PROCEDURE DIVISION.

           DISPLAY ECRAN-INTRO.
           ACCEPT WS-INTRO-STOP.  
           DISPLAY ECRAN-SAISI.
           ACCEPT ECRAN-SAISI.
           DISPLAY BLANKING.
           COMPUTE WS-NB-PIZZA ROUNDED = WS-NB-INVITE * 1.1.
           DISPLAY WS-NB-PIZZA.

           IF WS-PRENOM = SPACES OR WS-NOM = SPACES
               DISPLAY "Nom et prénom obligatoires !"
           ACCEPT WS-PRENOM
           END-IF.
           DISPLAY "CHecking"

           STOP RUN.