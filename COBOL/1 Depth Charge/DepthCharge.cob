      *-----------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DEPTHC.
      *-----------------------
       AUTHOR. James Roesemann.
      * MODIDFIED FORM A PROGRAM WRITTEN BY Dana Noftle.
      DATE-WRITTEN. September 9th 2022

      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------

      *******************************************************
      * This program is a recreation of the depth charge      *
      * game published in the NOV - DEC 1974 issue of         *
      * Creative Computing Magazine.  
      * 
      *
      * Game Rec id only for JCL compatiblity. will write  non JCl
      *compatible version for now.                            *
      * NOTE the program assumes an input file of GAMEREC     *
      * If This program is run with an interpreter rather 
      * than though JCl, create an empty file named GAMEREC   *
      * in the same directy as the program.                   *
      *   
      *******************************************************
       INPUT-OUTPUT SECTION.
      * only enable the file control if you want to make it compatibl
      *with JCl
      *FILE-CONTROL.
      *     SELECT GAME-RECORDS ASSIGN TO GAMEREC.
      *    ORGANIZATION IS LINE SEQUENTIAL
      *    ACCESS IS SEQUENTIAL.

      *-------------
       DATA DIVISION.
      *-------------

      * FILE SECTION.
      * FD  GAME-RECORDS
      *     RECORD CONTAINS 80 CHARACTERS
      *     RECORDING MODE IS F.
      * 01  GAME-BOARD-O.
      *     02  SHOTS-O PIC 9(3).
      *     02  DIMENSION-O PIC 9(03).
      *     02  TRIES-O PIC 9(3).

      * 01  SUB-LOCATION-O.
      *     02 SUB-X-POS-O PIC 9(3).
      *     02 SUB-Y-POS-O PIC 9(3).
      *     02 SUB-Z-POS-O PIC 9(3).

       WORKING-STORAGE SECTION.

       01  GAME-BOARD.
      * KEEP DIMENSION LESS THAN 100
           02  SHOTS PIC 9(3).
           02  DIMENSION PIC 9(03).
           02  TRIES  PIC 9(3).

       01  SUB-LOCATION.
           02 SUB-X-POS PIC 9(3).
           02 SUB-Y-POS PIC 9(3).
           02 SUB-Z-POS PIC 9(3).

       01  TORPEDO-CORDINATES.
           02  T-X-POS PIC 9(3).
           02  T-Y-POS PIC 9(3).
           02  T-Z-POS PIC 9(3).

       01  SHOT-CORDINATES.
           02  SHOT-X PIC 9(3).
           02  SHOT-Y PIC 9(3).
           02  SHOT-Z PIC 9(3).


      *------------------
       PROCEDURE DIVISION.
      *------------------
       GET-DIMENSION.
           DISPLAY "ENTER A NUMBER BETWEEN 1 AND 100."
           ACCEPT DIMENSION.
           IF DIMENSION IS > 99 OR < 2
           DISPLAY "YOU HAVE ENTERED AN INCORRECT VALUE"
           GO TO GET-DIMENSION
           ELSE 
      *I DON'T REMBER WHERE I FOUND THIS FUNCTION TO DETERMINE 
      *SHOTS MAYBE REPLACE IT LATER
           COMPUTE SHOTS = FUNCTION LOG(DIMENSION) / FUNCTION LOG(2).
           ADD 1 TO SHOTS.

       GAME-START.
           DISPLAY '            DEPTH CHARGE GAME'.
           DISPLAY 'YOU ARE THE CAPTIN OF THE DESTROYER USS DIGITAL'
           DISPLAY 'AN ENEMY SUB HAS BEEN CAUSING YOU TROUBLE. YOUR'
           DISPLAY 'MISSION IS TO DESTROY IT.'
           DISPLAY 'YOU HAVE ' SHOTS ' SHOTS'
           DISPLAY 'SPECIFY DEPTH CHARGE EXPLOSION POINT WITH A TRIO'
           DISPLAY 'OF NUMBERS -- THE FIRST TWO ARE THE SURFACE'
           DISPLAY 'CORDINATES. THE THIRD IS THE DEPTH.'
           DISPLAY ' '
           DISPLAY '                 GOOD LUCK'.
           DISPLAY ' '
           GO TO GENERATE-SUB-LOCATION.

       GENERATE-SUB-LOCATION.
           COMPUTE SUB-X-POS = DIMENSION * FUNCTION RANDOM.
           COMPUTE SUB-Y-POS = DIMENSION * FUNCTION RANDOM.
           COMPUTE SUB-Z-POS = DIMENSION * FUNCTION RANDOM.
      *TESTING
           DISPLAY 'SUB X ' SUB-X-POS 
           DISPLAY 'SUB Y ' SUB-Y-POS 
           DISPLAY 'SUB Z ' SUB-Z-POS 

           GO TO EVALUATE-INPUT.

       GIVEN-WRONG-INPUT.
           DISPLAY 'YOU HAVE ENTERED AN INCORRECT VALUE. TRY AGAIN.'.

       EVALUATE-INPUT.
           DISPLAY 'ENTER THREE NUMBERS BETWEEN 1 AND ' DIMENSION'.'
           ACCEPT T-X-POS.
           IF T-X-POS  <  2 OR > DIMENSION
           GO TO GIVEN-WRONG-INPUT
           ELSE
      *     ACCEPT T-Y-POS FROM USER-INPUT.
           ACCEPT T-Y-POS.
           IF T-Y-POS  <  2 OR > DIMENSION
           GO TO GIVEN-WRONG-INPUT
           ELSE
      *     ACCEPT T-Z-POS FROM USER-INPUT.
           ACCEPT T-Z-POS.
           IF T-Z-POS  <  2 OR > DIMENSION
           GO TO GIVEN-WRONG-INPUT
           ELSE GO TO NEXT-TURN.

       NEXT-TURN.
           ADD 1 TO TRIES
           DISPLAY 'TRIAL #' SHOTS ' ' T-X-POS ' ' T-Y-POS ' ' T-Z-POS
           COMPUTE SHOT-X = FUNCTION ABS(SUB-X-POS - T-X-POS)
           COMPUTE SHOT-Y = FUNCTION ABS(SUB-Y-POS - T-Y-POS)
           COMPUTE SHOT-Z = FUNCTION ABS(SUB-Z-POS - T-Z-POS)
           COMPUTE SHOT-X = SHOT-X + SHOT-Y + SHOT-Z
           IF SHOT-X IS EQUAL TO 0
           GO TO ENEMY-HIT
           ELSE
      *DO SOME MATH HERE. PROBALY TO ANOTHER PARAGRAPH WITH SOANER STUFF
      *NEED TO FIGURE OUT THIS GUS SPAGETTI CODE. JUMPS ARROUND ALOT.
           GO TO SONAR.
       
      * I NEED TO RE WROK THE X AND Y CORDS. IT DOSENT FIT WITH HOW YOU
      * THINK OF IT ON A MAP. Z IS FINE.
       SONAR.
           DISPLAY 'SONAR REPORTS SHOT WAS ' WITH NO ADVANCING 
           IF T-Y-POS > SUB-Y-POS THEN DISPLAY 'NORTH' 
           WITH NO ADVANCING
           ELSE IF T-Y-POS < SUB-Y-POS THEN DISPLAY 'SOUTH' 
           WITH NO ADVANCING
           END-IF .
           IF T-X-POS > SUB-X-POS THEN DISPLAY 'EAST' 
           WITH NO ADVANCING
           ELSE IF T-X-POS < SUB-X-POS THEN DISPLAY 'WEST'
           WITH NO ADVANCING
           END-IF.
           IF (T-X-POS NOT = SUB-Y-POS AND T-X-POS NOT = SUB-X-POS)
           THEN DISPLAY ' AND ' WITH NO ADVANCING
           IF T-Z-POS > SUB-Z-POS THEN DISPLAY 'TOO LOW' 
           WITH NO ADVANCING
           ELSE IF T-Z-POS < SUB-Z-POS THEN DISPLAY 'TOO HIGH' 
           WITH NO ADVANCING
           ELSE IF T-Z-POS IS EQUAL TO SUB-Z-POS THEN DISPLAY 'DEPTH OK'
           WITH NO ADVANCING
           END-IF.
           DISPLAY '.'
           SUBTRACT 1 FROM SHOTS.
           IF SHOTS IS EQUAL TO 0 THEN GO TO ABANDON-SHIP
           ELSE GO TO EVALUATE-INPUT.


       ABANDON-SHIP.
           DISPLAY 'YOU HAVE BEEN TORPEDOED! ABANDON SHIP!'
           DISPLAY 'THE SUBMARINE WAS AT ' SUB-X-POS WITH NO ADVANCING
           DISPLAY ' ' SUB-Y-POS ' ' SUB-Z-POS
           GO TO GAME-STOP.


       ENEMY-HIT.
           DISPLAY 'B O O M !  YOU FOUND IT IN ' TRIES ' TRIES.'.

       GAME-STOP.
           STOP RUN.           



      * i need to fix GAMEREC. does not curently exist. 
      * need to create GAMEREC if it dosent Exist.
      * need to delete it when the game is over