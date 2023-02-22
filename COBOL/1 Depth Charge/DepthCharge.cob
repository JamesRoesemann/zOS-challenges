      *-----------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DEPTHC.
      *-----------------------
       AUTHOR. James Roesemann.
      * MODIDFIED FORM A PROGRAM WRITTEN BY Dana Noftle.
      DATE-WRITTEN. September 7th 2022

      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------

      *******************************************************
      * This program is a recreation of the depth charge      *
      * game published in the NOV - DEC 1974 issue of         *
      * Creative Computing Magazine.  
      * 
      *Intended to be combiled with gnuCOBOL (COBC)
      *into an executable program
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

      *-------------
       DATA DIVISION.
      *-------------

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

       01  PLAYER.
           02 PLAY-ANSWER PIC X(1).


      *------------------
       PROCEDURE DIVISION.
      *------------------
      *ACCEPT A 3 DIGIT NUMBER TO USE AS THE DIMENSTION OF THE GAME
      *USE THIS VALUE TO ALLOCATE SHOTS RELATIVE TO THE DIMENSION SIZE
       GET-DIMENSION.
           DISPLAY "ENTER A NUMBER BETWEEN 1 AND 100."
           ACCEPT DIMENSION.
           IF DIMENSION IS > 99 OR < 2
           DISPLAY "YOU HAVE ENTERED AN INCORRECT VALUE. TRY AGAIN."
           GO TO GET-DIMENSION
           ELSE 
           COMPUTE SHOTS = FUNCTION LOG(DIMENSION) / FUNCTION LOG(2).
           ADD 1 TO SHOTS.

       GAME-START.
           DISPLAY '            DEPTH CHARGE GAME'.
           DISPLAY 'DIMENSION OF SEARCH AREA ' DIMENSION.
           DISPLAY 'YOU ARE THE CAPTIN OF THE DESTROYER USS DIGITAL'
           DISPLAY 'AN ENEMY SUB HAS BEEN CAUSING YOU TROUBLE. YOUR'
           DISPLAY 'MISSION IS TO DESTROY IT.'
           DISPLAY 'YOU HAVE ' SHOTS ' SHOTS.'
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
           GO TO EVALUATE-INPUT.

       GIVEN-WRONG-INPUT.
           DISPLAY 'YOU HAVE ENTERED AN INCORRECT VALUE. TRY AGAIN.'.

      *ACCEPT USER INPUT AND EVALUATE IF IS CAN BE USED FOR CORDINATES.
       EVALUATE-INPUT.
           DISPLAY 'ENTER THREE NUMBERS BETWEEN 1 AND ' DIMENSION'.'
           DISPLAY 'TARGET POSITION Y:' WITH NO ADVANCING
      *     ACCEPT T-Y-POS FROM USER-INPUT.
           ACCEPT T-Y-POS.
           IF T-Y-POS  <  2 OR > DIMENSION
           GO TO GIVEN-WRONG-INPUT
           ELSE
      *     ACCEPT T-X-POS FROM USER-INPUT.     
           DISPLAY 'TARGET POSITION X:' WITH NO ADVANCING
           ACCEPT T-X-POS.
           IF T-X-POS  <  2 OR > DIMENSION
           GO TO GIVEN-WRONG-INPUT
           ELSE           
      *     ACCEPT T-Z-POS FROM USER-INPUT.
           DISPLAY 'TARGET POSITION Z:' WITH NO ADVANCING
           ACCEPT T-Z-POS.
           IF T-Z-POS  <  2 OR > DIMENSION
           GO TO GIVEN-WRONG-INPUT
           ELSE GO TO NEXT-TURN.

      * EVALUATE DEPTH CHARGE TARGET RELATIVE TO SUB LOCATION 
       NEXT-TURN.
           ADD 1 TO TRIES
           DISPLAY 'TRIAL #' SHOTS ' X:' T-X-POS ' Y:' T-Y-POS 
           ' Z:' T-Z-POS
           COMPUTE SHOT-X = FUNCTION ABS(SUB-X-POS - T-X-POS)
           COMPUTE SHOT-Y = FUNCTION ABS(SUB-Y-POS - T-Y-POS)
           COMPUTE SHOT-Z = FUNCTION ABS(SUB-Z-POS - T-Z-POS)
           COMPUTE SHOT-X = SHOT-X + SHOT-Y + SHOT-Z
           IF SHOT-X IS EQUAL TO 0
           GO TO ENEMY-HIT
           ELSE
           GO TO SONAR.
           
      * DISPLAY THE GENRAL LOCATION OF THE SUB RELATIVE TO THE TRGET.
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
           THEN DISPLAY ' AND' WITH NO ADVANCING
           END-IF.
      * THIS PART SEEMS CONFUUSING. I MUIIGHT CHAGE IT SO THAT ZERO
      *STARTS AT THE BOTTOM AND NOT THE TOP.     
           IF T-Z-POS < SUB-Z-POS THEN DISPLAY ' TOO LOW' 
           WITH NO ADVANCING
           ELSE IF T-Z-POS > SUB-Z-POS THEN DISPLAY ' TOO HIGH' 
           WITH NO ADVANCING
           ELSE IF T-Z-POS IS EQUAL TO SUB-Z-POS 
           THEN DISPLAY ' DEPTH OK'
           WITH NO ADVANCING
           END-IF.
           DISPLAY '.'
           SUBTRACT 1 FROM SHOTS.
      *testing
           DISPLAY TRIES     
           IF SHOTS IS EQUAL TO 0 THEN GO TO ABANDON-SHIP
           ELSE GO TO EVALUATE-INPUT.

       ABANDON-SHIP.
           DISPLAY 'YOU HAVE BEEN TORPEDOED! ABANDON SHIP!'
           DISPLAY 'THE SUBMARINE WAS AT ' SUB-X-POS WITH NO ADVANCING
           DISPLAY ' ' SUB-Y-POS ' ' SUB-Z-POS
           GO TO NEW-GAME.

       ENEMY-HIT.
           DISPLAY 'B O O M !  YOU FOUND IT IN ' TRIES ' TRIES.'.
           
       NEW-GAME.
           MOVE 000 TO TRIES.    
           DISPLAY 'DO YOU WANT TO PLAY AGAIN? (Y OR N):' 
           WITH NO ADVANCING
           ACCEPT PLAY-ANSWER
           IF PLAY-ANSWER IS EQUAL TO 'Y' OR 'y' GO TO GET-DIMENSION
           ELSE IF PLAY-ANSWER IS EQUAL TO 'N' OR 'n' GO TO GAME-STOP
           ELSE DISPLAY 'I''M SORRY. I DIDN''T UNDERSTAND THAT.'
           GO TO NEW-GAME.

       GAME-STOP.
           STOP RUN.           
