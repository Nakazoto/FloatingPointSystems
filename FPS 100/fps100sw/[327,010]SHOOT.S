"****** SHOOT = FOR HASI TEST = REL A.0  , AUG 79 *****************************
$TITLE SHOOT
$ENTRY SHOOT,0
$EXT FPUT
"
$COMMON /PUT/ COUNT /I, W /I, X /I, Y /I, Z /I
"
SHOOT:  LDSPI 14; DB=2-1               "EXCHANGE SET-UP
        LDSPI 15; DB=5                 "LOOP COUNTER
        LDSPI 13; DB=PUT;              "ELEMENT LIST
         LDMA                          "GET 1ST ELEMENT
LOOP:   INC 14
        MOV 14,1                       "EXCH DEST
        LDSPI 2; DB=MD                 "ELEMENT TO SEND
        JSR FPUT
        DEC 15
        BGT LOOP;                      "CHECK IF DONE
         INC 13; SETMA                 "GET NEXT ELEMENT
"
        RETURN
$END
