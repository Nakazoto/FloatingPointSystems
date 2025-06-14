; VFC10.CMD - INSTALL VFC100 COMMAND FILE
; RSX11M FPS100 FEB 80
; VFC100.FTN
; LIB100 MUST BE BUILT AND IN PLACE
;
 .ENABLE GLOBAL
 .ENABLE SUBSTITUTION
 .IFDF $MAST .GOTO 10
; SETUP
 @SETUP
;
 .10:
;
; COMPILE VFC100
;
 .IFINS FOR FOR VFC100=VFC100'$FOR2'
 .IFINS F4P F4P VFC100=VFC100'$F4P2'
;
; BUILD VFC100
;
    .OPEN  VFC100.CMD
  .DATA   VFC100/CP,VFC100=VFC100,LB:'$LUIC'LIB100/LB
   .DATA /
   .DATA UNITS=12
   .DATA //
   .CLOSE
 TKB @VFC100
 PIP VFC100.TSK/PU
;
; DELETE INTERMEDIATE FILES
;
 PIP VFC100.OBJ;*,VFC100.CMD;*/DE
 .EXIT
 