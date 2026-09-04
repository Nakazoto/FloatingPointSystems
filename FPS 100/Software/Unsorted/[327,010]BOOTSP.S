"****** BOOTSP = BOOTSTRAP FOR SUPER100 = REL 0.0  , AUG 79 *******************
"
        $TITLE BOOTSP
        $ENTRY BOOTSP
        $EXT EXTASK
        $EXT OVHNDL
"
"      INSERT SYSDEF
$NOLIST
$INSERT SYSDEF
$LIST
"  LOCAL VARIABLES
"
        IOMASK  = 175377       "IMASK REG VALUE
        HALTRC  = 034           "RTC HALT VALUE
        STATU2   = 052260       "APST2 REG VALUE
"
BOOTSP: LDDA;DB=IMASK           "SET DEVICE ADDR TO MASK REG
        OUT;DB=IOMASK           "ENABLE ONLY NEEDED INTERRUPTS
        LDDA;DB=RTCCST          "POINT TO RTC
        OUT;DB=ZERO             "CLEAR LOAD REG.
        LDDA;DB=RTCCTL          "POINT TO CONTROL REG
        OUT;DB=HALTRC           "HALT REAL-TIME CLOCK
        SWDB                    "CLEAR SWITCH-ACK FLAG
        LDDPA;DB=ZERO            "DPA = 0
        LDDA;DB=APST2           "POINT TO STAT REG
        OUT;DB=STATU2            "SET TO SUPER MODE
        JMP EXTASK                "GOTO EXECUTE TASK
"
"
        NOP
        NOP
        NOP
        NOP
        JMP OVHNDL
"
"
        $END
