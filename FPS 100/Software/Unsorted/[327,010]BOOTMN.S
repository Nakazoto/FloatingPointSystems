"****** BOOTMN = BOOTSTRAP FOR MINI100 = REL 1.0  , NOV 79 ********************
"
        $TITLE BOOTSP
        $ENTRY BOOTSP
        $EXT OVHNDL
"
$NOLIST
$INSERT MINDEF
$LIST
"
$COMMON /DOT/ DTABLK(33.) /I
$INSERT MINCOM
"
"  LOCAL VARIABLES
"
        IOMASK  = 175377       "IMASK REG VALUE (ENABLE HOST AND RTC)
        HALTRC  = 034           "RTC HALT VALUE
        STATU2   = 004260       "APST2 REG VALUE
"
BOOTSP: LDDA;DB=IMASK           "SET DEVICE ADDR TO MASK REG
        OUT;DB=IOMASK           "ENABLE ONLY NEEDED INTERRUPTS
        LDDA;DB=RTCCST          "POINT TO RTC
        OUT;DB=ZERO             "CLEAR LOAD REG.
        LDDA;DB=RTCCTL          "POINT TO CONTROL REG
        OUT;DB=HALTRC           "HALT REAL-TIME CLOCK
        SWDB                    "CLEAR SWITCH-ACK FLAG
        LDOMA; DB=SSAREA        "SET SMA = START OF SAVE AREA
        LDDPA;DB=ZERO            "DPA = 0
        LDDA; DB=APST3          "POINT TO STAT3 REG
        OUT; DB=ZERO            "SET SRA=0
        LDDA;DB=APST2           "POINT TO STAT2 REG
        OUT;DB=STATU2           "SET USER MODE, USER MA, INTS ON
        JMP OVHNDL              "MAKE SUBR RES, AND RUN IT
"
"
        NOP
        JMP BOOTSP
"
"
        $END
