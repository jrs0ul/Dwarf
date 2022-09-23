 PROCESSOR 6502
    include "vcs.h"
    include "macro.h"


MAPHEIGHT                        = 6
MAPWIDTH                         = 12
PLAYERHEIGHT                     = 9
LADDERHEIGHT                     = 11
LINESPERCELL                     = 9
MAP_CELL_WIDTH                   = 12
DIGITS_PTR_COUNT                 = 12
X_OFFSET_TO_RIGHT_FOR_MINING     = 7
MIN_PLAYER_Y                     = 9
MAX_PLAYER_Y                     = 54
MAX_PLAYER_X                     = 140
INITIAL_PLAYER_LIVES             = 3
MAX_PLAYER_LIVES                 = 7

LAVA_START_POS_LEFT              = $05 ; x=0; y=5
LAVA_START_POS_RIGHT             = $B5
INITIAL_LAVA_SLEEP               = 8
LAVA_SLEEP_AFTER_PRIZE           = 10
INITIAL_LAVA_DELAY               = 29
MINIMUM_LAVA_DELAY               = 10
MAX_LAVA_X                       = 11


;Main goals
GOALX1                           = 2
GOALX2                           = 130
GOALY                            = 12

;Colors
LIVES_BG                         = $80
SCORE_COLOR                      = 15
LADDERS_COLOR                    = $C6
GROUND_COLOR                     = $93
LAVA_COLOR_BEFORE                = $34
LAVA_COLOR_AFTER                 = $14
LAVA_FROZEN_COLOR                = $9A

SCORE_FOR_PRIZE                  = 10
SCORE_FOR_BRICK                  = 2
SCORE_FOR_LAVA                   = 48
MIN_DISTANCE_FROM_PRIZE          = 18

DEATH_INTERVAL                   = 64


NO_ILLEGAL_OPCODES               = 1 ; DASM needs it

;----------------------------------------------------
;           RAM
;-----------------------------------------------------
    SEG.U VARS
    ORG $80

GAMEMAP0                ds MAPHEIGHT  ;6 columns of the GameMap, 
GAMEMAP1                ds MAPHEIGHT  ;single cells are represented as bits
GAMEMAP2                ds MAPHEIGHT
GAMEMAP3                ds MAPHEIGHT
GAMEMAP4                ds MAPHEIGHT
GAMEMAP5                ds MAPHEIGHT

LAVAMAP0                ds MAPHEIGHT
LAVAMAP1                ds MAPHEIGHT
LAVAMAP2                ds MAPHEIGHT
LAVAMAP3                ds MAPHEIGHT
LAVAMAP4                ds MAPHEIGHT
LAVAMAP5                ds MAPHEIGHT

LADDER1X                ds 1 ;Ladder X positions
LADDER2X                ds 1
LADDER3X                ds 1
LADDER4X                ds 1
LADDER5X                ds 1

PLAYERY                 ds 1  ; Player's Y position
PLAYERX                 ds 1  ; Player's X position

LAVA_POS                ds 1  ;4 bits X, 4 bits Y
LAVA_TIMER              ds 1  ;Hazzard's delay
LAVA_DELAY              ds 1  ;Limit for the timer to increment
LAVA_SLEEP              ds 1  ;Makes lava inactive

OLDPLAYERY              ds 1   ;Fallback data when player colides with a wall
OLDPLAYERX              ds 1
OLDPLAYER_FRAME         ds 1

PLAYER_FLIP             ds 1
RANDOM                  ds 1  ;random 8bit number

PLAYERPTR               ds 2  ;16bit address of the active sprites's frame graphics
PLAYER_FRAME            ds 1  ;frame index

PRIZE_SOUND_INTERVAL    ds 1
DEATH_SOUND_INTERVAL    ds 1
CURRENT_LAVA_COLOR      ds 1
PRIZEX                  ds 1    ;hidden prize's location
CURRENT_PRIZE_Y         ds 1

ROOMS_COMPLETED         ds 1

GAME_STATE              ds 1

SCORE_DIGITS_IDX        ds 3   ;indexes of highscore digits, 4 bits - one digit
BUTTON_PRESSED          ds 1   ;Is joystick button being pressed right now?
GENERATING              ds 1   ;Is the map being generared at the moment?
SCREEN_FRAME            ds 1

TEMPVARS                ds 6

SCORE_PTR               ds DIGITS_PTR_COUNT  ; pointers to digit graphics
;----------------------- 
; Overlays
;
    ORG TEMPVARS
TMPNUM                  ds 1
TMPNUM1                 ds 1
TEMPY                   ds 1
TEMP_X_INDEX            ds 1  ; Temp variable used in drawing, and in input checking

LADDER_LINE_IDX         ds 1
LINE_IDX                ds 1  ; line counter for a map cell

;reusing gamemap0's empty 4 bits
    ORG GAMEMAP0
;4 bit variables (max 16 values)
LAVA_DIR                ds 1
PLAYER_LIVES            ds 1
PRIZEY                  ds 1  ;randomly generated row number where the prize is located (1..4)
LAVA_ACCEL              ds 1  ;timer for lava acceleration

    ORG GAMEMAP3                ;these are used to hide ladders, 0 height - ladder is hidden, non zero, we're going to draw that amount of lines
LADDERHEIGHT1           ds 1
LADDERHEIGHT2           ds 1
LADDERHEIGHT3           ds 1
LADDERHEIGHT4           ds 1
LADDERHEIGHT5           ds 1

    ORG LAVAMAP0
CURRENT_LADDER_Y        ds 1

    ORG LAVAMAP3
LAVA_STUCK              ds 1 ;0 - lava is not stuck
MAP_ORIENTATION         ds 1

;------------------------------------------------------
;                  123 | 5 bytes free
;------------------------------------------------------
    ;           ROM
    SEG
    ORG $F000 ;4K Rom , F8000 = 2K

Reset:

    CLEAN_START ; from macro.h

    lda #1
    sta RANDOM

    lda #%00010100 ;make thin ball-ray and the playfield on top of everything
    sta CTRLPF



    lda #0
    sta SCREEN_FRAME
    sta COLUBK  ;black background everywhere

    sta GAME_STATE

Main:
    jsr Vsync
    jsr VBlank
    jsr Kernel
    jsr Overscan
    jmp Main
;--------------------------
EnterNewMap:
    
    lda ROOMS_COMPLETED
    lsr
    sta TMPNUM
    lda #INITIAL_LAVA_DELAY
    cmp TMPNUM
    bcc SetMinimumDelay
    sec
    sbc TMPNUM
    jmp storeLavaDelay
SetMinimumDelay:
    lda #MINIMUM_LAVA_DELAY
storeLavaDelay:
    sta LAVA_DELAY

    lda MAP_ORIENTATION
    and #$0F
    sta TMPNUM

    ;set player coordinates
    lda #MAX_PLAYER_Y
    sta PLAYERY
    sta OLDPLAYERY

    lda TMPNUM
    beq heroOnLeft
    lda #130
    jmp storePlayerPos
heroOnLeft:
    lda #3
storePlayerPos:
    sta PLAYERX
    sta OLDPLAYERX

    lda #0
    sta LAVA_TIMER

    lda TMPNUM
    sta LAVA_DIR

    lda #INITIAL_LAVA_SLEEP
    sta LAVA_SLEEP

    sta GENERATING
    sta TMPNUM1

    lda TMPNUM
    beq PutLavaOnLeft
    lda #LAVA_START_POS_RIGHT
    jmp StoreLavaPosition
PutLavaOnLeft:
    lda #LAVA_START_POS_LEFT
StoreLavaPosition:
    sta LAVA_POS


    lda #1
    sta GENERATING

    jsr UpdateRandomNumber
    and #11
    tax
    stx TEMP_X_INDEX ; saves the X of prize
    lda PRIZE_X_POSSITIONS,x ; gets an appropriate position for the missile based on X
    sta PRIZEX
    jsr UpdateRandomNumber
    and #3
    clc
    adc #1
    sta PRIZEY
    lda #255
    sta CURRENT_PRIZE_Y


    ldx #0
genloop:
    lda GAMEMAP0,x
    ora #128
    sta GAMEMAP0,x
    lda #$FF

    sta GAMEMAP1,x
    sta GAMEMAP2,x
    sta GAMEMAP4,x
    lda #$F0
    sta GAMEMAP3,x
    lda #%01111111
    sta GAMEMAP5,x
    lda #0
    sta LAVAMAP0,x
    sta LAVAMAP1,x
    sta LAVAMAP2,x
    sta LAVAMAP3,x
    sta LAVAMAP4,x
    sta LAVAMAP5,x
    inx 
    cpx #MAPHEIGHT
    bne genloop

    lda TMPNUM
    beq ClearMapOnLeft
    ;Clearing map on right
    ldx #5
    lda #%00000001
    sta GAMEMAP5,x
    jmp startGenerateLadders
ClearMapOnLeft:
    ldx #5
    lda #0
    sta GAMEMAP0,x
    lda #%00000111
    sta GAMEMAP1,x

startGenerateLadders:
    lda #4                      ;inverted first y position
    sta TEMPY
    ;----------
ladderLoop:                     ;  let's generate a ladder for each of the map rows
    jsr UpdateRandomNumber
    and #11                     ;  limit to 0..11 range
    tax                         ;  and transfer to X register

    ldy TEMPY
    cpy #4
    beq secondRowCheck
    jmp compareLastRow
secondRowCheck:
    cmp #2                      ; don't put ladder in first two cells
    bcc ladderLoop
compareLastRow:
    cpy #0
    beq lastRowCheck
    jmp compareToPrev
lastRowCheck:
    cmp #9                      ;don't let place ladders near the exit point
    bcs ladderLoop
    cmp #2
    bcc ladderLoop

compareToPrev:
    cmp TMPNUM1
    beq ladderLoop
    stx TMPNUM1

    cpx TEMP_X_INDEX            ;compare ladder x with prize x
    beq checkIfLadderYIsTheSame
    jmp saveLadderPostionToRam
checkIfLadderYIsTheSame:
    lda PRIZEY
    and #$0F
    cmp TEMPY
    beq ladderLoop              ;generate ladder one more time

saveLadderPostionToRam:

    lda LADDER_X_POSSITIONS,x
    ldy TEMPY
    sta LADDER1X,y              ;  store sprite position to ram variable


nextLadder:
    dec TEMPY
    bpl ladderLoop


    rts
;------------------------------------------
;X - ladder x
;TEMPY - ladder y
RemoveGroundForLadders:
    lda MAP_3CELLS_LOOKUP,x
    clc
    adc TEMPY
    tay


    lda MAP_3CELLS_INTERSECTIONS,x
    stx TMPNUM                  ;store x

    cmp #1
    beq onlyOneSegmentUsed
                                ;two segments used for a map tile

    lda GAMEMAP0,y + MAPHEIGHT
    and MAP_CLEAR_PATTERN_BY_X_SEG2,x
    sta GAMEMAP0,y + MAPHEIGHT

onlyOneSegmentUsed:

    lda GAMEMAP0,y
    and MAP_CLEAR_PATTERN_BY_X_SEG1,x
    sta GAMEMAP0,y


    rts

;------------------------------------------
; Fake subroutine
drawPlayfield:

    sta COLUPF              ;3 5

    stx TEMP_X_INDEX        ;3 8    save scanline index
    ldx TMPNUM              ;3 11

    lda GAMEMAP0,x          ;4 15
    sta PF0                 ;3 18

    lda GAMEMAP1,x          ;4 22
    sta PF1                 ;3 25


    lda GAMEMAP2,x          ;4 29
    sta PF2                 ;3 32

    ;------right side of the screen

    lda GAMEMAP3,x          ;4 36
    sta PF0                 ;3 39


    lda GAMEMAP4,x          ;4 46
    sta PF1                 ;3 49

    lda GAMEMAP5,x          ;4 55
    sta PF2                 ;3 58

    ;enable missile if a map row matches prize Y

    lda #LADDERHEIGHT   ;has nothing to do with ladders, just like this number
    sbc LINE_IDX        ; subtract current map tile line index
    lsr                 ; A / 2
    lsr                 ; A / 2

    ;A needs to be 2 for missile to be enabled
    
    cpx CURRENT_PRIZE_Y  ;3
    beq enableMissile    ;3
    lda #0               ;2

enableMissile:
    
    ;---------------------------------------------
    sta WSYNC               ;finish the scanline
    ;---------------------------------------------
    nop
    nop
    sta ENAM0               ;3 6  ; enables or disables missile depending on A

    lda CURRENT_LAVA_COLOR  ;3 9
    sta COLUPF              ;3 12

    lda LAVAMAP0,x          ;4 16
    sta PF0                 ;3 19

    lda LAVAMAP1,x          ;4 23
    sta PF1                 ;3 26

    lda LAVAMAP2,x          ;4 30
    sta PF2                 ;3 33

    ;------right side of the screen

    lda LAVAMAP3,x          ;4 37
    sta PF0                 ;3 40

    lda LAVAMAP4,x          ;4 44
    sta PF1                 ;3 47
    
    nop                     ;2 49

    lda LAVAMAP5,x          ;4 53
    sta PF2                 ;3 56

    lda #0                  ;2 58   Lets turn off the playfield for one scanline
    sta PF0                 ;3 61
    sta PF1                 ;3 64

    jmp exitDrawPlayfield   ;3 67


;--------------------------------------------------
Kernel:
    sta WSYNC
    lda INTIM
    bne Kernel ; wait for vsync timer

    sta VBLANK
;------------
    lda GAME_STATE
    cmp #0
    beq drawTitle
    jmp drawGame
drawTitle:
    jsr TitleScreen
    
    jmp endofKernel

drawGame:
;--------------
    sta WSYNC
;--------------
    lda #4              ;2 2
    sta TEMPY           ;3 5  index for the second ladder
    
    ldy #PLAYERHEIGHT   ;2 7
    sty TMPNUM1         ;3 10  store player scanline index

    lda #MAPHEIGHT-1    ;2 12
    sta TMPNUM          ;3 15  bottom of the screenmap

    lda #LINESPERCELL   ;2 17
    sta LINE_IDX        ;3 20

    ldy #0              ;2 22
    sty LADDER_LINE_IDX ;3 25

    lda PLAYER_FLIP     ;3 33
    sta REFP0           ;3 36

    ldx #54             ;2 38 scanlines, max scanlines / 3

KERNEL_LOOP:
    ;------------------------------------------------------------------------
    cpx PLAYERY             ;3 13
    bcs drawThePlayer       ;3 16
    lda LADDER_GFX,y        ;4 20
    sta GRP1                ;3 23
    dey                     ;2 25
    sty LADDER_LINE_IDX     ;3 28


drawThePlayer:

    ldy TMPNUM1             ;3 31
    beq nope                ;2 33   PLAYER_LINE_IDX == 0
    cpx PLAYERY             ;3 36   can we draw the player sprite?
    bcs nope                ;3 39   < PLAYERY

    lda PLAYERCOLORS,y      ;4 43    sets the player colors
    sta COLUP0              ;3 47
    lda (PLAYERPTR),y       ;5 53    let's load a line from a sprite frame
    sta GRP0                ;3 56    and store it to the Player0 sprite
    dec TMPNUM1             ;5 61
nope:
    ldy LADDER_LINE_IDX     ;3 64

    lda #GROUND_COLOR       ;2 66

    ;----------------------------------------------------------------

    sta WSYNC               ;wait for the scanline to be drawn

    ;----------------------------------------------------------------

    jmp drawPlayfield       ;3 3; faking subroutine
    ;----------------------------
                            ;  67
exitDrawPlayfield:

    ldx LINE_IDX            ;3 70   decrement current line count for one map cell
    dex                     ;2 72
    ;---------------------------------------------
    sta WSYNC
    ;---------------------------------------------
    sta PF2                 ;3 3  clearing the remaining playfield register
    bne cont                ;2 5  not all lines for a cell have been drawn, continue

    ldx TEMPY               ;3 8  load ladder index
    lda LADDERHEIGHT1,x     ;4 12  reset the ladder sprite
    and #$0F                ;2 14
    tay                     ;2 16
    dec TEMPY               ;5 21   let's position next ladder, decrease ladder index
    dec TMPNUM              ;5 26   move to next map cell
    ldx #LINESPERCELL       ;2 28   reset line count
    ;---------------------------------------------
    sta WSYNC
    ;---------------------------------------------
cont:
    stx LINE_IDX            ;3 3 save current line count
    ldx TEMP_X_INDEX        ;3 6 restore X (scanline index)

    ;---------------------------------------------
    dex                     ;2 8
    bne KERNEL_LOOP ;       ;2 10
    ;==================================================
doneDrawing:

    jsr DrawScore

    sta WSYNC   ;let's draw an empty line
    sta HMOVE       ;3 3
    lda PLAYER_LIVES
    and #$0F
    tax
    lda LIVES_LOOKUP,x;2 5
    sta NUSIZ0      ;3 8
    sta NUSIZ1      ;3 11
    lda #LIVES_BG   ;2 13
    sta COLUBK      ;3 16

    sta HMCLR       ;3    let's clear HM
    ldy #PLAYERHEIGHT

lives_bar_loop:

    sta WSYNC
    sta HMOVE           ;3 3
    cpx #2
    bcc continueLives2
    lda DWARF_GFX_0,y   ;4 7
    sta GRP0            ;3 10
    cpx #3
    bcc continueLives2
    sta GRP1            ;3 17

    cpx #6
    bne continueLives
    ;6 lives
    SLEEP 22
    lda #0
    sta GRP1
continueLives:
    cpx #4
    bne continueLives2
    ;4 lives
    SLEEP 12
    lda #0
    sta GRP1

continueLives2:

    dey                 ;2 35
    bne lives_bar_loop  ;3

endofKernel:

    rts

;--------------------------------------------------------------
DrawScore:

    lda #0
    sta GRP0
    sta GRP1
    sta REFP0           ;3  turn off mirroring
    sta REFP1

    lda #%00000011      ;2
    sta NUSIZ0          ;3
    sta NUSIZ1          ;3


    sta WSYNC           ;let's draw an empty line
    SLEEP 36
    sta RESP0           ;2 reset sprite pos
    sta RESP1

    sta HMCLR
    lda #$10           ;2  move p2 sprite left a bit
    sta HMP1           ;3

    lda #SCORE_COLOR     ;2 47
    sta COLUP1          ;3 50
    sta COLUP0          ;3 53

    sta WSYNC
    sta HMOVE               ;3 3
    SLEEP 27                ;27 30
    lda #%00000011          ;2 32
    sta NUSIZ0              ;3 35
    sta NUSIZ1              ;3 38
    sta VDELP0              ;3 41
    sta VDELP1              ;3 44
    ldy #PLAYERHEIGHT - 1   ;2 46
    sty TEMP_X_INDEX        ;3 49
    sta HMCLR               ;3 51

score_line_loop:

    lda (SCORE_PTR+2),y     ;5 56
    tax                     ;2 58
    lda (SCORE_PTR),y       ;5 63
    sta WSYNC
    nop
    sty TEMP_X_INDEX        ;3 6
    sta TMPNUM              ;3 9
    lda (SCORE_PTR+10),y    ;5 14
    sta GRP0                ;3 17
    lda (SCORE_PTR+8),y     ;5 22 
    sta GRP1                ;3 25
    lda (SCORE_PTR+6),y     ;5 30
    sta GRP0                ;3 33

    lda (SCORE_PTR+4),y     ;5 38
    ldy TMPNUM              ;3 41 

    sta GRP1                ;3 44
    stx GRP0                ;3 47
    sty GRP1                ;3 50 
    sta GRP0                ;3 53

    ldy TEMP_X_INDEX        ;3
    dey                     ;2
    bpl score_line_loop     ;2 

    lda #0                  ;2
    sta VDELP0              ;3
    sta VDELP1              ;3
    sta HMCLR


    rts


;---------------------------------------------------------------
TitleScreen:

    sta WSYNC

    lda #15 ;15 * 8 = 120
    sta TMPNUM
    ldy #7
titleLoop:
    
    sta WSYNC               ;finish the scanline
    ;---------------------------------------------

    lda TMPNUM             ;3 3
    adc TMPNUM1            ;3 6
    sta COLUPF             ;3 9

    lda TITLE0,y           ;4 13
    sta PF0                ;3 16

    lda TITLE1,y           ;4 20
    sta PF1                ;3 23

    lda TITLE2,y           ;4 27
    sta PF2                ;3 30

    ;------right side of the screen

    lda TITLE3,y           ;4 34
    sta PF0                ;3 37

    lda TITLE4,y           ;4 41
    sta PF1                ;3 44


    lda TITLE5,y           ;4 48
    sta PF2                ;3 51


    ldx TMPNUM
    dex
    stx TMPNUM
    beq decreaseIdx
    jmp titleLoop
decreaseIdx:
    lda #15
    sta TMPNUM
    dey

    bpl titleLoop

    lda #0                  ;2 58   Lets turn off the playfield for one scanline
    sta PF0                 ;3 61
    sta PF1                 ;3 64
    sta PF2

    sta WSYNC

    ldx #20
emptyLoop:
    sta WSYNC
    dex
    bne emptyLoop

    jsr DrawScore

    ldx #21 ;remaining scanlines
emptyLoop1:
    sta WSYNC
    dex
    bne emptyLoop1

    sta WSYNC
    sta HMOVE               ;3 3
    lda #%00000011          ;2 35
    sta NUSIZ0              ;3 38
    sta NUSIZ1              ;3 41
    sta VDELP0              ;3 44
    sta VDELP1              ;3 47
    ldy #12                 ;2 49
    sty TEMP_X_INDEX        ;3 51
    lda #$09
    sta COLUP0
    sta COLUP1

bitmap_line_loop:

    ldy TEMP_X_INDEX        ;3 57
    lda CPR_6,y
    sta TMPNUM
    sta WSYNC
    lda CPR_1,y             ;4 61
    sta GRP0                ;3 64
    nop
    lda CPR_2,y             ;4 68
    sta GRP1                ;3 71

    lda CPR_3,y             ;4 13
    sta GRP0                ;3 16

    nop
    nop
    nop
    ldx CPR_5,y             ;4 27
    lda CPR_4,y             ;4 31
    ldy TMPNUM              ;3 34 

    sta GRP1                ;3 37
    stx GRP0                ;3 40
    sty GRP1                ;3 43
    sta GRP0                ;3 46

    dec TEMP_X_INDEX        ;5 51
    bpl bitmap_line_loop    ;2 53

    lda #0                  ;2
    sta VDELP0              ;3
    sta VDELP1              ;3




    rts
;-------------------------------------------------
;TMPNUM1 - hundreds and thousands
;TEMPY - tens and hundreds of thousands
CheckScoreForBonuses:
    lda TMPNUM1
    and #$0F
    sta TMPNUM

    lda SCORE_DIGITS_IDX+1
    and #$0F
    
    cmp TMPNUM
    bne scoreHasChanged
    jmp noBonuses
scoreHasChanged:
    cmp #5      ;500,1500,2500 etc.
    beq addLife
    jmp noBonuses
addLife:
    lda PLAYER_LIVES
    and #$0F
    tax
    inx
    cpx #MAX_PLAYER_LIVES  ;max lives
    bcs noBonuses
    ;save lives to ram
    stx TMPNUM
    lda PLAYER_LIVES
    and #$F0
    ora TMPNUM
    sta PLAYER_LIVES
    lda #32
    sta PRIZE_SOUND_INTERVAL
noBonuses:
    rts

;---------------------------------------------------------------

Overscan:
    sta WSYNC
    lda #2
    sta VBLANK
    lda #34
    sta TIM64T

    ;some game logic here

    lda GENERATING
    cmp #1
    beq notColliding

    ;----------
    lda DEATH_SOUND_INTERVAL
    bne notColliding        ;if the character dies


    jsr LadderCollision

    ;Let's check PLAYER-PLAYFIELD collision
    bit CXP0FB
    bpl notColliding
    ;so the player is colliding with the playfield


    lda LAVA_SLEEP
    bne dealWithMining
    ;let's check if the player collides with the lava
    jsr CalcLavaCollision

dealWithMining:
    lda PLAYER_FRAME
    cmp #%00011111
    bne notMining
    ;player is coliding with the playfield and swings his axe

    lda PLAYER_FLIP
    cmp #0  ; facing right
    bne checkCellCollision
    lda PLAYERX
    adc #X_OFFSET_TO_RIGHT_FOR_MINING       ;need to add a bit to player x, when it's facing right
    sta PLAYERX

checkCellCollision:

    lda PLAYERX
    ldx #0
divx:
    inx
    sbc #MAP_CELL_WIDTH
    bcs divx
    dex ;playerX / 12 - 1

    cpx #MAPWIDTH
    bcs doneMining ; nope, the x >= MAPWIDTH

    stx TMPNUM;

    lda PLAYERY
    ldx #0
divy:
    cmp #10
    bcc doneDividing
    sec
    sbc #10
    inx
    jmp divy
doneDividing:
    lda PLAYERY
    cmp Y_POSITIONS_WHERE_YOU_CAN_MINE,x
    bne notMining   ;don't let mine while climbing the ladders
    stx TEMPY

    jsr Mine

    lda SCORE_DIGITS_IDX+1
    sta TMPNUM1
    lda SCORE_DIGITS_IDX+2
    sta TEMPY
    lda TEMP_X_INDEX ;score is stored here
    ldx #0
    ldy #0
    jsr IncrementScore
    jsr CheckScoreForBonuses


doneMining:

    ;----   Let's restore the X
    lda PLAYER_FLIP
    cmp #0 ; facing right
    bne notColliding ;let's not reset the animation frame
    lda PLAYERX
    clc
    sbc #X_OFFSET_TO_RIGHT_FOR_MINING
    sta PLAYERX

    jmp notColliding

notMining:           ; some kind of collision

    lda OLDPLAYERX   ; let's restore previous player state before the collision
    sta PLAYERX
    lda OLDPLAYERY
    sta PLAYERY
    lda OLDPLAYER_FRAME
    sta PLAYER_FRAME


notColliding:

    bit CXM0P               ;player vs missile(prize)
    bvs hideDemPrize        ;the 6th bit is set
    jmp OverscanLoop
hideDemPrize:
    lda #32                ;some other value than 255
    sta CURRENT_PRIZE_Y
    sta PRIZE_SOUND_INTERVAL
    lda ROOMS_COMPLETED
    lsr
    clc 
    adc #LAVA_SLEEP_AFTER_PRIZE
    sta LAVA_SLEEP          ;freeze lava
    ;--
    lda SCORE_DIGITS_IDX+1
    sta TMPNUM1
    lda SCORE_DIGITS_IDX+2
    sta TEMPY
    lda #SCORE_FOR_PRIZE
    ldy #0
    ldx #0
    jsr IncrementScore
    jsr CheckScoreForBonuses

;checkLadderCollision:


OverscanLoop:
    sta WSYNC
    lda INTIM
    bne OverscanLoop

    rts
;-----------------------------
LadderCollision:
    bit CXPPMM
    bpl exitLadderCollision

    lda PLAYER_FRAME
    cmp #%00010000
    bne exitLadderCollision

    lda PLAYERX
    clc
    adc #5 ; hacky hack
prepareDivide:
    ldx #0
dividing_x:
    inx
    sec
    sbc #MAP_CELL_WIDTH
    bcs dividing_x
    dex
    cpx #MAPWIDTH
    bcc setLadderXToPlayer
    ldx #MAPWIDTH - 1
setLadderXToPlayer:
    lda PLAYER_X_POSITIONS_BY_MAP_X,x
    sta PLAYERX
    sta OLDPLAYERX

exitLadderCollision:

    rts
;-----------------------------
;TEMPY is row y
Mine:
    lda #0
    sta TEMP_X_INDEX

    ldx TMPNUM

    lda MAP_3CELLS_LOOKUP,x
    clc
    adc TEMPY
    tay ; store the cell offset in Y


    ldx TMPNUM
    lda MAP_3CELLS_INTERSECTIONS,x
    cmp #1
    beq mine_onlyOneSegmentUsed
    ;Let's change the second segment

    lda GAMEMAP0,y + MAPHEIGHT
    and MAP_CLEAR_PATTERN_BY_X_SEG2,x
    sta GAMEMAP0,y + MAPHEIGHT


    lda LAVA_SLEEP
    beq mine_onlyOneSegmentUsed
    lda LAVAMAP0,y + MAPHEIGHT
    and MAP_CLEAR_PATTERN_BY_X_SEG2,x
    sta LAVAMAP0,y + MAPHEIGHT


mine_onlyOneSegmentUsed:

    lda GAMEMAP0,y
    and MAP_FILL_PATTERN_BY_X_SEG1,x
    beq tryMiningLava ; there are no bricks to mine

    lda GAMEMAP0,y
    and MAP_CLEAR_PATTERN_BY_X_SEG1,x
    sta GAMEMAP0,y
    lda #SCORE_FOR_BRICK
    sta TEMP_X_INDEX    ;the score

tryMiningLava:
    lda LAVA_SLEEP
    beq exitMining ;lava is flowing

    lda LAVAMAP0,y
    and MAP_FILL_PATTERN_BY_X_SEG1,x
    beq exitMining ;there's nothing there

    lda LAVAMAP0,y
    and MAP_CLEAR_PATTERN_BY_X_SEG1,x
    sta LAVAMAP0,y
    lda #SCORE_FOR_LAVA
    sta TEMP_X_INDEX ;the score

exitMining:

    rts
;-----------------------------
CalcLavaCollision:

    ;CALC X1
    lda PLAYERX
    ldx #0
    sec
    sbc #4
    cmp PLAYERX
    bcc lava_divx ;the result is less than PLAYERX
    lda #0        ;since the negative value would be bigger
lava_divx:
    cmp #MAP_CELL_WIDTH
    bcc lava_doneDividingX
    sec
    sbc #MAP_CELL_WIDTH
    inx
    jmp lava_divx
lava_doneDividingX:
    cmp #6
    bcc lava_checkBoundsX
    inx

lava_checkBoundsX:
    cpx #MAPWIDTH
    bcc lava_storeX ; the x < MAPWIDTH, let's clamp it

    ldx #MAPWIDTH - 1 ; let's clamp X
lava_storeX:
    stx TMPNUM;
;----
    ;CALC X2
    lda PLAYERX
    ldx #0
    clc
    adc #4
lava_divx_2:
    cmp #MAP_CELL_WIDTH
    bcc lava_doneDividingX_2
    sec
    sbc #MAP_CELL_WIDTH
    inx
    jmp lava_divx_2
lava_doneDividingX_2:
    cmp #6
    bcc lava_checkBoundsX_2
    inx

lava_checkBoundsX_2:
    cpx #MAPWIDTH
    bcc lava_storeX_2 ; the x < MAPWIDTH, let's clamp it

    ldx #MAPWIDTH - 1 ; let's clamp X
lava_storeX_2:
    stx TMPNUM1;

;----
    lda PLAYERY
    ldx #0
lava_divy:
    cmp #10
    bcc lava_doneDividing
    sec
    sbc #10
    inx
    jmp lava_divy
lava_doneDividing:

    stx TEMPY


    ldx TMPNUM

    lda MAP_3CELLS_LOOKUP,x
    clc
    adc TEMPY
    tay ; store the cell offset in Y


    ldx TMPNUM
    lda MAP_3CELLS_INTERSECTIONS,x
    cmp #1
    beq lava_onlyOneSegmentUsed
    ;Let's change the second segment

    lda LAVAMAP0,y + MAPHEIGHT
    ldx TMPNUM              ;load x coord
    and MAP_FILL_PATTERN_BY_X_SEG2,x
    bne PlayerDies


lava_onlyOneSegmentUsed:

    lda LAVAMAP0,y
    ldx TMPNUM
    and MAP_FILL_PATTERN_BY_X_SEG1,x
    bne PlayerDies


;repeat the same check but with X2
    ldx TMPNUM1

    lda MAP_3CELLS_LOOKUP,x
    clc
    adc TEMPY
    tay ; store the cell offset in Y


    ldx TMPNUM1
    lda MAP_3CELLS_INTERSECTIONS,x
    cmp #1
    beq lava_onlyOneSegmentUsed_2
    ;Let's change the second segment

    lda LAVAMAP0,y + MAPHEIGHT
    ldx TMPNUM1              ;load x coord
    and MAP_FILL_PATTERN_BY_X_SEG2,x
    bne PlayerDies


lava_onlyOneSegmentUsed_2:

    lda LAVAMAP0,y
    ldx TMPNUM1
    and MAP_FILL_PATTERN_BY_X_SEG1,x
    bne PlayerDies


    jmp notCollidingWithLava

PlayerDies:
    lda #DEATH_INTERVAL
    sta DEATH_SOUND_INTERVAL

notCollidingWithLava:

    rts
;-------------------------

;Vertical Sync, 3 scanlines long
;The frame drawing is done
Vsync:

    lda #2
    ldx #49
    sta WSYNC
    sta VSYNC
    stx TIM64T
    sta CXCLR   ;clear the collision registers

    sta WSYNC

    lda #LADDERS_COLOR ; reset the ladder color after the sprite was used to draw the score
    sta COLUP1

    sta WSYNC

    lda #0
    sta COLUBK ;reset the background color
    sta NUSIZ1
    lda #%00110000  ;make the missile thick
    sta NUSIZ0      ;reset sprite special params


    sta WSYNC
    sta VSYNC
    rts

;----------------------------
IncrementScore:
    clc
    sed
    sta TMPNUM 
    lda SCORE_DIGITS_IDX
    adc TMPNUM
    sta SCORE_DIGITS_IDX
    stx TMPNUM
    lda SCORE_DIGITS_IDX+1
    adc TMPNUM
    sta SCORE_DIGITS_IDX+1
    sty TMPNUM
    lda SCORE_DIGITS_IDX+2
    adc TMPNUM
    sta SCORE_DIGITS_IDX+2
    cld
    rts
;---------------------------
IncreaseTheFrame
    ldx PLAYER_FRAME
    inx
    cpx #16 ;we must not let go to animation frame 3, 00010 000
    bcc storeframe1
    ldx #0
storeframe1:
    lda PLAYER_FRAME
    sta OLDPLAYER_FRAME
    stx PLAYER_FRAME
    rts
;---------------------------
ProcessInput:

    lda SWCHA
    asl         ;shift left, bit might fall into CARRY flag
    sta TMPNUM
    bcs checkLeft
    lda TMPNUM1
    bne checkLeft
moveRight:
    lda PLAYER_FLIP
    beq itWasFacingRightAlready ; if facing right
    lda #%00000000
    sta REFP0
    sta PLAYER_FLIP
    lda #12
    sta PLAYER_FRAME

    lda PLAYERX; add +2 to prevent getting stuck in a wall
    clc
    adc #2
    sta PLAYERX

itWasFacingRightAlready:

    lda PLAYERX
    clc
    adc #1
    cmp #MAX_PLAYER_X
    bcs checkLeft
  
    ldx PLAYERX
    stx OLDPLAYERX
    ldx PLAYERY
    stx OLDPLAYERY
    sta PLAYERX
 

    ;so right was activated, let's skip left direction
    lda TMPNUM
    asl
    sta TMPNUM

    
    jsr IncreaseTheFrame
    jmp checkButton


checkLeft:
    lda TMPNUM
    asl
    sta TMPNUM
    bcs checkDown
    lda TMPNUM1
    bne checkDown
moveLeft:

    lda PLAYER_FLIP
    cmp #8; facing left
    beq itwasFacingLeftAlready
    lda #%00001000
    sta REFP0
    sta PLAYER_FLIP
    lda #12
    sta PLAYER_FRAME
    dec PLAYERX
    dec PLAYERX
itwasFacingLeftAlready:

    lda PLAYERX
    sec
    sbc #1
    cmp #1
    bcc checkDown
    ldx PLAYERX
    stx OLDPLAYERX
    ldx PLAYERY
    stx OLDPLAYERY
    sta PLAYERX

    jsr IncreaseTheFrame
    jmp checkButton

checkDown:
    lda TMPNUM
    asl
    sta TMPNUM
    bcs checkButton
moveDown:
    lda PLAYERY
    sec
    sbc #1
    cmp #MIN_PLAYER_Y
    bcc checkButton
    beq setMinY
    ldx PLAYERY
    jmp storeOldY
setMinY:
    tax
storeOldY:
    stx OLDPLAYERY
    sta PLAYERY
    lda OLDPLAYER_FRAME
    cmp #%00010000
    beq movingDownAlready
    lda #%00010000  ; after 3 x lsr it will turn into 2(3rd frame - climbing)
    ldx PLAYER_FRAME
    stx OLDPLAYER_FRAME
    sta PLAYER_FRAME

    ldx PLAYERX
    stx OLDPLAYERX
    lda PLAYER_FLIP
    cmp #8
    bne goDownWhenFacingRight
    inx
    inx
    jmp wentDown
goDownWhenFacingRight:
    dex
    dex
wentDown:
    stx PLAYERX

movingDownAlready:

    jmp noInput
checkButton:
;----------------------------------------------
    bit INPT4   ;checking button press;
    bmi buttonNotPressed ;jump if the button wasn't pressed
    ;----

    lda BUTTON_PRESSED
    cmp #1
    beq noInput
    lda #1
    sta BUTTON_PRESSED
    
    lda #%00011111  ; after 3 x lsr it will turn into 3(4th frame - mining)
    sta PLAYER_FRAME

    
    lda #2
    sta AUDC0
    lda #1
    sta AUDF0
    lda #6
    sta AUDV0

    jmp noInput

buttonNotPressed:
    lda #0
    sta BUTTON_PRESSED

noInput:

    rts

;------------------------------------------

UpdateRandomNumber:

    lda RANDOM
    lsr 
    bcc noeor
    eor #$B4
noeor:
    sta RANDOM

    rts
;-------------------------
SetSpriteXPos
    sty WSYNC
    bit 0           ;3 3    waste 3 cycles
    sec             ;2 5    set carry flag
DivideLoop
    sbc #15         ;2 7    subtract 15
    bcs DivideLoop  ;2 9    branch until negative
    eor #7          ;2 11   calculate fine offset
    asl             ;2 13
    asl             ;2 15
    asl             ;2 17
    asl             ;2 19

    sta HMP0,x
    sta RESP0,x


    rts
;------------------------
;assumes that lava Y is in TMPNUM
CheckLowerGroundTile:


    ; check if there's a hole below
    lda MAP_3CELLS_LOOKUP,x
    clc
    adc TMPNUM
    sec
    sbc #1
    tay

    lda GAMEMAP0,y
    and MAP_FILL_PATTERN_BY_X_SEG1,x
    bne notlower

    lda LAVAMAP0,y
    and MAP_FILL_PATTERN_BY_X_SEG1,x
    beq mustLowerLava

notlower:
    lda #0
    jmp finishLowerGroundTileCheck

mustLowerLava:
    lda #1

finishLowerGroundTileCheck:

    rts
;-------------------------
FillInLavaTile:
    lda LAVA_POS
    and #$F0
    lsr
    lsr
    lsr
    lsr
    tax ;load lava x to x

    ;-----
    ;get the lava Y
    lda LAVA_POS
    and #$0F
    sta TMPNUM  ;store it in TMPNUM

    lda MAP_3CELLS_LOOKUP,x

    clc
    adc TMPNUM

    tay ; store the cell offset in Y

    lda MAP_3CELLS_INTERSECTIONS,x
    cmp #1
    beq onlyONE
    ;Let's change the second segment

    lda LAVAMAP0,y + MAPHEIGHT  ; adding MAPHEIGHT helps to reach the next column
    ora MAP_FILL_PATTERN_BY_X_SEG2,x
    sta LAVAMAP0,y + MAPHEIGHT

onlyONE:
    lda LAVAMAP0,y
    ora MAP_FILL_PATTERN_BY_X_SEG1,x
    sta LAVAMAP0,y

    lda #0
    sta AUDV1

    rts
;------------------------
StoreLavaPosX
    txa
    asl
    asl
    asl
    asl
    sta TMPNUM
    lda LAVA_POS
    and #$0F
    ora TMPNUM

    sta LAVA_POS
    ;done storing x
    rts
;-------------------------
AccelerateLava:
    lda LAVA_ACCEL
    and #$0F
    tax
    inx
    cpx #2
    stx TMPNUM
    lda LAVA_ACCEL
    and #$F0
    ora TMPNUM
    sta LAVA_ACCEL
    bcs reduce_delay
    jmp exitAcceleration
reduce_delay:
    lda LAVA_ACCEL
    and #$F0
    sta LAVA_ACCEL
    ldx LAVA_DELAY
    dex
    cpx #MINIMUM_LAVA_DELAY
    bcc exitAcceleration
    stx LAVA_DELAY
exitAcceleration:
    rts
;------------------------
;Fix the lava postion after it was mined
CorrectTheLavaPos:

    lda #LAVA_COLOR_BEFORE
    sta CURRENT_LAVA_COLOR

    lda LAVA_POS
    lsr
    lsr
    lsr
    lsr
    tax
    lda LAVA_POS
    and #$0F
    sta TEMPY
    lda LAVA_DIR
    and #$0F
    sta TMPNUM1
    lda LAVA_STUCK
    and #$F0
    sta LAVA_STUCK
    
    jmp moveThePosX
changeLavaDir:
    lda LAVA_DIR
    and #$0F
    cmp #0
    beq increaseDir
    dec LAVA_DIR
    jmp moveThePosX
increaseDir:
    inc LAVA_DIR

moveThePosX:
    stx TMPNUM
    lda LAVA_DIR
    and #$0F
    cmp #0 ;0 - left; 1 - right
    bne movePositionRight
    dex
    bmi changeLavaDir
    jmp checkForBackLava
movePositionRight:
    inx
    cpx #MAPWIDTH
    bcs changeLavaDir

checkForBackLava:
    lda MAP_3CELLS_LOOKUP,x
    clc
    adc TEMPY
    tay ; store the cell offset in Y


    lda LAVAMAP0,y
    and MAP_FILL_PATTERN_BY_X_SEG1,x
    beq thereWasNoLava
    jmp exitLavaPosUpdate

thereWasNoLava: 
    ;maybe there is a wall ?
    lda GAMEMAP0,y
    and MAP_FILL_PATTERN_BY_X_SEG1,x
    bne changeLavaDir


    
    ldy TEMPY
    iny
    cpy #MAPHEIGHT
    bcs exitLavaPosUpdate
    sty TEMP_X_INDEX

    lda MAP_3CELLS_LOOKUP,x
    clc
    adc TEMP_X_INDEX
    tay ; store the cell offset in Y

    lda LAVAMAP0,y
    and MAP_FILL_PATTERN_BY_X_SEG1,x
    beq moveThePosX
    jmp exitLavaPosUpdate

revertMovement:
    ldx TMPNUM

exitLavaPosUpdate:
    txa
    asl
    asl
    asl
    asl
    and #$F0
    ora TEMPY
    sta LAVA_POS
    lda LAVA_DIR
    and #$F0
    ora TMPNUM1
    sta LAVA_DIR
    rts


;-------------------------
LavaLogic:

    ldy LAVA_TIMER
    iny
    lda #11
    sta AUDC1
    lda LAVA_TIMER
    sta AUDF1
    lda #6
    sta AUDV1

    cpy LAVA_DELAY ;if LAVA_TIMER reaches the value of LAVA_DELAY, lava starts to move
    bcc lavaDelay


    ldx LAVA_SLEEP
    cpx #0
    bne lavaSlumber
    jmp moveTheLava
lavaSlumber:
    dex
    stx LAVA_SLEEP
    cpx #0
    beq fixLavaPos
    jmp notFix
fixLavaPos:
    jsr CorrectTheLavaPos ; check if lava was disconnected from it's flow by the player
notFix:
    ldy #0
    jmp lavaDelay


moveTheLava:
    jsr AccelerateLava
    jsr FillInLavaTile
;----

    lda LAVA_STUCK
    and #$0F
    cmp #2
    bcs raiseLava

    ;get the lava Y
    lda LAVA_POS
    and #$0F
    sta TMPNUM  ;store it in TMPNUM
    cmp #0
    beq moveLavaHorizontaly ; lava is at 0 row
    jsr CheckLowerGroundTile
    cmp #1
    beq lowerLava

    ;----
moveLavaHorizontaly:
    ldy #0

    lda LAVA_DIR
    and #$0F
    cmp #0
    bne moveLavaLeft
    cpx #MAX_LAVA_X
    bcs rightSideReached
    inx
;-------
    jsr CheckGroundTiles
    cmp #1
    beq rightSideReached
;-------
    jmp storeLavaX

moveLavaLeft:
    dex
;------
    jsr CheckGroundTiles
    cmp #1
    beq leftSideReached
;------
    cpx #255 ; "-1" :)
    beq leftSideReached

storeLavaX:
    jsr StoreLavaPosX
    jmp lavaDelay

leftSideReached:
    dec LAVA_DIR
    inc LAVA_STUCK
    jmp lavaDelay
rightSideReached:
    ldy #0
    inc LAVA_DIR
    inc LAVA_STUCK
    jmp lavaDelay
raiseLava:
    jsr LavaMovesUp
    jmp lavaDelay
lowerLava:
    jsr lavaMovesDown
lavaDelay:
    sty LAVA_TIMER

    rts
;--------------------------------------
lavaMovesDown:

    ldy #0 ;reset timer
    lda LAVA_POS
    and #$0F
    cmp #0
    beq exitLavaMoveDown ; don't let the lava to overflow
    sec
    sbc #1
    sta TMPNUM
    lda LAVA_POS
    and #$F0
    ora TMPNUM
    sta LAVA_POS
    lda LAVA_STUCK  ; if lava can move down, it's not stuck
    and #$F0
    sta LAVA_STUCK

exitLavaMoveDown:

    rts
;--------------------------------------
LavaMovesUp:
    ldy #0
    lda LAVA_POS
    and #$0F
    cmp #MAPHEIGHT-1
    bcs exitLavaMovesUp
    clc
    adc #1
    sta TMPNUM
    lda LAVA_POS
    and #$F0
    ora TMPNUM
    sta LAVA_POS

    lda LAVA_STUCK
    and #$F0
    sta LAVA_STUCK

exitLavaMovesUp:
    rts
;--------------------------------------
;checks if there is ground tile on X
; A = 1 ir there is, 0 if not
;lets assume lava y is in TMPNUM
CheckGroundTiles:

    ; check if it's possible to move right
    lda MAP_3CELLS_LOOKUP,x
    clc
    adc TMPNUM; add lava y
    tay
    
    lda MAP_3CELLS_INTERSECTIONS,x
    cmp #1
    beq check_seg1
    ;Let's change the second segment

    lda GAMEMAP0,y + MAPHEIGHT  ; adding MAPHEIGHT helps to reach the next column
    eor MAP_CLEAR_PATTERN_BY_X_SEG2,x
    and MAP_FILL_PATTERN_BY_X_SEG2,x
    bne sideReached

check_seg1:
    lda GAMEMAP0,y
    eor MAP_CLEAR_PATTERN_BY_X_SEG1,x
    and MAP_FILL_PATTERN_BY_X_SEG1,x
    bne sideReached

    ldy #0
    lda #0
    jmp exitGroundCheck
sideReached:
    lda #1
exitGroundCheck:
    rts;

;-----------------------------------------------------
UpdateSpriteFrames:

    ;set the frame for the sprite
    lda PLAYER_FRAME
    lsr ;shift left by 3 bits 00000111 -> 0, 00001111 -> 1
    lsr
    lsr
    ;we have to skip 8 screen frames to get first animation frame change
    tax
    lda DWARF_PTR_LOW,x
    sta PLAYERPTR
    lda DWARF_PTR_HIGH,x
    sta PLAYERPTR+1

;--------- update score digits

    ldy #0

digitsUpdate:

    sty TMPNUM ;store Y
    tya        ; Y -> A
    lsr        ; A / 2
    lsr        ; A / 2
    tay        ; A -> Y
    lda SCORE_DIGITS_IDX,y
    and #$F
    tax

    ldy TMPNUM ;restore Y
    lda DIGITS_PTR_LOW,x
    sta SCORE_PTR,y
    iny
    lda DIGITS_PTR_HIGH,x
    sta SCORE_PTR,y

    ldy TMPNUM ;restore Y
    tya        ; Y -> A
    lsr        ; A / 2
    lsr        ; A / 2
    tay        ; A -> Y

    lda SCORE_DIGITS_IDX,y
    and #$F0
    lsr
    lsr
    lsr
    lsr
    tax

    ldy TMPNUM ; restore index

    iny
    iny
    lda DIGITS_PTR_LOW,x
    sta SCORE_PTR,y
    iny
    lda DIGITS_PTR_HIGH,x
    sta SCORE_PTR,y

    iny

    cpy #DIGITS_PTR_COUNT
    bcc digitsUpdate

    rts
;----------------------------------------
;Make the prize appear or disappear depending on the player character's position
UpdatePrize:

    lda CURRENT_PRIZE_Y
    cmp #255
    bne dontUpdatePrize

    lda PRIZEY
    and #$0F        ;prizey is lower 4 bits
    tay
    lda Y_POSITIONS_WHERE_YOU_CAN_MINE,y
    cmp PLAYERY     ;is player at the same level as the prize?
    bne hidePrize   ;hide prize if not

    lda PRIZEX      ;procceed to compare prize's x coord
    sec
    sbc #MIN_DISTANCE_FROM_PRIZE

    bmi negativeX
    jmp comparePlayerX
negativeX:
    lda #0
comparePlayerX:
    cmp PLAYERX
    bcs hidePrize
    lda PRIZEX
    clc
    adc #MIN_DISTANCE_FROM_PRIZE
    cmp PLAYERX
    bcc hidePrize
    lda PRIZEY
    and #$0F
    jmp contUpdateX
hidePrize:
    lda #255

contUpdateX:
    sta CURRENT_PRIZE_Y
dontUpdatePrize:

    rts
;------------------------------------------------------
;Make the ladders appear when the player is above them
UpdateLadders:

    ldy #5
loopThroughLadders:

    lda Y_POSITIONS_WHERE_YOU_CAN_MINE,y
    cmp PLAYERY
    beq playerRowFound

    dey
    bpl loopThroughLadders

    jmp exitLadderUpdate

playerRowFound:
    dey
    bmi exitLadderUpdate
    sty TEMPY
    lda LADDER1X,y
    tay

    sec
    sbc #MIN_DISTANCE_FROM_PRIZE
    
    bmi ladder_negativeX
    jmp ladder_comparePlayerX
ladder_negativeX:
    lda #0
ladder_comparePlayerX:
    cmp PLAYERX
    bcs exitLadderUpdate

    tya
    clc
    adc #MIN_DISTANCE_FROM_PRIZE
    cmp PLAYERX
    bcc exitLadderUpdate

    dey
    dey

    ldx LADDER_SPRITE_X_TO_CELL_X,y ; get that ladder X
    
    ldy TEMPY
    lda CURRENT_LADDER_Y
    and #$F0
    ora TEMPY
    sta CURRENT_LADDER_Y
    lda LADDERHEIGHT1,y
    ora #$0A
    sta LADDERHEIGHT1,y
    jsr RemoveGroundForLadders
    lda LADDER_X_POSSITIONS,x
    jmp realyExit
exitLadderUpdate:
    lda CURRENT_LADDER_Y
    and #$0F
    tay
    lda LADDER1X,y
    
realyExit:
    rts
;------------------------------------------------------
TitleLogic

    bit INPT4   ;checking button press;
    bmi titleButtonNotPressed ;jump if the button wasn't pressed
    ;----

    lda BUTTON_PRESSED
    cmp #1
    beq titleNoInput
    lda #1
    sta BUTTON_PRESSED
    
    lda #1
    sta GAME_STATE
    lda #INITIAL_PLAYER_LIVES
    sta PLAYER_LIVES
    lda #0
    sta SCORE_DIGITS_IDX
    sta SCORE_DIGITS_IDX+1
    sta SCORE_DIGITS_IDX+2
    sta ROOMS_COMPLETED

    lda #LAVA_COLOR_BEFORE
    sta CURRENT_LAVA_COLOR

    jsr EnterNewMap

    jmp titleNoInput

titleButtonNotPressed:
    lda #0
    sta BUTTON_PRESSED

titleNoInput:

    ldx TMPNUM1
    inx
    stx TMPNUM1

    jsr UpdateSpriteFrames

    rts

;-----------------------------------------------------
ResetTheGame:

    lda #0
    sta GAME_STATE
    rts

;------------------------------------------------------
AnimateLavaColor:
    lda LAVA_SLEEP
    bne setFrozenLava
    jmp continueLavaAnim
setFrozenLava:
    ldx #LAVA_FROZEN_COLOR
    jmp gameOverTimer
continueLavaAnim:
    ldx CURRENT_LAVA_COLOR
    lda LAVA_TIMER
    lsr
    cmp #2
    bcs gameOverTimer
    dex
    cpx #LAVA_COLOR_AFTER
    beq resetLavaColor
    jmp gameOverTimer
resetLavaColor:
    ldx #LAVA_COLOR_BEFORE
gameOverTimer:
    stx CURRENT_LAVA_COLOR
;--

    rts
;-----------------------------------------------------
CheckIfPlayerAtTheBottomOfRow:
    lda PLAYERY
    ldx #0
PI_divy:
    cmp #10
    bcc processInput_doneDiv
    sec
    sbc #10
    inx
    jmp PI_divy
processInput_doneDiv:
    lda PLAYERY
    cmp Y_POSITIONS_WHERE_YOU_CAN_MINE,x
    bne dontLetMoveHorizontaly
    lda #0
    jmp processInput_storeTmpnum1
dontLetMoveHorizontaly:
    lda #255
processInput_storeTmpnum1:
    sta TMPNUM1

    rts
;--------------------------------------
PlayPrizeSound:
    ldx PRIZE_SOUND_INTERVAL
    beq exitPlayPrizeSound
    stx AUDF0
    lda #8
    sta AUDV0
    lda #4
    sta AUDC0
    dex
    stx PRIZE_SOUND_INTERVAL
exitPlayPrizeSound:
    rts
;----------------------------------------------
PlayDeathSound:
    tax
    dex
    lda #8
    sta AUDC1
    lda #DEATH_INTERVAL
    sec
    sbc DEATH_SOUND_INTERVAL
    lsr
    sta AUDF1
    lda #14
    sta AUDV1

    rts

;------------------------------------------------------
VBlank:
    
    jsr UpdateRandomNumber

    lda GAME_STATE
    cmp #0
    beq runTitleLogic
    jmp GameLogic
runTitleLogic:
    jsr TitleLogic
    jmp exitVBlank

GameLogic:
    jmp there
ResetThatGame:
    ldx #0
    stx DEATH_SOUND_INTERVAL
    stx AUDV1
    ldx PLAYER_LIVES
    dex
    stx PLAYER_LIVES
    txa
    and #$0F
    cmp #0
    bne enterNew
    jsr ResetTheGame
there:
    lda GENERATING
    cmp #1
    beq notReached
    
    jsr PlayPrizeSound
   
    jsr AnimateLavaColor

    lda DEATH_SOUND_INTERVAL
    beq noGameOver
    jsr PlayDeathSound
continueGameOver:
    cpx #0
    beq ResetThatGame
    stx DEATH_SOUND_INTERVAL
    jmp updatePlayerSpriteX

noGameOver:
    jsr LavaLogic

movePlayer:
    lda PRIZE_SOUND_INTERVAL
    bne checkButtonPress
    lda #0
    sta AUDV0
checkButtonPress:

    jsr CheckIfPlayerAtTheBottomOfRow

    jsr ProcessInput

updatePlayerSpriteX:

    jsr UpdatePrize
    jsr UpdateLadders

    ldx #1
    jsr SetSpriteXPos

    lda PLAYERX
    ldx #0
    jsr SetSpriteXPos
    lda PRIZEX
    ldx #2
    jsr SetSpriteXPos

    sta WSYNC
    sta HMOVE
    
    ldy SCREEN_FRAME
    iny
    cpy #2
    bcc continueVBlank
    ldy #0
continueVBlank:
    sty SCREEN_FRAME


    jsr UpdateSpriteFrames

    lda PLAYERX
    cmp #GOALX1
    bcs checkSecondGoal
    lda MAP_ORIENTATION
    and #$F0
    ora #1
    sta MAP_ORIENTATION
    jmp checkGoalY
checkSecondGoal:
    cmp #GOALX2
    bcc notReached
    lda MAP_ORIENTATION
    and #$F0
    sta MAP_ORIENTATION
checkGoalY:
    lda PLAYERY
    cmp #GOALY
    bcs notReached
    inc ROOMS_COMPLETED
enterNew:
    jsr EnterNewMap
notReached:

    lda #0
    sta GENERATING

exitVBlank:

    rts
;--------------------------------------------
    if (* & $FF)
        echo "------", [$FCF3 - *]d, "bytes free before End of code"
        align 256
    endif
;--------------------------------------------
;ROM constants

    ORG $FCF3
;--------------------------------------------
DIGITS_PTR_LOW:
    .byte <(ZERO_GFX)
    .byte <(ONE_GFX)
    .byte <(TWO_GFX)
    .byte <(THREE_GFX)
    .byte <(FOUR_GFX)
    .byte <(FIVE_GFX)
    .byte <(SIX_GFX)
    .byte <(SEVEN_GFX)
    .byte <(EIGHT_GFX)
    .byte <(NINE_GFX)

DIGITS_PTR_HIGH:
    .byte >(ZERO_GFX)
    .byte >(ONE_GFX)
    .byte >(TWO_GFX)
    .byte >(THREE_GFX)
    .byte >(FOUR_GFX)
    .byte >(FIVE_GFX)
    .byte >(SIX_GFX)
    .byte >(SEVEN_GFX)
    .byte >(EIGHT_GFX)
    .byte >(NINE_GFX)


DWARF_PTR_LOW:  ; low 8bits of 16bit address
    .byte <(DWARF_GFX_0)
    .byte <(DWARF_GFX_1)
    .byte <(DWARF_GFX_2)
    .byte <(DWARF_GFX_3)
DWARF_PTR_HIGH: ; high 8bits of 16bit address
    .byte >(DWARF_GFX_0)
    .byte >(DWARF_GFX_1)
    .byte >(DWARF_GFX_2)
    .byte >(DWARF_GFX_3)

;-------------------------------------------

ZERO_GFX:
    .byte %00000000
    .byte %00111100
    .byte %01100010
    .byte %01010010
    .byte %01010010
    .byte %01001010
    .byte %01001010
    .byte %01000110
    .byte %00111100

ONE_GFX:
    .byte %00000000
    .byte %01111110
    .byte %00011000
    .byte %00011000
    .byte %00011000
    .byte %01011000
    .byte %00111000
    .byte %00011000
    .byte %00001000

TWO_GFX:
    .byte %00000000
    .byte %01111110
    .byte %00110000
    .byte %00011000
    .byte %00001100
    .byte %00000110
    .byte %01000110
    .byte %01000110
    .byte %00111100

THREE_GFX:
    .byte %00000000
    .byte %00111100
    .byte %01000110
    .byte %01000110
    .byte %00011100
    .byte %00011110
    .byte %01000110
    .byte %01000110
    .byte %00111100

FOUR_GFX:
    .byte %00000000
    .byte %00001100
    .byte %00001100
    .byte %00001100
    .byte %11111110
    .byte %01001100
    .byte %00101100
    .byte %00011100
    .byte %00001100

FIVE_GFX:
    .byte %00000000
    .byte %00111100
    .byte %01000110
    .byte %00000110
    .byte %00000110
    .byte %01111100
    .byte %01000000
    .byte %01000000
    .byte %01111110

SIX_GFX:
    .byte %00000000
    .byte %00111100
    .byte %01100110
    .byte %01100110
    .byte %01100110
    .byte %01111100
    .byte %01100000
    .byte %00110000
    .byte %00011100

SEVEN_GFX:
    .byte %00000000
    .byte %00011000
    .byte %00011000
    .byte %00011000
    .byte %00011000
    .byte %00001100
    .byte %00001100
    .byte %00000110
    .byte %01111110

EIGHT_GFX:
    .byte %00000000
    .byte %00111100
    .byte %01100110
    .byte %01100110
    .byte %00111100
    .byte %00111100
    .byte %01100110
    .byte %01100110
    .byte %00111100

NINE_GFX:
    .byte %00000000
    .byte %00111000
    .byte %00001100
    .byte %00000110
    .byte %00111110
    .byte %01000110
    .byte %01000110
    .byte %01000110
    .byte %00111100

DWARF_GFX_0:
    .byte %00000000
    .byte %00000000
    .byte %01000100
    .byte %00101000
    .byte %00110000
    .byte %10110100
    .byte %10111000
    .byte %01110000
    .byte %00100000
    .byte %00110000

DWARF_GFX_1:
    .byte %00000000
    .byte %00000000
    .byte %00110000
    .byte %00100000
    .byte %00110000
    .byte %01010000
    .byte %01010000
    .byte %01110000
    .byte %00100000
    .byte %00110000

DWARF_GFX_2:
    .byte %00000000
    .byte %00000000
    .byte %00100100
    .byte %00100100
    .byte %00011000
    .byte %00011000
    .byte %01011010
    .byte %00111100
    .byte %00011000
    .byte %00011000

DWARF_GFX_3:
    .byte %00000000
    .byte %00000000
    .byte %01000100
    .byte %00101000
    .byte %00110000
    .byte %10110101
    .byte %10111101
    .byte %01110011
    .byte %00100110
    .byte %00110000

LADDER_GFX:
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %01000010
    .byte %01111110
    .byte %01000010
    .byte %01111110
    .byte %01000010
    .byte %01111110
    .byte %01000010
    .byte %00000000

FINE_ADJUST_BEGIN:      ;HMPx sprite movement lookup table
    .byte %01110000; Left 7 
    .byte %01100000; Left 6
    .byte %01010000; Left 5
    .byte %01000000; Left 4
    .byte %00110000; Left 3
    .byte %00100000; Left 2
    .byte %00010000; Left 1
    .byte %00000000; No movement.
    .byte %11110000; Right 1
    .byte %11100000; Right 2
    .byte %11010000; Right 3
    .byte %11000000; Right 4
    .byte %10110000; Right 5
    .byte %10100000; Right 6
    .byte %10010000; Right 7

FINE_ADJUST_TABLE EQU FINE_ADJUST_BEGIN - %11110001; %11110001 = -15

LADDER_X_POSSITIONS: ;x positions for ladder sprites based on the location in the map row
    .byte 2
    .byte 14
    .byte 26
    .byte 38
    .byte 50
    .byte 62
    .byte 74
    .byte 86
    .byte 98
    .byte 110
    .byte 122
    .byte 134

MAP_3CELLS_INTERSECTIONS: ;In how many playfield segments a map tile resides
    .byte 2 ;0
    .byte 1 ;1
    .byte 1 ;2
    .byte 1 ;3
    .byte 1 ;4
    .byte 2 ;5
    .byte 1 ;6
    .byte 1 ;7
    .byte 1 ;8
    .byte 2 ;9
    .byte 1 ;10
    .byte 1 ;11

MAP_3CELLS_LOOKUP: ;An offset from the start of GAMEMAP0 based on X
    .byte 0 ;0
    .byte 6 ;1
    .byte 6 ;2
    .byte 12 ;3
    .byte 12 ;4
    .byte 12 ;5
    .byte 18 ;6
    .byte 24 ;7
    .byte 24 ;8
    .byte 24 ;9
    .byte 30 ;10
    .byte 30 ;11

MAP_CLEAR_PATTERN_BY_X_SEG1:    ; lookup tables for map cell destruction, they contains a pattern used with AND
    .byte %01111111 ;0 first part
    .byte %11000111 ;1
    .byte %11111000 ;2
    .byte %11111000 ;3
    .byte %11000111 ;4
    .byte %00111111 ;5 first part
    .byte %00011111 ;6
    .byte %00011111 ;7
    .byte %11100011 ;8
    .byte %11111100 ;9 first part
    .byte %11110001 ;10
    .byte %10001111 ;11

MAP_CLEAR_PATTERN_BY_X_SEG2:    ;some cell columns(only 3) go through two playfield registers, this is the second part
    .byte %00111111 ;0 second part
    .byte %00000000 ;1
    .byte %00000000 ;2
    .byte %00000000 ;3
    .byte %00000000 ;4
    .byte %11101111 ;5 second part
    .byte %00000000 ;6
    .byte %00000000 ;7
    .byte %00000000 ;8
    .byte %11111110 ;9 second part
    .byte %00000000 ;10
    .byte %00000000 ;11

LIVES_LOOKUP:
    .byte 0 ;0 lives
    .byte 0 ;1 life
    .byte 0 ;2 lives
    .byte 0 ;3 lives
    .byte 1 ;4 lives
    .byte 1 ;5 lives
    .byte 3 ;6 lives
    .byte 3 ;7 lives

    ORG $FDF2 ; next data page

MAP_FILL_PATTERN_BY_X_SEG1:    ; lookup tables for filling lava in the lava map, they contains a pattern used with ORA
    .byte %10000000 ;0 first part
    .byte %00111000 ;1
    .byte %00000111 ;2
    .byte %00000111 ;3
    .byte %00111000 ;4
    .byte %11000000 ;5 first part
    .byte %11100000 ;6
    .byte %11100000 ;7
    .byte %00011100 ;8
    .byte %00000011 ;9 first part
    .byte %00001110 ;10
    .byte %01110000 ;11


MAP_FILL_PATTERN_BY_X_SEG2:    ;some cell columns(only 3) go through two playfield registers, this is the second part
    .byte %11000000 ;0 second part
    .byte %00000000 ;1
    .byte %00000000 ;2
    .byte %00000000 ;3
    .byte %00000000 ;4
    .byte %00010000 ;5 second part
    .byte %00000000 ;6
    .byte %00000000 ;7
    .byte %00000000 ;8
    .byte %00000001 ;9 second part
    .byte %00000000 ;10
    .byte %00000000 ;11


Y_POSITIONS_WHERE_YOU_CAN_MINE:
    .byte 9   ;bottom row
    .byte 18
    .byte 27
    .byte 36
    .byte 45
    .byte 54  ;top row

PLAYER_X_POSITIONS_BY_MAP_X:
    .byte 2         ;0
    .byte 14
    .byte 26
    .byte 38
    .byte 50
    .byte 62
    .byte 74
    .byte 86
    .byte 98
    .byte 110
    .byte 122
    .byte 134       ;11

PLAYERCOLORS:
    .byte $1A
    .byte $1A
    .byte $1A
    .byte $1A
    .byte $12
    .byte 15
    .byte 15
    .byte 15
    .byte 15
    .byte 15

PRIZE_X_POSSITIONS:
    .byte 3
    .byte 15
    .byte 27
    .byte 39
    .byte 51
    .byte 63
    .byte 75
    .byte 87
    .byte 99
    .byte 111
    .byte 123
    .byte 135

TITLE0:
    .byte %10000000
    .byte %10000000
    .byte %10000000
    .byte %10000000
    .byte %10000000
    .byte %10000000
    .byte %10000000
    .byte %10000000
TITLE1:
    .byte %11100001
    .byte %00010010
    .byte %00001010
    .byte %00001010
    .byte %00001010
    .byte %00001010
    .byte %00010010
    .byte %11100010
TITLE2:
    .byte %10011001
    .byte %10100110
    .byte %10100110
    .byte %10100110
    .byte %10100000
    .byte %10100000
    .byte %10100000
    .byte %00100000
TITLE3:
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %11110000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %11110000
TITLE4:
    .byte %10100010
    .byte %10100100
    .byte %10101000
    .byte %10111110
    .byte %10100001
    .byte %10100001
    .byte %10100001
    .byte %00111110
TITLE5:
    .byte %00000010
    .byte %00000010
    .byte %00000010
    .byte %00000010
    .byte %00011110
    .byte %00000010
    .byte %00000010
    .byte %00011110

LADDER_SPRITE_X_TO_CELL_X:
    .byte 0;0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 1;12
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 2 ;24
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 3 ;36
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 4 ;48
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 5 ;60
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 6
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 7
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 8
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 9
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 10
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 0
    .byte 11



    ;245 bytes used of 256
    ORG $FEFA ;next data page

CPR_1:
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00111110
    .byte %01000000
    .byte %10000000
    .byte %10001000
    .byte %10001100
    .byte %10001000
    .byte %10000000
    .byte %01000000
    .byte %00111110
    .byte %00000000
CPR_2:
    .byte %00000000
    .byte %11111111
    .byte %11101110
    .byte %11010110
    .byte %11010110
    .byte %10111010
    .byte %10111011
    .byte %00000000
    .byte %01000101
    .byte %01000101
    .byte %01010101
    .byte %01010101
    .byte %00101001
CPR_3:
    .byte %00000000
    .byte %11111111
    .byte %10101011
    .byte %00100111
    .byte %10101011
    .byte %10101011
    .byte %01110111
    .byte %00000000
    .byte %10100010
    .byte %00100010
    .byte %10101010
    .byte %00101010
    .byte %10010100
CPR_4:
    .byte %00000000
    .byte %00000000
    .byte %10000000
    .byte %11000000
    .byte %11100000
    .byte %11110000
    .byte %11111000
    .byte %00000000
    .byte %11011000
    .byte %10010000
    .byte %11010000
    .byte %10010000
    .byte %11010000
CPR_5:
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %01110010
    .byte %01000101
    .byte %00100101
    .byte %00010101
    .byte %01010101
    .byte %00100010
    .byte %00000000
    .byte %00000000
    .byte %00000000
CPR_6:
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %01110111
    .byte %01000100
    .byte %00100010
    .byte %00010001
    .byte %01010101
    .byte %00100010
    .byte %00000000
    .byte %00000000
    .byte %00000000


    ;------------------------------------------
    if (* & $FF)
        echo "------", [$FFFA - *]d, "bytes free before End of data"
        ;align 256
    endif
    ;------------------------------------------------------------------------------
    ;       Interrupt vectors

    ORG $FFFA

    .word Reset          ; NMI
    .word Reset          ; RESET
    .word Reset          ; IRQ

    END
