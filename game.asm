 PROCESSOR 6502
    include "vcs.h"
    include "macro.h"


MAPHEIGHT                        = 6
MAPWIDTH                         = 12
PLAYERHEIGHT                     = 9
LADDERHEIGHT                     = 11
LINESPERCELL                     = 9
DIGITS_PTR_COUNT                 = 12
X_OFFSET_TO_RIGHT_FOR_MINING     = 7
MIN_PLAYER_Y                     = 9
MAX_PLAYER_Y                     = 54
MAX_PLAYER_X                     = 140
MAX_PLAYER_LIVES                 = 3

LAVA_START_POS                   = $05 ; x=0; y=5
INITIAL_LAVA_SLEEP               = 15
LAVA_SLEEP_AFTER_PRIZE           = 10
INITIAL_LAVA_SPEED               = 22; could 10 be a minimum ?
MAX_LAVA_X                       = 11

MIN_X_DISTANCE_BETWEEN_LADDERS   = 3

;Main goal
GOALX                            = 130
GOALY                            = 12

;Colors
LIVES_BG                         = $80
SCORE_COLOR                      = 15
LADDERS_COLOR                    = $C6
GROUND_COLOR                     = $93
LAVA_COLOR_BEFORE                = $34
LAVA_COLOR_AFTER                 = $14

SCORE_FOR_PRIZE                  = 10
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
LAVA_DIR                ds 1  ;Hazzard's direction
LAVA_SPEED              ds 1
LAVA_SLEEP              ds 1  ;Makes lava inactive

OLDPLAYERY              ds 1   ;Fallback data when player colides with a wall
OLDPLAYERX              ds 1
OLDPLAYER_FRAME         ds 1

PLAYER_FLIP             ds 1
PLAYER_LIVES            ds 1

RANDOM                  ds 1  ;random 8bit number

PLAYERPTR               ds 2  ;16bit address of the active sprites's frame graphics
PLAYER_FRAME            ds 1  ;frame index

GAME_OVER_TIMER         ds 1
CURRENT_LAVA_COLOR      ds 1
PRIZEX                  ds 1
PRIZEY                  ds 1
CURRENT_PRIZE_Y         ds 1
SCREEN_FRAME            ds 1

GAME_STATE              ds 1
PRIZE_SOUND_INTERVAL    ds 1

SCORE_DIGITS_IDX        ds 3                 ;indexes of highscore digits, 4 bits - one digit
TMPNUM                  ds 1
TMPNUM1                 ds 1
TEMPY                   ds 1
TEMP_X_INDEX            ds 1  ; Temp variable used in drawing, and in input checking

LADDER_LINE_IDX         ds 1
LINE_IDX                ds 1  ; line counter for a map cell

SCORE_PTR               ds DIGITS_PTR_COUNT  ; pointers to digit graphics
GENERATING              ds 1   ;Is the map being generared at the moment?
BUTTON_PRESSED          ds 1   ;Is joystick button being pressed right now?

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
    ;set player coordinates
    lda #MAX_PLAYER_Y
    sta PLAYERY
    sta OLDPLAYERY
    lda #3
    sta PLAYERX
    sta OLDPLAYERX

    lda #0
    sta LAVA_TIMER
    sta LAVA_DIR

    lda #INITIAL_LAVA_SLEEP
    sta LAVA_SLEEP

    sta GENERATING
    sta TMPNUM1

    lda #LAVA_START_POS
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
    
    lda #%10000000
    sta GAMEMAP0,x
    lda #$FF
    sta GAMEMAP1,x
    sta GAMEMAP2,x
    sta GAMEMAP3,x
    sta GAMEMAP4,x
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
    cpx #6
    bne genloop

    ldx #5
    lda #0
    sta GAMEMAP0,x
    lda #%00000111
    sta GAMEMAP1,x

    lda #4                      ;inverted first y position
    sta TEMPY
    ;----------
ladderLoop:                     ;  let's generate a ladder for each of the map rows
    jsr UpdateRandomNumber
    and #11                     ;  limit to 0..11 range
    tax                         ;  and transfer to X register
    cmp TMPNUM1
    bcs XIsBiggerOrEqual        ; this X is bigger or equal

    lda TMPNUM1                 ; this X is smaller than previous
    stx TMPNUM1
    sec
    sbc TMPNUM1
    jmp compareResult
XIsBiggerOrEqual:
    sec
    sbc TMPNUM1

compareResult:
    stx TMPNUM1
    cmp #MIN_X_DISTANCE_BETWEEN_LADDERS
    bcc ladderLoop                          ;the difference in X between two ladders in two adjacent levels is too small, do it again!

    cpx TEMP_X_INDEX            ;compare ladder x with prize x
    beq checkIfLadderYIsTheSame
    jmp saveLadderPostionToRam
checkIfLadderYIsTheSame:
    lda PRIZEY
    cmp TEMPY
    beq ladderLoop              ;generate ladder one more time

saveLadderPostionToRam:

    lda LADDER_X_POSSITIONS,x
    ldy TEMPY
    sta LADDER1X,y              ;  store sprite position to ram variable


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


nextLadder:
    dec TEMPY
    lda TEMPY
    cmp #255
    bne ladderLoop


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

    lda #LADDERHEIGHT
    sbc LINE_IDX ; subtract current map tile line index
    lsr          ; A / 2
    lsr          ; A / 2

    ;A needs to be 2 for missile to be enabled
    
    cpx CURRENT_PRIZE_Y  ;3
    beq enableMissile    ;3
    lda #0               ;2

enableMissile:
    
    ;---------------------------------------------
    sta WSYNC               ;finish the scanline
    ;---------------------------------------------

    sta HMCLR               ;3 3  ; this resets the horizontal movement for the player sprite, placed it here instead nop
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

    ldy #LADDERHEIGHT   ;2 22
    sty LADDER_LINE_IDX ;3 25


    lda #GROUND_COLOR   ;2 27
    sta COLUPF          ;3 30

    lda PLAYER_FLIP     ;3 33
    sta REFP0           ;3 36


    ldx #54             ;2 38 scanlines, max scanlines / 3


KERNEL_LOOP:
    ;------------------------------------------------------------------------
    lda LADDER_GFX,y        ;4 29
    sta GRP1                ;3 32
    dey                     ;2 34
    sty LADDER_LINE_IDX     ;3 37


drawThePlayer:

    ldy TMPNUM1             ;3 40
    beq nope                ;2 42   PLAYER_LINE_IDX == 0
    cpx PLAYERY             ;3 45   can we draw the player sprite?
    bcs nope                ;2 47   < PLAYERY

    lda PLAYERCOLORS,y      ;4 51    sets the player colors
    sta COLUP0              ;3 54
    lda (PLAYERPTR),y       ;5 59    let's load a line from a sprite frame
    sta GRP0                ;3 62    and store it to the Player0 sprite
    dec TMPNUM1             ;5 67
nope:
    ldy LADDER_LINE_IDX     ;3 70

    lda #GROUND_COLOR       ;2 72

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
    lda LADDER1X,x          ;4 12
    sec                     ;2 14
divLoop:
    sbc #15                 ;2 16
    bcs divLoop             ;3 19

    tay                     ;2 21
    lda FINE_ADJUST_TABLE,y ;4 25

    sta RESP1
    sta HMP1

    ldy #LADDERHEIGHT       ;2 7  reset the ladder sprite
    ;---------------------------------------------
    sta WSYNC
    ;---------------------------------------------
    sta HMOVE               ;3 3
    dec TEMPY               ;5 8    let's position next ladder, decrease ladder index
    dec TMPNUM              ;5 13   move to next map cell
    ldx #LINESPERCELL       ;2 15   reset line count
cont:
    stx LINE_IDX            ;3 18 save current line count
    ldx TEMP_X_INDEX        ;3 21 restore X (scanline index)

    ;---------------------------------------------
    dex                     ;2 23
    bne KERNEL_LOOP ;       ;2 25
    ;==================================================
doneDrawing:

    jsr DrawScore

    lda #0
    sta GRP0
    sta GRP1
    sta GRP0
    sta GRP1

    sta WSYNC   ;let's draw an empty line
    sta HMOVE

    ldy PLAYER_LIVES ;3 2
    lda LIVES_LOOKUP,y
    sta NUSIZ0       ;3 5

    lda #LIVES_BG  ;2 10
    sta COLUBK     ;3 13
    lda #0         ;2 15
    SLEEP 2        ;6 21

    sta RESP0      ;2 reset sprite pos

    lda #$0        ;2
    sta HMP0       ;3  reset p1 x offset


    ldy #PLAYERHEIGHT

lives_bar_loop:

    sta WSYNC
    sta HMOVE
    lda PLAYER_LIVES
    cmp #1
    beq no_life
    lda DWARF_GFX_3,y
    sta GRP0

    jmp next_live_line
no_life:
    lda #0
    sta GRP0
next_live_line:
    sta HMCLR   ; let's clear HM
    dey
    bne lives_bar_loop
endofKernel:

    rts
;--------------------------------------------------------------
DrawScore:

    lda #0
    sta GRP0
    sta GRP1
    sta REFP0           ;3  turn off mirroring

    lda #%00000011      ;2
    sta NUSIZ0          ;3
    sta NUSIZ1          ;3


    sta WSYNC           ;let's draw an empty line
    SLEEP 22
    sta RESP0           ;2 reset sprite pos
    sta RESP1

    sta HMCLR
    lda #$10           ;2  move p2 sprite left a bit
    sta HMP1           ;3


    lda SCORE_COLOR     ;2
    sta COLUP1          ;3 
    sta COLUP0          ;3 

    sta WSYNC
    sta HMOVE
    SLEEP 45
    ldy #PLAYERHEIGHT - 1
    sty TEMP_X_INDEX

score_line_loop:

    ldy TEMP_X_INDEX        ;3 50

    lda (SCORE_PTR+10),y    ;5 55
    sta GRP0                ;3 58
    lda (SCORE_PTR+8),y     ;5 63
    sta GRP1                ;3 66

    sta WSYNC
    ;--------------------
    lda (SCORE_PTR+6),y     ;5 5
    sta GRP0                ;3 8
    lda (SCORE_PTR),y       ;5 13
    sta TMPNUM              ;3 16
    lda (SCORE_PTR+2),y     ;5 21 
    tax                     ;2 23
    lda (SCORE_PTR+4),y     ;5 28
    ldy TMPNUM              ;3 31 for some reason this has to be 27, otherwise it doesn't work :-/

    sta GRP1                ;3 34
    stx GRP0                ;3 37 fifth
    sty GRP1                ;3 40 last digit, sixth
    sta GRP0                ;3 43

    dec TEMP_X_INDEX        ;5 48
    bpl score_line_loop     ;2 50


    rts


;---------------------------------------------------------------
TitleScreen:

    sta WSYNC

    lda #20
    sta TMPNUM
    ldy #5
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
    lda #20
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

    ldx #36
emptyLoop1:
    sta WSYNC
    dex
    bne emptyLoop1


    rts


;---------------------------------------------------------------

Overscan:
    sta WSYNC
    lda #2
    sta VBLANK
    lda #35
    sta TIM64T

    ;some game logic here
    ;----------
    lda GAME_OVER_TIMER
    bne notColliding

    ;Let's check PLAYER-PLAYFIELD collision
    bit CXP0FB
    bpl notColliding
    ;so the player is colliding with the playfield

    ;let's check if the player collides with the lava
    jsr CalcLavaCollision


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
    sbc #12
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

    lda #1
    ldx #0
    ldy #0
    jsr IncrementScore

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
    jmp checkLadderCollision
hideDemPrize:
    lda #128                ;some other value than 255
    lda #LAVA_SLEEP_AFTER_PRIZE
    sta LAVA_SLEEP          ;freeze lava
    sta CURRENT_PRIZE_Y
    lda #SCORE_FOR_PRIZE
    ldy #0
    ldx #0
    jsr IncrementScore

checkLadderCollision:
    
    bit CXPPMM
    bpl OverscanLoop

    lda PLAYER_FRAME
    cmp #%00010000
    bne OverscanLoop

    lda PLAYERX
    clc
    adc #5 ; hacky hack
prepareDivide:
    ldx #0
dividing_x:
    inx
    sec
    sbc #12
    bcs dividing_x
    dex
    cpx #MAPWIDTH
    bcc setLadderXToPlayer
    ldx #MAPWIDTH - 1
setLadderXToPlayer:
    lda PLAYER_X_POSITIONS_BY_MAP_X,x
    sta PLAYERX
    sta OLDPLAYERX


OverscanLoop:
    sta WSYNC
    lda INTIM
    bne OverscanLoop

    rts
;-----------------------------
Mine
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
    ldx TMPNUM              ;load x coord
    and MAP_CLEAR_PATTERN_BY_X_SEG2,x
    sta GAMEMAP0,y + MAPHEIGHT


mine_onlyOneSegmentUsed:

    lda GAMEMAP0,y
    ldx TMPNUM
    and MAP_CLEAR_PATTERN_BY_X_SEG1,x
    sta GAMEMAP0,y

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
    cmp #12
    bcc lava_doneDividingX
    sec
    sbc #12
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
    cmp #12
    bcc lava_doneDividingX_2
    sec
    sbc #12
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
    sta GAME_OVER_TIMER

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
    ;sta SCORE_DIGITS_IDX+2
    cld
    rts

;---------------------------
ProcessInput:
    lda SWCHA
    asl         ;shift left, bit might fall into CARRY flag
    sta TMPNUM
    bcs checkLeft
moveRight:
    lda PLAYER_FLIP
    beq itWasFacingRightAlready ; if facing right
    lda #%00000000
    sta REFP0
    sta PLAYER_FLIP
    lda #12
    sta PLAYER_FRAME
    inc PLAYERX ; add +2 to prevent getting stuck in a wall
    inc PLAYERX
itWasFacingRightAlready:

    lda PLAYERX
    clc
    adc #1
    cmp #MAX_PLAYER_X
    bcs checkLeft
  
    ldx PLAYERX
    stx OLDPLAYERX
    sta PLAYERX
  
    
    ldx PLAYER_FRAME
    inx
    cpx #16 ;we must not let go to animation frame 3, 00010 000
    bcc storeframe1
    ldx #0
storeframe1:
    lda PLAYER_FRAME
    sta OLDPLAYER_FRAME
    stx PLAYER_FRAME

    ;so left was activated, let's skip right direction
    lda TMPNUM
    asl
    sta TMPNUM
    jmp checkButton


checkLeft:
    lda TMPNUM
    asl
    sta TMPNUM
    bcs checkDown
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
    sta PLAYERX
       ;----------

    
    ldx PLAYER_FRAME
    inx
    cpx #16 ;we must not let go to animation frame 3, 00010 000
    bcc storeframe2
    ldx #0
storeframe2:
    lda PLAYER_FRAME
    sta OLDPLAYER_FRAME
    stx PLAYER_FRAME

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
    lda #%00010000  ; after 3 x lsr it will turn into 2(3rd frame - climbing)
    ldx PLAYER_FRAME
    stx OLDPLAYER_FRAME
    sta PLAYER_FRAME
    jmp noInput
checkButton:
;----------------------------------------------

    lda #0
    sta AUDV0
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

;-----------------------------------------

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
    
    lda MAP_3CELLS_INTERSECTIONS,x
    cmp #1
    beq checkBelow_seg1
    ;Let's change the second segment

    lda GAMEMAP0,y + MAPHEIGHT  ; adding MAPHEIGHT helps to reach the next column
    eor MAP_CLEAR_PATTERN_BY_X_SEG2,x
    and MAP_FILL_PATTERN_BY_X_SEG2,x
    beq mustLowerLava

checkBelow_seg1:
    lda GAMEMAP0,y
    eor MAP_CLEAR_PATTERN_BY_X_SEG1,x
    and MAP_FILL_PATTERN_BY_X_SEG1,x
    beq mustLowerLava

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

    cpy LAVA_SPEED ;if LAVA_TIMER reaches the value of LAVA_SPEED, lava starts to move
    bcc lavaDelay

    ldx LAVA_SLEEP
    cpx #0
    bne lavaSlumber
    jmp moveTheLava
lavaSlumber:
    dex
    stx LAVA_SLEEP
    ldy #0
    jmp lavaDelay


moveTheLava:

    jsr FillInLavaTile
;----
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

    jmp lavaDelay
leftSideReached:
    dec LAVA_DIR
    jmp lavaDelay
rightSideReached:
    ldy #0
    inc LAVA_DIR
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

exitLavaMoveDown:
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

    ldy PRIZEY

    lda Y_POSITIONS_WHERE_YOU_CAN_MINE,y
    cmp PLAYERY
    bne hidePrize

    lda PRIZEX
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
    jmp contUpdateX
hidePrize:
    lda #255

contUpdateX:
    sta CURRENT_PRIZE_Y
dontUpdatePrize:

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
    lda #MAX_PLAYER_LIVES
    sta PLAYER_LIVES
    lda #0
    sta SCORE_DIGITS_IDX
    sta SCORE_DIGITS_IDX+1
    sta SCORE_DIGITS_IDX+2
    
    lda #INITIAL_LAVA_SPEED
    sta LAVA_SPEED

    lda LAVA_COLOR_BEFORE
    sta CURRENT_LAVA_COLOR

    jsr EnterNewMap


    jmp titleNoInput

titleButtonNotPressed:
    lda #0
    sta BUTTON_PRESSED

titleNoInput:
    

    ldx TMPNUM1
    dex
    bmi resetTmpNum
    jmp saveTmpNum
resetTmpNum:
    ldx #68
saveTmpNum:
    stx TMPNUM1


    jsr UpdateSpriteFrames

    ;lda #0
    ;ldx #0
    ;jsr SetSpriteXPos

    ;sta WSYNC
    ;sta HMOVE


    rts

;-----------------------------------------------------
ResetTheGame

    lda #0
    sta GAME_STATE
    rts

;------------------------------------------------------

VBlank:

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
    stx GAME_OVER_TIMER
    stx AUDV1
    ldx PLAYER_LIVES
    dex
    stx PLAYER_LIVES
    cpx #0
    bne enterNew
    jsr ResetTheGame
there:
    lda GENERATING
    cmp #1
    beq notReached
;-- lava color animation
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
    lda GAME_OVER_TIMER
    beq noGameOver
    tax
    dex
    lda #8
    sta AUDC1
    lda #DEATH_INTERVAL
    sec
    sbc GAME_OVER_TIMER
    lsr
    sta AUDF1
    lda #14
    sta AUDV1
continueGameOver:
    cpx #0
    beq ResetThatGame
    stx GAME_OVER_TIMER
    jmp updatePlayerSpriteX

noGameOver:
    jsr LavaLogic

movePlayer:
    jsr ProcessInput

updatePlayerSpriteX:

    jsr UpdatePrize

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
    bne continueVBlank
    ldy #0
continueVBlank:
    sty SCREEN_FRAME


    jsr UpdateSpriteFrames

    lda PLAYERX
    cmp #GOALX
    bcc notReached
    lda PLAYERY
    cmp #GOALY
    bcs notReached
    dec LAVA_SPEED
enterNew:
    jsr EnterNewMap
notReached:

    lda #0
    sta GENERATING

exitVBlank:

    rts
;--------------------------------------------
    if (* & $FF)
        echo "------", [$FBFA - *]d, "bytes free before End of code"
        align 256
    endif
;--------------------------------------------
;ROM constants

    ORG $FBF4
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
    .byte %01000010
    .byte %01111110
    .byte %01000010
    .byte %01111110
    .byte %01000010
    .byte %01111110
    .byte %01000010
    .byte %01111110
    .byte %01000010

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
    .byte 0
    .byte 12
    .byte 24
    .byte 36
    .byte 48
    .byte 60
    .byte 72
    .byte 84
    .byte 96
    .byte 108
    .byte 120
    .byte 132

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

LIVES_LOOKUP
    .byte 0 ;0 lives
    .byte 0 ;1 life
    .byte 0 ;2 lives
    .byte 1 ;3 lives

    ORG $FCF3 ; next data page

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

TITLE0
    .byte %10000000
    .byte %10000000
    .byte %10000000
    .byte %10000000
    .byte %10000000
    .byte %10000000
TITLE1
    .byte %11100001
    .byte %00010010
    .byte %00001010
    .byte %00001010
    .byte %00010010
    .byte %11100010
TITLE2
    .byte %10011001
    .byte %10100110
    .byte %10100110
    .byte %10100000
    .byte %10100000
    .byte %00100000
TITLE3
    .byte %00000000
    .byte %00000000
    .byte %11110000
    .byte %00000000
    .byte %00000000
    .byte %11110000
TITLE4
    .byte %10100110
    .byte %10101000
    .byte %10111110
    .byte %10100001
    .byte %10100001
    .byte %00111110
TITLE5
    .byte %00000010
    .byte %00000010
    .byte %00011110
    .byte %00000010
    .byte %00000010
    .byte %00011110


    ;87 bytes used of 256


    ;------------------------------------------
    if (* & $FF)
        echo "------", [$FFFA - *]d, "bytes free before End of data"
        align 256
    endif
    ;------------------------------------------------------------------------------
    ;       Interrupt vectors

    ORG $FFFA

    .word Reset          ; NMI
    .word Reset          ; RESET
    .word Reset          ; IRQ

    END
