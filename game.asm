 PROCESSOR 6502
    include "vcs.h"
    include "macro.h"


MAPHEIGHT                    = 6
MAPWIDTH                     = 12
PLAYERHEIGHT                 = 9
LADDERHEIGHT                 = 11
LINESPERCELL                 = 9
DIGITS_PTR_COUNT             = 12
X_OFFSET_TO_RIGHT_FOR_MINING = 7
MIN_PLAYER_Y                 = 9
MAX_PLAYER_Y                 = 54
GOALX                        = 130
GOALY                        = 12
LIVES_BG                     = $80

PLAYERS_COLOR                = 15
LADDERS_COLOR                = $C6
GROUND_COLOR                 = $96
LAVA_COLOR                   = $38


NO_ILLEGAL_OPCODES = 1 ; DASM needs it

;----------------------------------------------------
;           RAM
;-----------------------------------------------------
    SEG.U VARS
    ORG $80

GAMEMAP0         ds MAPHEIGHT  ;6 columns of the GameMap, 
GAMEMAP1         ds MAPHEIGHT  ;single cells are represented as bits
GAMEMAP2         ds MAPHEIGHT
GAMEMAP3         ds MAPHEIGHT
GAMEMAP4         ds MAPHEIGHT
GAMEMAP5         ds MAPHEIGHT

LAVAMAP0         ds MAPHEIGHT
LAVAMAP1         ds MAPHEIGHT
LAVAMAP2         ds MAPHEIGHT
LAVAMAP3         ds MAPHEIGHT
LAVAMAP4         ds MAPHEIGHT
LAVAMAP5         ds MAPHEIGHT

LADDER1X         ds 1 ;Ladder X positions
LADDER2X         ds 1
LADDER3X         ds 1
LADDER4X         ds 1
LADDER5X         ds 1
LADDER_IDX       ds 1


PLAYERY          ds 1  ; Player's Y position
PLAYERX          ds 1  ; Player's X position

LAVAX            ds 1
LAVAY            ds 1
LAVA_TIMER       ds 1  ;Hazzard's delay
LAVA_DIR         ds 1  ;Hazzard's direction

OLDPLAYERY       ds 1   ;Fallback data when player colides with a wall
OLDPLAYERX       ds 1
OLDPLAYER_FRAME  ds 1

PLAYER_DIR       ds 1  ; Player's direction
PLAYER_FLIP      ds 1

INPUT            ds 1  ; Input from the joystick
TEMP_X_INDEX     ds 1  ; Temp variable used in drawing, and in input checking

SCREENMAP_IDX    ds 1  ;index of the screenmap line; used in drawing

LINE_IDX         ds 1  ; line counter for a map cell

RANDOM           ds 1  ;random 8bit number

PLAYERPTR        ds 2  ;16bit address of the active sprites's frame graphics
PLAYER_FRAME     ds 1  ;frame index

TMPNUM           ds 1
TMPNUM1          ds 1
TEMPY            ds 1

SCREEN_FRAME     ds 1
LADDER_LINE_IDX  ds 1
PLAYER_LINE_IDX  ds 1

SCORE_LINE_IDX   ds 1
SCORE_PTR        ds DIGITS_PTR_COUNT  ; pointers to digit graphics
SCORE_DIGITS_IDX ds 6                 ;indexes of highscore digits (0..9)
BUTTON_PRESSED   ds 1   ;Is joystick button being pressed right now?

GENERATING       ds 1   ;Is the map being generared at the moment?

;------------------------------------------------------
;                  124 | 4 bytes free
;------------------------------------------------------
    ;           ROM
    SEG
    ORG $F000 ;4K Rom , F8000 = 2K

Reset:

    CLEAN_START ; from macro.h

    lda #PLAYERS_COLOR
    sta COLUP0

    sta RANDOM

    lda #%00010100 ;make thin ball-ray and the playfield on top of everything
    sta CTRLPF

    lda #LADDERS_COLOR
    sta COLUP1

    lda #0
    sta SCREEN_FRAME
    sta LADDER_IDX

    jsr EnterNewMap


Main:
    jsr Vsync
    jsr VBlank
    jsr Kernel
    jsr HUD
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
    sta LAVAX
    sta LAVA_TIMER
    sta LAVA_DIR
    sta GENERATING
    lda #5
    sta LAVAY


    jsr GenerateMap

    rts

;------------------------------------------
; Fake subroutine
drawLava:

    lda #LAVA_COLOR         ;3 9
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

    nop                     ;2 42
    
    lda LAVAMAP4,x          ;4 46
    sta PF1                 ;3 49
    
    nop                     ;2 51

    lda LAVAMAP5,x          ;4 55
    sta PF2                 ;3 58

    lda #0                  ;2 60   Lets turn off the playfield for one scanline
    sta PF0                 ;3 63
    sta PF1                 ;3 66

    jmp exitDrawLava        ;3 69


;--------------------------
Kernel:
    sta WSYNC
    lda INTIM
    bne Kernel ; wait for vsync timer

    sta VBLANK

    lda #0
    sta COLUBK      ;set X as the background color
    sta LADDER_IDX
    
    ldy #PLAYERHEIGHT
    sty PLAYER_LINE_IDX     

    lda #MAPHEIGHT-1
    sta SCREENMAP_IDX ; bottom of the screenmap

    lda #LINESPERCELL
    sta LINE_IDX

    ldy #LADDERHEIGHT
    sty LADDER_LINE_IDX


    lda #GROUND_COLOR
    sta COLUPF

    lda PLAYER_FLIP
    sta REFP0


    ldx #54 ; scanlines, max scanlines / 2


KERNEL_LOOP:
    ;------------------------------------------------------------------------
    lda LADDER_GFX,y        ;4 31
    sta GRP1                ;3 34
    dey                     ;2 36
    sty LADDER_LINE_IDX     ;3 39

    sta HMCLR               ;3 42

drawThePlayer:

    ldy PLAYER_LINE_IDX     ;3 45
    beq nope                ;2 47   PLAYER_LINE_IDX == 0
    cpx PLAYERY             ;3 50   can we draw the player sprite?
    bcs nope                ;2 52   < PLAYERY

    lda (PLAYERPTR),y       ;5 57    let's load a line from a sprite frame
    sta GRP0                ;3 60    and store it to the Player0 sprite
    dec PLAYER_LINE_IDX     ;5 65
nope:
    ldy LADDER_LINE_IDX     ;3 68

    ;----------------------------------------------------------------

    sta WSYNC           ;wait for the scanline to be drawn

    ;----------------------------------------------------------------

    lda #GROUND_COLOR
    sta COLUPF
    ;----------------------------------------- only here starts playfield drawing

    stx TEMP_X_INDEX        ;3 11    save scanline index

    ldx SCREENMAP_IDX       ;3 14

    lda GAMEMAP0,x          ;4 18
    sta PF0                 ;3 21

    lda GAMEMAP1,x          ;4 25
    sta PF1                 ;3 28

    lda GAMEMAP2,x          ;4 32
    sta PF2                 ;3 35

    ;------right side of the screen

    lda GAMEMAP3,x          ;4 39
    sta PF0                 ;3 42

    nop
    
    lda GAMEMAP4,x          ;4 46
    sta PF1                 ;3 49
    
    nop                     ;2 51

    lda GAMEMAP5,x          ;4 55
    sta PF2                 ;3 58
    
    ;--- some code I wanted to place in to this scanline

    lda #0                  ;2 60   Lets turn off the playfield for one scanline
    sta PF0                 ;3 68
    sta PF1                 ;3 71
    ;---------------------------------------------
    sta WSYNC               ;finish the scanline, we don't want to cram sprite drawing instructions to be here
    ;---------------------------------------------
    sta PF2                 ;3 3  clearing the remaining playfield register

    jmp drawLava            ;3 6; faking subroutine
    ;----------------------------
                            ;  69
exitDrawLava:

    ldx LINE_IDX            ;3 72   decrement current line count for one map cell
    dex                     ;2 74
    ;---------------------------------------------
    sta WSYNC
    ;---------------------------------------------
    sta PF2                 ;3 3  clearing the remaining playfield register
    bne cont                ;2 5  not all lines for a cell have been drawn, continue

    ldx LADDER_IDX          ;3 8
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
    inc LADDER_IDX          ;5 8    let's position next ladder
    dec SCREENMAP_IDX       ;5 13   move to next map cell
    ldx #LINESPERCELL       ;2 15   reset line count
cont:
    stx LINE_IDX            ;3 18 save current line count
    ldx TEMP_X_INDEX        ;3 21 restore X (scanline index)

    ;---------------------------------------------
    dex                     ;2 23
    bne KERNEL_LOOP ;       ;2 25
doneDrawing:
    
    rts

;-------------------------------------------------------------------------------------------------

HUD:

    ;let's draw a score
    ;---------------------------------------------
    lda #0
    sta GRP0
    sta WSYNC   ;let's draw an empty line
    sta HMOVE

    lda #%00000011      ;2 2
    sta NUSIZ0          ;3 5
    sta NUSIZ1          ;3 8

    lda PLAYERS_COLOR   ;2 10
    sta COLUP1          ;3 13
    lda #0              ;2 15
    
    sta REFP0           ;3  turn off mirroring
    sta RESP0           ;2 reset sprite pos
    sta RESP1

    ;sta HMP0           ;3  reset p1 x offset
    ;lda #254           ;2  move p2 sprite right a bit
    ;sta HMP1           ;3 

    ldy #PLAYERHEIGHT
    sty SCORE_LINE_IDX
    lda #1
    sta VDELP0  ;turn on vertical delay
    sta VDELP1


score_line_loop:

    ldy SCORE_LINE_IDX  ;3 50

    lda (SCORE_PTR),y   ;5 55
    sta TMPNUM          ;3 58
    lda (SCORE_PTR+2),y ;5 63
    sta TMPNUM1         ;3 66

    lda ZERO_GFX,y      ;4 70 first digit unfortunately inactive
    sta GRP0            ;3 73

    sta WSYNC           ;----------
    ;sta HMOVE           ;3 would love to execute this

    lda (SCORE_PTR+8),y ;5 5
    sta GRP1            ;3 8
    lda (SCORE_PTR+6),y ;5 13
    sta GRP0            ;3 16
    ldx TMPNUM1         ;3 19 fifth
    lda (SCORE_PTR+4),y ;5 24 fourth
    ldy TMPNUM          ;3 27 for some reason this has to be 27, otherwise it doesn't work :-/

    sta GRP1            ;3 30
    stx GRP0            ;3 33 fifth
    sty GRP1            ;3 36 last digit, sixth
    sta GRP0            ;3 39

    dec SCORE_LINE_IDX  ;5 44
    bpl score_line_loop ;2 47
    ;sta HMCLR           ;3 

;----------------------------------------
    lda #0
    sta GRP0
    sta WSYNC   ;let's draw an empty line
    sta HMOVE

    lda #%00000011 ;2 2
    sta NUSIZ0     ;3 5
    sta NUSIZ1     ;3 8

    lda #LIVES_BG  ;2 10
    sta COLUBK     ;3 13
    lda #0         ;2 15
    SLEEP 4        ;6 21

    sta RESP0      ;2 reset sprite pos
    sta RESP1

    lda #$0        ;2
    sta HMP0       ;3  reset p1 x offset
    lda #$11       ;2  move p2 sprite left a bit
    sta HMP1       ;3 


    ldy #PLAYERHEIGHT
    ldx #10 ; remaining lines
    lda #0
    sta VDELP0
    sta VDELP1

lives_bar_loop:

    sta WSYNC
    sta HMOVE
    cpy #0
    beq okok1
    lda DWARF_GFX_0,y
    sta GRP1
    lda DWARF_GFX_0,y
    sta GRP0
    sta HMCLR   ; let's clear HM

    dey
okok1:
    dex
    bne lives_bar_loop


    rts
;----------------------------
Overscan:
    sta WSYNC
    lda #2
    sta VBLANK
    lda #35
    sta TIM64T

    ;some game logic here
    ;----------
    ;Let's check PLAYER-PLAYFIELD collision
    bit CXP0FB
    bpl notColliding
    ;so the player is colliding with the playfield

    lda PLAYER_FRAME
    cmp #%00011111
    bne notMining
    ;player is coliding with the playfield and swings his axe

    lda PLAYER_DIR
    cmp #1
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
    ;cmp #MIN_PLAYER_Y
    ;bne notMining   ;don't let mine while climbing the ladders

    stx TEMPY

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

    ldy #0 ; first digit
    jsr IncrementScore

doneMining:

    ;----   Let's restore the X
    lda PLAYER_DIR
    cmp #1
    bne notColliding ;let's not reset the animation frame
    lda PLAYERX
    clc
    sbc #X_OFFSET_TO_RIGHT_FOR_MINING
    sta PLAYERX

    jmp notColliding

notMining:           ; some kind of collision

                     ;let's check if the player collides with the lava

    jsr CalcLavaCollision


    lda OLDPLAYERX   ; let's restore previous player state before the collision
    sta PLAYERX
    lda OLDPLAYERY
    sta PLAYERY
    lda OLDPLAYER_FRAME
    sta PLAYER_FRAME


notColliding:


OverscanLoop:
    sta WSYNC
    lda INTIM
    bne OverscanLoop

    rts
;-----------------------------
CalcLavaCollision:

    jmp CheckLava
ResetTheGame:
    jsr Reset
CheckLava:
    lda PLAYERX
    ldx #0
lava_divx:
    inx
    sbc #12
    bcs lava_divx
    dex ;playerX / 12 - 1

    stx TMPNUM;

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
    bne ResetTheGame


lava_onlyOneSegmentUsed:

    lda LAVAMAP0,y
    ldx TMPNUM
    and MAP_FILL_PATTERN_BY_X_SEG1,x
    bne ResetTheGame

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
    sta NUSIZ0 ;reset sprite special params
    sta NUSIZ1


    sta WSYNC
    sta VSYNC
    rts

;----------------------------
;set Y as number of the digit
IncrementScore:

    ldx SCORE_DIGITS_IDX,y
    inx
    cpx #10
    bne storeDigit
    ldx #0
    iny
    stx TMPNUM
    jsr IncrementScore
    ldx TMPNUM
    dey
storeDigit:
    stx SCORE_DIGITS_IDX,y

    rts

;---------------------------
ProcessInput:
    lda SWCHA
    asl         ;shift left, bit might fall into CARRY flag
    sta INPUT
    bcs checkLeft
moveRight:
    lda PLAYER_DIR
    cmp #1
    beq itWasFacingRightAlready
    lda #%00000000
    sta REFP0
    sta PLAYER_FLIP
    lda #12
    sta PLAYER_FRAME
    inc PLAYERX
    inc PLAYERX
itWasFacingRightAlready:

    lda #1
    sta PLAYER_DIR
    lda PLAYERX
    clc
    adc #1
    cmp #140
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
    lda INPUT
    asl
    sta INPUT
    jmp checkButton


checkLeft:
    lda INPUT
    asl
    sta INPUT
    bcs checkDown
moveLeft:

    lda PLAYER_DIR
    cmp #2
    beq itwasFacingLeftAlready
    lda #%00001000
    sta REFP0
    sta PLAYER_FLIP
    lda #12
    sta PLAYER_FRAME
    dec PLAYERX
    dec PLAYERX
itwasFacingLeftAlready:

    lda #2
    sta PLAYER_DIR

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
    lda INPUT
    asl
    sta INPUT
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

    jmp noInput

buttonNotPressed:
    ;ldx #0
    ;stx PLAYER_FRAME
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
GenerateMap:

    lda #1
    sta GENERATING

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
    inx 
    cpx #6
    bne genloop

    ldx #5
    lda #0
    sta GAMEMAP0,x
    lda #%00000111
    sta GAMEMAP1,x
    ;lda #%10000000
    ;sta LAVAMAP0,x

    lda #0
    sta LADDER_IDX
    lda #4                      ;inverted first y position
    sta TEMPY
    ;----------
ladderLoop:                     ;  let's generate a ladder for each of the map rows
    jsr UpdateRandomNumber
    and #11                     ;  limit to 0..11 range
    tax                         ;  and transfer to X register
    lda LADDER_X_POSSITIONS,x
    ldy LADDER_IDX
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
    inc LADDER_IDX
    lda #4
    sec
    sbc LADDER_IDX
    sta TEMPY 
    cmp #255
    bne ladderLoop



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
;-------------------------
LavaLogic

    ldy LAVA_TIMER
    iny
    cpy #20
    bcc lavaSleep

    ldx LAVAX

    ;-----
    
    lda MAP_3CELLS_LOOKUP,x
    clc
    adc LAVAY
    tay ; store the cell offset in Y

    lda MAP_3CELLS_INTERSECTIONS,x
    cmp #1
    beq onlyONE
    ;Let's change the second segment

    lda LAVAMAP0,y + MAPHEIGHT
    ora MAP_FILL_PATTERN_BY_X_SEG2,x
    sta LAVAMAP0,y + MAPHEIGHT

onlyONE:
    lda LAVAMAP0,y
    ora MAP_FILL_PATTERN_BY_X_SEG1,x
    sta LAVAMAP0,y


    ldy #0
;-----



    lda LAVA_DIR
    cmp #0
    bne moveLavaLeft
    cpx #11
    bcs rightSideReached

    inx
    jmp storeLavaX
moveLavaLeft:
    dex
    cpx #255
    beq leftSideReached

storeLavaX:
    stx LAVAX

    jmp lavaSleep
leftSideReached:
    dec LAVA_DIR
    jmp lowerLava
rightSideReached:
    inc LAVA_DIR
lowerLava:
    dec LAVAY

lavaSleep:
    sty LAVA_TIMER



    rts
;-------------------------
VBlank:

    lda GENERATING
    cmp #1
    beq notReached

    lda PLAYERY
    cmp #MAX_PLAYER_Y - 10
    bcs movePlayer ;start moving lava only when player descended to a lower level

    jsr LavaLogic

movePlayer:
    jsr ProcessInput

    lda PLAYERX
    ldx #0
    jsr SetSpriteXPos

    lda LAVAX
    ldx #4
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


animatePlayer:

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
    lsr        ; Y / 2
    tay        ; A -> Y
    ldx SCORE_DIGITS_IDX,y
    ldy TMPNUM ;restore Y
    lda DIGITS_PTR_LOW,x
    sta SCORE_PTR,y
    iny
    lda DIGITS_PTR_HIGH,x
    sta SCORE_PTR,y
    iny
    cpy #DIGITS_PTR_COUNT
    bne digitsUpdate


    lda PLAYERX
    cmp #GOALX
    bcc notReached
    lda PLAYERY
    cmp #GOALY
    bcs notReached
    jsr EnterNewMap
notReached:

    lda #0
    sta GENERATING

    rts
;--------------------------------------------
;ROM constants

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

;-------------------------------------------


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

MAP_CLEAR_PATTERN_BY_X_SEG1:    ; lookup tables for map cell destruction, they contains a patter used with AND
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

MAP_FILL_PATTERN_BY_X_SEG1:    ; lookup tables for map cell destruction, they contains a patter used with AND
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


    ;------------------------------------------
    ; free space check 
    if (* & $FF)
        echo "------", [$FFFA - *]d, "bytes free before End of Cartridge"
        align 256
    endif
    ;------------------------------------------------------------------------------
    ;       Interrupt vectors

    ORG $FFFA

    .word Reset          ; NMI
    .word Reset          ; RESET
    .word Reset          ; IRQ

    END
