 PROCESSOR 6502
    include "vcs.h"
    include "macro.h"


MAPHEIGHT = 6
PLAYERHEIGHT = 9
LADDERHEIGHT = 11
LINESPERCELL = 12
X_OFFSET_TO_RIGHT_FOR_MINING = 6


NO_ILLEGAL_OPCODES = 1 ; DASM needs it

;----------------------------------------------------
;           RAM
;-----------------------------------------------------
    SEG.U VARS
    ORG $80

GAMEMAP0         ds 6  ;screen map
GAMEMAP1         ds 6
GAMEMAP2         ds 6
GAMEMAP3         ds 6
GAMEMAP4         ds 6
GAMEMAP5         ds 6

LADDER1X         ds 1
LADDER2X         ds 1
LADDER3X         ds 1
LADDER4X         ds 1
LADDER5X         ds 1
LADDER_IDX       ds 1

TILECOLOR        ds 1

PLAYERY          ds 1  ; Player's Y position
PLAYERX          ds 1  ; Player's X position

OLDPLAYERY       ds 1
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
MINED_CELL_X     ds 1

SCREEN_FRAME     ds 1
LADDER_LINE_IDX  ds 1
PLAYER_LINE_IDX  ds 1

SCORE_HEIGHT     ds 1

;------------------------------------------------------
;                  63 | 65 bytes free
;------------------------------------------------------
    ;           ROM
    SEG
    ORG $F000 ;4K Rom , F8000 = 2K

Reset:

    CLEAN_START ; from macro.h

    lda #15     ; some gray color for the player
    sta COLUP0

    ;set player coordinates
    lda #69
    sta PLAYERY
    sta OLDPLAYERY
    lda #3
    sta PLAYERX
    sta OLDPLAYERX
    lda #132
    sta RANDOM

    lda #0
    sta LADDER1X
    lda #5
    sta LADDER2X
    lda #6
    sta LADDER3X
    lda #114
    sta LADDER4X
    lda #132
    sta LADDER5X


    lda #%00011000
    sta GRP1
    lda #$C6
    sta COLUP1

    lda #0
    sta SCREEN_FRAME
    sta LADDER_IDX

    
    jsr GenerateMap


Main:
    jsr Vsync
    jsr VBlank
    jsr Kernel 
    jsr Overscan
    jmp Main
;--------------------------
Kernel:
    sta WSYNC
    lda INTIM
    bne Kernel ; wait for vsync timer

    sta VBLANK


    lda #%00000000
    sta PF0
    sta PF1
    sta PF2

    lda #0
    sta GRP0 ;let's clear the sprite


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


    lda TILECOLOR
    sta COLUPF      ;3 brown bricks

    lda PLAYER_FLIP
    sta REFP0


    ldx #72 ; scanlines, max scanlines / 2

    ;sta WSYNC

KERNEL_LOOP:
    ;------------------------------------------------------------------------
    cpy #0                  ;2 21
    beq drawThePlayer       ;2 23 finished drawing one ladder sprite

    lda LADDER_GFX,y        ;4 
    sta GRP1                ;3
    dey                     ;2
    sta HMCLR

drawThePlayer:
    sty LADDER_LINE_IDX     ;3 41
    ldy PLAYER_LINE_IDX     ;3 44
    cpx PLAYERY             ;3 3     can we draw the player sprite?
    bcs nope                ;2 5     < PLAYERY
    cpy #0                  ;2 7     we already went through all sprite lines
    beq nope                ;2 9

    lda (PLAYERPTR),y       ;5 14    let's load a line from a sprite frame
    sta GRP0                ;3 17    and store it to the Player0 sprite
    dey                     ;2 19
    sty PLAYER_LINE_IDX     ;3 21
nope:
    ldy LADDER_LINE_IDX     ;3 22

    ;-------------------------------------------------------------------------

    sta WSYNC           ;wait for the scanline to be drawn

    ;---------------DRAW MAP--------------------------------------------------

    stx TEMP_X_INDEX        ;3 3    save scanline index

    ldx SCREENMAP_IDX       ;3 6

    lda GAMEMAP0,x          ;4 10
    sta PF0                 ;3 13

    lda GAMEMAP1,x          ;4 17
    sta PF1                 ;3 20

    lda GAMEMAP2,x          ;4 24
    sta PF2                 ;3 27

    ;------right side of the screen

    lda GAMEMAP3,x          ;4 31
    sta PF0                 ;3 34

    nop                     ;2 36
    nop                     ;2 38
    
    lda GAMEMAP4,x          ;4 42
    sta PF1                 ;3 45
    
    nop                     ;2 47
    nop                     ;2 49

    lda GAMEMAP5,x          ;4 53
    sta PF2                 ;3 56
    
    ;--- some code I wanted to place in to this scanline

    lda #0                  ;2 58   Lets turn off the playfield for one scanline
    ldx LINE_IDX            ;3 61   decrement current line count for one map cell

    sta PF0                 ;3 63
    sta PF1                 ;3 66
    dex                     ;2 72
    ;---------------------------------------------
    sta WSYNC           ;   finish the scanline, we don't want to cram sprite drawing instructions to be here
    ;---------------------------------------------
    sta PF2                 ;3 3  clearing the remaining playfield register
    bne cont                ;2 5

    ldx LADDER_IDX          ;3 8
    lda LADDER1X,x          ;4 12
    sec                     ;2 14
divLoop:
    sbc #15                 ;2 14
    bcs divLoop             ;3 17

    tay                     ;2 18
    lda FINE_ADJUST_TABLE,y ;4 22

    sta RESP1
    sta HMP1

    ldy #LADDERHEIGHT       ;2 7  reset the ladder sprite
    sta WSYNC
    sta HMOVE
    ;---------------------------------------------
    inc LADDER_IDX          ;let's position next ladder
    dec SCREENMAP_IDX       ;5 10  move to next map cell
    ldx #LINESPERCELL       ;2  reset line count
cont:
    stx LINE_IDX            ;3 12 save current line count
    ldx TEMP_X_INDEX        ;3 15 restore X (scanline index)

    ;---------------------------------------------
    dex                     ;2 17
    bne KERNEL_LOOP ;       ;2 19

    ;-----------------------------------------------




    ;let's draw a score
    ;---------------------------------------------
    lda #0
    sta GRP0
    sta WSYNC   ;let's draw an empty line
    sta HMOVE

    lda #%00000011 ;2 2
    sta NUSIZ0     ;3 5
    sta NUSIZ1     ;3 8

    lda #15        ;2 10
    sta COLUP1     ;3 13
    lda #0         ;2 15
    
    sta REFP0      ;3  turn off mirroring
    sta RESP0      ;2 reset sprite pos
    sta RESP1

    lda #$0        ;2
    sta HMP0       ;3  reset p1 x offset
    lda #254       ;2  move p2 sprite right a bit
    sta HMP1       ;3 
    nop            ;2 
    


    ldy #PLAYERHEIGHT
    sty SCORE_HEIGHT
    lda #1
    sta VDELP0
    sta VDELP1


score_line_loop:

    ldy SCORE_HEIGHT    ;3 52

    lda FIVE_GFX,y      ;4 56
    sta TMPNUM          ;3 59
    lda ZERO_GFX,y      ;4 63
    sta GRP0            ;3 66

    sta WSYNC           ;----------
    ;sta HMOVE           ;3 3

    lda ONE_GFX,y       ;4 7
    sta GRP1            ;3 10
    lda TWO_GFX,y       ;4 14
    sta GRP0            ;3 17
    lda FOUR_GFX,y      ;4 21
    tax                 ;2 23
    lda THREE_GFX,y     ;4 27
    ldy TMPNUM

    sta GRP1            ;3 30
    stx GRP0            ;3 33
    sty GRP1            ;3 36 last digit
    sta GRP0            ;3 39

    dec SCORE_HEIGHT    ;5 47
    ;sta HMCLR          ;3 42 let's clear HM

    bpl score_line_loop ;2 49

;----------------------------------------
    lda #0
    sta GRP0
    sta WSYNC   ;let's draw an empty line
    sta HMOVE

    lda #%00000011 ;2 2
    sta NUSIZ0     ;3 5
    sta NUSIZ1     ;3 8

    lda #65        ;2 10
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
    ldx #27 ; remaining lines
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

    lda #0
    sta COLUBK
    sta NUSIZ0
    sta NUSIZ1
    lda #$C6
    sta COLUP1



    rts
;----------------------------
Overscan:
    sta WSYNC
    lda #2
    sta VBLANK
    lda #35
    sta TIM64T

    ;some game logic here
    

    bit CXP0FB
    bpl notColliding
    ;so the player is colliding with the playfield
    lda OLDPLAYERX
    sta PLAYERX
    lda OLDPLAYERY
    sta PLAYERY
    lda OLDPLAYER_FRAME
    sta PLAYER_FRAME


notColliding:
    sta CXCLR

    
    jsr ProcessInput

   


    
OverscanLoop:
    sta WSYNC
    lda INTIM
    bne OverscanLoop

    rts


Vsync:

    lda #2
    ldx #49
    sta WSYNC
    sta VSYNC
    stx TIM64T
    sta WSYNC
    sta WSYNC
    lda #0
    sta WSYNC
    sta VSYNC
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
    cmp #9
    bcc checkButton
    ldx PLAYERY
    stx OLDPLAYERY
    sta PLAYERY
    lda #%00010000  ; after 3 x lsr it will turn into 2(3rd frame - climbing)
    ldx PLAYER_FRAME
    stx OLDPLAYER_FRAME
    sta PLAYER_FRAME
checkButton:
;----------------------------------------------
    bit INPT4   ;checking button press;
    bmi buttonNotPressed ;jump if the button wasn't pressed
    ;----

    lda #%00011111  ; after 3 x lsr it will turn into 3(4th frame - mining)
    sta PLAYER_FRAME

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

    cpx #12
    bcs doneMining ; nope, the x >= MAPWIDTH


    stx MINED_CELL_X;

    lda PLAYERY
    ldx #0
divy:
    inx
    sbc #12
    bcs divy
    dex     ; playerY / 12 - 1

    stx TEMPY

    ldx MINED_CELL_X

    lda MAP_3CELLS_LOOKUP,x
    clc
    adc TEMPY
    sta TMPNUM1


    lda MAP_3CELLS_INTERSECTIONS,x
    cmp #1
    beq mine_onlyOneSegmentUsed
    ;Let's change the second segment

    ldx TMPNUM1
    lda GAMEMAP0,x + 6
    ldx MINED_CELL_X              ;load x coord
    and MAP_CLEAR_PATTERN_BY_X_SEG2,x
    ldx TMPNUM1
    sta GAMEMAP0,x + 6

    ;jmp doneMining

mine_onlyOneSegmentUsed:

    ldx TMPNUM1
    lda GAMEMAP0,x
    ldx MINED_CELL_X
    and MAP_CLEAR_PATTERN_BY_X_SEG1,x
    ldx TMPNUM1
    sta GAMEMAP0,x

doneMining:

    ;----   Let's restore the X
    lda PLAYER_DIR
    cmp #1
    bne buttonNotPressed
    lda PLAYERX
    clc
    sbc #X_OFFSET_TO_RIGHT_FOR_MINING
    sta PLAYERX

buttonNotPressed:

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

    ldy #0
    lda #4                      ;inverted first y position
    sta TEMPY
    ;----------
ladderLoop:                     ;  let's generate a ladder for each of the map rows
    jsr UpdateRandomNumber
    and #11                     ;  limit to 0..11 range
    tax                         ;  and transfer to X register
    lda LADDER_X_POSSITIONS,x
    sta LADDER1X,y              ;  store sprite position to ram variable


    lda MAP_3CELLS_INTERSECTIONS,x
    stx TMPNUM                  ;store x

    cmp #1
    beq onlyOneSegmentUsed
                                ;two segments used for a map tile

    lda MAP_3CELLS_LOOKUP,x
    clc
    adc TEMPY
    sta TMPNUM1
    tax
    lda GAMEMAP0,x
    ldx TMPNUM              ;load x coord
    and MAP_CLEAR_PATTERN_BY_X_SEG1,x
    ldx TMPNUM1
    sta GAMEMAP0,x
    lda GAMEMAP0,x + 6
    ldx TMPNUM              ;load x coord
    and MAP_CLEAR_PATTERN_BY_X_SEG2,x
    ldx TMPNUM1
    sta GAMEMAP0,x + 6

    jmp nextLadder

onlyOneSegmentUsed:

    lda MAP_3CELLS_LOOKUP,x
    clc
    adc TEMPY
    sta TMPNUM1
    tax
    lda GAMEMAP0,x
    ldx TMPNUM
    and MAP_CLEAR_PATTERN_BY_X_SEG1,x
    ldx TMPNUM1
    sta GAMEMAP0,x


nextLadder:
    iny
    lda #5
    sty TEMPY
    sbc TEMPY
    sta TEMPY 
    cpy #5
    bne ladderLoop



    rts


;-------------------------
VBlank:

    lda PLAYERX
    ldx #0
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

    sta WSYNC
    sta HMOVE


    lda #$FC        ;brown bricks
    sta TILECOLOR
   
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
    ldx PLAYER_FRAME
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


ZERO_GFX:
    .byte %00000000
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
    .byte %00000000
    .byte %00111100
    .byte %01000110
    .byte %00000110
    .byte %00011100
    .byte %00011110
    .byte %01000110
    .byte %01000110
    .byte %00111100

FOUR_GFX:
    .byte %00000000
    .byte %00000000
    .byte %00001100
    .byte %00001100
    .byte %11111110
    .byte %11111110
    .byte %01000100
    .byte %00100100
    .byte %00010100
    .byte %00001100

FIVE_GFX:
    .byte %00000000
    .byte %00000000
    .byte %00111100
    .byte %01000110
    .byte %00000110
    .byte %00000110
    .byte %01111100
    .byte %01000000
    .byte %01000000
    .byte %01111110





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

MAP_3CELLS_LOOKUP:
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
