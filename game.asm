 PROCESSOR 6502
    include "vcs.h"
    include "macro.h"


MAPSIZE = 36        ;(12 * 6) / 2
MAPHEIGHT = 6
PLAYERHEIGHT = 9
LADDERHEIGHT = 11
LINESPERCELL = 12
NO_ILLEGAL_OPCODES = 1 ; DASM needs it

;----------------------------------------------------
;           RAM
;-----------------------------------------------------
    SEG.U VARS
    ORG $80


THEMAP          ds MAPSIZE

GAMEMAP0        ds 6 
GAMEMAP1        ds 6 
GAMEMAP2        ds 6 
GAMEMAP3        ds 6 
GAMEMAP4        ds 6
GAMEMAP5        ds 6

PLAYERY         ds 1  ; Player's Y position
PLAYERX         ds 1  ; Player's X position

PLAYER_DIR      ds 1  ; Player's direction
LADDERX         ds 1

INPUT           ds 1  ; Input from the joystick
TEMP_X_INDEX    ds 1  ; Temp variable used in drawing, and in input checking

SCREENMAP_IDX:        ;index of the screenmap line; used in drawing
TMPSCREENCELL1:       ;temporary variable for fillscreenmap
                ds 1
TMPSCREENCELL:        ;temporary variable for fillscreenmap
LINE_IDX:             ; line counter for a map cell
                ds 1

PLAYERPTR       ds 2  ;16bit address of the active sprites's frame graphics
PLAYER_FRAME    ds 1  ;frame index
TMPNUM          ds 1
SCREEN_FRAME    ds 1
LADDER_LINE_IDX ds 1
PLAYER_LINE_IDX ds 1
;------------------------------------------------------
;                  87 | 41 bytes free
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
    lda #3
    sta PLAYERX

    lda #140
    sta LADDERX


    lda #%00011000
    sta GRP1
    lda #$C6
    sta COLUP1

    lda #0
    sta SCREEN_FRAME


    
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

    lda #$FC
    sta COLUPF      ;brown bricks

    
    ldy #PLAYERHEIGHT
    sty PLAYER_LINE_IDX     

    lda #MAPHEIGHT-1
    sta SCREENMAP_IDX ; bottom of the screenmap

    lda #LINESPERCELL
    sta LINE_IDX

    ldy #LADDERHEIGHT
    sty LADDER_LINE_IDX

    ldx #72 ; scanlines, max scanlines / 2

    ;sta WSYNC

KERNEL_LOOP:
    ;------------------------------------------------------------------------
    cpy #0                  ;2 21
    beq resetTheLadder      ;2 23 finished drawing one ladder sprite

    lda LADDER_GFX,y        ;4 
    sta GRP1                ;3
    dey                     ;2
    jmp drawThePlayer

resetTheLadder:
    ldy #LADDERHEIGHT       ; reset the ladder sprite
    sta RESP1

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
    sta PF2                 ;3 69

    ;this probably adds up to the sprite scanline
    bne cont                ;2 2
    dec SCREENMAP_IDX       ;5 7  move to next map cell
    ldx #LINESPERCELL       ;2 9  reset line count
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
    SLEEP 4        ;6 21

    sta RESP0      ;2 reset sprite pos
    sta RESP1

    sta REFP0      ;3  turn off mirroring
    lda #$0        ;2
    sta HMP0       ;3  reset p1 x offset
    lda #$11       ;2  move p2 sprite left a bit
    sta HMP1       ;3 
    nop            ;2 
    


    ldy #PLAYERHEIGHT
    ldx #10 ; 10 lines

score_line_loop:

    sta WSYNC
    sta HMOVE
    cpy #0
    beq okok
    lda ONE_GFX,y
    sta GRP1
    lda ZERO_GFX,y
    sta GRP0
    nop
    lda TWO_GFX,y
    sta GRP0
    lda TWO_GFX,y
    sta GRP1
    lda ZERO_GFX,y
    sta GRP1
    lda ONE_GFX,y
    sta GRP0
    sta HMCLR   ; let's clear HM

    dey
okok:
    dex
    bne score_line_loop

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
    ldx #33 ; remaining lines

score_line_loop1:

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
    bne score_line_loop1

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
    lda #1
    sta PLAYER_DIR
    lda PLAYERX
    adc #6
    cmp #140
    bcs checkLeft
    sta PLAYERX


    ;-------------Let's check the collision
    jsr GetActiveMapCell

    lsr 
    bcs secondsegRight
    adc TMPNUM
    tax
    dex
    lda THEMAP,x
    eor #%00000001
    and #%00001111
    beq revertRight
    jmp walkRight
secondsegRight:
    adc TMPNUM
    tax
    dex
    lda THEMAP,x
    eor #%00010000
    and #%11110000
    beq revertRight
    jmp walkRight
revertRight ; need to revert x that was used for collision check
    lda PLAYERX
    sec
    sbc #6
    sta PLAYERX
    jmp exitRightCollisionCheck

walkRight:

    lda PLAYERX
    sec
    sbc #5
    sta PLAYERX

exitRightCollisionCheck:
    ;------------

    lda #%00000000
    sta REFP0
    ldx PLAYER_FRAME
    inx
    cpx #16 ;we must not let go to animation frame 3, 00010 000
    bcc storeframe1
    ldx #0
storeframe1:
    stx PLAYER_FRAME



checkLeft:
    lda INPUT
    asl
    sta INPUT
    bcs checkDown
moveLeft:
    lda #2
    sta PLAYER_DIR

    lda PLAYERX
    ;sbc #1
    adc #1
    cmp #2
    bcc checkDown
    sta PLAYERX
    ;-------------Let's check the collision
    jsr GetActiveMapCell
    lsr 
    bcs twosegLeft
    adc TMPNUM
    tax
    dex
    lda THEMAP,x
    eor #%00000001
    and #%00001111
    beq revertLeft
    jmp walkleft
twosegLeft:
    adc TMPNUM
    tax
    dex
    lda THEMAP,x
    eor #%00010000
    and #%11110000
    beq revertLeft
    jmp walkleft
revertLeft
    lda PLAYERX
    sec
    sbc #1
    sta PLAYERX
    jmp exitLeftColisionCheck

walkleft:
    lda PLAYERX
    sec
    sbc #2
    sta PLAYERX
exitLeftColisionCheck:
    ;----------
    lda #%00001000
    sta REFP0
    ldx PLAYER_FRAME
    inx
    cpx #16 ;we must not let go to animation frame 3, 00010 000
    bcc storeframe2
    ldx #0
storeframe2:
    stx PLAYER_FRAME

checkDown:
    lda INPUT
    asl
    sta INPUT
    bcs checkUp
moveDown:
    lda PLAYERY
    sbc 1
    cmp #9
    bcc checkUp
    sta PLAYERY
    lda #%00010000  ; after 3 x lsr it will turn into 2(3rd frame - climbing)
    sta PLAYER_FRAME
checkUp:
    lda INPUT
    asl
    bcs exit
    lda PLAYERY
    adc 1
    cmp #72
    bcs exit
    sta PLAYERY
    lda #%00010000  ; after 3 x lsr it will turn into 2(3rd frame - climbing)
    sta PLAYER_FRAME
exit:
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
    adc #6          ;need to add a bit to player x, when it's facing right
    sta PLAYERX

checkCellCollision:

    jsr GetActiveMapCell

    lsr
    bcs secondsegmentmined
    adc TMPNUM
    tax
    dex
    lda THEMAP,x
    and #%11110000
    jmp changemap
secondsegmentmined:
    adc TMPNUM
    tax
    dex
    lda THEMAP,x
    and #%00001111
changemap:
    sta THEMAP,x

    ;----   Let's restore the X
    lda PLAYER_DIR
    cmp #1
    bne buttonNotPressed
    lda PLAYERX
    clc
    sbc #6
    sta PLAYERX

buttonNotPressed:
    
    rts
;-------------------------------------
;Takes PLAYERX & PLAYERY
;Stores y*row len in TMPNUM
;X in A
GetActiveMapCell
    lda PLAYERX
    ldx #0
div:            ; divide PLAYERX by 12, result goes to x
    inx
    sbc #12
    bcs div
    txa         ;divide the result by 2
    sta TEMP_X_INDEX
    ;--------------
    lda PLAYERY
    ldx #0
divy:
    inx
    sbc #12
    bcs divy

    stx TMPNUM
    lda #7
    sbc TMPNUM
    tax

    lda #0
multiply:
    cpx #0
    beq zerorow
    adc #5
    dex
    jmp multiply
zerorow:
    
    sta TMPNUM ; store ( y * row len)
    lda TEMP_X_INDEX

    rts


;--------------------------------------
; Moves sprite horizontaly
; A : X of the sprite
; X : 0- player0, 1 - player1
PosSpriteX: 
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

;--------------------------------------------------------------
;Fills a row from the logic map to the screenmap
Fill_ScreenMaps_row:

cell_0_1: 

    lda #%00000000
    sta TMPSCREENCELL
    sta TMPSCREENCELL1

    lda THEMAP,x
    eor #%00010000
    and #%11110000
    bne nextseg0

    lda #%10000000 ;this goes to PF0
    sta TMPSCREENCELL
    lda #%11000000  ; PF1
    sta TMPSCREENCELL1

nextseg0:
    lda THEMAP,x
    eor #%00000001
    and #%00001111
    bne store_0_1
    ;
    lda TMPSCREENCELL1
    eor #%00111000
    sta TMPSCREENCELL1

store_0_1:

    lda TMPSCREENCELL
    sta GAMEMAP0,y
    
    lda TMPSCREENCELL1
    sta GAMEMAP1,y

;--------------------------------------------------------------
cell_2_3: 

    inx

    lda #0
    sta TMPSCREENCELL

    lda THEMAP,x
    eor #%00010000
    and #%11110000
    bne nextseg1

    lda TMPSCREENCELL1
    eor #%00000111
    sta TMPSCREENCELL1

nextseg1:

    lda THEMAP,x
    eor #%00000001
    and #%00001111
    bne store_2_3
    ;
    lda TMPSCREENCELL
    eor #%00000111
    sta TMPSCREENCELL


store_2_3:
    
    lda TMPSCREENCELL
    sta GAMEMAP2,y
    lda TMPSCREENCELL1
    sta GAMEMAP1,y  ; PF1

;--------------------------------------------------------------
cell_4_5: ;------------------------
    
    inx

    lda #0
    sta TMPSCREENCELL1

    lda THEMAP,x
    eor #%00010000
    and #%11110000
    bne nextseg2

    lda TMPSCREENCELL
    eor #%00111000
    sta TMPSCREENCELL

nextseg2:

    lda THEMAP,x
    eor #%00000001
    and #%00001111
    bne store_4_5
    ;
    lda TMPSCREENCELL
    eor #%11000000
    sta TMPSCREENCELL
    lda #%00010000
    sta TMPSCREENCELL1


store_4_5:

    lda TMPSCREENCELL
    sta GAMEMAP2,y
    lda TMPSCREENCELL1
    sta GAMEMAP3,y

;--------------------------------------------------------------
cell_6_7: ;------------------------

    inx

    lda #%0
    sta TMPSCREENCELL

    lda THEMAP,x
    eor #%00010000
    and #%11110000
    bne nextseg3

    lda TMPSCREENCELL1
    eor #%11100000
    sta TMPSCREENCELL1

nextseg3:

    lda THEMAP,x
    eor #%00000001
    and #%00001111
    bne store_6_7
    
    lda #%11100000
    sta TMPSCREENCELL


store_6_7:

    lda TMPSCREENCELL1
    sta GAMEMAP3,y

    lda TMPSCREENCELL
    sta GAMEMAP4,y

;--------------------------------------------------------------
cell_8_9: ;------------------------

    inx

    lda #0
    sta TMPSCREENCELL1

    lda THEMAP,x
    eor #%00010000
    and #%11110000
    bne nextseg4

    lda TMPSCREENCELL
    eor #%00011100
    sta TMPSCREENCELL

nextseg4:

    lda THEMAP,x
    eor #%00000001
    and #%00001111
    bne store_8_9
    ;
    
    lda TMPSCREENCELL
    eor #%00000011
    sta TMPSCREENCELL
    lda #%00000001
    sta TMPSCREENCELL1


store_8_9:

    lda TMPSCREENCELL
    sta GAMEMAP4,y  ;
    lda TMPSCREENCELL1
    sta GAMEMAP5,y


    ;--------------------------------------------------------------
cell_10_11: ;----------------------

    inx


    lda THEMAP,x
    eor #%00010000
    and #%11110000
    bne nextseg5

    lda TMPSCREENCELL1
    eor #%00001110
    sta TMPSCREENCELL1

nextseg5:

    lda THEMAP,x
    eor #%00000001
    and #%00001111
    bne store_10_11_y0
    ;
    lda TMPSCREENCELL1
    eor #%01110000
    sta TMPSCREENCELL1

    

store_10_11_y0:

    lda TMPSCREENCELL1
    sta GAMEMAP5,y
    sta GAMEMAP5,y

    rts

;--------------------------------------------------------------
;Fills screen map according to what is on the logics map aka THEMAP
FillScreenMap:

    ldx #0
    stx TMPSCREENCELL
    stx TMPSCREENCELL1
    ldy #MAPHEIGHT-1

rowloop:

    jsr Fill_ScreenMaps_row

    inx
    dey
    cpy #255    ;supposedly -1
    bne rowloop


    rts

;--------------------------
GenerateMap:

    ldx #MAPSIZE
    lda #%00010001
genloop:
    sta THEMAP,x
    dex
    bne genloop
    
    rts


;-------------------------
VBlank:

    lda PLAYERX
    ldx #0
    jsr PosSpriteX
    sta WSYNC
    sta HMOVE

    ;lda LADDERX
    ;ldx #1
    ;jsr PosSpriteX

;    ldx SCREEN_FRAME
;    inx
;    cpx #2
;    bne continueVBlank

;    ldx #0

;continueVBlank:
 ;   stx SCREEN_FRAME

    jsr FillScreenMap
    ;jsr FillScreenMapWithRomData


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
