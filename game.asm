 PROCESSOR 6502
    include "vcs.h"
    include "macro.h"


MAPSIZE = 36        ;(12 * 6) / 2
PLAYERHEIGHT = 9
LINESPERCELL = 6

;----------------------------------
    ;           RAM
    SEG.U VARS
    ORG $80


THEMAP          ds MAPSIZE

GAMEMAP0        ds 12 
GAMEMAP1        ds 12 
GAMEMAP2        ds 12 
GAMEMAP3        ds 12 
GAMEMAP4        ds 12
GAMEMAP5        ds 12

PLAYERY         ds 1  ; Player's Y position
PLAYERX         ds 1  ; Player's X position
INPUT           ds 1  ; Input from the joystick
TEMP_X_INDEX    ds 1
SCREENMAP_IDX   ds 1  ; index of the screenmap
LINE_IDX        ds 1  ; line counter for a map cell

TMPSCREENCELL   ds 1    ;two temp variables to help fill the screenmap
TMPSCREENCELL1  ds 1

PLAYERPTR       ds 2    ;16bit address of the active sprites's frame graphics
PLAYER_FRAME    ds 1    ;frame index
;------------------------------------------------------
;                  117 | 9 free
;----------------------------------
    ;           ROM
    SEG
    ORG $F000 ;4K Rom , F8000 = 2K

Reset:

    ; Clear ram and TIA registers
    ldx #0
    lda #0
Clear:
    sta 0,x
    inx
    bne Clear

    lda #15     ; some gray color for the player
    sta COLUP0

    ;set player coordinates
    lda #65
    sta PLAYERY
    lda #2
    sta PLAYERX

    lda #%00011000
    sta GRP1
    lda #$C6
    sta COLUP1

    
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

    lda #$1C
    sta COLUPF      ;make playfield yellow

    ldy #PLAYERHEIGHT

    lda #11
    sta SCREENMAP_IDX ; bottom of the screenmap

    lda #LINESPERCELL
    sta LINE_IDX


    ldx #72 ; scanlines, max scanlines / 2

@KERNEL_LOOP:

    jsr DrawPlayer

    sta WSYNC           ;wait for the scanline to be drawn

    ;---------------DRAW MAP----------------------

    stx TEMP_X_INDEX    ;3 3    save scanline index

    ldx SCREENMAP_IDX   ;3 6

    lda GAMEMAP0,x      ;4 10
    sta PF0             ;3 13

    lda GAMEMAP1,x      ;4 17
    sta PF1             ;3 20

    lda GAMEMAP2,x      ;4 24
    sta PF2             ;3 27


    lda GAMEMAP3,x      ;4 31
    sta PF0             ;3 34

    nop                 ;2 36
    nop                 ;2 38
    
    lda GAMEMAP4,x      ;4 42
    sta PF1             ;3 45
    
    nop                 ;2 47
    nop                 ;2 49

    lda GAMEMAP5,x      ;4 53
    sta PF2             ;3 56

    ;---------------------------------------------
    sta WSYNC           ;finish the scanline, we don't want to cram sprite drawing instructions to be here
    lda #0              ; Lets turn off the playfield for one scanline
    sta PF0             ;3 
    sta PF1             ;3 
    sta PF2             ;3 

    ldx LINE_IDX        ;decrement current line count for one map cell
    dex
    bne @cont
    dec SCREENMAP_IDX   ;5 move to next map cell
    ldx #LINESPERCELL   ;reset line count
@cont:
    stx LINE_IDX        ;save current line count
    ldx TEMP_X_INDEX    ;restore X (scanline index)

    ;---------------------------------------------
    dex              ;
    bne @KERNEL_LOOP ;

    rts
;-----------------------------
DrawPlayer:
    cpx PLAYERY     ;can we draw the player sprite?
    bcs @nope       ; < PLAYERY
    cpy #0          ;we already went through all sprite lines
    beq @nope

    lda (PLAYERPTR),y
    sta GRP0
    dey
@nope:
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

    lda PLAYERX
    ldx #0
    jsr PosSpriteX
    sta WSYNC
    sta HMOVE

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
    lda PLAYERX
    adc 1
    cmp #140
    bcs checkLeft
    sta PLAYERX
    lda #%00000000
    sta REFP0
    ldx PLAYER_FRAME
    inx
    cpx #16 ;we must not let go to animation frame 3, 00010 000
    bne storeframe1
    ldx #0
storeframe1:
    stx PLAYER_FRAME
checkLeft:
    lda INPUT
    asl
    sta INPUT
    bcs checkDown
moveLeft:
    lda PLAYERX
    sbc 1
    cmp #2
    bcc checkDown
    sta PLAYERX
    lda #%00001000
    sta REFP0
    ldx PLAYER_FRAME
    inx
    cpx #16 ;we must not let go to animation frame 3, 00010 000
    bne storeframe2
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
checkUp:
    lda INPUT
    asl
    bcs exit
    lda PLAYERY
    adc 1
    cmp #72
    bcs exit
    sta PLAYERY
exit:
    rts
;--------------------------------------
PosSpriteX: ;stole this from Adventure
    ldy     #$02
    sec
@loop:
    iny
    sbc #$0f
    bcs @loop
    eor #$ff
    sbc #$06
    asl
    asl 
    asl
    asl
    sty WSYNC
@loop1:
    dey
    bpl @loop1
    sta RESP0,x
    sta HMP0,x

    rts

;--------------------------------------------------------------
cell_0_1: 

    lda #%00000000
    sta TMPSCREENCELL
    sta TMPSCREENCELL1

    lda THEMAP,x
    eor #%00010000
    and #%11110000
    bne nextseg0

    lda #%01110000
    sta TMPSCREENCELL

nextseg0:
    lda THEMAP,x
    eor #%00000001
    and #%00001111
    bne store_0_1
    ;
    lda TMPSCREENCELL
    eor #%10000000
    sta TMPSCREENCELL
    lda #%11000000       ;this goes to PF1
    sta TMPSCREENCELL1

store_0_1:

    lda TMPSCREENCELL
    sta GAMEMAP0,y
    dey
    sta GAMEMAP0,y
    
    lda TMPSCREENCELL1
    sta GAMEMAP1,y
    iny
    sta GAMEMAP1,y

    rts
;--------------------------------------------------------------
cell_2_3: 

    inx

    lda THEMAP,x
    eor #%00010000
    and #%11110000
    bne nextseg1

    lda TMPSCREENCELL1
    eor #%00111000
    sta TMPSCREENCELL1

nextseg1:

    lda THEMAP,x
    eor #%00000001
    and #%00001111
    bne store_2_3
    ;
    lda TMPSCREENCELL1
    eor #%00000111
    sta TMPSCREENCELL1


store_2_3:

    lda TMPSCREENCELL1
    sta GAMEMAP1,y  ; 
    dey
    sta GAMEMAP1,y

    rts

;--------------------------------------------------------------
cell_4_5: ;------------------------
    
    inx

    lda #%00000000
    sta TMPSCREENCELL

    lda THEMAP,x
    eor #%00010000
    and #%11110000
    bne nextseg2

    lda #%00000111
    sta TMPSCREENCELL

nextseg2:

    lda THEMAP,x
    eor #%00000001
    and #%00001111
    bne store_4_5
    ;
    lda TMPSCREENCELL
    eor #%00111000
    sta TMPSCREENCELL

    

store_4_5:

    lda TMPSCREENCELL
    sta GAMEMAP2,y
    iny
    sta GAMEMAP2,y
    
    rts
;--------------------------------------------------------------
cell_6_7: ;------------------------

    inx

    lda #%00000000
    sta TMPSCREENCELL1

    lda THEMAP,x
    eor #%00010000
    and #%11110000
    bne nextseg3

    lda TMPSCREENCELL
    eor #%11000000
    sta TMPSCREENCELL
    lda #%00010000
    sta TMPSCREENCELL1

nextseg3:

    lda THEMAP,x
    eor #%00000001
    and #%00001111
    bne store_6_7
    ;
    lda TMPSCREENCELL1
    eor #%11100000
    sta TMPSCREENCELL1


store_6_7:

    lda TMPSCREENCELL
    sta GAMEMAP2,y  
    dey
    sta GAMEMAP2,y

    lda TMPSCREENCELL1
    sta GAMEMAP3,y
    iny
    sta GAMEMAP3,y

    rts
;--------------------------------------------------------------
cell_8_9: ;------------------------

    inx

    lda #%00000000
    sta TMPSCREENCELL

    lda THEMAP,x
    eor #%00010000
    and #%11110000
    bne nextseg4

    lda #%11100000
    sta TMPSCREENCELL

nextseg4:

    lda THEMAP,x
    eor #%00000001
    and #%00001111
    bne store_8_9
    ;
    lda TMPSCREENCELL
    eor #%00011100
    sta TMPSCREENCELL


store_8_9:

    lda TMPSCREENCELL
    sta GAMEMAP4,y  ; y = 11
    dey
    sta GAMEMAP4,y

    rts

;--------------------------------------------------------------
cell_10_11: ;----------------------

    inx
    lda #%00000000
    sta TMPSCREENCELL1



    lda THEMAP,x
    eor #%00010000
    and #%11110000
    bne nextseg5

    lda TMPSCREENCELL
    eor #%00000011
    sta TMPSCREENCELL
    lda #%00000001
    sta TMPSCREENCELL1

nextseg5:

    lda THEMAP,x
    eor #%00000001
    and #%00001111
    bne store_10_11_y0
    ;
    lda TMPSCREENCELL1
    eor #%00001110
    sta TMPSCREENCELL1

    

store_10_11_y0:

    lda TMPSCREENCELL
    sta GAMEMAP4,y  ; y = 10
    iny
    sta GAMEMAP4,y
    lda TMPSCREENCELL1
    sta GAMEMAP5,y
    dey
    sta GAMEMAP5,y

    rts


;--------------------------------------------------------------
;Fills screen map according to what is on the logics map aka THEMAP
FillScreenMap:

    ldx #0
    stx TMPSCREENCELL
    stx TMPSCREENCELL1
    ldy #11             ;we have 2x6 = 12 rows, since we're counting from 0, it's 11

@rowloop:

    jsr cell_0_1
    jsr cell_2_3
    jsr cell_4_5
    jsr cell_6_7
    jsr cell_8_9
    jsr cell_10_11

    inx
    dey
    cpy #255    ;supposedly -1
    bne @rowloop


    rts

;----------------------------------

FillScreenMapWithRomData:
    ldx #11
@rep0:
    lda ROM_GAMEMAP0,x
    sta GAMEMAP0,x
    dex
    bne @rep0

    lda ROM_GAMEMAP0,0
    sta GAMEMAP0,0

    ldx #11
@rep1:
    lda ROM_GAMEMAP1,x
    sta GAMEMAP1,x
    dex
    bne @rep1

    lda ROM_GAMEMAP1,0
    sta GAMEMAP1,0

    ldx #11
@rep2:
    lda ROM_GAMEMAP2,x
    sta GAMEMAP2,x
    dex
    bne @rep2

    lda ROM_GAMEMAP2,0
    sta GAMEMAP2,0

    ldx #11
@rep3:
    lda ROM_GAMEMAP3,x
    sta GAMEMAP3,x
    dex
    bne @rep3

    lda ROM_GAMEMAP3,0
    sta GAMEMAP3,0

    ldx #11
@rep4:
    lda ROM_GAMEMAP4,x
    sta GAMEMAP4,x
    dex
    bne @rep4


    lda ROM_GAMEMAP4,0
    sta GAMEMAP4,0


    ldx #11
@rep5:
    lda ROM_GAMEMAP5,x
    sta GAMEMAP5,x
    dex
    bne @rep5

    lda ROM_GAMEMAP5,0
    sta GAMEMAP5,0

    rts
;--------------------------
GenerateMap:
    ldx #0
@GenLoop:
    lda #%00010000
    sta THEMAP,x
    inx
    cpx #MAPSIZE
    bne @GenLoop

    lda #%00000001
    ldy #6
    sta THEMAP,y
    ldy #7
    sta THEMAP,y
    ldy #8
    sta THEMAP,y
    ldy #9
    sta THEMAP,y
    ldy #10
    sta THEMAP,y
    ldy #11
    sta THEMAP,y

    ldy #18
    sta THEMAP,y
    ldy #19
    sta THEMAP,y
    ldy #20
    sta THEMAP,y
    ldy #21
    sta THEMAP,y
    ldy #22
    sta THEMAP,y
    ldy #23
    sta THEMAP,y

    ldy #30
    sta THEMAP,y
    ldy #31
    sta THEMAP,y
    ldy #32
    sta THEMAP,y
    ldy #33
    sta THEMAP,y
    ldy #34
    sta THEMAP,y
    ldy #35
    sta THEMAP,y

    rts


;-------------------------
VBlank:
    
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
DWARF_PTR_HIGH: ; high 8bits of 16bit address
    .byte >(DWARF_GFX_0)
    .byte >(DWARF_GFX_1)


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


LADDER_GFX:
    .byte %00000000
    .byte %00000000
    .byte %01000010
    .byte %01111110
    .byte %01000010
    .byte %01111110
    .byte %01000010
    .byte %01111110
    .byte %01000010
    .byte %01000010


ROM_GAMEMAP0: ; only first four bits are legit
    .byte %00001110
    .byte %00001111
    .byte %00001111
    .byte %00001011
    .byte %11111111
    .byte %11111111
    .byte %11101111
    .byte %11101111
    .byte %11101111
    .byte %01011111
    .byte %01011111
    .byte %10111111


ROM_GAMEMAP1:
    .byte %00000100
    .byte %00001110
    .byte %00001010
    .byte %00001010
    .byte %11111111
    .byte %11111111
    .byte %11110110
    .byte %01110101
    .byte %01110100
    .byte %10101101
    .byte %10101110
    .byte %11011111


ROM_GAMEMAP2:
    .byte %01010101
    .byte %00110111
    .byte %01010101
    .byte %00100010
    .byte %11111111
    .byte %11111111
    .byte %11110100
    .byte %01110111
    .byte %01110100
    .byte %10101111
    .byte %11011100
    .byte %11111111


ROM_GAMEMAP3: ; only 4 first bits are legit
    .byte %11101110
    .byte %11001111
    .byte %11001111
    .byte %11101011
    .byte %11111111
    .byte %11111111
    .byte %01110110
    .byte %01110101
    .byte %01110100
    .byte %10101101
    .byte %11011110
    .byte %11111111

ROM_GAMEMAP4:
    .byte %11101110
    .byte %11011101
    .byte %11011111
    .byte %11111011
    .byte %11111111
    .byte %11111111
    .byte %11000100
    .byte %10111101
    .byte %10001101
    .byte %10111101
    .byte %11000101
    .byte %11111111

ROM_GAMEMAP5:
    .byte %11101110
    .byte %11011101
    .byte %11011111
    .byte %11111011
    .byte %11111111
    .byte %11111111
    .byte %11110000
    .byte %11110111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111



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
