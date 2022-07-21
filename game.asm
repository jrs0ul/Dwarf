 PROCESSOR 6502
    include "vcs.h"
    include "macro.h"


MAPSIZE = 39
PLAYERHEIGHT = 9
LINESPERCELL = 8

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
TEMPY           ds 1  ; player posY + height
INPUT           ds 1  ; Input from the joystick
TEMP_X_INDEX    ds 1
SCREENMAP_IDX   ds 1  ; index of the screenmap
LINE_IDX        ds 1  ; line counter for a map cell
;------------------------------------------------------
;                  118 | 10 free
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
    lda #100
    sta PLAYERY
    lda #10
    sta PLAYERX

    
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


    ldx #96 ; scanlines, max scanlines / 2

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

    lda DWARF_GFX_0,y
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
    sta PLAYERX
checkLeft:
    lda INPUT
    asl
    sta INPUT
    bcs checkDown
moveLeft:
    lda PLAYERX
    sbc 1
    sta PLAYERX

checkDown:
    lda INPUT
    asl
    sta INPUT
    bcs checkUp
moveDown:
    lda PLAYERY
    sbc 1
    sta PLAYERY
checkUp:
    lda INPUT
    asl
    bcs exit
    lda PLAYERY
    adc 1
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
;------------------------
FillScreenMap:

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
    inx
    lda #%00010001
    sta THEMAP,x
    cpx #MAPSIZE
    bne @GenLoop

    lda #%00000000
    sta THEMAP,0
    sta THEMAP,1
    sta THEMAP,2

    rts


;-------------------------
VBlank:
    lda PLAYERX
    ldx #0
    jsr PosSpriteX
    sta WSYNC
    sta HMOVE
    jsr FillScreenMap
    rts

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
