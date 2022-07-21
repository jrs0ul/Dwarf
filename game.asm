 PROCESSOR 6502
    include "vcs.h"
    include "macro.h"


MAPSIZE = 46
PLAYERHEIGHT = 9

    ;----------------------------------
    ;           RAM
    SEG.U VARS
    ORG $80

;GAMEMAP ds MAPSIZE ; 13x7 map, one cell is 4bits
PLAYERY ds 1  ; Player's Y position
PLAYERX ds 1  ; Player's X position
TEMPY   ds 1  ; player posY + height
INPUT   ds 1  ; Input from the joystick
tempYindex ds 1
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

    ldx #96 ; scanlines, max scanlines / 2

    lda #%00000000
    sta PF0
    sta PF1
    sta PF2

    lda #0
    sta GRP0 ;let's clear the sprite


    lda #$1C
    sta COLUBK      ;set X as the background color
    ldy #PLAYERHEIGHT

loop:
    ;---------------DRAW MAP----------------------
    txa                 ;2 2
    lsr                 ;2 4
    bcs @ex             ;2 6
    lsr                 ;2 8
    bcs @ex             ;2 10
    lsr                 ;2 12
    bcs @ex             ;2 14

    sty tempYindex      ;3 17   save Y
    tay                 ;2 19


    lda GAMEMAP1,y      ;4 23
    sta PF1             ;3 26

    lda GAMEMAP3,y      ;4 30
    sta PF0             ;3 33

    lda GAMEMAP2,y      ;4 37
    sta PF2             ;3 40

    lda GAMEMAP5,y      ;4 44
    sta PF1             ;3 47
    
    nop                 ;2 49

    lda GAMEMAP4,y      ;4 54
    sta PF2             ;3 57


    lda GAMEMAP0,y      ;4 61
    sta PF0             ;3 64

    ldy tempYindex      ;3 67  load Y back
@ex:
    ;---------------------------------------------
    sta WSYNC       ;finish the scanline, we don't want to cram sprite drawing instructions to be here
    ;---------------------------------------------
    jsr DrawPlayer
    sta WSYNC       ;wait for the scanline to be drawn
    dex             ;
    bne loop        ;

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
GenerateMap:
;    ldx #0
;    lda #%11101010
;@maploop:
;    sta GAMEMAP,x
;    inx
;    ;rol
;    cpx #MAPSIZE
;    bne @maploop

    
    rts

;-------------------------
VBlank:
    lda PLAYERX
    ldx #0
    jsr PosSpriteX
    sta WSYNC
    sta HMOVE
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

GAMEMAP0:
    .byte %00000000
    .byte %11101110
    .byte %11011111
    .byte %11011111
    .byte %11111011
    .byte %11111111
    .byte %11111111
    .byte %11101111
    .byte %11101111
    .byte %11101111
    .byte %01011111
    .byte %01011111
    .byte %10111111


GAMEMAP1:
    .byte %01010101
    .byte %11101110
    .byte %11011111
    .byte %11011111
    .byte %11111011
    .byte %11111111
    .byte %11111111
    .byte %11110110
    .byte %01110101
    .byte %01110100
    .byte %10101101
    .byte %10101110
    .byte %11011111


GAMEMAP2:
    .byte %00000000
    .byte %11101110
    .byte %11011111
    .byte %11011111
    .byte %11111011
    .byte %11111111
    .byte %11111111
    .byte %11110100
    .byte %01110111
    .byte %01110100
    .byte %10101111
    .byte %10101100
    .byte %11011111


GAMEMAP3:
    .byte %00000000
    .byte %11101110
    .byte %11011111
    .byte %11011111
    .byte %11111011
    .byte %11111111
    .byte %11111111
    .byte %11110100
    .byte %10101010
    .byte %01010101
    .byte %10101010
    .byte %01010101
    .byte %10101010

GAMEMAP4:
    .byte %00000000
    .byte %11101110
    .byte %11011111
    .byte %11011111
    .byte %11111011
    .byte %11111111
    .byte %11111111
    .byte %11110100
    .byte %10101010
    .byte %01010101
    .byte %10101010
    .byte %01010101
    .byte %10101010

GAMEMAP5:
    .byte %00000000
    .byte %11101110
    .byte %11011111
    .byte %11011111
    .byte %11111011
    .byte %11111111
    .byte %11111111
    .byte %11110100
    .byte %10101010
    .byte %01010101
    .byte %10101010
    .byte %01010101







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
