 PROCESSOR 6502
    include "vcs.h"
    include "macro.h"

    ;----------------------------------
    ;           RAM
    SEG.U VARS
    ORG $80

GAMEMAP ds 46 ; 13x7 map, one cell is 4bits
PLAYERY ds 1  ; Player's Y possition
TEMPY   ds 1  ; player posY + height
INPUT   ds 1  ; Input from the joystick
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

    ldx #0

    lda #%01010000
    sta PF0
    lda #%10101010
    sta PF1
    lda #%01010101
    sta PF2

    lda #0
    sta GRP0 ;let's clear the sprite


    ldy #0

loop:
    inx
    stx COLUBK      ;set X as the background color

    jsr DrawPlayer

    sta WSYNC       ;wait for the scanline to be drawn
    cpx #192
    bne loop

    rts
;-----------------------------
DrawPlayer:
    cpx PLAYERY     ;can we draw the player sprite?
    bcs yep         ; >= PLAYERY ?
    jmp nope
yep:
    cpy #8
    bcs hide
    lda DWARF_GFX_0,y
    sta GRP0
    iny
    jmp nope
hide:
    lda #0
    sta GRP0
nope:
    rts

;----------------------------
Overscan:
    sta WSYNC
    lda #2
    sta VBLANK
    lda #35
    sta TIM64T

    ;I might want to place some game logic here

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
    lda PLAYERY
    adc 1
    sta PLAYERY
checkLeft:
    lda INPUT
    asl
    ;sta INPUT
    bcs checkDown
moveLeft:
    lda PLAYERY
    sbc 1
    sta PLAYERY
checkDown:
;    lda INPUT
;    asl
;    sta INPUT
;    bcs checkUp
;moveDown:
;    lda PLAYERY
;    adc 1
;    sta PLAYERY
;checkUp:
;    lda INPUT
;    asl
;    bcs exit
;    lda PLAYERY
;    sbc 1
;    sta PLAYERY
;exit:
    rts
;-------------------------
VBlank:
    jsr ProcessInput
    rts

DWARF_GFX_0:
    .byte %00110000
    .byte %00100000
    .byte %01110000
    .byte %10111000
    .byte %10110100
    .byte %00110000
    .byte %00101000
    .byte %01000100
    .byte %00000000
    


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
