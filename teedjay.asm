;==============================================================
; WLA-DX bank and slot setup
; BIOS starts at $0000
; ROM starts at $4000 in a real MSX (or $8000)
; RAM should be available at $C000 (MSX has at least 16kb)
; VRAM has 16kb and is accessible thru I/O ports only
;
; wla-z80 teedjay.asm
; wlalink teedjay.link teedjay.rom
; open teedjay.rom
;============================================================== 
.memorymap 
    defaultslot 0 
    slotsize $4000 
    slot 0 $4000
.endme

.rombankmap 
    bankstotal 1 
    banksize $4000 
    banks 1 
.endro

.bank 0 slot 0 

.DEFINE DATAP $98 ; VDP Data port $98 works on all MSX models (TMS9918/TMS9929/V9938 or V9958) 
.DEFINE CMDP $99  ; VDP Command port $99 works on all MSX models (TMS9918/TMS9929/V9938 or V9958) 
.DEFINE HKEYI $FD9A ; the H.KEYI interrupt hook from the bios
.DEFINE HTIMI $FD9F ; the VDP interrupt hook (called from HKEYI routine)

; MSX 16 byte header
; +0	ID	Put these first two bytes at 041H and 042H ("AB") to indicate that it is an additional ROM.
; +2	INIT	Address of the routine to call to initialize a work area or I/O ports, or run a game, etc.
; +4	STATEMENT	Runtime address of a program whose purpose is to add instructions to the MSX-Basic using CALL.
; +6	DEVICE	Execution address of a program used to control a device built into the cartridge. For example, a disk interface.
; +8	TEXT	Pointer of the tokenizen Basic program contained in ROM.
; +10	Reserved	6 bytes reserved for future updates.

MSX_ROM:
    .db "AB"
    .dw START
    .dw 0
    .dw 0
    .dw 0
    .db 0,0,0,0,0,0

; VDP info https://github.com/cbmeeks/TMS9918/blob/master/tms9918a.txt

START:
    di
    call CLEAR_VDP
    call SETUP_REGISTERS
    call WRITE_HELLO_WORLD
    call WRITE_COLOR_ATTRIBUTES
    call DEFINE_SPRITES
    call PRINT_SPRITES
    ;call SETUP_HTIMI
    call SETUP_HKEYI
    im 1
    ei
    ld b,0

; update background and foreground colors from memory address
LOOP:
    ld a,($E000)
    rla
    rla
    rla
    rla
    di
    out (CMDP),a
    ld a,$87 
    out (CMDP),a
    nop
    nop
    ei
    jr LOOP
    
VBLANK:
    push af
    in a, (CMDP)
    bit 7, a
    jr z, NO_VBLANK
    ; only update if VBLANK flag was active
    ld a,($E000)
    inc a
    ld ($E000),a
NO_VBLANK:
    pop af
    ret

SETUP_HKEYI:
    ; The most used interrupt mode on MSX is IM 1. 
    ; In this mode the processor always calls a fixed address: 0038h. 
    ; At this address every MSX BIOS has a jump to the interrupt routine (hook HKEYI).
    ld a, $C3       ; JP xxxx
    ld (HKEYI), a
    ld hl, VBLANK
    ld (HKEYI + 1), hl
    ret

SETUP_HTIMI:
    ; The HKEYI hook jumps to a MSX BIOS function that checks VDP for VBLANK interrupt.
    ; If interrupt is triggered by VBLANK it calls other interrupt routine (hook HTIMI).
    ld a, $C3       ; JP xxxx
    ld (HTIMI), a
    ld hl, VBLANK
    ld (HTIMI + 1), hl
    ret

;============================================================== 
; This "Hello World" example was adapted from the "famous" Z80
; and TMS9918 / TMS9928 / TMS9929 / V9938 or V9958 VDP code.
; https://www.msx.org/forum/development/msx-development/nyyrikkis-helloworld-code
; This should work on SVI, MSX, Colecovision, Memotech and many 
; other Z80 based home computers or game consoles.
;============================================================== 

CLEAR_VDP:
    ; Let's set VDP write address to $0000 
    XOR A 
    OUT (CMDP),A 
    LD A,$40 
    OUT (CMDP),A 
    ; Now let's clear first 16Kb of VDP memory 
    LD B,$00 
    LD HL,$3FFF 
    LD C,DATAP
CLEAR: 
    OUT (C),B 
    DEC HL 
    LD A,H 
    OR L 
    NOP ; Let's wait 8 clock cycles just in case VDP is not quick enough. 
    NOP 
    JR NZ,CLEAR
    RET

SETUP_REGISTERS:
    LD C,CMDP 

    ;---------------------------------------- 
    ; Register 0 to $0 
    ; 
    ; Set mode selection bit M3-M5 to zero and 
    ; disable external video & lightpen / horizontal interrupt 
    ;
    LD A,$00
    LD E,$80 
    OUT (C),A 
    OUT (C),E 

    ;---------------------------------------- 
    ; Register 1
    ; 
    ; Select 32 column mode, Graphic 1 M1-M2 = 0
    ; enable screen and enable vertical interrupt 
    ; 16x16 sprites and magnified 2x
    ;
    LD A,$E3
    LD E,$81
    OUT (C),A 
    OUT (C),E 

    ;---------------------------------------- 
    ; Register 2 to $0 (00BB BB00 0000 0000)
    ; 
    ; Set pattern name table to $0000 
    ;
    LD A,$00
    LD E,$82 
    OUT (C),A 
    OUT (C),E 

    ;---------------------------------------- 
    ; Register 3 set color table to $0400 (00BB BBBB BB00 0000 ))
    ; 
    LD A,$10    ; $10 * $40 = $0400
    LD E,$83
    OUT (C),A 
    OUT (C),E 
    
    ;---------------------------------------- 
    ; Register 4 to $1 (00BB B000 0000 0000 )
    ; Set pattern generator table to $0800 
    ;
    LD A,$01    ; $01 * $800 = $0800
    LD E,$84
    OUT (C),A 
    OUT (C),E

    ;---------------------------------------- 
    ; Registers 5 (Sprite attribute) to $1000 (00BB BBBB B000 0000)
    ;
    LD A,$20    ; $20 * $80 = $1000 base address for sprite attributes
    LD E,$85
    OUT (C),A 
    OUT (C),E

    ;---------------------------------------- 
    ; Registers 6 (Sprite pattern) to $2000 (00BB B000 0000 0000)
    ;
    LD A,$04    ; $04 * $800 = $2000 base address for sprite patterns
    LD E,$86
    OUT (C),A 
    OUT (C),E

    ;---------------------------------------- 
    ; Register 7 to $F0 
    ; Set colors to black 
    ;
    LD A,$F4
    LD E,$87
    OUT (C),A 
    OUT (C),E 

    RET

WRITE_HELLO_WORLD:
    LD C,CMDP 
    ;---------------------------------------- 
    ; Let's set VDP write address to $808 so, that we can write 
    ; character patterns to memory 
    ; (No need to write SPACE it is clear char already) 
    ;
    LD A,$08 
    OUT (C),A 
    LD A,$48 
    OUT (C),A 
    ; Let's copy character set 
    LD HL, CHARS 
    LD B, CHARS_END-CHARS 
COPYCHARS: 
    LD A,(HL) 
    OUT (DATAP),A 
    INC HL 
    NOP ; Let's wait 8 clock cycles just in case VDP is not quick enough. 
    NOP 
    DJNZ COPYCHARS 

    ; Let's set write address to start of tiles table $0000
    XOR A 
    OUT (C),A 
    LD A,$40 
    OUT (C),A 
    ; Let's put characters to screen 
    LD HL,ORDER 
    LD B,ORDER_END-ORDER 
COPYORDER: 
    LD A,(HL) 
    OUT (DATAP),A 
    INC HL 
    NOP ; Let's wait 8 clock cycles just in case VDP is not quick enough. 
    NOP 
    DJNZ COPYORDER
    RET

WRITE_COLOR_ATTRIBUTES:
    LD C,CMDP 
    ; Let's set write address to color attribute to $0400
    LD A,$00
    OUT (C),A 
    LD A,$44
    OUT (C),A 
    LD HL,SPRITES
    LD B,SPRITES_END-SPRITES
COPYCOLORS: 
    LD A,(HL)
    OUT (DATAP),A
    INC HL
    NOP ; Let's wait 8 clock cycles just in case VDP is not quick enough. 
    NOP 
    DJNZ COPYCOLORS 
    RET

DEFINE_SPRITES:
    LD C,CMDP 
    LD A,$00
    OUT (C),A 
    LD A,$60         ; bit 14 + $2000
    OUT (C),A 
    LD HL,SPRITES 
    LD B,SPRITES_END-SPRITES
COPY_SPRITE_PATTERNS: 
    LD A,(HL) 
    OUT (DATAP),A 
    INC HL 
    NOP ; Let's wait 8 clock cycles just in case VDP is not quick enough. 
    NOP 
    DJNZ COPY_SPRITE_PATTERNS 
    RET

PRINT_SPRITES:
    LD C,CMDP 
    LD A,$00
    OUT (C),A 
    LD A,$50         ; bit 14 + $1000
    OUT (C),A 
    LD HL, SPRITE_ATTRIBUTES 
    LD B, SPRITE_ATTRIBUTES_END-SPRITE_ATTRIBUTES
COPY_SPRITE_ATTRIBUTES: 
    LD A,(HL) 
    OUT (DATAP),A 
    INC HL 
    NOP ; Let's wait 8 clock cycles just in case VDP is not quick enough. 
    NOP 
    DJNZ COPY_SPRITE_ATTRIBUTES
    RET

    ; Character set: 
    ; -------------- 
ORDER: 
    .db 1,2,3,3,4,0,5,4,6,3,7 
ORDER_END: 

CHARS: 
    ; H 
    .db %10001000 
    .db %10001000
    .db %10001000 
    .db %11111000 
    .db %10001000 
    .db %10001000 
    .db %10001000 
    .db %00000000 
    ; e 
    .db %00000000 
    .db %00000000 
    .db %01110000 
    .db %10001000 
    .db %11111000 
    .db %10000000 
    .db %01110000 
    .db %00000000 
    ; l 
    .db %01100000 
    .db %00100000 
    .db %00100000 
    .db %00100000 
    .db %00100000 
    .db %00100000 
    .db %01110000 
    .db %00000000 
    ; o 
    .db %00000000 
    .db %00000000 
    .db %01110000 
    .db %10001000 
    .db %10001000 
    .db %10001000 
    .db %01110000 
    .db %00000000 
    ; W 
    .db %10001000 
    .db %10001000 
    .db %10001000 
    .db %10101000 
    .db %10101000 
    .db %11011000 
    .db %10001000 
    .db %00000000 
    ; r 
    .db %00000000 
    .db %00000000 
    .db %10110000 
    .db %11001000 
    .db %10000000 
    .db %10000000 
    .db %10000000 
    .db %00000000 
    ; d 
    .db %00001000 
    .db %00001000 
    .db %01101000 
    .db %10011000 
    .db %10001000 
    .db %10011000 
    .db %01101000 
    .db %00000000 
CHARS_END:

SPRITE_ATTRIBUTES:

    // 4 first sprite patterns are same 32x32
    .DB $10,$10,$00,$02     ; y,x,pattern,color
    .DB $32,$10,$01,$03     ; y,x,pattern,color
    .DB $10,$32,$02,$04     ; y,x,pattern,color
    .DB $32,$32,$03,$05     ; y,x,pattern,color

    // 4 next sprite patterns are same 32x32
    .DB $70,$10,$04,$06     ; y,x,pattern,color
    .DB $92,$10,$05,$07     ; y,x,pattern,color
    .DB $70,$32,$06,$08     ; y,x,pattern,color
    .DB $92,$32,$07,$09     ; y,x,pattern,color

    // 4 next sprite patterns are same 32x32
    .DB $10,$80,$08,$0a     ; y,x,pattern,color
    .DB $32,$80,$09,$0b     ; y,x,pattern,color
    .DB $10,$a2,$0a,$0c     ; y,x,pattern,color
    .DB $32,$a2,$0b,$0d     ; y,x,pattern,color

    // 4 next sprite patterns are same 32x32
    .DB $70,$80,$0c,$0e     ; y,x,pattern,color
    .DB $92,$80,$0d,$0f     ; y,x,pattern,color
    .DB $70,$a2,$0e,$02     ; y,x,pattern,color
    .DB $92,$a2,$0f,$03     ; y,x,pattern,color

SPRITE_ATTRIBUTES_END:

SPRITES:
; --- Slot 0 - GHOST V0
    .DB $01,$07,$1C,$38,$70,$60,$E0,$C6
    .DB $CE,$C4,$C0,$CF,$D8,$C0,$DF,$F3
    .DB $E0,$E0,$70,$18,$1C,$1C,$0E,$66
    .DB $76,$26,$07,$E7,$33,$03,$FF,$33
; 
; --- Slot 1 - GHOST H0
    .DB $07,$07,$0E,$18,$38,$38,$70,$66
    .DB $6E,$64,$E0,$E7,$CC,$C0,$FF,$CC
    .DB $80,$E0,$38,$1C,$0E,$06,$07,$63
    .DB $73,$23,$03,$F3,$1B,$03,$FB,$CF
; 
; --- Slot 2 - GHOST V1
    .DB $01,$07,$1C,$38,$70,$60,$E4,$CE
    .DB $C4,$C0,$C0,$CF,$C0,$C0,$DF,$F3
    .DB $E0,$E0,$70,$18,$1C,$1C,$4E,$E6
    .DB $46,$06,$07,$F7,$03,$03,$FF,$33
; 
; --- Slot 3 - GHOST H1
    .DB $07,$07,$0E,$18,$38,$38,$72,$67
    .DB $62,$60,$E0,$EF,$C0,$C0,$FF,$CC
    .DB $80,$E0,$38,$1C,$0E,$06,$27,$73
    .DB $23,$03,$03,$F3,$03,$03,$FB,$CF
SPRITES_END:
