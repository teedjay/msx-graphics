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
    call WRITE_HELLO_WORLD
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
WRITE_HELLO_WORLD:
    ; Let's set VDP write address to $0000 
    XOR A 
    OUT (CMDP),A 
    LD A,$40 
    OUT (CMDP),A 

    ; Now let's clear first 16Kb of VDP memory 
    LD B,0 
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

    ; Now it is time to set up VDP registers: 
    LD C,CMDP 

    ;---------------------------------------- 
    ; Register 0 to $0 
    ; 
    ; Set mode selection bit M3 (maybe also M4 & M5) to zero and 
    ; disable external video & horizontal interrupt 
    ;
    LD A,$00
    LD E,$80 
    OUT (C),A 
    OUT (C),E 

    ;---------------------------------------- 
    ; Register 1 to $70 
    ; 
    ; Select 40 column mode, enable screen and enable vertical interrupt 
    ;
    LD A,$70 
    LD E,$81
    OUT (C),A 
    OUT (C),E 

    ;---------------------------------------- 
    ; Register 2 to $0 
    ; 
    ; Set pattern name table to $0000 
    ;
    LD A,$00
    LD E,$82 
    OUT (C),A 
    OUT (C),E 

    ;---------------------------------------- 
    ; Register 3 is ignored as 40 column mode does not need color table 
    ; 
    
    ;---------------------------------------- 
    ; Register 4 to $1 
    ; Set pattern generator table to $800 
    ;
    LD A,$01 
    LD E,$84
    OUT (C),A 
    OUT (C),E

    ;---------------------------------------- 
    ; Registers 5 (Sprite attribute) is ignored as 40 column mode does not have sprites 
    ;

    ;---------------------------------------- 
    ; Registers 6 (Sprite pattern) is ignored as 40 column mode does not have sprites 
    ;

    ;---------------------------------------- 
    ; Register 7 to $F0 
    ; Set colors to white on black 
    ;
    LD A,$F0 
    LD E,$87
    OUT (C),A 
    OUT (C),E 

    ; allow VDP registers to settle (probably not needed)
    NOP
    NOP

    ;---------------------------------------- 
    ; Let's set VDP write address to $808 so, that we can write 
    ; character set to memory 
    ; (No need to write SPACE it is clear char already) 
    ;
    LD A,8 
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

    ; Let's set write address to start of name table 
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
    DJNZ COPYORDER 

    ; The end 
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