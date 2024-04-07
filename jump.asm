; Copyright (c) 2024 Adrian Pilkington

; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:

; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

;;; Platform game Jump for zx81 
;;; using charactor based sprites (LARGE!)
;;;
;;; https://youtube.com/@byteforever7829

;some #defines for compatibility with other assemblers
#define         DEFB .byte 
#define         DEFW .word
#define         EQU  .equ
#define         ORG  .org

;;;;;#define DEBUG_NO_SCROLL

; keyboard port for shift key to v
#define KEYBOARD_READ_PORT_SHIFT_TO_V $FE
; keyboard space to b
#define KEYBOARD_READ_PORT_SPACE_TO_B $7F 
; starting port numbner for keyboard, is same as first port for shift to v
#define KEYBOARD_READ_PORT $FE 
#define SCREEN_WIDTH 32
#define SCREEN_HEIGHT 23   ; we can use the full screen becuase we're not using PRINT or PRINT AT ROM subroutines

#define NO_JUMP 0


VSYNCLOOP       EQU      5

; character set definition/helpers
__:				EQU	$00	;spacja
_QT:			EQU	$0B	;"
_PD:			EQU	$0C	;funt 
_SD:			EQU	$0D	;$
_CL:			EQU	$0E	;:
_QM:			EQU	$0F	;?
_OP:			EQU	$10	;(
_CP:			EQU	$11	;)
_GT:			EQU	$12	;>
_LT:			EQU	$13	;<
_EQ:			EQU	$14	;=
_PL:			EQU	$15	;+
_MI:			EQU	$16	;-
_AS:			EQU	$17	;*
_SL:			EQU	$18	;/
_SC:			EQU	$19	;;
_CM:			EQU	$1A	;,
_DT:			EQU	$1B	;.
_NL:			EQU	$76	;NEWLINE

_BL             EQU $80; solid block

_0				EQU $1C
_1				EQU $1D
_2				EQU $1E
_3				EQU $1F
_4				EQU $20
_5				EQU $21
_6				EQU $22
_7				EQU $23
_8				EQU $24
_9				EQU $25
_A				EQU $26
_B				EQU $27
_C				EQU $28
_D				EQU $29
_E				EQU $2A
_F				EQU $2B
_G				EQU $2C
_H				EQU $2D
_I				EQU $2E
_J				EQU $2F
_K				EQU $30
_L				EQU $31
_M				EQU $32
_N				EQU $33
_O				EQU $34
_P				EQU $35
_Q				EQU $36
_R				EQU $37
_S				EQU $38
_T				EQU $39
_U				EQU $3A
_V				EQU $3B
_W				EQU $3C
_X				EQU $3D
_Y				EQU $3E
_Z				EQU $3F


;;;; this is the whole ZX81 runtime system and gets assembled and 
;;;; loads as it would if we just powered/booted into basic

           ORG  $4009             ; assemble to this address
                                                                
VERSN:          DEFB 0
E_PPC:          DEFW 2
D_FILE:         DEFW Display
DF_CC:          DEFW Display+1                  ; First character of display
VARS:           DEFW Variables
DEST:           DEFW 0
E_LINE:         DEFW BasicEnd 
CH_ADD:         DEFW BasicEnd+4                 ; Simulate SAVE "X"
X_PTR:          DEFW 0
STKBOT:         DEFW BasicEnd+5
STKEND:         DEFW BasicEnd+5                 ; Empty stack
BREG:           DEFB 0
MEM:            DEFW MEMBOT
UNUSED1:        DEFB 0
DF_SZ:          DEFB 2
S_TOP:          DEFW $0002                      ; Top program line number
LAST_K:         DEFW $fdbf
DEBOUN:         DEFB 15
MARGIN:         DEFB 55
NXTLIN:         DEFW Line2                      ; Next line address
OLDPPC:         DEFW 0
FLAGX:          DEFB 0
STRLEN:         DEFW 0
T_ADDR:         DEFW $0c8d
SEED:           DEFW 0
FRAMES:         DEFW $f5a3
COORDS:         DEFW 0
PR_CC:          DEFB $bc
S_POSN:         DEFW $1821
CDFLAG:         DEFB $40
MEMBOT:         DEFB 0,0 ;  zeros
UNUNSED2:       DEFW 0

            ORG 16509       ;; we have to push the place in memory for this here becuase basic has 
                    ;; to start at 16514 if memory was tight we could use the space between UNUSED2
                    ;; and Line1 for variables

Line1:          DEFB $00,$0a                    ; Line 10
                DEFW Line1End-Line1Text         ; Line 10 length
Line1Text:      DEFB $ea                        ; REM

    

preinit
;; initialise variables that are once per game load/start

initVariables
;; initialise variables per game
    xor a
    
    ld (vertAcceleration), a
    ld de, 500    
    ld hl, Display+1 
    add hl, de
    ld (currentPlayerLocation), hl
    ld hl, playerSpriteRightMove
    ld (playerSpritePointer), hl
    ld a, 5
    ld (playerXPos), a
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
gameLoop    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ld b,VSYNCLOOP
waitForTVSync	
	call vsync
	djnz waitForTVSync
    
    ld hl, (playerSpritePointer)
    ;ld hl, testSprite
    ld de, (currentPlayerLocation)
    ;ld de, Display+1+37
    ld c, 8
    ld b, 8    
    call drawSprite    
    
    
    ; ld de, 8
    ; ld bc, Display+1+37
    ; call print_number16bits    
    ; ld de, 14
    ; ld bc, testSprite
    ; call print_number16bits        
    ; jp gameLoop
    
    ;; read keys
    ld a, KEYBOARD_READ_PORT_SHIFT_TO_V			
    in a, (KEYBOARD_READ_PORT)					; read from io port	
    bit 1, a                            ; Z
    jp z, moveLeft


    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 3, a					        ; N
    jp z, moveRight
    
    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 2, a						    ; M
    jp z, doJump
     
    jp updateRestOfScreen                       ; if no key pressed continue

moveLeft         
    ld a, (playerXPos)
    dec a
    cp 1
    jp z, updateRestOfScreen   
    ld (playerXPos), a
    
    ld hl, (currentPlayerLocation)
    dec hl
    ld (currentPlayerLocation), hl  

    ld a, (spriteFrameCycle)
    inc a
    cp 3
    jp z, spriteNextLeft     
    
    ld (spriteFrameCycle),a    
    ld hl, playerSpriteLeftMove
    ld de,64     ; 8 by 8 blocks
    add hl, de
    ld (playerSpritePointer), hl    
    jp updateRestOfScreen 
spriteNextLeft    
    ld hl, playerSpriteLeftMove
    ld (playerSpritePointer), hl    
    xor a
    ld (spriteFrameCycle), a
    jp updateRestOfScreen 
    
moveRight       
    ld a, (playerXPos)
    inc a
    cp 24
    jp z, updateRestOfScreen   
    ld (playerXPos), a
    
    
    ld hl, (currentPlayerLocation)
    inc hl
    ld (currentPlayerLocation), hl  
    
    ld a, (spriteFrameCycle)
    inc a
    cp 3
    jp z, spriteNextRight     
    
    ld (spriteFrameCycle),a    
    ld hl, playerSpriteRightMove
    ld de,64
    add hl, de
    ld (playerSpritePointer), hl    
    jp updateRestOfScreen 
spriteNextRight    
    ld hl, playerSpriteRightMove
    ld (playerSpritePointer), hl    
    xor a
    ld (spriteFrameCycle), a
    jp updateRestOfScreen 
    
doJump   
    ld a, (vertAcceleration)
    add a, 3
    ld (vertAcceleration), a 
    
    jp updateRestOfScreen 
    
updateRestOfScreen   
    ; update vertical position of sprite 
    ld a, (vertAcceleration)
    ; if vertAcceleration > 0 
    cp 1
    jp nz, checkPositionAboveBottom
    ;; else move player vertically and decrement  vertAcceleration
    ld de, (currentPlayerLocation)
    push de
    pop hl
    ld de, -33     ; move player up
    add hl, de
    push hl
    pop de
    ld (currentPlayerLocation), de      
    
    ld a, (vertAcceleration)
    dec a
    ld (vertAcceleration),a     

checkPositionAboveBottom

    ld de, 8
    ld bc, $bad0
    call print_number16bits    
    
    ld de, $1cf      
    ld hl, (currentPlayerLocation)
    sbc hl, de
    jp nc, checkVertLessZero    ; check result is > 0 
    jp skipMove

checkVertLessZero
    ld a, (vertAcceleration)
    cp 1
    jp nz, skipMove
    ; move player down
    ld de, (currentPlayerLocation)
    push de
    pop hl
    ld de, 33     ; move player down
    add hl, de
    push hl
    pop de
    ld (currentPlayerLocation), de        
      
skipMove       
    ld de, 14
    ld bc, (currentPlayerLocation)
    call print_number16bits    
    jp gameLoop


 
playerWon    



;;;; sprite code
;;;; our sprites are custom 8 by 8 charactor blocks - so will look fairly big (maybe too big)
;;;; the generic routines will look at an area of memory stored in hl before the call

;;;; on the zx81 each block is 2 "pixels" horizontally and 2 vertically pre encoded in the sprite memory
;;;; size of sprite in memory using bit pack is 16 * 16 = 256bits ==>>> 32bytes

;;; hl = start of sprite memory
;;; de = offset position in screen memory top left of sprite - no limit check done (yet)
;;; c  = width of sprite (normally 8 to keep things "simple")
;;; b  = rows in sprite (normally 8 to keep things "simple")
drawSprite         
    push bc    
    push de
    ld b, 0               ;; just doing columns in c so zero b
    ldir                  ;; ldir repeats ld (de), (hl) until bc = 0
    pop de
    ex de, hl
    ld bc, 33             ;; move next write position to next row
    add hl, bc
    ex de, hl
    pop bc
    djnz drawSprite    
    ret
     
        
; this prints at to any offset (stored in bc) from the top of the screen Display, using string in de
printstring
    push de ; preserve de
    ld hl,Display
    add hl,bc	
printstring_loop
    ld a,(de)
    cp $ff
    jp z,printstring_end
    ld (hl),a
    inc hl
    inc de
    jr printstring_loop
printstring_end	
    pop de  ; preserve de
    ret  
    
print_number16bits    ; bc stores the 16bits, print b then c, de stores offset from Display
    ld a, b
    call print_number8bits
    ld a, c
    inc de  ; move de over by 2
    inc de
    call print_number8bits
    ret

    
print_number8bits
    ld hl, (DF_CC)    
    add hl, de    
    push af ;store the original value of a for later
    and $f0 ; isolate the first digit
    rra
    rra
    rra
    rra
    add a,$1c ; add 28 to the character code
    ld (hl), a
    inc hl
    pop af ; retrieve original value of a
    and $0f ; isolate the second digit
    add a,$1c ; add 28 to the character code
    ld (hl), a  
    
    ret

;check if TV synchro (FRAMES) happend
vsync	
	ld a,(FRAMES)
	ld c,a
sync
	ld a,(FRAMES)
	cp c
	jr z,sync
	ret

    
                DEFB $76                        ; Newline        
Line1End
Line2			DEFB $00,$14
                DEFW Line2End-Line2Text
Line2Text     	DEFB $F9,$D4                    ; RAND USR
				DEFB $1D,$22,$21,$1D,$20        ; 16514                
                DEFB $7E                        ; Number
                DEFB $8F,$01,$04,$00,$00        ; Numeric encoding
                DEFB $76                        ; Newline
Line2End            
endBasic
                                                                
Display        	DEFB $76                                                 
				DEFB _J,_U,_M,_P, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,_B,_Y, 0,_A,_D,_R,_I,_A,_N, 0,_P,$76  ;Line0
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76  ;Line1              
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76  ;Line2
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76  ;Line3
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76  ;Line4
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76  ;Line5
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76  ;Line6
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76  ;Line7
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76  ;Line8
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76  ;Line9
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76  ;Line10
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76  ;Line11
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76  ;Line12
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76  ;Line13
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76  ;Line14
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76  ;Line15
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76  ;Line16
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76  ;Line17
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76  ;Line18
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76  ;Line19
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76  ;Line20
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76  ;Line21
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76  ;Line22
                DEFB  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,$76  ;Line23

                                                               
Variables:   
vertAcceleration   DEFB 0
currentPlayerLocation DEFW 0

;; 336 bytes of sprite data packed, each sprite 8*8 characters and 3 frames right then left
playerSpriteRightMove                
  DEFB	    $00, $00, $87, $80, $82, $00, $00, $00, $00, $00, $85, $80,
  DEFB      $81, $04, $00, $00, $00, $00, $02, $80, $07, $00, $00, $00,
  DEFB	    $00, $00, $87, $80, $04, $00, $00, $00, $00, $00, $86, $80,
  DEFB	    $02, $04, $00, $00, $00, $00, $87, $03, $04, $00, $00, $00,
  DEFB	    $00, $00, $85, $00, $86, $00, $00, $00, $00, $00, $85, $04,
  DEFB		$02, $01, $00, $00, $00, $00, $00, $87, $80, $82, $00, $00,
  DEFB  	$00, $00, $00, $85, $80, $81, $04, $00, $00, $00, $00, $02,
  DEFB		$80, $07, $00, $00, $00, $00, $00, $87, $80, $04, $00, $00,
  DEFB		$00, $00, $87, $01, $80, $85, $00, $00, $00, $00, $00, $00,
  DEFB		$07, $06, $00, $00, $00, $00, $00, $06, $00, $05, $00, $00,
  DEFB		$00, $00, $00, $86, $00, $82, $00, $00, $00, $00, $87, $80,
  DEFB  	$82, $00, $00, $00, $00, $00, $85, $80, $81, $04, $00, $00,
  DEFB		$00, $00, $02, $80, $07, $00, $00, $00, $00, $00, $87, $80,
  DEFB		$04, $00, $00, $00, $00, $00, $05, $80, $85, $00, $00, $00,
  DEFB		$00, $00, $86, $03, $06, $00, $00, $00, $00, $00, $85, $00, 
  DEFB		$05, $00, $00, $00, $00, $00, $81, $00, $82, $00, $00, $00,
playerSpriteLeftMove    
  DEFB  	$00, $00, $81, $80, $04, $00, $00, $00, $00, $87, $82, $80,
  DEFB		$05, $00, $00, $00, $00, $00, $84, $80, $01, $00, $00, $00,
  DEFB		$00, $00, $87, $80, $04, $00, $00, $00, $00, $00, $05, $80,
  DEFB		$85, $00, $00, $00, $00, $00, $86, $03, $06, $00, $00, $00,
  DEFB		$00, $00, $85, $00, $05, $00, $00, $00, $00, $00, $81, $00,
  DEFB  	$82, $00, $00, $00, $00, $00, $00, $81, $80, $04, $00, $00,
  DEFB		$00, $00, $87, $82, $80, $05, $00, $00, $00, $00, $00, $84,
  DEFB		$80, $01, $00, $00, $00, $00, $00, $87, $80, $04, $00, $00,
  DEFB		$00, $00, $87, $01, $80, $06, $00, $00, $00, $00, $00, $87,
  DEFB		$03, $04, $00, $00, $00, $00, $00, $06, $00, $05, $00, $00,
  DEFB  	$00, $00, $02, $01, $87, $05, $00, $00, $00, $00, $81, $80,
  DEFB		$04, $00, $00, $00, $00, $87, $82, $80, $05, $00, $00, $00,
  DEFB		$00, $00, $84, $80, $01, $00, $00, $00, $00, $00, $87, $80,
  DEFB		$04, $00, $00, $00, $00, $00, $05, $80, $02, $04, $00, $00,
  DEFB	    $00, $00, $86, $84, $00, $00, $00, $00, $00, $00, $85, $00,
  DEFB  	$86, $00, $00, $00, $00, $00, $81, $00, $06, $00, $00, $00

    
   
    
testSprite
                DEFB   0,  0,  0,  0,  0,  0,  0,  0
                DEFB   0,  7,  3,  3,  3,  3,132,  0
                DEFB   0,  5,  8,  0,  0,  6,133,  0
                DEFB   0,  5,  0,  8,  6,  0,133,  0
                DEFB   0,  5,  0,  6,  8,  0,133,  0
                DEFB   0,  5,  6,  0,  0,  8,133,  0
                DEFB   0,130,131,131,131,131,129,  0
                DEFB   0,  0,  0,  0,  0,  0,  0,  0
spriteFrameCycle
    DEFB 0
playerSpritePointer
    DEFW 0 
playerXPos    
    DEFB 0   
VariablesEnd:   DEFB $80
BasicEnd: 
#END
