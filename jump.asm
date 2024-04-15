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

;;; Known bugs
;;    1) a copy of players feet get left behind after a jump
;;    2) if the gold is landed on from top the score doesn't increase and stuck in room

;;; todo list 
;;    1) add enemy sprites to room config
;;    2) add moveable enemy spites
;;    3) rewrite enemy sprite draw to make it a smaller proper subroutines
;;    4) design more rooms
;;    5) add verical bariiers to rooms
;;    6) add player lives, and killed if hits enemy
;;    7) add disapearing platforms and to room config
;;    8) add more platforms
;;    9) optimise the player check collision with gold to use only outer blocks

;some #defines for compatibility with other assemblers
#define         DEFB .byte 
#define         DEFW .word
#define         EQU  .equ
#define         ORG  .org
CLS				EQU $0A2A
;;;;;#define DEBUG_NO_SCROLL


#define KEYBOARD_READ_PORT_P_TO_Y	$DF
; for start key 
#define KEYBOARD_READ_PORT_A_TO_G	$FD
; keyboard port for shift key to v
#define KEYBOARD_READ_PORT_SHIFT_TO_V $FE
; keyboard space to b
#define KEYBOARD_READ_PORT_SPACE_TO_B $7F 
; keyboard q to t
#define KEYBOARD_READ_PORT_Q_TO_T $FB

; starting port numbner for keyboard, is same as first port for shift to v
#define KEYBOARD_READ_PORT $FE 
#define SCREEN_WIDTH 32
#define SCREEN_HEIGHT 23   ; we can use the full screen becuase we're not using PRINT or PRINT AT ROM subroutines
#define SHAPE_CHAR_WALL 189
#define TREASURE_CHARACTER 141  ; $$$$


VSYNCLOOP       EQU      3

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
    ld a, 10
    ld (jumpDelayBackoff), a
    
    xor a
    ld (moveRoomFlag), a
    ld (score_mem_tens),a
	ld (score_mem_hund),a    
    ld (currentRoom), a
    ld (groundPlatFlag), a  ; set to zero as we start player above
    ld (justJumpFlag),a
    ld a, 1
    ld (roomJustEnteredFlag), a
    xor a  ; fastest way to zero register a
    ld (YSpeed), a
    ld de, 269    
    ld hl, Display+1 
    add hl, de
    ld (currentPlayerLocation), hl
    ld (previousPlayerLocation), hl
    ld hl, playerSpriteRightMove
    ld (playerSpritePointer), hl
    ld a, 5
    ld (playerXPos), a
    ld a, 9
    ld (playerYPos), a      ; this is the position above the bottom so 0 is the bottom most
    ld a, 2
    ld (compareValueGround), a
    
    xor a
    ld (enemySpriteFrame), a
    ld (enemySpriteOneFrame), a
    ld hl, enemySpriteZero
    ld (enemySpritePointerZero), hl
    ld hl, enemySpriteOne
    ld (enemySpritePointerOne), hl
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
gameLoop    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ld b,VSYNCLOOP
waitForTVSync	
	call vsync
	djnz waitForTVSync
    
    ld hl, (currentPlayerLocation) ;; hl is the location to start checking
    ld de, -33
    add hl, de
    ld b, 9 ;; b is the rows to check 
    ld c, 8 ;; c is the columns to check    
    call checkAndGoldCollect  ; we check this before the blank sprite is drawn     
    
    ; if just entered room draw room
    ld a, (roomJustEnteredFlag)
    cp 1
    jp nz, skipRoomDraw
    call drawRoom
   
    ;; we're resetting player x y and currentPlayerLocation to fixed number
    ;; this needs to come from room config - player may then start in different location 
    ld a, 5
    ld (playerXPos), a
    ld a, 9
    ld (playerYPos), a      ; this is the position above the bottom so 0 is the bottom most
    ld de, 269    
    ld hl, Display+1 
    add hl, de
    ld (currentPlayerLocation), hl
    
    xor a
    ld (goldFoundInRoom), a
    ld (roomJustEnteredFlag),a
    
skipRoomDraw     
    call checkIfPlatformOrGround   ; sets groundPlatFlag
    
    jp nz, setBiggerBlankSprite
    ld a, (groundPlatFlag)
    cp 1
    jp z, setSmallerBlankSprite      ;if platform  set smaller 
    jp setBiggerBlankSprite          ; else
setSmallerBlankSprite    
    ld b, 9
    jp skipsetBlankSprite       
setBiggerBlankSprite        
    ld b, 10   ; pre set the normal blank sprite rows to 9
skipsetBlankSprite       
    xor a
    ld (justJumpFlag), a
       
    ld hl, (currentPlayerLocation)
    ld de, -33
    add hl, de
    ex de, hl
    ld hl, blankSprite
    ld c, 8
    call drawSprite    
        
    ld hl, (playerSpritePointer)    
    ld de, (currentPlayerLocation)
    ld c, 8
    ld b, 8    
    call drawSprite
    
    call drawPlatforms   ; always do this as jump may corrupt them  
    
    call drawEnemySprites    

; keyboard layout for reading keys on ZX81
; BIT   left block      right block  BIT
; off                                off in <port>, when ld a, <port>
;       0  1 2 3 4     4 3 2 1 0                 <<< bit to check for each column after in a, $fe 
; 3   ( 1  2 3 4 5 ) ( 6 7 8 9 0 )     4
; 2   ( Q  W E R T ) ( Y U I O P )     5
; 1   ( A  S D F G ) ( H I K L n/l)    6
; 0   (sft Z X C V ) ( B N M . spc)    7
;
; to read keys 1 2 3 4 5
; set all bits except bit 3 of register A = 1 1 1 1 0 1 1 1= f7, then execute in a, $fe  (fe is the "keyboard read port")
; now register a will contain a bit pattern to check for which key in that block was set, eg Key "1" = bit 0 of a
; ld a, $f7    
; in a, $fe    
; similarly for the rest, to read from block A S D F G, set a to 1 1 1 1 1 1 1 0 1 = $fd

    
    ;; read keys
    ld a, KEYBOARD_READ_PORT_P_TO_Y			
    in a, (KEYBOARD_READ_PORT)					; read from io port	
    bit 1, a                            ; O
    jp z, moveLeft


    ld a, KEYBOARD_READ_PORT_P_TO_Y			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 0, a					        ; P
    jp z, moveRight
    
    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 0, a						    ; SPACE
    jp z, doJump
     
    jp updateRestOfScreen                       ; if no key pressed continue

moveLeft         
    ld a, (playerXPos)
    dec a
    cp 0
    jp z, updateRestOfScreen   
    ld (playerXPos), a
        
    ld hl, (currentPlayerLocation)
    ld (previousPlayerLocation), hl
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

    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 0, a						    ; SPACE
    jp z, doJump
    
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
    cp 24          ;;; this prevents the player moving past edge, but if it's a door
                   ;; trigger seperate code to move to new room
    ;jp z, updateRestOfScreen   
    push af   ;; preserve flags (as well as register a)
    call z, checkAtDoorRight    
    pop af    ;; restore flags (as well as register a)
    jp z, updateRestOfScreen   
    ld (playerXPos), a
    
    
    
    ld hl, (currentPlayerLocation)
    ld (previousPlayerLocation), hl
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

    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 0, a						    ; SPACE
    jp z, doJump
    
    jp updateRestOfScreen 
spriteNextRight    
    ld hl, playerSpriteRightMove
    ld (playerSpritePointer), hl    
    xor a
    ld (spriteFrameCycle), a
    jp updateRestOfScreen 
    
doJump      ; triggered when jump key pressed just sets the YSpeed      
    ld a, (groundPlatFlag)
    cp 1
    jp z, setYSpeed
    jp updateRestOfScreen
setYSpeed    ;;; we've allowed the jusp to happen - can't keep jumping in mid air!
    ld a, 6
    ld (YSpeed), a   
    ld a, 1
    ld (justJumpFlag),a
 

updateRestOfScreen    
    ld a, (moveRoomFlag)
    cp 1
    call z, executeMoveRoom
    
    ld a, (YSpeed)
    cp 0
    jp nz, jumpUpLoopExe
    jp skipDecrmentYSpeed           
jumpUpLoopExe

    ld a, (playerYPos)   ; check player not at top (ie his head his top of room!)
    cp 14
    jp z, zeroYSpeedBangedHead
    ;cp 14
    ;jp z, checkYPosition    
    ld b, 2    
jumpUpLoop    
    ld hl, (currentPlayerLocation)
    ld (previousPlayerLocation), hl      
    ld de, -33 
    add hl, de
    ld (currentPlayerLocation), hl  
       
    ld a, (playerYPos)
    inc a
    ld (playerYPos), a    
    djnz jumpUpLoop  
    
    ld a, (YSpeed)
    dec a
    ld (YSpeed),a
    jp checkYPosition

zeroYSpeedBangedHead
    xor a
    ld (YSpeed), a
skipDecrmentYSpeed
   
checkYPosition  ; need to bring player back to ground   
    ld a, (groundPlatFlag)
    cp 0
    jp z, landPlayer
    jp skipLandPlayer
    
;checkYPositiontestSucceeded
landPlayer
    ld hl, (currentPlayerLocation)
    ld (previousPlayerLocation), hl
    ld de, 33 
    add hl, de
    ld (currentPlayerLocation), hl
    ld a, (playerYPos)    
    dec a
    ld (playerYPos), a    
    
skipLandPlayer        
      
skipMove                

;;; player x,y debug
#ifdef DEBUG_PLAYER_XY
    ld de, 34
    ld a, (playerXPos)
    call print_number8bits    
    ld a, (playerYPos)
    ld de, 38
    call print_number8bits
#endif
   
    jp gameLoop
    

;set a flag if the next row is platform or ground    
checkIfPlatformOrGround
#ifdef DEBUG_GROUND
;;;;;;;; start of debug
    ld bc, (currentPlayerLocation)     ;; currentPlayerLocation is already offset to
    ld de, 42
    call print_number16bits

    ld hl, (currentPlayerLocation)     ;; currentPlayerLocation is already offset to Display+1    
    ld de, 266    ; offset hl to +1 row from bottom of sprite
    add hl, de
    
    push hl
    pop bc
    ld de, 47
    call print_number16bits
    
    ld hl, (DF_CC)     ;; currentPlayerLocation is already offset to Display+1    
    push hl
    pop bc
    ld de, 52
    call print_number16bits    
;;;;;;;;;; END OF DEBUG
#endif

    ld hl, (currentPlayerLocation)     ;; currentPlayerLocation is already offset to Display+1    
    ld de, 266    ; offset hl to +1 row from bottom of sprite
    add hl, de
    
   
    ld b, 4   ; loop counter to check blocks just under sprite
compareGNDLoop    
    ld a, (hl)
    inc hl
    cp SHAPE_CHAR_WALL  
    jp z, setFlagGroundPlatform
    djnz compareGNDLoop
    
    
    xor a
    ld (groundPlatFlag), a
    jp checkIfPlatformOrGroundEND; no ground or platform found
setFlagGroundPlatform
    ld a, 1
    ld (groundPlatFlag), a    
    ;ld (comparePlatformOrGround), a
checkIfPlatformOrGroundEND    
    ret


checkAndClearDoor
;; works on the premise that there's always 4 gold per room
    ld a, (goldFoundInRoom)
    cp 4
    jr z, doorClear
    jr noDoorClear    
doorClear    
    ;; this is long winded approach becasue on zx81 can't use the iy or ix registers todo offsets
    ld de, (RoomConfigAddress)
    ld hl, 2
    add hl, de
    
    ld e, (hl)                   ; load the low byte of the address into register e
    inc hl                       ; increment hl to point to the high byte of the address
    ld d, (hl)                   ; load the high byte of the address into register d
    
    ld hl, (DF_CC)
    add hl, de 
    ld (doorStartAddress), hl
    ;; no loop for hieght of door 
    ld de, (RoomConfigAddress)
    ld hl, 4
    add hl, de
    ld a, (hl)
    ld b, a
    xor a   ; clear a to blank character now door open 
    ld hl, (doorStartAddress)
doorClearLoop

    ld (hl), a
    ld de,33
    add hl, de
    djnz doorClearLoop
    
    ;; we'll also need to clear all other doors 
    ;; we'll also need to set a flag to allow move to next room
noDoorClear
    ret
    
;;; set a move room flag is attempted to move to a door on right (will need checkAtDoorLeft later)
;; only check if player is at edge from moveRight code
checkAtDoorRight
    ;; ok doors are always on edge of room so use currentPlayerLocation
    ;; to check if the wall next to us is blank
    ld hl, (currentPlayerLocation)  ;; remember this is the top right position of 8*8 player
    ld de, 8   ; add 9 to currentPlayerLocation giving the boarder 
    add hl, de
    ld a, (hl)
    cp 0
    jp z, setMoveRoomFlag
    jp noMoveRoomFlag
setMoveRoomFlag    
    ld a, 1
    ld (moveRoomFlag), a

    ;ld de, moveRoomDebugFlagText
    ;ld bc, 91
    ;call printstring
    
noMoveRoomFlag
    ret
    
executeMoveRoom
    xor a
    ld (moveRoomFlag), a  ; first thing clear this flag otherwise continually move room :)
    ld de, moveRoomDebugTest
    ld bc, 76
    call printstring
    ld a, 1
    ld (roomJustEnteredFlag), a   ; set this will trigger a full room redraw
    ld a, (currentRoom) 
    inc a
    ld (currentRoom), a
    
    ret 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
drawRoom
    call CLS
    ld hl, RoomConfig
    ld a, (currentRoom)    
    cp 0
    jp z, skipCalcualteRoomCOnfig   
    ld de, 44 ; this is the current length of room, will need revisiting if it gets longer
    ld b,a       
drawRoomCalcOffsetToRoom    
    ;;; ad 32 to offset to get next room
    add hl, de 
    djnz drawRoomCalcOffsetToRoom
skipCalcualteRoomCOnfig
    ld (RoomConfigAddress), hl
    push hl
#ifdef DEBUG
    ld bc, (RoomConfigAddress)     ;; currentPlayerLocation is already offset to
    ld de, 87
    call print_number16bits
#endif    
    pop hl
    
    ; draw full boarder for every room   
    ld de, TopLineText
    ld bc, 2
    call printstring
    
    ld a, (currentRoom)
    daa
    ld de, 11
    call print_number8bits
    
    
    ld de, 33    
    ld hl, Display+1    
    add hl, de        
    ld b, 32
drawRoom_drawLineZero         ; draw the boarder at top and bottom
    ld (hl), SHAPE_CHAR_WALL
    inc hl
    djnz drawRoom_drawLineZero
    ld de, 759   ; this is 32+DF_CC to the left most char of bottom row
    ld hl, Display+1    
    add hl, de        
    ld b, 32
drawRoom_drawLineLast  
    ld (hl), SHAPE_CHAR_WALL
    inc hl
    djnz drawRoom_drawLineLast        
    
    ld b, 23            ;; best way to just draw column down each side of screen
    ld de, 31
    ld hl, Display+1
drawColZero      
    ld (hl), SHAPE_CHAR_WALL          
    add hl, de  
    ld (hl), SHAPE_CHAR_WALL    
    inc hl    
    inc hl
    djnz drawColZero    
    
drawRoomDoors   ; basically overwrite the boarder with character 0
    ;; this is long winded approach becasue on zx81 can't use the iy or ix registers todo offsets
    ld de, (RoomConfigAddress)
    ld hl, 2
    add hl, de
    
    ld e, (hl)                   ; load the low byte of the address into register e
    inc hl                       ; increment hl to point to the high byte of the address
    ld d, (hl)                   ; load the high byte of the address into register d
    
    ld hl, (DF_CC)
    add hl, de 
    ld (doorStartAddress), hl
    ;; no loop for hieght of door 
    ld de, (RoomConfigAddress)
    ld hl, 4
    add hl, de
    ld a, (hl)
    ld b, a
    ld a, 41
    ld hl, (doorStartAddress)
doorDrawLoop

    ld (hl), a
    ld de,33
    add hl, de
    djnz doorDrawLoop

    ld hl, 28        
    ld b, 4
drawTreasure    
    ld de, (RoomConfigAddress)    
    push hl
        add hl, de    
        ld e, (hl)                   ; load the low byte of the address into register e
        inc hl                       ; increment hl to point to the high byte of the address
        ld d, (hl)                   ; load the high byte of the address into register d    
    
    pop hl
        inc hl 
        inc hl
    push hl       
        ld hl, (DF_CC)
        add hl, de 
        ld a, TREASURE_CHARACTER    ; inverse $ for treasure
        ld (hl), a
    pop hl
    djnz drawTreasure    

    
drawPlatforms
    ;; this is long winded approach becasue on zx81 can't use the iy or ix registers todo offsets
    ld de, (RoomConfigAddress)
    ld hl, 17
    add hl, de
    
    ld e, (hl)                   ; load the low byte of the address into register e
    inc hl                       ; increment hl to point to the high byte of the address
    ld d, (hl)                   ; load the high byte of the address into register d
    
    ld hl, (DF_CC)
    add hl, de 
    ld (platformStartAddress), hl

    ld de, (RoomConfigAddress)
    ld hl, 19
    add hl, de
    ld a, (hl)
    ld b, a
    
    ;ld de, (RoomConfigAddress)
    ;ld hl, 16
    ;;add hl, de
    ;ld a, (hl)         
    ld hl, (platformStartAddress)
drawPlatform1
    ld a, (hl)
    cp 0
    jr nz, skipDrawAlreadySprite1            
    ld a, SHAPE_CHAR_WALL ; force all platforms to be same to help ease checking landed
    ld (hl), a
skipDrawAlreadySprite1    
    inc hl
    djnz drawPlatform1


    ld de, (RoomConfigAddress)
    ld hl, 21
    add hl, de
    
    ld e, (hl)                   ; load the low byte of the address into register e
    inc hl                       ; increment hl to point to the high byte of the address
    ld d, (hl)                   ; load the high byte of the address into register d
    
    ld hl, (DF_CC)
    add hl, de 
    ld (platformStartAddress), hl

    ld de, (RoomConfigAddress)
    ld hl, 23
    add hl, de
    ld a, (hl)
    ld b, a
    
    ; ld de, (RoomConfigAddress)
    ; ld hl, 20
    ; add hl, de
    ; ld a, (hl)     
    ld hl, (platformStartAddress)
drawPlatform2
    ld a, (hl)
    cp 0
    jp nz, skipDrawAlreadySprite2
    ld a, SHAPE_CHAR_WALL ; force all platforms to be same to help ease checking landed
    ld (hl), a
skipDrawAlreadySprite2    
    inc hl
    djnz drawPlatform2


    ld de, (RoomConfigAddress)
    ld hl, 25
    add hl, de
    
    ld e, (hl)                   ; load the low byte of the address into register e
    inc hl                       ; increment hl to point to the high byte of the address
    ld d, (hl)                   ; load the high byte of the address into register d
    
    ld hl, (DF_CC)
    add hl, de 
    ld (platformStartAddress), hl

    ld de, (RoomConfigAddress)
    ld hl, 27
    add hl, de
    ld a, (hl)
    ld b, a
    
    ; ld de, (RoomConfigAddress)
    ; ld hl, 24
    ; add hl, de
    ; ld a, (hl)      
    ld hl, (platformStartAddress)
drawPlatform3
    ld a, (hl)
    cp 0
    jp nz, skipDrawAlreadySprite3
    ld a, SHAPE_CHAR_WALL ; force all platforms to be same to help ease checking landed
    ld (hl), a
skipDrawAlreadySprite3
    inc hl

    djnz drawPlatform3
    
    ret

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
    ldir                  ;; ldir repeats ld (de), (hl) until bc = 0 and increments hl and de
    pop de
    ex de, hl    
    ld bc, 33             ;; move next write position to next row
    add hl, bc
    ex de, hl
    pop bc
    djnz drawSprite    
    ret


;;; work in progrerss currently crashes - 
;; if this could be made to work then the platforms would appear in blank bits of sprite
;; which would made game play better
drawSprite_OR_BACKGROUND         
    push bc    
    push de
    
    ld b, c    ; get column loop counter in b 
drawSprite_OR_ColLoop
    ld a, (hl)
    inc hl
    or d
    or e
    ld (de), a
    inc de
    djnz drawSprite_OR_ColLoop

    pop de
    ex de, hl    
    ld bc, 33             ;; move next write position to next row
    add hl, bc
    ex de, hl
    pop bc
    djnz drawSprite_OR_BACKGROUND    
    ret  




;; hl is the location to start checking
;; b is the rows to check
;; c is the columns to check
checkAndGoldCollect 
    xor a
    ld (goldFoundCount), a
checkAndGoldCollectRowLoop
    push bc          
    ld b, c    ; get column loop counter in b 
         push hl
GoldCollectColLoop       
            ld a, (hl)
            inc hl            
            cp TREASURE_CHARACTER
            jr z, foundGold_YES           
            djnz GoldCollectColLoop
        pop hl
        ld de, 33             ;; move next write position to next row
        add hl, de
    pop bc
    djnz checkAndGoldCollectRowLoop    
    jp justPrintScore
    
foundGold_YES
    pop hl  ;; as we jumped out of the loop need to pop these
    pop bc  ;; as we jumped out of the loop need to pop these
    
    ld a, 1
    ld (goldFoundCount),a    ;set this for just below where score gets inc'd
    ld c, 1    
    ld a, (goldFoundInRoom)
    add a, c
    ld (goldFoundInRoom), a
    
    call checkAndClearDoor        
    
    ld a, (goldFoundCount)
    cp 0
    jp z, justPrintScore
    ld b, a

    ld a,(score_mem_tens)				; add one to score, scoring is binary coded decimal (BCD)
	add a,1	
	daa									; z80 daa instruction realigns for BCD after add or subtract
	ld (score_mem_tens),a	
	cp 153
	jr z, addOneToHund
	jr skipAddHund
addOneToHund
	ld a, 0
	ld (score_mem_tens), a
    ld a, (score_mem_hund)
	add a, 1
	daa                                   ; z80 daa instruction realigns for BCD after add or subtract
	ld (score_mem_hund), a
skipAddHund	
    
justPrintScore
    ld bc, 23
    ld de, score_mem_tens
    call printNumber
    ld bc, 21
    ld de, score_mem_hund
    call printNumber    
    
    ret
    
drawEnemySprites    
    ld a, (enemySpriteFrame)
    inc a
    cp 3
    jp z, resetEnemySpriteZ 
    
    ld (enemySpriteFrame), a
    ld hl, (enemySpritePointerZero)
    ld de,16     ; 4 by 4 blocks
    add hl, de
    ld (enemySpritePointerZero), hl    
    
    jp skipResetEnemySpriteZ     
resetEnemySpriteZ    
    xor a
    ld (enemySpriteFrame), a
    ld hl, enemySpriteZero
    ld (enemySpritePointerZero), hl
skipResetEnemySpriteZ         
    ld de, 640
    ld hl, Display+1 
    add hl, de
    ex de, hl
    ld hl, (enemySpritePointerZero)
    ld b, 4
    ld c, 4
    call drawSprite         

    ld a, (enemySpriteOneFrame)
    inc a
    cp 3
    jp z, resetEnemySpriteOne 
    
    ld (enemySpriteOneFrame), a
    ld hl, (enemySpritePointerOne)
    ld de,16     ; 4 by 4 blocks
    add hl, de
    ld (enemySpritePointerOne), hl    
    
    jp skipResetEnemySpriteOne     
resetEnemySpriteOne    
    xor a
    ld (enemySpriteOneFrame), a
    ld hl, enemySpriteOne
    ld (enemySpritePointerOne), hl
skipResetEnemySpriteOne         
    ld de, 113
    ld hl, Display+1 
    add hl, de
    ex de, hl
    ld hl, (enemySpritePointerOne)
    ld b, 4
    ld c, 4
    call drawSprite       
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

printNumber
    ld hl,Display
    add hl,bc	
printNumber_loop
    ld a,(de)
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
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76                     
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           

Variables: 
LineOfBlank
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                               
  
YSpeed   
    DEFB 0
currentPlayerLocation 
    DEFW 0
previousPlayerLocation    
    DEFW 0
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

    
   
; used to clear current location before move    
blankSprite
    DEFB   0,  0,  0,  0,  0,  0,  0,  0
    DEFB   0,  0,  0,  0,  0,  0,  0,  0
    DEFB   0,  0,  0,  0,  0,  0,  0,  0
    DEFB   0,  0,  0,  0,  0,  0,  0,  0
    DEFB   0,  0,  0,  0,  0,  0,  0,  0
    DEFB   0,  0,  0,  0,  0,  0,  0,  0
    DEFB   0,  0,  0,  0,  0,  0,  0,  0
    DEFB   0,  0,  0,  0,  0,  0,  0,  0    
    DEFB   0,  0,  0,  0,  0,  0,  0,  0
    DEFB   0,  0,  0,  0,  0,  0,  0,  0      
    DEFB   8,  8,  0,  0,  0,  0,  0,  0      
blockFilled    ;8*10
    DEFB   8,  8,  8,  8,  8,  8,  8,  8
    DEFB   8,  8,  8,  8,  8,  8,  8,  8
    DEFB   8,  8,  8,  8,  8,  8,  8,  8
    DEFB   8,  8,  8,  8,  8,  8,  8,  8
    DEFB   8,  8,  8,  8,  8,  8,  8,  8
    DEFB   8,  8,  8,  8,  8,  8,  8,  8
    DEFB   8,  8,  8,  8,  8,  8,  8,  8
    DEFB   8,  8,  8,  8,  8,  8,  8,  8
    DEFB   8,  8,  8,  8,  8,  8,  8,  8     
    DEFB   8,  8,  8,  8,  8,  8,  8,  8    

enemySprites   ;; keeping these to 4*4 for speed and size
enemySpriteZero
	DEFB $07, $03, $03, $84, $05, $00, $00, $85, $05, $00, $00, $85,
	DEFB $82, $83, $83, $81, $87, $83, $83, $04, $85, $00, $00, $05,
	DEFB $85, $00, $00, $05, $02, $03, $03, $01, $00, $00, $00, $00,
	DEFB $00, $07, $84, $00, $00, $82, $81, $00, $00, $00, $00, $00,
	DEFB $00, $00, $00, $00, $00, $87, $04, $00, $00, $02, $01, $00,
	DEFB $00, $00, $00, $00
enemySpriteOne
	DEFB $00, $85, $05, $00, $83, $81, $82, $83, $03, $84, $07, $03,
	DEFB $00, $85, $05, $00, $00, $02, $01, $00, $04, $81, $82, $87,
	DEFB $01, $84, $07, $02, $00, $87, $04, $00, $00, $85, $05, $00,
	DEFB $83, $87, $04, $83, $03, $02, $01, $03, $00, $85, $05, $00,
	DEFB $00, $85, $05, $00, $83, $06, $86, $83, $03, $86, $06, $03,
	DEFB $00, $85, $05, $00
    
TopLineText
    DEFB _J,_U,_M,_P, 136, _R, _O, _0, _M, 0, 28, 28, 0,136,136, _G, _O, _L, _D, 28, 28, 0,136, 136, 136,_B,_Y,_T,_E,32,$ff
moveRoomDebugTest
    DEFB _M,_O,_V,_E,_R,_O,_O,_M,$ff
moveRoomDebugFlagText
    DEFB _F,_L,_A,_G, $ff    
compareValueGround
    DEFB 0
comparePlatformOrGround
    DEFB 0
spriteFrameCycle
    DEFB 0
playerSpritePointer
    DEFW 0 
playerXPos    
    DEFB 0   
playerYPos    
    DEFB 0  
score_mem_tens
    DEFB 0
score_mem_hund
    DEFB 0
jumpDelayBackoff    
    DEFB 0
currentRoom
    DEFB 0
roomJustEnteredFlag
    DEFB 0
groundPlatFlag
    DEFB 0
justJumpFlag
    DEFB 0
goldFoundCount
    DEFB 0
goldFoundInRoom
    DEFB 0
moveRoomFlag
    DEFB 0
enemySpriteFrame    
    DEFB 0
enemySpriteOneFrame
    DEFB 0
enemySpritePointerZero    
    DEFW 0 
enemySpritePointerOne    
    DEFW 0
;================== Room config design - may only be partially implemented
;; fixed length of 32 bytes per room

; Room Config. Every room will have a predefined layout using simple constructs
; They will follow this common layout:
; 1) room id number - 8 bits 0 to 255 - room zero is start
; 2) doors  - 3 doors - can be enabled and disabled
;        a) orientation - east=1 west=2 up=3 down=4
;        b) start position (relative to Display+1 (DF_CC))
;        c) 8bit end position count from start
;        d) id of the room to enter next when pass through door
; 3) 3 platforms per room can be set valid or not 
;        a) character to draw
;        b) 16bit start position (relative to Display+1 (DF_CC))
;        c) 8bit end position count from start
;         terminated with 255 (chosen as that is never a valid character)
;         so you can have as many as you like
;        d) list of enemies 
;           i) start position
;           ii) end position 
;           iii) speed
;           iv) sprite memory location
; 4) list of tokens to collect
; ================
;; Enemy config - list of enemy types - including memory location of the sprite

;; early implementation will not have all the features
platformStartAddress
    DEFW 0
RoomConfigAddress
    DEFW 0
doorStartAddress
    DEFW 0
RoomConfig          ; each room is fixed at 32 bytes long

    DEFB 0    ; room ID
    ;;; DOORS  * 3 max enabled  
    DEFB 1    ; Door orientation east=1  0= door disabled
    DEFW 196   ; offset from DF_CC to top of door
    DEFB 8    ; 9 blocks high
    DEFB 1    ; ID of next room from this one
    DEFB 0    ; Door orientation east=1  0= door disabled
    DEFW 0   ; offset from DF_CC to top of door
    DEFB 0    ; 9 blocks high
    DEFB 0    ; ID of next room from this one
    DEFB 0    ; Door orientation east=1  0= door disabled
    DEFW 0   ; offset from DF_CC to top of door
    DEFB 0    ; 9 blocks high
    DEFB 0    ; ID of next room from this one  (byte 15)
    ;;; platforms max = 3 enabled            
    
    DEFB 8    ; character of platform 0 = disabled  (byte16)
    DEFW 610  ; start of platform   17,18
    DEFB 4    ; length   19
    
    DEFB 137    ; character of platform 0 = disabled  20
    DEFW 454  ; start of platform  21,22
    DEFB 6    ; length  23
    
    DEFB 128    ; character of platform 0 = disabled  24
    DEFW 364  ; start of platform  25,26
    DEFB 17    ; length             (byte 27)
    ;;; tokens 2 bytes each
    DEFW 211  ; treasure token offset from DF_CC   always 4 treasure (byte 28)
    DEFW 483  ; treasure token offset from DF_CC
    DEFW 168  ; treasure token offset from DF_CC
    DEFW 752  ; treasure token offset from DF_CC
    DEFB 0    ; enemy 1 sprite id
    DEFW 238  ; enemy 1start address
    DEFW 242  ; enemy 1end address
    DEFB 255  ;
    DEFB 255  ;
    DEFB 255  ;


    DEFB 1    ; room ID
    ;;; DOORS  * 3 max enabled  
    DEFB 1    ; Door orientation east=1  0= door disabled
    DEFW 460   ; offset from DF_CC to top of door
    DEFB 8    ; 9 blocks high
    DEFB 1    ; ID of next room from this one
    DEFB 0    ; Door orientation east=1  0= door disabled
    DEFW 0   ; offset from DF_CC to top of door
    DEFB 0    ; 9 blocks high
    DEFB 0    ; ID of next room from this one
    DEFB 0    ; Door orientation east=1  0= door disabled
    DEFW 0   ; offset from DF_CC to top of door
    DEFB 0    ; 9 blocks high
    DEFB 0    ; ID of next room from this one  (byte 15)
    ;;; platforms max = 3 enabled            
    
    DEFB 8    ; character of platform 0 = disabled  (byte16)
    DEFW 610  ; start of platform   17,18
    DEFB 6    ; length   19
    
    DEFB 137    ; character of platform 0 = disabled  20
    DEFW 454  ; start of platform  21,22
    DEFB 3    ; length  23
    
    DEFB 128    ; character of platform 0 = disabled  24
    DEFW 370  ; start of platform  25,26
    DEFB 5    ; length             (byte 27)
    ;;; tokens 2 bytes each
    DEFW 222  ; treasure token offset from DF_CC   always 4 treasure (byte 28)
    DEFW 715  ; treasure token offset from DF_CC
    DEFW 168  ; treasure token offset from DF_CC
    DEFW 752  ; treasure token offset from DF_CC
    DEFB 255  ;   spare
    DEFB 255  ;
    DEFB 255  ;
    DEFB 255  ;
    DEFB 255  ;
    DEFB 255  ;
    DEFB 255  ;
    DEFB 255  ;  


;;; rooms need defining this is just a copy of room 0

    DEFB 2    ; room ID   
    ;;; DOORS  * 3 max enabled  
    DEFB 1    ; Door orientation east=1  0= door disabled
    DEFW 196   ; offset from DF_CC to top of door
    DEFB 8    ; 9 blocks high
    DEFB 1    ; ID of next room from this one
    DEFB 0    ; Door orientation east=1  0= door disabled
    DEFW 0   ; offset from DF_CC to top of door
    DEFB 0    ; 9 blocks high
    DEFB 0    ; ID of next room from this one
    DEFB 0    ; Door orientation east=1  0= door disabled
    DEFW 0   ; offset from DF_CC to top of door
    DEFB 0    ; 9 blocks high
    DEFB 0    ; ID of next room from this one  (byte 15)
    ;;; platforms max = 3 enabled            
    
    DEFB 8    ; character of platform 0 = disabled  (byte16)
    DEFW 610  ; start of platform   17,18
    DEFB 6    ; length   19
    
    DEFB 137    ; character of platform 0 = disabled  20
    DEFW 454  ; start of platform  21,22
    DEFB 6    ; length  23
    
    DEFB 128    ; character of platform 0 = disabled  24
    DEFW 364  ; start of platform  25,26
    DEFB 17    ; length             (byte 27)
    ;;; tokens 2 bytes each
    DEFW 211  ; treasure token offset from DF_CC   always 4 treasure (byte 28)
    DEFW 483  ; treasure token offset from DF_CC
    DEFW 168  ; treasure token offset from DF_CC
    DEFW 752  ; treasure token offset from DF_CC
    DEFB 255  ;   spare
    DEFB 255  ;
    DEFB 255  ;
    DEFB 255  ;
    DEFB 255  ;
    DEFB 255  ;
    DEFB 255  ;
    DEFB 255  ;  



    DEFB 3    ; room ID   
    ;;; DOORS  * 3 max enabled  
    DEFB 1    ; Door orientation east=1  0= door disabled
    DEFW 196   ; offset from DF_CC to top of door
    DEFB 8    ; 9 blocks high
    DEFB 1    ; ID of next room from this one
    DEFB 0    ; Door orientation east=1  0= door disabled
    DEFW 0   ; offset from DF_CC to top of door
    DEFB 0    ; 9 blocks high
    DEFB 0    ; ID of next room from this one
    DEFB 0    ; Door orientation east=1  0= door disabled
    DEFW 0   ; offset from DF_CC to top of door
    DEFB 0    ; 9 blocks high
    DEFB 0    ; ID of next room from this one  (byte 15)
    ;;; platforms max = 3 enabled            
    
    DEFB 8    ; character of platform 0 = disabled  (byte16)
    DEFW 610  ; start of platform   17,18
    DEFB 1    ; length   19
    
    DEFB 137    ; character of platform 0 = disabled  20
    DEFW 454  ; start of platform  21,22
    DEFB 1    ; length  23
    
    DEFB 128    ; character of platform 0 = disabled  24
    DEFW 364  ; start of platform  25,26
    DEFB 1    ; length             (byte 27)
    ;;; tokens 2 bytes each
    DEFW 55  ; treasure token offset from DF_CC   always 4 treasure (byte 28)
    DEFW 55  ; treasure token offset from DF_CC
    DEFW 55  ; treasure token offset from DF_CC
    DEFW 55  ; treasure token offset from DF_CC
    DEFB 255  ;   spare
    DEFB 255  ;
    DEFB 255  ;
    DEFB 255  ;
    DEFB 255  ;
    DEFB 255  ;
    DEFB 255  ;
    DEFB 255  ;     
    
VariablesEnd:   DEFB $80
BasicEnd: 
#END
