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
;;    1) add enemy sprites to room config DONE
;;    2) add moveable enemy sprites  DONE
;;    3) rewrite enemy sprite draw to make it a smaller proper subroutines DONE
;;    4) design more rooms ONGOING
;;    5) add verical bariiers to rooms
;;    6) add player lives, and killed if hits enemy DONE
;;    7) add disapearing platforms and to room config 
;;    8) add more platforms  ONGOING (now can have 5)
;;    9) optimise the player check collision with gold to use only outer blocks DONE
;;   10) vertical moving enemys DONE
;;   11) add death scene DONE

;some #defines for compatibility with other assemblers
#define         DEFB .byte 
#define         DEFW .word
#define         EQU  .equ
#define         ORG  .org
CLS				EQU $0A2A
;;;;;#define DEBUG_NO_SCROLL
;;;;;#define DEBUG_PLAYER_XY
;;#define DEBUG_SPRITE_ADDRESS 1
;;#define DEBUG_PRINT_ROOM_NUMBER 1
;#define DEBUG_MULTIRATECOUNT 1
;#define DEBUG_START_IN_ROOM_X   1
;#define DEBUG_ROOM_TO_START_IN 12
;#define DEBUG_COLLISION_DETECT_1 1
;#define DEBUG_COLLISION_DETECT_2 1

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
#define INVERSE_TREASURE_CHARACTER 141  ; $$$$
#define NORMAL_TREASURE_CHARACTER 13  ; $$$$

#define INITIAL_PLAYER_X  5    ;;; TODO this should ultimately come from room config
#define INITIAL_PLAYER_Y  3
#define INITIAL_PLAYER_OFFSET 467
#define ENEMY_CHAR_1 133
#define ENEMY_CHAR_2 $5
#define ARROW_START 225
#define ARROW_END 198



#define LAST_ROOM 13

;70
#define SIZE_OF_ROOM_CONFIG Room_2_Config-Room_1_Config    
#define OFFSET_TO_TREASURE startOfRoom1Treasure-Room_1_Config
#define OFFSET_TO_ENEMY_SPRITES firstEnemyAddress-Room_1_Config
#define OFFSET_TO_ROOM_NAME RoomZeroName-Room_1_Config
#define OFFSET_TO_PLAYER_X_Y offsetToPlayerXY-Room_1_Config




VSYNCLOOP       EQU      2

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	jp intro_title		; main entry poitn to the code ships the memory definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
introWaitLoop
	ld b,128
introWaitLoop_1
    push bc	
        ld de, 496    
        ld hl, Display+1 
        add hl, de        
        ex de, hl
        ld hl, playerSpriteLeftMove
        ld c, 8
        ld b, 8    
        call drawSprite   
    pop bc
	djnz introWaitLoop_1
    jp read_start_key_1     ;; have to have 2 labels as not a call return
   
secondIntroWaitLoop    
   
    ld b, 128
introWaitLoop_2
    push bc
        ld de, 496    
        ld hl, Display+1 
        add hl, de        
        ex de, hl
        ld hl, playerSpriteRightMove
        ld c, 8
        ld b, 8    
        call drawSprite   
    pop bc
    djnz introWaitLoop_2

	jp read_start_key_2
	
intro_title
	call CLS  ; clears screen and sets the boarder
    ld a, (gameOverRestartFlag)
    cp 1
    call z, gameOverDeathScene
    
    xor a
    ld (gameOverRestartFlag), a
    
    ld a, (score_mem_tens)
    ld (last_score_mem_tens),a
    ld a, (score_mem_hund)
    ld (last_score_mem_hund),a        

    
	ld bc,111
	ld de,title_screen_txt
	call printstring
	ld bc,202    
	ld de,keys_screen_txt_1
	call printstring		
    
    ld bc,235    
	ld de,keys_screen_txt_2
	call printstring		
    

	   
	ld bc,336
	ld de,game_objective_txt
	call printstring	
	ld bc,436
	ld de,last_Score_txt
	call printstring	
	
    ld bc, 476
    ld de, last_score_mem_hund ; load address of hundreds
	call printNumber    
	ld bc, 478			; bc is offset from start of display
	ld de, last_score_mem_tens ; load address of  tens		
	call printNumber	
	ld bc,537	
	ld de,credits_and_version_1
	call printstring		
	ld bc,569	
	ld de,credits_and_version_2
	call printstring	
	ld bc,634	
	ld de,credits_and_version_3
	call printstring
    ld de, 496    
    ld hl, Display+1 
    add hl, de        
    ex de, hl
    ld hl, playerSpriteRightMove
    ld c, 8
    ld b, 8    
    call drawSprite
   
	
read_start_key_1
	ld a, KEYBOARD_READ_PORT_A_TO_G	
	in a, (KEYBOARD_READ_PORT)					; read from io port	
	bit 1, a									; check S key pressed 
	jp nz, secondIntroWaitLoop    
    ;; else drop into preinit then initVariables
    jp preinit
    
read_start_key_2
	ld a, KEYBOARD_READ_PORT_A_TO_G	
	in a, (KEYBOARD_READ_PORT)					; read from io port	
	bit 1, a									; check S key pressed 
	jp nz, introWaitLoop
    jp preinit

preinit
;; initialise variables that are once per game load/start

initVariables


    ;; initialise variables per game
   
    xor a
    ld (evenOddLoopFlag), a
    ld (evenOddLoopCount), a
    ld (moveRoomFlag), a
    ld (score_mem_tens),a
	ld (score_mem_hund),a    
    ld (currentRoom), a
    ld (currentRoomDisplay), a
    ld (groundPlatFlag), a  ; set to zero as we start player above
    ld (justJumpFlag),a
    ld a, 1
    ld (roomJustEnteredFlag), a
    xor a  ; fastest way to zero register a
    ld (YSpeed), a
    
    call setPlayerStartPosition    ;; last instruction in this call leaves hl
    ld (previousPlayerLocation), hl    ;; for this 
    
    ld hl, playerSpriteRightMove
    ld (playerSpritePointer), hl 
    ld a, 2
    ld (compareValueGround), a
    ld a, 6
    ld (playerLives), a
    
    xor a
    ld (gameOverRestartFlag), a
    ld (hitEnemyRestartRoomFlag), a
    ld (gameTime_Seconds), a
    ld (gameTime_Minutes), a
    ld (gameTimeCounterJIFFIES), a
    ld (enemySpriteFrameZero), a
    ld (enemySpriteFrameOne), a  
    ld (noJumpInNextGameLoopFlag), a 
    ld (bangedHeadFlag), a
    ld (landPlayerFlag), a
    
    call initialiseEnemysForRoom

    ld hl, enemySprite4by4Blank
    ld (enemySprite4by4BlankPointer), hl
    
    
    ld hl, Display+1
    ld de,ARROW_START
    add hl, de
    ld (deathArrowPostion), hl
    
    ld hl, Display+1
    ld de, ARROW_END
    add hl, de   
    ld (deathArrowEndPos), hl
    xor a
    ld (arrowEnabled), a


#ifdef DEBUG_START_IN_ROOM_X    
    ld a, DEBUG_ROOM_TO_START_IN
    ld (currentRoom), a    
    daa
    ld (currentRoomDisplay), a
#endif    

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
gameLoop    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ld b,VSYNCLOOP
waitForTVSync	
	call vsync
	djnz waitForTVSync
    
    xor a
    ld (evenOddLoopFlag), a    ; used for multi rate enemies
    
    ld a, (evenOddLoopCount)
    inc a
    ld (evenOddLoopCount), a
    cp 8    
    jr z, resetEvenOddAndSetFlag
    
    jr continueWithprintTime
resetEvenOddAndSetFlag    
    xor a
    ld (evenOddLoopCount), a
    ld a, 1
    ld (evenOddLoopFlag), a    ; used for multi rate enemies
    call drawTreasureSub    
    
continueWithprintTime      
    call printTime
    call printLives   
    call checkCollisionAndGoldCollect  ; we check this before the blank sprite is drawn     

    xor a   ; clear these used in checkCollisionAndGoldCollect
    ld (landPlayerFlag), a 
    ld (bangedHeadFlag), a
    
    ld a, (hitEnemyRestartRoomFlag)
    cp 1    
    call z, executeRestartCurrentRoom
    
    ld a, (gameOverRestartFlag)
    cp 1
    jp z, intro_title
    
   
    ; if just entered room draw room
    ld a, (roomJustEnteredFlag)
    cp 1
    jp nz, skipRoomDraw
    call resetTreasureInRoomSub
    call drawRoom  
    
    
    ;; we're resetting player x y and currentPlayerLocation to fixed number
    ;; this needs to come from room config - player may then start in different location 
    call setPlayerStartPosition
   
    xor a
    ld (goldFoundInRoom), a
    ld (roomJustEnteredFlag),a
    
skipRoomDraw    
    call blankEnemySprites
    call drawEnemySprites        
    call updateEnemySpritePositions
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
    
    call checkTreasureSub
    
    call drawPlatforms   ; always do this as jump may corrupt them  
    
    ld a, (currentRoom)
    cp 1
    jr z, drawArrowThisRoom     ;; special case for rooms 1 and 12 to have arrows
    cp 12
    jr z, drawArrowThisRoom    
    xor a 
    ld (arrowEnabled), a
    jr skipArraowDrawThisRoom
drawArrowThisRoom    
    ld a, 1
    ld (arrowEnabled), a
    call drawDeathAndUpdateArrow   
skipArraowDrawThisRoom
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

    ld a, (noJumpInNextGameLoopFlag)    
    cp 1 
    jr z, skipJumpKeyDetect_1
    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 0, a						    ; SPACE
    jp z, doJump
    
    ld a, KEYBOARD_READ_PORT_SHIFT_TO_V			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 1, a						    ; Z
    jp z, doJump    
skipJumpKeyDetect_1    
    xor a
    ld (noJumpInNextGameLoopFlag), a 
    jp updateRestOfScreen                       ; if no key pressed continue

moveLeft         
    ld a, (playerXPos)
    dec a
    cp 0      ;;; this prevents the player moving past edge, but if it's a door
              ;; trigger seperate code to move to new room
    push af   ;; preserve flags (as well as register a)
    call z, checkAtDoorLeft
    pop af    ;; restore flags (as well as register a)
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

    ld a, (noJumpInNextGameLoopFlag)    
    cp 1 
    jr z, skipJumpKeyDetect_3
    
    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 0, a						    ; SPACE
    jp z, doJump
    
    ld a, KEYBOARD_READ_PORT_SHIFT_TO_V			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 1, a						    ; Z
    jp z, doJump        
skipJumpKeyDetect_3    
    xor a
    ld (noJumpInNextGameLoopFlag), a 
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

    ld a, (noJumpInNextGameLoopFlag)    
    cp 1 
    jr z, skipJumpKeyDetect_2
    
    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 0, a						    ; SPACE
    jp z, doJump

    ld a, KEYBOARD_READ_PORT_SHIFT_TO_V			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 1, a						    ; Z
    jp z, doJump         
skipJumpKeyDetect_2
    xor a
    ld (noJumpInNextGameLoopFlag), a 
    jp updateRestOfScreen 
spriteNextRight    
    ld hl, playerSpriteRightMove
    ld (playerSpritePointer), hl    
    xor a
    ld (spriteFrameCycle), a
    jp updateRestOfScreen 
    
doJump      ; triggered when jump key pressed just sets the YSpeed      
    ld a, 1 
    ld (noJumpInNextGameLoopFlag), a
    ld a, (groundPlatFlag)   ;; this is so you can only jump off a platform
    cp 1
    jp z, setYSpeed
    jp updateRestOfScreen
setYSpeed    ;;; we've allowed the jump to happen - can't keep jumping in mid air
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
    ld a, 1
    ld (bangedHeadFlag), a 
    jp skipLandPlayer
    
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
    ld a, 1
    ld (landPlayerFlag),a 
      
    
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
#ifdef DEBUG_MULTIRATECOUNT
    ld a, (evenOddLoopCount)
    ld de, 68
    call print_number8bits
    
    ld a, (evenOddLoopFlag)
    ld de, 72
    call print_number8bits    
#endif    
#ifdef DEBUG_PRINT_ROOM_NUMBER
    ld a, (currentRoom)
    ld de, 73
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
    ld de, (Room_2_Config - Room_1_Config)  ; this handily calculates the room length from labels :)
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
;;; special case for the last room - it's setup so you can only 
;; ever get 3 of the 4 gold - if you do and it's the last room then player does a dance!!
    ld a, (currentRoom) 
    cp LAST_ROOM
    jr z, checkThreeGold
    jr checkNormalNumberGold
checkThreeGold
    ld a, (goldFoundInRoom)
    cp 3
    jp z, playerWonDoDanceMoves   ;;    :-)
    
checkNormalNumberGold    
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
    jp z, setMoveRoomFlagRight
    jp noMoveRoomFlagRight
setMoveRoomFlagRight    
    ld a, 1
    ld (moveRoomFlag), a

    ;ld de, moveRoomDebugFlagText
    ;ld bc, 91
    ;call printstring
    
noMoveRoomFlagRight
    ret
    
    
    
;;; set a move room flag is attempted to move to a door on right (will need checkAtDoorLeft later)
;; only check if player is at edge from moveRight code
checkAtDoorLeft
    ;; ok doors are always on edge of room so use currentPlayerLocation
    ;; to check if the wall next to us is blank
    ld hl, (currentPlayerLocation)  ;; remember this is the top right position of 8*8 player
    ld de, -1   ; add 9 to currentPlayerLocation giving the boarder 
    add hl, de
    ld a, (hl)
    cp 0
    jp z, setMoveRoomFlagLeft
    jp noMoveRoomFlagLeft
setMoveRoomFlagLeft    
    ld a, 1
    ld (moveRoomFlag), a

    ;ld de, moveRoomDebugFlagText
    ;ld bc, 91
    ;call printstring
    
noMoveRoomFlagLeft
    ret    
    
    
    
    
executeMoveRoom
    xor a
    ld (moveRoomFlag), a  ; first thing clear this flag otherwise continually move room :)
    ld (hitEnemyRestartRoomFlag), a
    
#ifdef DEBUG_ROOM_MOVE    
    ld de, moveRoomDebugTest
    ld bc, 76
    call printstring
#endif    
    ld a, 1
    ld (roomJustEnteredFlag), a   ; set this will trigger a full room redraw
    ld a, (currentRoom) 
    inc a
    ld (currentRoom), a
    daa
    ld (currentRoomDisplay), a
    call initialiseEnemysForRoom
    
    ret 

executeRestartCurrentRoom
    xor a
    ld (moveRoomFlag), a  ; first thing clear this flag otherwise continually move room :)
    ld (hitEnemyRestartRoomFlag), a
    
    ld hl, Display+1
    ld de,ARROW_START
    add hl, de
    ld (deathArrowPostion), hl
    
#ifdef DEBUG_ROOM_MOVE    
    ld de, moveRoomDebugTest
    ld bc, 76
    call printstring
#endif    
    ld a, 1
    ld (roomJustEnteredFlag), a   ; set this will trigger a full room redraw    
    call initialiseEnemysForRoom
    
    ret 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
drawRoom
#ifdef DEBUG_START_IN_ROOM_X    
    ld a, DEBUG_ROOM_TO_START_IN
    ld (currentRoom), a
    daa
    ld (currentRoomDisplay), a    
#endif    

    call CLS
    ld hl, RoomConfig
    ld a, (currentRoom)    
    cp 0
    jp z, skipCalcualteRoomCOnfig   
    ld de, SIZE_OF_ROOM_CONFIG 
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
    
    ld a, (currentRoomDisplay)
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

    call drawTreasureSub    
    call drawPlatforms   ; moved completely into own subroutine

    ld de, (RoomConfigAddress)
    ld hl, OFFSET_TO_ROOM_NAME
    add hl, de
    ld bc, 35
    ex de, hl    
    call printstring    
    
    ret

resetTreasureInRoomSub

    ld hl, OFFSET_TO_TREASURE        
    ld b, 4    
    ld de, (RoomConfigAddress)  
    add hl, de    

resetTreasureCheckLoop      
    inc hl ;; this gets us to the address of the enable disable flag for that treasure
    inc hl          ; hl is now the enable treasure flag in room config
    ld (hl), INVERSE_TREASURE_CHARACTER    
    inc hl
    djnz resetTreasureCheckLoop 
    ret


checkTreasureSub
    ld hl, OFFSET_TO_TREASURE        
    ld b, 4    

drawTreasureCheckLoop    
    push hl 
        ld de, (RoomConfigAddress)        
        add hl, de    

        ld e, (hl)                   ; load the low byte of the address into register e
        inc hl                       ; increment hl to point to the high byte of the address
        ld d, (hl)                   ; load the high byte of the address into register d        
        
        ld hl, (DF_CC)        
        add hl, de
        ld a, (hl)
    pop hl 
    
    inc hl ;; hl here is the OFFSET_TO_TREASURE plus however many loops times 3bytes in each treasure config
    inc hl ;; hl here is the OFFSET_TO_TREASURE plus however many loops times 3bytes in each treasure config

        
    ; push hl
    ; push de
    ; push bc
    ; push af         
        ; ld de, 69
        ; ld hl, (DF_CC)        
        ; add hl, de
        ; ld (hl), a     ; a here is what is in the location on screen of the treausre (ie treasure or nothing player)
    ; pop af 
    ; pop bc    
    ; pop de
    ; pop hl     
    cp INVERSE_TREASURE_CHARACTER
    jr z, continueCheckingForNextTreasure
    cp NORMAL_TREASURE_CHARACTER
    jr z, continueCheckingForNextTreasure

    ;; we need to get hl to point to the correct room config address offset up to the next enable disable for treasure
    push hl 
        ld de, (RoomConfigAddress)        
        add hl, de  
        
        ; push hl
        ; push de
        ; push bc
        ; push af 
            ; ld a, (hl)    
            ; ld de, 76
            ; call print_number8bits
        ; pop af 
        ; pop bc    
        ; pop de
        ; pop hl  
        xor a
    
        ld (hl), a
    pop hl     
continueCheckingForNextTreasure        
    inc hl   ; this gets hl to next treasure config
    
    djnz drawTreasureCheckLoop 
    ret


drawTreasureSub    
    ld hl, OFFSET_TO_TREASURE        
    ld b, 4    
drawTreasure    
    push hl  ;; hl here is the OFFSET_TO_TREASURE plus however many loops times 3bytes in each treasure config
        ld de, (RoomConfigAddress)        
        add hl, de    

        ;; after the next three lines de will point to the display address offset of the treasure
        ld e, (hl)                   ; load the low byte of the address into register e
        inc hl                       ; increment hl to point to the high byte of the address
        ld d, (hl)                   ; load the high byte of the address into register d        

        push hl 
            inc hl ;; this gets us to the address of the enable disable flag for that treasure
            ld a, (hl)  
            cp 0
        pop hl 
    pop hl  ;; hl here is the OFFSET_TO_TREASURE plus however many loops times 3bytes in each treasure config
    jr z, skipDrawTreasure    
    
    push hl       
        ld hl, (DF_CC)
        add hl, de                    
        ld a, (treasureCharacter)
        ld (hl), a
    pop hl  
skipDrawTreasure    
    inc hl ;; hl here is the OFFSET_TO_TREASURE plus however many loops times 3bytes in each treasure config
    inc hl ;; hl here is the OFFSET_TO_TREASURE plus however many loops times 3bytes in each treasure config   
    inc hl ;; hl here is the OFFSET_TO_TREASURE plus however many loops times 3bytes in each treasure config   
    djnz drawTreasure    

    ld a, (goldToggleControl)
    cp 1
    jr z, goldToggle
    ld a, 1
    ld (goldToggleControl), a
    ld a, INVERSE_TREASURE_CHARACTER    ; inverse $ for treasure
    ld (treasureCharacter), a 
    jr afterGoldToggle
goldToggle
    xor a
    ld (goldToggleControl), a
    ld a, NORMAL_TREASURE_CHARACTER    ; inverse $ for treasure
    ld (treasureCharacter), a 
afterGoldToggle      
    ret
    
    
drawPlatforms
    push hl
    push de
        ;; this is long winded approach becasue on zx81 can't use the iy or ix registers todo offsets
        ld de, (RoomConfigAddress)
        ld hl, 16     ;; offset to start of platform config in room config - enabled/disabled
        add hl, de
        ld b, 5       ;; 3 platforms (currently
platformLoop    
        push bc
        push hl 
            ld a, (hl)
            cp 0            ;; check config is the platform disabled or not
            jp z, skipPlatform
            inc hl    ;; this gets hl to point to the start of platform offset in room config
            call drawPlatform        
skipPlatform  
        pop hl  
        inc hl    ;; still need to increment hl to move to next platform in config
        inc hl
        inc hl
        inc hl

        pop bc
        djnz platformLoop
    pop de
    pop hl 
    ret

drawPlatform  
    push hl  
        ld e, (hl)                   ; load the low byte of the address into register e
        inc hl                       ; increment hl to point to the high byte of the address
        ld d, (hl)                   ; load the high byte of the address into register d
    
        ld hl, (DF_CC)
        add hl, de 
        ld (platformStartAddress), hl
    pop hl
    inc hl 
    inc hl    ; this gets hl to the memory location of the length of platform in room config
    ld a, (hl)
    ld b, a
    
    ld hl, (platformStartAddress)
drawPlatformLoop
    ld a, (hl)
    cp 129
    jr z, doTheDrawPlatformAnyway
    cp 130
    jr z, doTheDrawPlatformAnyway    
    cp 0    
    jr nz, skipDrawAlreadySprite1            
    
doTheDrawPlatformAnyway
    ld a, SHAPE_CHAR_WALL ; force all platforms to be same to help ease checking landed
    ld (hl), a
skipDrawAlreadySprite1    
    inc hl
    djnz drawPlatformLoop
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

;;;; TODO check only the outer edges of player

checkCollisionAndGoldCollect 
    ld hl, (currentPlayerLocation) ;; hl is the location to start checking
    ld de, -33
    add hl, de
    ld b, 8 ;; b is the rows to check 
    ld c, 8 ;; c is the columns to check    

    push hl
    push bc
    call checkForGold
    pop bc
    pop hl
    

    ld a, (bangedHeadFlag)
    cp 1        
    jr z, adjustToOneRowHigher
    
    ld a, (landPlayerFlag)
    cp 1        
    jr z, checkAdjustToOneRowHigher    
    jr noPlayerPosAdjustment
    
checkAdjustToOneRowHigher
    ld b, 10
    ld a, (YSpeed)    
    cp 0
    jp nz, noPlayerPosAdjustment
adjustToOneRowHigher    

    ld b, 10    
    ld hl, (currentPlayerLocation) ;; hl is the location to start checking
    ld de, -66
    add hl, de    
    
noPlayerPosAdjustment     
#ifdef DEBUG_FLAGS
    push hl
    ld de, 69
    ld a, (bangedHeadFlag)
    call print_number8bits    
    
    ld de, 73
    ld a, (bangedHeadFlag)
    call print_number8bits   
    pop hl
#endif    
    
    call checkForCollision
    ret 
    
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; checkForGold 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; checkForGold
checkForGold
    xor a
    ld (goldFoundTemp), a
    ld (goldFoundCount), a     
   
    push bc          
    ld b, 8      ; check whole player part of player top row
        push hl
GoldCollectColLoop_1  
            
           ; ld (hl), 8        
            
            ld a, (hl)
            inc hl
            cp NORMAL_TREASURE_CHARACTER
            jr z, incOnlyfoundGold_YES
            cp INVERSE_TREASURE_CHARACTER
            jr z, incOnlyfoundGold_YES            
            jr noGoldFoundBypass                
incOnlyfoundGold_YES              ;; keep a count of gold found
            ld a, (goldFoundTemp)
            inc a
            ld (goldFoundTemp), a
noGoldFoundBypass                         
            djnz GoldCollectColLoop_1          
        pop hl
    pop bc        

    push hl

    ld de, 33             ;; move next write position to next row        
    add hl, de
checkAndGoldCollectRowLoop

       push bc          
         ld b, 2      ; only check left and right edges
         push hl         
GoldCollectColLoop    
            ;ld (hl), 8    
            ld a, (hl)
            ld de, 7
            add hl, de       
            
            cp NORMAL_TREASURE_CHARACTER
            jr z, incOnlyfoundGold_YES_2
            cp INVERSE_TREASURE_CHARACTER
            jr z, incOnlyfoundGold_YES_2
            jr noGoldFoundBypass_2                
incOnlyfoundGold_YES_2              ;; keep a count of gold found
            ld a, (goldFoundTemp)
            inc a
            ld (goldFoundTemp), a
noGoldFoundBypass_2           
            djnz GoldCollectColLoop
        pop hl
        ld de, 33             ;; move next write position to next row        
        add hl, de
      pop bc
      djnz checkAndGoldCollectRowLoop       

   pop hl
   ld de, 297
   add hl, de
   
   push bc          
        ld b, 8      ; check whole part of playerbottom row
        push hl
GoldCollectColLoop_2       
            
            ;ld (hl), 8        
            
            ld a, (hl)            
            inc hl
            cp INVERSE_TREASURE_CHARACTER
            jr z, incOnlyfoundGold_YES_3
            cp NORMAL_TREASURE_CHARACTER
            jr z, incOnlyfoundGold_YES_3            
            jr noGoldFoundBypass_3               
incOnlyfoundGold_YES_3              ;; keep a count of gold found
            ld a, (goldFoundTemp)
            inc a
            ld (goldFoundTemp), a
noGoldFoundBypass_3                        
            djnz GoldCollectColLoop_2          
        pop hl
    pop bc        
  
    ld a, (goldFoundTemp)
    cp 0
    jp nz, foundGold_YES
    jp justPrintScore_GC     
    
   

foundGold_YES
    ld a, (goldFoundTemp)
    ld b, a
incGoldScoreLoop    
    push bc
    ld a, 1
    ld (goldFoundCount),a    ;set this for just below where score gets inc'd
    ld c, 1    
    ld a, (goldFoundInRoom)
    add a, c
    ld (goldFoundInRoom), a
    
    call checkAndClearDoor        
    
    ld a, (goldFoundCount)
    cp 0
    jp z, justPrintScore_GC
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
    pop bc 
    djnz incGoldScoreLoop
justPrintScore_GC
    ld bc, 23
    ld de, score_mem_tens
    call printNumber
    ld bc, 21
    ld de, score_mem_hund
    call printNumber    
    
    ret



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; checkForCollision with ememy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; checkForCollision with ememy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; checkForCollision with ememy
checkForCollision   
    xor a
    ld (hitEnemyRestartRoomFlag), a 
    
    push bc          
        ld b, 4      ; check middle part of player top row
        ;push hl
           inc hl
           inc hl
GoldCollectColLoop_1_CC                  
            ld a, (hl)
            ;ld (hl), 8
            inc hl                        
            cp 1    ; these are all the characters that can be an enemy sprite
                    ; some are commented out because they cause collision when player walks
            jp z, CollisionWithEnemy_CC
            cp 2
            jp z, CollisionWithEnemy_CC
            cp 3
            jp z, CollisionWithEnemy_CC
            cp 4
            jp z, CollisionWithEnemy_CC
            cp 5
            jp z, CollisionWithEnemy_CC
            cp 6
            jp z, CollisionWithEnemy_CC
            cp 7
            jp z, CollisionWithEnemy_CC
            ;cp 129
            ;jp z, CollisionWithEnemy_CC            
            ;cp 130
            ;jp z, CollisionWithEnemy_CC                        
            cp 131
            jp z, CollisionWithEnemy_CC                        
            cp 132
            jp z, CollisionWithEnemy_CC                
            cp 133
            jp z, CollisionWithEnemy_CC        
            cp 134
            jp z, CollisionWithEnemy_CC            
            ;cp 135
            ;jp z, CollisionWithEnemy_CC            
            djnz GoldCollectColLoop_1_CC          
 ;       pop hl
    pop bc        

    ld de, 27
    add hl, de
checkAndGoldCollectRowLoop_CC
    push bc
         ld b, 2      ; only check left and right edges      
GoldCollectColLoop_CC   
            ld a, (hl)
            ;ld (hl), 8
            
            cp 1    ; these are all the characters that can be an enemy sprite
                    ; some are commented out because they cause collision when player walks
            jp z, CollisionWithEnemy_CC
            cp 2
            jp z, CollisionWithEnemy_CC
            cp 3
            jp z, CollisionWithEnemy_CC
            ;cp 4
            ; jp z, CollisionWithEnemy_CC
            ; cp 5
            ; jp z, CollisionWithEnemy_CC
            ; cp 6
            ; jp z, CollisionWithEnemy_CC
            ; cp 7
            ; jp z, CollisionWithEnemy_CC
            
            
            ;cp 129
            ;jp z, CollisionWithEnemy_CC            
            ;cp 130
            ;jp z, CollisionWithEnemy_CC   

            
            cp 131
            jp z, CollisionWithEnemy_CC                        
            cp 132
            jp z, CollisionWithEnemy_CC                
            cp 133
            jp z, CollisionWithEnemy_CC        
            cp 134
            jp z, CollisionWithEnemy_CC            
            ;cp 135
            ;jp z, CollisionWithEnemy_CC            
            ld a, (hl)
            ld de, 7
            add hl, de             
            djnz GoldCollectColLoop_CC
        ld de, 19             ;; move next write position to next row        
        add hl, de
   pop bc   
   djnz checkAndGoldCollectRowLoop_CC    

   ;;; check YSpeed , if > 0 then wipe the line below and don't check for enemy or gold
   ;; YSpeed is only ever non zero when player moving up, not when moving down
   ld a, (YSpeed)
   cp 5 ;; this means we have just jumped so jp blankBottomRowInCheckCollision
   jr z, blankBotInCheckCollision_CC   
   cp 4 ;; this means we have just jumped so jp blankBottomRowInCheckCollision
   jr z, blankBotInCheckCollision_CC   
   cp 3 ;; this means we have just jumped so jp blankBottomRowInCheckCollision
   jr z, blankBotInCheckCollision_CC   
   cp 2 ;; this means we have just jumped so jp blankBottomRowInCheckCollision
   jr z, blankBotInCheckCollision_CC   
   cp 1
   jr z, blankBotInCheckCollision_CC
   cp 0
   jr z, endOfCheckCollision_CC

   

   
   

   
    
   push bc          
        ld b, 4      ; check middle part of player bottom row
        push hl
        inc hl 
        inc hl
GoldCollectColLoop_2_CC  
            
            ld a, (hl)
            ld (hl), 128     
            inc hl               
            cp 133
            jr z, CollisionWithEnemy_CC
            cp 134
            jr z, CollisionWithEnemy_CC            
            cp 5
            jr z, CollisionWithEnemy_CC            
            djnz GoldCollectColLoop_2_CC          
        pop hl
    pop bc        
    jr endOfCheckCollision_CC
    
blankBotInCheckCollision_CC
    ;;; this is the bit that blanks under the player when they jump or land
    ;; only blank middle 4 blocks
    ;ld de, 33
    ;add hl, de
    ld a, 0  ; when debugging set to 8 to see where it would be blanking        
    ld (hl), a
    inc hl
    ld (hl), a
    inc hl
    ld (hl), a
    inc hl
    ld (hl), a
    inc hl
    ld (hl), a
    inc hl
    ld (hl), a   
    inc hl
    ld (hl), a      
    jr endOfCheckCollision_CC     
    
  
CollisionWithEnemy_CC  ; uh oh :--///    
    pop hl  ;; as we jumped out of the loop need to pop these
    pop bc  ;; as we jumped out of the loop need to pop these
 
    ld a, 1
    ld (hitEnemyRestartRoomFlag), a 
    
    ld a, (playerLives)
    dec a
    cp 0
    jr z, gameOverRestart_CC   
    
    ld (playerLives), a
    
    jr endOfCheckCollision_CC
    
gameOverRestart_CC
    ld a, 1
    ld (gameOverRestartFlag),a        
    jr endOfCheckCollision_CC        


endOfCheckCollision_CC    
    ret
    
    
    
drawDeathAndUpdateArrow    
    ld a, (arrowEnabled)
    cp 0
    jr z,noDrawArrow
    
    ld de, (deathArrowPostion)        
    dec de    
    ld a, (deathArrowEndPos)
    cp e
    jr z, checkArrowLSB
    jr updateDeathArrowPos      
checkArrowLSB
    ld a, (deathArrowEndPos+1)
    cp d
    jr z, noDrawArrow

updateDeathArrowPos
    ld (deathArrowPostion), de

drawDeathArrow        
    ld hl, deathArrow
    ld c, 4
    ld b, 3    
    call drawSprite  
    
    ; need to draw a blank after arrow    
    ld de, (deathArrowPostion)   
    ld hl, 4
    add hl, de    
    xor a
    ld (hl), a
    ld de, 33
    add hl, de
    ld (hl), a    
    add hl, de
    ld (hl), a    
    add hl, de
    ld (hl), a     
    ret    ;; retrurn early here
    
noDrawArrow    ;; this just resets the arrow ready for next time it's fired
    ld hl, Display+1
    ld de,ARROW_START
    add hl, de
    ld (deathArrowPostion), hl
    
    ld hl, Display+1
    ld de, ARROW_END
    add hl, de   
    ld (deathArrowEndPos), hl
    xor a
    ld (arrowEnabled), a   
    ret    
    
updateEnemySpritePositions
    ;check the direction then decide which ST or END to compare
   
    ld a, (enemySpriteZeroPos_RATE)
    cp 1
    jp z, checkEvenOddZeroSprite  ; check evenOdd 
    jr afterCheckEvenOddZero
checkEvenOddZeroSprite
    ld a, (evenOddLoopFlag)    ; used for multi rate enemies
    cp 0
    jp z, noUpdateSpriteZero
afterCheckEvenOddZero 
;;;;;;;;;;;;
;;; we've done the checks and we should update the sprite position
;;;;;;;;;;;;   
;   ld hl, 33
;   ld (enemySpriteZeroPos_DIR), hl
;;; check which direction we're going to decide is checking end +'ve or -'ve
;;; horizontal enemies have DIR = 1 or -1 
;;; vertical enemies have DIR = +33 or -33 (this adds or subtracts one row)
    ld hl, (enemySpriteZeroPos_DIR)
    ld a, l
    cp 1                    ;; check the positive direction for horizontal enemy
    jr z, compareEndPos_Z_horiz
    cp 33                   ;; check the positive direction for vertical enemy
    jr z, compareEndPos_Z_vert
    cp -1
    jr z, compareStartPos_Z_horiz
    cp -33
    jr z, compareStartPos_Z_vert
    jp noUpdateSpriteZero  ;; shouldn't hit this !!!
    
compareEndPos_Z_horiz    
    ld hl, (enemySpriteZeroPos_CUR)
    ld de, (enemySpriteZeroPos_END)
    inc hl       ;; test update inc because column update for horizontal enemy
    ld a, h
    cp d
    jp nz, actuallyUpdateEnemy_Z
    ld a, l
    cp e
    jp nz, actuallyUpdateEnemy_Z
    ;; else we have hit the end reverse direction 
    ld hl, -1
    ld (enemySpriteZeroPos_DIR), hl
    jp actuallyUpdateEnemy_Z
compareEndPos_Z_vert    
    ld hl, (enemySpriteZeroPos_CUR)
    ld de, 33  ;; test adding 33 because row update for vertical enemy 
    add hl, de
    ld de, (enemySpriteZeroPos_END)
    
    ld a, h
    cp d
    jp nz, actuallyUpdateEnemy_Z
    ld a, l
    cp e
    jp nz, actuallyUpdateEnemy_Z
    ;; else we have hit the end reverse direction 
    ld hl, -33
    ld (enemySpriteZeroPos_DIR), hl
    jp actuallyUpdateEnemy_Z
    
compareStartPos_Z_horiz
    ld hl, (enemySpriteZeroPos_CUR)
    ld de, (enemySpriteZeroPos_ST)
    dec hl       ;; test update inc because column update for horizontal enemy
    ld a, h
    cp d
    jp nz, actuallyUpdateEnemy_Z
    ld a, l
    cp e
    jp nz, actuallyUpdateEnemy_Z
    ;; else we have hit the end reverse direction 
    ld hl, 1
    ld (enemySpriteZeroPos_DIR), hl
    jp actuallyUpdateEnemy_Z
    
compareStartPos_Z_vert      
    ld hl, (enemySpriteZeroPos_CUR)
    ld de, -33  ;; test adding -33 because row update for vertical enemy 
    add hl, de
    ld de, (enemySpriteZeroPos_ST)
    
    ld a, h
    cp d
    jp nz, actuallyUpdateEnemy_Z
    ld a, l
    cp e
    jp nz, actuallyUpdateEnemy_Z
    ;; else we have hit the end reverse direction 
    ld hl, 33
    ld (enemySpriteZeroPos_DIR), hl
    jp actuallyUpdateEnemy_Z
;;;;
;;;;  this is the code to actually update the sprite position based on 
;;;;  the amount ot add (or -'ve add i.e. subtract!) in enemySpriteZeroPos_DIR
;;;;  and current position (enemySpriteZeroPos_CUR)
actuallyUpdateEnemy_Z
    ld hl, (enemySpriteZeroPos_CUR)
    ld de, (enemySpriteZeroPos_DIR)
    add hl, de
    ld (enemySpriteZeroPos_CUR), hl
    
    ;ld bc, (enemySpriteZeroPos_DIR)     ;; currentPlayerLocation is already offset to
    ;ld de, 70
    ;call print_number16bits
    

noUpdateSpriteZero
;;; update sprite 2



    ;check the direction then decide which ST or END to compare
   
    ld a, (enemySpriteOnePos_RATE)
    cp 1
    jp z, checkEvenOddOneSprite  ; check evenOdd 
    jr afterCheckEvenOddOne
checkEvenOddOneSprite
    ld a, (evenOddLoopFlag)    ; used for multi rate enemies
    cp 0
    jp z, noUpdateSpriteOne
afterCheckEvenOddOne 
;;;;;;;;;;;;
;;; we've done the checks and we should update the sprite position
;;;;;;;;;;;;   
;   ld hl, 33
;   ld (enemySpriteOnePos_DIR), hl
;;; check which direction we're going to decide is checking end +'ve or -'ve
;;; horizontal enemies have DIR = 1 or -1 
;;; vertical enemies have DIR = +33 or -33 (this adds or subtracts one row)
    ld hl, (enemySpriteOnePos_DIR)
    ld a, l
    cp 1                    ;; check the positive direction for horizontal enemy
    jr z, compareEndPosOne_horiz
    cp 33                   ;; check the positive direction for vertical enemy
    jr z, compareEndPosOne_vert
    cp -1
    jr z, compareStartPosOne_horiz
    cp -33
    jr z, compareStartPosOne_vert
    jp noUpdateSpriteOne  ;; shouldn't hit this !!!
    
compareEndPosOne_horiz    
    ld hl, (enemySpriteOnePos_CUR)
    ld de, (enemySpriteOnePos_END)
    inc hl       ;; test update inc because column update for horizontal enemy
    ld a, h
    cp d
    jp nz, actuallyUpdateEnemyOne
    ld a, l
    cp e
    jp nz, actuallyUpdateEnemyOne
    ;; else we have hit the end reverse direction 
    ld hl, -1
    ld (enemySpriteOnePos_DIR), hl
    jp actuallyUpdateEnemyOne
compareEndPosOne_vert    
    ld hl, (enemySpriteOnePos_CUR)
    ld de, 33  ;; test adding 33 because row update for vertical enemy 
    add hl, de
    ld de, (enemySpriteOnePos_END)
    
    ld a, h
    cp d
    jp nz, actuallyUpdateEnemyOne
    ld a, l
    cp e
    jp nz, actuallyUpdateEnemyOne
    ;; else we have hit the end reverse direction 
    ld hl, -33
    ld (enemySpriteOnePos_DIR), hl
    jp actuallyUpdateEnemyOne
    
compareStartPosOne_horiz
    ld hl, (enemySpriteOnePos_CUR)
    ld de, (enemySpriteOnePos_ST)
    dec hl       ;; test update inc because column update for horizontal enemy
    ld a, h
    cp d
    jp nz, actuallyUpdateEnemyOne
    ld a, l
    cp e
    jp nz, actuallyUpdateEnemyOne
    ;; else we have hit the end reverse direction 
    ld hl, 1
    ld (enemySpriteOnePos_DIR), hl
    jp actuallyUpdateEnemyOne
    
compareStartPosOne_vert      
    ld hl, (enemySpriteOnePos_CUR)
    ld de, -33  ;; test adding -33 because row update for vertical enemy 
    add hl, de
    ld de, (enemySpriteOnePos_ST)
    
    ld a, h
    cp d
    jp nz, actuallyUpdateEnemyOne
    ld a, l
    cp e
    jp nz, actuallyUpdateEnemyOne
    ;; else we have hit the end reverse direction 
    ld hl, 33
    ld (enemySpriteOnePos_DIR), hl
    jp actuallyUpdateEnemyOne
;;;;
;;;;  this is the code to actually update the sprite position based on 
;;;;  the amount ot add (or -'ve add i.e. subtract!) in enemySpriteOnePos_DIR
;;;;  and current position (enemySpriteOnePos_CUR)
actuallyUpdateEnemyOne
    ld hl, (enemySpriteOnePos_CUR)
    ld de, (enemySpriteOnePos_DIR)
    add hl, de
    ld (enemySpriteOnePos_CUR), hl




noUpdateSpriteOne
    
    ret


drawEnemySprites
    ld hl, (enemySpritePointerZero)
    ld (TEMP_enemySpritePointer), hl
    ld hl, (enemySpriteZeroPos_CUR)
    ld (TEMP_enemySpritePos_CUR), hl
    ld a, (enemySpriteFrameZero)
    ld (TEMP_enemySpriteFrame), a
    call drawEnemySprite
    ld hl, (TEMP_enemySpritePointer)
    ld (enemySpritePointerZero), hl 
    ld a, (TEMP_enemySpriteFrame)
    ld (enemySpriteFrameZero), a
    


    ld hl, (enemySpritePointerOne)
    ld (TEMP_enemySpritePointer), hl
    ld hl, (enemySpriteOnePos_CUR)
    ld (TEMP_enemySpritePos_CUR), hl
    ld a, (enemySpriteFrameOne)
    ld (TEMP_enemySpriteFrame), a    
    call drawEnemySprite
    ld hl, (TEMP_enemySpritePointer)
    ld (enemySpritePointerOne), hl 
    ld a, (TEMP_enemySpriteFrame)
    ld (enemySpriteFrameOne), a    
    
    ret
    
;; drawEnemySprite 
;;;;;;;;;;;;;    
;; before call set these from config
;; TEMP_enemySpritePointer   
;; TEMP_enemySpritePos_CUR 
;; TEMP_enemySpriteFrame

;; set hl to the enemySpritePointer
;; after call 
;; ld hl, (TEMP_enemySpritePointer)
;; ld (enemySpritePointer_WHATEVER_INSTANCE), hl 
;; ld a, (TEMP_enemySpriteFrame)
;; ld (enemySpriteFrame), a
;; this will save the updated sprite 

drawEnemySprite 
    ;push hl
    ld a, (TEMP_enemySpriteFrame)
    inc a
    cp 4
    jp z, resetEnemySpriteZ 
    
    ld (TEMP_enemySpriteFrame), a
    ld hl, (TEMP_enemySpritePointer)
    ld de,16     ; 4 by 4 blocks
    add hl, de
    ld (TEMP_enemySpritePointer), hl    
    
    jp skipResetEnemySpriteZ     
resetEnemySpriteZ    
    xor a
    ld (TEMP_enemySpriteFrame), a
    ;pop hl   ;; hl contains the enemySprite address at start
    
    ld de, -48
    ld hl, (TEMP_enemySpritePointer)
    add hl, de
    
    ;;ld hl, enemySpriteZero
    ld (TEMP_enemySpritePointer), hl
    jr drawEnemyAfterPopHL
    
skipResetEnemySpriteZ         
    ;pop hl  ; have to pop here if didn't jump to resetEnemySpriteZ to maintain stack
drawEnemyAfterPopHL
    ld de, (TEMP_enemySpritePos_CUR)
    ld hl, (TEMP_enemySpritePointer)
    ld b, 4
    ld c, 4
    call drawSprite         
    
    ret    
    
    
blankEnemySprites    
    ld hl, (enemySpriteZeroPos_DIR)
    ld a, l
    cp 1                    ;; check the positive direction for horizontal enemy
    jr z, useHorizSpritePtr
    cp 33                   ;; check the positive direction for vertical enemy
    jr z, useVertSpritePtr
    cp -1
    jr z, useHorizSpritePtr
    cp -33
    jr z, useVertSpritePtr
    jp useHorizSpritePtr  ; default to horiz
    
useHorizSpritePtr    
    ld de, (enemySpriteZeroPos_CUR)
    dec de
    ld hl, (enemySprite4by4BlankPointer)
    ld b, 4
    ld c, 6
    call drawSprite    
    jr updateBlankSpriteOne
useVertSpritePtr
    ld de, (enemySpriteZeroPos_CUR)
    ld hl, -33
    add hl, de 
    ex de, hl
    ld hl, (enemySprite4by4BlankPointer)
    ld b, 6
    ld c, 4
    call drawSprite    


updateBlankSpriteOne
    ld hl, (enemySpriteOnePos_DIR)
    ld a, l
    cp 1                    ;; check the positive direction for horizontal enemy
    jr z, useHorizSpritePtr_one
    cp 33                   ;; check the positive direction for vertical enemy
    jr z, useVertSpritePtr_one
    cp -1
    jr z, useHorizSpritePtr_one
    cp -33
    jr z, useVertSpritePtr_one
    jp useHorizSpritePtr_one  ; default to horiz
    
useHorizSpritePtr_one    
    ld de, (enemySpriteOnePos_CUR)
    dec de
    ld hl, (enemySprite4by4BlankPointer)
    ld b, 4
    ld c, 6
    call drawSprite        
    jr endOfDrawBlank
useVertSpritePtr_one
    ld de, (enemySpriteOnePos_CUR)
    ld hl, -33
    add hl, de 
    ex de, hl
    ld hl, (enemySprite4by4BlankPointer)
    ld b, 6
    ld c, 4
    call drawSprite        



endOfDrawBlank    
    ret 
    
    
initialiseEnemysForRoom
   
    xor a    ; fastest and smallest way to clear a register
    ld (enemySpriteFrameZero),a 
    ld (enemySpriteFrameOne),a

#ifdef DEBUG_START_IN_ROOM_X    
    ld a, DEBUG_ROOM_TO_START_IN
    ld (currentRoom), a
    daa
    ld (currentRoomDisplay), a        
#endif    
    
    
    ld hl, RoomConfig
    ld a, (currentRoom)    
    cp 0
    jp z, skipCalcualteRoomCOnfig_E   
    ld de, SIZE_OF_ROOM_CONFIG ;; should use this but doesn't work: (Room_2_Config - Room_1_Config) 
    ld b,a       
drawRoomCalcOffsetToRoom_E    
    ;;; repeatedly add SIZE_OF_ROOM_CONFIG offset to get next room
    add hl, de 
    djnz drawRoomCalcOffsetToRoom_E
skipCalcualteRoomCOnfig_E
    ld (RoomConfigAddress), hl
    
    ld de, (RoomConfigAddress)
    ld hl, OFFSET_TO_ENEMY_SPRITES
    add hl, de
        
#ifdef DEBUG_SPRITE_ADDRESS
    push hl
    push hl 
    pop bc
    ld de, 68
    call print_number16bits
    pop hl 
    
    ld a, (currentRoom)
    ld de, 73
    call print_number8bits
hardLoop
    jp hardLoop
#endif         
    
    
    
    push hl    
        ld e, (hl)                  ; load the low byte of the address into register e
        inc hl                       ; increment hl to point to the high byte of the address
        ld d, (hl)                   ; load the high byte of the address into register d
        ld hl, Display+1 
        add hl, de 
        ld (enemySpriteZeroPos_ST), hl
    pop hl
    inc hl
    inc hl

    push hl    
        ld e, (hl)                   ; load the low byte of the address into register e
        inc hl                       ; increment hl to point to the high byte of the address
        ld d, (hl)                   ; load the high byte of the address into register d
        ld hl, Display+1 
        add hl, de 
        ld (enemySpriteOnePos_ST), hl    
    pop hl
    inc hl
    inc hl
    

    push hl    
        ld e, (hl)                   ; load the low byte of the address into register e
        inc hl                       ; increment hl to point to the high byte of the address
        ld d, (hl)                   ; load the high byte of the address into register d
        ld hl, Display+1 
        add hl, de 
    ld (enemySpriteZeroPos_END), hl
    pop hl
    inc hl
    inc hl

    push hl    
        ld e, (hl)                   ; load the low byte of the address into register e
        inc hl                       ; increment hl to point to the high byte of the address
        ld d, (hl)                   ; load the high byte of the address into register d
        ld hl, Display+1 
        add hl, de 
        ld (enemySpriteOnePos_END), hl    
    pop hl
    inc hl
    inc hl 


    push hl    
        ld e, (hl)                   ; load the low byte of the address into register e
        inc hl                       ; increment hl to point to the high byte of the address
        ld d, (hl)                   ; load the high byte of the address into register d
        ld hl, Display+1 
        add hl, de 
        ld (enemySpriteZeroPos_CUR), hl
    pop hl
    inc hl
    inc hl

    push hl    
        ld e, (hl)                   ; load the low byte of the address into register e
        inc hl                       ; increment hl to point to the high byte of the address
        ld d, (hl)                   ; load the high byte of the address into register d
        ld hl, Display+1 
        add hl, de 
    ld (enemySpriteOnePos_CUR), hl    
    pop hl
    inc hl
    inc hl       

    ;; not from config yet
    push hl 
        ld e, (hl)                   ; load the low byte of the address into register e
        inc hl                       ; increment hl to point to the high byte of the address
        ld d, (hl)                   ; load the high byte of the address into register d
        ld (enemySpriteZeroPos_DIR), de        
    pop hl 
    inc hl
    inc hl
    push hl 
        ld e, (hl)                   ; load the low byte of the address into register e
        inc hl                       ; increment hl to point to the high byte of the address
        ld d, (hl)                   ; load the high byte of the address into register d    
        ld (enemySpriteOnePos_DIR), de
    pop hl

    inc hl
    inc hl     
    
    ld a, (hl)
    ;ld a, 1
    ld (enemySpriteZeroPos_RATE), a
    inc hl
    ld a, (hl)
    ld (enemySpriteOnePos_RATE), a
    inc hl
    ;; the next two 16bit addreses are pointers to the sprite data

    ;ld hl, enemySpriteZero
    ;ld (enemySpritePointerZero), hl
    ;ld hl, enemySpriteOne
    ;ld (enemySpritePointerOne), hl 
    
    push hl    
        ld e, (hl)                   ; load the low byte of the address into register e
        inc hl                       ; increment hl to point to the high byte of the address
        ld d, (hl)                   ; load the high byte of the address into register d
        ld (enemySpritePointerZero), de
    pop hl
    inc hl
    inc hl     

    push hl    
        ld e, (hl)                   ; load the low byte of the address into register e
        inc hl                       ; increment hl to point to the high byte of the address
        ld d, (hl)                   ; load the high byte of the address into register d
        ld (enemySpritePointerOne), de
    pop hl
    inc hl
    inc hl     

;; orientation of enemy movement 0 = horizontal 1 = vertica


;;; not needed now use enemySpriteZeroPos_DIR=1 -1 (horiz)  or 33 -33 (vertical
    ld a, (hl)
    ld (enemySpriteZero_HorizVert), a
    inc hl
    ld a, (hl)    
    ld (enemySpriteOne_HorizVert), a
    
        
    
    ret
    
setPlayerStartPosition

    ld hl, RoomConfig
    ld a, (currentRoom)    
    cp 0
    jp z, skipCalcualteRoomCon_ST
    ld de, SIZE_OF_ROOM_CONFIG ;; should use this but doesn't work: (Room_2_Config - Room_1_Config) 
    ld b,a       
setPlayerCalcOffsetToRoom_E    
    ;;; repeatedly add SIZE_OF_ROOM_CONFIG offset to get next room
    add hl, de 
    djnz setPlayerCalcOffsetToRoom_E
skipCalcualteRoomCon_ST
    ld (RoomConfigAddress), hl
    
    ld de, (RoomConfigAddress)
    ld hl, OFFSET_TO_PLAYER_X_Y
    add hl, de
    
    ld a, (hl)
    ld (playerXPos), a
    inc hl 
    ld a, (hl)
    ld (playerYPos), a   
    
    inc hl 
    ld e, (hl)                  ; load the low byte of the address into register e
    inc hl                       ; increment hl to point to the high byte of the address
    ld d, (hl)                   ; load the high byte of the address into register d
    ld hl, Display+1 
    add hl, de 
    ld (currentPlayerLocation), hl
    
    ; ld a, INITIAL_PLAYER_X
    ; ld (playerXPos), a
    ; ld a, INITIAL_PLAYER_Y
    ; ld (playerYPos), a      ; this is the position above the bottom so 0 is the bottom most    
    ; ld de, INITIAL_PLAYER_OFFSET    
    ; ld hl, Display+1 
    ; add hl, de    
    ; ld (currentPlayerLocation), hl
    ret
    
gameOverDeathScene     
    ld hl, playerSpriteDeathSequence
    ld (deadPlayerSpritePointer), hl
    ld b,19  ; we have 20 frames in the player sprite that died     
laidToRestLoop
    push bc	                  
        ld de, 342    
        ld hl, Display+1
        add hl, de               
        push hl
        pop de
        ld hl, (deadPlayerSpritePointer)
        ld c, 8
        ld b, 8    
        call drawSprite             
        
        ld hl, (deadPlayerSpritePointer)   
        ld de,64     ; 8 by 8 blocks
        add hl, de
        ld (deadPlayerSpritePointer), hl                

          
        ld b, 255
delayLoopDeathScene            
        push bc
            ld b, 32
delayLoopDeathScene_2

            djnz delayLoopDeathScene_2                            
        pop bc
        djnz delayLoopDeathScene
        ld a, 1
        ld (arrowEnabled), a
        call drawDeathAndUpdateArrow   
    pop bc 
    
    djnz laidToRestLoop
    

        ld b, 255
delayLoopDeathScene_3            
        push bc
            ld b, 128
delayLoopDeathScene_4

            djnz delayLoopDeathScene_4
        pop bc
        djnz delayLoopDeathScene_3
    
    call CLS
    ret

playerWonDoDanceMoves    
    call CLS
    ld a, 1
    ld a, (danceLoop8BitCount)
    
    ld bc, 70    
    ld de, YouWonText
    
    call printstring
    
    ld hl, 199
    ld (dancePos), hl
    
    ld b, 64    
    
daneceLoop
    push bc
        ld b,10
danceMoves_1
        push bc	
            ld de, (dancePos)    
            ld hl, Display+1 
            add hl, de        
            ex de, hl
            ld hl, playerSpriteRightMove
            ld c, 8
            ld b, 8    
            call drawSprite   
        pop bc
        djnz danceMoves_1
        ld b, 10
danceMoves_2
        push bc
            ld de, (dancePos)
            ld hl, Display+1 
            add hl, de        
            ex de, hl
            ld hl, playerSpriteRightMove
            ld c, 8
            ld b, 8    
            call drawSprite   
        pop bc
        djnz danceMoves_2

    ld a, (danceLoop8BitCount)
    cp 10
    jp z, reverseDance
    jp forwardDanceMove
reverseDance    
    dec a
    ld (danceLoop8BitCount), a
    
    ld hl, (dancePos)
    dec hl
    ld (dancePos), hl    
    jp danceCheckLoop
forwardDanceMove    
    inc a
    ld (danceLoop8BitCount), a
    
    ld hl, (dancePos)
    inc hl
    ld (dancePos), hl
danceCheckLoop    
    pop bc        
    djnz daneceLoop

    pop hl  ; the thing that jumped here was in a subroutine that was called so pop stack
    ;; no ret, we jump to restart game 
    jp intro_title		


printTime    
    ld bc, 55 
    ld de, TimeText
    call printstring
    
    ld a, (gameTime_Seconds)
    ld de, 62    
    call print_number8bits    
    
    ld de, 61
    ld hl, (DF_CC)    
    add hl, de  
    ld a, _CL
    ld (hl), a
    
    ld a, (gameTime_Minutes)
    ld de, 59    
    call print_number8bits       
    
    ;; if minutes = 10 and seconds = 00 then decrease lives by 1 
    ld a, (gameTime_Minutes)
    cp $10      ; remember these time variables ar daa'd so are Binary Coded Decimal 
    jp z, checkMintesAreZero
    jp endOfPrintTimeRoutine    
checkMintesAreZero
    ld a, (gameTime_Seconds)
    cp 0
    jr z, decreaseLivesByOne
    jr endOfPrintTimeRoutine  
decreaseLivesByOne     
    ld a, (playerLives)
    ld a, 1     ; actually set lives to one !! a bit cruel but we don't want it too easy lol
    ld (playerLives), a
    
    ld a, 1
    ld (gameTime_Seconds), a
    
endOfPrintTimeRoutine
    ret
    
printLives
    ld bc, 46
    ld de, LivesText
    call printstring
    
    ld a, (playerLives)
    ld de, 51    
    call print_number8bits        
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
    
    ld a, (gameTimeCounterJIFFIES)
    inc a
    cp 50    
    jr z, skipJIFFIESUpdate
    
    ld (gameTimeCounterJIFFIES), a
    
    jr endOfVsync
    
skipJIFFIESUpdate
    xor a
    ld (gameTimeCounterJIFFIES), a  ; reset JIFFIES
    ld a, (gameTime_Seconds)   ;; increment seconds
    inc a
    daa
    cp $60
    jr z, updateMinutes
    
    ld (gameTime_Seconds), a
    jr endOfVsync
updateMinutes    
    xor a
    ld (gameTime_Seconds), a
    ld a, (gameTime_Minutes)    
    inc a
    daa
    ld (gameTime_Minutes), a
    
endOfVsync        
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

;; this uses 1024 bytes of memory but given we have a hughe 16Kb there should be room 
;; for more rooms
playerSpriteDeathSequence
	  DEFB	    $00, $00, $00, $81, $80, $04, $00, $00, $00, $00, $00, $80,
	  DEFB      $82, $07, $00, $00, $04, $00, $00, $85, $07, $00, $00, $87,
	  DEFB	    $02, $03, $03, $80, $80, $03, $03, $01, $00, $00, $00, $80,
	  DEFB	    $80, $00, $00, $00, $00, $00, $00, $07, $84, $00, $00, $00,
	  DEFB	    $00, $00, $00, $05, $85, $00, $00, $00, $00, $00, $87, $05,
	  DEFB		$85, $04, $00, $00, $00, $00, $00, $81, $80, $04, $00, $00,
	  DEFB  	$00, $00, $00, $80, $82, $07, $00, $00, $00, $00, $00, $85,
	  DEFB		$07, $00, $00, $00, $00, $87, $06, $80, $80, $86, $04, $00,
	  DEFB		$02, $01, $00, $80, $80, $00, $02, $01, $00, $00, $00, $07,
	  DEFB		$84, $00, $00, $00, $00, $00, $00, $05, $85, $00, $00, $00,
	  DEFB		$00, $00, $87, $05, $85, $04, $00, $00, $00, $00, $00, $81,
	  DEFB  	$80, $04, $00, $00, $00, $00, $00, $80, $82, $07, $00, $00,
	  DEFB		$00, $00, $00, $85, $07, $00, $00, $00, $00, $00, $06, $80,
	  DEFB		$80, $86, $00, $00, $00, $87, $01, $80, $80, $02, $04, $00,
	  DEFB		$00, $00, $00, $07, $84, $00, $00, $00, $00, $00, $00, $05,
	  DEFB		$85, $00, $00, $00, $00, $00, $87, $05, $85, $04, $00, $00,
	  DEFB	    $00, $00, $00, $81, $80, $04, $00, $00, $00, $00, $00, $80,
	  DEFB      $82, $07, $00, $00, $00, $00, $00, $85, $07, $00, $00, $00,
	  DEFB	    $00, $00, $06, $80, $80, $86, $00, $00, $00, $00, $05, $80,
	  DEFB	    $80, $85, $00, $00, $00, $00, $01, $07, $84, $02, $00, $00,
	  DEFB	    $00, $00, $00, $05, $85, $00, $00, $00, $00, $00, $87, $05,
	  DEFB		$85, $04, $00, $00, $00, $00, $00, $81, $80, $04, $00, $00,
	  DEFB  	$00, $00, $00, $80, $82, $07, $00, $00, $00, $00, $00, $85,
	  DEFB		$07, $00, $00, $00, $00, $00, $06, $80, $80, $86, $00, $00,
	  DEFB		$00, $00, $05, $80, $80, $85, $00, $00, $00, $00, $01, $07,
	  DEFB		$84, $02, $00, $00, $00, $00, $00, $05, $85, $00, $00, $00,
	  DEFB		$00, $00, $87, $05, $85, $04, $00, $00, $00, $00, $00, $87,
	  DEFB  	$83, $00, $00, $00, $00, $00, $00, $80, $07, $82, $00, $00,
	  DEFB		$00, $00, $00, $84, $80, $01, $00, $00, $00, $00, $87, $81,
	  DEFB		$82, $04, $00, $00, $00, $00, $05, $80, $80, $85, $00, $00,
	  DEFB		$00, $00, $05, $80, $80, $85, $00, $00, $00, $00, $00, $05,
	  DEFB		$85, $00, $00, $00, $00, $00, $00, $05, $85, $00, $00, $00,
	  DEFB	    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $81,
	  DEFB      $80, $04, $00, $00, $00, $00, $00, $80, $80, $07, $00, $00,
	  DEFB	    $00, $00, $00, $85, $07, $00, $00, $00, $00, $00, $06, $80,
	  DEFB	    $80, $86, $00, $00, $00, $00, $05, $80, $80, $85, $00, $00,
	  DEFB	    $00, $00, $01, $07, $84, $02, $00, $00, $00, $00, $00, $05,
	  DEFB		$85, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB  	$00, $00, $00, $87, $83, $00, $00, $00, $00, $00, $00, $80,
	  DEFB		$07, $82, $00, $00, $00, $00, $00, $84, $80, $01, $00, $00,
	  DEFB		$00, $00, $87, $81, $82, $04, $00, $00, $00, $00, $05, $80,
	  DEFB		$80, $85, $00, $00, $00, $00, $05, $80, $80, $85, $00, $00,
	  DEFB		$00, $00, $00, $05, $85, $00, $00, $00, $00, $00, $00, $00,
	  DEFB  	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB		$00, $00, $00, $81, $80, $04, $00, $00, $00, $00, $00, $80,
	  DEFB		$80, $07, $00, $00, $00, $00, $00, $85, $07, $00, $00, $00,
	  DEFB		$00, $00, $06, $80, $80, $86, $00, $00, $00, $00, $05, $80,
	  DEFB		$80, $85, $00, $00, $00, $00, $01, $07, $84, $02, $00, $00,
	  DEFB	    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB      $00, $00, $00, $00, $00, $00, $00, $87, $83, $00, $00, $00,
	  DEFB	    $00, $00, $00, $80, $07, $82, $00, $00, $00, $00, $00, $84,
	  DEFB	    $80, $01, $00, $00, $00, $00, $87, $81, $82, $04, $00, $00,
	  DEFB	    $00, $00, $05, $80, $80, $85, $00, $00, $00, $00, $05, $80,
	  DEFB		$80, $85, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB  	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB		$00, $00, $00, $00, $00, $00, $00, $81, $80, $04, $00, $00,
	  DEFB		$00, $00, $00, $80, $80, $07, $00, $00, $00, $00, $00, $85,
	  DEFB		$07, $00, $00, $00, $00, $00, $06, $80, $80, $86, $00, $00,
	  DEFB		$00, $00, $05, $80, $80, $85, $00, $00, $00, $00, $00, $00,
	  DEFB  	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB		$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $87,
	  DEFB		$83, $00, $00, $00, $00, $00, $00, $80, $07, $82, $00, $00,
	  DEFB		$00, $00, $00, $84, $80, $01, $00, $00, $00, $00, $87, $81,
	  DEFB		$82, $04, $00, $00, $00, $00, $05, $80, $80, $85, $00, $00,
	  DEFB	    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB	    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $81,
	  DEFB	    $80, $04, $00, $00, $00, $00, $00, $80, $80, $07, $00, $00,
	  DEFB	    $00, $00, $00, $85, $07, $00, $00, $00, $00, $00, $06, $80,
	  DEFB		$80, $86, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB  	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB		$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB		$00, $00, $00, $87, $83, $00, $00, $00, $00, $00, $00, $80,
	  DEFB		$07, $82, $00, $00, $00, $00, $00, $84, $80, $01, $00, $00,
	  DEFB		$00, $00, $87, $81, $82, $04, $00, $00, $00, $00, $00, $00,
	  DEFB  	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB		$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB		$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB		$00, $00, $00, $81, $80, $04, $00, $00, $00, $00, $00, $80,
	  DEFB		$80, $07, $00, $00, $00, $00, $00, $85, $07, $00, $00, $00,
	  DEFB	    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB	    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB	    $00, $00, $00, $00, $00, $00, $00, $87, $83, $00, $00, $00,
	  DEFB	    $00, $00, $00, $80, $07, $82, $00, $00, $00, $00, $00, $84,
	  DEFB		$80, $01, $00, $00, $07, $03, $04, $84, $07, $85, $03, $86,
	  DEFB  	$05, $87, $01, $85, $05, $85, $00, $85, $07, $86, $00, $85,
	  DEFB		$05, $85, $03, $01, $05, $00, $05, $81, $82, $85, $00, $00,
	  DEFB		$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB		$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB		$00, $00, $00, $00, $00, $00, $00, $00, $07, $03, $04, $84,
	  DEFB  	$07, $85, $03, $86, $05, $87, $01, $85, $05, $85, $00, $85,
	  DEFB		$07, $86, $00, $85, $05, $85, $03, $01, $05, $00, $05, $81,
	  DEFB		$82, $85, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB		$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB		$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB	    $07, $03, $04, $84, $07, $85, $03, $86, $05, $87, $01, $85,
	  DEFB      $05, $85, $00, $85, $07, $86, $00, $85, $05, $85, $03, $01,
	  DEFB	    $05, $00, $05, $81, $82, $85, $00, $00, $00, $00, $00, $00,
	  DEFB	    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB	    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB		$00, $00, $00, $00, $07, $03, $04, $84, $07, $85, $03, $86,
	  DEFB  	$05, $87, $01, $85, $05, $85, $00, $85, $07, $86, $00, $85,
	  DEFB		$05, $85, $03, $01, $05, $00, $05, $81, $82, $85, $00, $00,
	  DEFB		$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB		$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	  DEFB		$00, $00, $00, $00, $00, $00, $00, $00
    jp intro_title  ;; protection for memory overwrite
    jp intro_title  ;; protection for memory overwrite
    jp intro_title  ;; protection for memory overwrite
    
   
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

enemySpriteZeroPos_ST  
    DEFW 0
enemySpriteOnePos_ST    
    DEFW 0
enemySpriteZeroPos_END
    DEFW 0
enemySpriteOnePos_END   
    DEFW 0
enemySpriteZeroPos_DIR
    DEFW 0
enemySpriteOnePos_DIR  
    DEFW 0
enemySpriteZeroPos_CUR
    DEFW 0
enemySpriteOnePos_CUR
    DEFW 0
enemySpriteZeroPos_RATE
    DEFB 0
enemySpriteOnePos_RATE
    DEFB 0    
TEMP_enemySpritePointer
    DEFW 0
TEMP_enemySpritePos_CUR
    DEFW 0
enemySpriteZero_HorizVert
    DEFB 0
enemySpriteOne_HorizVert    
    DEFB 0
TEMP_enemySpriteFrame
    DEFB 0
enemySpriteFrameZero
    DEFB 0
enemySpriteFrameOne    
    DEFB 0
treasureCharacter
    DEFB NORMAL_TREASURE_CHARACTER
enemySprites   ;; keeping these to 4*4 for speed and size
enemySprite4by4BlankPointer
    DEFW 0
enemySprite4by4Blank
    DEFB 0, 0, 0 ,0, 0, 0
    DEFB 0, 0, 0 ,0, 0, 0
    DEFB 0, 0, 0 ,0, 0, 0
    DEFB 0, 0, 0 ,0, 0, 0
    DEFB 0, 0, 0 ,0, 0, 0 
deadPlayerSpritePointer
    DEFW 0

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
    
enemySpriteTwo
	DEFB $03, $84, $07, $03, $00, $85, $05, $00, $00, $85, $05, $00,
	DEFB $83, $81, $82, $83, $83, $83, $83, $83, $00, $85, $05, $00,
	DEFB $00, $85, $05, $00, $03, $03, $03, $03, $00, $00, $00, $00,
	DEFB $03, $84, $07, $03, $83, $81, $82, $83, $00, $00, $00, $00,
	DEFB $00, $00, $00, $00, $83, $83, $83, $83, $03, $03, $03, $03,
	DEFB $00, $00, $00, $00    
    
enemySpriteThree
	DEFB $00, $00, $00, $00, $00, $00, $00, $00, $06, $03, $03, $86,
	DEFB $82, $00, $00, $81, $00, $00, $00, $00, $00, $00, $00, $00,
	DEFB $87, $03, $03, $04, $85, $04, $87, $05, $00, $00, $00, $00,
	DEFB $00, $00, $00, $00, $00, $06, $86, $00, $00, $05, $85, $00,
	DEFB $00, $00, $00, $00, $00, $00, $00, $00, $00, $87, $04, $00,
	DEFB $00, $02, $01, $00
enemySpriteFour
	DEFB $05, $00, $00, $85, $82, $83, $83, $81, $07, $03, $03, $84,
	DEFB $05, $00, $00, $85, $87, $00, $00, $04, $85, $83, $83, $05,
	DEFB $85, $03, $03, $05, $02, $00, $00, $01, $00, $00, $00, $00,
	DEFB $00, $82, $81, $00, $00, $07, $84, $00, $00, $00, $00, $00,
	DEFB $00, $00, $00, $00, $00, $87, $04, $00, $00, $02, $01, $00,
	DEFB $00, $00, $00, $00
enemySpriteFive    
	DEFB $00, $85, $05, $00, $87, $81, $82, $04, $81, $04, $87, $82,
	DEFB $82, $05, $85, $81, $00, $87, $04, $00, $00, $81, $82, $00,
	DEFB $87, $82, $81, $04, $85, $81, $82, $05, $01, $02, $01, $02,
	DEFB $00, $81, $82, $00, $00, $82, $81, $00, $00, $82, $81, $00,
	DEFB $00, $87, $04, $00, $00, $81, $82, $00, $87, $82, $81, $04,
	DEFB $85, $81, $82, $05    
enemySpriteSix    
	DEFB $03, $84, $07, $03, $87, $81, $82, $04, $81, $00, $00, $82,
	DEFB $82, $00, $00, $81, $00, $87, $04, $00, $00, $81, $82, $00,
	DEFB $87, $05, $85, $04, $85, $04, $87, $05, $03, $84, $07, $03,
	DEFB $00, $81, $82, $00, $00, $82, $81, $00, $00, $82, $81, $00,
	DEFB $00, $87, $04, $00, $00, $81, $82, $00, $87, $82, $81, $04,
	DEFB $85, $81, $82, $05
enemySpriteSeven    
	DEFB $05, $85, $80, $00, $07, $83, $82, $83, $05, $85, $80, $00,
	DEFB $00, $85, $85, $00, $04, $85, $80, $00, $82, $83, $82, $06,
	DEFB $05, $85, $80, $00, $01, $81, $85, $04, $00, $85, $80, $00,
	DEFB $05, $83, $82, $83, $07, $85, $80, $00, $05, $85, $85, $00,
	DEFB $00, $85, $80, $00, $05, $83, $82, $83, $07, $85, $80, $00,
	DEFB $05, $81, $85, $04    
enemySpriteEight	
	DEFB $00, $81, $82, $00, $81, $86, $06, $82, $84, $06, $86, $07,
	DEFB $00, $84, $07, $00, $00, $81, $82, $00, $81, $01, $02, $82,
	DEFB $84, $04, $87, $07, $00, $84, $07, $00, $00, $06, $86, $00,
	DEFB $06, $00, $00, $86, $86, $00, $00, $06, $00, $86, $06, $00,
	DEFB $00, $06, $86, $00, $06, $87, $00, $86, $86, $00, $01, $06,
	DEFB $00, $86, $06, $00

;4 by 3 poisoned arrow
deathArrow
	DEFB $87, $00, $00, $06, 
    DEFB $84, $03, $03, $07, 
    DEFB $00, $00, $00, $02
    
gameTime_Seconds
    DEFB 0 
gameTime_Minutes
    DEFB 0
gameTimeCounterJIFFIES  ; a nod to C64 1/60th of a second count reset at 60.
    DEFB 0
    
TimeText
    DEFB _T,_I,_M,_E,_EQ,$ff
LivesText
    DEFB _L,_I,_V,_E,_S,_EQ,$ff    
TopLineText
    DEFB _J,_U,_M,_P, 136, _R, _O, _O, _M, 0, 28, 28, 0,136,136, _G, _O, _L, _D, 28, 28, 0,136, 136, 136,_B,_Y,_T,_E,32,$ff    
moveRoomDebugTest
    DEFB _M,_O,_V,_E,_R,_O,_O,_M,$ff
moveRoomDebugFlagText
    DEFB _F,_L,_A,_G, $ff    
YouWonText
    DEFB _Y,_O,_U,__,_W,_O,_N,__,_N,_O,_W,__,__,_D,_A,_N,_C,_E,_CL,_MI,_CP,_CP,_CP,$ff    


title_screen_txt
	DEFB	_Z,_X,_8,_1,__,_J,_U,_M,_P,$ff
keys_screen_txt_1
	DEFB	_S,__,_T,_O,__,_S,_T,_A,_R,_T,26,__,_O,__,_L,_E,_F,_T,26,__,_P,__,_R,_I,_G,_H,_T,$ff
keys_screen_txt_2
	DEFB	__,__,__,__,__,__,__,_Z,__,_O,_R,__,_S,_P,_A,_C,_E,__,_EQ,__,_J,_U,_M,_P,,$ff    

game_objective_txt
	DEFB	_T,_O,__,_W,_I,_N,__,_C,_O,_L,_L,_E,_C,_T,__, _A,_L,_L,__,_G,_O,_L,_D,$ff
	
last_Score_txt
	DEFB 21,21,21,21,_L,_A,_S,_T,__,__,_S,_C,_O,_R,_E,21,21,21,21,$ff	
high_Score_txt
	DEFB 21,21,21,21,_H,_I,_G,_H,__,__,_S,_C,_O,_R,_E,21,21,21,21,$ff		
credits_and_version_1
	DEFB __,_B,_Y,__,_A,__,_P,_I,_L,_K,_I,_N,_G,_T,_O,_N,__, _2,_0,_2,_4,$ff
credits_and_version_2
	DEFB __,__,_V,_E,_R,_S,_I,_O,_N,__,_V,_1,_DT,_5,$ff    
credits_and_version_3
	DEFB __,__,__,_Y,_O,_U,_T,_U,_B,_E,_CL, _B,_Y,_T,_E,_F,_O,_R,_E,_V,_E,_R,$ff       
    
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
bangedHeadFlag
    DEFB 0
landPlayerFlag
    DEFB 0
score_mem_tens
    DEFB 0
score_mem_hund
    DEFB 0
last_score_mem_tens
    DEFB 0
last_score_mem_hund
    DEFB 0    
DEBUG_DUMMY_VAR_1  
    DEFW 0,0,0,0,0
currentRoom
    DEFB 0
currentRoomDisplay
    DEFB 0    
DEBUG_DUMMY_VAR_2 
    DEFW 0,0,0,0,0
dancePos
    DEFW 0 
danceLoop8BitCount    
    DEFB 0
roomJustEnteredFlag
    DEFB 0
hitEnemyRestartRoomFlag
    DEFB 0
gameOverRestartFlag    
    DEFB 0
groundPlatFlag
    DEFB 0
justJumpFlag
    DEFB 0
goldFoundCount
    DEFB 0
goldToggleControl
    DEFB 0
goldFoundTemp    
    DEFB 0
playerLives
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
evenOddLoopFlag    
    DEFB 0
evenOddLoopCount    
    DEFB 0
deathArrowPostion
    DEFW 0
deathArrowEndPos
    DEFW 0
arrowEnabled
    DEFB 0
noJumpInNextGameLoopFlag
    DEFB 0
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
Room_1_Config
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
    
    DEFB 1    ; 1 = enabled 0 = disabled  (byte16)
    DEFW 610  ; start of platform   17,18
    DEFB 7    ; length   19
    
    DEFB 1    ; 1 = enabled 0 = disabled  
    DEFW 454  ; start of platform  21,22
    DEFB 6    ; length  23
    
    DEFB 1    ; 1 = enabled 0 = disabled  
    DEFW 364  ; start of platform  25,26
    DEFB 5    ; length             (byte 27)

    DEFB 1    ; 1 = enabled 0 = disabled  
    DEFW 529  ; start of platform  25,26
    DEFB 5    ; length             (byte 27)

    DEFB 1    ; 1 = enabled 0 = disabled  
    DEFW 661  ; start of platform  25,26
    DEFB 5    ; length             (byte 27)        
startOfRoom1Treasure      
    ;;; tokens 2 bytes each
    DEFW 169  ; treasure token offset from DF_CC   always 4 treasure (byte 28)    
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 170  ; treasure token offset from DF_CC  307
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 422  ; treasure token offset from DF_CC    
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 722  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    ;; enemy definition gets loaded into these when room entered
firstEnemyAddress      ;;  36 bytes   
    DEFW 270  ; enemySpriteZeroPos_ST 
    DEFW 108  ; enemySpriteOnePos_ST  
    DEFW 600  ; enemySpriteZeroPos_END
    DEFW 126  ; enemySpriteOnePos_END 
    ;DEFW 111  ; enemySpriteOnePos_END 
    DEFW 435  ; enemySpriteZeroPos_CUR
    DEFW 109  ; enemySpriteOnePos_CUR 
    DEFW 33    ; enemySpriteZeroPos_DIR
    DEFW 1    ; enemySpriteOnePos_DIR 
    DEFB 1    ; enemy 0 full rate enemy = 0; slow rate = 1
    DEFB 1    ; enemy 1 full rate enemy = 0; slow  rate = 1    
    DEFW enemySpriteZero
    DEFW enemySpriteOne
    DEFB  0  ; enemy zero orientation horizontal = 0 vertical = 1
    DEFB  0  ; enemy one orientation horizontal = 0 vertical = 1    
offsetToPlayerXY
    ; DEFB 5        ;; X position left most is zero
    ; DEFB 3        ;; Y position bottom is 0
    ; DEFW 467      ;; screen memory offset
    DEFB 17        ;; X position left most is zero
    DEFB 7        ;; Y position bottom is 0
    DEFW 347      ;; screen memory offset    
RoomZeroName    
    DEFB _C,_E,_N,_T,0,_C,_A,_V,_QM,0,$ff
    
    
Room_2_Config    
    DEFB 1    ; room ID
    ;;; DOORS  * 3 max enabled  
    DEFB 1    ; Door orientation east=1  0= door disabled
    DEFW 297   ; offset from DF_CC to top of door
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
    
    DEFB 1    ; 1 = enabled 0 = disabled  
    DEFW 560  ; start of platform  25,26
    DEFB 5    ; length             (byte 27)    

    DEFB 0    ; 1 = enabled 0 = disabled  
    DEFW 64  ; start of platform  25,26
    DEFB 2    ; length             (byte 27)        
    ;;; tokens 2 bytes each
    
    DEFW 388  ; treasure token offset from DF_CC   always 4 treasure (byte 28)
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 389  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 421  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 422  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
     
    DEFW 120  ; enemySpriteZeroPos_ST 
    DEFW 635  ; enemySpriteOnePos_ST  
    DEFW 126  ; enemySpriteZeroPos_END
    DEFW 643  ; enemySpriteOnePos_END 
    DEFW 120  ; enemySpriteZeroPos_CUR
    DEFW 640  ; enemySpriteOnePos_CUR 
    DEFW 1    ; enemySpriteZeroPos_DIR
    DEFW 1    ; enemySpriteOnePos_DIR 
    DEFB 0    ; enemy 0 full rate enemy = 1; half rate = 0
    DEFB 1    ; enemy 1 full rate enemy = 1; half rate = 0
    DEFW enemySpriteZero
    DEFW enemySpriteThree    
    DEFB  0  ; enemy zero orientation horizontal = 0 vertical = 1
    DEFB  0  ; enemy one orientation horizontal = 0 vertical = 1        
    DEFB 5        ;; X position left most is zero
    DEFB 3        ;; Y position bottom is 0
    DEFW 467      ;; screen memory offset    
    DEFB _P,_R,_I,_N,_T,_F,_OP,_CP,0,0,$ff    
    
    


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
    
    DEFB 0    ; 1 = enabled 0 = disabled  
    DEFW 394  ; start of platform  25,26
    DEFB 13    ; length             (byte 27)   
    
    DEFB 0    ; 1 = enabled 0 = disabled  
    DEFW 64  ; start of platform  25,26
    DEFB 2    ; length             (byte 27)  
    ;;; tokens 2 bytes each
  
    DEFW 300  ; treasure token offset from DF_CC   always 4 treasure (byte 28)
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 333  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 303  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 336  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    
    DEFW 640  ; enemySpriteZeroPos_ST 
    DEFW 113  ; enemySpriteOnePos_ST  
    DEFW 647  ; enemySpriteZeroPos_END
    DEFW 122  ; enemySpriteOnePos_END 
    DEFW 640  ; enemySpriteZeroPos_CUR
    DEFW 113  ; enemySpriteOnePos_CUR 
    DEFW 1    ; enemySpriteZeroPos_DIR
    DEFW 1    ; enemySpriteOnePos_DIR 
    DEFB 1    ; enemy 0 full rate enemy = 1; half rate = 0
    DEFB 0    ; enemy 1 full rate enemy = 1; half rate = 0  
    DEFW enemySpriteZero
    DEFW enemySpriteOne    
    DEFB  0  ; enemy zero orientation horizontal = 0 vertical = 1
    DEFB  0  ; enemy one orientation horizontal = 0 vertical = 1        
    DEFB 5        ;; X position left most is zero
    DEFB 3        ;; Y position bottom is 0
    DEFW 467      ;; screen memory offset    
    DEFB _R,_O,_O,_M,0,_3,_QM,0,0,0,$ff


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

    DEFB 1    ; 1 = enabled 0 = disabled  (byte16)
    DEFW 610  ; start of platform   17,18
    DEFB 4    ; length   19
    
    DEFB 1    ; 1 = enabled 0 = disabled  
    DEFW 454  ; start of platform  21,22
    DEFB 6    ; length  23
    
    DEFB 1    ; 1 = enabled 0 = disabled  
    DEFW 532  ; start of platform  25,26
    DEFB 3    ; length             (byte 27)

    DEFB 1    ; 1 = enabled 0 = disabled  
    DEFW 370  ; start of platform  25,26
    DEFB 8    ; length             (byte 27)

    DEFB 1    ; 1 = enabled 0 = disabled  
    DEFW 661  ; start of platform  25,26
    DEFB 5    ; length             (byte 27)            
   
    ;;; tokens 2 bytes each
    DEFW 239  ; treasure token offset from DF_CC   always 4 treasure (byte 28)
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 514  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 515  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 516  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    
    DEFW 103  ; enemySpriteZeroPos_ST 
    DEFW 333   ; enemySpriteOnePos_ST  
    DEFW 124  ; enemySpriteZeroPos_END
    DEFW 355  ; enemySpriteOnePos_END 
    DEFW 120  ; enemySpriteZeroPos_CUR
    DEFW 350  ; enemySpriteOnePos_CUR 
    DEFW 1    ; enemySpriteZeroPos_DIR
    DEFW 1    ; enemySpriteOnePos_DIR 
    DEFB 1    ; enemy 0, full rate enemy = 1; half rate = 0
    DEFB 1    ; enemy 1, full rate enemy = 1; half rate = 0  
    DEFW enemySpriteOne
    DEFW enemySpriteThree    
    DEFB  0  ; enemy zero orientation horizontal = 0 vertical = 1
    DEFB  0  ; enemy one orientation horizontal = 0 vertical = 1        
    DEFB 5        ;; X position left most is zero
    DEFB 3        ;; Y position bottom is 0
    DEFW 467      ;; screen memory offset    
    DEFB _A,_R,_G,_C,0,_A,_R,_G,_V,0,$ff

    DEFB 4    ; room ID   
    ;;; DOORS  * 3 max enabled  
    DEFB 1    ; Door orientation east=1  0= door disabled
    DEFW 427   ; offset from DF_CC to top of door
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
    DEFW 607  ; start of platform   17,18
    DEFB 4    ; length   19
    
    DEFB 137    ; character of platform 0 = disabled  20
    DEFW 454  ; start of platform  21,22
    DEFB 3    ; length  23
    
    DEFB 128    ; character of platform 0 = disabled  24
    DEFW 364  ; start of platform  25,26
    DEFB 3    ; length             (byte 27)
    
    DEFB 0    ; 1 = enabled 0 = disabled  
    DEFW 394  ; start of platform  25,26
    DEFB 13    ; length             (byte 27)        

    DEFB 0    ; 1 = enabled 0 = disabled  
    DEFW 64  ; start of platform  25,26
    DEFB 3    ; length             (byte 27)       
    ;;; tokens 2 bytes each
    DEFW 579  ; treasure token offset from DF_CC   always 4 treasure (byte 28)
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 580  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 271  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 304  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    
    DEFW 640  ; enemySpriteZeroPos_ST 
    DEFW 102  ; enemySpriteOnePos_ST  
    DEFW 647  ; enemySpriteZeroPos_END
    DEFW 122  ; enemySpriteOnePos_END 
    DEFW 640  ; enemySpriteZeroPos_CUR
    DEFW 113  ; enemySpriteOnePos_CUR 
    DEFW 1    ; enemySpriteZeroPos_DIR
    DEFW 1    ; enemySpriteOnePos_DIR 
    DEFB 1    ; enemy 0 full rate enemy = 1; half rate = 0
    DEFB 0    ; enemy 1 full rate enemy = 1; half rate = 0  
    DEFW enemySpriteOne
    DEFW enemySpriteThree      
    DEFB  0  ; enemy zero orientation horizontal = 0 vertical = 1
    DEFB  1  ; enemy one orientation horizontal = 0 vertical = 1   
    DEFB 5        ;; X position left most is zero
    DEFB 3        ;; Y position bottom is 0
    DEFW 467      ;; screen memory offset    
    DEFB _L,_O,_S,_T,0,_G,_O,_L,_D,0,$ff
    
    DEFB 5    ; room ID   
    ;;; DOORS  * 3 max enabled  
    DEFB 1    ; Door orientation east=1  0= door disabled
    DEFW 526   ; offset from DF_CC to top of door
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
    
    DEFB 0    ; 1 = enabled 0 = disabled  
    DEFW 394  ; start of platform  25,26
    DEFB 13    ; length             (byte 27)        
    
    DEFB 0    ; 1 = enabled 0 = disabled  
    DEFW 64  ; start of platform  25,26
    DEFB 2    ; length             (byte 27)    
    ;;; tokens 2 bytes each
    DEFW 508  ; treasure token offset from DF_CC   always 4 treasure (byte 28)
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 510  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 511  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 513  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 640  ; enemySpriteZeroPos_ST 
    DEFW 333  ; enemySpriteOnePos_ST  
    DEFW 647  ; enemySpriteZeroPos_END
    DEFW 348  ; enemySpriteOnePos_END 
    DEFW 640  ; enemySpriteZeroPos_CUR
    DEFW 345  ; enemySpriteOnePos_CUR 
    DEFW 1    ; enemySpriteZeroPos_DIR
    DEFW 1    ; enemySpriteOnePos_DIR 
    DEFB 1    ; enemy 0 full rate enemy = 1; half rate = 0
    DEFB 1    ; enemy 1 full rate enemy = 1; half rate = 0  
    DEFW enemySpriteZero
    DEFW enemySpriteThree  
    DEFB  0  ; enemy zero orientation horizontal = 0 vertical = 1
    DEFB  0  ; enemy one orientation horizontal = 0 vertical = 1    
    DEFB 5        ;; X position left most is zero
    DEFB 3        ;; Y position bottom is 0
    DEFW 467      ;; screen memory offset    
    DEFB _A,_L,_L,_T,_I,_M,_I,_N,_G,0,$ff


    
    DEFB 6    ; room ID   
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
    
    DEFB 0    ; 1 = enabled 0 = disabled  
    DEFW 394  ; start of platform  25,26
    DEFB 13    ; length             (byte 27)        
    
    DEFB 0    ; 1 = enabled 0 = disabled  
    DEFW 64  ; start of platform  25,26
    DEFB 2    ; length             (byte 27)    
    ;;; tokens 2 bytes each
    DEFW 583  ; treasure token offset from DF_CC   always 4 treasure (byte 28)
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 584  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 585  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 586  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    
    DEFW 640  ; enemySpriteZeroPos_ST 
    DEFW 113  ; enemySpriteOnePos_ST  
    DEFW 647  ; enemySpriteZeroPos_END
    DEFW 122  ; enemySpriteOnePos_END 
    DEFW 640  ; enemySpriteZeroPos_CUR
    DEFW 113  ; enemySpriteOnePos_CUR 
    DEFW 1    ; enemySpriteZeroPos_DIR
    DEFW 1    ; enemySpriteOnePos_DIR 
    DEFB 1    ; enemy 0 full rate enemy = 1; half rate = 0
    DEFB 0    ; enemy 1 full rate enemy = 1; half rate = 0  
    DEFW enemySpriteOne
    DEFW enemySpriteTwo    
    DEFB  0  ; enemy zero orientation horizontal = 0 vertical = 1
    DEFB  0  ; enemy one orientation horizontal = 0 vertical = 1    
    DEFB 5        ;; X position left most is zero
    DEFB 3        ;; Y position bottom is 0
    DEFW 467      ;; screen memory offset    
    DEFB _S,_E,_E,_N,0,_B,_4,_QM,_QM,0,$ff


    DEFB 7    ; room ID   
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
    
    DEFB 0    ; 1 = enabled 0 = disabled  
    DEFW 394  ; start of platform  25,26
    DEFB 13    ; length             (byte 27)        
    
    DEFB 0    ; 1 = enabled 0 = disabled  
    DEFW 64  ; start of platform  25,26
    DEFB 2    ; length             (byte 27)    
    ;;; tokens 2 bytes each
    DEFW 583  ; treasure token offset from DF_CC   always 4 treasure (byte 28)
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 584  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 585  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 586  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    
    DEFW 640  ; enemySpriteZeroPos_ST 
    DEFW 113  ; enemySpriteOnePos_ST  
    DEFW 647  ; enemySpriteZeroPos_END
    DEFW 122  ; enemySpriteOnePos_END 
    DEFW 640  ; enemySpriteZeroPos_CUR
    DEFW 113  ; enemySpriteOnePos_CUR 
    DEFW 1    ; enemySpriteZeroPos_DIR
    DEFW 1    ; enemySpriteOnePos_DIR 
    DEFB 1    ; enemy 0 full rate enemy = 1; half rate = 0
    DEFB 0    ; enemy 1 full rate enemy = 1; half rate = 0      
    DEFW enemySpriteFour
    DEFW enemySpriteTwo       
    DEFB  0  ; enemy zero orientation horizontal = 0 vertical = 1
    DEFB  1  ; enemy one orientation horizontal = 0 vertical = 1   
    DEFB 5        ;; X position left most is zero
    DEFB 3        ;; Y position bottom is 0
    DEFW 467      ;; screen memory offset    
    DEFB _S,_T,_R,_C,_M,_P,_OP,_CP,__,__,$ff


    DEFB 8    ; room ID   
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
    
    DEFB 0    ; 1 = enabled 0 = disabled  
    DEFW 394  ; start of platform  25,26
    DEFB 13    ; length             (byte 27)        
    
    DEFB 0    ; 1 = enabled 0 = disabled  
    DEFW 64  ; start of platform  25,26
    DEFB 2    ; length             (byte 27)    
    ;;; tokens 2 bytes each
    DEFW 583  ; treasure token offset from DF_CC   always 4 treasure (byte 28)
    DEFB 1    ; is the trreasure enabled or not - used when controlling flashing
    DEFW 584  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when controlling flashing    
    DEFW 585  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when controlling flashing    
    DEFW 586  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when controlling flashing    
    DEFW 640  ; enemySpriteZeroPos_ST 
    DEFW 113  ; enemySpriteOnePos_ST  
    DEFW 647  ; enemySpriteZeroPos_END
    DEFW 122  ; enemySpriteOnePos_END 
    DEFW 640  ; enemySpriteZeroPos_CUR
    DEFW 113  ; enemySpriteOnePos_CUR 
    DEFW 1    ; enemySpriteZeroPos_DIR
    DEFW 1    ; enemySpriteOnePos_DIR 
    DEFB 1    ; enemy 0 full rate enemy = 1; half rate = 0
    DEFB 1    ; enemy 1 full rate enemy = 1; half rate = 0      
    DEFW enemySpriteFive
    DEFW enemySpriteFive       
    DEFB  0  ; enemy zero orientation horizontal = 0 vertical = 1
    DEFB  0  ; enemy one orientation horizontal = 0 vertical = 1   
    DEFB 5        ;; X position left most is zero
    DEFB 3        ;; Y position bottom is 0
    DEFW 467      ;; screen memory offset
    DEFB _W,_A,_C,_K,_Y,_R,_A,_C,_E,_S,$ff
        
        

    DEFB 9    ; room ID   
    ;;; DOORS  * 3 max enabled  
    DEFB 1    ; Door orientation east=1  0= door disabled
    DEFW 130   ; offset from DF_CC to top of door
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
    DEFW 370  ; start of platform   17,18
    DEFB 24    ; length   19
    
    DEFB 5    ; character of platform 0 = disabled  20
    DEFW 595  ; start of platform  21,22
    DEFB 6    ; length  23
    
    DEFB 128    ; character of platform 0 = disabled  24
    DEFW 364  ; start of platform  25,26
    DEFB 1    ; length             (byte 27)
    
    DEFB 0    ; 1 = enabled 0 = disabled  
    DEFW 394  ; start of platform  25,26
    DEFB 13    ; length             (byte 27)        
    
    DEFB 0    ; 1 = enabled 0 = disabled  
    DEFW 64  ; start of platform  25,26
    DEFB 2    ; length             (byte 27)    
    ;;; tokens 2 bytes each
    DEFW 583  ; treasure token offset from DF_CC   always 4 treasure (byte 28)
    DEFB 1    ; is the trreasure enabled or not - used when controlling flashing
    DEFW 584  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when controlling flashing    
    DEFW 585  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when controlling flashing    
    DEFW 586  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when controlling flashing    
    DEFW 635  ; enemySpriteZeroPos_ST 
    DEFW 635  ; enemySpriteOnePos_ST  
    DEFW 654  ; enemySpriteZeroPos_END
    DEFW 654  ; enemySpriteOnePos_END 
    DEFW 637  ; enemySpriteZeroPos_CUR
    DEFW 650  ; enemySpriteOnePos_CUR 
    DEFW 1    ; enemySpriteZeroPos_DIR
    DEFW 1    ; enemySpriteOnePos_DIR 
    DEFB 1    ; enemy 0 full rate enemy = 1; half rate = 0
    DEFB 1    ; enemy 1 full rate enemy = 1; half rate = 0      
    DEFW enemySpriteFive
    DEFW enemySpriteFive       
    DEFB  0  ; enemy zero orientation horizontal = 0 vertical = 1
    DEFB  0  ; enemy one orientation horizontal = 0 vertical = 1   
    DEFB 20        ;; X position left most is zero
    DEFB 14        ;; Y position bottom is 0
    DEFW 119      ;; screen memory offset
    DEFB _L,_O,_G,_A,_N,_S,__,_R,_U,_N,$ff        
    
    
    
    
    DEFB 10    ; room ID   
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
    DEFB 10    ; length   19
    
    DEFB 137    ; character of platform 0 = disabled  20
    DEFW 454  ; start of platform  21,22
    DEFB 2    ; length  23
    
    DEFB 128    ; character of platform 0 = disabled  24
    DEFW 364  ; start of platform  25,26
    DEFB 18    ; length             (byte 27)
    
    DEFB 0    ; 1 = enabled 0 = disabled  
    DEFW 394  ; start of platform  25,26
    DEFB 13    ; length             (byte 27)        
    
    DEFB 0    ; 1 = enabled 0 = disabled  
    DEFW 64  ; start of platform  25,26
    DEFB 2    ; length             (byte 27)    
    ;;; tokens 2 bytes each
    DEFW 584  ; treasure token offset from DF_CC   always 4 treasure (byte 28)
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 585  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 586  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 670  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    
    DEFW 176  ; enemySpriteZeroPos_ST 
    DEFW 217  ; enemySpriteOnePos_ST  
    DEFW 506  ; enemySpriteZeroPos_END
    DEFW 481  ; enemySpriteOnePos_END 
    DEFW 308  ; enemySpriteZeroPos_CUR
    DEFW 349  ; enemySpriteOnePos_CUR 
    DEFW 33    ; enemySpriteZeroPos_DIR
    DEFW 33    ; enemySpriteOnePos_DIR 
    DEFB 1    ; enemy 0 full rate enemy = 1; half rate = 0
    DEFB 1    ; enemy 1 full rate enemy = 1; half rate = 0  
    DEFW enemySpriteThree
    DEFW enemySpriteThree    
    DEFB  0  ; enemy zero orientation horizontal = 0 vertical = 1
    DEFB  0  ; enemy one orientation horizontal = 0 vertical = 1 
    DEFB 5        ;; X position left most is zero
    DEFB 14        ;; Y position bottom is 0
    DEFW 104      ;; screen memory offset    
    DEFB _X,_P,_E,_R,_I,_M,_E,_N,_T,__,$ff   ; this has to be 10 characters


    DEFB 11    ; room ID   
    ;;; DOORS  * 3 max enabled  
    DEFB 1    ; Door orientation east=1  0= door disabled
    DEFW 264   ; offset from DF_CC to top of door
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
    
    DEFB 6    ; character of platform 0 = disabled  (byte16)
    DEFW 529  ; start of platform   17,18
    DEFB 7    ; length   19
    
    DEFB 6    ; character of platform 0 = disabled  20
    DEFW 540  ; start of platform  21,22
    DEFB 10    ; length  23
    
    DEFB 6    ; character of platform 0 = disabled  24
    DEFW 554  ; start of platform  25,26
    DEFB 6    ; length             (byte 27)
    
    DEFB 6    ; 1 = enabled 0 = disabled  
    DEFW 661  ; start of platform  25,26
    DEFB 7    ; length             (byte 27)        
    
    DEFB 6    ; 1 = enabled 0 = disabled  
    DEFW 672  ; start of platform  25,26
    DEFB 10    ; length             (byte 27)    
    ;;; tokens 2 bytes each
    DEFW 277  ; treasure token offset from DF_CC   always 4 treasure (byte 28)
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 283  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 290  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 291  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    
    DEFW 206  ; enemySpriteZeroPos_ST 
    DEFW 220  ; enemySpriteOnePos_ST  
    DEFW 602  ; enemySpriteZeroPos_END
    DEFW 616  ; enemySpriteOnePos_END 
    DEFW 569  ; enemySpriteZeroPos_CUR
    DEFW 253  ; enemySpriteOnePos_CUR 
    DEFW 33    ; enemySpriteZeroPos_DIR
    DEFW 33    ; enemySpriteOnePos_DIR 
    DEFB 0    ; enemy 0 full rate enemy = 1; half rate = 0
    DEFB 0    ; enemy 1 full rate enemy = 1; half rate = 0  
    DEFW enemySpriteEight
    DEFW enemySpriteEight   
    DEFB  1  ; enemy zero orientation horizontal = 0 vertical = 1
    DEFB  1  ; enemy one orientation horizontal = 0 vertical = 1 
    DEFB 2        ;; X position left most is zero
    DEFB 14        ;; Y position bottom is 0
    DEFW 101      ;; screen memory offset    
    DEFB _O,_N,__,_L,_E,_F,_T,__,__,__,$ff   ; this has to be 10 characters



    DEFB 12    ; room ID   
    ;;; DOORS  * 3 max enabled  
    DEFB 1    ; Door orientation east=1  0= door disabled
    DEFW 295   ; offset from DF_CC to top of door
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
    
    DEFB 6    ; character of platform 0 = disabled  (byte16)
    DEFW 529  ; start of platform   17,18
    DEFB 7    ; length   19
    
    DEFB 6    ; character of platform 0 = disabled  20
    DEFW 540  ; start of platform  21,22
    DEFB 9    ; length  23
    
    DEFB 6    ; character of platform 0 = disabled  24
    DEFW 553  ; start of platform  25,26
    DEFB 7    ; length             (byte 27)
    
    DEFB 6    ; 1 = enabled 0 = disabled  
    DEFW 685  ; start of platform  25,26
    DEFB 7    ; length             (byte 27)        
    
    DEFB 6    ; 1 = enabled 0 = disabled  
    DEFW 672  ; start of platform  25,26
    DEFB 9    ; length             (byte 27)    
    ;;; tokens 2 bytes each
    DEFW 342  ; treasure token offset from DF_CC   always 4 treasure (byte 28)
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 343  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 344  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 345  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    
    DEFW 107  ; enemySpriteZeroPos_ST 
    DEFW 120  ; enemySpriteOnePos_ST  
    DEFW 602  ; enemySpriteZeroPos_END
    DEFW 615  ; enemySpriteOnePos_END 
    DEFW 569  ; enemySpriteZeroPos_CUR
    DEFW 252  ; enemySpriteOnePos_CUR 
    DEFW 33    ; enemySpriteZeroPos_DIR
    DEFW 33    ; enemySpriteOnePos_DIR 
    DEFB 1    ; enemy 0 full rate enemy = 1; half rate = 0
    DEFB 1    ; enemy 1 full rate enemy = 1; half rate = 0  
    DEFW enemySpriteSeven
    DEFW enemySpriteSeven    
    DEFB  1  ; enemy zero orientation horizontal = 0 vertical = 1
    DEFB  1  ; enemy one orientation horizontal = 0 vertical = 1 
    DEFB 2        ;; X position left most is zero
    DEFB 8        ;; Y position bottom is 0
    DEFW 299      ;; screen memory offset    
    DEFB _T,_H,_E,__,_B,_E,_L,_L,_S,__,$ff   ; this has to be 10 characters


    DEFB 13    ; room ID   
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
    DEFB 10    ; length   19
    
    DEFB 137    ; character of platform 0 = disabled  20
    DEFW 454  ; start of platform  21,22
    DEFB 2    ; length  23
    
    DEFB 128    ; character of platform 0 = disabled  24
    DEFW 364  ; start of platform  25,26
    DEFB 18    ; length             (byte 27)
    
    DEFB 0    ; 1 = enabled 0 = disabled  
    DEFW 394  ; start of platform  25,26
    DEFB 13    ; length             (byte 27)        
    
    DEFB 0    ; 1 = enabled 0 = disabled  
    DEFW 64  ; start of platform  25,26
    DEFB 2    ; length             (byte 27)    
    ;;; tokens 2 bytes each
    DEFW 583  ; treasure token offset from DF_CC   always 4 treasure (byte 28)
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 584  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 585  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    DEFW 691  ; treasure token offset from DF_CC
    DEFB 1    ; is the trreasure enabled or not - used when 
    
    DEFW 640  ; enemySpriteZeroPos_ST 
    DEFW 113  ; enemySpriteOnePos_ST  
    DEFW 647  ; enemySpriteZeroPos_END
    DEFW 122  ; enemySpriteOnePos_END 
    DEFW 643  ; enemySpriteZeroPos_CUR
    DEFW 115  ; enemySpriteOnePos_CUR 
    DEFW 1    ; enemySpriteZeroPos_DIR
    DEFW 1    ; enemySpriteOnePos_DIR 
    DEFB 1    ; enemy 0 full rate enemy = 1; half rate = 0
    DEFB 0    ; enemy 1 full rate enemy = 1; half rate = 0  
    DEFW enemySpriteTwo
    DEFW enemySpriteThree    
    DEFB  0  ; enemy zero orientation horizontal = 0 vertical = 1
    DEFB  0  ; enemy one orientation horizontal = 0 vertical = 1 
    DEFB 5        ;; X position left most is zero
    DEFB 3        ;; Y position bottom is 0
    DEFW 467      ;; screen memory offset    
    DEFB _Z,_X,_8,_1,_R,_U,_L,_E,_S,__,$ff   ; this has to be 10 characters
        

    
VariablesEnd:   DEFB $80
BasicEnd: 
#END

