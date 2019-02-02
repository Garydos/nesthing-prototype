;;;;;;;;;;;;;;;
; ROM header info
;;;;;;;;;;;;;;;
	.inesprg 1   ; 1x 16KB PRG code
	.ineschr 1   ; 1x  8KB CHR data
	.inesmap 0   ; mapper 0 = NROM, no bank swapping
	.inesmir 1   ; background mirroring
;;;;;;;;;;;;;;;
; Variable declerations
;;;;;;;;;;;;;;;


	.rsset $0000
famitonescratch		.rs 3 ; save some initial scratch space for the FamiTone audio engine
scratchptr			.rs 2 ; scratch space, mostly for pointers
enemyptr			.rs 1 ; pointer to recently accessed enemy in enemylist
num1				.rs 1 ; multiplicand
num2				.rs 1 ; multiplier, should be small
buttons1			.rs 1 ; buttons for player 1
buttons2			.rs 1 ; buttons for player 2
nextsprite			.rs 1 ; keeps track of next sprite to be allocated to new entities
rect1_x				.rs 1 ; bounding box variables for axis-aligned bounding box collisions
rect1_y				.rs 1
rect1_width			.rs 1
rect1_height		.rs 1
rect2_x				.rs 1
rect2_y				.rs 1
rect2_width			.rs 1
rect2_height		.rs 1
testrectcollis		.rs 1
; Player1 struct
player1x			.rs 1 ; player1 x position
player1y			.rs 1 ; player1 y position
player1xvel			.rs 1
player1xvelfract	.rs 1 ; fractional part of player1x used for
						  ; velocity calculations
player1yvel			.rs 1
player1yvelfract	.rs 1 ; fractional part of player1y used for
						  ; velocity calculations
player1xaccel		.rs 1
player1yaccel		.rs 1
player1dashing		.rs 1
player1dashtimer	.rs 1
player1jumping		.rs 1
player1jumpingtimer .rs 1
player1invinc		.rs 1
player1invinctimer	.rs 1
player1knockback	.rs 1
player1knockbacktimer	.rs 1
player1hurt			.rs 1
player1falling		.rs 1
player1canjump		.rs 1
player1anim			.rs 1 ; animation number of current player1 animation
player1animtimer	.rs 1 ; timer for current player1 frame
player1animframe	.rs 1 ; frame number of current player1 animation
player1mirrored		.rs 1 ; are player1's sprites mirrored
; Level variables
scrolling  .rs 1  ; is the screen scrolling or not
scroll     .rs 1  ; horizontal scroll count
nametable  .rs 1  ; which nametable to use, 0 or 1
columnLow  .rs 1  ; low byte of new column address
columnHigh .rs 1  ; high byte of new column address
sourceLow  .rs 1  ; source for column data
sourceHigh .rs 1
columnNumber .rs 1  ; which column of level data to draw (or other use)
rowNumber	 .rs 1  ; row number to draw (or other use)
tiletest	.rs 1

;enemy variables
	.rsset $0400 ;enemy stack starts at $0400
enemylistsize	.rs 1 ;size of enemy list
enemylist		.rs $FE ; reserve full page for entities (this is just an estimate for now, once
					; the enemy struct has been finalized, do some math to figure out
					; roughly the max number of enemies on screen at any time)
					
;; enemy Struct
	.rsset $0000
enemy_type			.rs 1 ;the enemy's type, used
					;to know what subroutines to run on it, what sprites
					;to use in animations, etc..
enemy_spr_ptr		.rs 1 ; pointer to first sprite of enemy.  All sprites
						  ; start with $02xx so no need for second byte in pointer
enemy_x			.rs 1 ;  x position
enemy_y			.rs 1 ;  y position
enemy_xvel			.rs 1
enemy_xvelfract	.rs 1 ; fractional part used for
						  ; velocity calculations
enemy_yvel			.rs 1
enemy_yvelfract	.rs 1 ; fractional part used for
						  ; velocity calculations
enemy_xaccel		.rs 1
enemy_yaccel		.rs 1
enemy_anim			.rs 1 ; animation number of current animation
enemy_animtimer	.rs 1 ; timer for current frame
enemy_animframe	.rs 1 ; frame number of current animation
enemy_mirrored		.rs 1 ; are enemy's sprites mirrored
enemy_size			.rs 1 ; marks the end of the struct, and its size
ENEMY_STRUCT_SIZE = (enemy_size)

;;;;;;;;;;;;;;;
; Constant declerations
;;;;;;;;;;;;;;;

SPRITE_OFFSET	= $0200 ;location of sprites in RAM
PLAYER_1_SPRITE	= SPRITE_OFFSET + 0 ;location of PLAYER1 sprite in ram
PLAYER_1_ANIM_WALK_SPEED = $8  ;walk speed of player
PLAYER_1_ANIM_WALK_FRAMES = $01 ; total frames - 1
PLAYER_1_DASH_TIME	= $20		; amount of time dash lasts
PLAYER_1_DASH_BLINK_TIME =  PLAYER_1_DASH_TIME + $40  ; amount of time dash lasts + amount of time before next dash
PLAYER_1_INVINC_BLINK_TIME = $40 ;amount of time player is invincible after being hurt
PLAYER_1_HURT_TIME = $20 ;amount of time player is hurt (player can't press buttons during this time)
PLAYER_1_KNOCKBACK_BLINK_TIME = $19 ;amount of time till player can jump back again)
PLAYER_1_KNOCKBACK_TIME = $18 ;amount of time player jumps back when hurt
PLAYER_1_JUMP_TIMER_LIM = $08 ;amount of time jump lasts
SCREEN_SCROLL_REGION = $78 ; the middle of the screen where scrolling will start

;;;;;;;;;;;;;;;
; PRG-ROM Bank 1
;;;;;;;;;;;;;;;

	.bank 0
	.org $8000 
	
	
RESET:
	SEI          ; disable IRQs
	CLD          ; disable decimal mode
	;LDX #$40
	;STX $4017    ; disable APU frame IRQ
	LDX #$FF
	TXS          ; Set up stack
	INX          ; now X = 0
	STX $2000    ; disable NMI
	STX $2001    ; disable rendering
	;STX $4010    ; disable DMC IRQs
  
vblankwait1:       ; First wait for vblank to make sure PPU is ready
	BIT $2002
	BPL vblankwait1
  
clrmem:
	LDA #$00
	STA $0000, x
	STA $0100, x
	STA $0300, x
	STA $0400, x
	STA $0500, x
	STA $0600, x
	STA $0700, x
	LDA #$FE
	STA $0200, x    ;move all sprites off screen
	INX
	BNE clrmem
  
vblankwait2:      ; Second wait for vblank, PPU is ready after this
	BIT $2002
	BPL vblankwait2
	
	
;;;;;;;;;;
;move this code to its own subroutine later
;;;;;;;;;;
LoadPalettes:
	LDA $2002             ; read PPU status to reset the high/low latch
	LDA #$3F
	STA $2006             ; write the high byte of $3F00 address
	LDA #$00
	STA $2006             ; write the low byte of $3F00 address
	LDX #$00              ; start out at 0
LoadPalettesLoop:
	LDA palette, x        ; load data from address (palette + the value in x)
						  ; 1st time through loop it will load palette+0
						  ; 2nd time through loop it will load palette+1
						  ; 3rd time through loop it will load palette+2
						  ; etc
	STA $2007             ; write to PPU
	INX                   ; X = X + 1
	CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
	BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
						; if compare was equal to 32, keep going down



LoadSprites:
	LDX #$00              ; start at 0
LoadSpritesLoop:
	LDA sprites, x        ; load data from address (sprites +  x)
	STA $0200, x          ; store into RAM address ($0200 + x)
	INX                   ; X = X + 1
	CPX #$20              ; Compare X to hex $20, decimal 32
	BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
						; if compare was equal to 32, keep going down
			  
			  
InitializeNametables:
	LDA #$01
	STA nametable
	LDA #$00
	STA scroll
	STA columnNumber
InitializeNametablesLoop:
	JSR DrawNewColumn     ; draw bg column
	LDA scroll            ; go to next column
	CLC
	ADC #$08
	STA scroll
	INC columnNumber
	LDA columnNumber      ; repeat for first nametable 
	CMP #$20
	BNE InitializeNametablesLoop
	
	LDA #$00
	STA nametable
	LDA #$00
	STA scroll
	JSR DrawNewColumn     ; draw first column of second nametable
	INC columnNumber

	LDA #$00              ; set back to increment +1 mode
	STA $2000
InitializeNametablesDone:

InitializeAttributes:
	LDA #$01
	STA nametable
	LDA #$00
	STA scroll
	STA columnNumber
InitializeAttributesLoop:
	JSR DrawNewAttributes     ; draw attribs
	LDA scroll                ; go to next column
	CLC
	ADC #$20
	STA scroll

	LDA columnNumber      ; repeat for first nametable 
	CLC 
	ADC #$04
	STA columnNumber
	CMP #$20
	BNE InitializeAttributesLoop

	LDA #$00
	STA nametable
	LDA #$00
	STA scroll
	JSR DrawNewAttributes     ; draw first column of second nametable
InitializeAttributesDone:

	LDA #$21
	STA columnNumber



	LDA #%10010100   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1,
					 ; +32 increment mode
	STA $2000

	LDA #%00011110   ; enable sprites, enable background, no clipping on left side
	STA $2001
	
	ldx #LOW(nesthing_ost_music_data)
	ldy #HIGH(nesthing_ost_music_data)
	lda #$80;This sets Famitone to use NTSC mode.
	jsr FamiToneInit

	lda #0;Play first subsong
	jsr FamiToneMusicPlay

		
	;write the universal palette background color
	LDA #$3F
	STA $2006
	LDA #$00
	STA $2006
	;$3C = sky blue
	LDA #$3C
	STA $2007
	
;;;;;;;;
; temp section, move later
;;;;;;;;

;initialize some variables
	LDA #$00
	STA player1anim
	STA player1animtimer
	STA nametable
	STA scrolling
	STA player1x
	STA enemylistsize
	STA player1invinc
	STA player1hurt
	STA player1knockback
	LDA #$BF
	STA player1y
	LDA #PLAYER_1_DASH_BLINK_TIME
	STA player1dashtimer
	LDA #PLAYER_1_INVINC_BLINK_TIME
	STA player1invinctimer
	LDA #PLAYER_1_KNOCKBACK_BLINK_TIME
	STA player1knockbacktimer
	LDA #$01
	STA player1canjump
	LDA #$10
	STA nextsprite ;player1 sprite takes 4 sprites, each sprite is 4 bytes
					;$04 * $04 = $10

					
	JSR TestSpawnEnemy	
	
;;;;;;;;
; end temp section
;;;;;;;;
Forever:
	JMP Forever     ;jump back to Forever, infinite loop

	
NMI:
	LDA scrolling
	BEQ NotScrolling
NTSwapCheck:
	LDA scroll            ; check if the scroll just wrapped from 255 to 0
	BNE NTSwapCheckDone  
NTSwap:
	LDA nametable         ; load current nametable number (0 or 1)
	EOR #$01              ; exclusive OR of bit 0 will flip that bit
	STA nametable         ; so if nametable was 0, now 1
						;    if nametable was 1, now 0
NTSwapCheckDone:

NewAttribCheck:
	LDA scroll
	AND #%00011111            ; check for multiple of 32
	BNE NewAttribCheckDone    ; if low 5 bits = 0, time to write new attribute bytes
	jsr DrawNewAttributes
NewAttribCheckDone:


NewColumnCheck:
	LDA scroll
	AND #%00000111            ; throw away higher bits to check for multiple of 8
	BNE NewColumnCheckDone    ; done if lower bits != 0
	JSR DrawNewColumn         ; if lower bits = 0, time for new column

	lda columnNumber
	clc
	adc #$01             ; go to next column
	and #%01111111       ; only 128 columns of data, throw away top bit to wrap
	sta columnNumber
NewColumnCheckDone:

NotScrolling:

	LDA #$00
	STA scrolling	; reset scrolling
	STA $2006       ; clean up PPU address registers
	STA $2006
	
	LDA scroll
	STA $2005        ; write the horizontal scroll count register

	LDA #$00         ; no vertical scrolling
	STA $2005
	
	
	LDA #$00
	STA $2003       ; set the low byte (00) of the RAM address
	LDA #$02
	STA $4014       ; set the high byte (02) of the RAM address, start the transfer

	
	;;This is the PPU clean up section, so rendering the next frame starts properly.
	LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
	ORA nametable    ; select correct nametable for bit 0
	STA $2000

	LDA #%00011110   ; enable sprites, enable background, no clipping on left side
	STA $2001
	

	;;;;;;;;;;;;;;
	; MAIN GAME LOOP FUNCTIONS
	;;;;;;;;;;;;;;
	JSR ResetPlayerAcceleration
	JSR ReadControllers
	JSR HandlePlayerInput
	JSR ApplyPlayerGrav
	JSR ApplyPlayer1Phys
	JSR CheckPlatformCollisions
	JSR UpdateEnemies
	JSR TestBoxCollision
	JSR UpdateSprites
	JSR TickTimers
	JSR TestGetTile
	JSR FamiToneUpdate
	
	RTI
	
TickTimers:
	INC player1animtimer
	JSR TickDashTimer
	JSR TickHurtTimer
	JSR TickKnockbackTimer
TickTimersDone:
	RTS
	
TickHurtTimer:
	LDA player1invinctimer
	CMP #PLAYER_1_INVINC_BLINK_TIME
	BCS TickHurtTimerDone
	INC player1invinctimer
	LDA player1invinctimer
	CMP #PLAYER_1_HURT_TIME
	BNE TickHurtTimerDone
	LDA #$00 ;disable player hurt, no longer invincible
	STA player1hurt
	STA player1invinc
TickHurtTimerDone:
	RTS
	
TickKnockbackTimer:
	LDA player1knockbacktimer
	CMP #PLAYER_1_KNOCKBACK_BLINK_TIME
	BCS TickKnockbackTimerDone
	INC player1knockbacktimer
	LDA player1knockbacktimer
	CMP #PLAYER_1_KNOCKBACK_TIME
	BNE TickKnockbackTimerDone
	LDA #$00 ;disable knockback, set walking animation
	STA player1knockback
	STA player1dashing
	STA player1anim
	STA player1animframe
	STA player1animtimer
	;past time when player knockback should be applied, so freeze player
	LDA #$00
	STA player1xvel
	;allow dashing immedietely after recovering
	LDA #PLAYER_1_DASH_BLINK_TIME
	STA player1dashtimer
TickKnockbackTimerDone:
	RTS
	
TickDashTimer:
	LDA player1dashtimer
	CMP #PLAYER_1_DASH_BLINK_TIME
	BCS TickDashTimerDone
	INC player1dashtimer
	LDA player1dashtimer
	CMP #PLAYER_1_DASH_TIME
	BNE TickDashTimerDone
	LDA #$00 ;disable dashing, set walking animation
	STA player1dashing
	STA player1anim
	STA player1animframe
	STA player1animtimer
TickDashTimerDone:
	RTS
	
	
;;;;;;;;
; TEMP TEST FUNCTIONS, DELETE AFTER DONE TESTING
;;;;;;;;

TestBoxCollision:
	LDA player1x
	STA rect1_x
	LDA player1y
	STA rect1_y
	LDA #$10
	STA rect1_width
	STA rect1_height
	STA rect2_width
	STA rect2_height
	
	LDA enemylist+enemy_x
	STA rect2_x
	LDA enemylist+enemy_y
	STA rect2_y
	
	JSR CheckRectCollision
	STA testrectcollis
	BEQ TestBoxCollisionDone
	;check if dashing or not
	LDA player1dashing
	BEQ PlayPlayerhurtanim ;player got hurt
	;enemy got hurt
	;load enemy hurt animation
	LDA #$01
	STA enemylist+enemy_anim
	JMP TestBoxCollisionDone
PlayPlayerhurtanim:
	LDA #$00 ;push to left
	JSR Player1GetHurt
TestBoxCollisionDone:
	RTS

TestGetTile:
	LDA columnNumber
	PHA
	
	JSR SetRowNumberToPlayer
	JSR SetColumnNumberToPlayer
	JSR GetTileFromCurrentColumnRow
	STA tiletest
	
	PLA
	STA columnNumber
	RTS
	
TestSpawnEnemy:
	LDA #$80
	TAX
	LDA #$80
	TAY
	LDA #$00
	;spawn an enemy at screen location x = $80, y = $80, where type = $00
	JSR SpawnEnemy
	RTS
	
;;;;;;;;;;;;;;
; Enemy management
;;;;;;;;;;;;;;

UpdateEnemies:
	LDA enemylistsize
	TAX
	LDA #$00
	STA enemyptr
UpdateEnemiesLoop:
	;first save X
	TXA
	PHA
		
	;Use a jump table to decide how to initialize the enemy
	;grab the proper offset
	LDA enemyptr
	TAX
	LDA enemylist, X ;load in the enemy type number
	
	ASL A; multiply by 2, since each address is 2 bytes long
	TAX
	LDA enemyupdatetable, X
	STA scratchptr
	LDA enemyupdatetable+1, X
	STA scratchptr+1
	;build the return address to put on the stack
	;so that RTS returns to right after the jump
	LDA #HIGH(EnemyUpdateReturnAdr-1)
	PHA
	LDA #LOW(EnemyUpdateReturnAdr-1)
	PHA
	JMP [scratchptr]
	
EnemyUpdateReturnAdr:
	JSR AdvanceEnemyPtrLoop

	;restore X
	PLA
	TAX
	DEX 
	BNE UpdateEnemiesLoop
	RTS
	
	;Jump table for enemy routines
enemyupdatetable:
	.dw UpdateBoomba	; $00
	
AdvanceEnemyPtr:
	;advance enemyptr through enemylist, skipping over sections marked $FF
	;which indicate holes in the list
AdvanceEnemyPtrLoop:
	LDA enemyptr
	CLC
	ADC #ENEMY_STRUCT_SIZE ;advance to next enemy
	STA enemyptr
	TAX
	LDA enemylist, X
	CMP #$FF
	BEQ AdvanceEnemyPtrLoop
	RTS

UpdateBoomba:
	JSR BoombaLogic
	JSR BoombaPhysics
	JSR BoombaCollision
	JSR CheckEnemyPlatformCollisions
	JSR UpdateEnemySpritePosition
	JSR UpdateBoombaSpriteAnimation
	RTS
	
BoombaCollision:
	RTS
	
BoombaLogic:
	;Boombas simply walk to the left
	
	;get enemylist offset, store in Y
	LDA enemyptr
	TAY

	;get sprite offset for enemy, store in X
	LDA enemylist+enemy_spr_ptr, Y
	TAX
	
	;always walk left, unless dying
	LDA enemylist+enemy_anim, Y
	CMP #$01
	BNE BoombaLogicWalk
	;stop walking, boomba dead
	LDA #$00
	STA enemylist+enemy_xaccel, Y
	STA enemylist+enemy_xvel, Y
	JMP BoombaLogicWalkDone
BoombaLogicWalk:
	LDA #-$04
	STA enemylist+enemy_xaccel, Y
BoombaLogicWalkDone:
	;gravity is applied
	LDA #$02
	STA enemylist+enemy_yaccel, Y
	RTS
	
BoombaPhysics:
	;Simply update x and y vels, no friction
	
	;get enemylist offset, store in Y
	LDA enemyptr
	TAY
	
	
	;Add accel to vels
	;x accel
	LDA enemylist+enemy_xvelfract, Y
	CLC
	ADC enemylist+enemy_xaccel, Y
	STA enemylist+enemy_xvelfract, Y
	
	;y accel
	LDA enemylist+enemy_yvelfract, Y
	CLC
	ADC enemylist+enemy_yaccel, Y
	STA enemylist+enemy_yvelfract, Y
	
	;Apply vels
	JSR ApplyEnemyXVel
	JSR ApplyEnemyYVel
	
	;speed limit
	; x speed limit check
	LDA enemylist+enemy_xvel, Y
	BMI XSpeedLimitEnemyMinus
	LDA enemylist+enemy_xvel, Y
	CMP #$02
	BCC XSpeedLimitEnemyDone
	LDA #$01
	JMP XSpeedLimitEnemyDone
XSpeedLimitEnemyMinus:
	LDA enemylist+enemy_xvel, Y
	CMP #$-01
	BCS XSpeedLimitEnemyDone
	LDA #$-01
XSpeedLimitEnemyDone:
	STA enemylist+enemy_xvel, Y
	
	; y speed limit check
	LDA enemylist+enemy_yvel, Y
	BMI YSpeedLimitEnemyMinus
	LDA enemylist+enemy_yvel, Y
	CMP #$03
	BCC YSpeedLimitEnemyDone
	LDA #$02
	JMP YSpeedLimitEnemyDone
YSpeedLimitEnemyMinus:
	LDA enemylist+enemy_yvel, Y
	CMP #$-02
	BCS YSpeedLimitEnemyDone
	LDA #$-02
YSpeedLimitEnemyDone:
	STA enemylist+enemy_yvel, Y
	
	; apply current velocities
	LDA enemylist+enemy_y, Y
	CLC
	ADC enemylist+enemy_yvel, Y
	STA enemylist+enemy_y, Y

	LDA enemylist+enemy_x, Y
	CLC
	ADC enemylist+enemy_xvel, Y
	STA enemylist+enemy_x, Y
	
	RTS

SpawnEnemy:
	;Parameters: X, Y = x and y screen coordinates to spawn enemy, respectively
	;			 A = type of enemy to spawn
	;
	;Create a new enemy in enemylist, set its type, then call InitializeEnemy
	;to initialize it properly
	
	PHA ;save type of enemy
	
	
	;Multiply the size of an enemy struct by the amount of enemies in
	;the list to get the proper offset
	LDA enemylistsize
	STA num2
	LDA #ENEMY_STRUCT_SIZE
	STA num1
	JSR Mult
	
	STA scratchptr ;temp store offset
	PLA ;restore enemy type
	STA scratchptr+$01 ;temp store enemy type
	TXA
	PHA ;temp store X
	
	LDA scratchptr
	TAX
	LDA scratchptr+$01
	STA enemylist, X ;use the proper offset to set enemy type byte
	
	PLA ;restore X coordinate
	STA enemylist + enemy_x, X ;store it in the proper place
	TYA ;get the Y coordinate
	STA enemylist + enemy_y, X ;store it in the proper place
	
	INC enemylistsize ;we've added an enemy, increase size
	TXA
	STA enemyptr ;store offset into enemyptr in anticipation of call to InitializeEnemy
	
	JSR InitializeEnemy
	RTS
	
InitializeEnemy:
	;Paramters: enemyptr = pointer to location of enemy struct
	;
	;Take the enemy at enemyptr and initialize its variables properly
	;according to its type
	
	;Use a jump table to decide how to initialize the enemy
	;grab the proper offset
	LDA enemyptr
	TAX
	LDA enemylist, X ;load in the enemy type number
	
	ASL A; multiply by 2, since each address is 2 bytes long
	TAX
	LDA enemyinittable, X
	STA scratchptr
	LDA enemyinittable+1, X
	STA scratchptr+1
	JMP [scratchptr]
	; no RTS needed as called functions have RTS in them
	
	;Jump table for enemy inits
enemyinittable:
	.dw InitEnemyBoomba	; $00
	
InitEnemyBoomba:
	;get offset again
	LDA enemyptr
	TAX
	TAY

	;set walking animation
	LDA #$00
	STA enemylist+enemy_anim, Y
	;allocate sprites
	LDA nextsprite
	PHA ;save our starting sprite position to avoid loading it again
	STA enemylist + enemy_spr_ptr, X
	CLC
	ADC #$10 ;reserve 4 sprites (4 bytes per sprite)
	STA nextsprite
	PLA ;reload sprite start position
	TAX ;now that we are dealing with the sprites, use it as the offset
	; Restore sprites to default palettes
	LDA #$02
	STA $0202, X
	STA $0206, X
	STA $020A, X
	STA $020E, X
	RTS
	
;;;;;;;;;;;;;;
; Physics engine
;;;;;;;;;;;;;;

CheckRectCollision:
	;checks if rect1 and rect2 are overlapping
	;stores result in A
	
	;rect1.x < rect2.x + rect2.width &&
	LDA rect2_x
	CLC
	ADC rect2_width
	STA scratchptr
	LDA rect1_x
	CMP scratchptr
	BCS CheckRectNoCollision
	
	;rect1.x + rect1.width > rect2.x &&
	LDA rect1_x
	CLC
	ADC rect1_width
	STA scratchptr
	LDA rect2_x
	CMP scratchptr
	BCS CheckRectNoCollision
	
	;rect1.y < rect2.y + rect2.height &&
	LDA rect2_y
	CLC
	ADC rect2_height
	STA scratchptr
	LDA rect1_y
	CMP scratchptr
	BCS CheckRectNoCollision
	
	;rect1.y + rect1.height > rect2.y
	LDA rect1_y
	CLC
	ADC rect1_height
	STA scratchptr
	LDA rect2_y
	CMP scratchptr
	BCS CheckRectNoCollision
	
	;there was a collision
	LDA #$01
	RTS
CheckRectNoCollision:
	;there was no collision
	LDA #$00
	RTS

;;enemy physics section

IsEnemyXVelZero:
	LDA enemylist+enemy_xvel, Y
	ORA enemylist+enemy_xvelfract, Y
	BNE EnemyXVelNotZero
	; it is zero
	LDA #$00
	RTS
EnemyXVelNotZero:
	; it isn't zero
	LDA #$01
IsEnemyXVelNotZeroDone:
	RTS
	
IsEnemyXVelNeg:
	LDA enemylist+enemy_xvel, Y
	BMI EnemyXVelNeg
	LDA enemylist+enemy_xvelfract, Y
	BMI EnemyXVelNeg
	;it isn't negative
	LDA #$01
	RTS
EnemyXVelNeg:
	; it is neg
	LDA #$00
IsEnemyXVelNegDone:
	RTS
	
ApplyEnemyXVel:
	;Take the velocity from the fractional portion
	;and apply it to the integral portion
	LDA enemylist+enemy_xvelfract, Y
	BMI ApplyEnemyXVelNeg ;if negative, make sure to keep it negative
	LSR A
	LSR A
	CLC
	ADC enemylist+enemy_xvel, Y
	STA enemylist+enemy_xvel, Y
	;remove the part we just added
	LDA enemylist+enemy_xvelfract, Y
	AND #%00000011
	STA enemylist+enemy_xvelfract, Y
	RTS
ApplyEnemyXVelNeg:
	;offset due to two's compliment
	SEC
	SBC #$01
	LSR A
	ORA #%10000000
	LSR A
	ORA #%10000000
	CLC
	ADC #$01
	CLC
	ADC enemylist+enemy_xvel, Y
	STA enemylist+enemy_xvel, Y
	;remove the part we just added
	LDA enemylist+enemy_xvelfract, Y
	AND #%00000011
	BEQ ApplyEnemyXVelNegDone ;if it's just 0, then don't two's compliment it
	;apply 1's to make it negative
	ORA #%11111100
ApplyEnemyXVelNegDone:
	STA enemylist+enemy_xvelfract, Y
	RTS
	
IsEnemyYVelZero:
	LDA enemylist+enemy_yvel, Y
	ORA enemylist+enemy_yvelfract, Y
	BNE EnemyYVelNotZero
	; it is zero
	LDA #$00
	RTS
EnemyYVelNotZero:
	; it isn't zero
	LDA #$01
IsEnemyYVelNotZeroDone:
	RTS
	
IsEnemyYVelNeg:
	LDA enemylist+enemy_yvel, Y
	BMI EnemyYVelNeg
	LDA enemylist+enemy_yvelfract, Y
	BMI EnemyYVelNeg
	;it isn't negative
	LDA #$01
	RTS
EnemyYVelNeg:
	; it is neg
	LDA #$00
IsEnemyYVelNegDone:
	RTS
	
ApplyEnemyYVel:
	;Take the velocity from the fractional portion
	;and apply it to the integral portion
	LDA enemylist+enemy_yvelfract, Y
	BMI ApplyEnemyYVelNeg ;if negative, make sure to keep it negative
	LSR A
	LSR A
	CLC
	ADC enemylist+enemy_yvel, Y
	STA enemylist+enemy_yvel, Y
	;remove the part we just added
	LDA enemylist+enemy_yvelfract, Y
	AND #%00000011
	STA enemylist+enemy_yvelfract, Y
	RTS
ApplyEnemyYVelNeg:
	;offset due to two's compliment
	SEC
	SBC #$01
	LSR A
	ORA #%10000000
	LSR A
	ORA #%10000000
	CLC
	ADC #$01
	CLC
	ADC enemylist+enemy_yvel, Y
	STA enemylist+enemy_yvel, Y
	;remove the part we just added
	LDA enemylist+enemy_yvelfract, Y
	AND #%00000011
	BEQ ApplyEnemyYVelNegDone ;if it's just 0, then don't two's compliment it
	;apply 1's to make it negative
	ORA #%11111100
ApplyEnemyYVelNegDone:
	STA enemylist+enemy_yvelfract, Y
	RTS

;;end enemy section



ApplyPlayerGrav:
	LDA player1dashing
	BNE ApplyPlayerNoGrav;no gravity when dashing
	LDA player1yaccel
	CLC
	ADC #$01
	STA player1yaccel
	RTS
ApplyPlayerNoGrav:
	LDA #$00
	STA player1yvel
	STA player1yvelfract
	STA player1yaccel
	RTS

ResetPlayerAcceleration:
	;If no buttons are being pushed, there should be no acceleration
	;along the x axis
	LDA #$00
	STA player1xaccel
	STA player1yaccel
	RTS
	
	

IsPlayer1XVelZero:
	LDA player1xvel
	ORA player1xvelfract
	BNE Player1XVelNotZero
	; it is zero
	LDA #$00
	RTS
Player1XVelNotZero:
	; it isn't zero
	LDA #$01
IsPlayer1XVelNotZeroDone:
	RTS
	
IsPlayer1XVelNeg:
	LDA player1xvel
	BMI Player1XVelNeg
	LDA player1xvelfract
	BMI Player1XVelNeg
	;it isn't negative
	LDA #$01
	RTS
Player1XVelNeg:
	; it is neg
	LDA #$00
IsPlayer1XVelNegDone:
	RTS
	
ApplyPlayer1XVel:
	;Take the velocity from the fractional portion
	;and apply it to the integral portion
	LDA player1xvelfract
	BMI ApplyPlayer1XVelNeg ;if negative, make sure to keep it negative
	LSR A
	LSR A
	CLC
	ADC player1xvel
	STA player1xvel
	;remove the part we just added
	LDA player1xvelfract
	AND #%00000011
	STA player1xvelfract
	RTS
ApplyPlayer1XVelNeg:
	;offset due to two's compliment
	SEC
	SBC #$01
	LSR A
	ORA #%10000000
	LSR A
	ORA #%10000000
	CLC
	ADC #$01
	CLC
	ADC player1xvel
	STA player1xvel
	;remove the part we just added
	LDA player1xvelfract
	AND #%00000011
	BEQ ApplyPlayer1XVelNegDone ;if it's just 0, then don't two's compliment it
	;apply 1's to make it negative
	ORA #%11111100
ApplyPlayer1XVelNegDone:
	STA player1xvelfract
	RTS
	
IsPlayer1YVelZero:
	LDA player1yvel
	ORA player1yvelfract
	BNE Player1YVelNotZero
	; it is zero
	LDA #$00
	RTS
Player1YVelNotZero:
	; it isn't zero
	LDA #$01
IsPlayer1YVelNotZeroDone:
	RTS
	
IsPlayer1YVelNeg:
	LDA player1yvel
	BMI Player1YVelNeg
	LDA player1yvelfract
	BMI Player1YVelNeg
	;it isn't negative
	LDA #$01
	RTS
Player1YVelNeg:
	; it is neg
	LDA #$00
IsPlayer1YVelNegDone:
	RTS
	
ApplyPlayer1YVel:
	;Take the velocity from the fractional portion
	;and apply it to the integral portion
	LDA player1yvelfract
	BMI ApplyPlayer1YVelNeg ;if negative, make sure to keep it negative
	LSR A
	LSR A
	CLC
	ADC player1yvel
	STA player1yvel
	;remove the part we just added
	LDA player1yvelfract
	AND #%00000011
	STA player1yvelfract
	RTS
ApplyPlayer1YVelNeg:
	;offset due to two's compliment
	SEC
	SBC #$01
	LSR A
	ORA #%10000000
	LSR A
	ORA #%10000000
	CLC
	ADC #$01
	CLC
	ADC player1yvel
	STA player1yvel
	;remove the part we just added
	LDA player1yvelfract
	AND #%00000011
	BEQ ApplyPlayer1YVelNegDone ;if it's just 0, then don't two's compliment it
	;apply 1's to make it negative
	ORA #%11111100
ApplyPlayer1YVelNegDone:
	STA player1yvelfract
	RTS

ApplyPlayer1Phys:
	;; - SPAGHETTI WARNING -
	;; The following section of code may contain
	;; hard-to-follow branching, may assume
	;; prior conditions (i.e. it expects a certain
	;; subroutine to be called before or after its
	;; own execution in order to initialize values),
	;; or may just be hard to follow in general.
	;; Edit at your own risk.

	; simulate friction and acceleration
	
Player1XAccel:
	;first check for dashing
	LDA player1dashing
	BEQ xvelacc
	;handle dash acceleration
	LDA player1mirrored
	BNE DashLeft
DashRight:
	LDA #$02
	STA player1xaccel
	JMP xvelacc
DashLeft:
	LDA #$-02
	STA player1xaccel

xvelacc:
	;x vel/acc friction
	LDA player1xaccel
	BNE Player1XApplyAccel
Player1XApplyFrict:
	LDA player1falling
	BNE Player1YAccel 	;if player is in the air, don't apply friction
	LDA player1hurt		;if player is in hurt animation, don't apply friction
	BNE Player1YAccel
	JSR IsPlayer1XVelZero
	BEQ Player1YAccel
	LDA player1xvel
	BMI Player1XApplyFrictNeg
Player1XApplyFrictPos:
	LDA player1xvelfract
	SEC
	SBC #$02
	STA player1xvelfract
	;correct friction to make sure it doesn't add velocity
	LDA player1xvel
	PHA
	LDA player1xvelfract
	PHA
	
	JSR ApplyPlayer1XVel
	LDA player1xvel
	BMI Player1XFrictCorrect
	JMP Player1XFrictCorrectDone
Player1XFrictCorrect:
	PLA ;remove temp stored values
	PLA
	;set velocity to zero
	LDA #$00
	STA player1xvel
	STA player1xvelfract
	JMP Player1YAccel
Player1XFrictCorrectDone:
	PLA
	STA player1xvelfract
	PLA
	STA player1xvel
	JMP Player1YAccel
Player1XApplyFrictNeg:
	LDA player1xvelfract
	CLC
	ADC #$02
	STA player1xvelfract
	;correct friction to make sure it doesn't add velocity
	LDA player1xvel
	PHA
	LDA player1xvelfract
	PHA
	
	JSR ApplyPlayer1XVel
	LDA player1xvel
	BMI Player1XFrictCorrectDone
	PLA ;remove temp stored values
	PLA
	;set velocity to zero
	LDA #$00
	STA player1xvel
	STA player1xvelfract
	JMP Player1YAccel
Player1XApplyAccel:
	;Apply the acceleration
	LDA player1xvelfract
	CLC
	ADC player1xaccel
	STA player1xvelfract

Player1YAccel:
	;y vel/acc friction
	LDA player1yaccel
	BNE Player1YApplyAccel
	JMP Player1ApplyVels ;no friction in the air
Player1YApplyAccel:
	;Now apply the acceleration
	LDA player1yaccel
	CLC
	ADC player1yvelfract
	STA player1yvelfract
	
Player1ApplyVels:
	JSR ApplyPlayer1XVel
	JSR ApplyPlayer1YVel



	; first make sure speed limit is enforced
	
	; y speed limit check
	BIT player1yvel
	BMI YSpeedLimitMinus
	LDA player1yvel
	CMP #$05
	BCC YSpeedLimitDone
	LDA #$04
	JMP YSpeedLimitDone
YSpeedLimitMinus:
	LDA player1yvel
	CMP #$-04
	BCS YSpeedLimitDone
	LDA #$-04
YSpeedLimitDone:
	STA player1yvel
	
	; x speed limit check
	; first check if player's walking or dashing
	LDA player1dashing
	BNE XSpeedLimitDashing
	;walking
	BIT player1xvel
	BMI XSpeedLimitMinusWalking
	LDA player1xvel
	CMP #$02
	BCC XSpeedLimitDone
	LDA #$01
	JMP XSpeedLimitDone
XSpeedLimitMinusWalking:
	LDA player1xvel
	CMP #$-01
	BCS XSpeedLimitDone
	LDA #$-01
	JMP XSpeedLimitDone
	
XSpeedLimitDashing:
	;dashing
	BIT player1xvel
	BMI XSpeedLimitMinus
	LDA player1xvel
	CMP #$03
	BCC XSpeedLimitDone
	LDA #$02
	JMP XSpeedLimitDone
XSpeedLimitMinus:
	LDA player1xvel
	CMP #$-02
	BCS XSpeedLimitDone
	LDA #$-02
XSpeedLimitDone:
	STA player1xvel
	
	; apply current velocities
	LDA player1y
	CLC
	ADC player1yvel
	STA player1y

	LDA player1x
	CLC
	ADC player1xvel
	;If player is on the very edge of the screen, keep him
	;from going farther
	CMP #$F8 ;comparing to the very last values of player1x
			 ;due to wraparound
	BCS ClampPlayerx
	STA player1x
	JMP HandleScrolling
	
ClampPlayerx:
	LDA #$00
	STA player1x

HandleScrolling:
	;Handle any scrolling
	LDA player1x
	CMP #SCREEN_SCROLL_REGION
	BCC ApplyPlayer1PhysDone;we are behind the line, no need to scroll
	BEQ ApplyPlayer1PhysDone;we are standing on the line, no need to scroll

	;calculate how much to scroll by
	LDA player1x
	SEC
	SBC #SCREEN_SCROLL_REGION
	STA scratchptr
	BEQ ApplyPlayer1PhysDone ;if we're adding zero to the scroll, don't do anything
	CMP #$02
	BNE ContinueScrollAdjust
	;if scroll amount is 2, we need to make sure scroll is even,
	;otherwise we risk skipping over the new column check
	LDA #%00000001
	BIT scroll
	BEQ ContinueScrollAdjust ;scroll is even, continue
	;scroll is not even, adjust the $02 down to a $01 to align it
	DEC scratchptr
ContinueScrollAdjust:
	LDA scroll
	CLC
	ADC scratchptr
	STA scroll
	;set the scrolling variable
	;to let the NMI next frame know to check for new columns
	LDA #01
	STA scrolling
	;snap player x position back to middle of screen
	LDA #SCREEN_SCROLL_REGION
	STA player1x
ApplyPlayer1PhysDone:
	RTS


SetColumnNumberToPlayer:
	;calculate the current column number of player1 by using
	;player1x, store in columnNumber
	LDA player1x ;divide player1x by 8 to get the screen tile number
	LSR A
	LSR A
	LSR A
	STA scratchptr ;store resulting tile number in scratch variable
	LDA columnNumber ;columnNumber is the column on the far right side of the screen,
					;so subtract by $21 to get the column number on the left hand side of the
					;current screen
	SEC
	SBC #$21
	;now add that value to the tile value we just calculated from player1x
	CLC
	ADC scratchptr
	AND #%01111111 ;clamp value
	;and store it in columnNumber
	STA columnNumber
	RTS
	
SetRowNumberToPlayer:
	;calculate the current row number of player1 by using
	;player1y, store in columnNumber
	LDA player1y ;divide player1x by 8 to get the screen tile number
	LSR A
	LSR A
	LSR A
	STA rowNumber ;store it into rowNumber
	RTS
	
;; enemy platform functions
SetColumnNumberToEnemy:
	;calculate the current column number of player1 by using
	;player1x, store in columnNumber
	LDA enemylist+enemy_x, Y ;divide player1x by 8 to get the screen tile number
	LSR A
	LSR A
	LSR A
	STA scratchptr ;store resulting tile number in scratch variable
	LDA columnNumber ;columnNumber is the column on the far right side of the screen,
					;so subtract by $21 to get the column number on the left hand side of the
					;current screen
	SEC
	SBC #$21
	;now add that value to the tile value we just calculated from player1x
	CLC
	ADC scratchptr
	AND #%01111111 ;clamp value
	;and store it in columnNumber
	STA columnNumber
	RTS
	
SetRowNumberToEnemy:
	;calculate the current row number of player1 by using
	;player1y, store in columnNumber
	LDA enemylist+enemy_y, Y ;divide player1x by 8 to get the screen tile number
	LSR A
	LSR A
	LSR A
	STA rowNumber ;store it into rowNumber
	RTS


CheckEnemyPlatformCollisions:
	;Move the enemy offset stored in Y to X as well, in order
	;to use the INC opcode correctly
	TYA
	TAX

	;align enemy y to tiles by adding by 1
	INC enemylist+enemy_y, X
	;no need to check if enemy is falling for now
	;account for scrolling
	LDA scroll
	AND #%00000111
	CLC
	ADC enemylist+enemy_x, Y
	STA enemylist+enemy_x, Y

	JSR CheckEnemyPlatformCollision
	
	;restore enemy x
	LDA scroll
	AND #%00000111
	STA scratchptr
	LDA enemylist+enemy_x, Y
	SEC
	SBC scratchptr
	STA enemylist+enemy_x, Y
	;restore enemy y
	DEC enemylist+enemy_y, X
	RTS
	
CheckEnemyPlatformCollision:

	LDA columnNumber
	PHA
	
	JSR SetRowNumberToEnemy
	JSR SetColumnNumberToEnemy
	
	JSR EnemyPlatformCollisDown
	
	JSR EnemyPlatformCollisRight
	
	JSR EnemyPlatformCollisLeft
	
	;No need to check for updwards collisions
	
	PLA
	STA columnNumber
CheckEnemyPlatformCollisDone
	RTS
	
EnemyPlatformCollisDown:
	;; Assumes rowNumber and columnNumber currently point to upper-left of player
	;; (first sprite)
	;save rowNumber and columnNumber
	LDA rowNumber
	PHA
	LDA columnNumber
	PHA
	
	;If moving up, ignore downward collisions
	LDA buttons1
	AND #%00001000
	BNE EnemyPlatformCollisDownDone

	
	;Set row number to directly below player
	INC rowNumber
	INC rowNumber
	
	;First tile under player
	JSR EnemyStaticDownCollis

	;Second tile under player
	INC columnNumber
	JSR EnemyStaticDownCollis

	;Possible third tile under player
	LDA enemylist+enemy_x, Y
	AND #%0000111
	BEQ EnemyPlatformCollisDownDone
	INC columnNumber
	JSR EnemyStaticDownCollis
	
EnemyPlatformCollisDownDone:
	;restore rowNumber and columnNumber
	PLA
	STA columnNumber
	PLA
	STA rowNumber
	RTS
	
EnemyStaticDownCollis:
	JSR GetTileFromCurrentColumnRow
	CMP #$30 ; for now only tile $30 is collidable
	BNE EnemyStaticDownCollisDone
	;We have collided with a downward tile, so move the player back up
	LDA enemylist+enemy_y, Y
	AND #%11111000
	STA enemylist+enemy_y, Y
	;no need to mark falling or jumping
EnemyStaticDownCollisDone:
	RTS
	
	
EnemyPlatformCollisRight:
	;; Assumes rowNumber and columnNumber currently point to upper-left of player
	;; (first sprite)
	;save rowNumber and columnNumber
	LDA rowNumber
	PHA
	LDA columnNumber
	PHA
	
	;Set row number to directly below player
	INC columnNumber
	INC columnNumber
	
	;First tile under player
	JSR EnemyStaticRightCollis

	;Second tile under player
	INC rowNumber
	JSR EnemyStaticRightCollis

	;Possible third tile under player
	LDA enemylist+enemy_y, Y
	AND #%0000111
	BEQ EnemyPlatformCollisRightDone
	INC rowNumber
	JSR EnemyStaticRightCollis
	
EnemyPlatformCollisRightDone:
	;restore rowNumber and columnNumber
	PLA
	STA columnNumber
	PLA
	STA rowNumber
	RTS
	
EnemyStaticRightCollis:
	JSR GetTileFromCurrentColumnRow
	CMP #$30 ; for now only tile $30 is collidable
	BNE EnemyStaticRightCollisDone
	;We have collided with a downward tile, so move the player back up
	LDA enemylist+enemy_x, Y
	AND #%11111000
	STA enemylist+enemy_x, Y
EnemyStaticRightCollisDone:
	RTS
	
EnemyPlatformCollisLeft:
	;; Assumes rowNumber and columnNumber currently point to upper-left of player
	;; (first sprite)
	;save rowNumber and columnNumber
	LDA rowNumber
	PHA
	LDA columnNumber
	PHA
	
	;First tile under player
	JSR EnemyStaticLeftCollis

	;Second tile under player
	INC rowNumber
	JSR EnemyStaticLeftCollis

	;Possible third tile under player
	LDA enemylist+enemy_y, Y
	AND #%0000111
	BEQ EnemyPlatformCollisLeftDone
	INC rowNumber
	JSR EnemyStaticLeftCollis
	
EnemyPlatformCollisLeftDone:
	;restore rowNumber and columnNumber
	PLA
	STA columnNumber
	PLA
	STA rowNumber
	RTS
	
EnemyStaticLeftCollis:
	JSR GetTileFromCurrentColumnRow
	CMP #$30 ; for now only tile $30 is collidable
	BNE EnemyStaticLeftCollisDone
	;We have collided with a downward tile, so move the player back up
	LDA enemylist+enemy_x, Y
	AND #%11111000
	CLC
	ADC #%00001000
	STA enemylist+enemy_x, Y
EnemyStaticLeftCollisDone:
	RTS

; end enemy platform functions
	
	
CheckPlatformCollisions:
	;align player1y to tiles by adding by 1
	INC player1y
	;set player to falling, later if a down collision is detected,
	;then it'll set falling back to $00
	LDA #$01
	STA player1falling
	;account for scrolling
	LDA scroll
	AND #%00000111
	CLC
	ADC player1x
	STA player1x

	JSR CheckPlayerPlatformCollisions
	
	;restore player1x
	LDA scroll
	AND #%00000111
	STA scratchptr
	LDA player1x
	SEC
	SBC scratchptr
	STA player1x
	;restore player1y
	DEC player1y
	RTS
	
CheckPlayerPlatformCollisions:

	LDA columnNumber
	PHA
	
	JSR SetRowNumberToPlayer
	JSR SetColumnNumberToPlayer
	
	JSR PlayerPlatformCollisDown
	
	JSR PlayerPlatformCollisRight
	
	JSR PlayerPlatformCollisLeft
	
	;No need to check for updwards collisions
	
	PLA
	STA columnNumber
CheckPlayerPlatformCollisDone
	RTS
	
PlayerPlatformCollisDown:
	;; Assumes rowNumber and columnNumber currently point to upper-left of player
	;; (first sprite)
	;save rowNumber and columnNumber
	LDA rowNumber
	PHA
	LDA columnNumber
	PHA
	
	;If moving up, ignore downward collisions
	LDA buttons1
	AND #%00001000
	BNE PlayerPlatformCollisDownDone

	
	;Set row number to directly below player
	INC rowNumber
	INC rowNumber
	
	;First tile under player
	JSR PlayerStaticDownCollis

	;Second tile under player
	INC columnNumber
	JSR PlayerStaticDownCollis

	;Possible third tile under player
	LDA player1x
	AND #%0000111
	BEQ PlayerPlatformCollisDownDone
	INC columnNumber
	JSR PlayerStaticDownCollis
	
PlayerPlatformCollisDownDone:
	;restore rowNumber and columnNumber
	PLA
	STA columnNumber
	PLA
	STA rowNumber
	RTS
	
PlayerStaticDownCollis:
	JSR GetTileFromCurrentColumnRow
	CMP #$30 ; for now only tile $30 is collidable
	BNE PlayerStaticDownCollisDone
	;We have collided with a downward tile, so move the player back up
	LDA player1y
	AND #%11111000
	STA player1y
	;also mark that the player is no longer falling or jumping
	LDA #$00
	STA	player1falling
	STA player1jumping
PlayerStaticDownCollisDone:
	RTS
	
	
PlayerPlatformCollisRight:
	;; Assumes rowNumber and columnNumber currently point to upper-left of player
	;; (first sprite)
	;save rowNumber and columnNumber
	LDA rowNumber
	PHA
	LDA columnNumber
	PHA
	
	;If moving left, ignore rightward collisions
	;LDA buttons1
	;AND #%00000010
	;BNE PlayerPlatformCollisRightDone

	
	;Set row number to directly below player
	INC columnNumber
	INC columnNumber
	
	;First tile under player
	JSR PlayerStaticRightCollis

	;Second tile under player
	INC rowNumber
	JSR PlayerStaticRightCollis

	;Possible third tile under player
	LDA player1y
	AND #%0000111
	BEQ PlayerPlatformCollisRightDone
	INC rowNumber
	JSR PlayerStaticRightCollis
	
PlayerPlatformCollisRightDone:
	;restore rowNumber and columnNumber
	PLA
	STA columnNumber
	PLA
	STA rowNumber
	RTS
	
PlayerStaticRightCollis:
	JSR GetTileFromCurrentColumnRow
	CMP #$30 ; for now only tile $30 is collidable
	BNE PlayerStaticRightCollisDone
	;We have collided with a downward tile, so move the player back up
	LDA player1x
	AND #%11111000
	STA player1x
PlayerStaticRightCollisDone:
	RTS
	
PlayerPlatformCollisLeft:
	;; Assumes rowNumber and columnNumber currently point to upper-left of player
	;; (first sprite)
	;save rowNumber and columnNumber
	LDA rowNumber
	PHA
	LDA columnNumber
	PHA
	
	;If moving right, ignore leftward collisions
	;LDA buttons1
	;AND #%00000001
	;BNE PlayerPlatformCollisLeftDone
	
	;First tile under player
	JSR PlayerStaticLeftCollis

	;Second tile under player
	INC rowNumber
	JSR PlayerStaticLeftCollis

	;Possible third tile under player
	LDA player1y
	AND #%0000111
	BEQ PlayerPlatformCollisLeftDone
	INC rowNumber
	JSR PlayerStaticLeftCollis
	
PlayerPlatformCollisLeftDone:
	;restore rowNumber and columnNumber
	PLA
	STA columnNumber
	PLA
	STA rowNumber
	RTS
	
PlayerStaticLeftCollis:
	JSR GetTileFromCurrentColumnRow
	CMP #$30 ; for now only tile $30 is collidable
	BNE PlayerStaticLeftCollisDone
	;We have collided with a downward tile, so move the player back up
	LDA player1x
	AND #%11111000
	CLC
	ADC #%00001000
	STA player1x
PlayerStaticLeftCollisDone:
	RTS
	
;;;;;;;;;;;;;;
; Level engine
;;;;;;;;;;;;;;

GetTileFromCurrentColumnRow:
	;; Uses columnNumber and rowNumber to retrieve the appropriate tile from
	;; the meta-tile map, and stores it into the accumulator A
	;; Call this subroutine with "GetTileFromCurrentSource"
	;; if you have already set sourceLow and sourceHigh to point to the correct column
	JSR GetColumnDataStart
GetTileFromCurrentSource:
	TYA ; save Y register
	PHA

	;divide row number by 2 to get meta-tile row
	LDA rowNumber
	LSR A
	TAY
	LDA [sourceLow], Y ;get appropriate meta-tile
	PHA ; save meta-tile
	
	;get the proper column in the meta-tile
	LDA columnNumber
	AND #%00000001 ; now either 0 or 1, depending on if it was odd or even
	ASL A ; now either 0 or 2
	STA scratchptr
	;now get the proper row in the meta-tile
	LDA rowNumber
	AND #%00000001 ; now either 0 or 1, depending on if it was odd or even
	CLC
	ADC scratchptr ; add to what we got previously to get the final offset
	STA scratchptr

	
	;now we get the actual tile value
	PLA ;get meta-tile
	ASL A ;multiply by 4
	ASL A 
	CLC
	ADC scratchptr ;add the proper offset for the tile we want
	TAY
	LDA metatiles, Y ;load the tile
	STA scratchptr ; temp store
	
	PLA ; restore Y register
	TAY
	;restore return value
	LDA scratchptr
	RTS
	
GetColumnDataStart:
	;; Gets the start of the column data for the column number
	;; currently in columnNumber and stores the pointer inside
	;; sourceLow and sourceHigh

	;convert column number to metatile number
	LDA columnNumber
	AND #%00000001 ; If column number is odd, round down to nearest even number
				   ; so that it's aligned to the meta tiles
	BEQ ConvertToMetaTileEven
	LDA columnNumber
	SEC
	SBC #$01
	JMP ConvertToMetaTile
ConvertToMetaTileEven:
	LDA columnNumber
ConvertToMetaTile:
	LSR A ; column number / 2 = meta tile number
	
	; now we find the offset for the meta tile
	PHA; meta tile number * 16 = column data offset
	ASL A
	ASL A
	ASL A
	ASL A         
	STA sourceLow
	PLA
	LSR A
	LSR A
	LSR A
	LSR A
	STA sourceHigh

	LDA sourceLow       ; column data start + offset = address to load column data from
	CLC 
	ADC #LOW(columnData)
	STA sourceLow
	LDA sourceHigh
	ADC #HIGH(columnData)
	STA sourceHigh
	RTS
	
DrawMetaTileFirstColumn:
	;save Y value
	STA scratchptr
	TYA
	PHA
	LDA scratchptr

	;A register should already have meta-tile number
	ASL A; multiply by 4, since each meta tile is 4 bytes long
	ASL A
	TAY
	LDA metatiles, Y
	STA $2007             ; write to PPU
	LDA metatiles+$01, Y
	STA $2007
	
	;restore Y value
	PLA
	TAY
	RTS
	
DrawMetaTileSecondColumn:
	;save Y value
	STA scratchptr
	TYA
	PHA
	LDA scratchptr

	;A register should already have meta-tile number
	ASL A; multiply by 4, since each meta tile is  bytes long
	ASL A
	TAY
	LDA metatiles+$02, Y
	STA $2007             ; write to PPU
	LDA metatiles+$03, Y
	STA $2007
	
	;restore Y value
	PLA
	TAY
	RTS
	
DrawNewColumn:
	LDA scroll       ; calculate new column address using scroll register
	LSR A
	LSR A
	LSR A            ; shift right 3 times = divide by 8
	STA columnLow    ; $00 to $1F, screen is 32 tiles wide

	LDA nametable     ; calculate new column address using current nametable
	EOR #$01          ; invert low bit, A = $00 or $01
	ASL A             ; shift up, A = $00 or $02
	ASL A             ; $00 or $04
	CLC
	ADC #$20          ; add high byte of nametable base address ($2000)
	STA columnHigh    ; now address = $20 or $24 for nametable 0 or 1

	JSR GetColumnDataStart

DrawColumn:
	LDA #%00000100        ; set to increment +32 mode
	STA $2000

	LDA $2002             ; read PPU status to reset the high/low latch
	LDA columnHigh
	STA $2006             ; write the high byte of column address
	LDA columnLow
	STA $2006             ; write the low byte of column address
	LDX #$0F              ; copy 15 meta tiles
	LDY #$00
	LDA columnNumber
	AND #%00000001 ; If column number is odd, then draw the second column
				   ; of the meta tiles
	BNE DrawColumnLoopOdd
DrawColumnLoop:
	LDA [sourceLow], y
	JSR DrawMetaTileFirstColumn
	INY
	DEX
	BNE DrawColumnLoop
	RTS
DrawColumnLoopOdd:
	LDA [sourceLow], y
	JSR DrawMetaTileSecondColumn
	INY
	DEX
	BNE DrawColumnLoopOdd
	RTS
	
DrawNewAttributes:
	LDA nametable
	EOR #$01          ; invert low bit, A = $00 or $01
	ASL A             ; shift up, A = $00 or $02
	ASL A             ; $00 or $04
	CLC
	ADC #$23          ; add high byte of attribute base address ($23C0)
	STA columnHigh    ; now address = $23 or $27 for nametable 0 or 1

	LDA scroll
	LSR A
	LSR A
	LSR A
	LSR A
	LSR A
	CLC
	ADC #$C0
	STA columnLow     ; attribute base + scroll / 32

	LDA columnNumber  ; (column number / 4) * 8 = column data offset
	AND #%11111100
	ASL A
	STA sourceLow
	LDA columnNumber
	LSR A
	LSR A
	LSR A
	LSR A
	LSR A
	LSR A
	LSR A
	STA sourceHigh

	LDA sourceLow       ; column data start + offset = address to load column data from
	CLC 
	ADC #LOW(attribData)
	STA sourceLow
	LDA sourceHigh
	ADC #HIGH(attribData)
	STA sourceHigh

	LDY #$00
	LDA $2002             ; read PPU status to reset the high/low latch
DrawNewAttributesLoop
	LDA columnHigh
	STA $2006             ; write the high byte of column address
	LDA columnLow
	STA $2006             ; write the low byte of column address
	LDA [sourceLow], y    ; copy new attribute byte
	STA $2007

	INY
	CPY #$08              ; copy 8 attribute bytes
	BEQ DrawNewAttributesLoopDone 

	LDA columnLow         ; next attribute byte is at address + 8
	CLC
	ADC #$08
	STA columnLow
	JMP DrawNewAttributesLoop
DrawNewAttributesLoopDone:

  rts
	
;;;;;;;;;;;;;;
; Sprite updates
;;;;;;;;;;;;;;
UpdateSprites:
	JSR UpdatePlayer1Sprite
	RTS
		
UpdateEnemySpritePosition:
	;Parameters - Expects enemyptr to point to offset from enemylist of current enemy
	
	;get enemylist offset, store in Y
	LDA enemyptr
	TAY

	;get sprite offset for enemy, store in X
	LDA enemylist+enemy_spr_ptr, Y
	TAX

	;update the sprite
	JSR UpdateSpritePosition2x2

	RTS
	
UpdatePlayer1Sprite:
	JSR UpdatePlayer1SpritePosition
	JSR UpdatePlayer1SpriteAnimation
	RTS
	
UpdatePlayer1SpritePosition:
	;Player1 is made up of 4 sprites,
	;so move them accordingly

	;Do different things for mirrored player
	LDA player1mirrored
	BNE UpdatePlayer1SpritePositionMirrored
	
	;Unmirror all sprites
	JSR UnmirrorPlayer1Sprites
	
	;y axis
	LDA player1y
	STA PLAYER_1_SPRITE		;upper left sprite
	STA PLAYER_1_SPRITE+$04	;upper right sprite
	CLC
	ADC #$08
	STA PLAYER_1_SPRITE+$08 ;bottom left sprite
	STA PLAYER_1_SPRITE+$0C ;bottom right sprite
	
	;x axis
	LDA player1x
	STA PLAYER_1_SPRITE+$03	;upper left sprite
	STA PLAYER_1_SPRITE+$0B	;bottom left sprite
	CLC
	ADC #$08
	STA PLAYER_1_SPRITE+$07	;upper right sprite
	STA PLAYER_1_SPRITE+$0F ;bottom right sprite
	
	RTS
	
UpdatePlayer1SpritePositionMirrored:
	;Mirrored
	
	;Mirror all sprites
	JSR MirrorPlayer1Sprites
	
	;y axis
	LDA player1y
	STA PLAYER_1_SPRITE		;upper left sprite
	STA PLAYER_1_SPRITE+$04	;upper right sprite
	CLC
	ADC #$08
	STA PLAYER_1_SPRITE+$08 ;bottom left sprite
	STA PLAYER_1_SPRITE+$0C ;bottom right sprite
	
	;x axis
	LDA player1x
	STA PLAYER_1_SPRITE+$07	;upper right sprite
	STA PLAYER_1_SPRITE+$0F ;bottom right sprite
	CLC
	ADC #$08
	STA PLAYER_1_SPRITE+$03	;upper left sprite
	STA PLAYER_1_SPRITE+$0B	;bottom left sprite
	
	RTS

MirrorPlayer1Sprites:
	LDA PLAYER_1_SPRITE+$02
	ORA #%01000000
	STA PLAYER_1_SPRITE+$02
	
	LDA PLAYER_1_SPRITE+$06
	ORA #%01000000
	STA PLAYER_1_SPRITE+$06
	
	LDA PLAYER_1_SPRITE+$0A
	ORA #%01000000
	STA PLAYER_1_SPRITE+$0A
	
	LDA PLAYER_1_SPRITE+$0E
	ORA #%01000000
	STA PLAYER_1_SPRITE+$0E
	RTS

UnmirrorPlayer1Sprites:
	LDA PLAYER_1_SPRITE+$02
	AND #%10111111
	STA PLAYER_1_SPRITE+$02
	
	LDA PLAYER_1_SPRITE+$06
	AND #%10111111
	STA PLAYER_1_SPRITE+$06
	
	LDA PLAYER_1_SPRITE+$0A
	AND #%10111111
	STA PLAYER_1_SPRITE+$0A
	
	LDA PLAYER_1_SPRITE+$0E
	AND #%10111111
	STA PLAYER_1_SPRITE+$0E
	RTS

;Enemy sprite functions

UpdateSpritePosition2x2:
	;Player1 is made up of 4 sprites,
	;so move them accordingly

	;Do different things for mirrored player
	LDA enemylist+enemy_mirrored, Y
	BNE UpdateSpritePositionMirrored
	
	;Unmirror all sprites
	JSR UnmirrorSprites
	
	;y axis
	LDA enemylist+enemy_y, Y
	STA $0200,X		;upper left sprite
	STA $0204,X		;upper right sprite
	CLC
	ADC #$08
	STA $0208,X ;bottom left sprite
	STA $020C,X ;bottom right sprite
	
	;x axis
	LDA enemylist+enemy_x, Y
	STA $0203,X	;upper left sprite
	STA $020B,X	;bottom left sprite
	CLC
	ADC #$08
	STA $0207,X	;upper right sprite
	STA $020F,X ;bottom right sprite
	
	RTS
	
UpdateSpritePositionMirrored:
	;Mirrored
	
	;Mirror all sprites
	JSR MirrorSprites
	
	;y axis
	LDA enemylist+enemy_y, Y
	STA $0200,X		;upper left sprite
	STA $0204,X	;upper right sprite
	CLC
	ADC #$08
	STA $0208,X ;bottom left sprite
	STA $020C,X ;bottom right sprite
	
	;x axis
	LDA enemylist+enemy_x, Y
	STA $0207,X	;upper right sprite
	STA $020F,X ;bottom right sprite
	CLC
	ADC #$08
	STA $0203,X	;upper left sprite
	STA $020B,X	;bottom left sprite
	
	RTS

MirrorSprites:
	LDA $0202,X
	ORA #%01000000
	STA $0202,X
	
	LDA $0206,X
	ORA #%01000000
	STA $0206
	
	LDA $020A,X
	ORA #%01000000
	STA $020A,X
	
	LDA $020E,X
	ORA #%01000000
	STA $020E,X
	RTS

UnmirrorSprites:
	LDA $0202,X
	AND #%10111111
	STA $0202,X
	
	LDA $0206,X
	AND #%10111111
	STA $0206,X
	
	LDA $020A,X
	AND #%10111111
	STA $020A,X
	
	LDA $020E,X
	AND #%10111111
	STA $020E,X
	RTS

	
;;;;;;;;;;;;;;
; Animation engine
;;;;;;;;;;;;;;

Player1GetHurt:
	;Parameter: A indicates whether player will get pushed to the left ($00)
	;			or right ($01) when hurt
	
	;push player towards proper direction
	BNE Player1GetHurtToRight
	LDA #-$01
	STA scratchptr
	JMP Player1GetHurtStart
Player1GetHurtToRight:
	LDA #$01
	STA scratchptr
	
Player1GetHurtStart:
	LDA player1invinctimer
	CMP #PLAYER_1_INVINC_BLINK_TIME
	BCC Player1GetHurtDone
	;player got hurt, initalize appropriate variables
	LDA #$00
	STA player1invinctimer
	LDA #$03 ;player hurt animation
	STA player1anim
	LDA #$01 ;set player hurt and invincible variables
	STA player1hurt
	STA player1invinc
	;make player get knocked back
	JSR Player1GetKnockback
Player1GetHurtDone:
	RTS
	
Player1GetKnockback:
	;Parameter: the first byte of
	;			scratchptr is the velocity added to playerx to push him in that direction
	
	LDA player1knockbacktimer
	CMP #PLAYER_1_KNOCKBACK_BLINK_TIME
	BCC Player1GetKnockbackDone
	;player got knocked back, initalize appropriate variables
	LDA #$00
	STA player1knockbacktimer
	STA player1dashing ;stop any dashing
	STA player1xvelfract ;stop horizontal movement
	STA player1yvelfract ;stop vertical movement
	LDA #$01 ;set player knockback variable
	STA player1knockback
	;push player towards proper direction
	LDA scratchptr
	STA player1xvel
	;make player jump slightly
	LDA #-$02
	STA player1yvel
Player1GetKnockbackDone:
	RTS

UpdateBoombaSpriteAnimation:
	;Use a jump table to decide which animation to draw
	LDA enemylist+enemy_anim, Y
	ASL A; multiply by 2, since each address is 2 bytes long
	TAX
	LDA boombaanimtable, X
	STA scratchptr
	LDA boombaanimtable+1, X
	STA scratchptr+1
	JMP [scratchptr]
	; animation subroutine is expected to call RTS,
	; so no RTS needed here

	;Jump table for player 1 animations
boombaanimtable:
	.dw BoombaWalking ; $00
	.dw BoombaHurt ; $01
	
BoombaWalking:
	;get sprite offset
	LDA enemylist+enemy_spr_ptr, Y
	TAX

	; Restore sprites to default
	LDA #$10
	STA $0201, X
	LDA #$11
	STA $0205, X
	LDA #$12
	STA $0209, X
	LDA #$13
	STA $020D, X
	RTS

BoombaHurt:
	;get sprite offset
	LDA enemylist+enemy_spr_ptr, Y
	TAX

	; Restore sprites to default
	LDA #$14
	STA $0201, X
	LDA #$15
	STA $0205, X
	LDA #$16
	STA $0209, X
	LDA #$17
	STA $020D, X
	RTS

UpdatePlayer1SpriteAnimation:
	;Use a jump table to decide which animation to draw
	LDA player1anim
	ASL A; multiply by 2, since each address is 2 bytes long
	TAX
	LDA player1animtable, X
	STA scratchptr
	LDA player1animtable+1, X
	STA scratchptr+1
	JMP [scratchptr]
	; animation subroutine is expected to call RTS,
	; so no RTS needed here

	;Jump table for player 1 animations
player1animtable:
	.dw Player1Standing ; $00
	.dw Player1AnimWalk ; $01
	.dw Player1AnimDash	; $02
	.dw Player1HurtAnim	; $03

Player1HurtAnim:
	LDA #$20
	STA PLAYER_1_SPRITE + $01
	LDA #$21
	STA PLAYER_1_SPRITE + $05
	LDA #$22
	STA PLAYER_1_SPRITE + $09
	LDA #$23
	STA PLAYER_1_SPRITE + $0D
	
	RTS
	
Player1Standing:
	; Restore sprites to default
	LDA #$00
	STA PLAYER_1_SPRITE + $01
	LDA #$01
	STA PLAYER_1_SPRITE + $05
	LDA #$02
	STA PLAYER_1_SPRITE + $09
	LDA #$03
	STA PLAYER_1_SPRITE + $0D
	
	RTS
	
Player1AnimDash:
	; Load dashing sprites
	LDA #$0C
	STA PLAYER_1_SPRITE + $01
	LDA #$0D
	STA PLAYER_1_SPRITE + $05
	LDA #$0E
	STA PLAYER_1_SPRITE + $09
	LDA #$0F
	STA PLAYER_1_SPRITE + $0D
	RTS
	
Player1AnimWalk:
	;Take care of resetting timers, etc...
	LDA #PLAYER_1_ANIM_WALK_SPEED
	CMP player1animtimer
	BCS Player1AnimWalkCont
	LDA #$00 ; resetting animation timer for next frame
	STA player1animtimer
	INC player1animframe
	LDA #PLAYER_1_ANIM_WALK_FRAMES ; check if we've run through all the frames
	CMP player1animframe
	BCS Player1AnimWalkCont
	LDA #$00 ; go back to first frame
	STA player1animframe
Player1AnimWalkCont:
	; Use a jump table to decide which frame to draw
	LDA player1animframe
	ASL A; multiply by 2, since each address is 2 bytes long
	TAX
	LDA Player1WalkFrameTable, X
	STA scratchptr
	LDA Player1WalkFrameTable+1, X
	STA scratchptr+1
	JMP [scratchptr]
	; animation subroutine is expected to call RTS,
	; so no RTS needed here
	
Player1WalkFrameTable:
	.dw Player1WalkFrame1
	.dw Player1WalkFrame2
	
Player1WalkFrame1:
	LDA #$00
	STA PLAYER_1_SPRITE + $01
	LDA #$01
	STA PLAYER_1_SPRITE + $05
	LDA #$04
	STA PLAYER_1_SPRITE+$09
	LDA #$05
	STA PLAYER_1_SPRITE+$0D
	RTS
Player1WalkFrame2:
	LDA #$00
	STA PLAYER_1_SPRITE + $01
	LDA #$01
	STA PLAYER_1_SPRITE + $05
	LDA #$06
	STA PLAYER_1_SPRITE+$09
	LDA #$07
	STA PLAYER_1_SPRITE+$0D
	RTS
  
;;;;;;;;;;;;;;
; Input handling functions
;;;;;;;;;;;;;;
HandlePlayerInput:
	JSR HandlePlayer1Input
	RTS
	
HandlePlayer1Input:
	;; - SPAGHETTI WARNING -
	;; The following section of code may contain
	;; hard-to-follow branching, may assume
	;; prior conditions (i.e. it expects a certain
	;; subroutine to be called before or after its
	;; own execution in order to initialize values),
	;; or may just be hard to follow in general.
	;; Edit at your own risk.
	
	;If currently being knocked back, don't allow movement
	LDA player1knockback
	CMP #$01
	BNE HandlePlayer1InputUp
	JMP HandlePlayer1InputDone

HandlePlayer1InputUp:
	LDA #%00001000 ; check for up button
	BIT buttons1
	BEQ HandlePlayer1InputDown ;branch if up not pressed
	LDA #$-01
	STA player1yaccel
HandlePlayer1InputDown:
	LDA #%00000100 ; check for down button
	BIT buttons1
	BEQ HandlePlayer1InputLeft ;branch if up not pressed
	LDA #$01
	STA player1yaccel
HandlePlayer1InputLeft:
	;if dashing, ignore horizontal inputs
	LDA player1dashing
	BNE HandlePlayer1InputA
	LDA #%00000010 ; check for left button
	BIT buttons1
	BEQ HandlePlayer1InputRight ;branch if up not pressed
	LDA #$-01
	STA player1xaccel
	; Mirror player sprites
	LDA #$01
	STA player1mirrored
	;start walking animation
	LDA #$01
	CMP player1anim
	BEQ HandlePlayer1InputRight
	STA player1anim
	;make sure animation is started properly
	LDA #$00
	STA player1animframe
	STA player1animtimer
HandlePlayer1InputRight:
	LDA #%00000001 ; check for right button
	BIT buttons1
	BEQ HandlePlayer1InputHoriz ;branch if up not pressed
	LDA #$01
	STA player1xaccel
	; Unmirror player sprites
	LDA #$00
	STA player1mirrored
	;start walking animation
	LDA #$01
	CMP player1anim
	BEQ HandlePlayer1InputA
	STA player1anim
	;make sure animation is started properly
	LDA #$00
	STA player1animframe
	STA player1animtimer
	JMP HandlePlayer1InputA
HandlePlayer1InputHoriz:
	LDA #%0001111 ;check if ANY of the directional buttons are pressed
	BIT buttons1
	BNE HandlePlayer1InputA ;there was at least 1 directional button pressed
HandlePlayer1InputHorizNone:	
	;no horizontal buttons pressed, so stop the walking animation
	;if walking
	LDA player1anim
	CMP #$01
	BNE HandlePlayer1InputA
	LDA #$00
	STA player1anim
HandlePlayer1InputA:
	LDA #%10000000 ; check for A button
	BIT buttons1
	BEQ HandlePlayer1InputANone ;branch if up not pressed
	LDA player1jumping      ;if in the middle of jumping, continue
	BNE Player1JumpRoutine
	LDA player1falling
	BNE HandlePlayer1InputB ;if falling, don't allow the player to jump
	LDA player1canjump
	BEQ HandlePlayer1InputB ;only execute the following section of player can jump
							;or is currently jumping
Player1JumpRoutine:
	INC player1jumpingtimer
	LDA player1jumpingtimer
	CMP #PLAYER_1_JUMP_TIMER_LIM
	BCS Player1ResetJumping ;if jump timer is over, don't allow jumping
	;TODO: add code for jump timer to make jump
	;last longer depending on how long
	;a is held
	LDA #$01
	STA player1jumping
	STA player1falling
	LDA #$00
	STA player1canjump ;don't let the player jump again
	LDA #$-03
	STA player1yvel
	JMP HandlePlayer1InputB
HandlePlayer1InputANone:
	;can jump again if already hit the ground
	LDA player1falling
	BNE Player1ResetJumping
	LDA #$01
	STA player1canjump
Player1ResetJumping:
	LDA #$00
	STA player1jumpingtimer
	;A was not pressed, so not jumping
	LDA #$00
	STA player1jumping

HandlePlayer1InputB:
	LDA #%01000000 ; check for B button
	BIT buttons1
	BEQ HandlePlayer1InputDone ;branch if up not pressed
	;start dashing if the timer is over
	LDA player1dashtimer
	CMP #PLAYER_1_DASH_BLINK_TIME
	BCC HandlePlayer1InputDone ; still in recovery period
	LDA #$01
	STA player1dashing
	;start dashing animation
	LDA #$02
	STA player1anim
	;make sure animation is started properly
	LDA #$00
	STA player1animframe
	STA player1animtimer
	;reset dashtimer
	LDA #$00
	STA player1dashtimer
HandlePlayer1InputDone:
	RTS
	
	
;;;;;;;;;;;;;;
; Controller reading functions
;;;;;;;;;;;;;;
ReadControllers:
	JSR ReadController1
	JSR ReadController2
	RTS

ReadController1:
	LDA #$01
	STA $4016
	LDA #$00
	STA $4016
	LDX #$08
ReadController1Loop:
	LDA $4016
	LSR A            ; bit0 -> Carry
	ROL buttons1     ; bit0 <- Carry
	DEX
	BNE ReadController1Loop
	RTS

ReadController2:
	LDA #$01
	STA $4016
	LDA #$00
	STA $4016
	LDX #$08
ReadController2Loop:
	LDA $4017
	LSR A            ; bit0 -> Carry
	ROL buttons2     ; bit0 <- Carry
	DEX
	BNE ReadController2Loop
	RTS 
	
	
;;;;;;;;
; Math functions
;;;;;;;;
	
; General 8bit * 8bit = 8bit multiply
; by White Flame 20030207

; Multiplies "num1" by "num2" and returns result in .A
; Instead of using a bit counter, this routine early-exits when num2 reaches zero, thus saving iterations.


; Input variables:
;   num1 (multiplicand)
;   num2 (multiplier), should be small for speed
;   Signedness should not matter

; .X and .Y are preserved
; num1 and num2 get clobbered

Mult:
	LDA #$00
	BEQ MultEnterLoop

MultDoAdd:
	CLC
	ADC num1
MultLoop:
	ASL num1
MultEnterLoop: ;For an accumulating multiply (.A = .A + num1*num2), set up num1 and num2, then enter here
	LSR num2
	BCS MultDoAdd
	BNE MultLoop
MultEnd:
	RTS
	
;;;;;;;;
; Famitone import
;;;;;;;;
	.include "famitone2.asm"
	
;;;;;;;;;;;;;;
; PRG ROM Bank 2
;;;;;;;;;;;;;;
	.bank 1
	.org $A000
	.incbin "nesthing_ost.dmc"
	.include "nesthing_ost.asm"
  
palette:
  .db $0F,$17,$18,$29,$34,$38,$18,$2A,$38,$39,$3A,$3B,$3C,$3D,$3E,$0F ;nametable palettes
  .db $0F,$10,$37,$0F,$07,$10,$2D,$0F,$0F,$00,$0F,$36,$31,$02,$38,$3C ;sprite palettes
  
sprites:
     ;vert tile attr horiz
  .db $80, $00, $00, $80   ;sprite 0
  .db $80, $01, $00, $88   ;sprite 1
  .db $88, $02, $01, $80   ;sprite 2
  .db $88, $03, $01, $88   ;sprite 3

  ;Meta-tiles are arranged in column-first format,
  ;to make horizontal scrolling easier
  ;Each meta-tile is made up of 4 different tiles
  ;Maximum of 64 ($40; or $00-$39 numbered) meta-tiles
metatiles:
			;Tile positions:
			;top-left	bot-left	top-right	bot-right
	.db		$24,		$24,		$24,		$24 ; 		$00, should always be
													;		all background
	.db		$30,		$31,		$30,		$31 ; 		$01, basic top ground tile
	.db		$31,		$31,		$31,		$31 ;		$02, all ground tile
	.db		$24,		$32,		$24,		$24 ;		$03, floor flower tile
	.db		$24,		$24,		$24,		$33 ;		$04, floor grass tile
	.db		$24,		$36,		$24,		$37 ;		$05, bushes tile A
	.db		$24,		$30,		$24,		$24 ;		$06, single-tile platform


	
	;one column of map data to test out meta-tiles
	;note: all columns will have 3 wasted bytes
columnData:
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile

	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $03, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $04, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $06, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $05, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $01, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $00, $00, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $01, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $00, $00, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $01, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $00, $00, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $01, $00, $00, $00
	.db	$00, $00, $00, $00, $03, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $01, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile

	.db $00, $00, $00, $00, $01, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $01, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $01, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $01, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile

	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $03, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $04, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile

	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $01, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $01, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $01, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $01, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $00, $00, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $00, $00, $FF 		; 15 meta tiles to fill a column plus one filler tile

	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $01, $00, $00, $00, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $01, $00, $00, $00, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile

	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile

	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $03, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $04, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile

	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile
	
	.db $00, $00, $00, $00, $00, $00, $00, $00
	.db	$00, $00, $00, $00, $00, $01, $02, $FF 		; 15 meta tiles to fill a column plus one filler tile


	
attribData:
  .incbin "attrib.bin"


  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  

;;;;;;;;;;;;;; 
; CHR ROM
;;;;;;;;;;;;;; 
  .bank 2
  .org $0000
  .incbin "nesthing.chr"