	processor 6502
	include vcs.h
	include macro.h
	org $F000
	
;-----------RAM DATA-----------------
PlayerYPos = $80
PlayerVisibleLine = $81

EnemyYPos = $8A
EnemyVisibleLine = $8B
EnemyMovement = $8C
EnemyMovementTimer = $8D
EnemyFollowing = $8E

Score = $90
DigitOnes = $91
DigitTens = $93
ScoreGfx = $94
TenCounter = $95

MissileYPos = $9A
MissileVisibleLine = $9B
MissileTimer = $9C

Temp = $A0
PlayerBuffer = $A1
EnemyBuffer = $A2

Dead = $B0
BGColor = $B1
PlayerColor = $B2
EnemyColor = $B3

DeadSoundTimer = $C0

Start
	SEI	
	CLD  	
	LDX #$FF	
	TXS	
	LDA #0		
	
	
	
ClearMem 
	STA 0,X		
	DEX		
	BNE ClearMem	
	
PlayfieldSettings
	
;STORE COLORS
	LDA #$C4
	STA PlayerColor
	LDA #$18
	STA EnemyColor
	LDA #$DC
	STA BGColor
	
	LDA #80
	STA PlayerYPos
	
	LDA #30
	STA EnemyYPos
	
	LDA #30
	STA MissileYPos
	
	LDA #0
	STA Dead
	STA DeadSoundTimer
	STA EnemyMovementTimer
	STA EnemyMovement
	STA EnemyFollowing
	STA PlayerBuffer
	STA EnemyBuffer
	STA MissileTimer
	STA TenCounter
	STA Score
	
;Missile Settings
	LDA #$20
	STA NUSIZ1
	
	STA WSYNC
	SLEEP #52
	STA RESM1
	
MainLoop
	LDA  #2
	STA  VSYNC
	STA  WSYNC
	STA  WSYNC
	STA  WSYNC
	
	;Set Timer
	LDA  #43	
	STA  TIM64T	
	LDA #0		
	STA  VSYNC 	

;-------------------GAME LOGIC-------------------
	
	LDA Dead
	CMP #1
	BEQ SkipPlayerMovement
	
;Player Movement	
	LDA #%00100000 ;Down on Joystick
	BIT SWCHA
	BNE SkipDown
	DEC PlayerYPos
SkipDown

	LDA #%00010000 ;Up on Joystick
	BIT SWCHA
	BNE SkipUp
	INC PlayerYPos
SkipUp
	
;Top Border
	LDA PlayerYPos
	CMP #90
	BCC InRange0	; <=
	LDA #90
	STA PlayerYPos
InRange0

;Bottom Border
	LDA PlayerYPos
	CMP #10
	BCS InRange1	; >=
	LDA #10
	STA PlayerYPos
InRange1
SkipPlayerMovement
	
;---------------------------------------------

	LDA Dead
	CMP #1
	BEQ DeadLogic
	
;Enemy Movement
	INC EnemyMovementTimer
	LDA EnemyMovementTimer
	CMP #120
	BNE SkipChangeMovement
	
ChangeMovement
	LDA #0
	STA EnemyMovementTimer
	LDA EnemyFollowing
	CMP #0
	BEQ MakeFollowing
	
	LDA #0
	STA EnemyFollowing
	JMP SkipChangeMovement
MakeFollowing
	LDA #1
	STA EnemyFollowing
	
SkipChangeMovement
	
	LDA EnemyFollowing
	CMP #1
	BNE SkipEnemyFollow
	
;Enemy follows Player
	LDA EnemyYPos
	CMP PlayerYPos
	BCS GoDown
	INC EnemyYPos
	LDA #0
	STA EnemyMovement
	JMP SkipGoDown
GoDown
	DEC EnemyYPos
	LDA #1
	STA EnemyMovement
SkipGoDown
	JMP FinishEnemyMovement

SkipEnemyFollow
	LDA EnemyMovement
	CMP #1
	BEQ GoDown2
	INC EnemyYPos
	JMP SkipGoDown2
GoDown2
	DEC EnemyYPos
SkipGoDown2

FinishEnemyMovement

;Keep enemy in range

;Top Border
	LDA EnemyYPos
	CMP #90
	BCC InRange2	; <=
	LDA #90
	STA EnemyYPos
InRange2

;Bottom Border
	LDA EnemyYPos
	CMP #10
	BCS InRange3	; >=
	LDA #10
	STA EnemyYPos
InRange3
	
	

;---------------------------------------------

;Missile Movement

	LDA #%00010000	;Move Left
	STA HMM1		;Missile Move!
;Missile Timer
	INC MissileTimer

;---------------------------------------------
	JMP NotDead

DeadLogic
	;Stop Missile
	LDA #%00000000
	STA HMM1
	
	;Set dead COLORS
	LDA #$34
	STA BGColor
	
	LDA #$3A
	STA PlayerColor
	STA EnemyColor
	
	LDA DeadSoundTimer
	CMP #60
	BEQ SkipSound
	INC DeadSoundTimer
	LDA #15
	STA AUDV0
	STA AUDV1
	LDA #1
	STA AUDC0
	STA AUDC1
	LDA #17
	STA AUDF0
	LDA #7
	STA AUDF1
	JMP NotDead
SkipSound
	LDA #0
	STA AUDV0
	STA AUDV1
NotDead


;PLAYER AND MISSILE COLLIDE
	LDA #%10000000
	BIT CXM1P
	BEQ NoCollision
	LDA #1
	STA Dead
	JMP CollisionEnd
NoCollision
CollisionEnd
	STA CXCLR

;When Missile Timer hits 60, add score and reset missile!
	LDA MissileTimer
	CMP #$68
	BNE SkipPoint
	INC Score
	STA WSYNC
	SLEEP #52
	STA RESM1
	LDA EnemyYPos
	STA MissileYPos
	LDA #0
	STA MissileTimer
	
	INC TenCounter
	LDA TenCounter
	CMP #$A
	BNE SkipPoint
	LDA Score
	ADC #$5
	STA Score
	LDA #0
	STA TenCounter
SkipPoint

;Set vague Player and Enemy X Position by sleeping the processor
	STA WSYNC
	SLEEP #26
	STA RESP0
	SLEEP #26
	STA RESP1
	
	
;Set up Score Display
	LDA Score
	AND #$0F
	STA Temp
	ASL
	ASL
	ADC Temp
	STA DigitOnes
	LDA Score
	AND #$F0
	LSR
	LSR
	STA Temp
	LSR
	LSR
	ADC Temp
	STA DigitTens

;-------------------END LOGIC--------------------
;Load timer and wait
WaitForVblankEnd
	LDA INTIM	
	BNE WaitForVblankEnd

;-------------------DRAWING----------------------
	
;Process Horizontal Movements
	STA HMOVE
	
;Start drawing!
	STA WSYNC	
	STA VBLANK  	
	

;----------------DRAWING SCORE--------------------
	
;SCORE COLOR
	LDA #$0E
	STA COLUBK
	LDA #$00
	STA COLUPF
	
;Loop counter - 5
	LDX #5
	STA WSYNC
	STA WSYNC
ScoreDrawLoop
	LDA #0
	STA PF1
	LDY DigitTens
	LDA Numbers,Y
	AND #$F0
	STA ScoreGfx
	LDY DigitOnes
	LDA Numbers,Y
	AND #$0F
	ORA ScoreGfx
	STA ScoreGfx
	SLEEP #2
	STA PF1
	STA WSYNC
	
;Once again, yes the code is copied, thx assembly really cool...
	LDA #0
	STA PF1
	LDY DigitTens
	LDA Numbers,Y
	AND #$F0
	STA ScoreGfx
	LDY DigitOnes
	LDA Numbers,Y
	AND #$0F
	ORA ScoreGfx
	STA ScoreGfx
	SLEEP #2
	STA PF1
	STA WSYNC
	
	INC DigitOnes
	INC DigitTens
	DEX
	BNE ScoreDrawLoop
	STX PF1
	STA WSYNC
	STA WSYNC
	
;---------------END DRAWING SCORE-----------------

;PLAYER COLOR
	LDA PlayerColor
	STA COLUP0
	
;ENEMY COLOR
	LDA EnemyColor
	STA COLUP1
	
	LDA BGColor
	STA COLUBK ; set the background color
	
;--------------------------------------------------

;How many lines in playfield
	LDY #91
	
	STA WSYNC
	STA HMOVE 	
GameDrawLoop 
	STA WSYNC	
	
	;Draw Graphics from Buffer
	LDA PlayerBuffer
	STA GRP0

;-----------------DRAWING PLAYER-------------------
DrawPlayer
	CPY PlayerYPos
	BNE SkipDrawPlayer
	LDA #8
	STA PlayerVisibleLine
SkipDrawPlayer

	LDA #0
	STA PlayerBuffer
	
	LDX PlayerVisibleLine
	BEQ FinishPlayer
	
	LDA TurtleGraphic-1,X		
	STA PlayerBuffer	
	DEC PlayerVisibleLine
FinishPlayer
;-----------------END PLAYER-----------------------

	STA WSYNC
	
	LDA EnemyBuffer
	STA GRP1
;-----------------DRAWING MISSILE------------------
DrawMissile
	CPY MissileYPos
	BNE SkipDrawMissile
	LDA #4
	STA MissileVisibleLine
SkipDrawMissile

	LDA #0
	STA ENAM1
	
	LDX MissileVisibleLine
	BEQ FinishMissile
	
	LDA #2
	STA ENAM1
	DEC MissileVisibleLine
FinishMissile
;-----------------END MISSILE----------------------

;-----------------DRAWING ENEMY------------------
DrawEnemy
	CPY EnemyYPos
	BNE SkipDrawEnemy
	LDA #8
	STA EnemyVisibleLine
SkipDrawEnemy

	LDA #0
	STA EnemyBuffer
	
	LDX EnemyVisibleLine
	BEQ FinishEnemy
	
	LDA EnemyGraphic-1,X		
	STA EnemyBuffer
	DEC EnemyVisibleLine
FinishEnemy
;-----------------END ENEMY----------------------

	DEY		
	BNE GameDrawLoop	
;---------END OF DRAWING----------

	LDA #2		
	STA WSYNC  	
	STA VBLANK 	
	LDX #30		
OverScanWait
	STA WSYNC
	DEX
	BNE OverScanWait
	JMP  MainLoop      
 
;---------GRAPHIC DATA HERE----------
TurtleGraphic
	.byte #%11001100
	.byte #%10001000
	.byte #%11111110
	.byte #%11111011
	.byte #%11111011
	.byte #%01110000
	.byte #%00000000
	.byte #%00000000

EnemyGraphic
	.byte #%01101100
	.byte #%00111111
	.byte #%01111101
	.byte #%00111100
	.byte #%01111110
	.byte #%11111100
	.byte #%11011110
	.byte #%01111100

;---------NUMBER DATA HERE----------
Numbers
	.byte #%11110111
	.byte #%10010101
	.byte #%10010101
	.byte #%10010101
	.byte #%11110111

	.byte #%00010001
	.byte #%00010001
	.byte #%00010001
	.byte #%00010001
	.byte #%00010001
	
	.byte #%11110111
	.byte #%00010001
	.byte #%11110111
	.byte #%10000100
	.byte #%11110111
	
	.byte #%11110111
	.byte #%00010001
	.byte #%11110011
	.byte #%00010001
	.byte #%11110111
	
	.byte #%10010101
	.byte #%10010101
	.byte #%11110111
	.byte #%00010001
	.byte #%00010001
	
	.byte #%11110111
	.byte #%10000100
	.byte #%11110111
	.byte #%00010001
	.byte #%11110111

	.byte #%01110011
	.byte #%10000100
	.byte #%11110111
	.byte #%10010101
	.byte #%11110111

	.byte #%11110111
	.byte #%00010001
	.byte #%00010001
	.byte #%00010001
	.byte #%00010001

	.byte #%11110111
	.byte #%10010101
	.byte #%11110111
	.byte #%10010101
	.byte #%11110111

	.byte #%11110111
	.byte #%10010101
	.byte #%11110111
	.byte #%00010001
	.byte #%11110111

	.byte #%01100010
	.byte #%10010101
	.byte #%11110111
	.byte #%10010101
	.byte #%10010101

	.byte #%11100110
	.byte #%10010101
	.byte #%11100110
	.byte #%10010101
	.byte #%11100110
	
	.byte #%11110111
	.byte #%10000100
	.byte #%10000100
	.byte #%10000100
	.byte #%11110111
	
	.byte #%11100110
	.byte #%10010101
	.byte #%10010101
	.byte #%10010101
	.byte #%11100110
	
	.byte #%11110111
	.byte #%10000100
	.byte #%11110111
	.byte #%10000100
	.byte #%11110111

	.byte #%11110111
	.byte #%10000100
	.byte #%11110111
	.byte #%10000100
	.byte #%10000100
	
	org $FFFC
	.word Start
	.word Start