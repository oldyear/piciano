; microsound.asm
	LIST	p=16F628a	;tell assembler what chip we are using
	include "P16F628a.inc"	;include the defaults for the chip
	__config 0x3D18			;sets the configuration settings (oscillator type etc.)

;/***************************************************
; Jump to Main Loop
;
;***************************************************/
    org 0x0
    goto   startupcode       ; plenty of room; won't run into our ISR code.
;/***************************************************	
;   ContextSave:
;   Saves important registers before running our interrupt.
;  (cool code taken from http://www.piclist.com/techref/microchip/ints16Fintro-wcw.htm)
;
;***************************************************/
    org 0x4
ContextSave
    movwf          temp_w_isr      ;save w in temp.  movwf does not change STATUS
                               ;; NOTE However, that temp_w must exist in all RAM banks.
    swapf          STATUS,w    ;put unchanged STATUS in w with nibbles reversed.
    clrf           STATUS      ;now use RAM page 0
    movwf          temp_s       ;save STATUS in temp_s
    movf           PCLATH,W     ; save PCLATH as well
    movwf          temp_pclath
    clrf           PCLATH       ; now on code page 0 for sure too.
    movf		   FSR
	movwf		   temp_fsr	   ;

; 200 Cycles total in interrupt.
dispathc_int
	bcf PIR1, TMR2IF  ;Gotta clear this interrrupt flag!
	goto int_sound
	;goto intexit  ;Other interrupt  possible? Maybe should just go straight to intsound?

	cblock 0x20

	;Sound Struct
	Key1Th ;Ticks whole part high.
	;Key1Tl ;Ticks decimal part.
	Key1Curr; How many updates have we have until our next key set.
	Key1State;Is our square wave High (32) or Low (0)
	
	Key2Th
	;Key2Tl
	Key2Curr
	Key2State

	Key3Th ;Ticks whole part high.
	;Key3Tl ;Ticks decimal part.
	Key3Curr
	Key3State
	
	Key4Th ;Ticks whole part high.
	;Key4Tl ;Ticks decimal part.
	Key4Curr
	Key4State

	Key5Th ;Ticks whole part high.
	;Key5Tl ;Ticks decimal part.
	Key5Curr
	Key5State
	
	Key6Th ;Ticks whole part high.
	;Key6Tl ;Ticks decimal part.
	Key6Curr
	Key6State
	
	Key7Th ;Ticks whole part high.
	;Key7Tl ;Ticks decimal part.
	Key7Curr
	Key7State
	
	Key8Th ;Ticks whole part high.
	;Key8Tl ;Ticks decimal part.
	Key8Curr
	Key8State

	outChannel ;This is a sum of all Keys states then pushed to the PWM register
	
	;Main Loop Stuff
	currsixth
	keyInSixth

	;State Save Stuff
	ActiveKeys    ;Which Structs are active?
	PORTA_temp
	temp_w
	temp_w_isr
	temp_s
	temp_pclath
	fired
	temp_fsr
	endc

	org 0x42
;**************************************************************
;;Sound generation! called via the ISR when Timer1 int is up
;
;One Key Length:
; 4 minimum if no key set
; 5 on average (should just have to decf and skip til next note)
; 9 if key goes low
; 11 if key goes high
;
;**************************************************************
int_sound
Key1				
	movf Key1Th, w			
	btfsc STATUS, Z			;If the ticksHigh register for this key isn't set then we skip this key.
	goto Key1Done;int_exit			;Maybe be able to go all the way to the end of the update depending on 
	;First key check		;how keypresses get to this heezy.
	decfsz Key1Curr, f			
	goto Key1Done			
Key1Set				 		;If 0 we need to toggle the state of the key.
	movf Key1Th, w			
	movwf Key1Curr			
	btfsc Key1State, 6			
	goto Key1Low			
Key1High				
	movlw .32 			
	movwf Key1State			
	addwf outChannel, f			
	goto Key1Done			
Key1Low
	movlw 0
	movwf Key1State
Key1Done
				
Key2				
	movf Key2Th, w			
	btfsc STATUS, Z			
	goto Key2Done;int_sound_out			
				
	decfsz Key2Curr, f			
	goto Key2Done			
Key2Set				
	movf Key2Th, w 			
	movwf Key2Curr			
	btfsc Key2State, 6			
	goto Key2Low			
Key2High				
	movlw .32			
	movwf Key2State			
	addwf outChannel, f			
	goto Key2Done			
Key2Low				
	movlw 0 			
	movwf Key2State			
Key2Done				
				
Key3				
	movf Key3Th, w			
	btfsc STATUS, Z			
	goto Key3Done;int_sound_out
				
	decfsz Key3Curr, f			
	goto Key3Done			
Key3Set				
	movf Key3Th, w  			
	movwf Key3Curr			
	btfsc Key3State, 6			
	goto Key3Low			
Key3High				
	movlw .32         			
	movwf Key3State			
	addwf outChannel, f			
	goto Key3Done			
Key3Low				
	movlw 0           		
	movwf Key3State			
Key3Done				
				
Key4				
	movf Key4Th, w			
	btfsc STATUS, Z			
	goto Key4Done;int_sound_out
			
	decfsz Key4Curr, f			
	goto Key4Done			
Key4Set				
	movf Key4Th, w 		
	movwf Key4Curr			
	btfsc Key4State, 6		
	goto Key4Low			
Key4High				
	movlw .32         			
	movwf Key4State			
	addwf outChannel, f			
	goto Key4Done			
Key4Low				
	movlw 0           		
	movwf Key4State			
Key4Done				

Key5				
	movf Key5Th, w			
	btfsc STATUS, Z			
	goto Key5Done;int_sound_out
				
	decfsz Key5Curr, f			
	goto Key5Done			
Key5Set				
	movf Key5Th, w 		
	movwf Key5Curr			
	btfsc Key5State, 6		
	goto Key5Low			
Key5High				
	movlw .32         			
	movwf Key5State			
	addwf outChannel, f			
	goto Key5Done			
Key5Low				
	movlw 0           		
	movwf Key5State			
Key5Done

Key6				
	movf Key6Th, w			
	btfsc STATUS, Z	
	goto Key6Done;int_sound_out
	
	decfsz Key6Curr, f			
	goto Key6Done			
Key6Set				
	movf Key6Th, w 		
	movwf Key6Curr			
	btfsc Key6State, 6		
	goto Key6Low			
Key6High				
	movlw .32         			
	movwf Key6State			
	addwf outChannel, f			
	goto Key6Done			
Key6Low				
	movlw 0           		
	movwf Key6State			
Key6Done

Key7				
	movf Key7Th, w			
	btfsc STATUS, Z			
	goto Key7Done;int_sound_out
				
	decfsz Key7Curr, f			
	goto Key7Done			
Key7Set				
	movf Key7Th, w 		
	movwf Key7Curr			
	btfsc Key7State, 6		
	goto Key7Low			
Key7High				
	movlw .32         			
	movwf Key7State			
	addwf outChannel, f			
	goto Key7Done			
Key7Low				
	movlw 0           		
	movwf Key7State			
Key7Done

Key8				
	movf Key8Th, w			
	btfsc STATUS, Z			
	goto Key8Done;int_sound_out
			
	decfsz Key8Curr, f			
	goto Key8Done			
Key8Set				
	movf Key8Th, w 		
	movwf Key8Curr			
	btfsc Key8State, 6		
	goto Key8Low			
Key8High				
	movlw .32         			
	movwf Key8State			
	addwf outChannel, f			
	goto Key8Done			
Key8Low				
	movlw 0           		
	movwf Key8State			
Key8Done

;*****************************************************************************
;
;Let's throw our generated amplitude into the PWM register!
;CCPR1L is the low 8 bits of PWM 256 values for us sounds good!
;
;*****************************************************************************
int_sound_out
	movf outChannel, w
	movwf CCPR1L
	clrf outChannel			;Out channel needs to be reset. Gets filled up every call.
	;goto intexit

intexit
	movf temp_pclath, W
	movwf PCLATH
	swapf temp_s, W
	movwf STATUS
	swapf temp_fsr, W
	movwf FSR
	swapf temp_w_isr, F
	swapf temp_w_isr, W
	 retfie

	org 0xFC
;/****************************************
;  Set up code for the pic pre main loop.
;
;
;
;****************************************/
startupcode
	;Gotta get a clean slate for the ports. Oddly this effects PORTA's ability to be used as input.
	;This is how the code works in the reference manual.
	clrf PORTA
	clrf PORTB
	clrf outChannel
	clrf temp_w

	movlw 0x07
	movwf CMCON	;turn off comparators

	bcf STATUS, RP1 ;Exit Bank Zero
	bsf STATUS, RP0	;Bank one sets

	movlw 0xFF
	movwf TRISA ;All of PORTA will be inputs
	movlw 0X00
	movwf TRISB ;PORTB will be all outputs
	bsf PIE1, TMR2IE  ;Turn timer 2 interrupt enable on.
	movlw .200        ;Decimal 200 at 20khz we have 200 cycles before we have to update.
	movwf PR2
	bcf STATUS, RP0;Exit bank one

	bsf T2CON, TMR2ON ; Turn on timer 2
	bsf INTCON, GIE	;Turn on all unmasked interupts
	bsf INTCON, PEIE;Turn on peripheral interrupts

	movlw b'00001100'
		;bits 10	low bits of CPR1
	movwf CCP1CON; turn PWM on Bits 4:5 are CPR1H (the low bits of the PWM duty cycle)
	movlw b'00000000'
	movwf CCPR1L ; The MS 8 bits of PWM. Start at 0

;========================================================
;
;    Start of our main loop.
;    All we are doing is checking for keys pressed.
;
;=======================================================

testkeyboard
	clrf ActiveKeys;  The current number of channels occupied
	clrf keyInSixth;How far into this sixth is this key. values 0 - 5 for 6
					;keys in a 6th
	clrf temp_w
	;movlw 0xFF
	;movwf PORTA_temp;Set up our temp PORTA for anding ??

	movlw b'00000001'
	movwf PORTB 	;Test First Sixlet
	movf PORTA, W   ;Lets check if we have any keypresses on the first Sixth	
	btfss STATUS, Z
	call testnote	;if STATUS is not zero we have a note!
	btfsc temp_w, RA7       ;Did we get a bad return code? Aka notes full?
	goto testkeyboard

	movlw .8		;Second sixth starts with index 8 into noteValue
	movwf keyInSixth

	movlw b'00000010'
	movwf PORTB		;Test Second Sixlet
	movf PORTA, W   ;Lets check if we have any keypresses on the first Sixth	
	btfss STATUS, Z
	call testnote	;if STATUS is not zero we have a note!
	btfsc temp_w, RA7       ;Did we get a bad return code? Aka notes full?
	goto testkeyboard

	movlw .16
	movwf keyInSixth

	movlw b'00000100'
	movwf PORTB		;Test Third Sixlet
	movf PORTA, W   ;Lets check if we have any keypresses on the first Sixth	
	btfss STATUS, Z
	call testnote	;if STATUS is not zero we have a note!
	btfsc temp_w, RA7       ;Did we get a bad return code? Aka notes full?
	goto testkeyboard

	movlw .24
	movwf keyInSixth

	movlw b'00010000'
	movwf PORTB		;Test Fourth Sixlet
	movf PORTA, W   ;Lets check if we have any keypresses on the first Sixth	
	btfss STATUS, Z
	call testnote	;if STATUS is not zero we have a note!
	btfsc temp_w, RA7       ;Did we get a bad return code? Aka notes full?
	goto testkeyboard

	movlw .32
	movwf keyInSixth

	movlw b'00100000'
	movwf PORTB		;Test Fifth Sixlet
	movf PORTA, W   ;Lets check if we have any keypresses on the first Sixth	
	btfss STATUS, Z
	call testnote	;if STATUS is not zero we have a note!
	btfsc temp_w, RA7       ;Did we get a bad return code? Aka notes full?
	goto testkeyboard

	movlw .40
	movwf keyInSixth

	movlw b'01000000'
	movwf PORTB		;Test Sixth Sixlet
	movf PORTA, W   ;Lets check if we have any keypresses on the first Sixth	
	btfss STATUS, Z
	call testnote	;if STATUS is not zero we have a note!
	btfsc temp_w, RA7       ;Did we get a bad return code? Aka notes full?
	goto testkeyboard

	;goto clearNotes
;****************************************************************************
;  ClearNotes.
;
;
;
;  Needs to check every note to see if we are keeping that note alive or not
;  has issue if a key is held, it will erase it's Curr record.
;
;  Check it's active status
;
;***************************************************************************
clearNotes   
	btfss ActiveKeys, RB0
	clrf Key1Th
	btfss ActiveKeys, RB1
	clrf Key2Th
	btfss ActiveKeys, RB2
	clrf Key3Th
	btfss ActiveKeys, RB3
	clrf Key4Th
	btfss ActiveKeys, RB4
	clrf Key5Th
	btfss ActiveKeys, RB5
	clrf Key6Th
	btfss ActiveKeys, RB6
	clrf Key7Th
	btfss ActiveKeys, RB7
	clrf Key8Th

DoneClear
	goto testkeyboard

	cblock
	tempNoteValue
	endc

;******************************************************************
;
; Test which note is active! And if we find which note is active
; call newnote to activate it!
;
;*******************************************************************
testnote
	btfss PORTA, RA0;
	goto testnote2
	call  newnote
	movwf temp_w
	btfsc temp_w, RA7       ;Did we get a bad return code? Aka notes full or already pressed.
	return

testnote2
	incf  keyInSixth, f

	btfss PORTA, RA1;
	goto testnote3
	call  newnote
	movwf temp_w
	btfsc temp_w, RA7
	return

testnote3
	incf  keyInSixth, f

	btfss PORTA, RA2;
	goto testnote4
	call  newnote
	movwf temp_w
	btfsc temp_w, RA7
	return

testnote4
	incf  keyInSixth, f

	btfss PORTA, RA3;
	goto testnote5
	call  newnote
	movwf temp_w
	btfsc temp_w, RA7
	return

testnote5
	incf  keyInSixth, f

	btfss PORTA, RA4;
	goto testnote6
	call  newnote
	movwf temp_w
	btfsc temp_w, RA7
	return

testnote6
	incf  keyInSixth, f

	btfss PORTA, RA5;
	goto testnote7
	call  newnote
	movwf temp_w
	btfsc temp_w, RA7
	return

testnote7
	incf  keyInSixth, f

	btfss PORTA, RA6;
	goto testnote8
	call  newnote
	movwf temp_w
	btfsc temp_w, RA7
	return

testnote8
	incf  keyInSixth, f

	btfss PORTA, RA7;
	goto testnoteend
	call  newnote
	movwf temp_w
testnoteend
	return

;*********************************************************************************
;This is funky.  Filling up the sound structs is tricky I think.
;I can probably kludge this by just checking if the struct is filled every time.
;The only funky thing about this is if I add a note that's already added
;in a later struct it's going to increase volume. Hmmm.... I guess this is okay
;given how short a time the volume increase is likely to be and, because
;there is no possibility of wraparound on the CCP1L register. (maxxed at 255)
;by nature of the ISR.
;Gotta test right here whether or not the note exists already.
;The only way to do this better would be to run the sample rate slower (or increase clock rate)
;or maybe disable interrupts?
;we are probably going to get glitchyness in keypress registration (albeit not much) unless
;we do that. The longest a glitch should last is probably 1 or 2 updates.
;about 0.00005% error at worst per cycle.
;*********************************************************************************
newnote  
	movf ActiveKeys, W
	sublw 0xFF           ;If we ActiveKeys is full we can't add any more.
	btfsc STATUS, Z
	retlw .128         ;Error code, full.

	movf keyInSixth, W
	call noteValue
	movwf tempNoteValue
	call checkNote
	movwf temp_w
	btfsc temp_w, RA6	 ;Check if note already exists (RA6)
	return               ;
	;Note doesn't exist, let's add it to the struct!
	call findFree
	movwf FSR
	movf tempNoteValue, W
	movwf INDF;Key
	incf  FSR
	clrf  INDF;KeyCurr
	incf  INDF;KeyCurr, F
	incf  FSR
	clrf  INDF;KeyState

	retlw .0

;findFree finds a freeSpace
findFree
	btfsc ActiveKeys, RB0
	goto findFree2
	bsf ActiveKeys, RB0
	retlw Key1Th
findFree2
	btfsc ActiveKeys, RB1
	goto findFree3
	bsf ActiveKeys, RB1
	retlw Key2Th
findFree3
	btfsc ActiveKeys, RB2
	goto findFree4
	bsf ActiveKeys, RB2
	retlw Key3Th
findFree4
	btfsc ActiveKeys, RB3
	goto findFree5
	bsf ActiveKeys, RB3
	retlw Key4Th
findFree5
	btfsc ActiveKeys, RB4
	goto findFree6
	bsf ActiveKeys, RB4
	retlw Key5Th
findFree6
	btfsc ActiveKeys, RB5
	goto findFree7
	bsf ActiveKeys, RB5
	retlw Key6Th
findFree7
	btfsc ActiveKeys, RB6
	goto findFree8
	bsf ActiveKeys, RB6
	retlw Key7Th
findFree8
	bsf ActiveKeys, RB7
	retlw Key8Th

;Checks to see if the note is already being played.
checkNote

CheckNoteOne
	movf tempNoteValue, W
	subwf Key1Th, W
	btfsc STATUS, Z
	goto CheckNoteOneReactivate
	goto CheckNoteTwo
CheckNoteOneReactivate
	bsf ActiveKeys, RB0
	retlw .64

CheckNoteTwo
	movf tempNoteValue, W
	subwf Key2Th, W
	btfsc STATUS, Z
	goto CheckNoteTwoReactivate
	goto CheckNoteThree
CheckNoteTwoReactivate
	bsf ActiveKeys, RB1
	retlw .64

CheckNoteThree
	movf tempNoteValue, W
	subwf Key3Th, W
	btfsc STATUS, Z
	goto CheckNoteThreeReactivate
	goto CheckNoteFour
CheckNoteThreeReactivate
	bsf ActiveKeys, RB2
	retlw .64

CheckNoteFour
	movf tempNoteValue, W
	subwf Key4Th, W
	btfsc STATUS, Z
	goto CheckNoteFourReactivate
	goto CheckNoteFive
CheckNoteFourReactivate
	bsf ActiveKeys, RB3
	retlw .64

CheckNoteFive
	movf tempNoteValue, W
	subwf Key5Th, W
	btfsc STATUS, Z
	goto CheckNoteFiveReactivate
	goto CheckNoteSix
CheckNoteFiveReactivate
	bsf ActiveKeys, RB4
	retlw .64

CheckNoteSix
	movf tempNoteValue, W
	subwf Key6Th, W
	btfsc STATUS, Z
	goto CheckNoteSixReactivate
	goto CheckNoteSeven
CheckNoteSixReactivate
	bsf ActiveKeys, RB5
	retlw .64

CheckNoteSeven
	movf tempNoteValue, W
	subwf Key7Th, W
	btfsc STATUS, Z
	goto CheckNoteSevenReactivate
	goto CheckNoteEight
CheckNoteSevenReactivate
	bsf ActiveKeys, RB6
	retlw .64

CheckNoteEight
	movf tempNoteValue, W
	subwf Key8Th, W
	btfsc STATUS, Z
	goto CheckNoteEightReactivate
	goto CheckNoteEnd
CheckNoteEightReactivate
	bsf ActiveKeys, RB7
	retlw .64

CheckNoteEnd	
	retlw .0     ;Note doesn't exist!!
	

	org 0x200
; Formula= (SAMPLERATE / FREQUENCY) / 2
; Some rounding and displacement to make things more fluid.
; Number of samples the note is up.
; Organized by sixth. 8 values per sixth
noteValue;20khz sample rate.
	movwf temp_w
	movlw HIGH Notes
	movwf PCLATH
	movf  keyInSixth, W
	addwf   PCL, f      ;   Move the PC this many forward
Notes
	retlw	.255	;	38.8909		D??/E??			D?1/E?1
	retlw	.182	;	55		A?			A1
	retlw	.129	;	77.7817		D?/E?			D?2/E?2
	retlw	.91	;	110		A			A2
	retlw	.64	;	155.563		d?/e?			D?3/E?3
	retlw	.45	;	220		a			A3
	retlw	.32	;	311.127		d?'/e?'			D?4/E?4
	retlw	.23	;	440		a'			A4 A440
									
	retlw	.243	;	41.2034		E?			E1
	retlw	.172	;	58.2705		A??/B??			A?1/B?1
	retlw	.121	;	82.4069		E			E2
	retlw	.86	;	116.541		A?/B?			A?2/B?2
	retlw	.61	;	164.814		e			E3
	retlw	.43	;	233.082		a?/b?			A?3/B?3
	retlw	.30	;	329.628		e'			E4
	retlw	.22	;	466.164		a?'/b?'			A?4/B?4
									
	retlw	.229	;	43.6535		F?			F1
	retlw	.162	;	61.7354		B?			B1
	retlw	.115	;	87.3071		F			F2
	retlw	.81	;	123.471		B			B2
	retlw	.57	;	174.614		f			F3
	retlw	.40	;	246.942		b			B3
	retlw	.29	;	349.228		f'			F4
	retlw	.21	;	493.883		b'			B4
									
	retlw	.216	;	46.2493		F??/G??			F?1/G?1
	retlw	.153	;	65.4064		C great octave			C2 Deep C
	retlw	.108	;	92.4986		F?/G?			F?2/G?2
	retlw	.76	;	130.813		c small octave			C3 Low C
	retlw	.54	;	184.997		f?/g?			F?3/G?3
	retlw	.38	;	261.626		c' 1-line octave			C4 Middle C
	retlw	.27	;	369.994		f?'/g?'			F?4/G?4
	retlw	.20	;	523.251		c'' 2-line octave			C5 Tenor C
									
	retlw	.204	;	48.9994		G?			G1
	retlw	.144	;	69.2957		C?/D?			C?2/D?2
	retlw	.102	;	97.9989		G			G2
	retlw	.72	;	138.591		c?/d?			C?3/D?3
	retlw	.51	;	195.998		g			G3
	retlw	.36	;	277.183		c?'/d?'			C?4/D?4
	retlw	.26	;	391.995		g'			G4
	retlw	.19	;	554.365		c?''/d?''			C?5/D?5
									
	retlw	.193	;	51.9131		G??/A??			G?1/A?1
	retlw	.136	;	73.4162		D			D2
	retlw	.96	;	103.826		G?/A?			G?2/A?2
	retlw	.68	;	146.832		d			D3
	retlw	.48	;	207.652		g?/a?			G?3/A?3
	retlw	.34	;	293.665		d'			D4
	retlw	.24	;	415.305		g?'/a?'			G?4/A?4
	retlw	.18	;	587.33		d''			D5
	retlw   .18 ;

	end
	