LIST P=18F4620
    
#include <P18F4620.INC>

config OSC = HSPLL      ; Oscillator Selection bits (HS oscillator, PLL enabled (Clock Frequency = 4 x FOSC1))
config FCMEN = OFF      ; Fail-Safe Clock Monitor Enable bit (Fail-Safe Clock Monitor disabled)
config IESO = OFF       ; Internal/External Oscillator Switchover bit (Oscillator Switchover mode disabled)

; CONFIG2L
config PWRT = ON        ; Power-up Timer Enable bit (PWRT enabled)
config BOREN = OFF      ; Brown-out Reset Enable bits (Brown-out Reset disabled in hardware and software)
config BORV = 3         ; Brown Out Reset Voltage bits (Minimum setting)

; CONFIG2H
config WDT = OFF        ; Watchdog Timer Enable bit (WDT disabled (control is placed on the SWDTEN bit))
config WDTPS = 32768    ; Watchdog Timer Postscale Select bits (1:32768)

; CONFIG3H
config CCP2MX = PORTC   ; CCP2 MUX bit (CCP2 input/output is multiplexed with RC1)
config PBADEN = OFF     ; PORTB A/D Enable bit (PORTB<4:0> pins are configured as digital I/O on Reset)
config LPT1OSC = OFF    ; Low-Power Timer1 Oscillator Enable bit (Timer1 configured for higher power operation)
config MCLRE = ON       ; MCLR Pin Enable bit (MCLR pin enabled; RE3 input pin disabled)

; CONFIG4L
config STVREN = OFF     ; Stack Full/Underflow Reset Enable bit (Stack full/underflow will not cause Reset)
config LVP = OFF        ; Single-Supply ICSP Enable bit (Single-Supply ICSP disabled)
config XINST = OFF      ; Extended Instruction Set Enable bit (Instruction set extension and Indexed Addressing mode disabled (Legacy mode))

; CONFIG5L
config CP0 = OFF        ; Code Protection bit (Block 0 (000800-003FFFh) not code-protected)
config CP1 = OFF        ; Code Protection bit (Block 1 (004000-007FFFh) not code-protected)
config CP2 = OFF        ; Code Protection bit (Block 2 (008000-00BFFFh) not code-protected)
config CP3 = OFF        ; Code Protection bit (Block 3 (00C000-00FFFFh) not code-protected)

; CONFIG5H
config CPB = OFF        ; Boot Block Code Protection bit (Boot block (000000-0007FFh) not code-protected)
config CPD = OFF        ; Data EEPROM Code Protection bit (Data EEPROM not code-protected)

; CONFIG6L
config WRT0 = OFF       ; Write Protection bit (Block 0 (000800-003FFFh) not write-protected)
config WRT1 = OFF       ; Write Protection bit (Block 1 (004000-007FFFh) not write-protected)
config WRT2 = OFF       ; Write Protection bit (Block 2 (008000-00BFFFh) not write-protected)
config WRT3 = OFF       ; Write Protection bit (Block 3 (00C000-00FFFFh) not write-protected)

; CONFIG6H
config WRTC = OFF       ; Configuration Register Write Protection bit (Configuration registers (300000-3000FFh) not write-protected)
config WRTB = OFF       ; Boot Block Write Protection bit (Boot Block (000000-0007FFh) not write-protected)
config WRTD = OFF       ; Data EEPROM Write Protection bit (Data EEPROM not write-protected)

; CONFIG7L
config EBTR0 = OFF      ; Table Read Protection bit (Block 0 (000800-003FFFh) not protected from table reads executed in other blocks)
config EBTR1 = OFF      ; Table Read Protection bit (Block 1 (004000-007FFFh) not protected from table reads executed in other blocks)
config EBTR2 = OFF      ; Table Read Protection bit (Block 2 (008000-00BFFFh) not protected from table reads executed in other blocks)
config EBTR3 = OFF      ; Table Read Protection bit (Block 3 (00C000-00FFFFh) not protected from table reads executed in other blocks)

; CONFIG7H
config EBTRB = OFF      ; Boot Block Table Read Protection bit (Boot Block (000000-0007FFh) not protected from table reads executed in other blocks)


global_vars   udata_acs 
state res 1 ; state variable, bit0 => Write, bit1 => Review, bit2 => Read
rb3_state res 1 ; state variable, bit0==0 => Button pressed, bit0==1 => Button not pressed,  bit1==1 => Button released
not_initialized_char res 1
curr_index res 1 ; index of current char to be entered and waited
twenty_secs res 1 ; 20
counter res 2  ; timer0 counter, needs to be 2 bytes
twenty res 1
zero res 1
one res 1
two res 1
char0 res 1     ; total of 6 saved chars
char1 res 1
char2 res 1
char3 res 1
char4 res 1
char5 res 1
message_write res 1
message_review res 1
message_read res 1
not_initalized_char res 1
nop_counter res 1
disp_char1 res 1    ; on DISP3
disp_char0 res 1    ; on DISP4
pressed_key res 1
last_pressed_key res 1
key_hit_counter res 1 ; keeps track of how many hits one key got in 1 second
transition_flag res 1 
thirty res 1
thirty_counter res 1
five res 1
five_counter res 1
rd0_state res 1	    ; state variable, bit0==0 => Button pressed, bit0==1 => Button not pressed,  bit1==1 => Button released
rd1_state res 1
rd2_state res 1
rd3_state res 1

 
 
MESSAGE_WRITE equ b'00000001'
MESSAGE_REVIEW equ b'00000010'
MESSAGE_READ equ b'00000100'
NOT_INITIALIZED_CHAR equ b'00001000' ; char of '_' on the display
TWENTY equ 0x14
ONE equ 0x01
   
w_temp  udata 0x23
w_temp

status_temp udata 0x24
status_temp

pclath_temp udata 0x25
pclath_temp

portb_var   udata 0x26
portb_var
   
org     0x00
goto    init

org     0x08
goto    isr             ;go to interrupt service routine

TABLE_NUMS:
    addwf PCL
    retlw b'00111111' ;0
    retlw b'00000110' ;1
    retlw b'01011011' ;2
    retlw b'01001111' ;3
    retlw b'01100110' ;4
    retlw b'01101101' ;5
    retlw b'01111101' ;6
    retlw b'00000111' ;7
    retlw b'01111111' ;8
    retlw b'01101111' ;9
TABLE:
    ADDWF PCL
    RETLW b'01011111' ; a
    RETLW b'01111100' ; b
    RETLW b'01011000' ; c
    RETLW b'01011110' ; d
    RETLW b'01111011' ; e
    RETLW b'01110001' ; f
    RETLW b'01101111' ; g
    RETLW b'01110100' ; h
    RETLW b'00000100' ; i
    RETLW b'00001110' ; j
    RETLW b'01110101' ; k
    RETLW b'00111000' ; l
    RETLW b'01010101' ; m
    RETLW b'01010100' ; n
    RETLW b'01011100' ; o
    RETLW b'01110011' ; p  special calculation to fetch this and below ones
    RETLW b'01010000' ; r
    RETLW b'01100100' ; s
    RETLW b'01111000' ; t
    RETLW b'00011100' ; u
    RETLW b'00101010' ; v
    RETLW b'01101110' ; y
    RETLW b'01011011' ; z
    RETLW b'00000000' ; whitespace

init:
    clrf counter
    clrf state
    clrf zero
    clrf key_hit_counter
    clrf curr_index
    clrf transition_flag
    setf nop_counter ; let nops iterate over 256 times
    movlw NOT_INITIALIZED_CHAR
    movwf not_initalized_char ; '_' variable
    movlw TWENTY
    movwf twenty
    movlw ONE
    movwf one
    movff one, rb3_state ; bit0 is=1 not pressed, bit1=0 not released
    movlw 0x02
    movwf two
    movlw 0x05
    movwf five
    movff five,five_counter
    movff twenty, twenty_secs
    movlw MESSAGE_WRITE
    movwf message_write
    movlw MESSAGE_REVIEW
    movwf message_review
    movlw MESSAGE_READ
    movwf message_read
    movff not_initalized_char, char0
    movff not_initalized_char, char1
    movff not_initalized_char, char2
    movff not_initalized_char, char3
    movff not_initalized_char, char4
    movff not_initalized_char, char5
    movff not_initalized_char, disp_char1
    movff not_initalized_char, disp_char0
    setf last_pressed_key
    movlw d'30'
    movwf thirty
    movff thirty, thirty_counter
    
    ;Disable interrupts
    clrf    INTCON
    clrf    INTCON2

    ;Configure Output Ports
    movlw 0x0F
    movwf ADCON1 ; digital port
    clrf   PORTA  ; clear PORTA
    movlw  b'11000011'
    movwf  TRISA  ; A2, A3, A4, A5 are output
    clrf   PORTD  ; clear PORTD
    clrf   TRISD  ; PORTD is output

    ;Configure Input/Interrupt Ports
    movlw   b'00011000' ; RB3 and RB4 is input pin
    movwf   TRISB       ; TRISB = w_reg = b'00010000' 
    bcf     INTCON2, 7  ; Pull-ups are enabled - clear INTCON2<7>
    clrf    PORTB

    ;Initialize Timer0
    movlw   b'00000111' ;read/write in 16-bit operations
                        ;Timer0 increment from internal clock with a prescaler of 1:32.
			;Disable Timer1 by setting TMR0ON to 0 (for now)
    movwf   T0CON       ;T0CON = b'00000100'
    
    movlw   0x67        ;FFFF-85ED=31250 for 1 sec period of waiting time in MESSAGE_WRITE state; 
    movwf   TMR0H
    movlw   0x6A
    movwf   TMR0L
    goto initial_main
    
set_rb3_state:
    BCF rb3_state,1 ; reset released if exists !!!  
    btfss rb3_state , 0 ; what is rb3's state
    bra rb3_release_check   ; if rb3 pin's state==0 (pressed)
    bra rb3_press_check ; if rb3 pin's state==1 (not pressed)
    rb3_press_check: ; state==1 (not pressed)
	btfsc PORTB,3
	return
	clrf rb3_state,0
	return
    rb3_release_check: ;state==0 (pressed)
	btfsc PORTB,3
	bra release_rb3
	return
	release_rb3:
	   setf rb3_state ; rb3_state[1]==1 means released
           BCF rb3_state,0  ; rb3_state[0]==1 means button not pressed anymore, other bits are not used anyways
	return 
set_rd0_state:
    BCF rd0_state,1
    btfss rd0_state , 0 
    bra rd0_release_check  
    bra rd0_press_check 
    rd0_release_check: ; state==1 (not pressed)
	btfsc PORTD,0
	return
	clrf rd0_state,0
	return
    rd0_press_check: ;state==0 (pressed)
	btfsc PORTD,0
	bra release_rd0
	return
	release_rd0:
	   setf rd0_state 
           BCF rd0_state,0  
	return 
set_rd1_state:
    BCF rd1_state,1
    btfss rd1_state , 0 
    bra rd1_release_check  
    bra rd1_press_check 
    rd1_release_check: ; state==1 (not pressed)
	btfsc PORTD,1
	return
	clrf rd1_state,0
	return
    rd1_press_check: ;state==0 (pressed)
	btfsc PORTD,1
	bra release_rd1
	return
	release_rd1:
	   setf rd1_state 
           BCF rd1_state,0  
	return 
set_rd2_state:
    BCF rd2_state,1
    btfss rd2_state , 0 
    bra rd2_release_check  
    bra rd2_press_check 
    rd2_release_check: ; state==1 (not pressed)
	btfsc PORTD,2
	return
	clrf rd2_state,0
	return
    rd2_press_check: ;state==0 (pressed)
	btfsc PORTD,2
	bra release_rd2
	return
	release_rd2:
	   setf rd2_state 
           BCF rd2_state,0  
	return 
set_rd3_state:
    BCF rd3_state,1
    btfss rd3_state , 0 
    bra rd3_release_check  
    bra rd3_press_check 
    rd3_release_check: ; state==1 (not pressed)
	btfsc PORTD,3
	return
	clrf rd3_state,0
	return
    rd3_press_check: ;state==0 (pressed)
	btfsc PORTD,3
	bra release_rd3
	return
	release_rd3:
	   setf rd3_state 
           BCF rd3_state,0  
	return 	
poll_keypad:
    bcf PORTB, 0
    bcf PORTB, 1 ; do not change input pins only clear output pins.
    bcf PORTB, 2
    clrf PORTD
    
    ; poll RB0 column
    bcf PORTB, 0  ; set RB0=0 low
    bsf PORTB, 1  
    bsf PORTB, 2
    movlw b'00001111' ;RD0,RD1,RD2,RD3 are input rows
    movwf TRISD
    nop nop nop nop nop nop
     poll_1_4_7_star:
	BTFSS PORTD, 3
	retlw d'1' ; 1 is pressed
	BTFSS PORTD, 2
	retlw d'4' ; 4 is pressed
	BTFSS PORTD, 1
	retlw d'7' ; 7 is pressed
	BTFSS PORTD, 0
	retlw d'10' ; * is pressed
	
    clrf TRISD
    clrf PORTD	; safe to change output port
   
    
    
    bcf PORTB, 1  ; set RB1=1 (LOW)
    bsf PORTB, 0  
    bsf PORTB, 2  
    movlw b'00001111' ;RD0,RD1,RD2,RD3 are input rows
    movwf TRISD
    
    nop nop nop nop nop nop
     poll_2_5_8_0:
	BTFSS PORTD, 3
	retlw d'2'
	BTFSS PORTD, 2
	retlw d'5' 
	BTFSS PORTD, 1
	retlw d'8' 
	BTFSS PORTD, 0
	retlw d'0' 
  
    clrf TRISD
    clrf PORTD
    
   
    
    bcf PORTB, 2  ; set RB2=1 (LOW)
    bsf PORTB, 1  
    bsf PORTB, 0  
    movlw b'00001111' ;RD0,RD1,RD2,RD3 are input rows
    movwf TRISD
    nop nop nop nop nop nop
    poll_3_6_9_square:
	BTFSS PORTD, 3
	retlw d'3'
	BTFSS PORTD, 2
	retlw d'6' 
	BTFSS PORTD, 1
	retlw d'9' 
	BTFSS PORTD, 0
	retlw d'12' 

    retlw 0xFF ;   return 0xFF if any value is not pressed.

which_char_to_display:  ; sets char0 to be displayed in porta. this subroutine is checked many times in a second.
    movlw 0xFF
    cpfseq pressed_key		; if no pressed key, '_' will be displayed.
    bra smthg_pressed		; something is pressed
	 cpfseq last_pressed_key    ; if there is no previosly pressed key within 1 sec, return '_' 
	 bra smthg_pressed	   ; else keep displaying the same char as previous state's char. char0 needs to be returned again.
	 movff not_initalized_char, char0
	 return
    smthg_pressed:      ; when curr keypad is 0xFF but prev 25ms loop's state is set keep printing the same char, or curr keypad is pressed
    cpfseq last_pressed_key	    ; here record the very first key press to keep track of it in idle states.
    bra not_the_very_first		    ; if very first key press, jump to another key config
    very_first:
    movff pressed_key,  last_pressed_key ; else, set the prev key as curr key to make sue of it later.
    bra calculate_wreg
    not_the_very_first:
    movlw 0xFF
    cpfseq pressed_key	    
    bra new_key	    ; second key press in 1 second or  pressed on totally diff key in 1 sec
    bra idle_now
    idle_now:
	movff last_pressed_key, pressed_key    ; if there is no key press (idle) but we are in a 's state, counter and curr displayed char needs to be maintained.
	bra calculate_wreg   ; act like we,rre still in the previous state
    new_key:
	movf last_pressed_key, WREG ; to check if it is the same press with current's
	cpfseq pressed_key
	bra diff_key_press
	same_key:		; if same key is pressed again within 1 sec, increment the hit counter.
	INCF key_hit_counter ; hit++ as the key is same as the previous one.
	movlw 0x03 ; i did not use sublw due to picsimlab bug
	subwf key_hit_counter
	btfsc STATUS, N ; if negative hit, add 3 back.
	addwf key_hit_counter  ; else keep the original value of the counter for later use in the same state !!! 
	bra calculate_wreg
	diff_key_press:
	    CLRF        key_hit_counter 
	    movff	pressed_key, last_pressed_key
	    movff	char4, char5	    ; swap chars in the display 
	    movff	char3, char4
	    movff	char2, char3
	    movff	char1, char2
	    movff	char0, char1	    ; char0 will be set in calculate_wreg
	    movlw	NOT_INITIALIZED_CHAR  ; if char5 is still an uninitialized char, continue without transitioning ! 
	    cpfseq	char5
	    setf	transition_flag	    ; with this flag after setting char0 i will transition states before 1 sec expires.
    
    calculate_wreg:
    ; calculate wreg for look-up table
    movlw 0x10
    cpfslt pressed_key		; only key 2 to key 9 has input letters on keypad.
    return
    decf pressed_key		; for example, 2nd key is pressed, then wreg of lookup table will be 0,1,2 only for 3 chars.
    decf pressed_key		; range of letters for a key formula ==> (key-2)*3
    movlw 0x03
    MULWF pressed_key		; multiplication results is in PRODL.
    INCF pressed_key
    INCF pressed_key
    movf key_hit_counter, WREG	; move hit count to WREG.
    addwf PRODL, WREG		;keep the result in wreg to index look-up table.
    RLNCF WREG,WREG		; multiply by 2 as instruction byte size
    call TABLE			; fetches binary version of the corresponding char from the look-up table.
    movwf char0
    TSTFSZ  transition_flag	; if this new char0 fulls the total char count limit that is 6,
    bra end_writing_state
    return
    end_writing_state:
    movff   message_review , state  ; main will start with next state after displaying ends.
    clrf transition_flag
    return
  
initial_main:
    call set_rb3_state
    btfss rb3_state,1 ; repeat until released button is set
    goto initial_main
    
    ;;;; rb3 is released ;;;;
    bsf state, 0 ; set bit0=1 as state becomes message write
    ;Enable interrupts
    movlw   b'10101000' ;Enable Global, TIMER0 and RB interrupts by setting GIE, TMR0IE and RBIE bits to 1
    movwf   INTCON
    bsf     T0CON, 7    ;Enable Timer0 by setting TMR0ON to 1
main:
    btfsc   state,0         ; Is state 0?
    bra     mes_write
    btfsc   state,1	    ; Is state 1?
    bra	    mes_review
    btfsc   state,2	    ; Is state 2?
    bra	    mes_read
    mes_write:
	    call poll_keypad ; check key press
	    movwf pressed_key ; keep pressed key in pressed_key
	    
	    bcf PORTB,0 ;RB0 cleaned 
	    bcf PORTB,1 ;RB1 cleaned
	    bcf PORTB,2 ;RB2 cleaned
	    clrf   TRISD  ; PORTD is output
	    clrf   PORTD  ; clear PORTD	; safe to change output port
	    call which_char_to_display ; for all cases in 1 second this sets set char0.
	    
	movlw	0x14	    ; to check against 20
	cpfseq twenty_secs
	goto not_twenty
	goto display_twenty
	not_twenty:
	movlw	0x0A    ; TIMER0 interrupt burada geliyor, asagidan devam ediyor. 
	cpfseq twenty_secs
	goto not_ten
	goto display_ten
	not_ten:
	cpfsgt	twenty_secs    
	goto	less_than_ten
	goto	larger_than_ten
	less_than_ten:    
	    movf twenty_secs, 0
	    MULLW 0x02
	    movff  PRODL, WREG   ; number x 2
	    call TABLE_NUMS	
	    bsf PORTA, 3	;  select display3
	    movwf PORTD		
	    
	    waitx_x:
	    INCF nop_counter	; wait 256 times to display  the digit enough amount of time 
	    btfss STATUS, 3	; did overflow occur?
	    goto waitx_x
	    bcf PORTA, 3
	    TSTFSZ twenty_secs ; if 0, since interrupt hits here, immediately continue next state.
	    goto not_zero_yet
	    goto time_expired
	    not_zero_yet:
		goto finish_loop  ; this also enables the intterup back again
	    time_expired:
		movff   message_review , state  ; global counter, 20 secs, is expired. We will not visit mes_write anymore.
		goto main  ; no need to enable the 20-seconds-interrupt again as we transition to next state
	larger_than_ten:
	    subwf twenty_secs   ; decrement counter by 10 to have modulo
	    movff twenty_secs, WREG
	    MULLW 0x02
	    movff  PRODL, WREG   ; remainder x 2 will be the new pc for the right digit
	    call TABLE_NUMS	
	    bsf PORTA,3
	    movwf PORTD   ; second digit was kept in WREG
	   
	    wait1x_x:
	    INCF nop_counter
	    btfss STATUS, 3 ; did overflow occur?
	    goto wait1x_x
	    bcf PORTA, 3
	    movlw 0x0A
	    addwf twenty_secs ; put back temp subtracted amount
	    movlw b'00000110'
	    bsf PORTA, 2	    
	    movwf PORTD   ; display "1" 
	   
	    wait1x_1:
	    INCF nop_counter
	    btfss STATUS, 3 ; did overflow occur?
	    goto wait1x_1
	    bcf PORTA, 2
	    goto finish_loop
	display_ten:
	    movlw b'00000110' 
	    bsf PORTA, 2
	    movwf PORTD   ; display "1" 
	   
	    wait10_1:
	    INCF nop_counter
	    btfss STATUS, 3 ; did overflow occur?
	    goto wait10_1
	    
	    bcf PORTA,2
	    movlw b'00111111' 
	    bsf PORTA,3
	    movwf  PORTD    ; display "0" 
	   
	    wait10_0:
	    INCF nop_counter
	    btfss STATUS, 3 ; did overflow occur?
	    goto wait10_0
	    
	    bcf PORTA,3
	    goto    finish_loop
	display_twenty:
	    movlw b'01011011' 
	    bsf PORTA, 2
	    movwf PORTD   ; display "2" 
	   
	    wait20_2:
	    INCF nop_counter
	    btfss STATUS, 3 ; did overflow occur?
	    goto wait20_2
	    bcf PORTA,2
	    movlw b'00111111' 
	    bsf PORTA,3
	    movwf PORTD    ; display "0" 
	   
	    wait20_0:
	    INCF nop_counter
	    btfss STATUS, 3 ; did overflow occur?
	    goto wait20_0
	    bcf PORTA,3
	finish_loop:
	    bsf PORTA, 4	;  select display4
	    movff char1, PORTD
	    wait_char1:
	    INCF nop_counter	
	    btfss STATUS, 3	; did overflow occur?
	    goto wait_char1
	    bcf PORTA, 4
	    
	    bsf PORTA, 5	;  select display5
	    movff char0, PORTD	
	    wait_char0:
	    INCF nop_counter	
	    btfss STATUS, 3	; did overflow occur?
	    goto wait_char0
	     
	    bcf PORTA, 5
	    goto    main
    mes_review:
    mes_read:
    goto    main

isr:
    btfss   INTCON, 2       ; check TMR0IF, is this a timer0 interrupt?
    retfie    ;No. Goto PORTB on change interrupt handler part
    call    save_registers  ;Save current content of STATUS and PCLATH registers to be able to restore them later
    goto    timer_interrupt ;Yes. Goto timer interrupt handler part
    
;;;;;;;;;;;;;;;;;;;;;;;; Timer interrupt handler part ;;;;;;;;;;;;;;;;;;;;;;;;;;
timer_interrupt:
    bcf         INTCON, 2               ;Clear TMR0IF !!!
    TSTFSZ       TMR0L                  ;did it really overflow ?
    goto	timer_interrupt_erroneous_exit    ;No, then exit from interrupt service routine
    decf	twenty_secs             ;Yes, then one second has passed
    clrf	key_hit_counter
    setf	last_pressed_key	    ; clear this when switching seconds
    movlw	NOT_INITIALIZED_CHAR  ; if char not entered do not save anything
    cpfseq	char0		    ; if char entered needs to be saved after 1 sec, save it and shift the chars on display.
    bra save_char
    bra timer_interrupt_exit
    save_char:	
    movff	char4, char5	    ; swap chars in the display 
    movff	char3, char4
    movff	char2, char3
    movff	char1, char2
    movff	char0, char1
    movlw	NOT_INITIALIZED_CHAR  ; if char5 is still an uninitialized char, continue without transitioning ! 
    cpfseq	char5	    
    movff       message_review , state  ; else, transition to next state ! 
    
timer_interrupt_exit:
    movlw   0x67      
    movwf   TMR0H
    movlw   0x6A
    movwf   TMR0L
    timer_interrupt_erroneous_exit:
	call    restore_registers   ;Restore STATUS and PCLATH registers to their state before interrupt occurs
	retfie 

;;;;;;;;;;;; Register handling for proper operation of main program ;;;;;;;;;;;;
save_registers:
    movwf 	w_temp          ;Copy W to TEMP register
    swapf 	STATUS, w       ;Swap status to be saved into W
    clrf 	STATUS          ;bank 0, regardless of current bank, Clears IRP,RP1,RP0
    movwf 	status_temp     ;Save status to bank zero STATUS_TEMP register
    movf 	PCLATH, w       ;Only required if using pages 1, 2 and/or 3
    movwf 	pclath_temp     ;Save PCLATH into W
    clrf 	PCLATH          ;Page zero, regardless of current page
    return

restore_registers:
    movf 	pclath_temp, w  ;Restore PCLATH
    movwf 	PCLATH          ;Move W into PCLATH
    swapf 	status_temp, w  ;Swap STATUS_TEMP register into W
    movwf 	STATUS          ;Move W into STATUS register
    swapf 	w_temp, f       ;Swap W_TEMP
    swapf 	w_temp, w       ;Swap W_TEMP into W
    return

end


