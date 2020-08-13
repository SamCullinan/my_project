 
    #define Debug
    ;
    ; This Application uses TMR0 to set a "seconds" flag in the interrupt
    ;        handler. The context saving is handled correctly, finally. And the
    ;        mainline code checks if the seconds flag is set. If so, it calls the
    ;        Digit_A chain of subroutines to increment the seconds and display.
    ;        Otherwise (!!!), it simply continues in the loop, looking for pressed
     buttons.
    ;        Unfortunately, the button debouncing routine sucks. And it is
   ;        possible to screw something up. So, I may make the button push call
     the
  ;        Digit_A routine. That would work, but not the way I want it to. I
     really
   ;        want it to just add minutes separate from hours, so that if I hit the
   ;        minutes 59 times, it doesn't roll over hours. This may just be a nit-
     pick
   ;        that I decide to live with anyway. We'll see.
   ;
   ; Hardware Notes:
   ;   This application runs on a 16F628A executing at 4 MHz
   ;
   ;        Port B is hooked to a bus 8 bits wide with 3 '573s on it
   ;        the latch signal is controlled by port A
   ;        I have set port A to have <4,3> be minutes and hours
   ;        advance, and <2,1,0> be the '573 latch signals
   ;        Good old port B is the 8-bit data bus.
   ;
   ;        Seconds were deleted, but the code remained.
   ;

   ; MPLAB crap here

              ; absolute listing tabs=5, lines=97, trim long lines=ON

              LIST R=DEC, P=16F628A

                     INCLUDE "p16f628a.inc"

   ; Registers
   CBLOCK 0x020
           	_w, _status                         ;   Context Register Save Values
           	digitA ;
           	digitB ;
           	digitC ;
           	digitD ;
           	digitE ;
           	digitF ;
           	scratch;
           	keys;
           	PMregister;
           	temp_w;
           	second_flag;
           	bres_hi;
           	bres_mid;
           	bres_lo;

             ;adopted from the roman black 1-second routine

   ENDC

   PAGE

       __CONFIG _CP_OFF & _PWRTE_ON & _WDT_OFF & _XT_OSC

   	;     Mainline of Timer

   		org        0

             	movlw    2                          ;   Setup the Count
             	goto     Mainline


   		org       4

   Int                                ;   Interrupt Handler

           	movwf      _w                         ;   Save Context Registers
           	movf       STATUS, w                  ;    - Assume TMR0 is the only enabled
      Interrupt
           	movwf      _status

           ;Roman Black's 1-sec interrupter
            	tstf    bres_mid;                test if mid = 0
            	skpnz                                   ;nz = no underflow needed
            	decf    bres_hi,f;               z, no underflow so dec msb
            	decfsz bres_mid,f;      dec mid byte = subtract 256
             	goto IntEnd;

             	tstf    bres_hi;                test hi byte
             	skpz            ;                       z = high and mid one second!
             	goto IntEnd;
            	;from here to IntEnd is code for when you reach 1 second
             	movlw   0x0F
             	movwf   bres_hi
             	movlw   0x42
             	movwf   bres_mid
             	movlw   0x40
             	addwf   bres_lo,f
            	skpnc   ;no overflow
            	incf    bres_mid,f      ;add 256 (absorbing error!)

             	movlw     0x01;
             	movwf     second_flag;

  IntEnd
            	bcf      INTCON, T0IF               ;   Reset the Interrupt Flag
            	bcf      STATUS, RP0
            	movf     _status, w                 ;   Restore the Context Registers
             	movwf    STATUS
            	swapf    _w, f
            	swapf    _w, w
             	retfie                                       ;return from interrupt


  Digit_A
            	incf      digitA, f
            	movlw     0xA
             	subwf     digitA, w
            	btfss     STATUS, C                 ;A overflow?
                       goto Display

   Digit_B
             	movlw     0x00
            	movwf     digitA         ;clear A
             	incf      digitB, f
             	movlw     0x6
             	subwf     digitB, w
             	btfss     STATUS, C
                       goto Display
   Clear_seconds
           	movlw       0x00           ;
           	movwf       digitA         ;
           	movwf       digitB         ;

   Digit_C
             	incf    digitC, f
             	movlw   0xA
     	        subwf   digitC, w
	        btfss   STATUS, C
                goto Display

   Digit_D
             	movlw   0x00
             	movwf   digitC
             	incf    digitD, f
            	movlw   0x6
             	subwf   digitD, w
             	btfss   STATUS, C
                goto Display

   Clear_minutes
           	movlw     0x00
           	movwf     digitC
           	movwf     digitD;

   Digit_E
             	incf    digitE, f
             	swapf   digitF, w
             	iorwf   digitE, w
             	movwf   PMregister;
             	movlw   0x13
             	subwf   PMregister, w
             	btfsc   STATUS, C
                     goto Clear_hours
             	movlw   0xA
            	subwf   digitE, w
             	btfss   STATUS, C
                     goto Display

   Digit_F
             	movlw   0x00
             	movwf   digitE
             	incf    digitF, f
	        goto    Display

   Clear_hours
        	movlw     0x01;
       		movwf     digitE;
           	movlw     0x00;
           	movwf     digitF;

   Display

             	swapf   digitB,   w       ;seconds first
            	iorwf   digitA,   w       ;load a into w
             	movwf   PORTB             ;swap nibbles
             	bsf               PORTA,0         ;or with b
            	 movlw   0x02              ;put on port
            	call Delay                        ;enable latch bit 0
             	bcf               PORTA,0         ;delay

             	swapf   digitD,   w         ;minutes next
             	iorwf   digitC,   w
             	movwf   PORTB
             	bsf               PORTA,1           ;latch bit 1
             	movlw   0x02
             	call Delay
             	bcf               PORTA,1

             	swapf   digitF, w       ;hours next
             	iorwf   digitE, w
             	movwf   PORTB
             	bsf             PORTA,2         ;latch bit 1
             	movlw   0x02
              	call Delay
              	bcf                 PORTA,2

              	movlw    0xFF
              	movwf    PORTB                ;tristates the bus
              	movlw    0x00
              	movwf    PORTA
              	movlw    0x00
              	movwf    second_flag;
              	return

   Delay                                                      ;delays by amount in w
              	movwf       temp_w                         ; subroutine, not goto!
   Delay_loop
           decfsz      temp_w,f
           	goto        Delay_loop
           return



   Mainline                                                    ;   Setup the PWM And then
                                                                        ;Monitor it, Updating
      "PWMOn"
             	movlw    0x00
              	movwf    second_flag;
              	movlw    0x08                           ; all digits are 0
              	movwf    digitA
              	movlw    0x05
              	movwf    digitB
              	movlw    0x09                           ;
              	movwf    digitC
              	movlw    0x05
              	movwf    digitD
              	movlw    0x02                           ; Except the hours
              	movwf    digitE
              	movlw    0x01
              	movwf    digitF                         ; So it starts at midnight

              	movlw    0x0F
              	movwf    bres_hi
              	movlw    0x42 +1
              	movwf    bres_mid
              	movlw    0x40
              	movwf    bres_lo                        ;loads 1 million plus 1 256 into
                                                                      ;the 24 bit 
register
 
              	movlw     0x00
              	movwf     PMregister
              	movlw     0x00
              	movwf     temp_w
              	bcf               STATUS, RP1
              	bsf       STATUS, RP0            ; Goto Bank 1 to set Port Direction
              	movlw     0x00;
              	movwf     TRISB           ; PORTB is Output
              	movlw     0x18;
              	movwf     TRISA           ; PORTA is output too
              	movlw    0x0C0                   ; Debug - Minimum Timer Delay
	
              	movwf    OPTION_REG
              	bcf      STATUS, RP0                ;       Go back to Bank 0
              	clrf     TMR0                                     ; Start the Timer from
      Scratch

              	;movlw   0xC8
              	;movwf   OPTION_REG
              	movlw    0x00;
              	movwf    PORTA;
              	movlw    0xFF;
              	movwf    PORTB;
              	movlw b'10100000'
              	;movlw (1 << GIE) + (1 << T0IE)
              	movwf INTCON                    ;       Enable Interrupts

   Loop                                                 ; Loop Here
            ;   if button 1 and 2 are on for 1 second, then
              ;   blink the display and show the time
              ;   remember that the 74141 is display blanking
              ;   for invalid BCD codes. Button 1 increments
              ;   hours, button 2 increments seconds
              ;   if none is selected for 5 seconds it goes back to
              ;   displaying the time. Use WDT for this one. Just
              ;   remember to check that the context register for option
              ;   and status go back the way they should.

              	btfsc     second_flag, 0; second flag set
                        call Digit_A;

              	movf      PORTA, w
              	xorwf     keys, w
              	andlw     0x18               ;only check the button pins
              	btfsc     STATUS,Z           ;test, if Z=0, no buttons
              	goto      Loop               ;no, loop again
              	xorwf     keys,f

   Debounce
              	movlw     0x80
              	call      Delay

              	movf      PORTA, w
              	movwf     keys

              	btfss     keys, 3         ;minutes down
                       goto    add_minutes     ;
              	btfss     keys, 4         ;hours down
                       goto    add_hours
           goto         Loop            ; Let the Interrupt Handler Work in the
      Background

   add_hours
              	incf         digitE, f
              	swapf        digitF, w
              	iorwf        digitE, w
              	movwf        PMregister;
              	movlw        0x13
              	subwf        PMregister, w
              	btfsc        STATUS, C
                       goto reset_hours
              	movlw     0xA
              	subwf     digitE, w
              	btfss     STATUS, C
                        goto    Loop
              	movlw     0x00
              	movwf     digitE
              	incf      digitF, f
                       goto    Loop

   reset_hours
              	movlw        0x01;
              	movwf        digitE;
              	movlw        0x00;
              	movwf        digitF;
                        goto    Loop

   add_minutes
              	incf         digitC, f
              	movlw        0xA
              	subwf        digitC, w
              	btfss        STATUS, C
                goto      Loop
              	movlw   0x00
              	movwf   digitC
              	incf    digitD,   f
              	movlw   0x6
              	subwf   digitD,   w
              	btfss   STATUS,   C
                  goto      Loop
              	movlw   0x00
              	movwf   digitC
              	movwf   digitD;
                 goto      Loop


   end
