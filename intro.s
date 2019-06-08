.import source "../lib/lib.s"
.import source "../lib/easingLib.s"
.import source "../lib/const.s"

.var music = LoadSid("work-files/Very_Bland.sid")

_outputMusicInfo()

//Values
.label logomask1 = %00011000
.label rasterLine = $2f
.label maxSplitSize = $3f
.label framebufferSize = $80
.label animationFrameSize = 32
.label spriteShiftOffsets = $100/15 
.label animationFrameCount = 19 //includes this value

//Zeropage
.label zp_fldFrameLutPtr = $df
.label zp_frameLutPtr = $e0
.label zp_animationOffsetPtr = $e1
.label zp_rasterbarOffsetPtr = $e2
.label zp_spriteScrollOffsetPtr = $e3
.label zp_bitmask_controlchar = $e4
.label zp_bitmask_color = $e5
.label zp_bitmask_speed = $e6
.label zp_bitmask_raster = $dd
.label zp_rasterBarRenderTrigger = $dc
.label zp_spriteScrollCurrentColor = $e7
.label zp_spriteScrollCurrentSpeed = $e8
.label zp_virtual_d011 = $e9
.label zp_spriteScrollDelayTimer = $de
.label zp_spriteScrollColorFlasherCounter = $f8
.label zp_animationSelectorPtr = $d9
.label zp_animationDatasetSelector = $d8

//used to pass params to functions
.label functionCallParam1 = $ee 
.label functionCallParam2 = $ef


//todo rename these to correct ZP naming and check if they are still used!
.label splitPtr = $d7
.label splitIdx = $d6

.label swingPtr = $d5
.label swingIdx = $d4

.label D016Buffer = $d3

//addresses
.label logomap = $4000 //todo rename this
.label FLD_FRAME_LUT = $cf00


.pc = $0801 "Basic Upstart"
:BasicUpstart(start) // 10 sys$0810
.pc =$5000 "Program"
start:
    :mov #$00: swingPtr
	:mov #$00: $d020
	:mov #$0c: $d021
	:fill_1K($00, $d800)
    :fill_1K($ff, $0400) //clear screen with blank chars
    jsr funcInitData
    sei
    lda #$36
    sta $01
    cli
	:setupInterrupt(irq, rasterLine) // last six chars (with a few raster lines to stabalize raster)
!loop:
    jmp !loop-

/********************************************
MAIN INTERRUPT LOOP
*********************************************/

irq:
	:startInterrupt()
	:doubleIRQ(rasterLine)
    lda #logomask1
    sta $d018

    lda D016Buffer
    sta $d016

    nop
    nop
    nop
    :waitX(4)
    nop

    lda #$00
    sta $d020
    sta $d021

    lda FRAMEBUFFER_BORDERS
    sta $d020
    sta $d021
    ldx FRAMEBUFFER_CONTROL
    stx $d018
    ldy #$06
!loop:
    dey
    bne !loop-
    nop
    nop

    .for(var i=0;i<framebufferSize/8;i++){
        .var offset = (i * 8)+1
        ldx FRAMEBUFFER_CONTROL + offset
        stx $d018
        lda FRAMEBUFFER_COLORS + offset
        sta $d020
        sta $d021
        nop
        nop
        nop
        nop
        .for(var j = 1;j<7;j++){
            .if((offset + j) == 63){
                lda FRAMEBUFFER_COLORS + offset + j
                sta $d020
                sta $d021
                ldx FRAMEBUFFER_CONTROL + offset + j
                stx $d018
                ldy #$06
            !loop:
                dey
                bne !loop-
                nop
                nop
                bit $ea
            }
            else{
                lda FRAMEBUFFER_COLORS + offset + j
                sta $d020
                sta $d021
                ldx FRAMEBUFFER_CONTROL + offset + j
                stx $d018
                ldy #$08
            !loop:
                dey
                bne !loop-
                nop
            }
        }
        lda FRAMEBUFFER_COLORS + offset + 7
        sta $d020
        sta $d021
        ldx FRAMEBUFFER_CONTROL + offset + 7
        stx $d018
        ldy #$06
    !loop:
        dey
        bne !loop-
        nop
        nop
    }

    lda #$00
    nop
    nop
    nop
    nop
    nop
    sta $d020
    sta $d021
    inc swingPtr
    lda #$ff
    sta REG_SPRITE_ENABLE

    lda #$1b
    sta $d011

    jsr funcDisplaySpriteSplitA

    //swing for logo
    ldx swingPtr
    lda #$00
    sta swingIdx
    lda XPLOTS,x
    and #%00000111
    sta swingIdx
    sec
    lda #$07
    sbc swingIdx
    ora #%11010000
    sta D016Buffer
    lda XPLOTS,x
    lsr
    lsr
    lsr
    clc
    tax 
    //note - you need X!
    stx stashx1
    .for(var row=0;row<6;row++){
        .for(var col=0;col<40;col++){
            lda logomap+(row*$100)+col,x 
            sta $0400+((1+row)*40)+col
        }
    }
    jsr funcDisplaySpriteSplitB
    ldx stashx1: #$00
    .for(var row=6;row<8;row++){
        .for(var col=0;col<40;col++){
            lda logomap+(row*$100)+col,x 
            sta $0400+((1+row)*40)+col
        }
    }


//hyperscreen split


!loop:
lda $d012
cmp #$f8
bne !loop-


    lda #$13
    sta $d011

    .for(var row=8;row<12;row++){
        .for(var col=0;col<40;col++){
            lda logomap+(row*$100)+col,x 
            sta $0400+((1+row)*40)+col
        }
    }

    ldy zp_spriteScrollCurrentSpeed
    bne !loop+
    jsr funcRenderSpriteScroller
    jmp !skip+
!loop:
    jsr funcRenderSpriteScroller
    dey
    bne !loop- 
!skip:
    jsr funcDrawRasters
    jsr funcRenderRasterBars
    jsr funcFlashSpriteColors
    jsr music.play     
//----------------------------------------------

    lda #$00
    sta REG_SPRITE_ENABLE
    sta $d020

	:mov #<irq: $fffe
	:mov #rasterLine:$d012
	:mov #$ff: $d019
	:endInterrupt()


/********************************************
FUNCTIONS
*********************************************/

funcInitData:
    lda #%10000000
    sta zp_bitmask_controlchar
    lda #%01000000
    sta zp_bitmask_color
    lda #%00100000
    sta zp_bitmask_speed
    lda #%00010000
    sta zp_bitmask_raster

    lda #$01
    sta zp_spriteScrollCurrentColor
    sta zp_spriteScrollCurrentSpeed

    lda #$02
    sta zp_animationDatasetSelector

    lda #$00
    sta zp_frameLutPtr
    sta zp_animationOffsetPtr
    sta zp_rasterbarOffsetPtr
    sta zp_fldFrameLutPtr
    sta zp_spriteScrollDelayTimer
    sta zp_spriteScrollColorFlasherCounter
    sta zp_rasterBarRenderTrigger
    sta zp_animationSelectorPtr

    lda #<SCROLLTEXT
    sta <mem_spriteScolltextOffsetPtr
    lda #>SCROLLTEXT
    sta >mem_spriteScolltextOffsetPtr

    ldx #$00
    ldy #$00
    lda #music.startSong-1
    jsr music.init

    lda #$c0
!loop:
    cmp $d012
    bne !loop-

    rts

funcRenderRasterBars:
    ldx zp_rasterbarOffsetPtr
    lda RASTEROFFSETS,x
    tax
    .for(var i=0;i<$80;i++){
        lda FRAMEBUFFER_BACKGROUND+i,x
        sta FRAMEBUFFER_COLORS+i
        lda #$18
        sta FRAMEBUFFER_CONTROL+i
    }
    inc zp_rasterbarOffsetPtr
    lda zp_frameLutPtr
    tax 
    lda ANIMATION_SELECTOR_LUT,x
    sta functionCallParam1
    lda zp_animationOffsetPtr
    sta functionCallParam2
    jsr funcRenderFrambufferAnimationFrame
    inc zp_frameLutPtr
    inc zp_animationOffsetPtr
!skip:
rts

/*
Render animation frame - called by funcRenderRasterBars
functionCallParam1 = animation frame offset
functionCallParam2 = y bounce of animation offset
*/
funcRenderFrambufferAnimationFrame:
    lda functionCallParam1
    tax
    lda ANIMATION_FRAMES_LUT,x
    sta frametarget
    inx 
    lda ANIMATION_FRAMES_LUT,x
    sta frametarget + 1
    lda functionCallParam2
    tax
    lda zp_animationDatasetSelector
    beq !normal+
    cmp #$02
    bne !skip+
    lda PHASE1_RASTER_ANIMATION_OFFSETS,x
    jmp !setup+
!skip:
    cmp #$01
    bne !skip+
    lda PHASE2_RASTER_ANIMATION_OFFSETS,x
    jmp !setup+
!skip:
!setup:
    cpx #$ff
    bne !abnormal+
    dec zp_animationDatasetSelector
    jmp !abnormal+
!normal:
    lda RASTER_ANIMATION_OFFSETS,x
!abnormal:
    clc
    adc #31
    tax
    ldy #31
!loop:
    lda frametarget: ANIMATION_FRAMES,y
    beq !skip+
    sta FRAMEBUFFER_COLORS,x
    lda #$1a
    sta FRAMEBUFFER_CONTROL,x
!skip:
    dex
    dey
    bne !loop-
    rts
    
//SPRITE SCROLLER
funcRenderSpriteScroller:
    lda zp_spriteScrollDelayTimer
    beq !skip+
    inc zp_spriteScrollDelayTimer
    bne !noReset+
    lda #$01
    sta zp_spriteScrollCurrentSpeed
!noReset:
    rts
!skip:
    ldx zp_spriteScrollOffsetPtr
    inx
    cpx #spriteShiftOffsets
    beq !skip+
    stx zp_spriteScrollOffsetPtr
    rts
!skip:
    lda #$00
    sta zp_spriteScrollOffsetPtr
    .for(var i=14;i>0;i--){
        lda SPRITE_POINTERS+i-1
        sta SPRITE_POINTERS+i
        lda SPRITE_COLORS+i-1
        sta SPRITE_COLORS+i
    }
    jsr funcGetNextScrollTextChar
    bit zp_bitmask_controlchar
    beq !skip+ //not a control char, just act normal
    bit zp_bitmask_color
    beq !next+
    jsr funcGetNextScrollTextChar
    sta zp_spriteScrollCurrentColor
    jmp !finish+
!next:
    bit zp_bitmask_speed
    beq !next+
    and #%00001111
    sta zp_spriteScrollCurrentSpeed
    cmp #$00
    bne !finish+
    lda #$80
    sta zp_spriteScrollDelayTimer
    jmp !finish+
!next:
    bit zp_bitmask_raster
    beq !next+
    lda #$01
    sta zp_rasterBarRenderTrigger
    jmp !finish+
!next:

!finish:  
    /*get a real char - no two control chars next to each other!*/
    jsr funcGetNextScrollTextChar
!skip:
    clc
    /*
    sbc #$20 = the start of our font is at space char
    adc #$c0 = sprite pointers are at $3000
    */
    adc #($c0-$20) //we just do it in one step instead as $a0
    sta SPRITE_POINTERS
    lda zp_spriteScrollCurrentColor
    sta SPRITE_COLORS
    rts

funcFlashSpriteColors:
    ldy #$00
    ldx zp_spriteScrollColorFlasherCounter
!loop:
    lda SPRITE_COLORS,y
    and #%11110000
    cmp #$00
    beq !skip+
    sta flashPtr //base offset of zero
    lda flashPtr: SPRITE_FLASH_COLORS,x
    sta SPRITE_COLORS,y
!skip:
    iny
    cpy #$10
    bne !loop-
    inx
    cpx #$10
    bne !skip+
    ldx #$00
!skip:
    stx zp_spriteScrollColorFlasherCounter
    rts

funcDrawRasters:
    lda zp_rasterBarRenderTrigger
    bne !skip+
    rts
!skip:
    cmp #$01
    bne !skip+
    lda RASTER_COLORS + $100
    sta FRAMEBUFFER_BACKGROUND + $100
!skip:
    tax
    lda RASTER_COLORS + $100,x
    sta FRAMEBUFFER_BACKGROUND + $100 ,x
    txa
    eor #$ff
    lda RASTER_COLORS,x
    sta FRAMEBUFFER_BACKGROUND,x
    inc zp_rasterBarRenderTrigger
    rts

/*
called by renderSpriteScroller
*/
funcGetNextScrollTextChar:
    lda mem_spriteScolltextOffsetPtr: SCROLLTEXT
    bne !skip+
    lda #<SCROLLTEXT
    sta mem_spriteScolltextOffsetPtr
    lda #>SCROLLTEXT
    sta mem_spriteScolltextOffsetPtr + 1
    lda SCROLLTEXT
!skip:
    inc mem_spriteScolltextOffsetPtr
    bne !skip+
    inc mem_spriteScolltextOffsetPtr + 1
!skip:
    rts

.var bitmasks = List()
.eval bitmasks.add(%00000001)  
.eval bitmasks.add(%00000010)  
.eval bitmasks.add(%00000100)  
.eval bitmasks.add(%00001000)  
.eval bitmasks.add(%00010000)  
.eval bitmasks.add(%00100000)  
.eval bitmasks.add(%01000000)  
.eval bitmasks.add(%10000000)  

funcDisplaySpriteSplitA:
    lda #$00
    sta REG_SPRITE_X_MSB
    ldx zp_spriteScrollOffsetPtr
.for(var i=0;i<8;i++){
        .var baseline = 0
        .var off = ((i+baseline)*($100/15))
        lda SPRITE_SCROLL_X_LO + off,x
        sta REG_SPRITE_X_0 + (i*2)
        lda SPRITE_SCROLL_X_HI + off,x
        beq !skip+
        lda REG_SPRITE_X_MSB
        ora #bitmasks.get(i)
        sta REG_SPRITE_X_MSB
    !skip:
        lda SPRITE_SCROLL_Y + off,x
        sta REG_SPRITE_Y_0 + (i*2)
        lda SPRITE_POINTERS + i + baseline
        sta REG_SPRITE_DATA_PTR_0 + i
        lda SPRITE_COLORS + i + baseline
        and #%00001111
        sta REG_SPRITE_COLOUR_0 + i
}
    rts

funcDisplaySpriteSplitB:
    lda #$00
    sta REG_SPRITE_X_MSB
    ldx zp_spriteScrollOffsetPtr
.for(var i=0;i<7;i++){ //don't use sp8 as the overlap sprite is sp8
        .var baseline = 8
        .var off = ((i+baseline)*($100/15))
        lda SPRITE_SCROLL_X_LO + off,x
        sta REG_SPRITE_X_0 + (i*2)
        lda SPRITE_SCROLL_X_HI + off,x
        beq !skip+
        lda REG_SPRITE_X_MSB
        ora #bitmasks.get(i)
        sta REG_SPRITE_X_MSB
    !skip:
        lda SPRITE_SCROLL_Y + off,x
        sta REG_SPRITE_Y_0 + (i*2)
        lda SPRITE_POINTERS + i + baseline
        sta REG_SPRITE_DATA_PTR_0 + i
        lda SPRITE_COLORS + i + baseline
        and #%00001111
        sta REG_SPRITE_COLOUR_0 + i
}
    rts


/********************************************
DATASETS
*********************************************/

//used for plotting the logo
.align $100
.pc = * "Data sets"
XPLOTS:	
.fill $100, sinus(i, $7f, $7f, $100)

.pc = * "SPRITE SCROLLER DATASETS"

.align $100
SPRITE_SCROLL_Y:
.fill $40, easeOut(i,$c8,-$10,$40)
.fill $40, easeIn(i,$b8,$10,$40)
.fill $08, $c8+(i/$08*$37)
.fill $08, easeOut(i,$ff,-$18,$08)
.fill $08, easeIn(i,$e7,$18,$08)
.fill $04, easeOut(i,$ff,-$08,$04)
.fill $04, easeIn(i,$f7,$08,$04)
.fill $04, easeOut(i,$ff,-$02,$04)
.fill $04, easeIn(i,$fd,$02,$04)
.fill $58, $ff

.align $100
SPRITE_SCROLL_X_LO:
.for(var i=$100;i>0;i--){
    .byte <(i/$100*346)
}

.align $100
SPRITE_SCROLL_X_HI:
.for(var i=$100;i>0;i--){
    .byte >(i/$100*346)
}

.align $100
SPRITE_POINTERS:
.fill $0f, $c0 //fill sprites with spaces

SPRITE_COLORS:
.fill $0f, $01

.align $100
SPRITE_FLASH_COLORS:
.byte $00, $06, $0e, $0f, $03, $01, $01, $01, $03, $0f, $0e, $06, $00, $00, $00, $00 //first line never gets used
.byte $10, $16, $1e, $1f, $13, $11, $11, $11, $13, $1f, $1e, $16, $10, $10, $10, $10
.byte $20, $2b, $2c, $2f, $21, $2f, $2c, $2b, $20, $2b, $2c, $2f, $21, $2f, $2c, $2b
.byte $3b, $34, $3b, $34, $36, $34, $36, $34, $3b, $34, $3b, $34, $36, $34, $36, $34
.byte $40, $4b, $49, $42, $47, $41, $47, $42, $49, $4b, $46, $4e, $41, $4e, $46, $40


.pc = * "RASTER EFFECTS"

//used for raster offset animation
.align $100
RASTEROFFSETS:	
//.fill $100, sinus(i, $ff, $ff, $100)
.fill $80, easeIn(i, $ff, -$ff, $80)
.fill $80, easeOut(i, $00, $ff, $80)

RASTER_COLORS: //$ff is the 'stop' color

.var rbars = LoadPicture("work-files/raster_colors.png")
.var rasterLut = Hashtable()
.eval rasterLut.put($000000,$00)
.eval rasterLut.put($ffffff,$01)
.eval rasterLut.put($68372b,$02)
.eval rasterLut.put($70a4b2,$03)
.eval rasterLut.put($6f3d86,$04)
.eval rasterLut.put($588d43,$05)
.eval rasterLut.put($352879,$06)
.eval rasterLut.put($b8c76f,$07)
.eval rasterLut.put($b0ffb9,$08)
.eval rasterLut.put($6f4f25,$09) 
.eval rasterLut.put($433900,$09) 
.eval rasterLut.put($9a6759,$0a)
.eval rasterLut.put($ffa0ac,$0b)
.eval rasterLut.put($00ff0d,$0c)
.eval rasterLut.put($9ad284,$0d)
.eval rasterLut.put($6c5eb5,$0e)
.eval rasterLut.put($fffff0,$0f)

.for(var i=0;i<$200;i++){
    .eval var tmpColor = rbars.getPixel(0,i)
    .print toHexString(tmpColor) + " = " + rasterLut.get(tmpColor)
    .byte rasterLut.get(tmpColor)
}


//SCROLLER!!!
.pc = $8000 "scrolltext"
.align $100
SCROLLTEXT:
.text " "
scrollColor($01)
.text " "
scrollSpeed($02)
.text "  PARTY PEOPLE "
scrollColor($04)
.text "WE NEED SOME         "
scrollColor($21)
.text "     RASTER BARS!"
scrollRaster()
.text " "
scrollSpeed($00)
.text "  "
scrollSpeed($02)
.text "  "
scrollSpeed($03)
.text "  "
scrollColor($04)
.text "  WE "
scrollColor($21)
.text " LOVE " 
scrollColor($04)
.text " OLD SKOOL EFFECTS... "
scrollSpeed($02)
.text " ...AND THAT IS WHAT I HAVE FOR YOU TODAY LADIES AND GENTLEMEN..."
scrollColor($02)
.text "  "
scrollSpeed($03)
.text " COME AND GATHER ROUND BOYS AND GIRLS WHILE I TELL YOU ABOUT PARTYSCROLLERS FROM THE OLD DAYS."
scrollColor($31)
.text "  "
scrollSpeed($02)
.text "...AND NOW FOR SOME MESSAGES FROM PARTYPEOPLE AT SYNTAX..."
scrollSpeed($01)
.text " ARE YOU READY? "
scrollColor($05)
.text "  "
scrollSpeed($02)
.text "EVILEYE I AM ON THE BIG SCREEN!! LUL"
scrollColor($07)
.text "  "
scrollSpeed($03)
.text "AZRYL SEZ LIFE SUX WHEN YOUR GIRLFRIEND DOESNT :)"
scrollColor($0a)
.text "  "
scrollSpeed($02)
.text "STYLE/CHROME HERE."
scrollSpeed($03)
.text " I DONT KNOW WHAT THIS DEMO IS, BUT IT MUST RULE BECAUSE SCROLLTEXTS. UPVOTE FOR SURE."
scrollColor($01)
.text "  (CENSORED)  "
scrollColor($0a)
.text "  PEACE OUT!"
scrollColor($0d)
.text "  "
scrollSpeed($03)
.text "VOLTAGE ON THE KEYS AT "
scrollColor($11)
.text "  SYNTAX 2018  "
 scrollColor($0d)
.text "  !!!!!!!  OHHHH YEAH...  SHOUTOUTS TO ALL THE RAD SCENERS OUT THERE.  BLERG.. I'M ALREADY HUNGOVER, BUT BACK ON THE BAD BOYS AGAIN. I'LL GROW UP LATER, MAYBE.  STAY FROSTY.  VOLT OUT."
scrollColor($21)
.text "  "
scrollSpeed($02)
.text "  RELOAD HERE, WE ARE BACK! DEMO OR DIE!  "
scrollColor($01)
.text "  "
scrollSpeed($03)
.text "  MIKNIK DOWN HERE IN VIC AGAIN, CHEERS EVERYONE!  "
scrollColor($0e)
.text "  "
scrollSpeed($02)
.text "  JAZZCAT HERE ON THE KEYS... "
scrollSpeed($03)
.text "SO, HERE WE ARE AT FUCKING SYNTAX MAN... OOOH YEAHHHH.. NOTHING BEATS A SWEET SCROLLER, THIS IS WHERE IT IS AT, THIS IS WHERE IT ALL BEGAN. ANYWAY, HANDING OVER TO SOMEONE ELSE SO I CAN DOWN SOME MORE BEERZ... SEE YOU NEXT TIME!  "
scrollColor($11)
.text "  "
scrollSpeed($02)
.text "  ZIG HERE... ITS BEEN DECADES SINCE I DID ANY SERIOUS C64 CODE.  CREDITS GO TO 4-MATT FOR THE GREAT TUNE."
scrollColor($01)
.text "  "
scrollSpeed($03)
.text "  I JUST LOVE THIS MACHINE AND THE DEMOSCENE CULTURE AROUND IT. AS CRAZY AS THIS WORLD BECOMES, 8 BITS MAKES IT FUN...  "
scrollColor($41)
.text "  "
scrollSpeed($01)
.text "  ...GREETINGS TO EVERYONE IN DEFAME, THE SYNTAX CREW, AND ALL YOU GREAT SYNTAX PARTY PEOPLE.  "
.byte $00

/*
These macros work with the scroller
*/
.macro scrollSpeed(speed){
    .byte %10100000 | speed
}

.macro scrollColor(color){
    .byte %11000000 
    .byte color
}

.macro scrollRaster(){
    .byte %10010000 
}




/*
ANIMATION FRAMES
We know we have a specific palette and dimensions for rotating 
generates $800 bytes of animation frames
*/
.pc = $9000 "Animation Frames"

.var frames = List()
.for(var i=0;i<(animationFrameCount+1);i++){
    .if(i<10){
    .eval frames.add(LoadPicture("work-files/rotatingbar_000"+i+".png"))  
    }
    else{
    .eval frames.add(LoadPicture("work-files/rotatingbar_00"+i+".png"))  
    }
}

.var frameLut1 = Hashtable()
.eval frameLut1.put($ff0000,blue)
.eval frameLut1.put($ffa0a0,lgrey)
.eval frameLut1.put($00ff00,lblue)
.eval frameLut1.put($b0ffb0,white)
.eval frameLut1.put($ffffff,black)

.align $100
ANIMATION_FRAMES:
.for(var i=0;i<frames.size();i++){
    .for(var j=0;j<animationFrameSize;j++){
        .eval var tmpColor = frames.get(i).getPixel(0,j)
        .print toHexString(tmpColor) + " = " + frameLut1.get(tmpColor)
        .byte frameLut1.get(tmpColor)
    }
}

//used to position rotating raster animation on the framebuffer y location 
.align $100
PHASE1_RASTER_ANIMATION_OFFSETS:
.fill $100, $c0
PHASE2_RASTER_ANIMATION_OFFSETS:
.fill $100, easeInOut(i, $c0,-$60, $100)
RASTER_ANIMATION_OFFSETS:
.fill $100, cosinus(i, $30, $30, $100)


//memory pointers to the animation frames to save ram
.align $100
ANIMATION_FRAMES_LUT:
.pc = * "Animation Frames LUT"
.for(var i=0;i<animationFrameCount+1;i++){
    .word ANIMATION_FRAMES + (animationFrameSize * i)
}
.byte $ff,$ff
//15 x 4 = 60 frame addresses - 4 because we use 4 color luts of the same frame sequence - giving us 60 frames
//60 frames x 2 bytes (lo/hi) = 120 bytes = $78
//.label animationFrameLutSize = ((frames.size() * 4)*2) //$78

.align $100
ANIMATION_SELECTOR_LUT:
.pc=* "ANIMATION SELECTOR LUT"
.fill $40,(easeInOut(i,0,animationFrameCount,$40)*2)
.fill $40,(easeInOut(i,animationFrameCount,-animationFrameCount,$40)*2)
.fill $40,(easeIn(i,animationFrameCount,-animationFrameCount,$40)*2)
.fill $40,(easeOut(i,animationFrameCount,-animationFrameCount,$40)*2)

/********************************************
DISPLAY BUFFERS
*********************************************/

.align $100
FRAMEBUFFER_CONTROL:
.byte $18, $18, $18, $18, $18, $18, $18, $18
.byte $18, $18, $18, $18, $18, $18, $18, $18
.byte $18, $18, $18, $18, $18, $18, $18, $18
.byte $18, $18, $18, $18, $18, $18, $18, $18
.byte $18, $18, $18, $18, $18, $18, $18, $18
.byte $18, $18, $18, $18, $18, $18, $18, $18
.byte $18, $18, $18, $18, $18, $18, $18, $18
.byte $18, $18, $18, $18, $18, $18, $18, $18
.byte $18, $18, $18, $18, $18, $18, $18, $18
.byte $18, $18, $18, $18, $18, $18, $18, $18
.byte $18, $18, $18, $18, $18, $18, $18, $18
.byte $18, $18, $18, $18, $18, $18, $18, $18
.byte $18, $18, $18, $18, $18, $18, $18, $18
.byte $18, $18, $18, $18, $18, $18, $18, $18
.byte $18, $18, $18, $18, $18, $18, $18, $18
.byte $18, $18, $18, $18, $18, $18, $18, $18
.byte $18, $18, $18, $18, $18, $18, $18, $18

.align $100
FRAMEBUFFER_COLORS:
.byte $04
.byte $00, $01, $00, $01, $00, $01, $00, $01 
.byte $00, $01, $00, $01, $00, $01, $00, $01 
.byte $00, $01, $00, $01, $00, $01, $00, $01 
.byte $00, $01, $00, $01, $00, $01, $00, $01 
.byte $00, $01, $00, $01, $00, $01, $00, $01 
.byte $00, $01, $00, $01, $00, $01, $00, $01 
.byte $00, $01, $00, $01, $00, $01, $00, $01 
.byte $00, $01, $00, $01, $00, $01, $04, $01 
.byte $00, $01, $00, $01, $00, $01, $00, $01 
.byte $00, $01, $00, $01, $00, $01, $00, $01 
.byte $00, $01, $00, $01, $00, $01, $00, $01 
.byte $00, $01, $00, $01, $00, $01, $00, $01 
.byte $00, $01, $00, $01, $00, $01, $00, $01 
.byte $00, $01, $00, $01, $00, $01, $00, $01 
.byte $00, $01, $00, $01, $00, $01, $00, $01 
.byte $00, $01, $00, $01, $00, $01, $00, $06 
FRAMEBUFFER_BORDERS:
.byte $06

.align $100
FRAMEBUFFER_BACKGROUND:
.fill $200,$00

 .pc=music.location "Music"
 .fill music.size, music.getData(i)

/********************************************
CALLING MACROS (to fixed address outputs)
*********************************************/

/*
startAdr = base address of the d011 frames. Each frame contains frameSize of $d011 values and (max) logoCharRows 
lutAdr = double-byte LUT address (lo/hi) of the frames
frameCount = total number of frames to render
frameSize = number of raster lines in each frame
maxSplitSize = total raster lines to use in FLD total 
*/
_equalCharPackSpecial("work-files/pinstripe-logo.gif",$4000,$2000)
_spriteFontReader("work-files/spritefont.gif",$3000,60)

/********************************************
MACROS
*********************************************/

.macro _spriteFontReader(filename, startAdr, charCount) {
    .var spriteData = List()
    .var pic = LoadPicture(filename)
	.for (var char=0; char<charCount; char++) {
	    .for (var row=0; row<21; row++) {
            .eval spriteData.add(pic.getSinglecolorByte((char * 3), row) ^ $ff)
            .eval spriteData.add(pic.getSinglecolorByte((char * 3)+1, row) ^ $ff)
            .eval spriteData.add(pic.getSinglecolorByte((char * 3)+2, row) ^ $ff)
        }
        .eval spriteData.add(0)
    }
	.pc = startAdr "sprite font"
	.fill spriteData.size(), spriteData.get(i)
}

.macro _equalCharPackSpecial(filename, screenAdr, charsetAdr) {
	.var charMap = Hashtable()
	.var charNo = 0
	.var screenData = List()
	.var charsetData = List()
	.var pic = LoadPicture(filename)

	// Graphics should fit in 8x8 Single collor / 4 x 8 Multi collor blocks
	.var PictureSizeX = pic.width/8
	.var PictureSizeY = pic.height/8

	.for (var charY=0; charY<PictureSizeY; charY++) {
		.for (var charX=0; charX<PictureSizeX; charX++) {
			.var currentCharBytes = List()
			.var key = ""
			.for (var i=0; i<8; i++) {
				.var byteVal = pic.getSinglecolorByte(charX, charY*8 + i) ^ $ff
				.eval key = key + toHexString(byteVal) + ","
				.eval currentCharBytes.add(byteVal)
			}
			.var currentChar = charMap.get(key)
			.if (currentChar == null) {
				.eval currentChar = charNo
				.eval charMap.put(key, charNo)
				.eval charNo++
				.for (var i=0; i<8; i++) {
					.eval charsetData.add(currentCharBytes.get(i))
				}
			}
			.eval screenData.add(currentChar)
		}
	}
	.pc = screenAdr "logomatrix"
    .for(var rows=0;rows<12;rows++){
    .align $100
        .for(var x=0;x<15;x++)
            .byte $ff

        .for(var x=0;x<40;x++)
            .byte screenData.get(x+(rows*40))

        .for(var x=0;x<15;x++)
            .byte $ff
    }
	.pc = charsetAdr "charset"
	.fill charsetData.size(), charsetData.get(i)

	.pc = charsetAdr + $800 "charset2"
    .for(var i=0;i<charsetData.size();i++){
        .if(mod(i,2)==0){
            .byte charsetData.get(i) & %10101010
        }
        else{
            .byte charsetData.get(i) & %01010101
        }
    }

    .pc = charsetAdr + $800 + $800 - $08 "blank filler char"
    .fill $08, $00
    .pc = charsetAdr + $800 + $800 - $10 "blank filler char"
    .fill $08, $ff
    .pc = charsetAdr + $800 - $08 "blank filler char"
    .fill $08, $00
    .pc = charsetAdr + $800 - $10 "blank filler char"
    .fill $08, $ff
 
    }

.macro _outputMusicInfo(){
    //----------------------------------------------------------
// Print the music info while assembling
.print ""
.print "SID Data"
.print "--------"
.print "location=$"+toHexString(music.location)
.print "init=$"+toHexString(music.init)
.print "play=$"+toHexString(music.play)
.print "songs="+music.songs
.print "startSong="+music.startSong
.print "size=$"+toHexString(music.size)
.print "name="+music.name
.print "author="+music.author
.print "copyright="+music.copyright
.print ""
.print "Additional tech data"
.print "--------------------"
.print "header="+music.header
.print "header version="+music.version
.print "flags="+toBinaryString(music.flags)
.print "speed="+toBinaryString(music.speed)
.print "startpage="+music.startpage
.print "pagelength="+music.pagelength
}



