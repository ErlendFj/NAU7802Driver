{{
 NAU7802 (amp and adc)driver (for strain gauge application (scale))
 Erlend Fj. 2020
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
 Supports register configuration and read-out of results - all mainstream chip functions. 
 
 Revisions:


-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
}}
{
 Acknowledgements: I have mainly built upon the work of Sparkfun - where I bought the chip break-out and strain gauge elements 

=======================================================================================================================================================================


                                                                                                    
                                                 GND           GND   VDDA             +3.3           
                                                  |             |      |   ----|-----|               
                                                  =   2x100n    =      |   |   |     |               
                                              ----|-------------|------|   |   |     |               
                           A+ ------47R ----- |                        |   |   |    2k2              
                                         |  | |   +----------------+   |   |  2k2    |               
                                    100n =  | |----REFP       AVDD ----|   |   |     |               
                                         |  |-----|VIN1N      DVDD ---------   |     |               
            WHEATSTONE     A- ------47R -|--------|VIN1P      SDIO ------------|-----|-- SDA         
              BRIDGE       B- ----|---------------|VIN2N      SCLK ------------|-------- SCL         
              INPUTS              | |-------------|VIN2P      DRDY ------INT             TO PROPELLER
                             100n = |      |------|VBG        XOUT |                                 
                                  | |      |   |--|REFN        XIN |                                 
                                  | | 100n =   |--|AVSS       DVSS --------|                         
                           B+ ----|--      |   |  +----------------+       |                         
                                           |   |        NAU7802            |                         
                                          GND GND                         GNDN
}


CON
        _clkmode = xtal1 + pll16x                                       'Standard clock mode * crystal frequency = 80 MHz
        _xinfreq = 5_000_000
        

          clk_freq = (_clkmode >> 6) * _xinfreq                         ' system freq as a constant
          mSec     = clk_freq / 1_000                                   ' ticks in 1ms
          uSec     = clk_freq / 1_000_000                               ' ticks in 1us

          POSXMS   = posx / mSec                                        ' millis in posx
          DELTATIX = posx + (posx // mSec) + 2           
          


                    'Register overview
                  
                     'NAME           ADDR            DESCRIPTION
                     '------------------------------------------------------------------------
                      PU_CTRL    =   $00          '  PU_CTRL AVDDS OSCS CR CS PUR PUA PUD RR $00      
                      CTRL1_CRP  =   $01          '  DRDY[6] VLDO[5:3] GAINS[2:0] 0$00                 
                      CTRL2_CHS  =   $02          '  CRS[1:0] CALS CALMOD[1:0] $00            
                      OCAL1_B2   =   $03          '  CH1 OFFSET Calibration[23:16] $00        
                      OCAL1_B1   =   $04          '  CH1 OFFSET Calibration[15:8] $00         
                      OCAL1_B0   =   $05          '  CH1 OFFSET Calibration[7:0] $00          
                      GCAL1_B3   =   $06          '  CH1 GAIN Calibration[31:24] $00          
                      GCAL1_B2   =   $07          '  CH1 GAIN Calibration[23:16] $80          
                      GCAL1_B1   =   $08          '  CH1 GAIN Calibration[15:8] $00           
                      GCAL1_B0   =   $09          '  CH1 GAIN Calibration[7:0] $00            
                      OCAL2_B2   =   $0A          '  CH2 OFFSET Calibration[23:16] $00        
                      OCAL2_B1   =   $0B          '  CH2 OFFSET Calibration[15:8] $00         
                      OCAL2_B0   =   $0C          '  CH2 OFFSET Calibration[7:0] $00          
                      GCAL2_B3   =   $0D          '  CH2 GAIN Calibration[31:24] $00          
                      GCAL2_B2   =   $0E          '  CH2 GAIN Calibration[23:16] $80          
                      GCAL2_B1   =   $0F          '  CH2 GAIN Calibration[15:8] $00           
                      GCAL2_B0   =   $10          '  CH2 GAIN Calibration[7:0] $00            
                      I2C_CTRL   =   $11          '  CRSD FDR SPE/WPD SI BOPGA TS / BGPCP $00 
                      ADCO_B2    =   $12          '  ADC_OUT[23:16] RO                         
                      ADCO_B1    =   $13          '  ADC_OUT[15:8] RO                          
                      ADCO_B0    =   $14          '  ADC_OUT[7:0] RO                           
                      OTP_B1     =   $15          '  OTP[15:8] RO                              
                      OTP_B0     =   $16          '  OTP[7:0] RO
                      
                      PGA_CTRL   =   $1B          '  Misc PGA control
                      PWR_CTRL   =   $1C          '  Misc power control
                      
                      DRCode     =   $1F          '  Device Revision Code RO


              'Bit mask values
              '--------------------------------------------------------------------------------
               bZerosPU_CTRL        =   %00000000     '  W For zeroing (the reset bit)
               bResetPU_CTRL        =   %00000001     '  W Reset all registers       
               bPWRdigPU_CTRL       =   %00000010     '  W Power up digital part            
               bPWRanaPU_CTRL       =   %00000100     '  W Power up analog part      
               bPWRrdyPU_CTRL       =   %00001000     '  R Power up is ready         
               bCYCLstPU_CTRL       =   %00010000     '  T Synch conversion          
               bCYCLryPU_CTRL       =   %00100000     '  R ADC cycle ready           
               bCLKselPU_CTRL       =   %01000000     '  W Select clock source                          
               bAVDDslPU_CTRL       =   %10000000     '  W Select AVDD source        

               bGAINselCTRL1       =   %00000111      '  W Gain= x1 ... x128                                                
               bVLDOselCTRL1       =   %00111000      '  W Volt= 2.4 ... 4.5  Note: can not be higher than supply voltage   
               bDRDYselCTRL1       =   %01000000      '  W Select between conversion ready or OSC output                    
               bCRPPselCTRL1       =   %10000000      '  W Conversion ready pin active low or active high                   
                                                                                                                            
               bCALmodeCTRL2       =   %00000011      '  W Selects what to calibrate                                        
               bCALtrigCTRL2       =   %00000100      '  T Trigger a calibration                                            
               bCALerrorCTRL2      =   %00001000      '  R Calibration has failed or not                                    
               bCONVrateCTRL2      =   %01110000      '  W Conversion rate 10SPS ... 320SPS                                 
               bACHselCTRL2        =   %10000000      '  W ADC channel 1 or 2 select                                        
             
               bBGPCPenI2C_CTRL     =   %00000001     '  W Disable bandgap chopper                           
               bTSpgaI2C_CTRL       =   %00000010     '  W Sets temperature as PGA input                     
               bBOPGAenI2C_CTRL     =   %00000100     '  W Enables PGA burnout                               
               bSIoffsI2C_CTRL      =   %00001000     '  W Short input and measure offset                    
               bWPUdisI2C_CTRL      =   %00010000     '  W Disable weak I2C pull-up                          
               bSPUenI2C_CTRL       =   %00100000     '  W Enable strong I2C pull-up                         
               bFRDenI2C_CTRL       =   %01000000     '  W Enable fast read                                  
               bCRSDenI2C_CTRL      =   %10000000     '  W Enable SDA low to signal conversion ready
               bCLK_CHP_off         =   %00110000     '  W disable clock chopping and delay
               bPGA_CHP_off         =   %00000001     '  W disable PGA chopper
               bPGA_CAP_en          =   %10000000     '  W enable capacitor across ch2
               bDRCode              =   %00001111     '  R Chip revision code
               

               pwrStatus          =   0
               pwrOnDig           =   1
               pwrOnAna           =   2
               pwrOnAll           =   3
               pwrOffDig          =   4
               pwrOffAna          =   5
               pwrOffAll          =   6
               
               #1, standard, fast, lowsens                ' configuration modes
               #1, offsInt, reserved, offsSys, gainSys    ' calibration modes
               #0, VINx, tSensor                          'PGA input selection

               ChipAddr = $2A
               
          'Used by Demo

          demoPINscl   = 1
          demoPINsda   = 2
          

          

VAR
          LONG signInt



  
OBJ
          i2c      : "i2cDriver"
          pst      : "Parallax Serial Terminal" 


  
PUB Main  | value, status, regByte                                                                'Demo code


   pst.Start(9600)                                                      'Remember to start the terminal at 9600b/s
   WAITCNT(3*clkfreq + cnt)
   pst.Chars(pst#NL, 2)
   pst.Chars(pst#NL, 2)   
   pst.Str(String("Test of NAU7802 - press Enter"))
   value := pst.DecIn
   pst.Chars(pst#NL, 2)

   Init(demoPINscl, demoPINsda)

   pst.Str(String("PWR_CTRL status: "))
   pst.Bin(i2c.ReadByteA8(ChipAddr, PU_CTRL), 8)
   pst.Chars(pst#NL, 2)

   pst.Str(String("Power status: "))
   pst.Bin(Power(pwrOnAll), 8)
   pst.Chars(pst#NL, 2)

   pst.Str(String("PWR_CTRL status: "))
   pst.Bin(i2c.ReadByteA8(ChipAddr, PU_CTRL), 8)
   pst.Chars(pst#NL, 2)
                                                                      
   pst.Str(String("Revision code: "))
   pst.Hex(RevCode, 4)
   pst.Chars(pst#NL, 2)

   pst.Str(String("Configure: "))
   pst.Bin(Configure(standard), 8)
   pst.Chars(pst#NL, 2)

   pst.Str(String("CTRL2_CHS: "))
   pst.Bin(i2c.ReadByteA8(ChipAddr, CTRL2_CHS), 8)
   pst.Chars(pst#NL, 2)

   pst.Str(String("Recalibrate offsSys: "))
   pst.Bin(Recal(offsSys), 8)
   pst.Chars(pst#NL, 2)
   pst.Str(String("Recalibrate offsInt: "))
   pst.Bin(Recal(offsInt), 8)
   pst.Chars(pst#NL, 2)
   pst.Str(String("Recalibrate gainSys: "))
   pst.Bin(Recal(gainSys), 8)
   pst.Chars(pst#NL, 2)         
   REPEAT
     pst.Str(String("ADC read: "))
     pst.Dec(Read(0)~>5 -4140)                                                                          'to remove 'noisy' bits and offset
     pst.Chars(pst#NL, 2)   
     WAITCNT(clkfreq + cnt) 


     
PUB Init(PINscl, PINsda)                                                            

   IFNOT i2c.IsInitialized                                                                              'set pin numbers to use
     i2c.Init(PINscl, PINsda)

   RETURN TRUE
     

PUB Power(cmd) | regByte                                                 

   CASE cmd
     pwrStatus:
        RETURN i2c.ReadByteA8(ChipAddr, PU_CTRL) & bPWRrdyPU_CTRL                                       'return power ready status - will not work unless the digital is powered
        
     pwrOnDig:
        i2c.WriteByteA8(ChipAddr, PU_CTRL, bResetPU_CTRL)                                               'first of all reset all registers
        i2c.WriteByteA8(ChipAddr, PU_CTRL, bZerosPU_CTRL)                                               'then un-reset
        i2c.WriteByteA8(ChipAddr, PU_CTRL, bPWRdigPU_CTRL)                                              'write the power-up-digital bit first, so the chip can start responding)
        WAITCNT(clkfreq/1000 + cnt)
        
     pwrOnAna:                                                      
        i2c.WriteByteA8(ChipAddr, PU_CTRL, regByte | bPWRanaPU_CTRL)                                    'power up analog part - will not work unless the digital is powered

     pwrOnAll:
        i2c.WriteByteA8(ChipAddr, PU_CTRL, bResetPU_CTRL)                                               'first of all reset all registers
        i2c.WriteByteA8(ChipAddr, PU_CTRL, bZerosPU_CTRL)                                               'then un-reset                   
        i2c.WriteByteA8(ChipAddr, PU_CTRL, bPWRdigPU_CTRL)                                              'write the power-up-digital bit first, so the chip can start responding
        WAITCNT(clkfreq/1000 + cnt)                                                                                                                   
                                                                                                                                                      
        i2c.WriteByteA8(ChipAddr, PU_CTRL, i2c.ReadByteA8(ChipAddr, PU_CTRL) | bPWRanaPU_CTRL)          'then power up the analog part                                         
        WAITCNT(clkfreq/1000 + cnt)                                                                                                                   
        
     pwrOffDig:
        i2c.WriteByteA8(ChipAddr, PU_CTRL, i2c.ReadByteA8(ChipAddr, PU_CTRL) & !bPWRdigPU_CTRL)         'power down digital part

     pwrOffAna:
        i2c.WriteByteA8(ChipAddr, PU_CTRL, i2c.ReadByteA8(ChipAddr, PU_CTRL) & !bPWRanaPU_CTRL)         'power down analog part

     pwrOffAll:   
        i2c.WriteByteA8(ChipAddr, PU_CTRL, i2c.ReadByteA8(ChipAddr, PU_CTRL) & !(bPWRdigPU_CTRL & bPWRanaPU_CTRL))      'power down both parts - entger low power mode
        
     OTHER: RETURN FALSE   
        
   RETURN i2c.ReadByteA8(ChipAddr, PU_CTRL) & bPWRrdyPU_CTRL > 0                                        'return power ready status     


   

PUB Configure(mode) | regbyte                                              

   CASE mode   
     standard:
       LDO(33)
       Gain(128)
       SPS(10)
       Channel(0)       

     fast:
       LDO(33)
       Gain(128)
       SPS(320)
       Channel(0)
     
     lowsens:
       LDO(33)
       Gain(1)
       SPS(10)
       Channel(0)
     
     OTHER:                                                             
       RETURN FALSE
                                                                                                        
   i2c.WriteByteA8(ChipAddr, OTP_B1, bCLK_CHP_off)                                                      'disable clock chopping etc.       
   regByte:= i2c.ReadByteA8(ChipAddr, PWR_CTRL)
   i2c.WriteByteA8(ChipAddr, PWR_CTRL, regByte | bPGA_CAP_en)                                           'enable capacitor across ch2
   i2c.WriteByteA8(ChipAddr, PWR_CTRL, regByte | bCYCLstPU_CTRL)                                        'trigger conversion
   RETURN Recal(offsInt)
       
       
PUB LDO(voltSel) | bVolt, regbyte                                                                     

   bVolt := LOOKDOWN(voltSel : 45, 42, 39, 36, 33, 30, 27, 24)                                          'LDO voltage selected - in 10x volt
   IF bVolt == 0                                                                                        'if not found, do not write to register
     RETURN FALSE
   bVolt -= 1                                                                                           '-1 because I used lookdown instead of lookdownz
   regByte:= i2c.ReadByteA8(ChipAddr, CTRL1_CRP) & !bVLDOselCTRL1                                       'blank out the old LDO bits
   i2c.WriteByteA8(ChipAddr, CTRL1_CRP, regByte | bVolt<< 3)                                            'write byte with new middle 3 bits representing LDO voltage                                                                        
   RETURN bVolt
      

PUB Gain(gainSel) | bGain, regbyte                                                              

   bGain := LOOKDOWN(gainSel : 1, 2, 4, 8, 16, 32, 64, 128)                                             'convert from desired gain dB to bit numbers to set
   IF bGain == 0                                                                                        'if not found, do not write to register
     RETURN FALSE
   bGain -= 1                                                                                           '-1 because I used lookdown instead of lookdownz
   regByte:= i2c.ReadByteA8(ChipAddr, CTRL1_CRP) & !bGAINselCTRL1                                       'blank out the old Gain bits
   i2c.WriteByteA8(ChipAddr, CTRL1_CRP, regByte | bGain)                                                'write byte with new lower 3 bits representing Gain                                                                      
   RETURN bGain                                                   

   
PUB SPS(SPSsel) | bSPS, regbyte                                                   

   bSPS := LOOKDOWN(SPSsel : 10, 20, 40, 80, -1, -1, -1, 320)                                          'index 5,6,7 not valid
   IF bSPS == 0                                                                                        'if not found, do not write to register
     RETURN FALSE
   bSPS -= 1
   regByte:= i2c.ReadByteA8(ChipAddr, CTRL2_CHS) & !bCONVrateCTRL2                                     'blank out the old SPS bits
   i2c.WriteByteA8(ChipAddr, CTRL2_CHS, regByte | bSPS<< 4)                                            'write byte with new middle 3 bits representing SPS                                                                     
   RETURN bSPS
   

PUB Recal(mode) | regbyte, bCal

   bCal := LOOKDOWN(mode : offsInt, reserved, offsSys, gainSys)                
   IF bCal == 0                                                                                        'if not found, do not write to register
     RETURN FALSE
   bCal-= 1          
   regByte:= i2c.ReadByteA8(ChipAddr, CTRL2_CHS) & !bCALmodeCTRL2              
   i2c.WriteByteA8(ChipAddr, CTRL2_CHS, regByte | bCal)                                                'set calibration mode   
   i2c.WriteByteA8(ChipAddr, CTRL2_CHS, regByte | bCALtrigCTRL2)                                       'set calibrate trigger bit
   WAITCNT(clkfreq + cnt)                                                                              'calibration takes time
   RETURN i2c.ReadByteA8(ChipAddr, CTRL2_CHS) &  bCALerrorCTRL2                                        'check if calibration has failed

     
PUB Channel(ch) | regbyte                                                           

   regByte:= i2c.ReadByteA8(ChipAddr, CTRL2_CHS) & !bACHselCTRL2
   CASE ch
       1: i2c.WriteByteA8(ChipAddr, CTRL2_CHS, regByte | bACHselCTRL2)
       0: i2c.WriteByteA8(ChipAddr, CTRL2_CHS, regByte)
       OTHER: RETURN FALSE                                                      
   RETURN i2c.ReadByteA8(ChipAddr, CTRL2_CHS) & !bACHselCTRL2                                                
   

PUB Read(condition)                                      

   CASE condition
      0: RETURN (i2c.ReadTripleA8(ChipAddr, ADCO_B2)<< 8)~> 8                                          'no condition / direct read 3 bytes, 24 bit sign extension
      1: REPEAT 200                                                                                    'avoid infinite loop                       
           IF IsConvReady                                                                              'condition on conversion finished 
             RETURN (i2c.ReadTripleA8(ChipAddr, ADCO_B2)<< 8)~> 8                                      'read 3 bytes, 24 bit sign extension
           WAITCNT(clkfreq/1000 + cnt)                                                                 'avoid jamming down the bus                                                    
         RETURN FALSE                    
      OTHER: RETURN FALSE


PUB PGAin(input)| regByte, bInp

    bInp := LOOKDOWN(input : VINx, tSensor) 
   IF bInp == 0                                                                                        'if not found, do not write to register
     RETURN FALSE
   bInp -= 1
   regByte:= i2c.ReadByteA8(ChipAddr, I2C_CTRL) & !bTSpgaI2C_CTRL                                      'if temperature sensor is selected, first do Gain(1), 
   i2c.WriteByteA8(ChipAddr, I2C_CTRL, regByte | bInp<< 1)                                             'then after VINx is agin selected, reset (to 128)         
                                                                            
   RETURN Recal(bInp)                                           
   

PUB IsConvReady

   RETURN i2c.ReadByteA8(ChipAddr, PU_CTRL) &  bCYCLryPU_CTRL > 0                                      'check if calibration has failed
   

PUB IsCalibrated

   RETURN i2c.ReadByteA8(ChipAddr, CTRL2_CHS) &  bCALerrorCTRL2 > 0                                    'check if calibration has failed
   


PUB RevCode

   RETURN i2c.ReadByteA8(ChipAddr, DRCode) & bDRCode                                                   'read chip revison code

   
PUB SetBits(register, bmask, bitval, shiftl) | regByte                                                 'General method for setting bits in a register

   regByte:= i2c.ReadByteA8(ChipAddr, register) & !bmask                                               'blank out the old bits
   i2c.WriteByteA8(ChipAddr, register, regByte | bitval<< shiftl)                                      'write byte with new bits 


DAT

{{

  Terms of Use: MIT License

  Permission is hereby granted, free of charge, to any person obtaining a copy of this
  software and associated documentation files (the "Software"), to deal in the Software
  without restriction, including without limitation the rights to use, copy, modIFy,
  merge, PUBlish, distribute, sublicense, and/or sell copies of the Software, and to
  permit persons to whom the Software is furnished to do so, subject to the following
  conditions:

  The above copyright notice and this permission notice shall be included in all copies
  or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
  INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
  PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
  CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
  OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

}}