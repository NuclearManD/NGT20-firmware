''***************************************
''*  VGA Terminal 40x15 v1.0            *
''*  Author: Chip Gracey                *
''*  Copyright (c) 2006 Parallax, Inc.  *
''*  See end of file for terms of use.  *
''***************************************

CON

  _clkmode = xtal1+pll16x
  _clkfreq = 80_000_000

  vga_params = 21   
  x_tiles = 18
  y_tiles = 16

  x_screen = x_tiles << 4
  y_screen = y_tiles << 4
   
  width = 0             '0 = minimum
  x_scale = 1           '1 = minimum
  y_scale = 1           '1 = minimum
  x_spacing = 6         '6 = normal
  y_spacing = 13        '13 = normal

  x_chr = x_scale * x_spacing
  y_chr = y_scale * y_spacing

  y_offset = y_spacing / 6 + y_chr - 1

  x_limit = x_screen / (x_scale * x_spacing)
  y_limit = y_screen / (y_scale * y_spacing)
  y_max = y_limit - 1

  y_screen_bytes = y_screen << 2
  y_scroll = y_chr << 2
  y_scroll_longs = y_chr * y_max
  y_clear = y_scroll_longs << 2
  y_clear_longs = y_screen - y_scroll_longs    
  paramcount = 14

  _memlen = 3
  _vbytes = 1024*_memlen

  fov   = 60
  y_fov = 60           

VAR
          
  byte strptr1[256]     
  byte VRAM[_vbytes]  
  long  x, y, bitmap_base 
  long  coords[128]  ' calculation cog sends back coords into this register
  long  iostat       ' calculation status register
  word  iowrite[64]  ' send addresses to calculation cog in this register

  
OBJ                                 
                        
  pst   : "Parallax Serial Terminal"
                        
  dbg   : "Parallax Serial Terminal"

PUB boot |in, xa, xb, ya, yb, i, c, dx, dy , tmp, red, grn, blu, mask, ext_ptr   , time, za, zb

  ' put status LED on output mode
  dira[7]~~

  ' start remote interface
  pst.startrxtx(1,0,0,115200)
  dbg.start(115200)
  ' tell the remote that we have initialized       
  pst.char(48)
              
  repeat                  
    in:=pst.charin
    dbg.str(string("vga."))
    !outa[7]          
    case in
      32:
        pst.str(string("NGT30",13))
        dbg.str(string("get_device_id();",$D)) 
      48:
        dbg.str(string("reboot();",$D))
        reboot
      49:
        dbg.str(string("clear();",$D))                  
      50:
        x:=pst.charin
        y:=pst.charin+1     
      51:
        pst.strin(@strptr1)',255)
        dbg.str(string("print(",34))
        dbg.str(@strptr1)
        dbg.str(string(34,");",$D))
        'str(@strptr1)   
      52:                 
        xa:=(pst.charin<<8)|pst.charin
        ya:=y_screen-(pst.charin<<8)|pst.charin
        xb:=(pst.charin<<8)|pst.charin
        yb:=y_screen-(pst.charin<<8)|pst.charin      
        'gr.plot(xa,ya)
        'gr.line(xb,yb)      
      53:
        'out(0)
        in:=pst.charin     
        'gr.color(in&3)                         
        repeat x_screen*y_screen
          'out(32)   
          x :=0
          y:=1     
      54:                    
        xa:=(pst.charin<<8)|pst.charin
        yb:=y_screen-(pst.charin<<8)|pst.charin  
        xb:=(pst.charin<<8)|pst.charin
        ya:=y_screen-(pst.charin<<8)|pst.charin  
        'gr.box(xa, ya, xb-xa, yb-ya)     
      55:      
        xa:=(pst.charin<<8)|pst.charin
        ya:=y_screen-(pst.charin<<8)|pst.charin-13  
        pst.strin(@strptr1+ext_ptr)',255)
        'gr.text(xa, ya, @strptr1+ext_ptr)
        ext_ptr:=256-ext_ptr   
      56:      
        xa:=(pst.charin<<8)|pst.charin
        yb:=y_screen-(pst.charin<<8)|pst.charin
        'gr.plot(xa,ya)                                                                  
      57:
        in:=pst.charin                             
        'gr.color(in&3)
      59:
        in:=pst.charin                             
        'gr.textmode(in,in,6,8)
      60:
        'gr.finish   
      61:
        in:=pst.charin
        mask:=(8*(in>>6))
        'tmp:=colors[in&63]&!($FF<<mask)
        xb:=pst.charin
        xa:=tmp|(xb<<mask)                             
        'colors[in&63]:=xa                                     
      62:                
        xa:=((pst.charin<<8)|pst.charin+ y_screen-(pst.charin<<8)|pst.charin+ pst.charin+(pst.charin<<8)|pst.charin+@VRAM)
      63:
        xa:=(pst.charin<<8)|pst.charin
        
        dbg.str(string("store_vram("))
        dbg.dec(xa)
        dbg.char(",")
        dbg.dec(pst.charin)
        dbg.str(string(");",$D))
      64:              
        pst.char(x_tiles)   
        pst.char(y_tiles)
        pst.char("S")
        dbg.str(string("get_screen_size();",$D))
      65:
        'gr.vec((pst.charin<<8)|pst.charin, y_screen-(pst.charin<<8)|pst.charin, (pst.charin<<8)|pst.charin, pst.charin<<5,(pst.charin<<8)|pst.charin+@VRAM) 
      66:
        'xa:=(pst.charin<<8)|pst.charin
        'if xa<x_tiles*y_tiles    
        '  screen[xa]:=(screen[xa]&$3FF)|((pst.charin&63)<<10)  
      67:      
        'gr.line((pst.charin<<8)|pst.charin,y_screen-(pst.charin<<8)|pst.charin)
      100:         
        dbg.str(string("render_3d();",$D))   
      101:
        pst.char(_memlen) ' 3 kilobytes
        pst.char(00)
        dbg.str(string("get_mem_len();",$D))
      102:
        dbg.str(string("translatef("))
        dbg.dec(getLong)
        dbg.char(",")
        dbg.dec(getLong)
        dbg.char(",")
        dbg.dec(getLong)
        dbg.str(string(");",$D))        
      103:            
        dbg.str(string("rotatef("))
        dbg.dec((pst.charin<<8)|pst.charin)
        dbg.char(",")
        dbg.dec((pst.charin<<8)|pst.charin)
        dbg.str(string(");",$D))
      104:    
        dbg.str(string("set_line("))
        dbg.dec(pst.charin)
        dbg.char(",")
        dbg.dec((pst.charin<<8)|pst.charin)
        dbg.str(string(");",$D))   
      105:                                         
        dbg.str(string("del_line("))
        dbg.dec(pst.charin)
        dbg.str(string(");",$D))
      106:                                         
        dbg.str(string("3d_frame();",$D))
        repeat tmp from 0 to 32       
          if !(iowrite[tmp]==0 )
            dbg.str(string("LINE: ("))
            dbg.dec(coords[tmp*4+2])
            dbg.char(",")
            dbg.dec(coords[tmp*4+3])
            dbg.str(string(") TO ("))
            dbg.dec(coords[tmp*4])
            dbg.char(",")
            dbg.dec(coords[tmp*4+1])
            dbg.str(string(");",$D))   
      $D:
        'newline
    waitcnt(cnt+2_400_000)
    pst.char(48)
    outa[7]~
pub getLong
  return (pst.charin<<24)|(pst.charin<<16)|(pst.charin<<8)|pst.charin  
PUb sin(degree, range) : s | c,z,angle
  angle := (degree*91)~>2  ' *22.75
  c := angle & $800
  z := angle & $1000
  if c
    angle := -angle
  angle |= $E000>>1
  angle <<= 1                    ' All dead history now....................................................................................................................................................................................................................................  And Forever.
  s := word[angle]
  if z
    s := -s
  return (s*range)~>16     ' return sin:=-range..+range    
PUB cos(degree,range)
  return sin(degree+90,range)
pub tan(degree,range)
  return sin(degree,range*range)/cos(degree,range)  
dat                           
                        org 0  
renderer
                        mov     q1, par    
                        rdword  ctrl, q1   ' control register
                        add     q1, #2
                        rdword  iord, q1   ' pasm in main out    
                        add     q1, #2
                        rdword  iowr, q1   ' coords for 2d renderer  
                        add     q1, #2
                        rdword  x_con, q1  ' x constant  
                        add     q1, #2                   
                        rdword  y_con, q1  ' y constant           
                        add     q1, #2   
                        rdword  camarg, q1 ' camera variables
                        add     q1, #2   
                        rdword  vmem, q1   ' video memory pointer

                        mov     q1, par
                        xor     q2, q2     ' trick from z80 asm
                        
                        wrword  q2, q1
                        add     q1, #2   
                        wrword  q2, q1  
                        add     q1, #2   
                        wrword  q2, q1  
                        add     q1, #2    
                        wrword  q2, q1
                        add     q1, #2    
                        wrword  q2, q1      
                        add     q1, #2     
                        wrword  q2, q1
                        add     q1, #2   
                        wrword  q2, q1 
                                            
    main3dloop                                 
                        ' wait for start instruction
                        rdlong  q1, ctrl
                        sub     q1, #255 wz
              if_nz     jmp     #main3dloop
    
                        ' Get temporary trig constants for camera rotation
                        mov     q5, camarg
                        add     q5, #12    ' yaw
                        rdlong  q6, q5
                        mov     q1, q6
                        call    #sin_
                        mov s1, q1
                                                 
                        mov     q1, q6
                        call    #cos_
                        mov c1, q1
                        
                        add     q5, #4   ' @yaw+4=@pitch
                        rdlong  q6, q5 
                        mov     q1, q6
                        call    #sin_
                        mov     s2, q1
                                                   
                        mov     q1, q6
                        call    #cos_ 
                        mov     c2, q1
                        
                        ' sync now to save 8+ cycles
                        mov q2, camarg 
                        rdlong  camx, q2
                        add q2, #4
                                      
                        
                        mov     index, #0   
                        rdlong  camy, q2
                        add     q2, #4
                        rdlong  camz, q2
                        
:subloop
                        mov     q1, index       ' tell the main cog how many points have been computed       
                        wrlong  q1, ctrl
                        
                        cmp     index, #32 wz
                   if_z jmp     #:done
                        mov     q1, index     ' this code gets the RAM address of each line
                        shl     q1, #1
                        add     q1, iord
                        rdword  q1, q1
                        cmp     q1, #0 wz
                   if_z add     index, #1       ' find first one that isn't zero
                   if_z jmp     #:subloop
                        and     q1, ram_mask
                        add     q1, vmem

                        ' q1 now has pointer to 3d coordinates   
                        
                        mov     qx, camx
                        rdlong  q3, q1
                        sub     qx, q3

                        add     q1, #4  ' next value
                                    
                        mov     qy, camy  
                        rdlong  q3, q1
                        sub     qy, q3

                        add     q1, #4  ' next value
                                                   
                        mov     qz, camz
                        rdlong  q3, q1
                        sub     qz, q3

                        mov     save, q1
                        add     save, #4
                        
                        call    #convert    ' convert first set of points
                        mov     px_, dx_      ' and load them into other registers
                        mov     py_, dy_
                      
                        mov     qx, camx
                        rdlong  q1, save
                        sub     qx, q1
                                    
                        add     save, #4
                                   
                        mov     qy, camy
                        rdlong  q1, save
                        sub     qy, q1
                                
                        add     save, #4
                                       
                        mov     qz, camz 
                        rdlong  q1, save
                        sub     qz, q1
                        
                        call    #convert    ' convert second set of points
                        
                        ' write mem

                        mov     q1, index
                        shl     q1, #4
                        add     q1, iowr
                        wrlong  px_,q1
                        add     q1, #4
                        wrlong  py_,q1
                        add     q1, #4
                        wrlong  dx_,q1
                        add     q1, #4
                        wrlong  dy_,q1
                        
                        add     index, #1
                        jmp     #:subloop          
                        
:done              
                        jmp     #main3dloop
'
' Multiply
'
'   in:         q1 = 16-bit multiplicand (q1[31..16] must be 0)
'               q2 = 16-bit multiplier
'
'   out:        q1 = 32-bit product
'                    
multiply_       ' setup
                                       
                        mov     q4, q1
                        xor     q4, q2
                        abs     q1, q1
                        abs     q2, q2

                        mov       vRes, #0      ' Primary accumulator (and final result)   
                        mov       tmp1, q1      ' Both my secondary accumulator,
                        shl       tmp1, #16     ' and the lower 16 bits of v1.    
                        mov       tmp2, q2      ' This is the upper 16 bits of v2,
                        shr       tmp2, #16     ' which will sum into my 2nd accumulator.
                        mov       temp, #16        ' Instead of 4 instructions 32x, do 6 instructions 16x.          
:loop                   ' v1_hi_lo * v2_lo
                        shr       q2, #1 wc     ' get the low bit of v2          
                  if_c  add       vRes, q1      ' (conditionally) sum v1 into my 1st accumulator
                        shl       q1, #1        ' bit align v1 for the next pass 
                        ' v1_lo * v2_hi
                        shl       tmp1, #1 wc   ' get the high bit of v1_lo, *AND* shift my 2nd accumulator
                  if_c  add       tmp1, tmp2    ' (conditionally) add v2_hi into the 2nd accumulator
                        ' repeat 16x
                        djnz      temp, #:loop     ' I can't think of a way to early exit this
                        ' finalize
                        shl       tmp1, #16     ' align my 2nd accumulator
                        add       vRes, tmp1    ' and add its contribution
                        mov       q1, vRes
                       
                        shl     q4,#1           wc
              if_c      neg     q1, q1
                     
multiply__ret             ret  

divide                  
                        mov     q4, q1
                        xor     q4, q2
                        abs     q1, q1
                        abs     q2, q2
                        
                        mov     temp,#32                ' Divide a 32 bit unsigned dividend
                        mov     remainder,#0            '  by a 32 bit unsigned divisor to
                        
:loop                   shl     q1,#1        wc   '  get a 32 bit unsigned quotient
                        rcl     remainder,#1            '  and a 32 bit unsigned remainder
                        cmpsub  remainder,q2  wc,wr
                        rcl     q3,#1
                        djnz    temp,#:loop

                        mov q1, q3
                                     
                        shl     q4,#1       wc
              if_c      neg     q1, q1
                        
divide_ret              ret
temp                    long    0   
remainder               long    0   


cos_
                        add     q1, #90
sin_
                        mov     q2, #91
                        call    #multiply_
                        sar     q1, #2        
                        test    q1, sin_90 wc           'get quadrant 2|4 into c
                        test    q1, sin_180 wz          'get quadrant 3|4 into nz
                        negc    q1, q1                  'if quadrant 2|4, negate offset
                        or      q1,sin_table            'or in sin table address >> 1
                        shl     q1,#1                   'shift left to get final word address
                        rdword  q1,q1                   'read word sample from $E000 to $F000
                        negnz   q1,q1                   'if quadrant 3|4, negate sample
                        shl     q1, precision_pasm  
                        sar     q1, #16
cos__ret
sin__ret
                        ret       
convert                                
                        mov     q6, qz
                        ' start by recalculating qz because it relies on unmodified qx          
                        mov     q1, qx
                        mov     q2, s1
                        call    #multiply_
                        mov     q5, q1
                        mov     q1, qz
                        mov     q2, c1
                        call    #multiply_
                        add     q5, q1
                        mov     qz, q5
                        sar     qz, precision_pasm                                         

                        ' qx next for calculating qy
                        
                        mov     q1, qx
                        mov     q2, c1
                        call    #multiply_
                        mov     q5, q1
                        mov     q1, q6
                        mov     q2, s1
                        call    #multiply_
                        sub     q5, q1
                        mov     qx, q5
                        sar     qx, precision_pasm

                        ' finish with qy
                        
                        mov     q1, qx
                        mov     q2, s2
                        call    #multiply_
                        mov     q5, q1
                        mov     q1, qy
                        mov     q2, c2
                        call    #multiply_
                        add     q5, q1
                        mov     qy, q5
                        sar     qy, precision_pasm
                        
                        ' now we need to translate the coordinates
                        
                        mov     q1, x_con
                        mov     q2, qz
                        call    #multiply_
                        mov     q2, qx
                        call    #divide
                        mov     dx_, q1
                        
                        ' and the y
                         
                        mov     q1, y_con
                        mov     q2, qy
                        call    #multiply_
                        mov     q2, qx
                        call    #divide
                        mov     dy_, q1

                        
                        add     dx_, x_prod
                        add     dy_, y_prod
convert_ret                        
                        ret

                        
                        
 
sin_90                  long    $0800                   '90 degree bit
sin_180                 long    $1000                   '180 degree bit
sin_table               long    $E000 >> 1              'sine table address shifted right
ram_mask                long    $7FFF                  
precision_pasm          long    7
                         
x_prod                  long    x_screen>>1
y_prod                  long    y_screen>>1        

  _color long 2
  _plot  long 4
  _line  long 5                 
                        
q1                      res     1       'temps
q2                      res     1
q3                      res     1  
q4                      res     1   
q5                      res     1 
q6                      res     1


tmp1                    res     1   
tmp2                    res     1 
vRes                    res     1

camx                    res     1
camy                    res     1
camz                    res     1

index                   res     1


qx                      res     1
qy                      res     1
qz                      res     1

s1                      res     1
c1                      res     1
s2                      res     1
c2                      res     1

dx_                     res     1       'line/plot coordinates
dy_                     res     1
px_                     res     1
py_                     res     1
sx_                     res     1
sy_                     res     1
                                 
ctrl                    res     1
iord                    res     1
iowr                    res     1  
num_pts                 res     1
x1s                     res     1
y1s                     res     1
z1s                     res     1
x2s                     res     1
y2s                     res     1
z2s                     res     1
camarg                  res     1 ' x,y,z,yaw,pitch 
x_con                   res     1
y_con                   res     1
gfxcmdreg               res     1 
vmem                    res     1
                                     
points                  res     1

save                    res     1
                                 

                        fit                         
           
{{
================================================================================================================================
=                                                   TERMS OF USE: MIT License                                                  =                                                            
================================================================================================================================
=Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation    = 
=files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy,    =
=modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software=
=is furnished to do so, subject to the following conditions:                                                                   =
=                                                                                                                              =
=The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.=
=                                                                                                                              =
=THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE          =
=WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR         =
=COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,   =
=ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                         =
================================================================================================================================
}}                         