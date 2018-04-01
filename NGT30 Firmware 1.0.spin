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

  long  vga_status      'status: off/visible/invisible  read-only       (21 contiguous longs)
  long  vga_enable      'enable: off/on                 write-only
  long  vga_pins        'pins: byte(2),topbit(3)        write-only
  long  vga_mode        'mode: interlace,hpol,vpol      write-only
  long  vga_videobase   'video base @word               write-only
  long  vga_colorbase   'color base @long               write-only              
  long  vga_hc          'horizontal cells               write-only
  long  vga_vc          'vertical cells                 write-only
  long  vga_hx          'horizontal cell expansion      write-only
  long  vga_vx          'vertical cell expansion        write-only
  long  vga_ho          'horizontal offset              write-only
  long  vga_vo          'vertical offset                write-only
  long  vga_hd          'horizontal display pixels      write-only
  long  vga_hf          'horizontal front-porch pixels  write-only
  long  vga_hs          'horizontal sync pixels         write-only
  long  vga_hb          'horizontal back-porch pixels   write-only
  long  vga_vd          'vertical display lines         write-only
  long  vga_vf          'vertical front-porch lines     write-only
  long  vga_vs          'vertical sync lines            write-only
  long  vga_vb          'vertical back-porch lines      write-only
  long  vga_rate        'pixel rate (Hz)                write-only   
                                  
  long  colors[64]    
  byte strptr1[256]     
  byte VRAM[_vbytes]  
  long  x, y, bitmap_base   
  word  screen[x_tiles * y_tiles]                                                             
  long  bitmap[x_tiles * y_tiles << 4 + 16]     'add 16 longs to allow for 64-byte alignment

  
  word  coords[128]  ' calculation cog sends back coords into this register
  long  iostat       ' calculation status register
  word  iowrite[64]  ' send addresses to calculation cog in this register

  
  long camera_x, camera_y, camera_z 
              
  long camera_yaw, camera_pitch
  
OBJ

  vga   : "VGA"     
  gr    : "graphics_a"
  pst   : "Parallax Serial Terminal"

PUB boot |mode,in, xa, xb, ya, yb, i, c, dx, dy , tmp, red, grn, blu, mask, ext_ptr   , time, za, zb

  ' put status LED on output mode
  dira[7]~~

  ' start remote interface
  pst.startrxtx(1,0,0,115200)

  ' begin timer for one second GPU bootup
  time:=clkfreq+cnt
  
  'start vga
  bitmap_base := (@bitmap + $3F) & $7FC0
  repeat x from 0 to x_tiles - 1
    repeat y from 0 to y_tiles - 1
      screen[y * x_tiles + x] := bitmap_base >> 6 + y + x * y_tiles
  longmove(@vga_status, @vgaparams, vga_params)
  vga_videobase := @screen
  vga_colorbase := @colors
  vga.start(@vga_status)    
  ext_ptr:=0
  
  'init colors
  repeat i from 0 to 63
    colors[i] := %%010_0_100_0_333_0_000_0
     
  'init tile screen
  i:=0
  repeat x from 0 to vga_hc - 1
    repeat y from 0 to vga_vc - 1
      screen[x + y * vga_hc] := (i/5) << 10 + bitmap_base >> 6 + x * vga_vc + y
      i:=i+1

      
  'start and setup graphics
  gr.start
  gr.setup(x_tiles, y_tiles, 0, 0, bitmap_base)       
  gr.color(1)
  out(0)

  ' start calculation cog              
  iowrite[0]:=@iostat
  iowrite[1]:=@iowrite
  iowrite[2]:=@coords
  iowrite[3]:=x_screen*91/tan(fov/2,91*2) 
  iowrite[4]:=y_screen*91/tan(y_fov/2,91*2)  
  iowrite[5]:=@camera_x
  iowrite[6]:=@vram         
  cognew(@renderer, @iowrite)

  ' wait for setup completion
  waitcnt(time)

  ' tell the remote that we have initialized       
  pst.char(48)
     
  mode:=1   
  repeat                  
    in:=pst.charin
    !outa[7]          
    case in
      32:
        pst.str(string("NGT30",13))  
      48:
        reboot
      49:  
        out(0)                  
      50:
        x:=pst.charin
        y:=pst.charin+1     
      51:
        pst.strin(@strptr1)',255)
        str(@strptr1)   
      52:                 
        xa:=(pst.charin<<8)|pst.charin
        ya:=y_screen-(pst.charin<<8)|pst.charin
        xb:=(pst.charin<<8)|pst.charin
        yb:=y_screen-(pst.charin<<8)|pst.charin      
        gr.plot(xa,ya)
        gr.line(xb,yb)      
      53:
        out(0)
        in:=pst.charin     
        gr.color(in&3)                         
        repeat x_screen*y_screen
          out(32)   
          x :=0
          y:=1     
      54:                    
        xa:=(pst.charin<<8)|pst.charin
        yb:=y_screen-(pst.charin<<8)|pst.charin  
        xb:=(pst.charin<<8)|pst.charin
        ya:=y_screen-(pst.charin<<8)|pst.charin  
        gr.box(xa, ya, xb-xa, yb-ya)     
      55:      
        xa:=(pst.charin<<8)|pst.charin
        ya:=y_screen-(pst.charin<<8)|pst.charin-13  
        pst.strin(@strptr1+ext_ptr)',255)
        gr.text(xa, ya, @strptr1+ext_ptr)
        ext_ptr:=256-ext_ptr   
      56:      
        xa:=(pst.charin<<8)|pst.charin
        yb:=y_screen-(pst.charin<<8)|pst.charin
        gr.plot(xa,ya)                                                                  
      57:
        in:=pst.charin                             
        gr.color(in&3)
      59:
        in:=pst.charin                             
        gr.textmode(in,in,6,8)
      60:
        gr.finish   
      61:
        in:=pst.charin
        mask:=(8*(in>>6))
        tmp:=colors[in&63]&!($FF<<mask)
        xb:=pst.charin
        xa:=tmp|(xb<<mask)                             
        colors[in&63]:=xa                    
        in:=0  ' prevent next block of IFs from doing anything
      62:                
        gr.pix((pst.charin<<8)|pst.charin, y_screen-(pst.charin<<8)|pst.charin, pst.charin,(pst.charin<<8)|pst.charin+@VRAM)
      63:
        xa:=(pst.charin<<8)|pst.charin
        if xa<_vbytes
          VRAM[xa]:=pst.charin   
      64:              
        pst.char(x_tiles)   
        pst.char(y_tiles)
        pst.char("S")
      65:
        gr.vec((pst.charin<<8)|pst.charin, y_screen-(pst.charin<<8)|pst.charin, (pst.charin<<8)|pst.charin, pst.charin<<5,(pst.charin<<8)|pst.charin+@VRAM) 
      66:
        xa:=(pst.charin<<8)|pst.charin
        if xa<x_tiles*y_tiles    
          screen[xa]:=(screen[xa]&$3FF)|((pst.charin&63)<<10)  
      67:      
        gr.line((pst.charin<<8)|pst.charin,y_screen-(pst.charin<<8)|pst.charin)
      100:         
        iostat:=255
        tmp:=0
        repeat while iostat==255
        repeat while tmp<32
          repeat while tmp<iostat
            gr.plot(coords[tmp*4+2],coords[tmp*4+3])
            gr.line(coords[tmp*4],coords[tmp*4+1])
            tmp++                    
        wordmove(@coords,@iowrite,64)    
        wordmove(@iowrite,@iowrite+64,64)
        wordmove(@iowrite+64,@coords,64)
        iostat:=255
        tmp:=0
        repeat while iostat==255
        repeat while tmp<32
          repeat while tmp<iostat                                                  
            gr.plot(coords[tmp*4+2],coords[tmp*4+3])
            gr.line(coords[tmp*4],coords[tmp*4+1])
            tmp++                    
        wordmove(@coords,@iowrite,64)    
        wordmove(@iowrite,@iowrite+64,64)
        wordmove(@iowrite+64,@coords,64)
      101:
        pst.char(_memlen) ' 3 kilobytes
        pst.char(00)
      104:                 
        xa:=pst.charin                          
        iowrite[xa]:=(pst.charin<<8)|pst.charin|$8000
      105:      
        iowrite[pst.charin]:=0 ' turn the line off
      $D:
        newline
    pst.char(48)
    outa[7]~         
pub min3(a,b,c) : q
  if a<b and a<c
    q:=a
  elseif b<c
    q:=b
  else
    q:=c
pub max3(a,b,c) : q
  if a>b and a>c
    q:=a
  elseif b>c
    q:=b
  else
    q:=c
pub translatef(x1,y1,z1) 
  camera_x:=x1
  camera_y:=y1
  camera_z:=z1   
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
pub rotatef(yaw,pitch)
  camera_yaw:=360-yaw//360
  camera_pitch:=360-pitch//360    
PUB out(c)  | t_c

'' Print a character
''
''       $00 = home
''  $01..$03 = color
''  $04..$07 = color schemes
''       $09 = tab
''       $0D = return
''  $20..$7E = character

  case c

    $00:                'home?
      gr.clear
      x :=0
      y:=1
    $08:
      if x>1
        x:=x-1
      t_c:=gr.getcolor
      gr.color(0)  
      gr.box((x+1) * x_spacing, y_screen - (y*y_spacing+y_offset), x_spacing, y_spacing)
      gr.color(t_c)
    $09:                'tab?
      repeat
        out($20)
      while x & 7

    $0D:                'return?
      newline

    $20..$7E:           'character?
      gr.text(x * x_chr+6, y_screen - (y*y_spacing+y_offset), @c)
      gr.finish
      if ++x == x_limit
        newline


PUB str(string_ptr)

'' Print a zero-terminated string

  repeat strsize(string_ptr)
    out(byte[string_ptr++])
                          
PRI newline

  if ++y == y_limit
    gr.finish
    repeat x from 0 to x_tiles - 1
      y := bitmap_base + x * y_screen_bytes
      longmove(y, y + y_scroll, y_scroll_longs)
      longfill(y + y_clear, 0, y_clear_longs)
    y := y_max
  x := 0    

DAT

vgaparams               long    0               'status
                        long    1               'enable
                        long    %010_111        'pins
                        long    %011            'mode
                        long    0               'videobase
                        long    0               'colorbase
                        long    x_tiles        'hc
                        long    y_tiles        'vc
                        long    1               'hx
                        long    1               'vx
                        long    0               'ho
                        long    0               'vo
                        long    512             'hd
                        long    16              'hf
                        long    96              'hs
                        long    48              'hb
                        long    380             'vd
                        long    11              'vf
                        long    2               'vs
                        long    31              'vb
                        long    20_000_000      'rate 
dat                           
                        org 0  
renderer
                        mov     q1, par    
                        rdlong  ctrl, q1   ' control register
                        add     q1, #4
                        rdlong  iord, q1   ' pasm in main out    
                        add     q1, #4
                        rdlong  iowr, q1   ' coords for 2d renderer  
                        add     q1, #4
                        rdlong  x_con, q1  ' x constant  
                        add     q1, #4                   
                        rdlong  y_con, q1  ' y constant           
                        add     q1, #4   
                        rdlong  camarg, q1 ' camera variables
                        add     q1, #4   
                        rdlong  vmem, q1   ' video memory pointer
                                            
    main3dloop                                 
                        ' wait for start instruction
                        rdlong  q1, ctrl
                        test    q1, #255 wz
              if_z      jmp     #main3dloop
    
                        ' Get temporary trig constants for camera rotation
                        mov     q5, camarg
                        add     q5, #12    ' yaw
                        rdlong  q4, q5
                        mov     q1, q4
                        call    #sin_
                        mov s1, q1
                                                 
                        mov     q1, q4
                        call    #cos_
                        mov c1, q1
                        
                        add     q5, #4   ' @yaw+4=@pitch
                        rdlong  q4, q5 
                        mov     q1, q4
                        call    #sin_
                        mov     s2, q1
                                                   
                        mov     q1, q4
                        call    #cos_
                        
                        ' sync now to save 8 cycles
                        mov q2, camarg 
                        rdlong  q4, q1
                        add q2, #4
                              
                        mov     c2, q1
                        
                        mov     index, 0   
                        rdlong  q5, q2
                        add     q2, #4
                        rdlong  q6, q2
                        
:subloop
                        mov     q1, index       ' tell the main cog how many points have been computed       
                        wrlong  q1, ctrl
                        
                        test    index, #32 wz
                   if_z jmp     #:done
                        mov     q1, index     ' this code gets the RAM address of each line
                        shl     q1, #1
                        add     q1, iord
                        rdword  q1, q1     wz
                   if_z add     index, #1       ' find first one that isn't zero
                   if_z jmp     #:subloop
                        and     q1, ram_mask
                        add     q1, vmem

                        ' q1 now has pointer to 3d coordinates   
                        
                        mov     qx, q4
                        rdlong  q3, q1
                        sub     qx, q3

                        add     q1, #4  ' next value
                                    
                        mov     qy, q5  
                        rdlong  q3, q1
                        sub     qy, q3

                        add     q1, #4  ' next value
                                                   
                        mov     qz, q6 
                        rdlong  q3, q1
                        sub     qz, q3

                        mov     save, q1
                        add     save, #4
                        
                        call    #convert    ' convert first set of points
                        mov     px_, dx_      ' and load them into other registers
                        mov     py_, dy_
                      
                        mov     qx, q4
                        rdlong  q1, save
                        sub     qx, q1
                                    
                        add     save, #4
                                   
                        mov     qy, q5  
                        rdlong  q1, save
                        sub     qy, q1
                                
                        add     save, #4
                                       
                        mov     qz, q6 
                        rdlong  q1, save
                        sub     qz, q1
                        
                        call    #convert    ' convert second set of points
                        
                        ' write mem

                        mov     q1, index
                        shl     q1, #3
                        add     q1, iowr
                        wrword  px_,q1
                        add     q1, #2
                        wrword  py_,q1
                        add     q1, #2
                        wrword  dx_,q1
                        add     q1, #2
                        wrword  dy_,q1
                        
                        
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
multiply_
                        mov     q4, q1
                        xor     q4, q2
                        abs     q1, q1
                        abs     q2, q2
                        mov     q3,#16
                        shl     q2,#16
                        shr     q1,#1           wc

:loop   if_c            add     q1,q2           wc
                        rcr     q1,#1           wc
                        djnz    q3,#:loop
                        shl     q4,#1           wc
              if_c      neg     q1, q1
multiply__ret           ret

divide                  
                        mov     q4, q1
                        xor     q4, q2
                        abs     q1, q1
                        abs     q2, q2
                        
                        shl     q2,#15            'get divisor into y[30..15]
                        mov     q3,#16            'ready for 16 quotient bits
:loop                   cmpsub  q1,q2       wc    'y =< x? Subtract it, quotient bit in c
                        rcl     q1,#1             'rotate c into quotient, shift dividend
                        djnz    q3,#:loop         'loop until done  
                        shl     q4,#1       wc
              if_c      neg     q1, q1
divide_ret              ret                      'quotient in x[15..0],
                                                 'remainder in x[31..16]

cos_
                        add     q1, #90
sin_
                        mov     q2, #91
                        call    #multiply_
                        sar     q1, #2
                        mov     q3, q1
                        mov     q2, q1
                        and     q2, sine_90_   wz
                  if_nz neg     q1, q1
                        or      q1, sine_table_
                        shl     q1, 1
                        rdword  q2, q1
                        and     q3, sine_180_  wz
                  if_nz neg     q2, q2
                        mov     q1, q2
                        shl     q1, precision_pasm  
                        sar     q1, #16
cos__ret
sin__ret
                        ret       
convert
                        ' start by recalculating qz because it relies on unmodified qx           
                        mov     q1, qx
                        mov     q2, s1
                        call    #multiply_
                        mov     q4, q1
                        mov     q1, qz
                        mov     q2, c1
                        call    #multiply_
                        add     q4, q1
                        mov     qz, q4
                        sar     qz, precision_pasm                                         

                        ' qx next for calculating qy
                        
                        mov     q1, qx
                        mov     q2, c1
                        call    #multiply_
                        mov     q4, q1
                        mov     q1, qz
                        mov     q2, s1
                        call    #multiply_
                        sub     q4, q1
                        mov     qx, q4
                        sar     qx, precision_pasm

                        ' finish with qy
                        
                        mov     q1, qx
                        mov     q2, s2
                        call    #multiply_
                        mov     q4, q1
                        mov     q1, qy
                        mov     q2, c2
                        call    #multiply_
                        add     q4, q1
                        mov     qy, q4
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
convert_ret                        
                        ret

                        
                        
 
sine_90_                long    $0800                   '90 degree bit
sine_180_               long    $1000                   '180 degree bit
sine_table_             long    $E000 >> 1              'sine table address shifted right
ram_mask                long    $7FFF                  
precision_pasm          long    7
                                 

  _color long 2
  _plot  long 4
  _line  long 5                 
                        
q1                      res     1       'temps
q2                      res     1
q3                      res     1
q4                      res     1   
q5                      res     1
q6                      res     1

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