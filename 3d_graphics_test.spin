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
  scale = 4
  x_tiles = 4*scale
  y_tiles = 3*scale

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
  long  bitmap_base   
  word  screen[x_tiles * y_tiles]                                                             
  long  bitmap[x_tiles * y_tiles << 4 + 16]     'add 16 longs to allow for 64-byte alignment

  byte str[8]
  long stak[256]

OBJ

  vga   : "vga"          
  gr    : "3d_graphics" 
  pst   : "Parallax Serial Terminal"

PUB boot |mode,in, xa, xb, ya, yb, i, c, dx, dy , tmp, red, grn, blu, mask, ext_ptr , s, z, yaw, pitch, x, y                
  pst.StartRxTx(31, 30, 0, 115200)
  pst.str(string("starting...",$D))
  dira[7]~~   
  'start tv
  bitmap_base := (@bitmap + $3F) & $7FC0
  repeat x from 0 to x_tiles - 1
    repeat y from 0 to y_tiles - 1
      screen[y * x_tiles + x] := bitmap_base >> 6 + y + x * y_tiles
  pst.str(string("loaded screen variable",$D))
  
  longmove(@vga_status, @vgaparams, vga_params)
  vga_videobase := @screen
  vga_colorbase := @colors
  vga.start(@vga_status)
  
  pst.str(string("loaded VGA",$D))
  ext_ptr:=0
  'init colors
  repeat i from 0 to 63
    colors[i] := %%010_0_100_0_333_0_000_0 
  pst.str(string("loaded colors",$D))
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
  
                           
  {gr.add_point(100,10,10)                                                      CUBE
  gr.add_point(300,10,10)
  gr.add_point(100,30,10)                                                   010
  gr.add_point(100,10,30)                                 '                --*--
  gr.add_point(300,10,30)                                 '              ~-  |  ~-                      0=100/10
  gr.add_point(100,30,30)                                 '        011*--    ~    --*110                1=300/30
  gr.add_point(300,30,10)                                 '           |  -~  |  -~  |
  gr.add_point(300,30,30)   }                             '           ~    --*--111 |
  gr.add_line(10,10,10,30,10,10)                          '           |    --*--000 ~
  gr.add_line(30,10,10,30,30,10)                          '           |  ~-  |  ~-  |
  gr.add_line(30,30,10,30,30,30)                          '        001*--    ~    --*100     \
  gr.add_line(30,30,30,10,30,30)                          '              -~  |  -~            \
  gr.add_line(10,30,30,10,10,30)                          '                --*-- 101           \
  gr.add_line(10,10,30,10,10,10)                          '                                     \
                                                          '         ^                        -----=& CAMERA
  gr.add_line(10,10,30,30,10,30)                          '         |Y
  gr.add_line(30,10,10,30,10,30)                          '       Z/ \X
                                   
  gr.add_line(10,10,10,10,30,10)
  gr.add_line(30,10,30,30,30,30)
                         
  gr.add_line(10,30,10,30,30,10)
  gr.add_line(10,30,10,10,30,30)    
                                                          '           
  pst.str(string("wrote points to renderer",$D))    
  x:=-70*91
  y:=20*91
  z:=0*91
  yaw:=0
  pitch:=0     
  gr.translatef(x/91,y/91,z/91)
  gr.color(3)
  'cognew(stats,@stak)
  dira[7]~~   
  repeat     
    !outa[7]       
    gr.text(4,96-4,stRing("CAM:"))   
    rjdec(x/91,3)
    gr.text(28,96-4,@str)
    gr.text(46,96-4,string(",20,"))
    rjdec(z/91,3)
    gr.text(70,96-4,@str)
    gr.text(88,96-4,string(":"))
    rjdec(yaw,3)
    gr.text(94,96-4,@str)
    gr.text(114,96-4,string(","))
    rjdec(pitch,3)
    gr.text(118,96-4,@str) 
    waitcnt(cnt+2000000)
    outa[7]~          
    waitcnt(cnt+2000000)
    next     
    in:=pst.charin
    if in==119
      ' forward 
      x+=cos(yaw,91)
      z+=sin(yaw,91)
    elseif in==115
      ' backward
      x-=cos(yaw,91)
      z-=sin(yaw,91)
    elseif in==97
      ' left
      x+=cos(yaw-90,91)
      z+=sin(yaw-90,91)
    elseif in==100
      ' right
      x+=cos(yaw+90,91)
      z+=sin(yaw+90,91) 
    else
      if in==101
        yaw+=1
      elseif in==114
        yaw-=1
      elseif in==102
        pitch+=1                                                                           
      elseif in==103
        pitch-=1
      else
        next
      gr.rotatef(yaw,pitch)
      next 
    gr.translatef(x/91,20,z/91)        
    '}
  {s:=-40                   
  gr.translatef(-10,20,-10)
  repeat                       
    waitcnt(cnt+40000000)
    pst.str(string($D,"------------------------"))  
    pst.str(string($D,"FPS: "))
    pst.dec(gr.getfps)          
    pst.str(string($D,"vtx: "))
    pst.dec(gr.getverts)       
    pst.str(string($D,"deg: "))
    pst.dec(gr.sin(273,100))          
    {pst.str(string($D,"x: "))
    pst.dec(gr.translate_x(20,30))                       
    pst.str(string($D,"y: "))
    pst.dec(gr.translate_y(20,30))  }     
    gr.translatef(-40,20,s)           
    s:=s+1          '}
pub stats | time
  
  repeat
    time:=cnt
    gr.wait
    gr.color(2)
    gr.text(4,70,stRing("GPU:"))   
    'rjdec(gr.getfps,3)
    gr.color(0)
    gr.box(28,74,30,14)
    gr.color(2)
    'gr.text(28,70,@str)
    gr.continue
    waitcnt(time+clkfreq/4)
pub rjdec(number, length) | idx        

'' ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
'' // Converts an integer number to the decimal string of that number padded with zeros.
'' //
'' // Returns a pointer to the converted string.
'' //
'' // Number - A 32 bit signed integer number to be converted to a string.
'' // Length - The length of the converted string, "+" or "-" will be concatenated onto the head of converted string.
'' ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
                
  idx:=0
  if(number < 0)
    str[0]:= "-"
    idx:=1      
  repeat result from length-1-idx to 0
    str[result+idx] := ((||(number // 10)) + "0")
    number /= 10

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
DAT

vgaparams               long    0               'status
                        long    1               'enable
                        long    %010111         'pins
                        long    %0000           'mode
                        long    0               'videobase
                        long    0               'colorbase
                        long    x_tiles         'hc
                        long    y_tiles         'vc
                        long    2               'hx
                        long    2               'vx
                        long    0               'ho
                        long    0               'vo
                        long    640             'hd
                        long    13              'hf
                        long    96              'hs
                        long    45              'hb
                        long    480             'vd
                        long    9               'vf
                        long    2               'vs
                        long    30              'vb
                        long    25_175_000      'rate   
           
{{
┌──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│                                                   TERMS OF USE: MIT License                                                  │                                                            
├──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation    │ 
│files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy,    │
│modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software│
│is furnished to do so, subject to the following conditions:                                                                   │
│                                                                                                                              │
│The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.│
│                                                                                                                              │
│THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE          │
│WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR         │
│COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,   │
│ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                         │
└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
 }}                        