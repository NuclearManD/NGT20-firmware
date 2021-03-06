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
  long  tv_status     '0/1/2 = off/visible/invisible           read-only
  long  tv_enable     '0/? = off/on                            write-only
  long  tv_pins       '%ppmmm = pins                           write-only
  long  tv_mode       '%ccinp = chroma,interlace,ntsc/pal,swap write-only
  long  tv_screen     'pointer to screen (words)               write-only
  long  tv_colors     'pointer to colors (longs)               write-only               
  long  tv_hc         'horizontal cells                        write-only
  long  tv_vc         'vertical cells                          write-only
  long  tv_hx         'horizontal cell expansion               write-only
  long  tv_vx         'vertical cell expansion                 write-only
  long  tv_ho         'horizontal offset                       write-only
  long  tv_vo         'vertical offset                         write-only
  long  tv_broadcast  'broadcast frequency (Hz)                write-only
  long  tv_auralcog   'aural fm cog                            write-only

  'word  screen[screensize]      
  long  colors[64]  
  long  tvcolors[64] 
  byte strptr1[256]
  byte strptr2[256]
  byte strptr3[256]
  byte VRAM[2048]
  long  x, y, bitmap_base   
  word  screen[x_tiles * y_tiles]                                                             
  long  bitmap[x_tiles * y_tiles << 4 + 16]     'add 16 longs to allow for 64-byte alignment   

OBJ

  vga   : "vga"
  tv    : "TV"
  gr    : "graphics"
  pst   : "Parallax Serial Terminal"

PUB boot |mode,in, xa, xb, ya, yb, i, c, dx, dy , tmp, red, grn, blu, mask, ext_ptr 
  dira[7]~~   
  'start tv
  bitmap_base := (@bitmap + $3F) & $7FC0
  repeat x from 0 to x_tiles - 1
    repeat y from 0 to y_tiles - 1
      screen[y * x_tiles + x] := bitmap_base >> 6 + y + x * y_tiles
  longmove(@vga_status, @vgaparams, vga_params)
  vga_videobase := @screen
  vga_colorbase := @colors
  vga.start(@vga_status)
  longmove(@tv_status, @tvparams, paramcount)
  tv_screen := @screen
  tv_colors := @tvcolors
  tv.start(@tv_status)
  ext_ptr:=0
  'init colors
  repeat i from 0 to 63
    colors[i] := %%010_0_100_0_333_0_000_0
  repeat i from 0 to 63
    tvcolors[i] := $f8380702
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
  pst.start(115200) 
  mode:=1  
  x :=0
  y:=1
  repeat while mode==1
    in:=pst.charin
    !outa[7]
    if in==1
      mode:=0
    else
      out(in)
    outa[7]~      
  pst.char(48)    
  mode:=1  ' maybe some code uses this?
  repeat                  
    in:=pst.charin
    !outa[7]          
    if in==32
      pst.str(string("NGP100",13))  
    elseif in==48
      reboot
    elseif in==49  
      out(0)                  
    elseif in==50
      x:=pst.charin
      y:=pst.charin+1     
    elseif in==51
      pst.strin(@strptr3)',255)
      str(@strptr3)   
    elseif in==52                 
      xa:=(pst.charin<<8)|pst.charin
      ya:=y_screen-(pst.charin<<8)|pst.charin
      xb:=(pst.charin<<8)|pst.charin
      yb:=y_screen-(pst.charin<<8)|pst.charin      
      gr.plot(xa,ya)
      gr.line(xb,yb)      
    elseif in==53
      out(0)
      in:=pst.charin     
      gr.color(in&3)                         
      repeat x_screen*y_screen
        out(32)   
        x :=0
        y:=1  
    elseif in==54                    
      xa:=(pst.charin<<8)|pst.charin
      yb:=y_screen-(pst.charin<<8)|pst.charin  
      xb:=(pst.charin<<8)|pst.charin
      ya:=y_screen-(pst.charin<<8)|pst.charin                
      i:=ya
      'repeat while i<yb
      '  gr.plot(xa,i)
      '  gr.line(xb,i)
      '  i:=i+1
      gr.box(xa, ya, xb-xa, yb-ya)     
    elseif in==55      
      xa:=(pst.charin<<8)|pst.charin
      ya:=y_screen-(pst.charin<<8)|pst.charin-13  
      pst.strin(@strptr1+ext_ptr)',255)
      gr.text(xa, ya, @strptr1+ext_ptr)
      ext_ptr:=256-ext_ptr   
    elseif in==56      
      xa:=(pst.charin<<8)|pst.charin
      yb:=y_screen-(pst.charin<<8)|pst.charin
      gr.plot(xa,ya)
      'mask:=!(3<<(xa*2))
      'xb:=bitmap_base[xa>>3]+ya<<2       
      'bitmap_base[x_tiles*yb>>4+xb>>4]:=bitmap_base[x_tiles*yb>>4+xb>>4]&(16*(yb//16))   
    elseif in==57
      in:=pst.charin                             
      gr.color(in&3)
    elseif in==59
      in:=pst.charin                             
      gr.textmode(in,in,6,8)
    elseif in==60
      gr.finish   
    elseif in==61
      in:=pst.charin
      mask:=(8*(in>>6))
      tmp:=colors[in&63]&!($FF<<mask)
      xb:=pst.charin
      xa:=tmp|(xb<<mask)                             
      colors[in&63]:=xa
      
      red:=(xb>>7)+((xb>>6)&1)*2
      grn:=(xb&%%300)>>4
      blu:=(xb&%%30)>>2     
      grn:=(grn<<1)&2+grn>>1
      blu:=(blu<<1)&2+blu>>1                    
      if red==grn and grn==blu
        ya:=0
        yb:=0
      else      
        yb:=hue(red, grn, blu)<<4  ' chroma
        ya:=8                
      ya:=ya|(2+2*(red+grn+blu)/3) ' luma
      if ya==1 or ya==7
        ya:=12  
      tmp:=tvcolors[in&63]&!($FF<<mask)
      tvcolors[in&63]:=tmp|((ya|yb)<<mask)
      in:=0  ' prevent next block of IFs from doing anything
    elseif in==62                
      gr.pix((pst.charin<<8)|pst.charin, y_screen-(pst.charin<<8)|pst.charin, pst.charin,(pst.charin<<8)|pst.charin+@VRAM)
    elseif in==63
      xa:=(pst.charin<<8)|pst.charin
      if xa<2048
        VRAM[xa]:=pst.charin   
    if in==64              
      pst.char(x_tiles)   
      pst.char(y_tiles)
      pst.char("D")
    elseif in==65
      gr.vec((pst.charin<<8)|pst.charin, y_screen-(pst.charin<<8)|pst.charin, (pst.charin<<8)|pst.charin, pst.charin<<5,(pst.charin<<8)|pst.charin+@VRAM) 
    elseif in==66
      xa:=(pst.charin<<8)|pst.charin
      if xa<x_tiles*y_tiles    
        screen[xa]:=(screen[xa]&$3FF)|((pst.charin&63)<<10)  
    elseif in==67                 
      xa:=(pst.charin<<8)|pst.charin
      ya:=y_screen-(pst.charin<<8)|pst.charin    
      gr.line(xa,ya) 
    elseif in==$D
      newline
    pst.char(48)
    outa[7]~
pub hue(a,b,c) : q
  if a==b and c==b
    q:=0
  else
    if a>b and a>c
      q:=0+(b-c)/(a-min3(a,b,c))
    elseif b>c
      q:=2+(c-a)/(b-min3(a,b,c))
    else
      q:=4+(a-b)/(c-min3(a,b,c))
    q:=360-q*60-90'+180 
    if q<0
      q:=q+360
    if q>360
      q:=q-360
     
    q:=(q/24)&$ff     
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

     {
PUB dec(value) | i

'' Print a decimal number

  if value < 0
    -value
    out("-")

  i := 1_000_000_000

  repeat 10
    if value => i
      out(value / i + "0")
      value //= i
      result~~
    elseif result or i == 1
      out("0")
    i /= 10


PUB hex(value, digits)

'' Print a hexadecimal number

  value <<= (8 - digits) << 2
  repeat digits
    out(lookupz((value <-= 4) & $F : "0".."9", "A".."F"))


PUB bin(value, digits)

'' Print a binary number

  value <<= 32 - digits
  repeat digits
    out((value <-= 1) & 1 + "0")

             }
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
tvparams                long    0               'status
                        long    1               'enable
tvparams_pins           long    %001_0101       'pins
                        long    %0000           'mode
                        long    0               'screen
                        long    0               'colors
                        long    x_tiles         'hc
                        long    y_tiles         'vc
                        long    10              'hx
                        long    1               'vx
                        long    0               'ho
                        long    0               'vo
                        long    55_250_000      'broadcast
                        long    0               'auralcog
           
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