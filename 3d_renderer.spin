con         
  _loop  = 15                                                                                       
    
  max_points = 10
  fov=60
  y_fov=50
  precision=16384 ' higher the more accurate; lower is a bigger world
var

  long  r_cog         

  long y_pixels,x_pixels

  long bitmap_base, bitmap_longs

  long command_loc           
                                   
  long graphics_initializer[20]
  byte safe
  byte pause ' set to 1 to pause rendering cog
   
  long num_points
                      
  long line_x1[max_points]  
  long line_y1[max_points]
  long line_z1[max_points]
  long line_x2[max_points]
  long line_y2[max_points]
  long line_z2[max_points]
   
  long camera_x, camera_y, camera_z 
              
  long camera_yaw, camera_pitch
                                                   
  long x_const,y_const
                     
pub start(x_tiles,y_tiles,cmd, base)    

  bitmap_longs:=x_tiles*y_tiles<<4
  bitmap_base:=base
  ' init 3D driver               
  num_points:=0   
                         
  y_pixels:=y_tiles<<4
  x_pixels:=x_tiles<<4  
  x_const:=x_pixels*91/tan(fov/2,91*2)
  y_const:=y_pixels*91/tan(y_fov/2,91*2)
  camera_yaw:=camera_pitch:=0
                                 
  graphics_initializer[0]:=@safe
  graphics_initializer[1]:=@pause     
  graphics_initializer[2]:=@num_points          
  graphics_initializer[3]:=@line_x1             
  graphics_initializer[4]:=@line_y1             
  graphics_initializer[5]:=@line_z1             
  graphics_initializer[6]:=@line_x2             
  graphics_initializer[7]:=@line_y2             
  graphics_initializer[8]:=@line_z2           
  graphics_initializer[9]:=@camera_x             
  graphics_initializer[10]:=@camera_y           
  graphics_initializer[11]:=@camera_z           
  graphics_initializer[12]:=@camera_yaw           
  graphics_initializer[13]:=@camera_pitch                                                                          
  graphics_initializer[14]:=x_const          
  graphics_initializer[15]:=y_const         ' not pointers because these are constants once the graphics are set up     
  graphics_initializer[16]:=cmd             ' communicate with main graphics cog                  

  command_loc:=cmd
  
  r_cog := cognew(@entry, @graphics_initializer) + 1         
  pause:=0 ' let 'er rip!
  
pub stop
  
  if r_cog
    cogstop(r_cog~ - 1) 
pub add_line(x1,y1,z1,x2,y2,z2)                
  line_x1[num_points]:=x1
  line_y1[num_points]:=y1 
  line_z1[num_points]:=z1                
  line_x2[num_points]:=x2
  line_y2[num_points]:=y2 
  line_z2[num_points]:=z2                                         
  repeat while(safe==0)
  num_points:=num_points+1      
pub getxconst
  return x_const  
pub getyconst
  return y_const
pub getxpixels
  return x_pixels    
pub getypixels
  return y_pixels
pub getverts
  return num_points                                                          
'pub translate_x(z,x)                                                   
'  return (x_pixels/2)+(x_const*(z-camera_z))/(x-camera_x)
'pub translate_y(y,x)
'  return (y_pixels/2)-(y_const*(y-camera_y))/(x-camera_x)                                                                   
pub translate_x(x,z) |q
  return (x_const*z/x)    
pub translate_y(x,y)
  return y_const*y/x
  'return (y_const*tan(arctan((y-camera_y)/(x-camera_x))-camera_pitch,256))>>8         '}    
pub translatef(x,y,z) 
  camera_x:=x
  camera_y:=y
  camera_z:=z
  pause:=0
pub rotatef(yaw,pitch)
  camera_yaw:=360-yaw//360
  camera_pitch:=360-pitch//360
  pause:=0
pub wait
  pause:=1
  repeat while(safe==0)
pub continue
  pause:=0  
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
{pub sqrt(VALUE) :ROOT | SUBT,COUNTe

  COUNTe := 0
  SUBT := 0
  VALUE := VALUE - 1

  REPEAT
    VALUE := VALUE - SUBT
    IF VALUE < 0
      RETURN COUNTe
    SUBT += 2
    COUNTe += 1
    IF VALUE == 0
      RETURN COUNTe
}
Pri clear

'' Clear bitmap

  setcommand(_loop, 0)                                  'make sure last command finished

  longfill(bitmap_base, 0, bitmap_longs)                'clear bitmap


PRI setcommand(cmd, argptr)      
  repeat while long[command_loc]                                  'wait for last command to finish
  long[command_loc] := cmd << 16 + argptr                         'write command and pointer    
dat                           
                        org 0
entry

renderer
                        mov     q1, par    
                        rdlong  safeloc, q1
                        add     q1, #4
                        rdlong  pauseloc, q1    
                        add     q1, #4
                        rdlong  num_pts, q1 
                        add     q1, #4
                        rdlong  x1s, q1    
                        add     q1, #4
                        rdlong  y1s, q1   
                        add     q1, #4
                        rdlong  z1s, q1   
                        add     q1, #4
                        rdlong  x2s, q1  
                        add     q1, #4
                        rdlong  y2s, q1 
                        add     q1, #4
                        rdlong  z2s, q1   
                        add     q1, #4
                        rdlong  cam_x, q1 
                        add     q1, #4
                        rdlong  cam_y, q1  
                        add     q1, #4
                        rdlong  cam_z, q1    
                        add     q1, #4
                        rdlong  cam_yaw, q1  
                        add     q1, #4
                        rdlong  cam_pitch, q1  
                        add     q1, #4 
                        rdlong  x_con, q1    
                        add     q1, #4  
                        rdlong  y_con, q1
                        add     q1, #4  
                        rdlong  gfxcmdreg, q1
                        
                        mov     timer, cnt
    main3dloop                         
                        jmp     #main3dloop
                        mov     q1, #1
                        wrbyte  q1, safeloc 
                        rdbyte  q1, pauseloc
                        test    q1, #1           wz
                if_z    jmp     #main3dloop 
                        rdword  points, num_pts wz
                if_z    jmp     #endDrawLoop
                        
                        ' Get temporary trig constants for camera rotation
                     
                        rdlong  q4, cam_yaw
                        mov     q1, q4
                        call    #sin_
                        mov s1, q1
                                                 
                        mov     q1, q4
                        call    #cos_
                        mov c1, q1
                        
                        
                        rdlong  q4, cam_pitch 
                        mov     q1, q4
                        call    #sin_
                        mov     s2, q1
                                                   
                        mov     q1, q4
                        call    #cos_
                        
                        ' sync now to save 8 cycles  
                        rdlong  q4, cam_x
                        
                        mov     c2, q1
                        
                        mov     index, 0   
                        rdlong  q5, cam_y  
                        rdlong  q6, cam_z                         
':waitloop3
              '          rdlong  q1, gfxcmdreg   wz                    
              'if_nz     jmp     #:waitloop3

                        'mov     q1, #1
                        'wrlong  q1, par   
                        'mov     q1, _color
                        'shl     q1, #16
                        'add     q1, par
                        'wrlong  q1, gfxcmdreg
                           
:subloop                    
                        mov     q1, #1
                        wrbyte  q1, safeloc
                        
                        mov     qx, q4                 
                        mov     q1, x1s
                        mov     q2, index
                        shl     q2, #2
                        add     q1, q2
                        rdlong  q1, q1
                        sub     qx, q1
                                    
                        mov     qy, q5        
                        mov     q1, y1s
                        mov     q2, index
                        shl     q2, #2
                        add     q1, q2
                        rdlong  q1, q1
                        sub     qy, q1
                                                   
                        mov     qz, q6
                        mov     q1, z1s
                        mov     q2, index
                        shl     q2, #2
                        add     q1, q2
                        rdlong  q1, q1
                        sub     qz, q1
                        
                        call    #convert    ' convert first set of points
                        mov     px_, dx_      ' and load them into other registers
                        mov     py_, dy_
                                              
                        mov     qx, q4
                        mov     q1, x2s
                        mov     q2, index
                        shl     q2, #2
                        add     q1, q2
                        rdlong  q1, q1
                        sub     qx, q1
                                               
                        mov     qy, q5
                        mov     q1, y2s
                        mov     q2, index
                        shl     q2, #2
                        add     q1, q2
                        rdlong  q1, q1
                        sub     qy, q1
                                               
                        mov     qz, q6
                        mov     q1, z2s
                        mov     q2, index
                        shl     q2, #2
                        add     q1, q2
                        rdlong  q1, q1
                        sub     qz, q1
                        
                        call    #convert    ' convert second set of points
                        
                        ' LOAD COLOR HERE
                        
                        
                        mov     q1, #0
                        wrbyte  q1, safeloc
                                                 
:waitloop
                        rdlong  q1, gfxcmdreg   wz                    
              if_nz     jmp     #:waitloop

                        wrlong  dx_,par
                        mov     q1, par
                        add     q1, 4
                        wrlong  dy_,q1
                        mov     q1, _plot
                        shl     q1, #16
                        add     q1, par
                        wrlong  q1, gfxcmdreg
                        
:waitloop2
                        rdlong  q1, gfxcmdreg   wz  ' wait for it to finish                  
              if_nz     jmp     #:waitloop2
                        
                        wrlong  px_,par
                        mov     q1, par
                        add     q1, 4
                        wrlong  py_,q1
                        mov     q1, _line
                        shl     q1, #16
                        add     q1, par
                        wrlong  q1, gfxcmdreg
                        
                        ' Now check for end of loop
                        add     index, #1
                        test    index, points  wz
                  if_nz jmp     #:subloop

                        
endDrawLoop
                        mov     q1, #1
                        wrbyte  q1, safeloc
                        rdlong  q1, #0
                        shr     q1, #6
                        waitcnt timer, q1
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
                        {
sqrt_
                        mov     q2, #0
                        mov     q3, #0
                        sub     q1, #1
:loop
                        sub     q1, q3    wc wz
                  if_c  jmp     :returnseq
                        add     q3, #1
                  if_z  jmp     :returnseq
                        add     q2, #2
                        jmp     #:loop
:returnseq
                        mov     q1, q3
                        ret
                        }
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

                        
                        
 
sine_90_                long    $0800                   '90° bit
sine_180_               long    $1000                   '180° bit
sine_table_             long    $E000 >> 1              'sine table address shifted right                       
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
timer                   res     1


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

safeloc                 res     1
pauseloc                res     1  
num_pts                 res     1
x1s                     res     1
y1s                     res     1
z1s                     res     1
x2s                     res     1
y2s                     res     1
z2s                     res     1
cam_x                   res     1  
cam_y                   res     1
cam_z                   res     1 
cam_yaw                 res     1  
cam_pitch               res     1 
x_con                   res     1
y_con                   res     1
gfxcmdreg               res     1

points                  res     1
                                 

                        fit