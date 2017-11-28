#pragma GCC diagnostic warning "-fpermissive"
//#pragma GCC diagnostic warning "-pedantic"
#include "NTIKernel.h"
void setup() {
  // put your setup code here, to run once:
  k_init();
  vga.println("System is booting with GEAR...");
  __redraw[gr.addProcess(shell_upd,shell_fup,(char*)"Shell",__empty,P_ROOT|P_KILLER)]=true;
  //println("Starting system processes...");
  //gr.addProcess(__empty,usage_daemon,(char*)"daemon0",P_ROOT);
  vga.println("Setup done, please wait...");
  vga.println("Any key to open shell.");
  while(!kbd.available());
  kbd.read();
  vga.clear();
  for(byte i=0;i<gr.processes;i++){
    gr.ftimes[i]=millis()+gr.fupd_rate;
  }
  stdo=(void (*)(const char*))noprnt;
  stde=(void (*)(const char*))noprnt;
  File r = SD.open("PGM.BAS",FILE_WRITE);
  r.println("print \"Hello World!\"");
  randomSeed(millis()+analogRead(A5));
}

void loop() {
  gr.run();
  if(gr.processes==0){
    vga.clear();
    vga.set_color(2);
    vga.println(F("Computer has crashed:\n  No more running processes.\n  Any key to reset arduino."));
    while(kbd.available())kbd.read();
    while(!kbd.available());
    reset();
  }
  if(kbd.available()){
    last_key=kbd.read();
    randomSeed(millis()*last_key);
  }else
    last_key=0;
  if(sel_process>=gr.processes){
    sel_process=0;
    sw_gui(0);
  }
}
bool dir=false;
String apps[5]=   {"TaskMan" ,"reboot", "cmd",  "creeperface","EDIT"};
void (*upd[5])()= {taskupd   ,__empty , __empty,cp_upd,edit_upd};
void (*fupd[5])()={taskfu    ,__empty , __empty,cp_fup,edit_fup};
void (*stp[5])()= {__empty   ,0       , term,   cp_strt,edit_start};//,__q,__q,__q};
byte napps=5;
void shell_fup(){
  if(sel_process==0){
    if(redraw){
      selected=0;
      window(0,1,15,napps+2);
      vga.set_cursor_pos(0,1);
      vga.set_color(0);
      vga.print("OPEN PROGRAM:");
      vga.set_cursor_pos(1,2);
      vga.set_color(3);
      vga.print(apps[0]);
      vga.set_color(2);
      for(byte i=1;i<napps;i++){
        vga.set_cursor_pos(1,2+i);
        vga.print(apps[i]);
      }
      term_close();
      redraw=false;
    }
  }else{
    if(redraw){
      redraw=false;
    }
  }
}
void shell_upd(){
  if(sel_process==0){  // Program Opener
    if(available()){
      char c=read();
      byte q=selected;
      if(c==PS2_DOWNARROW){
        selected++;
        if(selected>=napps)
          selected=0;
      }else if(c==PS2_UPARROW){
        selected--;
        if(selected==255)
          selected+=napps;
      }else if(c==PS2_ENTER){
        q=selected=launch((*upd[selected]),(*fupd[selected]),apps[selected].c_str(),(*stp[selected]));
        
        if(selected>0){
          sw_gui(1);
          vga.clear();
        }else
          alert("ERROR: too many processes!");
      }else if(c==(PS2_TAB)){
        selected=0;
        term_close();
        sw_gui(0);
        sel_process+=1;
        if(selected>=gr.processes)
          selected=0;
        vga.clear();
      }
      if(q!=selected){
        for(byte i=0;i<napps;i++){
          if(selected==i)
            vga.set_color(3);
          else
            vga.set_color(2);
          vga.set_cursor_pos(1,2+i);
          vga.print(apps[i]);
        }
      }
    }
  }else{ // Background
    if((last_key==PS2_ESC)&&(sel_process!=0)){
      gr.kill(sel_process);
      selected=0;
      term_close();
      sw_gui(0);
      sel_process=0;
    }else if(last_key==(PS2_TAB)){
      selected=0;
      term_close();
      sw_gui(0);
      sel_process+=1;
      if(selected>=gr.processes)
        selected=0;
    }
  }
}
long last_task_time=0;
byte last_task_count=0;
byte task_sel=0;
void taskfu(){
  if(last_task_time>millis())
    return;
  if(last_task_count>gr.processes){
    vga.set_color(0);
    vga.fill_box(112,(gr.processes+3)*13-6,219,(last_task_count+3)*13+3);
  }
  window(18,1,35,2+gr.processes);
  for(int i=0;i<gr.processes&&i<8;i++){
    if(task_sel==i)
      vga.set_color(3);
    else
      vga.set_color(2);
    vga.set_cursor_pos(18,1+i);
    vga.print(48+i);
    if(sel_process!=i)
      vga.print(':');
    else
      vga.print('*');
    vga.print(gr.getName(i));
    if((!strcmp(gr.getName(i),"TaskMan"))&&i!=gr.cprocess){
      alert((char*)"TaskMan already running.");
      gr.kill(i);
    }
  }
  vga.set_color(3);
  vga.set_cursor_pos(18,1+gr.processes);
  vga.print(F("Free RAM : "));
  vga.print(String(freeRam()));
  last_task_time=millis()+1000;
  last_task_count=gr.processes;
}
void taskupd(){
  byte q=task_sel;
  if(available()){
    char c=read();
    if(c==PS2_DOWNARROW){
      task_sel++;
    }else if(c==PS2_UPARROW){
      task_sel--;
    }else if(c==PS2_DELETE){
      int res=gr.kill(task_sel);
      if(res>=0)
        sel_process=res;  // update selected process
      else if(res==ACCESS_DENIED){
        system("sudo -f");
        system((String("terminate ")+String(task_sel)).c_str());
        alert("Self-hacked.");
      }else if(res==INVALID_ARGUMENT)
        alert("Error: A Glitch");
      else
        alert("Unknown Error");
      q=task_sel+1; // redraw IMMEDIATELY
    }
  }
  if(task_sel>=gr.processes)
    task_sel=0;
  else if(task_sel==255)
    task_sel+=gr.processes;
  if(q!=task_sel){
    last_task_time=0; // Allow next call to work...
    taskfu();         // Redraw graphics
  }
}
Sprite face;
long cp_score=65025;
bool alloc_face=false;
long cp_time;
int cpx=50;
int cpy=50;
int cpex, cpey;
byte face_buffer[20];
void cp_strt(){
  cp_score=65025;
  if(!alloc_face){
    cpex=random(10,100);
    cpex=random(10,80);
    alloc_face=true;
    face.binary_image=face_buffer;
    face.set_size(8,8);
    face.set_center(4,4);
    face.fill(0);
    face.pixel(2,2,1);
    face.pixel(5,2,1);
    face.pixel(2,5,1);  // this is the exact same sprite from
    face.pixel(5,5,1);  // the old NuclearGames CreeperFace
    face.pixel(3,4,1);  // game.  Good game, glad I didn't
    face.pixel(4,4,1);  // loose it.  Now I make it again :D
    face.upload();
  }
  cp_time=millis();
}
void cp_end(){
  cpex=random(10,cols-10);
  cpex=random(10,rows-10);
  cp_score=65025;
  gr.kill(gr.cprocess);
}
void cp_upd(){
  if(available()){
    char c = read();
    // check for some of the special keys
    if (c == PS2_LEFTARROW) {
      cpx=cpx-2;
    } else if (c == PS2_RIGHTARROW) {
      cpx=cpx+2;
    } else if (c == PS2_UPARROW) {
      cpy=cpy-2;
    } else if (c == PS2_DOWNARROW) {
      cpy=cpy+2;
    }
  }
  cp_score=cp_score-(millis()-cp_time);
  cp_time=millis();
  if (cpx==cpex && cpy==cpey){
    alert(("You win! Score: "+String(cp_score)).c_str());
    cp_end();
  }
  if(cp_score<0){
    alert("You failed.  GG.");
    cp_end();
  }
}
void cp_fup(){
  gr.ftimes[gr.cprocess]=millis()+200;
  cpex=(cpex+random(0,5))-2;
  cpey=(cpey+random(0,5))-2;
  if(cpex>cols){
    cpex=cpex-5;
  }
  if(cpex<0){
    cpex=cpex+5;
  }
  if(cpey>rows){
    cpey=cpey-5;
  }
  if(cpey<0){
    cpey=cpey+5;
  }
  if(cpx>cols){
    cpx=cpx-5;
  }
  if(cpx<0){
    cpx=cpx+5;
  }
  if(cpy>rows){
    cpy=cpy-5;
  }
  if(cpy<0){
    cpy=cpy+5;
  }
  vga.clear();
  //vga.set_color(0);
  //vga.fill_box(max(0,cpx-4), max(0,cpy-4), cpx+4, cpy+4);
  //vga.fill_box(max(0,cpex-4), max(0,cpey-4), cpex+4, cpey+4);
  face.display(cpx,cpy,0);
  face.display(cpex,cpey,0);
  for(byte i=0;i<40;i++)
    __redraw[i]=true;
}
unsigned short term_cnt=0;
void term(){
  if(!okcancel("CMD needs to stop GEAR.")){
    gr.kill(gr.cprocess); // die, it canceled.
    return;
  }else{
    stdo=term_print;
    stde=term_error;
    char term_cmd[64];
    vga.set_cursor_pos(0,0);
    vga.clear();
    vga.set_color(1);
    vga.println("Terminal Started.\nLeave with 'exit -L'");
    vga.print("> ");
    while(true){
      if(kbd.available()){
        char c=kbd.read();
        if((c=='\n')||(c=='\r')){
          term_cmd[term_cnt]=0;
          if(!strcmp(term_cmd,"exit -L"))
            break;
          vga.print('\r');
          system(term_cmd);
          vga.set_color(1);
          term_cnt=0;
          vga.print("> ");
        }else if(c==PS2_BACKSPACE){
          vga.print(c);
          term_cnt--;
        }else{
          term_cmd[term_cnt]=c;
          term_cnt++;
          vga.print(c);
        }
      }
    }
  }
  gr.kill(gr.processes-1);// stop
  gear_stopwait();// fix GEAR
}
void edit_start(){
  fileChooser();
  gr.kill(gr.cprocess);
}
void edit_upd(){
  
}
void edit_fup(){
  
}
