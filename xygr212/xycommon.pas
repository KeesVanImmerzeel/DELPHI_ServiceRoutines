unit xycommon;

{=========================================================================}
{ See Copyright notes at XYGRAPH.                                         }
{=========================================================================}

interface

uses Windows , {Messages,} SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    {StdCtrls, Spin,} ExtCtrls, Clipbrd{, AppEvnts}, xygraph;

{ these procedures and variables are to be used by XYGRAPH and XYGRAPH3D}
{ do not call them directly }

function mixcolor(cl1,cl2:Tcolor;f1,f2 : integer):Tcolor;
function reversecolor(cl:integer):integer;

procedure copyscreen(mode:integer);
procedure restorescreen;

procedure position0(x,y,z:single;var xp,yp:integer);

procedure printpart(x1,y1,x2,y2:integer);
procedure store;
procedure restore;
procedure setcursor(xp,yp:integer);
procedure xypixel(xp,yp:integer;col:Tcolor);
procedure xyrectangle(col1,col2,x1,y1,x2,y2:integer);
procedure xyline(col,x1,y1,x2,y2:integer);
procedure xypolygon(col1,col2:integer;poly:array of Tpoint);
procedure transpolygon(col1,col2:integer;poly:array of Tpoint);
procedure xycircle(col1,col2,x,y,r:integer);
procedure transcircle(col1,col2,x,y,r:integer);

procedure initwmf;
procedure openwmf(var s:string;var ok:boolean);
procedure closewmf(var ok:boolean);
procedure initgrid;
procedure dogrid(p1,p2:integer;xas:boolean);
procedure checkwmfrgpen;
procedure checkwmfpen;
procedure wmfredpen;
procedure wmfgrnpen;
procedure checkwmfbrush;
procedure checkwmffont;
procedure linewmf(x1,y1,x2,y2:integer);
procedure blokwmf(x1,y1,x2,y2:integer);
procedure ellipsewmf(x1,y1,x2,y2:integer);
procedure rop2wmf(n:integer);
procedure polygonwmf(points: array of Tpoint);
procedure addfine(p:integer);
procedure addcoarse(p:integer);
procedure pixelwmf(x,y:integer; col:Tcolor);
procedure movewmf(x,y:integer);
procedure drawwmf(x,y:integer);
procedure textwmfalign(n:integer);
procedure textwmfout(x,y:integer;s:string);
procedure bitmapwmf(x1,y1,x2,y2:integer);

procedure inittext3d(f,x1,x2,y1,y2:single;var ok:boolean);
procedure text3d(s:string;x,y,z:single;m1:integer;lbl:boolean; 
       oct,ml:integer;zlvert:boolean);

var xycanvas : Tcanvas;
    xypaintbox : Tpaintbox;
    frontcolor, backcolor : Tcolor;   {default kleuren}
    oldfcolor, oldbcolor : Tcolor;     { idem }
    dowmf, doprint : boolean;         {bezig met metafile of print}
    useroff : boolean;                {user coord uitgeschakeld}
    mode3d : boolean;                 {3D mode}
    igraph, ngraph : integer;         {actuele grafiek, aantal grafieken}
    res : integer;                    {resolutie}
    oldpen : Tpen;                    {vorige pen}
    oldbrush : Tbrush;                {vorige brush}
    cwidth, cheight : integer;        {afmetingen canvas}
    cvmode : boolean;                 {canvas mode}
    vol, volok : boolean;             {er is een volume, is geplot}
    xyfontangle : integer;            {hoek van font}
    transmode : integer;              {transparancy mode}
    transval : single;                {transparancy level}
    transopt : integer;               {transparancy options}
    prxoff,pryoff : integer;          {printer offset}
    prfac :integer;                   {printer factor}
    prshow : boolean;                 {toon voortgang}
    crsok : boolean;                  {mag crs maken}
    polartype : integer;              {polaire coordinaten: 0 = cart
                                        1=pol 2=radar 3-cyl}
    stereo : boolean;                 {stereo aan rood/groen}
    ffac : single;                    {correctiefactor fonts}
    xoff,yoff : integer;              {offset}
    linestyle : integer;              {line style, <0=none, 0=solid, >0 = dash}
    stereocol : array[0..3] of Tcolor; {kleur}
    framexyz : record  X,Y,Z : single; end; {coordinaten van 3D frame}
    colors, revcolors : array[0..255] of Tcolor; {kleurenschaal en reverse}
    sx1,sx2,sy1,sy2 : integer;        {x/y van hoogteschaal}

type symboltype = record
                   xp,yp:word;
                   style,size,fill,width,cd3d : byte;
                   color,cl1,cl2,cl3:Tcolor;
                 end;
var  lastsymbol : symboltype;         {laatst gemelde xysymbol}

     screencopy : Tbitmap;            {kopie van beeldscherm}

var graphfield, plotfield : record x1,x2,y1,y2 : integer; end;

type datapointer = ^Tdatatype;
type data4dpointer = ^T4dtype;

type graftype3d = record                       {3d uitbreidingen van buf}
                  dp : datapointer;            {data tabel}
                  dp4d : data4dpointer;        {4d data tabel}
                  m3d,sf,vl,bmp4d : boolean;   {3d mode, er is een surface, vol, 4d bmp}
                  scl,cnt,isbmp : boolean;     {er is een schaal/contour, cnt is bmp}
                  xs1,xs2,ys1,ys2 : integer;   {scherm pos schaal}
                  xc1,xc2,yc1,yc2 : integer;   {scherm pos contour}
                  z1,z2 : single;              {hoogtes}
                  xf0,xf1,yf0,yf1 : single;    {zoom factoren}
                  hm,drm : integer;            {hoogtemode, driehmode}
                  f3dsize : integer;           {font size ivm 3d text}
                  empt : single;               {leeg veld}
                  hasempt : boolean;           {er is een leegveld}
                  end;
     grafstype3d = array[1..maxgraf] of graftype3d;

var graphs3d : grafstype3d;
    xmid,ymid,zmid : single;      {middens van assen}
    facx,facy,facz : single;      {schaal factoren voor assen}
    ifacx,ifacy,ifacz : single;   {inverse factoren}
    sinp,cosp,sina,cosa : single; {sin en cos van kijkhoek}
    isinp,icosp,isina,icosa : single; {inverse factoren}
    sinr,cosr : single;           {sin en cos van draaiing}
    fac3d, r3d : single;          {reken factoren}
    ifac3d, ir3d : single;        {inverse factoren}
    xpc, xpc0, ypc : integer;     {midden vam beeldscherm}
    pixx3d, pixy3d, pixz3d : single; {aantal pixels voor lengte 1}
    pixok : boolean;              {ok voor abs -> user}
    kijkhoek,kijkhoogte : integer;{-id-}
    viewx,viewy,viewz : single;   {kijkpunt}
    empty : single;               {lege waarde bij surface}
    hasempty : boolean;           {er is een lege waarde}

procedure initxycommon;

implementation

{$I-}

uses printers;

var ff : file;
    textbm : Tbitmap;                {tekst in achtergrond}
    textbuf : array of array of boolean;
    textfac : single;

    ticx, ticy, ticz : single;    {tics in user coord}
    ticx2, ticy2, ticz2 : single; {tics in user coord}
    xmi,xma,ymi,yma : single;     {bereik}

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{XXXXXXXXXXXXXXXXXXXXXXXXX EX COMMON XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

function red(cl:Tcolor):integer;
begin red := colortorgb(cl) shr 0 and $FF; end;

function green(cl:Tcolor):integer;
begin green := colortorgb(cl) shr 8 and $FF; end;

function blue(cl:Tcolor):integer;
begin blue := colortorgb(cl) shr 16 and $FF; end;

function mixcolor(cl1,cl2:Tcolor;f1,f2 : integer):Tcolor;
begin
  if (f1<0) or (f2<0) or (f1+f2=0) then begin mixcolor := clblack; exit; end;
  mixcolor :=
   ( ( red(cl1) * f1 + red(cl2) * f2 ) div (f1+f2) ) and $FF shl 0 +
   ( ( green(cl1) * f1 + green(cl2) * f2 ) div (f1+f2) ) and $FF shl 8 +
   ( ( blue(cl1) * f1 + blue(cl2) * f2 ) div (f1+f2) ) and $FF shl 16;
end;

function reversecolor(cl:integer):integer; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{kleur omkeer RGB -> BGR voor direct mem}
begin
  result := (cl and $FF00)
         or (cl and $FF) shl 16
         or (cl and $FF0000) shr 16 ;
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{maskblt bitblt stretchblt setrop2 getrgnbox selectcliprgn}

procedure transparent(h:Thandle;col:tcolor;edge:boolean); {XXXXXXXXXXXXXXXXXXXX}
var r,g,b,tr,f1,f2,x0,y0,x1,y1,wi,he,x,y,yy,xp,t : integer;
    rectxy, rectbm : Trect;
    bm : Tbitmap;
    p : Pbytearray;
    oy : boolean;
begin
  r := col and $ff; g := col shr 8 and $ff; b := col shr 16 and $ff;

  tr := round(transval*8);
  if (tr<0) then tr := 0 else if (tr>8) then tr := 8;
  f1 := 256*(8-tr); f2 := tr;

  t := getrgnbox(h,rectxy); if (t=0) or (t=nullregion) then exit;
  with rectxy do
    begin x0 := left; y0 := top; x1 := right-1; y1 := bottom-1; end;
  {if edge then begin inc(x0); inc(y0); end;}
  if (x0>cwidth) or (y0>cheight) or (x1<0) or (y1<0) then exit;
  if (x0<0) then x0 := 0; if (x1>cwidth) then x1 := cwidth;
  if (y0<0) then y0 := 0; if (y1>cheight) then y1 := cheight;
  he := y1-y0+1; wi := x1-x0+1;
  if (he<1) or (wi<1) then exit;

  bm := Tbitmap.create; bm.pixelformat := pf24bit;
  bm.width := wi; bm.height := he;
  rectbm := rect(0,0,wi,he); rectxy := rect(x0,y0,x1+1,y1+1);
  xycanvas.copymode := cmSrcCopy; bm.canvas.copymode := cmSrcCopy;
  bm.canvas.CopyRect(rectbm,xycanvas,rectxy);
  {if not bitblt(bm.canvas.handle,0,0,wi,he,xycanvas.handle,x0,y0,srccopy)
    then begin bm.free; exit; end;}

  if (transmode=2) then {echte transparancy}

  for y := 0 to he-1 do
    begin
      p := bm.scanline[y]; yy := y+y0; xp := 0;
      for x := x0 to x0+wi-1 do if ptinregion(h,x,yy) then
        begin
          p[xp] := (b * (f1+p[xp]*f2)) shr 11; inc(xp);
          p[xp] := (g * (f1+p[xp]*f2)) shr 11; inc(xp);
          p[xp] := (r * (f1+p[xp]*f2)) shr 11; inc(xp);
        end else inc(xp,3);
    end

  else {quasi transparancy}

  for y := 0 to he-1 do
    begin
      p := bm.scanline[y]; yy := y+y0; xp := 0; oy := odd(yy);
      for x := x0 to x0+wi-1 do
       if ((odd(x) xor oy) and ptinregion(h,x,yy) ) then
         begin
           p[xp] := b; inc(xp);
           p[xp] := g; inc(xp);
           p[xp] := r; inc(xp);
         end else inc(xp,3);
    end;

  xycanvas.copyrect(rectxy,bm.canvas,rectbm);
  {bitblt(xycanvas.handle,x0,y0,wi,he,bm.canvas.handle,0,0,srccopy);}
  bm.free;
end;

procedure drawtransparent(x,y,r:integer;poly:array of Tpoint); {XXXXXXXXXXXXXXX}
var mode,col1,col2 : integer;
    h : Thandle;
    edge : boolean;
begin
  if length(poly)>1 then mode := 2
  else if (r>0) then mode := 1
  else exit;

  col2 := xybrush.color; col1 := xypen.color; h := 0;
  edge := (col1<>col2) or (transopt and 2 > 0);

  if (dowmf) then {alleen transmode=1}
    begin
      checkwmfpen; checkwmfbrush;
      rop2wmf(r2_maskpen);
      if (mode=2) then polygonwmf(poly)
        else ellipsewmf(x-r,y-r,x+r+1,y+r+1);
      rop2wmf(r2_copypen);
      if edge then
        begin
          xybrush.style := bsclear;
          checkwmfbrush;
          if (mode=2) then polygonwmf(poly)
            else ellipsewmf(x-r,y-r,x+r+1,y+r+1);
          xybrush.style := bssolid;
        end;
      exit;
    end;

  if (transmode>1) then
    begin
      if (mode=2) then h := createpolygonrgn(poly,length(poly),alternate)
        else h := createellipticrgn(x-r,y-r,x+r+1,y+r+1);
      {if h <>null then}
         begin transparent(h,col2,edge); deleteobject(h); end;
    end;

  if (transmode=1) or (h=0) then
    begin
      setrop2(xycanvas.handle,r2_maskpen);
      if (mode=2) then xycanvas.polygon(poly)
       else xycanvas.ellipse(x-r,y-r,x+r+1,y+r+1);
      setrop2(xycanvas.handle,r2_copypen);
    end;

  if edge then
    begin
      xybrush.style := bsclear;
      if (mode=2) then xycanvas.polygon(poly)
       else xycanvas.ellipse(x-r,y-r,x+r+1,y+r+1);
      xybrush.style := bssolid;
    end;
  xybrush.color := col2;
end;

{XXXXXXXXXX USER -> ABS en omgekeerd XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

(*ROUTINE VOOR USER -> ABS
  x := (x-xmid)*facx; y := (y-ymid)*facy; z := (z-zmid)*facz;
  x1 := x*cosp - y*sinp; y1 := x*sinp + y*cosp; {z1 := z}    {draaiing om z-as}
  x2 := x1; y2 := y1*sinh + z*cosh; {z2 = y1*cosh - z1*sinh} {draaiing om as in xy vlak}
 [x3 := x2*cosr - y2*sinr; y3 := x2*sinr + y2*cosr;] {draaiing om z-as bij stereo}
  if (r3d=0) then ff := fac3d else
    ff := fac3d*r3d/(r3d+y1*cosh-z*sinh);              {factor voor perspectief}
  xp := round(xpc+ff*x2); yp := round(ypc-ff*y2);

  INVERSE ROUTINES, R3D = 0:
  voor constante x, y of z: w := (w-wmid)*facw
  x2 := (xp-xpc)/fac3d; y2 := -(yp-ypc)/fac3d;
  x1 := x2; y2 := y1*sinh + z*cosh;
  x1 := x*cosp - y*sinp; y1 := x*sinp + y*cosp;
  oplossen afh van bekende: x, y of z;
  - bereken y2 en x1=x2;
  - als z bekend: bereken y1 uit y2, dan inverse draaiing van x1 en y1 geeft x,y
    (x := x1*cosp + y1*sinp; y := -x1*sinp+ y1*cosp;)
  - als y bekend: bereken x uit x1 en y, bereken y1 uit x en y, bereken z uit y1 en y2
  - als x bekend: bereken y uit x1 en y, bereken y1 uit x en y, bereken z uit y1 en y2
  x := x/facx+xmid; y := y/facy+ymid; z := z/facz+zmid;

  INVERSE ROUTINES, R3D =/= 0:
  voor constante x, y of z: w := (w-wmid)*facw
  x2 :=  (xp-xpc)/fac3d*(1+y1*cosh/r3d-z*sinh/r3d);
  y2 := -(yp-ypc)/fac3d*(1+y1*cosh/r3d-z*sinh/r3d);
  x1 := x2; y2 := y1*sinh + z*cosh;
  --> twee vergelijkingen in x1, y1 en z1=z:  f1(x1,y1,z1)=0 en f2(y1,z1=0)
  - als z=z1 bekend: bereken y1 uit f2, bereken x1 uit f1, inverse draaiing
    (x := x1*cosp + y1*sinp; y := -x1*sinp+ y1*cosp;)
  - als x of y bekend: -> elimineer y of x -> f3(x1,y1)=0
    drie vgl in x1,y1,z1 : los op, bereken y uit x of x uit y
  x1 := x*cosp - y*sinp; y1 := x*sinp + y*cosp;
  oplossen afh van constante: x, y of z;
  x := x/facx+xmid; y := y/facy+ymid; z := z/facz+zmid;

  c1 := -(yp-ypc)/fac3d; c2 := c1/r3d;
  f2: y1*sinh + z*cosh = c1 + c2*y1*cosh - c2*z*sinh
      y1*(sinh-c2*cosh) + z*(cosh+c2*sinh) = c1
  c3 := (xp-xpc)/fac3d;  c4 := c3/r3d;
  f1: x1 = c3 + c4*y1*cosh - c4*z*sinh
      x1 + y1*(-c4*cosh) + z*(c4*sinh) = c3;
  f3 : x1*cosp + y1*sinp = x   {x bekend}
  f3 : x1*sinp - y1*cosp = -y  {y bekend}

  a1.x + b1.y + c1.z = d1 (f1)
    0  + b2.y + c2.z = d2 (f2)
  a3.x + b3.y +  0   = d1 {f3)
  D  = (b1.c2.a3-a1.c2.b3-c1.b2.a3);
  Dx = (b1.c2.d3+c1.d2.b3-d1.c2.b3-c1.b2.d3};
  Dy = (d1.c2.a3-a1.c2.d3-c1.d2.a3);
  Dz = (a1.b2.d3+b1.d2.a3-a1.d2.b3-d1.b2.a3);
  x = Dx/D; y = Dy/D; z = Dz/D;
*)

procedure position0(x,y,z:single;var xp,yp:integer); {XXXXXXXXXXXXXXXXXXXXXXXXX}
var x1,x2,y1,y2,ff,t : single;
begin
  x := (x-xmid)*facx; y := (y-ymid)*facy; z := (z-zmid)*facz;
  x1 := x*cosp - y*sinp; y1 := x*sinp + y*cosp; {z1 := z}    {draaiing om z-as}
  x2 := x1; y2 := y1*sina + z*cosa; {z2 = y1*cosh - z1*sinh} {draaiing om as in xy vlak}
  if stereo then {draaiing om z-as bij stereo}
    begin t := x2; x2 := x2*cosr - y2*sinr; y2 := t*sinr + y2*cosr; end;
  if (r3d=0) then ff := fac3d else
    ff := fac3d*r3d/(r3d+y1*cosa-z*sina);       {factor voor perspectief}
  xp := round(xpc+ff*x2); yp := round(ypc-ff*y2);
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

procedure printpart(x1,y1,x2,y2:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
type regel = array[0..8000] of integer;
     pregel = ^regel;
var x,y,xp,yp,xx,xxp,pw,xp0,yp0 : integer;
    col : Tcolor;
    buf : pregel;
begin
  pw := xypen.width; xypen.width := 1;
  xp0 := prxoff - prfac div 2; yp0 := pryoff - prfac div 2;
  if (not doprint) and prshow then xypen.mode := pmnot;
  for y := y1 to y2 do
    begin
     if not doprint then
       if prshow then with xycanvas do begin moveto(x1,y); lineto(x2,y); end;
     buf := screencopy.scanline[y]; yp := yp0 + y * prfac; x := x1;
     while (x<=x2) do
      begin
       col := buf[x];
       if (col<>clwhite) then
         begin
          xx := x; while(xx<x2) and (buf[xx+1]=col) do inc(xx);
          xp := xp0 + x * prfac; xxp := xp0 + xx * prfac;
          printer.canvas.brush.color := reversecolor(col);
          printer.canvas.pen.color := printer.canvas.brush.color;
          printer.canvas.rectangle(xp,yp,xxp+prfac,yp+prfac);
          x := xx;
         end;
       inc(x);
     end;
   end;
   xypen.mode := pmcopy;
   xypen.width := pw;
end;

procedure copyscreen(mode:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var w,h : integer;
    crect : Trect;
begin
  screencopy := Tbitmap.create;
  w := cwidth; h := cheight;
  with screencopy do
    begin
      if (mode=1) then pixelformat := pf24bit else
      if (mode=2) then pixelformat := pf32bit;
      width := w; height := h;
      crect := rect(0,0,w,h);
      canvas.copymode := cmSrcCopy;
      canvas.copyrect(crect,xycanvas,crect);
    end;
end;

procedure restorescreen; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var crect : Trect;
begin
   with screencopy do crect := rect(0,0,width,height);
   xycanvas.copyrect(crect,screencopy.canvas,crect);
   screencopy.free;
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

procedure inittext3d(f,x1,x2,y1,y2:single;var ok:boolean); {XXXXXXXXXXXXXXXXXXX}
var h,i,ch : integer;
begin
  textfac := f; ok := true;
  with textbm.canvas.font do
    begin
      assign(xycanvas.font);
      color := clblack;
      with graphs3d[igraph] do
      if not doprint then
        begin
          size := round(xycanvas.font.size*textfac);
          f3dsize := size; textfac := size/xycanvas.font.size;
        end
      else
        if (f3dsize=0) then begin ok := false; exit; end
        else size := f3dsize*res;
    end;
  h := textbm.canvas.textheight('0'); textbm.height := h;
  setlength(textbuf,h); for i := 0 to h-1 do setlength(textbuf[i],40);

  ch := xycanvas.textheight('0');
  ticx := ch/pixx3d; ticy := ch/pixy3d; ticz := ch/pixz3d;
  ticx2 := ticx/2;   ticy2 := ticy/2;   ticz2 := ticz/2;
  xmi := x1;  xma := x2; ymi := y1; yma := y2;
end;

procedure showtext3d(s:string;col:Tcolor;x,y,z:single; {XXXXXXXXXXXXXXXXXXXXXXX}
         m1,m2,m3:integer;kp,av,cl:boolean);
  {m1: 0=xy vlak; 1 = xz vlak; 2 = yz vlak}
  {m2: 0=0, 1=90, 2=180, 3=270}
  {m3: just: 0=bo, 1=li, 2=on, 3=re}
  {kp= extra op de kop; av=extra achterstevoren; cl = gedraaid}
var tw,th,tw0,th0,xr,yr,xp,yp,c1,c2 : integer;
    u0,v0,du,dv,uw,vw,u,v : single;
    ipx,ipy,ipz,d : single;
    ipu,ipv,fu,fv,dipu,dipv : single;
    rot : boolean;
const orient : array[0..3] of integer = (1,0,2,3);

procedure setstring(m:integer); {----------------------------------------------}
var i,j,w,h,n,d,w2 : integer;
    p : pbytearray;
    ss : string;
  {m: 0=normaal; 1=op de kop; 2 = achterstevoren; 3 = gedraaid}
begin
  th := textbm.height;
  if cl then
   begin
    d := round(th*0.9); n := length(s); tw := th+(n-1)*d; w := th-1; h := tw-1;
    with textbm do with canvas do
     begin
      width := th; height := tw; rectangle(0,0,th,tw); brush.style := bsclear;
      for i := 1 to n do begin ss := s[i]; w2 := textwidth(ss);
          textout((th-w2) div 2,(i-1)*d,s[i]); end;
      brush.style := bssolid;
    end;
    if (length(textbuf[0])<tw) then for i := 0 to w do setlength(textbuf[i],tw);
    case m of
      0 : for i := 0 to h do begin p := textbm.scanline[i];
        for j := 0 to w do textbuf[j,i] := (p^[w-j]=0); end;
      1 : for i := 0 to h do begin p := textbm.scanline[i];
        for j := 0 to w do textbuf[j,i] := (p^[j]=0); end;
      2 : for i := 0 to h do begin p := textbm.scanline[h-i];
        for j := 0 to w do textbuf[j,i] := (p^[w-j]=0); end;
      3 : for i := 0 to h do begin p := textbm.scanline[h-i];
        for j := 0 to w do textbuf[j,i] := (p^[j]=0); end;
     end;
    textbm.height := th;
   end
  else
   begin
    tw := textbm.canvas.textwidth(s); th := textbm.height; w := tw-1; h := th-1;
    with textbm do with canvas do begin if (width<tw) then width := tw;
      rectangle(0,0,tw,th); textout(0,0,s);end;
    if (length(textbuf[0])<tw) then for i := 0 to h do setlength(textbuf[i],tw);
    case m of
      0 : for i := 0 to h do begin p := textbm.scanline[i];
        for j := 0 to w do textbuf[i,j] := (p^[j]=0); end;
      1 : for i := 0 to h do begin p := textbm.scanline[h-i];
        for j := 0 to w do textbuf[i,j] := (p^[j]=0); end;
      2 : for i := 0 to h do begin p := textbm.scanline[i];
        for j := 0 to w do textbuf[i,j] := (p^[w-j]=0); end;
      3 : for i := 0 to h do begin p := textbm.scanline[h-i];
        for j := 0 to w do textbuf[i,j] := (p^[w-j]=0); end;
     end;
   end;
end;

begin {------------------------------------------------------------------------}
  if (m1<0) or (m1>2) then exit;
  m2 := m2 and 3; m3 := m3 and 3; rot := odd(m2);
  c1 := orient[m2]; if kp then c1 := c1 xor 1; if av then c1 := c1 xor 2;
  setstring(c1); c2 := (m2+m3) and 3;
  if cl then tw0 := round(tw/textfac) else tw0 := xycanvas.textwidth(s);
  th0 := xycanvas.textheight(s);
  ipx := 1/pixx3d; ipy := 1/pixy3d; ipz := 1/pixz3d;
  d := th0/6; if (m3=3) then d := -d;

  case m1 of
   0 {XY}: begin u := x; v := y; ipu := ipx; ipv := ipy; end;
   1 {XZ}: begin u := x; v := z; ipu := ipx; ipv := ipz; end;
   2 {YZ}: begin u := y; v := z; ipu := ipy; ipv := ipz; end;
  end;
  fu := ipu/textfac; fv := ipv/textfac; dipu := d*ipu; dipv := d*ipv;
  if rot then begin du := th0*ipu; dv := tw0*ipv; end
         else begin du := tw0*ipu; dv := th0*ipv; end;
  case c2 of
    0 : begin u0 := u-du/2; v0 := v-dv;   end;
    1 : begin u0 := u;      v0 := v-dv/2; end;
    2 : begin u0 := u-du/2; v0 := v;      end;
    3 : begin u0 := u-du;   v0 := v-dv/2; end;
  end;
  if (m3 in [1,3]) then
     case m2 of 0 : u0 := u0+dipu; 1 : v0 := v0+dipv;
                2 : u0 := u0-dipu; 3 : v0 := v0-dipv; end;

  for xp := 0 to tw-1 do for yp := 0 to th-1 do if textbuf[yp,xp] then
    begin
     if rot then begin uw := u0+yp*fu; vw := v0 + xp*fv; end
            else begin uw := u0+xp*fu; vw := v0 + yp*fv; end;
     case m1 of 0 : position0(uw,vw,z,xr,yr);
                1 : position0(uw,y,vw,xr,yr);
                2 : position0(x,uw,vw,xr,yr);
               end;
     if dowmf then pixelwmf(xr,yr,col) else xycanvas.pixels[xr,yr] := col;
    end;
end;

procedure text3d(s:string;x,y,z:single;m1:integer;lbl:boolean; {XXXXXXXXXXXXXXX}
       oct,ml:integer;zlvert:boolean);
  {m1 :  1-4  = X-as, 5-8  = Y-as, 9-12 = Z-as, 13-16 = hoeken}
var x2,y2,z2,d : single;
    kp,av,cl : boolean;
    vl,ri,ju : integer;
    xp1,xp2,yp1,yp2:integer;
begin
  if lbl and (m1<13) then if odd(m1) then ml := -1 else dec(m1);
  kp := false; av := false; x2 := x; y2 := y; z2 := z; ju := 0; cl := false;
  if lbl and (m1 in [9..12]) then cl := zlvert;
  case m1 of
   1 : begin
      kp := (kijkhoogte<0); vl := 0;
      if not lbl then d := ticy else if (ml<0) then d := ticy*2
        else d := ticy + ml*ticy2;
      if (y<ymid) then d := -d; y2 := y+d;
      if (y>ymid) then ri := 2 else ri := 0;
    end;
   2 : begin
      kp := (kijkhoogte<0); vl := 0;
      if oct in [1,2,3,4] then ri := 3 else ri := 1;
      if oct in [3,4,7,8] then ju := 3 else ju := 1;
      if (y<ymid) then y2 := y - ticy else y2 := y + ticy;
    end;
   3 : begin
      if (viewy>ymi) and (viewy<yma) then exit;
      if not lbl then d := ticz else if (ml<0) then d := ticz*2
        else d := ticz + ml*ticz2;
      if (kijkhoogte<0) then begin d := -d; ju := 2; end;
      z2 := z-d; ri := 0; vl := 1; av := (y>ymid);
    end;
   4: begin
      if (viewy>ymi) and (viewy<yma) then exit;
      if oct in [1,2,5,6] then ri := 3 else ri := 1;
      ju := 4-ri; kp := (oct in [3,4,5,6]);
      if (kijkhoogte<0) then z2 := z + ticz else z2 := z - ticz;
      vl := 1; if (kijkhoogte<0) then ri := (ri + 2) and 3;
    end;

  5 : begin
      kp := (kijkhoogte<0); vl := 0;
      if not lbl then d := ticx else if (ml<0) then d := ticx*2
        else d := ticx + ml*ticx2;
      if (x<xmid) then d := -d; x2 := x+d;
      if (x>xmid) then ri := 1 else ri := 3;
    end;
   6 : begin
      kp := (kijkhoogte<0); vl := 0;
      if (oct in [3,4,5,6]) then ri := 2 else ri := 0;
      if (oct in [1,2,5,6]) then ju := 3 else ju := 1;
      if (x<xmid) then x2 := x - ticx else x2 := x + ticx;
    end;
  7 : begin
      if (viewx>xmi) and (viewx<xma) then exit;
      if not lbl then d := ticz else if (ml<0) then d := ticz*2
        else d := ticz + ml*ticz2;
      if (kijkhoogte<0) then begin d := -d; ju := 2; end;
      z2 := z-d; ri := 0; vl := 2; av := (x<xmid);
    end;
   8: begin
      if (viewx>xmi) and (viewx<xma) then exit;
      if (oct in [1,2,7,8]) then ri := 3 else ri := 1;
      ju := 4-ri; av := (oct in [1,2,3,4]);
      if (kijkhoogte<0) then z2 := z + ticz else z2 := z - ticz;
      vl := 2; if (kijkhoogte<0) then ri := (ri + 2) and 3;
    end;

  9,11:  begin
      { 1,4,5,8 = aan y-as; 1,2,4,7 = aan neg.zijde}
      if oct in [1,4,5,8] then
        if not lbl then d := ticx2 else if (ml<0) then d := ticx2+ticx
              else d := ticx2 + ml*ticx2
      else
        if not lbl then d := ticy2 else if (ml<0) then d := ticy2+ticy
              else d := ticy2 + ml*ticy2;
      if (oct in [1,2,4,7]) xor (m1=11) then begin d := -d; ri := 3; end else ri := 1;
      if oct in [1,4,5,8] then begin x2 := x+d; vl := 1; end
                          else begin y2 := y+d; vl := 2; end;
      av := oct in [2,3,4,5];
      if (kijkhoogte<0) and not cl then begin kp := true; av := not av; end;
      if cl then if oct in [2,4,6,8] then
        begin kp := true; av := not av; end;
      if (m1=11) and cl then begin kp := not kp; av := not av; end;
    end;
  10,12:  begin
      if oct in [1,4,5,8] then d := ticx2 else d := ticy2;
      ri := 0;
      if (oct in [1,2,4,7]) xor (m1=12) then
           begin d := -d; ju := 3; end else ju := 1;
      if oct in [1,4,5,8] then
        begin x2 := x+d; vl := 1; end else begin y2 := y+d; vl := 2; end;
      av := oct in [2,3,4,5];
    end;

  13,15 : begin
         if (x=0) then begin vl := 1; av := (oct in [3,4,5,6]); end
                  else begin vl := 2; av := (oct in [1,2,3,4]); end;
         if (kijkhoogte<0) then begin ju := 0; z2 := z-ticz2; end
          else begin ju := 2; z2 := z+ticz2; end;
       end;
  14,16 : begin
         if (x=0) then
            begin vl := 1; kp := (oct in [3,4,5,6]);
            if oct in [1,2,5,6] then ri := 3 else ri := 1; end
         else
            begin vl := 2; kp := (oct in [1,2,3,4]);
            if oct in [1,2,5,6] then ri := 1 else ri := 3; end;
         if (kijkhoogte<0) then z2 := z-ticz2 else z2 := z+ticz2;
         ju := ri; if (kijkhoogte<0) then ri := (ri + 2) and 3;
         end;

    end;
  if not lbl then
    begin
      position0(x, y, z, xp1,yp1);
      position0(x2,y2,z2,xp2,yp2);
      xyline(-1,xp1,yp1,xp2,yp2);
    end;
  showtext3d(s,xyfont.color,x2,y2,z2,vl,ri,ju,kp,av,cl);
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{XXXXXXXXXXXXXXXXXXXX EX XYGRAF XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

procedure store; begin oldpen.assign(xypen); oldbrush.assign(xybrush); end;
procedure restore; begin xypen.assign(oldpen); xybrush.assign(oldbrush); end;

procedure setcursor(xp,yp:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  {xp := (xp*res)+xoff; yp := (yp*res)+yoff;}
  if dowmf then movewmf(xp,yp) else xycanvas.moveto(xp,yp);
end;

procedure setorigin(x,y:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  with xycanvas do
    begin
      unrealizeobject(brush.handle);
      setbrushorgex(handle,x,y,nil);
      selectobject(handle,brush.handle);
    end;
end;

procedure xypixel(xp,yp:integer;col:Tcolor); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var df1,df2 : integer;
begin
  if (res=1) then
    if dowmf then pixelwmf(xp,yp,col)
         else xycanvas.pixels[xp,yp] := col
  else
    begin
      store; xypen.width := 1; xybrush.style := bssolid;
      df1 := res div 2; df2 := res-df1;
      xyrectangle(col,col,xp-df1,yp-df1,xp+df2,yp+df2);
      restore;
    end;
end;

procedure xyrectangle(col1,col2,x1,y1,x2,y2:integer); {XXXXXXXXXXXXXXXXXXXXXXXX}
begin
  if (col2>=0) then
    begin
     xybrush.color := col2;
     if (col1<0) then xypen.color := col2 else xypen.color := col1;
    end;
  if dowmf then
    begin checkwmfpen; checkwmfbrush; blokwmf(x1,y1,x2,y2); end
  else xycanvas.rectangle(x1,y1,x2,y2);
end;

procedure xypolygon(col1,col2:integer;poly:array of Tpoint); {XXXXXXXXXXXXXXXXX}
begin
  if (col2>=0) then
    begin
     xybrush.color := col2;
     if (col1<0) then xypen.color := col2 else xypen.color := col1;
    end;
  if dowmf then
    begin checkwmfpen; checkwmfbrush; polygonwmf(poly); end
  else xycanvas.polygon(poly);
end;

procedure transpolygon(col1,col2:integer;poly:array of Tpoint); {XXXXXXXXXXXXXX}
begin
  if (col2>=0) then
    begin
     xybrush.color := col2;
     if (col1<0) then xypen.color := col2 else xypen.color := col1;
    end;
  drawtransparent(0,0,0,poly);
end;

procedure xycircle(col1,col2,x,y,r:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  if (col2>=0) then
    begin
     xybrush.color := col2;
     if (col1<0) then xypen.color := col2 else xypen.color := col1;
    end
  else if (col1>=0) then xypen.color := col1;
  if dowmf then
    begin checkwmfpen; checkwmfbrush; ellipsewmf(x-r,y-r,x+r+1,y+r+1); end
  else xycanvas.ellipse(x-r,y-r,x+r+1,y+r+1);
end;

procedure transcircle(col1,col2,x,y,r:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  if (col2>=0) then
    begin
     xybrush.color := col2;
     if (col1<0) then xypen.color := col2 else xypen.color := col1;
    end;
  drawtransparent(x,y,r,[]);
end;

procedure xyline(col,x1,y1,x2,y2:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  if (col>=0) then xypen.color := col;
  if dowmf then begin checkwmfpen; linewmf(x1,y1,x2,y2); end
  else begin xycanvas.moveto(x1,y1); xycanvas.lineto(x2,y2); end;
end;
(*
Procedure CanvasTextOutAngle(C:TCanvas; X,Y:Integer; Angle:Word; S:String);
Var
  LogRec: TLOGFONT;
  OldFontHandle,
  NewFontHandle: HFONT;
Begin
  GetObject(C.Font.Handle,SizeOf(LogRec),Addr(LogRec));
  LogRec.lfEscapement:=Angle*10; {10th of a degree}
  LogRec.lfOrientation:=Angle*10;
  NewFontHandle := CreateFontIndirect(LogRec);
  OldFontHandle := SelectObject(C.Handle,NewFontHandle);
  C.Brush.Style := bsClear;
  C.TextOut(X,Y,S);
  NewFontHandle := SelectObject(C.Handle,OldFontHandle);
  DeleteObject(NewFontHandle);
End; *)

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{XXXXXXXXXXXXXXXXXXXXXXXXX EX WMF XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

const buflen = 64;

var fn : file;
    buf : array[1..buflen] of word;
    bufd : array[1..buflen div 2] of cardinal absolute buf;
    flsize, maxlen, nobj, ppx, ppy : integer;
    fnaam : string;

    ok : boolean;

procedure dosize(n:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  inc(flsize,n);
  if (n>maxlen) then maxlen := n;
end;

procedure dorecord;
begin
  blockwrite(fn,buf,bufd[1]);
  dosize(bufd[1]); ppx := -1;
end;

procedure fillintext(s:string; pos:integer; nul:boolean); {WWWWWWWWWWWWWWWWWWWW}
var i : integer;
begin
  if (s='') then s := ' ';
  if nul then s := s + #0;
  if odd(length(s)) then s := s + #32;
  for i := 1 to length(s) div 2 do
    begin
      buf[pos] := ord(s[i*2-1]) + ord(s[i*2]) shl 8;
      bufd[1] := pos;
      inc(pos);
    end;
end;

procedure fillincolor(color:Tcolor; pos:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  buf[pos] := red(color) + green(color) shl 8;
  buf[pos+1] := blue(color) + 2 shl 8;
  buf[pos+2] := 0;
end;

procedure startwmf(var s:string;var ok : boolean); {WWWWWWWWWWWWWWWWWWWWWWWWWWW}
var i : integer;
begin
  s := changefileext(s,'.wmf');
  fnaam := s;
  assignfile(fn,s); rewrite(fn,2);
  ok := (ioresult=0);  
  if ok then
   begin
     for i := 1  to 20 do buf[i] := 0;
     blockwrite(fn,buf,11+9);
     flsize := 20; maxlen := 0; nobj := 5;
    end;
end;

procedure bitmap(name:string; xp,yp:integer; fac:single); {WWWWWWWWWWWWWWWWWWWW}
var width,height : integer;
    p1,p2,t,len : integer;
begin
  assignfile(ff,name); reset(ff,2); if (ioresult<>0) then exit;
  seek(ff,8); blockread(ff,buf,4); width := buf[2]; height := buf[4];
  seek(ff,7);

(*  bufd[1] := 4; buf[3] := $0107; buf[4] := 3; dorecord; {SetStretchBltMode} *)

  p1 := filepos(fn);
  bufd[1] := 00; buf[3] := $0F43;  {StretchDIBits}
  buf[4] := 32; buf[5] := 204; buf[6] := 0; {standaard header?}
  buf[7] := height; buf[8] := width;
  buf[9] := 0; buf[10] := 0;
  if (fac<1.5) then
    begin buf[11] := round(height*fac); buf[12] := round(width*fac); end
  else
  begin buf[11] := round(height*fac-fac/2); buf[12] := round(width*fac-fac/2); end;
  buf[13] := yp; buf[14] := xp;
  blockwrite(fn,buf,14);

  repeat blockread(ff,buf,buflen,t); blockwrite(fn,buf,t) until (t<buflen);

  p2 := filepos(fn); len := (p2-p1);
  seek(fn,p1); bufd[1] := len; blockwrite(fn,buf,2); seek(fn,p2);
  inc(flsize,len); if (len>maxlen) then maxlen := len;

  closefile(ff);
end;

procedure finishwmf(x,y,inch:integer;var ok:boolean); {WWWWWWWWWWWWWWWWWWWWWWWW}
var csum,i  : integer;
begin
  bufd[1] := 3; buf[3] := 0; dorecord;  {EOF}

  bufd[1] := $9AC6CDD7;    {Aldus header}
  buf[3] := 0;
  buf[4] := 0; buf[5] := 0;
  buf[6] := x; buf[7] := y;
  buf[8] := inch;
  buf[9] := 0; buf[10] := 0;
  csum := 0; for i := 1 to 10 do csum := csum xor buf[i];
  buf[11] := csum;
  seek(fn,0); blockwrite(fn,buf,11);

  buf[1] := 1;             {standard header}
  buf[2] := 9;
  buf[3] := $300;
  buf[4] := flsize and 65535;  buf[5] := flsize shr 16;
  buf[6] := nobj;
  buf[7] := maxlen and 65535; buf[8] := maxlen shr 16;
  buf[9] := 0;
  blockwrite(fn,buf,9);
  closefile(fn);

  ok := fileexists(fnaam);
  if ok then
    begin
      assignfile(fn,fnaam); reset(fn,1);
      ok := (filesize(fn)=flsize*2); closefile(fn);
    end;
end;

procedure setsize(x,y:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 4; buf[3] := $0103; buf[4] := 8; dorecord; {Mapmode};
  bufd[1] := 5; buf[3] := $020B; buf[4] := 0; buf[5] := 0; dorecord; {Windoworg}
  bufd[1] := 5; buf[3] := $020C; buf[4] := y; buf[5] := x; dorecord; {Windowext}
end;

procedure setemptybrush; {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 7; buf[3] := $02FC; buf[4] := 1;
  buf[5] := 0; buf[6] := 0; buf[7] := 0;
  dorecord; {createbrushindirect}
end;

procedure setbrush(style:integer;color:Tcolor); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 7; buf[3] := $02FC; buf[4] := style;
  fillincolor(color,5); dorecord; {createbrushindirect}
end;

procedure textcolor(color:Tcolor); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 5; buf[3] := $0209;
  fillincolor(color,4); dorecord; {settextcolor}
end;

procedure setfont(he,wi,an:integer;font:Tfont); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
var s : string;
begin
  bufd[1] := 0; buf[3] := $02FB;
  buf[4] := word(he); buf[5] := wi;
  buf[6] := an*10; buf[7] := an*10;
  if fsbold in font.style then buf[8] := 700 else buf[8] := 400;
  buf[9] := 0; if fsitalic in font.style then buf[9] := 1;
  if fsunderline in font.style then buf[9] := buf[9] + 256;
  buf[10] := 0; if fsstrikeout in font.style then buf[10] := 1;
  buf[10] := buf[10] + 1*256;
  buf[11] := 0 + 0*256;
  buf[12] := 0 + 0*256;
  s := font.name; fillintext(s,13,true);
  dorecord; {createfontindirect}

  textcolor(font.color);
end;

procedure extrachar(n:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 4; buf[3] := $0108; buf[4] := n; dorecord; {settextcharextra}
end;

procedure textalign(n:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 4; buf[3] := $012E; buf[4] := n; dorecord; {settextalign}
  (*   0   6   2
      24  30  26
       8  14  10 *)
end;

procedure textout(x,y:integer;txt:string); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 0; buf[3] := $0521; buf[4] := length(txt);
  fillintext(txt,5,false); inc(bufd[1],2);
  buf[buf[1]-1] := y; buf[bufd[1]] := x; dorecord; {textout}

  exit;
  bufd[1] := 0; buf[3] := $0A32; buf[4] := y; buf[5] := x;
  buf[6] := length(txt); buf[7] := 0; fillintext(txt,8,false);
  dorecord; {exttextout}
end;

procedure setpen(style,wi:integer; color:Tcolor); {WWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 9; buf[3] := $02FA; buf[4] := style; buf[5] := wi; buf[6] := 0;
  fillincolor(color,7); dorecord; {createpenindirect}
end;

procedure selectobject(n:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 4; buf[3] := $012D; buf[4] := n; dorecord; {selectobject}
end;

procedure deleteobject(n:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 4; buf[3] := $01F0; buf[4] := n; dorecord; {deleteobject}
end;

procedure blokwmf(x1,y1,x2,y2:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 7; buf[3] := $041B;
  buf[4] := y2; buf[5] := x2; buf[6] := y1; buf[7] := x1;
  dorecord; {rectangle};
end;

procedure ellipse(x1,y1,x2,y2:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 7; buf[3] := $0418;
  buf[4] := y2; buf[5] := x2; buf[6] := y1; buf[7] := x1;
  dorecord; {ellipse};
end;

procedure bkmode(n:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 4; buf[3] := $0102; buf[4] := n; dorecord; {setbkmode}
end;

procedure rop2(n:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 4; buf[3] := $0104; buf[4] := n; dorecord; {setrop2}
end;

procedure comment(txt:string); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 0; buf[3] := $0626; buf[4] := 15;
  buf[5] := length(txt); fillintext(txt,6,false);
  dorecord; {escape};
end;

procedure movewmf(x,y:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  if (ppx=x) and (ppy=y) then exit;
  bufd[1] := 5; buf[3] := $0214; buf[4] := y; buf[5] := x;
  dorecord; {moveto};
  ppx := x; ppy := y;
end;

procedure drawwmf(x,y:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  if (ppx=x) and (ppy=y) then exit;
  bufd[1] := 5; buf[3] := $0213; buf[4] := y; buf[5] := x;
  dorecord; {lineto};
  ppx := x; ppy := y;
end;

procedure pixel(x,y:integer;color:Tcolor); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 7; buf[3] := $041F; fillincolor(color,4);
  buf[6] := y; buf[7] := x; dorecord; {setpixel};
end;

procedure polygon(points: array of Tpoint); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
var i,n : integer;
begin
  n := length(points);
  bufd[1] := n*2+4; buf[3] := $0324; buf[4] := n;
  for i := 1 to n do
    begin
      buf[i*2+3] := points[i-1].x;
      buf[i*2+4] := points[i-1].y;
    end;
  dorecord; {polygon};
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{XXXXXXXXXXXXXXXXXXXXXXXXX EX XYWMF XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

var penwidth : integer;
    penstyle : integer;
    pencolor,redcolor,grncolor : Tcolor;
    fontcolor : Tcolor;
    fontsize : integer;
    fontstyle: Tfontstyles;
    fontangle : integer;
    brushcolor : Tcolor;
    brushstyle : integer;

    cgrid,fgrid : array of word;
    ncgrid,nfgrid : integer;

procedure initgrid; {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  setlength(cgrid,0); setlength(fgrid,0);
  ncgrid := 0; nfgrid := 0;
end;

procedure addcoarse(p:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  inc(ncgrid); setlength(cgrid,ncgrid);
  cgrid[ncgrid-1] := p;
end;

procedure addfine(p:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  inc(nfgrid); setlength(fgrid,nfgrid);
  fgrid[nfgrid-1] := p;
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

procedure checkwmfpen; {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  if stereo then exit;
  with xycanvas.pen do
    if (color<>pencolor) or (integer(style)<>penstyle) or (width<>penwidth) then
      begin
        deleteobject(1);
        setpen(integer(style),width,color);
        selectobject(1);
        pencolor := color;
        penstyle := integer(style);
        penwidth := width;
      end;
end;

procedure checkwmfrgpen; {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  with xycanvas.pen do
    if (redcolor<>stereocol[1]) or (grncolor<>stereocol[2])
     or (integer(style)<>penstyle) or (width<>penwidth) then
      begin
        deleteobject(4);
        setpen(integer(style),width,stereocol[2]);
        selectobject(4);
        deleteobject(3);
        setpen(integer(style),width,stereocol[1]);
        selectobject(3);
        redcolor := stereocol[1]; grncolor := stereocol[2];
        penstyle := integer(style);
        penwidth := width;
      end
   else selectobject(3);
end;

procedure wmfredpen; begin selectobject(3); end;
procedure wmfgrnpen; begin selectobject(4); end;

procedure fontwmf; {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
var h,w,e : integer;
begin
  with xycanvas.font do
    begin
      h := height;
      if name = 'Small Fonts' then
         begin w := 0; e := size div 3; if fsbold in style then inc(e); end
      else
      if name = 'Arial' then
        begin w := round(xycanvas.textwidth('0')*0.85); e := 0; end
      else
      {if name = 'MS Sans Serif' then}
        begin w := 0; e := size div 3; if fsbold in style then inc(e); end;
    end;
  setfont(h,w,xyfontangle,xycanvas.font);
  extrachar(e);
end;

procedure checkwmffont; {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  with xycanvas.font do
    if (style<>fontstyle) or (size<>fontsize) or (xyfontangle<>fontangle) then
      begin
        deleteobject(2);
        fontwmf;
        selectobject(2);
        fontcolor := color;
        fontstyle := style;
        fontsize := size;
        fontangle := xyfontangle;
      end
    else if (color<>fontcolor) then
      begin
        textcolor(color);
        fontcolor := color;
      end;
end;

procedure checkwmfbrush; {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  with xycanvas.brush do
    if (color<>brushcolor) or (integer(style)<>brushstyle) then
      begin
        deleteobject(0);
        setbrush(integer(style),color);
        selectobject(0);
        brushcolor := color;
        brushstyle := integer(style);
      end;
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

procedure openwmf(var s:string;var ok:boolean); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  if (s='') then s := 'xygraf.wmf';
  startwmf(s,ok);
  dowmf := ok; ppx := -1;
end;

procedure initwmf; {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
var x,y : integer;
begin
  x := xypaintbox.width*res; y := xypaintbox.height*res;
  redcolor := $0000ff; grncolor := $00ff00;
  comment('File created by XYGRAPH');
  comment('(C) Wilko C Emmens 2002');
  setsize(x,y);
  setbrush(0,backcolor);
    brushcolor := backcolor; brushstyle := 0;
  selectobject(0);
  setpen(0,1,frontcolor);
    pencolor := frontcolor; penstyle := 0; penwidth := 1;
  selectobject(1);
  fontwmf;
    fontsize := xycanvas.font.size;
    fontcolor := xycanvas.font.color;
    fontstyle := xycanvas.font.style;
  selectobject(2);
  setpen(0,1,redcolor); selectobject(3);
  setpen(0,1,grncolor); selectobject(4);
  selectobject(1);
  bkmode(1);
end;

procedure pixelwmf(x,y:integer; col:Tcolor); begin pixel(x,y,col); end;
procedure linewmf(x1,y1,x2,y2:integer); begin movewmf(x1,y1); drawwmf(x2,y2); end;

procedure textwmfalign(n:integer); begin textalign(n); end;
procedure textwmfout(x,y:integer;s:string); begin textout(x,y,s); end;

procedure ellipsewmf(x1,y1,x2,y2:integer); begin ellipse(x1,y1,x2,y2); end;

procedure rop2wmf(n:integer); begin rop2(n); end;

procedure polygonwmf(points: array of Tpoint);
begin polygon(points); end;

procedure closewmf(var ok:boolean); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
var x,y : integer;
begin
  x := xypaintbox.width*res; y := xypaintbox.height*res;
  finishwmf(x,y,screen.pixelsperinch,ok);
  dowmf := false;
end;

procedure dogrid(p1,p2:integer;xas:boolean); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
var i : integer;
    col : Tcolor;
begin
  xycanvas.pen.style := psdot; col := xycanvas.pen.color;
  if (nfgrid>0) then
    begin
      xycanvas.pen.color := mixcolor(col,backcolor,1,3);
      checkwmfpen;
      for i := 0 to nfgrid-1 do
        if xas then linewmf(fgrid[i],p1,fgrid[i],p2)
               else linewmf(p1,fgrid[i],p2,fgrid[i]);
    end;
  if (ncgrid>0) then
    begin
      xycanvas.pen.color := col;
      checkwmfpen;
      for i := 0 to ncgrid-1 do
        if xas then linewmf(cgrid[i],p1,cgrid[i],p2)
               else linewmf(p1,cgrid[i],p2,cgrid[i]);
    end;
  xycanvas.pen.style := pssolid;
  xycanvas.pen.color := col;
end;

procedure bitmapwmf(x1,y1,x2,y2:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
var bmp : Tbitmap;
    crect1,crect2 : Trect;
    ok : boolean;
    s : string;
begin
  s := 'tempfile.bmp';
  screen.cursor := crhourglass;
  bmp := Tbitmap.create; ok := true;
  with bmp do
    begin
      pixelformat := screencopy.pixelformat;
      width := x2-x1+1; height := y2-y1+1;
      crect1 := rect(x1,y1,x2+1,y2+1);
      crect2 := rect(0,0,x2-x1+1,y2-y1+1);
      canvas.copymode := cmSrcCopy;
      canvas.copyrect(crect2,screencopy.canvas,crect1);
      try    savetofile(s);
      except ok := false;
      end;
      free;
    end;
  if ok then
    begin
      bitmap(s,x1*res,y1*res,res);
      deletefile(s);
    end;
  screen.cursor := crdefault;
end;

procedure initxycommon; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  xyfontangle := 0;
  textbm := Tbitmap.create;
  with textbm do
    begin
      pixelformat := pf8bit;
      canvas.pen.color := clwhite;
      canvas.brush.color := clwhite;
      canvas.font.color := clblack;
    end;
end;

end.
