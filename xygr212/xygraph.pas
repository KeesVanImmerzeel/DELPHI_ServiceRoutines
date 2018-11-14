unit xygraph; // version 2.1.2

{=========================================================================}
{                                                                         }
{ The sourcefiles XYGRAPH.PAS, XYGRAPH3D.PAS and XYCOMMON.PAS are         }
{ supplied as freeware. As such they are subjected to copyright by        }
{ international law.                                                      }
{                                                                         }
{ The source files are intended only to make your own DCU files.          }
{ Distribution or publication of XYGRAPH.PAS, XYGRAPH3D.PAS and/or        }
{ XYCOMMON.PAS completely or partly is not allowed, unless with specific  }
{ permission of the author.                                               }
{                                                                         }
{ Use of XYGRAPH, XYGRAPH3D and/or XYCOMMON in commercial software is     }
{ only allowed if the files have not been changed and my name is          }
{ mentioned as the author.                                                }
{                                                                         }
{ For all other matters the normal copyright restrictions act.            }
{                                                                         }
{ Wilko C Emmens, Castricum, The Netherlands, 2002                        }
{ mailto:wcemmens@solcon.nl                                               }
{ http://www.solcon.nl/wcemmens                                           }
{                                                                         }
{ Please contact the author for proposed modifications or improvements    }
{ or any other matter.                                                    }
{                                                                         }
{=========================================================================}

(* >> New in version 2.1.2:
   xyxaxis : text at tics
   xytitle : main title
   xy3dloadsurface : accepts missing data for xy3dshowsurface and xy3dshowcontour
   xy3dshowcontour : export of scale settings
   XY3DSetLabels : improved routine with 3D text that REPLACES the 2.1.1 version,
     the latter will NOT be retained

   >> New in version 2.1.1:
   XYPlotArray : facilitates plotting of data arrays and enables data tracking
   XYExportD : Record for retrieving data triggerd by mouse move
   xyinitruler : extended options
   XY3DSetLabels : sets labels for 3D graphs and modes for plotting them
   XYSetDataArray replaces XY3DSetDataArray
   XY3DShowContour : log z-scale

   >> New in version 2.1:
   XYPolarGraph : defines polar co-ordinates
   XYRadarDraw : plots a series of points in a radar graph
   XYRadarGraph : starts plotting in a radar graph
   XYSegBar : plot segmented bars, possibly outlined
   XYSetMonthNames : inputs month names for use in time axis
   XYTimeAxis : creates X-axis with a time scale that accepts TdateTime as an input
   XY3DCircle : plots a circle or polygon in arbitrary orientation
   XY3DCylinder : plots a cylinder, cone or bar in arbitrary orientation
   XY3DCylFrame : defines a cylindrical frame set and sets user co-ordinates
   XY3DSetStereo : starts red/green stereo plotting
   XY3DSpherFrame : defines a spherical frame set and sets user co-ordinates
   xy3dshowsurface : extended options;

   >> New in version 2.0: all 3D plotting, and:
   XYTextAngle : plot text at an arbitrary angle
   XYClearBuffer : clears the buffers with graph settings
   Y-axis: new types 5 and 6 with sideways text
   XYSaveAsmetafile2 : replaced by extended XYSaveAsMetafile

   >> New in version 1.3:
   XYCanvasGraph : initiates plotting on a general cancvas
   XYErrorBar :  plots error bars and related symbols
   XYLineWidth : sets linewidth (for high-res plotting)
   XYSaveAsBitmap : saves graph to disk as a bitmap file
   XYSaveAsMetafile2 : saves as metafile in high-resolution
   XYSetLineStyle : linestyle to draw dashed lines, also for width >1
   XYPutBuffer, XYGetBuffer : stores and retrieves settings for multiple paintboxes
   XYPrint2 : prints graph in high-resolution

   XYExportA, XYExportB, XYExportC : records for retrieving data
   XYPen, XYBrush, XYFont : copies of canvas pen, brush and font for high-res
   all axes plotting : invisible plotting in backgr. colour now will show gridlines
   XYLegendEntry : an errorbar-type entry has been added

   >> New in version 1.2:
   XYDrawFunction : simple routine to plot the result of functions
   XYPrint : direct printing of the graph on the default printer
   XYSetRatio : plot in fixed Y/X ratio
   XYSetReduction : set data reduction in graph based on visual appearance
   ruler function : relative co-ordinates implemented
   X and Y-axis plotting : improved routine to avoid double tics;
     smallest increment handling improved (see helpfile)
*)

interface

{.$D '>>>  XYGRAPH 2.1  (C) WILKO C EMMENS 2002   <<<'}

uses Windows , {Messages,} System.SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    {StdCtrls, Spin,} ExtCtrls, Clipbrd{, AppEvnts, xycommon};

const lin = false; log = true;
      fixed = true; sci = false;
      gridon = true; gridoff = false;
      clipon = false; clipoff = true;
      msgon = true; msgoff = false;
      frameon = true; frameoff = false;
      absl = true; rel = false;
      pix = true; userc = false;
      normal = false; reverse = true;
      straight = false; angle = true;

var   xypixx,xypixy : single;              {afmeting van 1 pixel}
      xycharwidth, xycharheight : integer; {karakter afmetingen}
      xyzoomx, xyzoomy : single;           {zoomfactoren}
      xylegendcount : integer;             {aantal entries}
      xylabels : array[0..4] of string;    {labels bij assen}

      xypen : Tpen; xybrush : Tbrush; xyfont: Tfont; {copies}

type Tbuftype = record                         {el. grafiek data}
                 lbl,ngr,cw,ch : integer;      {pb.label, aantal graphs, afm}
                 cvm,bmp,str,b4d : boolean;    {canvas mode, bitmap, stereo, bm 4d}
                 form : string;                {active form}
                end;
var xybuffers : array of Tbuftype;

type Texporttype = record                      {export record}
                   lbl,igr,xp,yp:integer;      {pb.label, graph nr, x/y pos}
                   xw : double;                {x-waarde}
                   yw : array[1..4] of single; {y-waardes}
                   xn,yn : integer;            {x/y nummer punt}
                  end;
var xyexporta, xyexportb, xyexportc, xyexportd : Texporttype; {vier export records}

type Taxistype = record                        {waardes en instellingen van assen}
                on : boolean;
                min,max : double;
                log : boolean;
                end;
type Tgraphtype = record                       {instelingen hele grafiek}
                 ok : boolean;                 {ok}
                 x1,y1,x2,y2 : integer;        {plotveld in abs waarden}
                 xaxis : Taxistype;            {asinstellingen}
                 yaxis : array[1..4] of Taxistype;
                end;

const maxgraf = 8;
type Tgraphdata = array[1..maxgraf] of Tgraphtype;
var xygraphdata : Tgraphdata;

type Tsimplefunctiontype = function(x:single):single;

type Tradar = record txt:string; mi,ma:single; end;

type Tbardata = record ypos:single; col:Tcolor; end;

type Tmonthnames = array[1..12] of string;

type Tdatatype = array of array of single;

type T4dtype = array of array of array of single;

type Tshowfunction = function(nx,ny:integer):string;

procedure xylinewidth(w:integer);
procedure xylanguage(n:integer);
procedure xyabstouser(xp,yp:integer; var x,y:single); overload;
procedure xyabstouser(xp,yp:integer; var x,y:double); overload;
procedure xyabstouser(xp,yp:integer; var x:double;var y:single); overload;
procedure xyusertoabs(x,y:double; var xp,yp:integer);

procedure xyunzoomx(n:integer);
procedure xyunzoomy(n:integer);
procedure xyunzoom(n:integer); overload;
procedure xyunzoom; overload;
procedure xysetzoom(n:integer; fx0,fx1:double; fy0,fy1:single);

procedure xycanvasgraph(canvas:Tcanvas;width,height:integer;
   bcolor,fcolor:Tcolor; scale:single; clear:boolean);
procedure xycleargraph(paintbox:Tpaintbox; bcolor,fcolor:Tcolor; scale:single);
procedure xystartgraph(x1,x2,y1,y2,dx1,dx2,dy1,dy2:integer; nclip:boolean);
procedure xysetratio(r:single);
procedure xysetreduction(xr,yr:integer);
procedure xysetgridlines(xcoarse,xfine,ycoarse,yfine:integer);
procedure xytitle(cl:Tcolor;txt:string);
procedure xyxaxis(cl:Tcolor;x1,x2,incr,zmx:single;txt:string;grid,log,fix:boolean);
procedure xyyaxis(cl:Tcolor;y1,y2,incr,zmx:single;txt:string;code:integer;grid,log,fix:boolean);
procedure xyyearaxis(cl:Tcolor;code,jaar:integer;grid:boolean);
procedure xysetmonthnames(mnames:Tmonthnames);
procedure xytimeaxis(cl:Tcolor;x1,x2:Tdatetime;txt:string;mode,dtcode,dcode,mcode,ycode:integer; grid:boolean; var tmode:boolean);
procedure xylabelaxis(cl:Tcolor;labels:array of string;grid:boolean);
procedure xysetlinestyle(ls,sp:integer;sl:single);
procedure xymove(x,y:double);
procedure xydraw(x,y:double);
procedure xysymbol(st,sz,fl:integer);
procedure xysymbolp(st,sz,fl:integer;x,y:double);
procedure xytextangle(cl:Tcolor;txt:string;x,y:double;xjust,yjust,mode,angle:integer);
procedure xytext(cl:Tcolor;txt:string;x,y:double;xjust,yjust,mode:integer);
procedure xyfinish;
procedure xyerrorbar(x0,y0:double;w1,w2,w3,h1,h2,h3,h4:single;abs,wpix:boolean;just:integer);
procedure xybar(cl:Tcolor;pos:double;wi,hi:single;just:integer);
procedure xysegbar(frcol:Tcolor;pos:double;wi,zero:single;just:integer;abs:boolean;
  data:array of Tbardata);
procedure xylegendentry(cd:integer; text:string);
procedure xylegendclear;
procedure xylegendmake(style,xp,yp,xjust,yjust,wi,he,len:integer; framed:boolean);
procedure xyinitruler(cl:Tcolor;xp,yp,j,mode:integer); overload;
procedure xysetusercoordinates(n,ny:integer);
procedure xydrawfunction(n:integer;f:Tsimplefunctiontype);
procedure xyplotarray
(var data:Tdatatype; st,dt:integer; show:Tshowfunction); overload;
procedure xyplotarray(var data:Tdatatype; st,dt:integer); overload;
procedure xysetdataarray(var data:Tdatatype;nx,ny:integer);

procedure xypolargraph(cl:Tcolor;r1,r2,zero:single;yjust:integer;
  rev,ang,grid,fix:boolean);
procedure xyradargraph(cl:Tcolor;axes:array of Tradar;style:integer;
  zero:single;yjust:integer;rev,ang,fix:boolean;opt:integer);
procedure xyradardraw(xw : array of single;sy,sf:integer);

procedure xycopytoclipboard;
procedure xysaveasbitmap(s:string; showtxt:boolean; var ok:boolean);
procedure xyprint(show:boolean);
procedure xyprint2(corr:single);
procedure xysaveasmetafile(s:string; showtxt:boolean; var ok:boolean;
   scale : integer); overload;
procedure XYMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
procedure XYMouseMove(Shift: TShiftState;  X, Y: Integer);
procedure XYMouseUp(Button: TMouseButton; var Shift: TShiftState; X, Y: Integer);
procedure XYKeyDown(var Key: Word; Shift: TShiftState);

procedure xyclearbuffer;
procedure xyputbuffer(n:integer);
procedure xygetbuffer(n:integer);

{for C++ use only:}

procedure initxygraph;

{the following procedures are needed for communication with XYGRAPH3D;
  do not call them directly}

procedure restartgraph(x1,x2,y1,y2:integer;st:boolean);
procedure setcont;
procedure copyzoom(n:integer;x0,x1,y0,y1:single);

{for backward compatibility only:}

procedure xysaveasmetafile(s:string; showtxt:boolean; var ok:boolean); overload;
procedure xysaveasmetafile2(s:string; showtxt:boolean; var ok:boolean;
   scale : integer);
procedure xyinitruler(cl:Tcolor;xp,yp,j:integer); overload;
type exporttype = Texporttype; simplefunctiontype = Tsimplefunctiontype;
   graphdata = Tgraphdata; graphtype = Tgraphtype; axistype = Taxistype;

implementation

uses system.math,printers,xygraph3d,xycommon;

var monthnames : Tmonthnames =
  ('Jan','Feb','Maa','Apr','Mei','Jun','Jul','Aug','Sep','Okt','Nov','Dec');

type datapointer = ^Tdatatype;

const maxrad = 12;
type astype = record                         {waardes en instellingen van assen}
                on,log,side : boolean;
                col : Tcolor;             
                mi,ma : double;
                dlog,pix,incr : single;
                org,len,ndec,ninc,typ : integer;
               end;
     dummyastype = record on:boolean; org,len:integer; end;
     graftype = record                         {instelingen hele grafiek}
                 x1,x2,y1,y2 : integer;        {veld in %}
                 xf0,xf1 : double;             {zoomfactoren X}
                 yf0,yf1 : single;             {zoomfactoren Y}
                 xgridc,xgridf,ygridc,ygridf : integer; {rasterlijnen}
                 ok,nclip,cont : boolean;      {ok,noclip,=contour}
                 zmxx, zmxy : single;          {zoomgrenzen}
                 case polar : integer of
                  0 : (as_ : array[0..4] of astype; {asinstellingen}
                      ndecy : integer;         {decimalen speciale Y-as}
                      shw : Tshowfunction;     {functie voor presentatie}
                      doshw : boolean;         {toon functie shw}
                      trck,tras,trkl : integer;{data tracking mode, yas,kol}
                      dat : datapointer);      {pointer naar datarray}
                 1,2: (xpc,ypc,rp:integer;     {pos centrum en straal}
                       col : Tcolor;           {kleur}
                       mi,ma: array[1..maxrad] of single; {y waarden}
                       ndec:array[1..maxrad] of shortint;{n decim}
                       pix : single;           {y dichtheid}
                       zero : single;          {nullijn}
                       rev : boolean;          {invers}
                       nrad:integer);          {aantal takken}
                 end;
     grafstype = array[1..maxgraf] of graftype;{array van alle grafieken}

type buftype = record  {totale grafiek}
                 labl : integer;
                 graphdat1 : grafstype;
                 graphdat2 : Tgraphdata;
                 pixx,pixy,zoomx,zoomy : single;
                 charw,charh,legendcnt : integer;
                 canw,canh : integer;
                 izm,zgrph : integer;
                 crx,cry,crj : integer;
                 crok : boolean;
                 ccol,fcol,bcol : Tcolor;
                 m3d : boolean;
                 case cvm : boolean of
                   false: (paintbox : Tpaintbox);
                   true:  (canvas : Tcanvas);
               end;

var buffer : array of buftype;
    bufcount : integer;

var xorg,yorg,xlen,ylen : integer;    {offset en lengte asssen}
    xmi,xma : double;                 {min en max waarde assen X}
    ymi,yma : single;                 {min en max waarde assen Y}
    polartype : integer;              {polaire coordinaten = 0,1,2}
    polxc,polyc,polrad : integer;     {- centrum en straal}
    polzero : single;                 {- hoek 0-as}
    polrev : boolean;                 {- hoek omgekeerd}
    yspan : single;                   {bereik Y-as bij ratio}
    xlog,ylog : boolean;              {assen logarithmisch}
    dxlog,dylog : single;             {omrekenfactoren bij log as}
    tsize,hsize : integer;            {ticsize}
    fsize : single;                   {relatieve fontgrootte}
    ndecx,ndecy,nincx,nincy : integer;{aantal decimalen}
    grafok, grafokx, grafoky : boolean;  {id}
    xincr, yincr : single;            {kleinste increment}
    zmaxx, zmaxy : single;            {max. zoomfactor (inverse)}
    xfix,yfix : boolean;              {alleen vaste komma notatie}
    noclip : boolean;                 {controleer niet op clippen}
    lposx,lposy : integer;            {laatste x,y plot positie}
    bar0 : integer;                   {nullijn voor bar bij user off}
    yxratio,aspect : single;          {y/x schaalverhouding}
    xred,yred : integer;              {data reductie parameters}
    nored : boolean;                  {data reductie uit}
    linew1,linew2 : integer;          {line widths}
    lineslope : single;               {helling bij overgang X - Y}
    dm : boolean;                     {d-m volgorde}
    dtmin : double;                   {vroegste datum}
    status : integer;                 {hoe ver zijn we gekomen}

    ndata, nkol  : integer;           {aantal punten in data array}
    xdata, ydata : integer;           {welk data punt}
    xdataold, ydataold : integer;     {vorig data punt}
    track : integer;                  {data tracking mode}
    trackok : boolean;                {data tracking enabled}
    doshowtrack : boolean;            {toon track ipv coord}
    trackas,trackkl : integer;        {y-as/kolom voor data tracking}
    istracking : integer;             {data tracking staat aan op gr. n}
    ld1, ld2 : integer;

    blokx0,blokx1,bloky0,bloky1 : integer; {positie blok}
    blokon : boolean;                 {blok aan}

    linex0,linex1,liney0,liney1 : integer; {positie lijn}
    lineon : boolean;                 {lijn aan}

    crossx, crossy : integer;         {positie lineaal}
    crossx0, crossy0 : integer;       {positie lineaal nul}
    cross0w : array[0..4] of double;  {nul waarden}
    crossxp, crossyp, crossj : integer; {positie lineaal gegevens}
    crosson, crossrel, crossnorel : boolean; {lineaal aan, relatief, geen rel.}
    crossmode : integer;              {lineaal mode}
    snapx, snapy : single;            {x en y snap}
    snapxok, snapyok : boolean;       {x en y snap ok}

    coordok : boolean;                {geef coordinaten}
    coordcolor : Tcolor;              {lineaal kleur}
    coordas : integer;                {bij welke y-as hoort lineaal}

    graphs : grafstype;               {instelingen van alle grafieken}
    pblabel : integer;                {label van paintbox}
    izoom, zgraph : integer;          {laatst gezoomde grafiek; 1 grafiek}

    dag : array[1..13] of integer;    {begindagen in het jaar van een maand}

    stil : boolean;                   {sille modus}
    rul3d : boolean;                  {3d ruler}
    hoogte3d : single;                {hoogte van 3D oppervlak}

    taal,q : integer;

const maanddagen : array[1..12] of integer =
        (31,28,31,30,31,30,31,31,30,31,30,31);
      zmax = 1/1000;
      lmax = 2.00;
      logtics : array[0..4] of set of byte =
         ( [10],
           [10,20,50],
           [10,20,30,40,50,60,70,80,90],
           [10,12,14,16,18,20,25,30,35,40,45,50,60,70,80,90],
           [10,11,12,13,14,15,16,17,18,19,20,22,24,26,28,30,32,34,36,38,40
             ,42,44,46,48,50,55,60,65,70,75,80,85,90,95] );

var  symbols : array of symboltype;   {lijst van symbolen die nog moeten}
     nsymbols : integer;              {aantal symbolen in de lijst}

     lastpoint : boolean;             {moet er nog een laatse punt}
     islastpoint : record x,y:integer;col:Tcolor end; {wat is dit punt}
     lastsymb : record col,wi,xx,yy,s1,s2,s3 : integer; end; {laatste symbool}
     lastsymok : boolean;             {lastsymbol is gevuld}

type legendtype = record
                   txt : string;
                   cl,c1,c2,c3 : Tcolor;
                   code : integer; {0=line, 1=bar, 2,3=symbol 4=errbar}
                   st3d : integer; {3d type}
                   lwidth, lstyle, lfill, lw1, lw2 : integer;
                  end;

var legends : array of legendtype;    {lijst van legend entries}

   winnt : boolean;                   {systeem is Win NT}
   point4 : array[0..3] of Tpoint;

var init : boolean = true;

{==============================================================================}

procedure setcont; begin graphs[igraph].cont := true; end;

procedure xylanguage(n:integer); begin taal := n; end; {XXXXXXXXXXXXXXXXXXXXXXX}

procedure xylinewidth(w:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin if (w>0) then xypen.width := w*res; end;

procedure drawline(x1,y1,x2,y2:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin xycanvas.moveto(x1,y1); xycanvas.lineto(x2,y2); end;

function posx(x:double):integer; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {berekent de x positie op het scherm}
var pos : double;
begin
   if xlog then if (x<=0) then x := xmi/2;
   if xlog then pos := xorg + ln(x/xmi)/dxlog*xlen
           else pos := xorg + (x-xmi)/(xma-xmi)*xlen;
   if (pos<-32768) then pos := -32768 else if (pos>32767) then pos := 32767;
   posx := round(pos);
end;

function posx2(x:double):integer; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {berekent de x positie op het scherm}
var pos : double;
begin
   with graphs[igraph].as_[0] do pos := org + (x-mi)/(ma-mi)*len;
   if (pos<-32768) then pos := -32768 else if (pos>32767) then pos := 32767;
   posx2 := round(pos);
end;

function posy(y:single):integer; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {berekent de y positie op het scherm}
var pos : double;
begin
   if ylog then if (y<=0) then y := ymi/2;
   if ylog then pos := yorg - ln(y/ymi)/dylog*ylen
           else pos := yorg - (y-ymi)/(yma-ymi)*ylen;
   if (pos<-32768) then pos := -32768 else if (pos>32767) then pos := 32767;
   posy := round(pos);
end;

function posy2(y:single;n:integer):integer; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {berekent de y positie op het scherm}
var pos : double;
begin
   with graphs[igraph].as_[n] do pos := org - (y-mi)/(ma-mi)*len;
   if (pos<-32768) then pos := -32768 else if (pos>32767) then pos := 32767;
   posy2 := round(pos);
end;

procedure polarpos(r,h:single;var xp,yp:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var n : integer;
begin
  if (polartype=2) then with graphs[igraph] do
    begin n := round(h); h := (h-1)/nrad*360;
     r := (r-mi[n])/(ma[n]-mi[n])*rp; end
  else r := (r-xmi)/(xma-xmi)*polrad;
  if (r<0) then r := 0;

  if polrev then h := -h; h := h + polzero;
  xp := polxc+round(cos(h/180*pi)*r);
  yp := polyc-round(sin(h/180*pi)*r);
end;

function xwaarde(x:integer):double; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {berekent x waarde bij een bepaalde positie scherm}
begin
   if (x<xoff) then x := xoff else if (x>cwidth+xoff) then x := cwidth+xoff;
   with graphs[igraph].as_[0] do
   if log then xwaarde := mi * exp((x-org)/len*dlog)
          else xwaarde := mi + (x-org)/len*(ma-mi);
end;

function ywaarde(y,n:integer):single; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {berekent y waarde bij een bepaalde positie scherm voor xyyaxis n}
begin
   if (y<yoff) then y := yoff else if (y>cheight+yoff) then y := cheight+yoff;
   if (n=0) then {actuele yas}
     begin
       if ylog then ywaarde := ymi * exp((yorg-y)/ylen*dylog)
               else ywaarde := ymi + (yorg-y)/ylen*(yma-ymi);
     end
   else
     with graphs[igraph].as_[n] do
       if log then ywaarde := mi * exp((org-y)/len*dlog)
              else ywaarde := mi + (org-y)/len*(ma-mi);
end;

procedure polarwaarde(xp,yp:integer;var x,h : single;var r:integer); {XXXXXXXXX}
var n : integer;
begin
  with graphs[igraph] do begin
  if (xp=xpc) and (yp=ypc) then
    begin h := 0; r := 0; x := mi[1]; exit; end;

  if (xp>xpc) then h := arctan(-(yp-ypc)/(xp-xpc)) else
  if (xp<xpc) then h := arctan(-(yp-ypc)/(xp-xpc))+pi else
  if (yp<ypc) then h := pi/2 else h := -pi/2;
  h := h/pi*180 - zero; if rev then h := -h;
  if (h<0) then repeat h := h+360 until (h>=0);
  if (h>=360) then repeat h := h-360 until (h<360);

  if (polar=1) then n := 1 else
    begin
      n := round(h/360*nrad)+1; if (n>nrad) then n := 1;
      h := n;
    end;

  x := sqrt(sqr(xpc-xp)+sqr(ypc-yp)); r := round(x);
  x := mi[n] + r/rp*(ma[n]-mi[n]);

  end;
end;

procedure xyabstouser(xp,yp:integer; var x,y:single); {XXXXXXXXXXXXXXXXXXXXXXXX}
var r : integer;
begin
  if (polartype>0) then polarwaarde(xp,yp,x,y,r)
  else begin x := xwaarde(xp*res+xoff); y := ywaarde(yp*res+yoff,0); end;
end;
procedure xyabstouser(xp,yp:integer; var x,y:double);
var r : integer; xx,yy : single;
begin
  if (polartype>0) then begin polarwaarde(xp,yp,xx,yy,r); x := xx; y := yy; end
  else begin x := xwaarde(xp*res+xoff); y := ywaarde(yp*res+yoff,0); end;
end;
procedure xyabstouser(xp,yp:integer; var x:double; var y:single);
var r : integer; xx : single;
begin
  if (polartype>0) then begin polarwaarde(xp,yp,xx,y,r); x := xx; end
  else begin x := xwaarde(xp*res+xoff); y := ywaarde(yp*res+yoff,0); end;
end;

procedure xyusertoabs(x,y:double; var xp,yp:integer); {XXXXXXXXXXXXXXXXXXXXXXXX}
begin
  if (polartype>0) then polarpos(x,y,xp,yp) else
  begin xp := round((posx(x)-xoff)/res); yp := round((posy(y)-yoff)/res); end;
end;

function plus(x:integer):integer; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin plus := max(x,0); end;

function datum(n:integer):string; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {geeft de datumweergave van dag n}
var m,d:integer;
begin
  if (n<1) or (n>dag[13]) then begin datum := ' - '; exit; end;
  m := 0; repeat inc(m) until (dag[m+1]>=n);
  d := n-dag[m]; datum := inttostr(d)+'-'+inttostr(m);
end;

{------------------------------------------------------------------------------}

function showdec(w:single; ndec:integer):string; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {geeft tekst normaal met n decimalen}
var f : single; s : string;
begin
 if (ndec<0) then
   begin
     f := power(10,-ndec); w := round(w/f) * f; ndec := 0;
   end;
 str(w:1:ndec,s); showdec := s;
end;

function lintext(w:single;ndec:integer):string; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
  {tekst bij lin. as eenheden in sci notatie}
var i,t,n : integer;
    s : string;
begin
  if (w=0) then s := '0' else
  if (ndec<=-3) then
    begin
      for i := 1 to -ndec do w := w / 10;
      str(w:1:0,s); s := s + 'E' + chr(48-ndec);
    end else
  if (ndec<=0) then str(w:1:0,s) else
  if (abs(w)>0.0095) then str(w:1:ndec,s) else
    begin
      t := 0; repeat w:=w*10; inc(t) until (abs(w)>1);
      n := ndec-t; if (n<0) then n := 0;
      str(w:1:n,s); s := s + 'E-' +chr(48+t);
    end;
 lintext := s;
end;

function logtxt(i,n:integer):string; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {tekst bij log. as eenheden}
var s,ss : string;
    p : integer;
begin
   case i of
     -2 : s := '0.01';
     -1 : s := '0.1';
      0 : s := '1';
      1 : s := '10';
      2 : s := '100';
      3 : s := '1000';
    else begin str(i:1,ss); s := '1E'+ss; end;
    end;
   ss := chr(48+n); p := pos('1',s); s[p] := ss[1];
   logtxt := s;
end;

function ndecim(f:single):integer; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
  {bepaalt hoeveel decimalen nodig zijn voor getal f}
begin
  f := abs(f);
  if (f=0) then ndecim := 99 else
  if (f>=1) then ndecim := -trunc(log10(f*1.001)) else
  {if (f<1) then} ndecim := -trunc(log10(f*1.001))+1;
end;

function signif(t:single;n : integer):string; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
  {geeft getal met n sign cijfers in sci notatie}
var s,ss : string;
    l,i : integer;
    tt : single;
begin
   if (t=0) then begin signif:= '0'; exit; end;
   tt := t; t := abs(t);
   l := trunc(log10(t))+1; if (t<1) then dec(l);
   if (l<=n+1) and (l>-2) then
     begin
       str(t:0:plus(n-l),s);
       if (l>n) then for i := n+1 to length(s) do s[i]:= '0';
     end
   else
     begin
      str(t:20:-1,ss);
      s := copy(ss,2,n+1) + 'E';
      if (ss[16]='-') then s := s + '-';
      if (ss[19]<>'0') then s := s + ss[19];
      s := s + ss[20];
    end;
   if (tt<0) then s := '-' + s;
   result := s;
end;

{------------------------------------------------------------------------------}

procedure xgrid(xp:integer; fine:boolean); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {tekent gridline voor x-as}
var i,y,mode{,d1,d2} : integer;
begin
  if (xp=xorg) then exit;
  if fine then mode := graphs[igraph].xgridf{ * res}
          else mode := graphs[igraph].xgridc{ * res};
  if (mode<0) then exit;

  if (mode=0) then
    begin xypen.style := pssolid; xyline(-1,xp,yorg,xp,yorg-ylen); end
  else
    begin
      if dowmf then if fine then addfine(xp) else addcoarse(xp)
      else
       for i := 0 to (ylen div res) do if (i and mode = 0) then
           begin y := yorg-i*res; xypixel(xp,y,xypen.color); end;
    end;
end;

procedure ygrid(yp:integer; fine:boolean); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {tekent gridline voor x-as}
var i,x,mode{,d1,d2} : integer;
begin
  if (yp=yorg) then exit;
  if fine then mode := graphs[igraph].ygridf{ * res}
          else mode := graphs[igraph].ygridc{ * res};
  if (mode<0) then exit;

  if (mode=0) then
    begin xypen.style := pssolid; xyline(-1,xorg,yp,xorg+xlen,yp); end
  else
    begin
      if dowmf then if fine then addfine(yp) else addcoarse(yp)
      else
      for i := 0 to (xlen div res) do if (i and mode = 0) then
        begin x := xorg+i*res; xypixel(x,yp,xypen.color) end;
    end;
end;

procedure drawlastpoint; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  with islastpoint do
    if dowmf then pixelwmf(x,y,col)
    else xycanvas.pixels[x,y] := col;
  lastpoint := false;
end;

procedure setlastpoint(x,y:integer;col:Tcolor); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  lastpoint := true;
  islastpoint.x := x;
  islastpoint.y := y;
  islastpoint.col := col;
end;

function checkred(xp,yp:integer):boolean; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
  {kijkt of lijn wel getekend moet worden}
var t : integer;
begin
  if nored then result := true else
    begin
      result := false;
      if (xred>=0) then
        begin
          if (xred=0) then t := xypen.width else t := xred;
          if (abs(xp-lposx)>=t) then result := true;
        end;
      if (yred>=0) then
        begin
          if (yred=0) then t := xypen.width else t := yred;
          if (abs(yp-lposy)>=t) then result := true;
        end;
    end;
end;

procedure xysetlinestyle(ls,sp:integer;sl:single); {XXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  if stereo then ls := 0;
  linestyle := ls * res; linew1 := 0; linew2 := 0;
  if (linestyle>0) then
     begin
       linew2 := linestyle; if (linew1<2*res) then linew1 := 2*res;
       if (sp=0) then sp := 50; linew1 := round(sp/100*linew2);
       if (linew1=0) then linew1 := 1*res;
       if (linew1=linew2) then linew1 := linew2-1*res;
     end;
  if (sl=0) then lineslope := 1 else lineslope := abs(sl);
end;

procedure dashline(x1,y1,x2,y2,w1,w2:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var t,xx1,xx2,yy1,yy2 : integer;
    f,a : single;
function ison(p:integer):boolean; begin ison := (p mod w2) < w1; end;
function pos(p:integer):integer;  begin pos := round(a+f*p); end;
begin
  if (x2=x1) and (y2=y1) then drawline(x1,y1,x1+1,y1)
  else
  if (abs(y2-y1)>abs(x2-x1)*lineslope) then
    begin
      if (y1>y2) then
        begin t := x1; x1 := x2; x2 := t; t := y1; y1 := y2; y2 := t; end;
      f := (x2-x1)/(y2-y1); a := x1 - f*y1;
      yy1 := y1; while not ison(yy1) do inc(yy1);
      yy2 := yy1; while ison(yy2) do inc(yy2);
      if (yy1<=y2) then repeat
        if (yy2>y2) then yy2 := y2;
        xx1 := pos(yy1); xx2 := pos(yy2);
        xyline(-1,xx1,yy1,xx2,yy2);
        yy1 := yy2+w2-w1; yy2 := yy1 + w1;
      until (yy1>y2);
    end
  else
    begin
      if (x1>x2) then
        begin t := x1; x1 := x2; x2 := t; t := y1; y1 := y2; y2 := t; end;
      f := (y2-y1)/(x2-x1); a := y1 - f*x1;
      xx1 := x1; while not ison(xx1) do inc(xx1);
      xx2 := xx1; while ison(xx2) do inc(xx2);
      if (xx1<=x2) then repeat
        if (xx2>x2) then xx2 := x2;
        yy1 := pos(xx1); yy2 := pos(xx2);
        xyline(-1,xx1,yy1,xx2,yy2);
        xx1 := xx2+w2-w1; xx2 := xx1 + w1;
      until (xx1>x2);
    end;
end;

procedure cliplineto(x2,y2:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
  {teken lijn met inachtname van clipping}
var x1,y1,xx,yy,t,x1p,y1p,x2p,y2p: integer;
    r,r1,r2,co,si,d,h:single;
procedure doline;
begin
  if (linestyle>1) then begin dashline(x1,y1,x2,y2,linew1,linew2); exit; end;
  if dowmf then with xycanvas.penpos do
    begin
      if (x=x1) and (y=y1) then drawwmf(x2,y2) else
      linewmf(x1,y1,x2,y2);
    end
  else drawline(x1,y1,x2,y2);
  with xypen do if (width=1) then
    if (x2<xorg+xlen) and (y2<yorg) and (y2>yorg-ylen) then
      setlastpoint(x2,y2,color);
end;
begin
  if not checkred(x2,y2) then exit;
  x1 := lposx; y1 := lposy; lposx := x2; lposy := y2;

  if (polartype>0) then  begin

  x1p := x1-polxc; y1p := y1-polyc; x2p := x2-polxc; y2p := y2-polyc;
  r1 := sqrt(sqr(x1p)+sqr(y1p)); r2 := sqrt(sqr(x2p)+sqr(y2p));
  if (r1>polrad) and (r2>polrad) then exit; {alles buiten}
  if (r1<polrad) and (r2<polrad) then {alles binnen}
      begin doline; exit; end;
  if (r2<r1) then {sorteer x1 en x2}
      begin t := x1; x1 := x2; x2 := t; t := y1; y1 := y2; y2 := t; end;
  r := sqrt(sqr(x2-x1)+sqr(y2-y1)); if (r=0) then exit;
    {bereken snijpunt met cirkel}
  co := -(y2-y1)/r; si := (x2-x1)/r; {hoekrotatie}
  d := x1p*co + y1p*si; h := sqrt(sqr(polrad)-sqr(d)); {snijpunt}
  x2 := polxc+round(d*co+h*si); y2 := polyc+round(d*si-h*co); {rotatie terug}
  doline; exit;

  end;

  xx := xorg + xlen; yy := yorg - ylen;
  if (x1>x2) then {sorteer x1 en x2}
    begin t := x1; x1 := x2; x2 := t; t := y1; y1 := y2; y2 := t; end;

  if (x1>xorg) and (x2<xx) and
    (y1>=yy) and (y1<=yorg) and (y2>=yy) and (y2<=yorg) then
  begin doline; exit; end; {lijn valt geheel binnen kader}

  if (x2<xorg) or (x1>xx) or ( (y1<yy) and (y2<yy) ) or ( (y1>yorg) and (y2>yorg) )
   then exit; {lijn valt geheel buiten kader}

  if (x1=x2) then {verticale lijn, test clipping y waarden}
    begin
      if (y1=y2) then exit;
      if (y1<yy) then y1 := yy else if (y1>yorg) then y1 := yorg;
      if (y2<yy) then y2 := yy-1 else if (y2>yorg) then y2 := yorg+1;
      doline; exit;
    end;

  {test nu clipping van x waarden}
  if (x1<xorg) then begin y1 := y1 + round((xorg-x1)/(x2-x1)*(y2-y1)); x1 := xorg; end;
  if (x2>xx)   then begin y2 := y2 + round((xx+1-x2)/(x2-x1)*(y2-y1)); x2 := xx+1; end;
  if ( (y1<yy) and (y2<yy) ) or ( (y1>yorg) and (y2>yorg) ) then exit;
   {lijn valt nu alsnog geheel buiten kader}

  if (y1>=yy) and (y1<=yorg) and (y2>=yy) and (y2<=yorg) then
     begin doline; exit; end; {lijn valt nu alsnog geheel binnen kader}

  {test nu clipping van y waarden}
  if (y2>y1) then
    begin
      if (y1<yy)   then begin x1 := x1 + round((yy-y1)/(y2-y1)*(x2-x1)); y1 := yy; end;
      if (y2>yorg) then begin x2 := x2 + round((yorg+1-y2)/(y2-y1)*(x2-x1)); y2 := yorg+1; end;
      doline;
    end
  else
    begin
      if (y2<yy)   then begin x2 := x2 + round((yy-1-y2)/(y2-y1)*(x2-x1)); y2 := yy-1; end;
      if (y1>yorg) then begin x1 := x1 + round((yorg-y1)/(y2-y1)*(x2-x1)); y1 := yorg; end;
      doline;
    end;
end;

procedure xymove(x,y:double); {XXXXXXXXXXXXXXXXXCCXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {verplaatst penpositie}
var xp,yp : integer;
begin
  if lastpoint then drawlastpoint;
  if not (grafoky or useroff or mode3d) then exit;

  if mode3d then begin xp := round(x); yp := round(y); end
  else
  if useroff then
    begin xp := round(x*res)+xoff; yp := round(y*res)+yoff; end
  else
    if (polartype>0) then polarpos(x,y,xp,yp) else
    begin xp := posx(x); yp := posy(y); end;

  if noclip or useroff or mode3d then setcursor(xp,yp);
  lposx := xp; lposy := yp;
end;

procedure xydraw(x,y:double); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {tekent lijn}
var xp,yp : integer;
begin
  if not (grafoky or useroff or mode3d) then exit;

  if mode3d then begin xp := round(x); yp := round(y); end
  else
  if useroff then
    begin xp := round(x*res)+xoff; yp := round(y*res)+yoff; end
  else
    if (polartype>0) then polarpos(x,y,xp,yp) else
    begin xp := posx(x); yp := posy(y); end;

  if (linestyle>=0) then xypen.style := pssolid;

  if dowmf then checkwmfpen;
  if noclip or useroff or mode3d then with xypen do
    begin
      if checkred(xp,yp) then
        begin
          if (linestyle>1) then
            begin
              dashline(lposx,lposy,xp,yp,linew1,linew2); setcursor(xp,yp);
            end
          else
            begin
              if dowmf then drawwmf(xp,yp) else xycanvas.lineto(xp,yp);
              if (width=1) then setlastpoint(xp,yp,color);
             end;
          lposx := xp; lposy := yp;
        end
     end
  else cliplineto(xp,yp);
end;

{------------------------------------------------------------------------------}

procedure drawsymbol(symb:symboltype); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {tekent symbool}
 {0=punt 1=ronje, 2=vierkant 3-6 = driehoeken 7=kruisX 8=kruis+}
 {3D: 20-26 = stijl van surface 28=blok 29=rondje 30=polyh 31=cyl
   deze symbolen alleen bij legenda, zie drawleg (xylegendmake) }
var d,d2 : integer;
    points : array[0..2] of Tpoint;
begin
  with xycanvas do with symb do begin

  d := (size div 2); d2 := d + d div 2;

  if (style=0) or (d<=0) then
    begin xypixel(xp,yp,pen.color); exit; end;

  pen.width := width; pen.style := pssolid; pen.color := color;
  case fill of
    0 : brush.style := bsclear;
    1 : begin brush.style := bssolid; brush.color := backcolor; end;
    2 : begin brush.style := bssolid; brush.color := color; end;
    3 : begin brush.style := bssolid; brush.color := cl1; end;
  end;
  case style of
   1 : xycircle(-1,-1,xp,yp,d);
   2 : xyrectangle(-1,-1,xp-d,yp-d,xp+d+1,yp+d+1);
   3 : begin points[0] := point(xp-d,yp+d); points[1] := point(xp+d,yp+d);
             points[2] := point(xp,yp-d2); xypolygon(-1,-1,points); end;
   4 : begin points[0] := point(xp-d,yp-d); points[1] := point(xp+d,yp-d);
             points[2] := point(xp,yp+d2); xypolygon(-1,-1,points); end;
   5 : begin points[0] := point(xp-d,yp+d); points[1] := point(xp-d,yp-d);
             points[2] := point(xp+d2,yp); xypolygon(-1,-1,points); end;
   6 : begin points[0] := point(xp+d,yp+d); points[1] := point(xp+d,yp-d);
             points[2] := point(xp-d2,yp); xypolygon(-1,-1,points); end;
   7 : if (xypen.width=1) then
         begin xyline(-1,xp,yp-d,xp,yp+d+1); xyline(-1,xp+d,yp,xp-d-1,yp); end
       else
         begin xyline(-1,xp,yp-d,xp,yp+d); xyline(-1,xp-d,yp,xp+d,yp); end;
   8 : if (xypen.width=1) then
         begin xyline(-1,xp-d+1,yp-d+1,xp+d,yp+d); xyline(-1,xp+d-1,yp-d+1,xp-d,yp+d); end
       else
         begin xyline(-1,xp-d,yp-d,xp+d,yp+d); xyline(-1,xp+d,yp-d,xp-d,yp+d); end;
   end;
   end;
end;

procedure drawallsymbols; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {tekent alle symbolden}
var i : integer;
begin
  if (nsymbols=0) then exit;
  store;
  for i := 1 to nsymbols do drawsymbol(symbols[i-1]);
  nsymbols := 0; lastsymok := false; restore;
end;

procedure addsymbol(x,y,st,sz,fl,wi:integer;col:Tcolor); {XXXXXXXXXXXXXXXXXXXXX}
 {voegt symbool toe aan lijst}
begin
  if ( not (st in [0..8]) ) or ( not fl in [0..2] ) then exit;

  if (sz=0) then
    if doprint then sz := round(xyfont.size*ffac)
               else sz := xyfont.size
  else
  if (sz<0) then begin x := 0; y := 0; end
  else sz := sz*res;

  with lastsymbol do
    begin xp := x; yp := y; style := st; size := sz; fill := fl;
          width := wi; color := col; cd3d := 0; end;

  if (sz<0) then exit;
  lastsymok := true;

  if (x<xoff) or (x>=cwidth+xoff) or (y<yoff) or (y>=cheight+yoff) then exit;
  if not (useroff or noclip or mode3d) then
    if (polartype>0) then begin
      if sqr(x-polxc)+sqr(y-polyc)>sqr(polrad) then exit;
    end else
    if (x<xorg) or (x>xorg+xlen) or (y<yorg-ylen) or (y>yorg) then exit;

  inc(nsymbols); setlength(symbols,nsymbols);
  symbols[nsymbols-1] := lastsymbol;
  if mode3d then drawallsymbols;
end;

procedure xysymbolp(st,sz,fl:integer;x,y:double); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {verwerkt symbool met x,y}
var xp,yp : integer;
begin
  if useroff or mode3d then
    begin xp := round(x*res)+xoff; yp := round(y*res)+yoff; end
  else
    if (polartype>0) then polarpos(x,y,xp,yp) else
    begin xp := posx(x); yp := posy(y); end;
  addsymbol(xp,yp,st,sz,fl,xypen.width,xypen.color);
end;

procedure xysymbol(st,sz,fl:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {verwerkt symbool zonder x,y}
var xp,yp : integer;
begin
  if noclip or useroff or mode3d then
    begin xp := xycanvas.penpos.x; yp := xycanvas.penpos.y; end
  else
    begin xp := lposx; yp := lposy; end;
  addsymbol(xp,yp,st,sz,fl,xypen.width,xypen.color);
end;

{------------------------------------------------------------------------------}

procedure xytextangle(cl:Tcolor;txt:string;x,y:double;xjust,yjust,mode,angle:integer);
 { mode 0=trans,noframe; 1=transp,frame; 2=obl,noframe; 3 = obl,frame
   + 8 = screen coordinates, +16 = reduced height}
var lines : array of string;
    nlines,p,wi,he,w,i,xp0,yp0,xp1,yp1,d,xo,yo,xp,yp,xx,yy,cl1,cl2,lhe,dhe : integer;
    ang1, ang2 : integer;
    abs, red : boolean;
    si,co : single;
    LogRec: TLOGFONT;
    OldFontHandle, NewFontHandle: HFONT;
const del = '|';
      dd = 3;
procedure rotate(var x,y : integer);
var dw,dh :integer;
begin
   dw := xo-x; dh := yo-y;
   x := xo-round(dh*si+dw*co);
   y := yo-round(dh*co-dw*si);
end;
begin
  if stereo then cl := stereocol[0]
  else if (cl<0) then cl := xyfont.color;
  ang1 := angle * 10; ang2 := ang1;
  if (mode>300) then begin ang2 := mode-1024; mode := 0; end;
  abs := (mode and 8 > 0); {abs coord}
  red := (mode and 16 >0); {gereduceerde hoogte}
  mode := mode and 7;
  if abs then
    begin xp0 := round(x); yp0 := round(y); end
  else
  if useroff or mode3d then
    begin xp0 := round(x*res)+xoff; yp0 := round(y*res)+yoff; end
  else
    if (polartype>0) then polarpos(x,y,xp,yp) else
    begin xp0 := posx(x); yp0 := posy(y); end;

  if (xp0<xoff) or (xp0>=cwidth+xoff) or (yp0<yoff) or (yp0>=cheight+yoff) then exit;
  if not (useroff or noclip or mode3d or abs) then
    if (xp0<xorg) or (xp0>xorg+xlen) or (yp0<yorg-ylen) or (yp0>yorg) then exit;

  if (angle=0) then begin si := 0; co := 1 end else
  begin
    while (angle<0) do inc(angle,360); angle := angle mod 360;
    si := sin(angle/180*pi); co := cos(angle/180*pi);
  end;
  xo := xp0; yo := yp0;
  if red then lhe := round(xycharheight*0.75) else
              lhe := round(xycharheight*0.95);
  dhe := xycharheight-lhe;

  setlength(lines,1); p := pos(del,txt); nlines := 0; d := dd * res;
  if (p=0) then
     begin lines[0] := txt; nlines := 1; end
  else
    begin
      repeat
        inc(nlines); setlength(lines,nlines);
        lines[nlines-1] := copy(txt,1,p-1);
        txt := copy(txt,p+1,length(txt));
        p := pos(del,txt);
      until (p=0);
      inc(nlines); setlength(lines,nlines);
      lines[nlines-1] := txt;
    end;
  he := nlines*lhe + dhe; wi := 0;
  for i := 0 to nlines-1 do
    begin
      w := xycanvas.textwidth(lines[i]);
      if (w>wi) then wi := w;
    end;

  wi := wi + d+d;
  if (xjust=0) then xp0 := xp0-wi div 2 else
   if (xjust<0) then xp0 := xp0 - wi;
  if (yjust=0) then yp0 := yp0-he div 2 else
   if (yjust<0) then yp0 := yp0 - he;

  if (mode>0) then
    begin
      if odd(mode) then cl1 := cl else cl1 := -1;
      if (mode and 2 =0) then cl2 := -1 else cl2 := backcolor;
      xypen.width := res; xp1 := xp0+wi; yp1 := yp0+he+1;
      if (angle=0) then xyrectangle(cl1,cl2,xp0,yp0,xp1,yp1)
      else
        begin
         xx := xp0; yy := yp0; rotate(xx,yy); point4[0] := point(xx,yy);
         xx := xp1; yy := yp0; rotate(xx,yy); point4[1] := point(xx,yy);
         xx := xp1; yy := yp1; rotate(xx,yy); point4[2] := point(xx,yy);
         xx := xp0; yy := yp1; rotate(xx,yy); point4[3] := point(xx,yy);
         xypolygon(cl1,cl2,point4);
        end;
    end;

  with xycanvas do
    begin
      font.color := cl; brush.style := bsclear;
      if (ang1<>0) or (ang2<>0) then
        begin
          setgraphicsmode(handle,2);
          GetObject(Font.Handle,SizeOf(LogRec),Addr(LogRec));
          LogRec.lfEscapement := Ang1;
          LogRec.lfOrientation := Ang2;
          LogRec.lfQuality := 4;
          NewFontHandle := CreateFontIndirect(LogRec);
          OldFontHandle := SelectObject(Handle,NewFontHandle);
          xyfontangle := angle;
        end;

      if dowmf then begin checkwmffont; textwmfalign(0); end;

      for i := 0 to nlines-1 do
        begin
          w := wi-(d+textwidth(lines[i])+d);
          if (xjust=0) then w := w div 2 else
          if (xjust=1) then w := 0;
          xp := xp0+d+w; yp := yp0+i*lhe;
          if (angle<>0) then rotate(xp,yp);
          if dowmf then textwmfout(xp,yp,lines[i])
            else textout(xp,yp,lines[i]);
       end;

      if (angle<>0) then
        begin
          NewFontHandle := SelectObject(Handle,OldFontHandle);
          DeleteObject(NewFontHandle);
        end;
      xyfontangle := 0;
    end;
end;

procedure xytext(cl:Tcolor;txt:string;x,y:double;xjust,yjust,mode:integer);
begin xytextangle(cl,txt,x,y,xjust,yjust,mode,0); end;

procedure vtext(cl:Tcolor;txt:string;x,y:single;xjust,yjust,mode:integer);
var i : integer;
    s : string;
begin
  s := '|'; for i := length(txt) downto 2 do insert(s,txt,i);
  xytextangle(cl,txt,x,y,xjust,yjust,mode+16,0);
end;

{------------------------------------------------------------------------------}

procedure xydrawfunction(n:integer;f:Tsimplefunctiontype); {XXXXXXXXXXXXXXXXXXX}
var t : integer;
    x : single;
begin
  if (n<1) then n := xypen.width;
  if (linestyle>=0) then xypen.style := pssolid;
  with graphs[igraph].as_[0] do
    begin
      t := 0; x := xwaarde(t+org); xymove(x,f(x));
      repeat
        t := t + n; if (t>len) then t := len;
        x := xwaarde(t+org); xydraw(x,f(x));
      until(t>=len);
    end;
end;

{------------------------------------------------------------------------------}

procedure verdeling(mi,ma:single; var incr:single;len,dist:integer;
     var grof,fijn : single; var nach:integer);
 {berekent een schaalverdeling, min dist apart}
var fac,scale,scale0,f,p : single;
    ok : boolean;
    tel,n : integer;
function pos(x:single):integer;
begin pos := round((x-mi)/(ma-mi)*len); end;
begin
   n := len div dist; if (n<1) then n := 1;
   if (mi>ma) then begin f := ma; ma := mi; mi := f; end;
   ok := false; f := 1;

   scale := ma-mi; scale0 := scale;

   if (incr<0) then incr := 0 else
   if (incr>0) then
     begin
       fac := 1;
       while (incr>7) do   begin incr := incr/10; fac := fac*10; end;
       while (incr<0.7) do begin incr := incr*10; fac := fac/10; end;
       if (incr>3.2) then incr := 5 else
       if (incr>1.4) then incr := 2 else incr := 1;
       incr := incr*fac;
     end;

   repeat

   scale := scale0/(n+1e-6)*f;
   fac := 1; nach:=0;
   while (scale>=5) do begin scale := scale/10; fac := fac*10; dec(nach); end;
   while (scale<0.5) do
      begin scale := scale*10; fac := fac/10; inc(nach); end;

   if scale<1 then begin grof := fac; fijn := fac/5; end
   else
   if scale<2 then begin grof := 2*fac; fijn := fac/2; end
   else
   begin grof := 5*fac; fijn := fac; end;

   p := grof*trunc(mi/grof-1); tel := 0;
   while (pos(p)<=len) do
    begin if (pos(p)>=0) then inc(tel); p := p+grof; end;
   if (tel<2) then f := f * 0.8 else ok := true;

   if (fijn<incr) then fijn := incr;
   if (grof<incr) then grof := incr
   else if (grof>fijn*2.4) and (grof<fijn*2.6) then grof := grof*2;

   until ok;
end;

procedure logverdeling(scale:single;len:integer; var fijn : single); {XXXXXXXXXXX}
 {berekent een log.schaalverdeling}
var f : single;
begin
   f := len/log10(scale)/(fsize*res);
   if (f>250) then fijn := 224 else
   if (f>160) then fijn := 123 else
   if (f>100) then fijn := 112 else
   if (f>040) then fijn := 012 else
   if (f>007) then fijn := 001 else
   fijn := 000;
end;

{------------------------------------------------------------------------------}

procedure selectfont(siz:single); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {selecteert font met relatieve grootte siz}
begin
  siz := siz*8*res ;
  if doprint then siz := siz / ffac;
  with xyfont do begin name := 'Arial'; size := round(siz); end;
  xycharwidth := xycanvas.textwidth('0');
  xycharheight := xycanvas.textheight('0');
end;

procedure xytitle(cl:Tcolor;txt:string); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
  {tekent titel}
var l,xp,yp : integer;
begin
  if not (grafok or mode3d) then exit;
  if stereo then cl := stereocol[0];
  selectfont(fsize*1.2);
  xyfont.color := cl;
  xyfont.style := [fsbold];
  xybrush.Style := bsclear;
  if (txt[1]='@') then
    begin
      xp := cwidth div 2; yp := 4*res;
      txt := copy(txt,2,length(txt));
      l := xycanvas.textwidth(txt);
    end
  else
    with graphs[igraph] do
      begin
        if (polar>0) then xp := xpc else
        with as_[0] do xp := org + len div 2;
        yp :=  y1+4*res;
        l := xycanvas.textwidth(txt);
    end;
  if dowmf then
    begin checkwmffont; textwmfalign(6);
    with graphs[igraph] do textwmfout(xp,yp,txt) end
  else
    with graphs[igraph] do xycanvas.textout(xp- l div 2,yp,txt);
  selectfont(fsize);
  xyfont.style := [];
end;

procedure xyunzoomx(n:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var i : integer;
begin
  if (n=0) then for i := 1 to maxgraf do with graphs[i] do
    begin xf0 := 0; xf1 := 1; copyzoom3d(i,0,1,yf0,yf1); end
  else if n in [1..maxgraf] then with graphs[n] do
    begin xf0 := 0; xf1 := 1; copyzoom3d(n,0,1,yf0,yf1); end
end;
procedure xyunzoomy(n:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var i : integer;
begin
  if (n=0) then for i := 1 to maxgraf do with graphs[i] do
    begin yf0 := 0; yf1 := 1; copyzoom3d(i,xf0,xf1,0,1); end
  else if n in [1..maxgraf] then with graphs[n] do
    begin yf0 := 0; yf1 := 1; copyzoom3d(n,xf0,xf1,0,1); end
end;
procedure xyunzoom(n:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin xyunzoomx(n); xyunzoomy(n); if (n=0) or (n=izoom) then izoom := 0; end;
procedure xyunzoom; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin xyunzoom(0); end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

procedure xyfinish; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  if lastpoint then drawlastpoint;
  drawallsymbols;
end;

procedure initcanvas(bcolor,fcolor:Tcolor;scale:single;clear:boolean); {XXXXXXXX}
var i,j : integer;
const labels : array[0..4] of string = ('X','Y1','Y2','Y3','Y4');
begin
  screen.cursor := crdefault; crossx := 0; crossy := 0;
  crosson := false; coordok := false; blokon := false; lineon := false;
  crossmode := 0; snapxok := false; snapyok := false; snapx := 0; snapy := 0;
  crossnorel := false; trackok := true;
  xypen := xycanvas.pen; xybrush := xycanvas.brush; xyfont := xycanvas.font;
  backcolor := bcolor; oldbcolor := bcolor; with xybrush do
     begin color := backcolor; style := bssolid; end;
  frontcolor := fcolor; oldfcolor := fcolor; with xypen do
     begin color := frontcolor; style := pssolid; width := res; mode := pmcopy; end;
  if (frontcolor<>backcolor) then coordcolor := frontcolor
    else coordcolor := not backcolor;
  if clear then begin xycanvas.rectangle(xoff,yoff,xoff+cwidth,yoff+cheight); {beep;} end;
  if (scale=0) then scale := 1; fsize := scale; selectfont(fsize);
  tsize := xycharwidth + 1; hsize := tsize div 2 + 1;
  for i := 1 to maxgraf do with graphs[i] do
    begin xgridc := 4; xgridf := 7; ygridc := 4; ygridf := 7;
        ok := false; cont := false; end;
  for i := 1 to maxgraf do with xygraphdata[i] do
    begin
      ok := false; x1 := 0; x2 := 0; y1 := 0; y2 := 0;
      with xaxis do
        begin on := false; min := 0; max := 0; log := false; end;
      for j := 1 to 4 do with yaxis[j] do
        begin on := false; min := 0; max := 0; log := false; end;
    end;
  ngraph := 0; igraph := 0; grafoky := false; zgraph := 0;
  nsymbols := 0; lastsymok := false; istracking := 0; track := 0;
  lastpoint := false; pblabel := 0; rul3d := false; status := 1;
  xylegendclear; xysetusercoordinates(0,0);
  if not (doprint or dowmf) then
  with xybuffers[0] do
     begin lbl := 0; ngr := 0; cw := cwidth; ch := cheight;
     cvm := cvmode; bmp := false; str := false; b4d := false;
     if cvm then form := screen.activeform.name
       else if xypaintbox.owner is Tform then form := xypaintbox.owner.name
       else form := screen.activeform.name;
     init3d; end;
  for i := 0 to 4 do xylabels[i] := labels[i];
end;

procedure xycanvasgraph(canvas:Tcanvas;width,height:integer; {XXXXXXXXXXXXXXXXX}
   bcolor,fcolor:Tcolor; scale:single; clear:boolean);
begin
  if init then initxygraph;
  xycanvas := canvas; res := 1; cvmode := true;
  cwidth := width; cheight := height;
  initcanvas(bcolor,fcolor,scale,clear);
end;

procedure restorefromprint; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  xycanvas := xypaintbox.canvas; res := 1;
  cwidth := xypaintbox.width; cheight := xypaintbox.height;
  xypen := xycanvas.pen; xybrush := xycanvas.brush; xyfont := xycanvas.font;
  with xybrush do
     begin color := backcolor; style := bssolid; end;
  with xypen do
     begin color := frontcolor; style := pssolid; width := res; mode := pmcopy; end;
  if (frontcolor<>backcolor) then coordcolor := frontcolor
     else coordcolor := not backcolor;
  selectfont(fsize);
  tsize := xycharwidth + 1; hsize := tsize div 2 + 1;
end;

procedure xycleargraph(paintbox:Tpaintbox; bcolor,fcolor:Tcolor; scale:single);
 {initiatie van grafiek velden}
var l : integer;
    s : string;
begin
  if init then initxygraph;
  if crosson then xypaintbox.invalidate;
  xypaintbox := paintbox; cvmode := false;
  if doprint or dowmf then with xypaintbox do with canvas do
     begin
       font.size := 16; font.style := [fsbold];
       if doprint then s := 'PRINTING ...' else s := 'SAVING ...';
       l := textwidth(s) div 2; textout(width div 2 - l,height div 2 - 8,s);
       font.size := 8; font.style := [];
     end;
  if doprint then xycanvas := printer.canvas else
                  xycanvas := xypaintbox.canvas;
  cwidth := xypaintbox.width*res; cheight := xypaintbox.height*res;
  initcanvas(bcolor,fcolor,scale,not dowmf);
  if dowmf then initwmf;
end;

procedure restartgraph(x1,x2,y1,y2:integer;st:boolean);
var i : integer;
begin
  {x1 := x1*res; x2 := x2*res; y1 := y1*res; y2 := y2*res;}
  xorg := x1; xlen := (x2-x1); yorg := y2; ylen := y2-y1;
  grafok := (xlen>0) and (ylen>0);
  if st then begin stil := true; exit; end;
  graphs[igraph].ok := grafok;
  if not stil then begin
     for i := 0 to 4 do graphs[igraph].as_[i].on := false; coordas := 0; end;
  xygraphdata[igraph].ok := grafok;
  xygraphdata[igraph].x1 :=x1; xygraphdata[igraph].x2 :=x2;
  xygraphdata[igraph].y1 :=y1; xygraphdata[igraph].y2 :=y2;
  {xyunzoom(igraph);}
  stil := st;
end;

procedure xystartgraph(x1,x2,y1,y2,dx1,dx2,dy1,dy2:integer; nclip:boolean); {XXXX}
 {set tekenveld van grafiek}
var i : integer;
begin
  if (ngraph=maxgraf) then begin grafok := false; exit; end;
  xyfinish; inc(ngraph); igraph := ngraph; xybuffers[0].ngr := ngraph;
  if (x1>x2) then begin i := x1; x1 := x2; x2 := i; end;
  if (x1<0) then x1 := 0; if (x2>100) then x2 := 100;
  x1 := round(x1/100*cwidth)+xoff; graphs[igraph].x1 := x1;
  x2 := round(x2/100*cwidth)+xoff; graphs[igraph].x2 := x2;
  if (y1>y2) then begin i := y1; y1 := y2; y2 := i; end;
  if (y1<0) then y1 := 0; if (y2>100) then y2 := 100;
  y1 := round(y1/100*cheight)+yoff; graphs[igraph].y1 := y1;
  y2 := round(y2/100*cheight)+yoff; graphs[igraph].y2 := y2;
  graphfield.x1 := x1; graphfield.x2 := x2;
  graphfield.y1 := y1; graphfield.y2 := y2;
  x1 := x1+dx1*res; x2 := x2 - dx2*res;
  y1 := y1+dy1*res; y2 := y2 - dy2*res;
  plotfield.x1 := x1; plotfield.x2 := x2;
  plotfield.y1 := y1; plotfield.y2 := y2;
  xorg := x1; xlen := (x2-x1); yorg := y2; ylen := y2-y1;
  grafok := (xlen>0) and (ylen>0); graphs[igraph].ok := grafok;
  zmaxy := 0; yxratio := -1; mode3d := false;
  if (ylen>0) then aspect := xlen/ylen else aspect := 1;
  noclip := nclip and (graphs[igraph].xf0=0) and (graphs[igraph].xf1=1)
                  and (graphs[igraph].yf0=0) and (graphs[igraph].yf1=1);
  graphs[igraph].nclip := noclip; grafoky := false;
  graphs[igraph].dat := nil; graphs[igraph].trck := 0;
  for i := 0 to 4 do graphs[igraph].as_[i].on := false; coordas := 0;
  with graphs[igraph].as_[0] do begin org := xorg; len := xlen; end;
  xylinewidth(1); xypen.style := pssolid;
  xybrush.color := backcolor; xybrush.style := bssolid;
  xygraphdata[igraph].ok := grafok;
  xygraphdata[igraph].x1 :=x1; xygraphdata[igraph].x2 :=x2;
  xygraphdata[igraph].y1 :=y1; xygraphdata[igraph].y2 :=y2;
  xysetreduction(-1,-1); xysetlinestyle(-1,0,0);
  graphs[igraph].polar := 0; polartype := 0; stereo := false;
  frontcolor := oldfcolor; backcolor := oldbcolor;
  if dowmf then rop2wmf(r2_copypen) else setrop2(xycanvas.handle,r2_copypen);
  start3d; status := 2;
end;

procedure xysetgridlines(xcoarse,xfine,ycoarse,yfine:integer); {XXXXXXXXXXXXXXX}
 {set de parameter voor de raster lijnen}
var i : integer;
begin
  if (igraph=0) then for i := 1 to maxgraf do with graphs[i] do
    begin xgridc := xcoarse; xgridf := xfine;
          ygridc := ycoarse; ygridf := yfine; end
  else with graphs[igraph] do
    begin xgridc := xcoarse; xgridf := xfine;
          ygridc := ycoarse; ygridf := yfine; end
end;

procedure xysetreduction(xr,yr:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  xred := xr; yred := yr; nored := (xr<0) and (yr<0);
end;

procedure xysetratio(r:single); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin if (r<0) then yxratio := 0 else yxratio := r; end;

procedure xyxaxis(cl:Tcolor;x1,x2,incr,zmx:single;txt:string;grid,log,fix:boolean);
  {tekent x-as}
var grof,fijn,p,pp,tp : single;
    i,l,xp,n1,n2,n3,px,yp,nf,rf,lext : integer;
    s,ext : string;
    draw,reverse : boolean;
begin
   xyfinish;
   grafokx := (x1<>x2) and grafok and (mode3d or (x1<x2));
   if not grafokx then exit;
   reverse := (x1>x2); if reverse then rf := -1 else rf := 1;
   if mode3d then log := false;
   x1 := x1*rf; x2 := x2*rf; status := 3;
   if (x1<=0) then log := false;
   if (cl=-255) then begin draw := true; cl := backcolor; end
   else draw := (colortorgb(cl)<>colortorgb(backcolor));
   if not draw then cl := frontcolor;
   xypen.color := cl; xyfont.color := cl;
   xybrush.Style := bsclear;
   if dowmf then
     begin checkwmfpen; checkwmffont; {checkwmfbrush;}
        textwmfalign(6); initgrid; end;
   xlog := log; xincr := incr; xfix := fix; useroff := false;
   xyzoomx := graphs[igraph].xf1-graphs[igraph].xf0;
   l := pos('|',txt); if (l=0) then ext := '' else
      begin ext := copy(txt,l+1,length(txt)); txt := copy(txt,1,l-1); end;
   if (ext='') then lext := 0 else lext := xycanvas.textwidth(ext);

   if draw then xyline(-1,xorg,yorg,xorg+xlen+1,yorg);

   if not xlog then
     begin
       yspan := (x2-x1)/aspect;
       xmi := x1 + graphs[igraph].xf0*(x2-x1);
       xma := x1 + graphs[igraph].xf1*(x2-x1);
     end
   else
     begin
       xmi := x1*power(x2/x1,graphs[igraph].xf0);
       xma := x1*power(x2/x1,graphs[igraph].xf1);
       if (xma/xmi<lmax) then xlog := false;
     end;

   if xlog then
     begin
       zmaxx := zmax; nincx := 0;
       xypixx := power(xma/xmi,1/xlen); dxlog := ln(xma/xmi);
       logverdeling(xma/xmi,xlen,fijn);
       n1 := round(fijn) div 100;
       n2 := round(fijn) div 10 mod 10;
       n3 := round(fijn) mod 10;

       p := int(log10(xmi))-1;
       while (posx(power(10,p))<=xorg+xlen) do
         begin
           tp := power(10,p);
           for i := 10 to 99 do
             begin
               pp := tp*i/10; px := posx(pp);
               if (px>=xorg) and (px<=xorg+xlen) then
                 begin
                   if i in logtics[n2] then
                     begin
                      if draw then xyline(-1,px,yorg,px,yorg+tsize);
                      if grid then xgrid(px,false);
                     end
                   else
                   if i in logtics[n3] then
                     begin
                       if draw then xyline(-1,px,yorg,px,yorg+hsize);
                       if grid then xgrid(px,true);
                     end;
                   if draw then
                    if (i in logtics[n1]) and not (i in [70,90]) then
                     begin
                      s := logtxt(round(p),round(i/10));
                      l := xycanvas.textwidth(s);
                      if dowmf then textwmfout(px,yorg+tsize+res-1,s) else
                      xycanvas.textout(px-l div 2,yorg+tsize+res-1,s);
                     end;
                 end;
             end;
           p := p+1;
         end;
       ndecx := ndecim(xypixx-1)+1;
     end

   else{lin}

     begin
       zmaxx := zmx/(x2-x1); if (zmaxx<zmax) then zmaxx := zmax;
       xypixx := (xma-xmi)/xlen;
       verdeling(xmi,xma,incr,xlen,5*xycharwidth+lext,grof,fijn,ndecx);
       nincx := ndecim(incr); dxlog := 1; xincr := incr;
       if (ndecx>nincx) then ndecx := nincx; nf := round(grof/fijn);

       p := grof*trunc(xmi/grof-1);
       while (posx(p)<=xorg+xlen) do
         begin
           xp := posx(p);
           if (xp>=xorg) then
             begin
               if draw then
                 begin
                   xyline(-1,xp,yorg,xp,yorg+tsize);
                   if fix then str(p*rf:1:plus(ndecx),s) else
                     begin if (abs(p)<grof/2) then p := 0; s := lintext(p*rf,ndecx); end;
                   s := s + ext; l := xycanvas.textwidth(s);
                   if dowmf then textwmfout(xp,yorg+tsize+res-1,s) else
                   xycanvas.textout(xp-l div 2,yorg+tsize+res-1,s);
                 end;
               if grid then xgrid(xp,false);
             end;
           for i := 1 to nf-1 do
             begin
               xp := posx(p+i*fijn);
               if (xp>=xorg) and (xp<=xorg+xlen) then
                 begin
                   if draw then xyline(-1,xp,yorg,xp,yorg+hsize);
                   if grid then xgrid(xp,true);
                 end;
             end;
           p := p+grof;
         end;
       ndecx := ndecim(xypixx);
     end;

   if (txt<>'') and draw then
     begin
       l := xycanvas.textwidth(txt);
       xp := xorg+xlen div 2; yp := yorg+tsize+xycharheight;
       if dowmf then textwmfout(xp,yp,txt)
        else xycanvas.textout(xp - l div 2,yp,txt);
     end;
   if dowmf and draw then dogrid(yorg,yorg-ylen,true);

   if stil then exit;
   xmi := xmi*rf; xma := xma*rf;

   with graphs[igraph].as_[0] do begin on := true; log := xlog; col := cl;
     mi := xmi; ma := xma; dlog := dxlog; org := xorg; len := xlen; side := false;
     ndec := ndecx; ninc := nincx; pix := xypixx; typ := 0;
     graphs[igraph].zmxx := zmaxx;
     if xlog then incr := 0 else incr := xincr;
     end;
   with xygraphdata[igraph].xaxis do
     begin on := true; min := xmi; max := xma; log := xlog; end;
end;

procedure xyyearaxis(cl:Tcolor;code,jaar:integer;grid:boolean); {XXXXXXXXXXXXXX}
  {speciale x-as bij tijdseenheden; code 1= per dag; 2=per week; 3=per maand}
const maandnaam  = ('JFMAMJJASOND');
var i,xp,yp,l,t,n,eerste: integer;
    c : char;
    s : string;
    st : TsystemTime;
    dt : Tdatetime;
    draw : boolean;
begin
  xyfinish; useroff := false;
  grafokx := grafok and (code in [1..3]); if not grafokx then exit;
  draw := (colortorgb(cl)<>colortorgb(backcolor));
  if (not draw) and (code<>2) then cl := frontcolor;
  xypen.color := cl; xyfont.color := cl;
  xybrush.Style := bsclear;
  if dowmf then
    begin checkwmfpen; textwmfalign(6); initgrid; end;
  xlog := false; xfix := true; nincx := 0; xincr := 1; status := 3;
  xyzoomx := graphs[igraph].xf1-graphs[igraph].xf0;

  if (jaar=0) then
    begin datetimetosystemtime(date,st); jaar := st.wyear; end;
  dt := encodedate(jaar,1,1); eerste := dayofweek(dt); {zon=1 zat=7}

  dag[1] := 0; dag[2] := maanddagen[1];
  dag[3] := dag[2]+maanddagen[2]; if isleapyear(jaar) then inc(dag[3]);
  for i := 4 to 13 do dag[i] := dag[i-1] + maanddagen[i-1];
  yxratio := 0; yspan := 0;

  case code of
  1 : begin
        xmi := graphs[igraph].xf0*(dag[13]+1);
        xma := graphs[igraph].xf1*(dag[13]+1);
        xypixx := (xma-xmi)/xlen; zmaxx := 2/365;
        if draw then
          begin
            xyline(-1,xorg,yorg,xorg+xlen+1,yorg);
            if (xypixx<0.3) then for i := 0 to dag[13] do
              begin xp := posx(i); if (xp>=xorg) and (xp<=xorg+xlen) then
                begin
                  xyline(-1,xp,yorg,xp,yorg+hsize);
                  if ( (xypixx<0.1) and odd(i) ) or (xypixx<0.05) then
                    begin
                      t := 1; while (dag[t+1]<i) and (t<13) do inc(t);
                      n := i - dag[t]; str(n,s); l := xycanvas.textwidth(s);
                      if dowmf then textwmfout(xp,yorg+hsize+res-1,s) else
                      xycanvas.textout(xp-l div 2,yorg+hsize+res-1,s);
                    end;
                end;
              end;
           end;
        for i := 1 to 13 do
          begin xp := posx(dag[i]+0.5); if (xp>=xorg) and (xp<=xorg+xlen) then
            begin
              if draw then xyline(-1,xp,yorg,xp,yorg+tsize);
              if grid then xgrid(xp,false);
            end;
          end;
        n := 2-eerste-7;
        if grid then repeat
           n := n + 7; xp := posx(n);
           if (xp>=xorg) and (xp<=xorg+xlen) then xgrid(xp,true);
        until (n>366);

        if draw then begin
        xyfont.style := [fsbold]; xypen.color := backcolor;
        xybrush.color := backcolor; xybrush.style := bssolid;
        yp := yorg+hsize+res*2;
        for i := 1 to 12 do
          begin
           c := maandnaam[i]; l := xycanvas.textwidth(c);
           xp := ( posx(dag[i])+posx(dag[i+1])) div 2;
           if (xp>xorg+hsize) and (xp<xorg+xlen-hsize) then
             begin
               xyrectangle(-1,-1,xp-xycharwidth,yp-2,xp+xycharwidth,yp+xycharheight);
               if dowmf then
                 begin checkwmffont; textwmfout(xp,yp,c); end
               else xycanvas.textout(xp - l div 2,yp,c);
             end;
          end; end;
         xypen.color := cl; xyfont.style := [];
         ndecx := ndecim(xypixx);
       end;
  2 : begin
        if (taal=1) then s := 'weeek nummer'
                    else s := 'number of week';
        xyxaxis(cl,0,53,1,2,s,grid,lin,fixed);
        with graphs[igraph].as_[0] do begin ndec := 0; ninc := 0; typ := 2; end;
        exit;
      end;
  3 : begin
        xmi := 0.5 + graphs[igraph].xf0*12;
        xma := 0.5 + graphs[igraph].xf1*12;
        xypixx := (xma-xmi)/xlen; zmaxx := 2/12;
        if draw then
          begin
            xyline(-1,xorg,yorg,xorg+xlen,yorg);
            xyfont.style := [fsbold];
          end;
        for i := 1 to 12 do
          begin
            xp := posx(i); if (xp>=xorg) and (xp<=xorg+xlen) then
              begin
                if grid then xgrid(xp,false);
                if draw then
                  begin
                    xyline(-1,xp,yorg,xp,yorg+tsize);
                    c := maandnaam[i]; l := xycanvas.textwidth(c);
                    if dowmf then
                        begin checkwmffont; textwmfout(xp,yorg+tsize{+res-1},c); end
                    else xycanvas.textout(xp- l div 2,yorg+tsize{+res-1},c);
                  end;
              end;
           end;
        if grid then for i := 1 to 11 do
          begin
            xp := posx(i+0.5);
            if (xp>=xorg) and (xp<=xorg+xlen) then xgrid(xp,true);
          end;
        xyfont.style := [];
        ndecx := ndecim(xypixx);
      end;
  end;
  if dowmf then dogrid(yorg,yorg-ylen,true);
  grafokx := true;

   with graphs[igraph].as_[0] do begin on := true; log := xlog; col := cl;
     mi := xmi; ma := xma; dlog := dxlog; org := xorg; len := xlen; side := false;
     ndec := ndecx; ninc := nincx; pix := xypixx; typ := code; incr := xincr;
     graphs[igraph].zmxx := zmaxx; end;
   with xygraphdata[igraph].xaxis do
     begin on := true; min := xmi; max := xma; log := xlog; end;
end;

procedure xysetmonthnames(mnames:Tmonthnames); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var i,l : integer;
begin
  for i := 1 to 12 do monthnames[i] := mnames[i];
  ld2 := 0; for i := 1 to 12 do
   begin l := length(monthnames[i]); if (l>ld2) then ld2 := l; end;
end;

procedure initdate; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var s : string;
    i,l : integer;
begin
  s := lowercase(FormatSettings.shortdateformat); dm := (pos('d',s)<pos('m',s));
  ld1 := 0; for i := 1 to 12 do
    begin l := length(FormatSettings.shortmonthnames[i]); if (l>ld1) then ld1 := l; end;
  ld2 := 0; for i := 1 to 12 do
    begin l := length(monthnames[i]); if (l>ld2) then ld2 := l; end;
  dtmin := encodedate(1,1,1);
end;

procedure xytimeaxis(cl:Tcolor;x1,x2:Tdatetime;txt:string; {XXXXXXXXXXXXXXXXXXX}
  mode,dtcode,dcode,mcode,ycode:integer; grid:boolean; var tmode:boolean);
  {tekent tijd x-as}
var l,xp,yp,yp1,yp2,yp3,lw,lw1,lw2,lw3,xend,i,dow,dx,
       timescale,lines,tel,tel2,inc1,inc2,p: integer;
    draw : boolean;
    d,m,y,h,n,s,ms:word;
    dt,dt0,step1,step2,f : double;
    dl,dl1,dl2,dl3 : single;
    color1 : Tcolor;
const hrs : double = 24;         ihrs : double = 1/24;
      mins : double = 24*60;     imins : double = 1/24/60;
      secs : double = 24*60*60;  isecs : double = 1/24/60/60;
   rij1 : array[1..12,1..2] of byte = {seconds and minutes}
    ((1,1),(2,1),(3,1),(4,1),(5,1),(6,2),(10,5),(12,3),(15,5),(20,5),(30,10),(60,15));
   rij2 : array[1..8,1..2] of byte =  {hours}
    ((1,1),(2,1),(3,1),(4,1),(6,2),(8,4),(12,3),(24,6));
   rij3 : array[1..5,1..2] of byte =  {days}
    ((1,1),(2,1),(3,1),(4,1),(7,1));
    {er is geen rij voor weken, die gaan apart}
   rij4 : array[1..6,1..2] of byte =  {months}
    ((1,1),(2,1),(3,1),(4,1),(6,2),(12,6));
   rij5 : array[1..12,1..2] of byte = {years}
    ((1,1),(2,1),(4,1),(5,1),(10,2),(15,5),(20,5),
       (25,5),(30,10),(40,10),(50,10),(100,20));
procedure tic; {---------------------------------------------------------------}
begin
  if (xp<xorg) or (xp>xend) then exit;
  if draw then xyline(-1,xp,yorg,xp,yorg+hsize);
  if grid then xgrid(xp,false);
end;
procedure ticplus(dt:double); {------------------------------------------------}
var s : string; l,yp : integer;
begin
  if (xp<xorg) or (xp>xend) then exit;
  xypen.color := color1; xyfont.color := color1;
  if grid then xgrid(xp,false);
  if not draw then exit;

  xyline(-1,xp,yorg,xp,yorg+hsize);

  if (dt>=dtmin) then
    begin
      decodedate(dt,y,m,d); str(d:1,s);
      yp := yorg+hsize;
      l := xycanvas.textwidth(s); inc(tel2);
      if (lines=0) then lines := 1;
      if dowmf then textwmfout(xp,yp,s) else xycanvas.textout(xp-l div 2,yp,s);
    end;
  xypen.color := cl; xyfont.color := cl;
end;
procedure plotdate(y,m,d:word); {----------------------------------------------}
var sd,sm,sy,s1,s2,s3:string;
begin
  if (xp<xorg) or (xp>xend) then exit;

  if grid then xgrid(xp,true);
  if not draw then exit;

  xyline(-1,xp,yorg,xp,yorg+tsize);

  if (m=0) then decodedate(dt,y,m,d);

  if (timescale=0) then
  begin
    sd := inttostr(d);
    if (mcode=0) then sm := inttostr(m) else
    if (mcode=1) then sm := FormatSettings.shortmonthnames[m] else
                      sm := monthnames[m];
    if (ycode=0) then sy := '' else
      begin sy := inttostr(y); if (ycode=1) then sy := copy(sy,3,2); end;
    if dm then
    if (dcode=0) then begin s1 := sd;        s2 := sm; s3 := sy; end else
    if (mcode=0) then begin s1 := sd+'-'+sm; s2 := sy; s3 := ''; end else
                      begin s1 := sd+' '+sm; s2 := sy; s3 := ''; end
    else
    if (dcode=0) then begin s1 := sm;        s2 := sd; s3 := sy; end else
    if (mcode=0) then begin s1 := sm+'-'+sd; s2 := sy; s3 := ''; end else
                      begin s1 := sm+' '+sd; s2 := sy; s3 := ''; end;
  end else
  begin
    if (timescale=1) then s1 := formatdatetime('hh:mm',dt)
       else s1 := formatdatetime('hh:mm:ss',dt);
    if (dtcode=0) then s2 := '' else
      if dm then s2 := inttostr(d)+'-'+inttostr(m) else
                 s2 := inttostr(m)+'-'+inttostr(d);
    s3 := '';
  end;

  l := xycanvas.textwidth(s1); lines := 1; inc(tel2);
  if dowmf then textwmfout(xp,yp1,s1) else xycanvas.textout(xp-l div 2,yp1,s1);
  if (s2<>'') then begin l := xycanvas.textwidth(s2); lines := 2;
  if dowmf then textwmfout(xp,yp2,s2) else xycanvas.textout(xp-l div 2,yp2,s2); end;
  if (s3<>'') then begin l := xycanvas.textwidth(s3); lines := 3;
  if dowmf then textwmfout(xp,yp3,s3) else xycanvas.textout(xp-l div 2,yp3,s3); end;
end;
procedure series; {------------------------------------------------------------}
var i : integer;
begin
  repeat
    xp := posx(dt); plotdate(0,0,0);
    for i := 1 to tel-1 do begin xp := posx(dt+step2*i); tic; end;
    dt := dt + step1;
  until (xp>xorg+xlen);
end;
procedure doesec(dl:double); {-------------------------------------------------}
var x,p,t : integer;
begin
  x := length(rij1); t := 1; p := trunc(dl);
  while (p>=rij1[t,1]) and (t<x) do inc(t);
  inc1 := rij1[t,1]; inc2 := rij1[t,2];
  if (inc1<60) then timescale := 2;
  tel := inc1 div inc2;  s := s - (s mod inc1);
  dt := encodedate(y,m,d) + encodetime(h,n,s,0);
  step1 := isecs*inc1; step2 := isecs*inc2; series;
end;
procedure doemin(dl:double); {-------------------------------------------------}
var x,p,t : integer;
begin
  x := length(rij1); t := 1; p := trunc(dl/60);
  while (p>=rij1[t,1]) and (t<x) do inc(t);
  inc1 := rij1[t,1]; inc2 := rij1[t,2];
  tel := inc1 div inc2; n := n - (n mod inc1);
  dt := encodedate(y,m,d) + encodetime(h,n,0,0);
  step1 := imins*inc1; step2 := imins*inc2; series;
end;
procedure doeuur(dl:double); {-------------------------------------------------}
var x,p,t : integer;
begin
  x := length(rij2); t := 1; p := trunc(dl/3600);
  while (p>=rij2[t,1]) and (t<x) do inc(t);
  inc1 := rij2[t,1]; inc2 := rij2[t,2];
  tel := inc1 div inc2;  h := h - (h mod inc1);
  dt := encodedate(y,m,d) + encodetime(h,0,0,0);
  step1 := ihrs*inc1; step2 := ihrs*inc2; series;
end;
procedure doedag(dl:double); {-------------------------------------------------}
var x,p,t : integer;
begin
  x := length(rij3); t := 1; p := trunc(dl);
  while (p>=rij3[t,1]) and (t<x) do inc(t);
  inc1 := rij3[t,1]; inc2 := rij2[t,2];
  d := d - ((d-1) mod inc1);
  tel := inc1 div inc2; dt := encodedate(y,m,d);
  step1 := inc1; step2 := inc2; series;
end;
procedure doeweek(dl:double); {------------------------------------------------}
procedure week(w:array of integer);
var i : integer;
begin
  repeat
    dt := encodedate(y,m,1);
    for i := 0 to 4 do if (w[i]>0) then
       begin
        xp := posx(dt+i*7.0); if (w[i]=2) then plotdate(y,m,1+i*7) else tic;
       end;
    if (w[1]=2) then if (xypixx<0.25) then
        for i := 1 to 30 do begin xp := posx(dt+i); tic; end;
    inc(m); if (m=13) then begin inc(y); m := 1; end;
   until (xp>xorg+xlen);
end;
begin
   if (dl<7)  then week([2,2,2,2,1]) else
   if (dl<14) then week([2,1,2,1,1]) else
   if (dl<31) then week([2,0,1,0,0]) else
                   week([2,0,0,0,0]);
end;
procedure doemaand(dl:double); {------------------------------------------------}
var x,p,t,i : integer;
begin
  x := length(rij4); t := 1; p := trunc(dl/30.5);
  while (p>=rij4[t,1]) and (t<x) do inc(t);
  inc1 := rij4[t,1]; inc2 := rij4[t,2];
  m := m - (m-1) mod inc1; tel := inc1 div inc2;
  repeat
  dt := encodedate(y,m,1); xp := posx(dt); plotdate(y,m,1);
  for i := 1 to tel-1 do
     begin dt := encodedate(y,m+i*inc2,1); xp := posx(dt); tic; end;
  inc(m,inc1); if (m>12) then begin inc(y); dec(m,12); end;
  until (xp>xorg+xlen);
end;
procedure doejaar(dl:double); {------------------------------------------------}
var x,p,t,i : integer;
begin
  x := length(rij5); t := 1; p := trunc(dl/365.25);
  while (p>=rij5[t,1]) and (t<x) do inc(t);
  inc1 := rij5[t,1]; inc2 := rij5[t,2];
  y := y - y mod inc1; tel := inc1 div inc2;
  repeat
  dt := encodedate(y,1,1); xp := posx(dt); plotdate(y,1,1);
  for i := 1 to tel-1 do
     begin dt := encodedate(y+i*inc2,1,1); xp := posx(dt); tic; end;
  inc(y,inc1);
  until (xp>xorg+xlen);
end;
begin {++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
   xyfinish; timescale := 0;
   grafokx := (x1<=x2) and grafok;
   if not grafokx then exit;
   if (cl=-255) then begin draw := true; cl := backcolor; end
   else draw := (colortorgb(cl)<>colortorgb(backcolor));
   if not draw then cl := frontcolor;
   xypen.color := cl; xyfont.color := cl;
   color1 := mixcolor(cl,backcolor,1,2);
   xybrush.Style := bsclear; lines := 0; status := 3;

   if (mcode=0) then l := 2 else if (mcode=1) then l := ld1 else l := ld2;
   if (dcode=0) then l := max(l,2) else l := l+3;
   if (ycode=2) then l := max(l,4);
   lw := (l+1) * xycharwidth;  {datum}
   lw1 := (5+1) * xycharwidth; {dag plus}
   lw2 := (5+1) * xycharwidth; {tijd zonder sec}
   lw3 := (8+1) * xycharwidth; {tijd met sec}

   if dowmf then
     begin checkwmfpen; checkwmffont; {checkwmfbrush;}
        textwmfalign(6); initgrid; end;
   xlog := false; xincr := 0; xfix := true; useroff := false;
   xyzoomx := graphs[igraph].xf1-graphs[igraph].xf0;
   if (x1<dtmin) then x1 := dtmin;
   if (x2<dtmin) then x2 := dtmin;
   if (mode>=0) then f := 2 else f := isecs+isecs;
   if (x2-x1<f) then
       begin x1 := max(dtmin,(x1+x2-f)/2); x2 := x1+f; end;
   yp1 := yorg+tsize+res-1;
   yp2 := yp1 + round(xycharheight*0.8);
   yp3 := yp1 + round(xycharheight*1.6);

   if draw then xyline(-1,xorg,yorg,xorg+xlen+1,yorg);

   yspan := 0; xend := xorg+xlen;
   xmi := x1 + graphs[igraph].xf0*(x2-x1);
   xma := x1 + graphs[igraph].xf1*(x2-x1);

   zmaxx := 2/(x2-x1); if (Mode<0) then zmaxx := zmaxx/24/60/60;
   xypixx := (xma-xmi)/xlen; {lengte per pixel}
   dl  := lw * xypixx;  {afstand per label}
   dl1 := lw1 * xypixx; {afstand per label}
   dl2 := lw2 * xypixx; {afstand per label}
   dl3 := lw3 * xypixx; {afstand per label}
   if (mode>=0) then xincr := 1 else xincr := 0; nincx := 0;

   decodedate(xmi,y,m,d); decodetime(xmi,h,n,s,ms);

   if (mode>0) then begin

   dx := trunc(dl/7)+1; dow := dayofweek(xmi); tel2 := 0;
   dt := encodedate(y,m,d) - (dow-mode) - 7.0*(dx-1); dt0:= dt;
   repeat
     xp := posx(dt); plotdate(0,0,0);
     if (xypixx<3.5) then for i := 1 to dx-1 do
        begin xp := posx(dt+i*7); tic; end;
     if (dx=1) then if (xypixx<0.25) then
        for i := 1 to 6 do begin xp := posx(dt+i);
          if (dl<1) then ticplus(dt+i) else ticplus(dtmin-1); end;
     dt := dt + 7.0*dx;
   until (xp>xend);

   end else

   if (mode<0) and (dl3<1.5) then {time mode, geef tijd}
   begin
     dl := dl3*secs; {een seconde}
     timescale := 1;
          if (dl<60)    then doesec(dl)
     else if (dl<3600)  then doemin(dl)
     else if (dl<86400) then doeuur(dl)
     else begin timescale := 0; doedag(dl3); end; {overgang naar datemode}
   end
   else {date mode, geef geen tijd}
   begin
     timescale := 0;
          if (dl<4)   then doedag(dl)
     else if (dl<31)  then doeweek(dl)
     else if (dl<366) then doemaand(dl)
     else                  doejaar(dl);
   end;
   tmode := (timescale>0);

   if (xypixx>30) then ndecx := 0 else
   if (mode>=0)   then ndecx := 1 else
   if (xypixx>1)     then ndecx := 1 else
   if (xypixx>ihrs)  then ndecx := 2 else
   if (xypixx>imins) then ndecx := 3 else
                          ndecx := 4;

   if (txt<>'') and draw then
     begin
       p := pos('|',txt); if (p>0) then
         if tmode then txt := copy(txt,p+1,length(txt)-p)
                  else txt := copy(txt,1,p-1);
       if (lines<2) then yp := yp1 else
       if (lines=2) then yp := yp2 else yp := yp3;
       l := xycanvas.textwidth(txt);
       xp := xorg+xlen div 2; yp := yp+xycharheight;
       if dowmf then textwmfout(xp,yp,txt)
         else xycanvas.textout(xp - l div 2,yp,txt);
     end;
   if dowmf and draw then dogrid(yorg,yorg-ylen,true);

   if stil then exit;

   with graphs[igraph].as_[0] do begin on := true; log := xlog; col := cl;
     mi := xmi; ma := xma; dlog := dxlog; org := xorg; len := xlen; side := false;
     ndec := ndecx; ninc := nincx; pix := xypixx; typ := 4; incr := xincr;
     graphs[igraph].zmxx := zmaxx;
     end;
   with xygraphdata[igraph].xaxis do
     begin on := true; min := xmi; max := xma; log := xlog; end;
end;

procedure xylabelaxis(cl:Tcolor;labels:array of string;grid:boolean); {XXXXXXXX}
var i,n,d,n1,n2,l,xp : integer;
    x1,x2 : single;
    s : string;
    draw : boolean;
begin
  xyfinish; useroff := false;
  n1 := 0; if (labels[0]='') then n1 := 1;
  n2 := length(labels)-1;
  n := n2-n1 + 1; yspan := 0;
  grafokx := grafok and (n>0); if not grafokx then exit;

  draw := (colortorgb(cl)<>colortorgb(backcolor));
  if not draw then cl := frontcolor;
  xypen.color := cl; xyfont.color := cl;
  xybrush.Style := bsclear;

  if dowmf and draw then
    begin checkwmfpen; textwmfalign(6); end;
  if dowmf then initgrid;
  xlog := false; xfix := true; nincx := 0; xincr := 1; status := 3;
  xyzoomx := graphs[igraph].xf1-graphs[igraph].xf0;

  x1 := 0.5; x2 := n+1-x1; yxratio := 0;
  xmi := x1 + graphs[igraph].xf0*(x2-x1);
  xma := x1 + graphs[igraph].xf1*(x2-x1);
  xypixx := (xma-xmi)/xlen; zmaxx := 2/n;
  if draw then xyline(-1,xorg,yorg,xorg+xlen+1,yorg);
  d := trunc(1/xypixx)-(xycharwidth div 3);
  for i := 1 to n do
    begin
      xp := posx(i); if (xp>=xorg) and (xp<=xorg+xlen) then
        begin
          if grid then xgrid(xp,false);
          if draw then
            begin
              xyline(-1,xp,yorg,xp,yorg+tsize);
              s := labels[i-1+n1]; l := xycanvas.textwidth(s);
              while (l>d) and (length(s)>1) do
                begin s := copy(s,1,length(s)-1); l := xycanvas.textwidth(s); end;
              if dowmf then begin checkwmffont; textwmfout(xp,yorg+tsize,s); end
               else xycanvas.textout(xp- l div 2,yorg+tsize,s);
            end;
        end;
    end;
  if grid then for i := 1 to n-1 do
    begin
      xp := posx(i+0.5);
      if (xp>=xorg) and (xp<=xorg+xlen) then xgrid(xp,true);
    end;
  ndecx := ndecim(xypixx);
  if dowmf then dogrid(yorg,yorg-ylen,true);

  grafokx := true;
   with graphs[igraph].as_[0] do begin on := true; log := xlog; col := cl;
     mi := xmi; ma := xma; dlog := dxlog; org := xorg; len := xlen; side := false;
     ndec := ndecx; ninc := nincx; pix := xypixx; typ := 0; incr := xincr;
     graphs[igraph].zmxx := zmaxx; end;
   with xygraphdata[igraph].xaxis do
     begin on := true; min := xmi; max := xma; log := xlog; end;
end;

procedure xyyaxis(cl:Tcolor;y1,y2,incr,zmx:single;txt:string;code:integer;grid,log,fix:boolean);
 {tekent y-as, code =positie}
var grof,fijn,p,pp,tp,z,dy,yc,rf : single;
    xp,bi,ltxt,i,l,h,py,n1,n2,n3,yp,xp2,nf : integer;
    s : string;
    draw,reverse,sidew : boolean;
begin
   xyfinish;
   grafoky := (y1<>y2) and grafok and (code in [1..8]) and
        (stil or (grafokx and ( (mode3d or (y1<y2))))) ;
   if not grafoky then exit;
   reverse := (y1>y2); if reverse then rf := -1 else rf := 1;
   {if mode3d then log := false;}
   y1 := y1*rf; y2 := y2*rf;
   if (y1<=0) then log := false;
   xyzoomy := graphs[igraph].yf1-graphs[igraph].yf0;

   yincr := incr; nincy := ndecim(incr);
   ltxt := 0; useroff := false; status := 4;

   if (cl=-255) then begin draw := true; cl := backcolor; end
   else draw := (colortorgb(cl)<>colortorgb(backcolor));
   if not draw then cl := frontcolor;
   xypen.color := cl; xyfont.color := cl;
   xybrush.Style := bsclear;
   if dowmf then
     begin checkwmfpen; checkwmffont; {checkwmfbrush;} initgrid;
     if odd(code) then textwmfalign(2) else textwmfalign(0); end;
   ylog := log; sidew := (code>4);
   case code of
    1,5,6 : begin xp := xorg; bi := -1; end;
     2 :    begin xp := xorg+res; bi := 1; end;
     3 :    begin xp := xorg+xlen-res; bi := -1; end;
    4,7,8 : begin xp := xorg+xlen; bi := 1; end;
    end;
   if draw then xyline(-1,xp,yorg,xp,yorg-ylen-1);

   if not ylog then
     begin
       if (not xlog) and (yxratio>0) and (yspan>0) then
         begin
           dy := yspan*yxratio; yc := (y2+y1)/2;
           y1 := yc - dy/2; y2 := yc + dy/2;
           with graphs[igraph] do begin
           dy := (xf1-xf0); yc := (yf1+yf0)/2;
           yf0 := yc - dy/2;; yf1 := yc + dy/2;
         end; end;
       ymi := y1 + graphs[igraph].yf0*(y2-y1);
       yma := y1 + graphs[igraph].yf1*(y2-y1);
     end
   else
     begin
       ymi := y1*power(y2/y1,graphs[igraph].yf0);
       yma := y1*power(y2/y1,graphs[igraph].yf1);
       if (yma/ymi<lmax) then ylog := false;
     end;

   if ylog then
     begin
       zmaxy := zmax; nincx := 0;
       xypixy := power(yma/ymi,1/ylen); dylog := ln(yma/ymi);
       logverdeling(yma/ymi,ylen,fijn);
       n1 := round(fijn) div 100;
       n2 := round(fijn) div 10 mod 10;
       n3 := round(fijn) mod 10;

       p := int(log10(ymi))-1;
       while (posy(power(10,p))>=yorg-ylen) do
         begin
           tp := power(10,p);
           for i := 10 to 99 do
             begin
               pp := tp*i/10; py := posy(pp);
               if (py>=yorg-ylen) and (py<=yorg) then
                 begin
                   if i in logtics[n2] then
                     begin
                      if draw then xyline(-1,xp,py,xp+tsize*bi,py);
                      if grid then ygrid(py,false);
                     end
                   else
                   if i in logtics[n3] then
                     begin
                      if draw then xyline(-1,xp,py,xp+hsize*bi,py);
                      if grid then ygrid(py,true);
                     end;
                   if draw then if i in logtics[n1] then
                     begin
                       s := logtxt(round(p),round(i/10));
                       l := xycanvas.textwidth(s);
                       yp := py-xycharheight div 2;
                       if dowmf then textwmfout(xp+(tsize+res-1)*bi,yp,s)
                       else
                         if (bi<0) then
                              xycanvas.textout(xp-tsize-l-res+1,py-xycharheight div 2,s)
                         else xycanvas.textout(xp+tsize+res,py-xycharheight div 2,s);
                       if (l>ltxt) then ltxt := l;
                     end;
                 end;
             end;
           p := p+1;
         end;
       ndecy := ndecim(xypixy-1)+1;
     end

   else {lin}

     begin
       if (zmaxy=0) then zmaxy := zmx/(y2-y1);
       xypixy := (yma-ymi)/ylen;
       verdeling(ymi,yma,incr,ylen,2*xycharheight,grof,fijn,ndecy);
       nincy := ndecim(incr); yincr := incr;
       if (ndecy>nincy) then ndecy := nincy; nf := round(grof/fijn);

       p := grof*trunc(ymi/grof-1);
       while (posy(p)>=yorg-ylen) do
         begin
           if (posy(p)<=yorg) then
             begin
               if draw then
                 begin
                   yp := posy(p);
                   xyline(-1,xp,yp,xp+tsize*bi,yp);
                   if fix then str(p*rf:1:plus(ndecy),s) else
                     begin if (abs(p)<grof/2) then p := 0; s := lintext(p*rf,ndecy); end;
                   l := xycanvas.textwidth(s); if (l>ltxt) then ltxt := l;
                   yp := yp-xycharheight div 2;
                   if dowmf then textwmfout(xp+(tsize+res-1)*bi,yp,s)
                   else
                     if (bi<0) then
                          xycanvas.textout(xp-tsize-l-res+1,yp,s)
                     else xycanvas.textout(xp+tsize+res,yp,s);
                 end;
               if grid or ( (p=0) and not mode3d ) then ygrid(posy(p),false);
             end;
           for i := 1 to nf-1 do
             begin
               yp := posy(p+i*fijn);
               if (yp>=yorg-ylen) and (yp<=yorg) then
                 begin
                   if draw then xyline(-1,xp,yp,xp+hsize*bi,yp);
                   if grid then ygrid(yp,true);
                 end;
             end;
           p := p+grof;
         end;
       ndecy := ndecim(xypixy);
     end;

   if (txt<>'') and draw then
   begin

   h := yorg-ylen-xycharheight - xycharheight div 2;
   if (txt[1]='@') then begin h := h - xycharheight; txt := copy(txt,2,length(txt)); end;
   l := xycanvas.TextWidth(txt);
   if sidew then
   case code of
     5 : xytextangle(cl,txt,xorg     -ltxt-tsize,      yorg-(ylen/2), 0,-1,8,90);
     6 : vtext      (cl,txt,xorg     -ltxt-tsize-hsize,yorg-(ylen/2), 0, 0,8);
     7 : xytextangle(cl,txt,xorg+xlen+ltxt+tsize,      yorg-(ylen/2), 0, 1,8,90);
     8 : vtext      (cl,txt,xorg+xlen+ltxt+tsize+hsize,yorg-(ylen/2), 0, 0,8);
   end
   else if dowmf then
   case code of
     1 : begin textwmfalign(0); textwmfout(max(xorg-ltxt-tsize,xoff),h,txt); end;
     2 : begin textwmfalign(0); textwmfout(xorg,h,txt); end;
     3 : begin textwmfalign(2); textwmfout(xorg+xlen,h,txt); end;
     4 : begin textwmfalign(2); textwmfout(min(xorg+xlen+ltxt+tsize,cwidth+xoff),h,txt); end;
   end
   else
   case code of
     1 : xycanvas.textout(max(xorg-ltxt-tsize,xoff),h,txt);
     2 : xycanvas.textout(xorg,h,txt);
     3 : xycanvas.textout(xorg+xlen-l,h,txt);
     4 : xycanvas.textout(min(xorg+xlen+ltxt+tsize,cwidth+xoff)-l,h,txt);
   end;

   end;

   if dowmf then dogrid(xorg,xorg+xlen,false);
   xypen.mode := pmcopy;

   if stil then begin graphs[igraph].ndecy := ndecy; exit; end;
   if (code in [5,6]) then code := 1;
   if (code in [7,8]) then code := 4;

   ymi := ymi*rf; yma := yma*rf;

   with graphs[igraph].as_[code] do begin on := true; log := ylog; col := cl;
     mi := ymi; ma := yma; dlog := dylog; org := yorg; len := ylen;
     ndec := ndecy; ninc := nincy; pix := xypixy; typ := 0;
     if ylog then incr := 0 else incr := yincr;
     graphs[igraph].zmxy := zmaxy; side := sidew; end;
   with xygraphdata[igraph].yaxis[code] do
     begin on := true; min := ymi; max := yma; log := xlog; end;
   coordas := code;
   if (zgraph=0) then zgraph := igraph else
     if (zgraph>0) and (zgraph<>igraph) then zgraph := -1;
end;

function showdata(nx,ny:integer):string; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  result := 'N='+inttostr(nx);
  result := result + ' X='+signif(graphs[igraph].dat^[nx,0],6);
  if (track=2) then
  result := result + ' Y='+signif(graphs[igraph].dat^[nx,trackkl],6);
  if (track=3) then
  result := result + ' / NY='+inttostr(ny)+' Y='+signif(graphs[igraph].dat^[nx,ny],6);
end;

procedure xyplotarray(var data:Tdatatype;st,dt:integer;show:Tshowfunction); {XX}
var n1,n2,ny,i,y,n,sst,ssz,sfi : integer;
    xmi,xma,ymi,yma,d,w : single;
    singley,adv,doshow : boolean;
    ysst : array[1..4] of integer;
const ycol : array[1..4] of Tcolor = (clblack,clred,clblue,clgreen);
procedure plotkolom(k,st,a:integer);
var i,xst : integer;
begin
  if adv then {set parameters}
    begin
      st := round(data[1,k]); xypen.color := round(data[2,k]);
      xylinewidth(round(data[3,k])); xysetlinestyle(round(data[4,k]),0,0);
      sst := round(data[5,k]); ssz := round(data[6,k]); sfi := round(data[7,k]);
    end;
  if st in [0,1] then {teken lijn}
    begin
       xymove(data[n1,0],data[n1,k]);
       for i := n1+1 to n2 do xydraw(data[i,0],data[i,k]);
    end;
  if st in [0,2] then {teken punten}
    begin
      if (a=0) or adv then xst := sst else
        begin xst := ysst[a]; inc(ysst[a]); end;
      if not xst in [1..8] then xst := 0;
      for i := n1 to n2 do with lastsymbol do
        xysymbolp(xst,ssz,sfi,data[i,0],data[i,k]);
      xyfinish;
    end;
  if st in [3..5] then {teken bar}
    begin
      if (n1=n2) then w := 1 else w := data[n1+1,0]-data[n1,0];
      if (st=4) then w := w * 0.7 else
      if (st=5) then w := w * 0.4;
      for i := n1 to n2 do xybar(xypen.color,data[i,0],w,data[i,k],0);
    end;
  if adv then begin xylinewidth(1); xysetlinestyle(-1,0,0); end; {reset}
end;
begin
  if (mode3d) or (status=0) then exit;
  n2 := length(data)-1; if (n2<1) then exit;
  ny := length(data[1])-1; if (ny<1) then exit;
  adv := (data[0,0]>0.5); if adv then n1 := 8 else n1 := 1;
  if (n1>n2) then exit;
  doshow := (dt and 8=0); dt := dt and 7;
  singley := true; if (ny>1) then for y := 1 to ny do
    if (round(data[0,y]) in [1..4]) then singley := false;
  if singley and (dt=3) then dt := 2;

  if (status=1) then {maak plotveld}
    begin
      xystartgraph(0,100,0,100,35,20,25,35,false);
      if (status=1) then {mislukt} exit;
    end;
  if (status=2) then {maak x-as}
    begin
      xmi := data[n1,0]; xma := data[n2,0];
      if (xma<xmi) then exit else
      if (xma=xmi) then begin xmi := xmi-1; xma := xma+1; end else
        begin d := (xma-xmi)/20; xmi := xmi-d; xma := xma+d; end;
      xyxaxis(frontcolor,xmi,xma,0,0,xylabels[0],false,false,false);
      if (status=2) then {mislukt} exit;
    end;
  if (status=3) and singley then {maak y-as}
    begin
      ymi := data[n1,1]; yma := ymi;
      for i := n1+1 to n2 do begin d := data[i,1]; if (d<ymi) then ymi := d;
        if (d>yma) then yma := d; end;
      if (yma=ymi) then begin ymi := ymi-1; yma := yma+1; end else
        begin d := (yma-ymi)/20; ymi := ymi-d; yma := yma+d; end;
      xyyaxis(frontcolor,ymi,yma,0,0,xylabels[1],1,false,false,false);
      if (status=3) then {mislukt} exit;
    end;
  if not singley then {test y-assen}
    for y := 1 to 4 do if not graphs[igraph].as_[y].on then
    begin
      ymi := 1e30; yma := -1e30;
      for n := 1 to ny do if (round(data[0,n])=y) then
        for i := n1 to n2 do begin d := data[i,n]; if (d<ymi) then ymi := d;
        if (d>yma) then yma := d; end;
      if (yma=ymi) then begin ymi := ymi-1; yma := yma+1; end else
        begin d := (yma-ymi)/20; ymi := ymi-d; yma := yma+d; end;
      if (yma>ymi) then xyyaxis(ycol[y],ymi,yma,0,0,xylabels[y],y,false,false,false);
    end;
  if (status=3) then {geen y-assen} exit;

  if not lastsymok then xysymbol(1,6,1); nsymbols := 0;
  with lastsymbol do begin sst := style; ssz := size; sfi := fill; end;
  ysst[1] := 1; ysst[2] := 1; ysst[3] := 1; ysst[4] := 1;
  trackas := 0; trackkl := 0;

  if singley then
    begin plotkolom(1,st,0); trackas := coordas; trackkl := 1; end
  else
    for y := 1 to ny do
      begin
        n := round(data[0,y]); if n in [1..4] then
          begin
            xysetusercoordinates(igraph,n); plotkolom(y,st,n);
            if (trackas=0) then begin trackas := n; trackkl := y; end;
          end;
      end;

  with graphs[igraph] do  begin dat := @data; shw := show;
    trck := dt; tras := trackas; trkl := trackkl; doshw := doshow; end;
end;

procedure xyplotarray(var data:Tdatatype;st,dt:integer); {XXXXXXXXXXXXXXXXXXXXX}
begin xyplotarray(data,st,dt,showdata); end;

procedure xysetdataarray(var data:Tdatatype;nx,ny:integer); {XXXXXXXXXXXXXXXXXX}
var i,j : integer;
begin
  setlength(data,nx+1); for i := 0 to nx do setlength(data[i],ny+1);
  for i := 0 to nx do data[i,0] := i;
  for i := 1 to ny do data[0,i] := i;
  for i := 1 to nx do for j := 1 to ny do data[i,j] := 0;
end;

{------------------------------------------------------------------------------}

procedure straal(cl1,cl2,r1,r2:integer;h:single;txt:string;ang:boolean); {XXXXX}
var si,co : single;
    xp,yp : integer;
begin
  if polrev then h := -h; h := h + polzero;
  if (h<0) then repeat h := h+360 until (h>=0);
  if (h>=360) then repeat h := h-360 until (h<360);
  si := sin(h/180*pi); co := cos(h/180*pi); inc(r1); inc(r2,1);
  xyline(cl1,polxc+round(co*r1),polyc-round(si*r1),
             polxc+round(co*r2),polyc-round(si*r2));
  if (txt='') then exit;
  r2 := r2-1+xycharwidth; if (polartype=2) then r2 := r2 + xycharwidth;
  xp := polxc+round(co*r2); yp := polyc-round(si*r2);
  if ang then
  if (xp>=polxc) then xytextangle(cl2,txt,xp,yp,1,0,8,round(h))
            else xytextangle(cl2,txt,xp,yp,-1,0,8,round(h)+180)
  else
  if (h<85) or (h>275) then  xytext(cl2,txt,xp,yp,1,0,8)  else
  if (h>95) and (h<265) then xytext(cl2,txt,xp,yp,-1,0,8) else
  if (h<180) then            xytext(cl2,txt,xp,yp,0,-1,8) else
                             xytext(cl2,txt,xp,yp,0,1,8);
end;

procedure xypolargraph(cl:Tcolor;r1,r2,zero:single;yjust:integer; {XXXXXXXXXXXX}
       rev,ang,grid,fix:boolean);
var i,t,ndecy,nf,rp,rf : integer;
    incr,grof,fijn,p,x1,x2 : single;
    cl4,cl2 : Tcolor;
    d15,d5,xg,xf,yg,yf,draw : boolean;
    s : string;
procedure stralen;
var i,t : integer;
    s : string;
begin
  t := xycharwidth;
  if (rp>15*res) and d15 then
     begin d15 := false; for i := 0 to 23 do if (i mod 3 >0) then
       begin
         if (polrad>xycharwidth*12) then s := inttostr(i*15) else s := '';
         if xg then straal(cl2,cl,rp,polrad,i*15,s,ang)
               else straal(cl,cl,polrad-t,polrad,i*15,s,ang);
       end;
     end;
  t := xycharwidth div 2 +1;
  if (rp>40*res) and d5 then
     begin d5 := false; for i := 0 to 71 do if (i mod 3 >0) then
          if xf then straal(cl4,cl,rp,polrad,i*5,'',false)
                else straal(cl2,cl,polrad-t,polrad,i*5,'',false); end;
end;
function posr(x:single):integer; begin posr := round((x-xmi)/(xma-xmi)*polrad); end;
begin
  xyfinish; grafoky := (r1<>r2) and grafok; if not grafoky then exit;
  polxc := xorg + xlen div 2; polyc := yorg - ylen div 2;
  polrad := min(xlen div 2,ylen div 2); polzero := zero; polrev := rev;
  if (polrad=0) then begin grafoky := false; exit; end;

  if (r1>r2) then rf := -1 else rf := 1;
  x1 := r1*rf; x2 := r2*rf; status := 0;
  xmi := x1 + graphs[igraph].xf0*(x2-x1);
  xma := x1 + graphs[igraph].xf1*(x2-x1);
  xg := (graphs[igraph].xgridc>=0) and grid;
  xf := (graphs[igraph].xgridf>=0) and grid;
  yg := (graphs[igraph].ygridc>=0) and grid;
  yf := (graphs[igraph].ygridf>=0) and grid;
  if xf then xg := true; if yf then yg := true;

  draw := (colortorgb(cl)<>colortorgb(backcolor));
  if not draw then cl := frontcolor;
  d15 := true; d5 := true; incr := 0;
  verdeling(xmi,xma,incr,polrad,2*xycharheight,grof,fijn,ndecy);
  nf := round(grof/fijn); xypixx := (xmi-xma)/polrad;
  cl4 := mixcolor(cl,backcolor,1,3);
  cl2 := mixcolor(cl,backcolor,1,1);
  xybrush.style := bsclear; xypen.style := pssolid;

  if draw then begin

  if not yg then xyline(cl2,polxc,polyc,polxc,polyc-polrad-1);

  p := grof*trunc(xmi/grof-1); rp := posr(p);
  while (rp<=polrad) do
    begin
      if (rp>0) then
        begin
           t := xycharwidth;
          if yg then xycircle(cl,-1,polxc,polyc,rp)
            else xyline(cl,polxc-t,polyc-rp,polxc+t+1,polyc-rp);
          stralen;
        end;
      for i := 1 to nf-1 do
        begin
          rp := posr(p+i*fijn);
          if (rp>0) and (rp<=polrad) then
            begin
              t := xycharwidth div 2;
              if yf then xycircle(cl4,-1,polxc,polyc,rp)
                else xyline(cl2,polxc-t,polyc-rp,polxc+t+1,polyc-rp);
              stralen;
            end;
        end;
      p := p+grof; rp := posr(p);
    end;

  xycircle(cl,-1,polxc,polyc,polrad);
  for i := 0 to 7 do straal(cl,cl,0,polrad,i*45,inttostr(i*45),ang);

  p := grof*trunc(xmi/grof-1); rp := posr(p);
  while (rp<=polrad) do
    begin
      if (rp>=0) then
        begin
          if fix then str(p*rf:1:plus(ndecy),s) else
            begin if (abs(p)<grof/2) then p := 0; s := lintext(p*rf,ndecy); end;
          xytext(cl,s,polxc+1,polyc-rp,yjust,0,2+8);
        end;
      p := p+grof; rp := posr(p);
    end;

  end;

  xypen.color := cl; useroff := false;
  polartype := 1; xmi := xmi*rf; xma := xma*rf;
  xorg := polxc-polrad; xlen := polrad*2+1;
  yorg := polyc+polrad; ylen := xlen;

  with graphs[igraph] do begin polar := 1;
    xpc := polxc; ypc := polyc; rp := polrad; nrad := 1;
    mi[1] := xmi; ma[1] := xma; pix := xypixx; zero := polzero; rev := polrev;
    ndec[1] := ndecim(xypixx); zmxy := 0.05; col := cl; end;
  with xygraphdata[igraph].xaxis do
    begin on := true; min := xmi; max := xma; log := false; end;
  with xygraphdata[igraph].yaxis[1] do
    begin on := true; min := 0; max := 360; log := false; end;
  if (zgraph=0) then zgraph := igraph else
    if (zgraph>0) and (zgraph<>igraph) then zgraph := -1;

end;

procedure xyradargraph(cl:Tcolor;axes:array of Tradar;style:integer; {XXXXXXXXX}
  zero:single;yjust:integer;rev,ang,fix:boolean;opt:integer);
{ style 0 : spaken verschillende schaal - geen lijnen
        1 : alle spaken dezelfde schaal - geen lijnen
        2 : idem rechte lijnen
        3 : idem ronde lijnen
  opt 1 = alle schalen; 2 = wel midden waarde;
      4 = ook fijne lijnen; 8 = geen buiten lijn}
var nax,cll,cl4,cl2,i,j,n,rf,t,rp,nf,t1,t2 : integer;
    x1,x2,xmi,xma,incr,grof,fijn,p,h,si,co,x,y : single;
    s,ss : string;
    draw,all,centre,coarse,fine : boolean;
    xc,yc:single; hc : integer; sc : string;
procedure draad(cl:Tcolor;r:integer);
var si,co,h : single;
    i,x,y,x0,y0 : integer;
begin
  if (style=3) then xycircle(cl,-1,polxc,polyc,r) else
  for i := 0 to nax do
    begin
      h := (i/nax)*360; if polrev then h := -h; h := h + polzero;
      si := sin(h/180*pi); co := cos(h/180*pi);
      x := polxc+round(co*r); y := polyc-round(si*r);
      if (i=0) then begin x0 := x; y0 := y; end
        else begin xyline(cl,x0,y0,x,y); x0 := x; y0 := y; end;
    end;
end;
function posr(r:single):integer; begin posr := round((r-xmi)/(xma-xmi)*polrad); end;
begin
  xyfinish; nax := length(axes); if (nax>maxrad) then nax := maxrad;
  grafoky := (nax>0) and grafok; if not grafoky then exit;
  polxc := xorg + xlen div 2; polyc := yorg - ylen div 2;
  polrad := min(xlen div 2,ylen div 2); polzero := zero; polrev := rev;
  if (polrad=0) then begin grafoky := false; exit; end;

  draw := (colortorgb(cl)<>colortorgb(backcolor));
  if not draw then cl := frontcolor;
  cl4 := mixcolor(cl,backcolor,1,2);
  cl2 := mixcolor(cl,backcolor,1,1);
  xybrush.style := bsclear; xypen.style := pssolid; status := 0;

  t := xycharwidth; t2 := xycharwidth div 2 + 1;
  polartype := 2; graphs[igraph].polar := 2; sc := '';
  if (style=2) and (nax<3) then style := 3;
  all := (style=0) or (opt and 1 >0);
  centre := (not all) xor (opt and 2 > 0);
  coarse := (style>1);
  fine := coarse and (opt and 4 > 0);

  for i := 0 to nax-1 do
  begin

  h := (i/nax)*360; if polrev then h := -h; h := h + polzero;
  si := sin(h/180*pi); co := cos(h/180*pi);

  if (i=0) or (style=0) then {bereken verdeling}
    begin
      x1 := axes[i].mi; x2 := axes[i].ma;
      if (x1=x2) then begin x1 := x2-0.5; x2 := x1+1; end;
      if (x1>x2) then rf := -1 else rf := 1;
      x1 := x1*rf; x2 := x2*rf; incr := 0;
      xmi := x1 + graphs[igraph].xf0*(x2-x1);
      xma := x1 + graphs[igraph].xf1*(x2-x1);
      incr := 0;
      verdeling(xmi,xma,incr,polrad,2*xycharheight,grof,fijn,ndecy);
      nf := round(grof/fijn);
    end;

  with graphs[igraph] do begin mi[i+1] := xmi*rf; ma[i+1] := xma*rf;
    ndec[i+1] := ndecim(abs(xma-xmi)/polrad); end;

  if draw then begin

  if (i=0) then begin {teken draden}
  if coarse then xypen.style := psdash;
  if (opt and 8=0) then draad(cl4,polrad);
  xypen.style := pssolid;

  p := grof*trunc(xmi/grof-1); rp := posr(p);
  if coarse then while (rp<=polrad) do
    begin
      if (rp>0) then draad(cl,rp);
      if fine then for j := 1 to nf-1 do
        begin
          rp := posr(p+j*fijn);
          if (rp>0) and (rp<=polrad) then draad(cl4,rp);
        end;
      p := p+grof; rp := posr(p);
   end;
  end;

  straal(cl,cl,0,polrad,i/nax*360,axes[i].txt,ang);

  p := grof*trunc(ymi/grof-1); rp := posr(p); {teken verdeling}
  while (rp<=polrad) do
    begin
      if (rp>=0) then
        begin
          x := polxc+rp*co; y := polyc-rp*si;
          if (rp>0) then if not coarse then
            xyline(cl,round(x-t*si),round(y-t*co),
                      round(x+(t+1)*si),round(y+(t+1)*co));
          if fix then str(p*rf:1:plus(ndecy),s) else
            begin if (abs(p)<grof/2) then p := 0; s := lintext(p*rf,ndecy); end;
          if (i=0) or all then
            if (rp>xycharheight) then xytextangle(cl,s,x,y,yjust,0,-2,round(h)-90)
            else if centre then
             if all then xytextangle(cl,s,x,y,yjust,0,-2,round(h)-90)
              else begin xc := x; yc := y; hc := round(h)-90; sc := s; end;
        end;
    for j := 1 to nf-1 do
      begin
        rp := posr(p+j*fijn);
        if (rp>0) and (rp<=polrad) then
          begin
            x := polxc+rp*co; y := polyc-rp*si;
            if not fine then
            xyline(cl2,round(x-t2*si),round(y-t2*co),
                      round(x+(t2+1)*si),round(y+(t2+1)*co));
          end;
      end;
   p := p+grof; rp := posr(p);
   end;
   if (yjust<>0) then straal(cl,cl,0,polrad,i/nax*360,'',ang);
   if (sc<>'') then xytextangle(cl,sc,xc,yc,yjust,0,-2,hc);

   end;

  end;

  xypen.color := cl; useroff := false;
  xorg := polxc-polrad; xlen := polrad*2+1;
  yorg := polyc+polrad; ylen := xlen;

  with graphs[igraph] do begin
    xpc := polxc; ypc := polyc; rp := polrad; nrad := nax;
    pix := 1; zero := polzero; rev := polrev; zmxy := 0.05; col := cl; end;
  with xygraphdata[igraph].xaxis do with graphs[igraph] do
    begin on := true; min := xf0; max := xf1; log := false; end;
  with xygraphdata[igraph].yaxis[1] do
    begin on := true; min := 1; max := nax; log := false; end;
  if (zgraph=0) then zgraph := igraph else
    if (zgraph>0) and (zgraph<>igraph) then zgraph := -1;
end;

procedure xyradardraw(xw : array of single;sy,sf:integer); {XXXXXXXXXXXXXXXXXXX}
var i,n : integer;
begin
  if (polartype<>2) then exit;
  n := length(xw); with graphs[igraph] do if (n>nrad) then n := nrad;
  if (n=0) then exit;
  xymove(xw[0],1);
  for i := n downto 1 do
    begin xydraw(xw[i-1],i);
      if (sy>0) and (xw[i-1]>graphs[igraph].mi[i]) then
        xysymbol(sy,0,sf); end;
  xyfinish;
end;

{------------------------------------------------------------------------------}

function checkcoord(var x1,x2,y1,y2:integer):boolean;
var t : integer;
begin
  if (x1>x2) then begin t := x1; x1 := x2; x2 := t; end;
  if (y1>y2) then begin t := y1; y1 := y2; y2 := t; end;
  if (x2<=xorg) or (x1>xorg+xlen) or (y1>yorg) or (y2<yorg-ylen) then
    begin result := false; exit; end;
  if (x1<=xorg) then x1 := xorg+1;
  if (x2>xorg+xlen) then x2 := xorg+xlen;
  if (y1<yorg-ylen) then y1 := yorg-ylen;
  if (y2>yorg) then y2 := yorg;
  result := true;
end;

procedure xyerrorbar(x0,y0:double;w1,w2,w3,h1,h2,h3,h4:single;abs,wpix:boolean;just:integer);
 {tekent errorbar: hor lijn, vert lijn + dwarseinden, bar}
var xp,yp,x1,x2,y1,y2,w,t : integer;
    leg : boolean;
procedure setxc(wi:single); { - - - - - - - - - - - - - - - - - - - - - - - - - -}
var lin : boolean;
begin
  lin := false;
  if leg then begin w := round(wi); lin := true; end
  else if wpix or useroff then begin w := round(wi*res); lin := true; end
  else if not xlog then begin w := round(wi/xypixx); lin := true; end;
  if lin then
    begin
      if (w=0) then w := 1;
      if (just=0) then if (xypen.width=1) then w := w or 1
                  else if not winnt then w := w and -2;
      if (just<0) then x1 := xp-w + 0 else
        if (just=0) then x1 := xp - w div 2
          else x1 := xp;
      if winnt and not odd(xypen.width) then dec(x1);
      x2 := x1+w+0;
    end
  else
   begin
     if (just<0) then begin x1 := posx(x0-wi); x2 := posx(x0); end else
     if (just=0) then begin x1 := posx(x0-wi/2); x2 := posx(x0+wi/2); end
     else begin x1 := posx(x0); x2 := posx(x0+wi); end;
     if (x2=x1) then x2 := x1+1;
   end;
end;
procedure setyc(h1,h2:single); { - - - - - - - - - - - - - - - - - - - - - - - -}
begin
  if not abs then begin h1 := y0+h1; h2 := y0+h2; end;
  if leg then begin y1 := round(h1); y2 := round(h2); end
  else
  if useroff then
       begin y1 := round(h1*res)+xoff; y2 := round(h2*res)+yoff; end
  else begin y1 := posy(h1);  y2 := posy(h2); end;
  if (y1>y2) then begin t := y1; y1 := y2; y2 := t; end;
  y2 := y2 + 1;
end;
procedure doline(x1,y1,x2,y2:integer);  { - - - - - - - - - - - - - - - - - - - -}
begin
  if noclip or useroff or checkcoord(x1,x2,y1,y2) then xyline(-1,x1,y1,x2,y2);
end;

begin
  if (not grafoky) or (polartype>0) then exit;
  xybrush.color := xypen.color; store;
  xypen.Style := pssolid;
  if (just=1000) then begin just := 0; leg := true end else leg := false;

  if leg then begin xp := round(x0); yp := round(y0) end
  else
  if useroff then begin xp := round(x0*res)+xoff; yp := round(y0*res)+yoff; end
    else begin xp := posx(x0); yp := posy(y0); end;

  if (w1>0) then  {teken hor lijn}
    begin
      setxc(w1); doline(x1,yp,x2,yp);
    end;

  if (w2>=0) then {teken vert lijn}
    begin
      setyc(h1,h2);
      if (w2>0) then {teken dwarslijntjes}
        begin
          setxc(w2); doline(x1,y1,x2,y1); doline(x1,y2,x2,y2);
        end;
      doline(xp,y1,xp,y2);
    end;

  if (w3>0) then  {teken bar}
    begin
      xypen.width := 1; setxc(w3); setyc(h3,h4);
      if checkcoord(x1,x2,y1,y2) or noclip or useroff then
        begin
          xypen.style := psclear;
          xyrectangle(-1,-1,x1,y1,x2+1,y2+1);
        end;
    end;

  restore;
end;

procedure xybar(cl:Tcolor;pos:double;wi,hi:single;just:integer); {XXXXXXXXXXXXXXXXXXXX}
 {tekent staaf als in bar chart; just: -1 = naar links, 0=midden, 1=naar rechts}
var cx : Tcolor;
begin
  cx := xypen.color; xypen.color := cl;
  xyerrorbar(pos,0,-1,-1,wi,-1,-1,0,hi,absl,userc,just);
  xypen.color := cx;
end;

procedure xysegbar(frcol:Tcolor;pos:double;wi,zero:single;just:integer;abs:boolean;
  data:array of Tbardata);
var i,n,pw,x1,x2,y1,y2 : integer;
    h : single;
    fcol : Tcolor;
begin
  if (not grafoky) or (polartype>0) then exit;
  n := length(data); if (n=0) then exit;
  if (frcol<0) then begin pw := xypen.width; xypen.width := 1; end;

  if (just<0) then begin x1 := posx(pos-wi); x2 := posx(pos); end else
  if (just=0) then begin x1 := posx(pos-wi/2); x2 := posx(pos+wi/2); end
  else begin x1 := posx(pos); x2 := posx(pos+wi); end;
  if (x2=x1) then x2 := x1+1;

  fcol := frcol;
  for i := 1 to n do with data[i-1] do
    begin
      if abs then h := ypos else h := ypos+zero;
      y1 := posy(h); y2 := posy(zero);
      if (frcol<0) then fcol := col;
      if (y1<>y2) then
         if checkcoord(x1,x2,y1,y2) or noclip or useroff then
            xyrectangle(fcol,col,x1,y1,x2+1,y2+1);
      zero := h;
    end;
  xypen.width := pw;
end;

{------------------------------------------------------------------------------}

procedure xylegendclear; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin xylegendcount := 0; setlength(legends,0); end;

procedure xylegendentry(cd:integer; text:string); {XXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var i : integer;
    col : Tcolor;
begin
  if not (cd in [0..4]) then exit;
  if stereo then col := frontcolor else
  case cd of
    0 : col := xypen.color;
    1 : col := xybrush.color;
   2,3 : col := lastsymbol.color;
    4 : col := xypen.color;
    end;
  if (colortorgb(col)=colortorgb(backcolor)) then exit;

  xylegendcount := length(legends);
  for i := 0 to xylegendcount-1 do with legends[i] do
    if ( (txt=text) and (code=cd) ) then exit;

  inc(xylegendcount); setlength(legends,xylegendcount);
  with legends[xylegendcount-1] do
    begin
      if (linestyle<=0) then lw1 := linestyle else lw1 := linew1;
      lw2 := linew2; code := cd; txt := text;
      case cd of
        0 : begin cl := col; lwidth := xypen.width; lstyle := integer(xypen.style); end;
        1 : begin cl := col; lstyle := integer(xybrush.style); end;
       2,3 : with lastsymbol do
        begin cl := col; lwidth := width; lstyle := style; lfill := fill;
           c1 := cl1; c2 := cl2; c3 := cl3; st3d := cd3d;
         end;
        4 : begin cl := col; lwidth := xypen.width; lstyle := integer(xybrush.style); end;
      end;
    end;
end;

procedure xylegendmake(style,xp,yp,xjust,yjust,wi,he,len:integer; framed:boolean);
 {style 0 = hor, 1 regel; 1 = hor, 2 regels; 2 = vert, 2 kol; 3 = vert, 1 kol}
const cbr = 20; {min breedte}
      cd1 = 4;  {hor spatie}
      cd2 = 6;  {hor spatie}
      cd3 = 1;  {ver spatie}
var i,mwi,mhe,w,x,y,fl,ll,tw,fh,h,h2,h3,h4,h5,h6,xof,br,d1,d2,d3 : integer;
    d : single;

procedure drawleg(xp,yp,n:integer); { X X X X X X X X X X X X X X X X X X X X X}
var sym : symboltype;
    he1,he2 : single;
    uoff : boolean;
    xc,yc,mode,x1,x2,x3,x4 : integer;
    point6 : array[0..5] of Tpoint;
 {0=lijn 1=bar 2,3=symbol(0-8) 4=err.bar 20...=3D
 {symbol: 0=punt 1=ronje, 2=vierkant 3-6 = driehoeken 7=kruisX 8=kruis+}
 {3D symbol: 20-26 = stijl van surface 29=blok 28=rondje 30=polyh 31=cyl}
begin
  xc := xp + ll div 2; yc := yp + h2; {centrum van tekentje}
  with legends[n] do with xycanvas do
  begin
  pen.color := cl; pen.width := lwidth; pen.style := pssolid;
  brush.color := c1; brush.style := bssolid;
  if stereo then begin
    pen.color := frontcolor; brush.style := bsclear; end;
  if (st3d>0) then mode := st3d else mode := code;
  case mode of
    0 : begin
         if (lw1<0) then pen.style := TPenStyle(lstyle);
         if (lw1>0) then dashline(xp,yc,xp+ll,yc,lw1,lw2)
                    else xyline(-1,xp,yc,xp+ll,yc);
        end;
    1 : begin
          brush.color := cl; brush.style := TBrushStyle(lstyle);
          pen.style := psclear;
          xyrectangle(-1,-1,xc-br div 2,yp+1,xc+br div 2+1,yp+h);
        end;
   2,3 : begin
          sym.xp := xc; sym.yp := yc; sym.style := lstyle;
          if doprint then sym.size := round(xyfont.size*ffac)
                 else sym.size := xyfont.size;
          sym.fill := lfill; sym.width := lwidth; sym.color := cl;
          drawsymbol(sym);
          xylinewidth(1); pen.color := cl;
          if (lfill=0) and (code=3) and (lstyle in [1..6]) then
              xycircle(-1,-1,xc,yc,res);
        end;
     4 : begin
           he1 := h2-res - lwidth; he2 := h2;
           brush.style := TBrushStyle(lstyle);
           uoff := useroff; useroff := true;
           xyerrorbar(xc,yc, h,he2,he1*2, he2,-he2, he1,-he1, false,true,1000);
           useroff := uoff;
         end;
       {3D symbolen:}
    20 : begin brush.style := bsclear; xyrectangle(cl,-1,xc-h2,yc+h2,xc+h2,yc+h4);
          xyline(cl,xc-h2,yc+h4,xc-h4,yc-h4); xyline(cl,xc-h4,yc-h4,xc+h4,yc-h4);
          xyline(cl,xc+h4,yc-h4,xc+h2,yc+h4); end;
    21 : begin xypixel(xc,yc,cl); xypixel(xc-h5,yc,cl); xypixel(xc+h5,yc,cl);
        xypixel(xc,yc+h5,cl); xypixel(xc-h2,yc+h5,cl); xypixel(xc+h2,yc+h5,cl);
        xypixel(xc,yc-h5,cl); xypixel(xc-h3,yc-h5,cl); xypixel(xc+h3,yc-h5,cl);end;
    22 : begin xyline(cl,xc-h5,yc,xc+h5,yc); xyline(cl,xc-h2,yc+h5,xc+h2,yc+h5);
        xyline(cl,xc-h3,yc-h5,xc+h3,yc-h5); end;
    23 : begin  xyline(cl,xc,yc-h5,xc,yc+h5); xyline(cl,xc-h4,yc-h5,xc-h2,yc+h5);
       xyline(cl,xc+h4,yc-h5,xc+h2,yc+h5); end;
  24..26 : begin
         point4[0] := point(xc-h2,yc+h5); point4[1] := point(xc+h2,yc+h5);
         point4[2] := point(xc+h3,yc-h5); point4[3] := point(xc-h3,yc-h5);
         if (lfill=0) then xybrush.style := bsclear;
         xypolygon(-1,-1,point4);
         if (mode<26) then begin xyline(cl,xc-h5,yc,xc+h5,yc);
                             xyline(cl,xc,yc-h5,xc,yc+h5); end;
           end;
   28 : begin if (lfill=0) then brush.style := bsclear; xycircle(-1,-1,xc,yc,h5); end;
   29 : begin
         point4[0] := point(xc,yc); point4[1] := point(xc,yc+h5);
         point4[2] := point(xc+h5,yc+h4); point4[3] := point(xc+h5,yc-h4);
         if (lfill=0) then brush.style := bsclear; xypolygon(-1,-1,point4);
         point4[2] := point(xc-h5,yc+h4); point4[3] := point(xc-h5,yc-h4);
         if (c2>=0) then xybrush.color := c2;
         if (lfill=0) then brush.style := bsclear; xypolygon(-1,-1,point4);
         point4[2] := point(xc,yc-h5); point4[1] := point(xc+h5,yc-h4);
         if (c3>=0) then xybrush.color := c3;
         if (lfill=0) then brush.style := bsclear; xypolygon(-1,-1,point4);
       end;
    30 : begin if (lfill=0) then brush.style := bsclear;
         point4[0] := point(xc,yc-h2); point4[1] := point(xc-h2,yc);
         point4[2] := point(xc,yc+h2); point4[3] := point(xc+h2,yc);
         xypolygon(-1,-1,point4); xyline(-1,xc,yc-h2,xc,yc+h2);
         xyline(-1,xc-h2,yc,xc,yc+h4); xyline(-1,xc+h2,yc,xc,yc+h4);
         end;
    31 : begin
         x1 := xc-h5; x2 := xc-h4; x3 := xc+xc-x2; x4 := xc+xc-x1;
         if (c1>=0) then xybrush.color := c1;
         if (lfill=0) then brush.style := bsclear;
         point4[0] := point(x1,yc-h6); point4[1] := point(x1,yc+h5);
         point4[2] := point(x2,yc+h2); point4[3] := point(x2,yc);
         xypolygon(-1,-1,point4);
         point4[0] := point(x3,yc); point4[1] := point(x3,yc+h2);
         xypolygon(-1,-1,point4);
         point4[2] := point(x4,yc+h5); point4[3] := point(x4,yc-h6);
         xypolygon(-1,-1,point4);
         if (c2>0) then xybrush.color := c2 else
         if (c3>0) then xybrush.color := c3;
         if (lfill=0) then brush.style := bsclear;
         point6[0] := point(x1,yc-h6); point6[1] := point(x2,yc);
         point6[2] := point(x3,yc); point6[3] := point(x4,yc-h6);
         point6[4] := point(x3,yc-h3); point6[5] := point(x2,yc-h3);
         xypolygon(-1,-1,point6);
       end;
    end; end;
end;
procedure drawtext(xp,yp,n:integer;center:boolean); { X X X X X X X X X X X X X}
var w : integer;
    s : string;
begin
  s := legends[n].txt;
  with xycanvas do
    begin
      if center then w := textwidth(s) div 2 else w := 0;
      if stereo then font.color := frontcolor
        else font.color := legends[n].cl;
      brush.style := bsclear;
    end;
  if dowmf then
    begin checkwmffont; textwmfout(xp,yp,s); end
  else xycanvas.textout(xp-w,yp,s);
end;

procedure drawhor(xp,yp,n:integer); { X X X X X X X X X X X X X X X X X X X X X}
begin drawleg(xp,yp,n); drawtext(xp+ll+d1,yp,n,false); end;
procedure drawver(xp,yp,n:integer); { X X X X X X X X X X X X X X X X X X X X X}
begin drawleg(xp-ll div 2,yp,n); drawtext(xp,yp+h,n,true); end;
procedure legdraw(xp,yp,n:integer); { X X X X X X X X X X X X X X X X X X X X X}
begin if odd(style) then drawver(xp,yp,n) else drawhor(xp,yp,n); end;

begin
  br := cbr*res; d1 := cd1*res; d2 := cd2*res; d3 := cd3*res;
  xp := xp*res+xoff; yp := yp*res+yoff;
  wi := wi*res; he := he*res; len := len*res;

  xylegendcount := length(legends);
  if (xylegendcount=0) then exit;
  if not (style in [0..3]) then exit;
  h := xycanvas.textheight('0');
  h2 := h div 2; h3 := h div 3; h4 := h div 4;
  h5 := h * 5 div 12; store; h6 := h div 6;

  tw := 0;
  for i := 0 to xylegendcount-1 do
    begin
      w := xycanvas.textwidth(legends[i].txt);
      if (w>tw) then tw := w;
    end;

  if (len<br) then ll := br else ll := len;
  if odd(style) then {teken en tekst onder elkaar}
    begin fl := max(ll,tw); ll := fl; fh := 2 * h; xof := fl div 2; end
  else {teken en tekst naast elkaar}
    begin fl := ll + d1 + tw; fh := h; xof := 0; end;

  if dowmf then if odd(style) then textwmfalign(6) {center alle tekst}
     else textwmfalign(0);

  case style of
   0,1 : begin mwi := d2+xylegendcount*(fl+d2); mhe := d3+fh+d3; end;
   2,3 : begin mwi := d2+fl+d2; mhe := d3+xylegendcount*fh+d3; end;
  end;
  if (wi<mwi) then wi := mwi;
  if (he<mhe) then he := mhe;

  if (xjust=0) then xp := xp-wi div 2 else
   if (xjust=-1) then xp := xp - wi;
  if (yjust=0) then yp := yp-he div 2 else
   if (yjust=-1) then yp := yp - he;

  if framed then with xycanvas do
    begin
      pen.width := res; pen.style := pssolid; brush.style := bssolid;
      xyrectangle(frontcolor,backcolor,xp,yp,xp+wi,yp+he+1);
    end;

  case style of
   0,1 : begin
         y := (he-fh) div 2;
         d := (wi - fl*xylegendcount) / (xylegendcount+1);
         for i := 0 to xylegendcount-1 do
           begin
             x := round(i*(d+fl)+d) + xof;
             legdraw(xp+x,yp+y,i);
           end;
       end;
   2,3 : begin
         x := (wi-fl) div 2 + xof;
         d := (he-d3-d3 - fh*xylegendcount) / (xylegendcount+1);
         for i := 0 to xylegendcount-1 do
           begin
             y := d3+round( d + i*(fh+d));
             legdraw(xp+x,yp+y,i);
           end;
       end;
   end;
   xylinewidth(1);
   restore;
   xylegendclear;
end;

{------------------------------------------------------------------------------}

procedure fillexport(x,y:integer; var exp:Texporttype); {XXXXXXXXXXXXXXXXXXXXXX}
var i,r : integer; xxw : single;
begin
  with exp do begin
    lbl := pblabel; igr := igraph; xp := x; yp:= y;
    if (graphs[igraph].polar>0) then
      begin polarwaarde(x,y,xxw,yw[1],r); xw := xxw; xn := 0; end
    else
    begin
    xw := xwaarde(x);
    if rul3d then begin
      yw[1] := ywaarde(y,1); yw[2] := hoogte3d; yw[3] := 0; yw[4] := 0; end
    else
    for i := 1 to 4 do
      if graphs[igraph].as_[i].on then yw[i] := ywaarde(y,i) else yw[i] := 0;
    end;
    xn := xdata; yn := ydata;
  end;
end;

procedure xyinitruler(cl:Tcolor;xp,yp,j,mode:integer); {XXXXXXXXXXXXXXXXXXXXXXX}
 {initieert lineaal}
begin
  crossxp := xp; crossyp := yp; crossj := j;
  coordok := (xp>=0) or (yp>=0);
  crossx := 0; crossy := 0; if mode3d then mode := 0;
  coordcolor := cl; crossmode := mode and 7;
  snapxok := mode and 8 > 0; snapyok := mode and 16 > 0;
  crossnorel := (mode and 32>0) or (crossmode>2);
  trackok := (mode and 64=0);
end;

procedure xyinitruler(cl:Tcolor;xp,yp,j:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin xyinitruler(cl,xp,yp,j,0); end;

procedure xysetusercoordinates(n,ny:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  if (n<0) or (n>maxgraf) then exit;
  xyfinish;
  if (n=0) then
    begin
      useroff := true; bar0 := ny;
      xypen.color := frontcolor; exit;
    end;
  useroff := false;
  if not graphs[n].ok then exit;
  polartype := graphs[n].polar;
  with graphs[n] do case polar of
  0 : begin
      if not (as_[ny].on and as_[0].on) then exit;
      with as_[0] do
        begin
          xlog := log; xmi := mi; xma := ma;
          xorg := org; xlen := len; dxlog := dlog;
        end;
      with as_[ny] do
        begin
          ylog := log; ymi := mi; yma := ma;
          yorg := org; ylen := len; dylog := dlog;
          xypen.color := col;
        end;
    end;
  1,2 : begin
       polxc := xpc; polyc := ypc; polrad := rp;
       xmi := mi[1]; xma := ma[1]; polzero := zero; polrev := rev;
       xypen.color := col;
       xorg := polxc-polrad; xlen := polrad*2+1;
       yorg := polyc+polrad; ylen := xlen;
      end;
  end;
end;

procedure tekencross(var x,y:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var f,r : single;
    xx,yy : integer;
begin
   xypen.mode := pmxor; xybrush.style := bsclear;
   if (coordcolor<0) then xypen.color := clwhite
      else xypen.color := coordcolor xor backcolor;
   xypen.width := 1;

   with graphs[igraph] do
   if (crossmode>7) then
     begin
       if not noclip then with as_[0] do if (x<org) or (x>org+len) then exit;
       if not noclip then with as_[coordas] do if (y>org) or (y<org-len) then exit;
       xycanvas.ellipse(x-5,y-5,x+6,y+6)
     end
   else
   if (polar=0) then
    begin
     with as_[0] do if (x<org) then x := org else if (x>org+len) then x := org+len;
     with as_[coordas] do if (y>org) then y := org else if (y<org-len) then y := org-len;
     if crossmode in [0,1] then with as_[coordas] do drawline(x,org,x,org-len);
     if crossmode in [0,2] then with as_[0] do drawline(org,y,org+len,y);
    end
   else
    begin
      r := sqrt(sqr(x-xpc)+sqr(y-ypc));
      if (r<1) then exit; f := rp/r;
      xx := xpc+round((x-xpc)*f); yy := ypc+round((y-ypc)*f);
      if (r>rp) then begin r := rp; x := xx; y := yy; end;
      if crossmode in [0,1] then xycircle(-1,-1,xpc,ypc,round(r));
      if crossmode in [0,2] then
         begin xycanvas.moveto(xpc,ypc); xycanvas.lineto(xx,yy); end;
    end;

   xypen.mode := pmcopy;
end;

procedure showcoord(x,y:integer;show:boolean); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var s,ss,s3d : string;
    l,n,n2,r : integer;
    xw,yw,h : single;
    xwd : double;
    dd,ad : double;
const sx = ' / ';
function snapx(var x:single;incr:single):single;
begin
  if (incr=0) then result := x else
  if (x<0) then result := -int(-x/incr+0.5)*incr else
        result := int(x/incr+0.5)*incr;
end;
begin
  if not coordok then exit;
  xybrush.color := backcolor;

  if doshowtrack then with graphs[igraph] do
    begin
      if show then s := shw(xdata,ydata) else s := shw(xdataold,ydataold);
      if not show then xyfont.color := backcolor
                  else xyfont.color := as_[0].col;
      case crossj of
         -1 : l := xycanvas.textwidth(s);
          0 : l := xycanvas.textwidth(s) div 2;
          1 : l := 0;
       end;
      xycanvas.textout(crossxp-l,crossyp,s);
      exit;
    end;

  with graphs[igraph] do begin if (polar>0) then
    begin
      polarwaarde(x,y,xw,yw,r);
      s := showdec(xw,ndec[1]);
      if (polar=2) then str(yw:1:0,ss) else
        begin
         if (r<60) then str(yw:1:0,ss) else str(yw:1:1,ss);
         ss := ss + #186;
       end;
      case crossj of
         -1 : l := xycanvas.textwidth(s+sx+ss);
          0 : l := xycanvas.textwidth(s+sx+s) div 2;
          1 : l := 0;
       end;
       if not show then xyfont.color := backcolor
                   else xyfont.color := col;
       xycanvas.textout(crossxp-l,crossyp,s+sx+ss);
       exit;
    end;

  xwd := xwaarde(x); yw := ywaarde(y,coordas);
  with graphs[igraph] do
     begin
       with as_[0] do
         if crossrel then
           if log then s := signif(xwd/cross0w[0],ndec) else
           if (typ=1) then s := inttostr(round(xwd-cross0w[0])) else
           if (typ=4) then begin dd := xwd-cross0w[0]; ad := abs(dd);
           case ndec of
            0 : begin n := trunc(ad/365.25); n2 := round(ad-365.25*n);
                 s := inttostr(n)+'y'+inttostr(n2)+'d'; end;
            1 : begin s := inttostr(round(ad))+'d'; end;
            2 : begin n := round(ad*24);
              s := inttostr(n div 24)+'d'+inttostr(n mod 24)+'h'; end;
            3 : begin n := round(ad*24*60);
              s := inttostr(n div 60)+'h'+inttostr(n mod 60)+'m'; end;
            4 : begin n := round(ad*24*60*60);
              s := inttostr(n div 60)+'m'+inttostr(n mod 60)+'s'; end;
           end;
           if (dd<0) then s := '-'+s;
            end else
           begin n := min(ndec,ninc); xw := xwd-cross0w[0]; s := showdec(snapx(xw,incr),n); end
         else
           if log then s := signif(xwd,ndec) else
           if (typ=1) then s := datum(round(xwd)) else
           if (typ=4) then case ndec of
             0: s := formatdatetime('yyyy-mm',xwd);
             1: s := formatdatetime('yy-mm-dd',xwd);
             2: s := formatdatetime('mm-dd hh:',xwd);
             3: s := formatdatetime('dd hh:mm',xwd);
             4: s := formatdatetime('hh:mm:ss',xwd);
            end
           else begin n := min(ndec,ninc); xw := xwd; s := showdec(snapx(xw,incr),n); end;
       with as_[coordas] do
         if crossrel then
           if log then ss := signif(yw/cross0w[coordas],ndec) else
           begin n := min(ndec,ninc); yw := yw-cross0w[coordas];
               ss := showdec(snapx(yw,incr),n); end
         else
           if log then ss := signif(yw,ndec) else
           begin n := min(ndec,ninc); ss := showdec(snapx(yw,incr),n); end;
         if rul3d then
            begin
              h := rul3dhoogte(xwd,yw);
              if (hasempty) and (h=empty) then
                begin s3d := sx+'----'; hoogte3d := 0; end
               else
                begin s3d := sx+showdec(h,ndecy); hoogte3d := h; end
            end
         else begin hoogte3d := 0; s3d := ''; end;

       case crossj of
         -1 : l := xycanvas.textwidth(s+sx+ss+s3d);
         0 : if rul3d then l := xycanvas.textwidth(s+sx+ss+s3d) div 2
             else l := xycanvas.textwidth(s+sx+s) div 2;
         1 : l := 0;
       end;

       end;

       if not show then
         begin
           xyfont.color := backcolor;
           xycanvas.textout(crossxp-l,crossyp,s+sx+ss+s3d);
         end
       else
         begin
           xyfont.color := as_[0].col;
           xycanvas.textout(crossxp-l,crossyp,s+sx);
           xyfont.color := as_[coordas].col;
           xycanvas.textout(xycanvas.penpos.x,crossyp,ss+s3d);
         end;
     end;
end;

procedure snap(var x,y:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
  {past snapping toe op x en y waarden}
var xd,xw : double;
    yd,yw : single;
    nx,nas,nmi,dmin,t1,t2,yy,nk,yt,d,k : integer;
begin
   if (track>0) then with graphs[igraph] do
     begin
       if (dat^[0,0]>0.5) then nmi := 8 else nmi := 1;
       xd := xwaarde(x); {zoek numer x-getal}
       if (xd<=dat^[1,0]) then nx := nmi else
       if (xd>=dat^[ndata,0]) then nx := ndata else
         begin
           t1 := nmi; t2 := ndata;
           while (t2-t1>1) do
             begin nx := (t2+t1) div 2;
              if (dat^[nx,0]>xd) then t2 := nx else t1 := nx; end;
           nx := t2; if (xd-dat^[nx-1,0])<(dat^[nx,0]-xd) then dec(nx);
         end;
       x := posx2(dat^[nx,0]); xdata := nx; ydata := trackkl;
       if (track=2) then y := posy2(dat^[nx,trackkl],trackas);
       if (track=3) then
         begin
           dmin := 10000; nk := trackkl; yt := y;
           for k := 1 to nkol do
           begin
             nas := round(dat^[0,k]); if nas in [1..4] then
               begin
                 yy := posy2(dat^[nx,k],nas); d := abs(yy-y);
                 if (d<dmin) then begin dmin := d; nk := k; yt := yy; end;
               end;
           end;
           y := yt; ydata := nk;
         end;
       exit;
     end;

   if (snapx>0) then
     begin
        xd := xwaarde(x);
        xw := int(abs(xd)/snapx+0.5)*snapx;
        if (xd<0) then xw := -xw;
        x := posx2(xw);
     end;
   if (snapy>0) then
     begin
        yd := ywaarde(y,coordas);
        yw := int(abs(yd)/snapy+0.5)*snapy;
        if (yd<0) then yw := -yw;
        y := posy2(yw,coordas);
     end;
end;

procedure cross(x,y:integer;show:boolean); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {tekent lineaal}
begin
   crossx := x; crossy := y;
   tekencross(x,y); if coordok then showcoord(x,y,show);
end;

procedure showtrack(nx,ny:integer;show:boolean); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var xw,yw : single;
    xp,yp,yk : integer;
begin
  if not istracking in [1..8] then exit;
  if graphs[istracking].ok then with graphs[istracking] do begin

  xw := dat^[nx,0]; yw := dat^[nx,ny]; yk := round(dat^[0,ny]);
  xp := posx2(xw);
  if (track=1) then yp := crossy else yp := posy2(yw,yk);
  with as_[trackas] do
     if (yp>org) then yp := org else if (yp<org-len) then yp := org-len;
  xdata := nx; ydata := ny;
  cross(xp,yp,show);
  if show then fillexport(xp,yp,xyexportd);
  end;
end;

procedure stattrack(m:integer;scrl:boolean); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var nmi,nx,ny,k,ist,tr,ig,ky,cm : integer;
    go,goscroll : boolean;
    xw,yw,xf,yf,dx,dy : double;
procedure scrollleft;
begin
  with graphs[igraph] do with as_[0] do
    begin
      dx := xf1-xf0; xf := xf0-(mi-xw)/(ma-mi)*dx;
      xf0 := xf-dx/10; if (xf0<0) then xf0 := 0;
      xf1 := xf0+dx; goscroll := true;
    end;
end;
procedure scrollright;
begin
  with graphs[igraph] do with as_[0] do
    begin
      dx := xf1-xf0; xf := xf1+(xw-ma)/(ma-mi)*dx;
      xf1 := xf+dx/10; if (xf1>1) then xf1 := 1;
      xf0 := xf1-dx; goscroll := true;
    end;
end;
procedure scrolldown;
begin
  with graphs[igraph] do with as_[ky] do
    begin
      dy := yf1-yf0; yf := yf0-(mi-yw)/(ma-mi)*dy;
      yf0 := yf-dy/10; if (yf0<0) then yf0 := 0;
      yf1 := yf0+dy; goscroll := true;
    end;
end;
procedure scrollup;
begin
  with graphs[igraph] do with as_[ky] do
    begin
      dy := yf1-yf0; yf := yf1+(yw-ma)/(ma-mi)*dy;
      yf1 := yf+dy/10; if (yf1>1) then yf1 := 1;
      yf0 := yf1-dy; goscroll := true;
    end;
end;
begin
  if cvmode then scrl := false;
  with graphs[igraph] do
    begin
      if (dat^[0,0]>0.5) then nmi := 8 else nmi := 1;
      nx := xdata; ny := ydata; go := false; goscroll := false;
      if (m in [1,2]) then
        begin
          if (m=1) and (nx<ndata) then inc(nx);
          if (m=2) and (nx>nmi) then dec(nx);
          xw := dat^[nx,0];
          if scrl then if (xw<as_[0].mi) then scrollleft;
          if scrl then if (xw>as_[0].ma) then scrollright;
          go := (xw>=as_[0].mi) and (xw<=as_[0].ma);
        end;
      if (m in [3,4]) then if (track=3) then
        begin
          if (m=3) and (ny<nkol) then for k := nkol downto ydata+1 do
            if (round(dat^[0,k]) in [1..4]) then ny := k;
          if (m=4) and (ny>1) then for k := 1 to ydata-1 do
            if (round(dat^[0,k]) in [1..4]) then ny := k;
          go := (ny<>ydata);
        end;
      if scrl then begin
        ky := round(dat^[0,ny]); yw := dat^[nx,ny];
        if (yw<as_[ky].mi) then scrolldown;
        if (yw>as_[ky].ma) then scrollup; end;

      if go or goscroll then
       begin
          showtrack(xdata,ydata,false);
          if goscroll then
            begin
              ist := istracking; tr := track; ig := igraph; cm := crossmode;
              xysetzoom(igraph,xf0,xf1,yf0,yf1); screen.cursor := crhandpoint;
              istracking := ist; track := tr; igraph := ig; crossmode := cm;
            end;
          xdata := nx; ydata := ny; showtrack(xdata,ydata,true);
          xdataold := xdata; ydataold := ydata;
       end;
    end;
end;

procedure closetrack; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  showtrack(xdata,ydata,false);
  istracking := 0; screen.cursor := crdefault;
end;

procedure setcrossrel; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var i : integer;
begin
  if (not crosson) or crossnorel or (track>0) then exit;
  cross(crossx,crossy,false);
  crossrel := not crossrel;
  if crossrel then with graphs[igraph] do
    begin
      crossx0 := crossx; crossy0 := crossy;
      if crossmode in [0,1] then cross0w[0] := xwaarde(crossx0)
        else if as_[0].log then cross0w[0] := 1 else cross0w[0] := 0;
      for i := 1 to 4 do with as_[i] do if on then
      if crossmode in [0,2] then cross0w[i] := ywaarde(crossy0,i)
        else if log then cross0w[i] := 1 else cross0w[i] := 0;
    end;
  tekencross(crossx0,crossy0);
  cross(crossx,crossy,true);
  fillexport(crossx,crossy,xyexportc);
end;

procedure blok(x,y:integer;show:boolean); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {tekent blok ivm zoom}
begin
   xypen.mode := pmxor;
   if {cvmode}mode3d then xypen.color := clwhite else
     xypen.color := coordcolor xor backcolor;
   xypen.width := 1;
   xycanvas.moveto(blokx0,bloky0); xycanvas.lineto(x,bloky0);
   xycanvas.lineto(x,y); xycanvas.lineto(blokx0,y);
   xycanvas.lineto(blokx0,bloky0);
   xypen.mode := pmcopy;
   xypen.style := pssolid;
   if coordok then showcoord(x,y,show);
   blokx1 := x; bloky1 := y;
end;

procedure line(x,y:integer;show:boolean); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {tekent lijn ivm doorsnede}
begin
   xypen.mode := pmxor;
   if {cvmode}mode3d then xypen.color := clwhite else
     xypen.color := coordcolor xor backcolor;
   xypen.width := 1;
   xycanvas.moveto(linex0,liney0); xycanvas.lineto(x,y);
   xypen.mode := pmcopy;
   xypen.style := pssolid;
   if coordok then showcoord(x,y,show);
   linex1 := x; liney1 := y;
end;

procedure zoom; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {rekent nieuwe zoom parameters uit en tekent grafiek opnieuw}
var xff0,xff1 : double; yff0,yff1 : single;
    t : integer;
begin
  if not graphs[igraph].cont then
    if cvmode or mode3d then exit;

  with graphs[igraph] do begin

  if (zmxy<zmax) then zmxy := zmax;
  if (abs(blokx0-blokx1)<=1) and (abs(bloky0-bloky1)<=1) then
    begin xyunzoom(igraph);
      if not cvmode then xypaintbox.refresh
        else zoomcontour; exit; end;
  if (blokx0>blokx1) then begin t := blokx0; blokx0 := blokx1; blokx1 := t; end;
  if (bloky0<bloky1) then begin t := bloky0; bloky0 := bloky1; bloky1 := t; end;
  if (polar>0) then if (blokx0>=xpc) or (blokx1<=xpc) then exit;
  if (polar>0) then
    begin yff0 := 0; yff1 := 1;
      xff0 := (ypc-bloky0)/rp; xff1 := (ypc-bloky1)/rp; end
  else begin
  with as_[0] do
    begin xff0 := (blokx0-org)/len; xff1 := (blokx1-org)/len; end;
  with as_[coordas] do
    begin yff0 := (org-bloky0)/len; yff1 := (org-bloky1)/len; end;
       end;
  xff0 := xf0 + xff0*(xf1-xf0);
  xff1 := xf0 + xff1*(xf1-xf0);
  yff0 := yf0 + yff0*(yf1-yf0);
  yff1 := yf0 + yff1*(yf1-yf0);
  xf0 := xff0; xf1 := xff1; yf0 := yff0; yf1 := yff1;
  if (xf0<0) then xf0 := 0;
  if (xf1>1) then xf1 := 1;
  if (yf0<0) then yf0 := 0;
  if (yf1>1) then yf1 := 1;
  if (xf1-xf0)<zmxx then
     begin xf0 := (xf1+xf0)/2-zmxx/2; xf1 := xf0+zmxx; end;
  if (yf1-yf0)<zmxy then
     begin yf0 := (yf1+yf0)/2-zmxy/2; yf1 := yf0+zmxy; end;
  copyzoom3d(igraph,xf0,xf1,yf0,yf1);

  end;
  izoom := igraph;
  if not cvmode then xypaintbox.refresh
    else zoomcontour;
end;

procedure xysetzoom(n:integer; fx0,fx1:double; fy0,fy1:single); {XXXXXXXXXXXXXX}
 {stelt zoom parameters handmatig in en tekent grafiek opnieuw}
begin
  if cvmode or (n<1) or (n>maxgraf) or mode3d then exit;
  if not graphs[n].ok then exit;
  with graphs[n] do begin

  if (fx0>=0) and (fx0<=1) then xf0 := fx0;
  if (fx1>=0) and (fx1<=1) then xf1 := fx1;
  if (fy0>=0) and (fy0<=1) then yf0 := fy0;
  if (fy1>=0) and (fy1<=1) then yf1 := fy1;
  if (xf1-xf0)<zmaxx then
     begin xf0 := (xf1+xf0)/2-zmxx/2; xf1 := xf0+zmxx; end;
  if (yf1-yf0)<zmaxy then
     begin yf0 := (yf1+yf0)/2-zmxy/2; yf1 := yf0+zmxy; end;
  copyzoom3d(n,xf0,xf1,yf0,yf1);

  end;
  xypaintbox.refresh;
  izoom := n;
end;

procedure initcrosssect(var Shift: TShiftState); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var xx1,yy1,xx2,yy2,n:integer;
    slope : single;
    ok : boolean;
begin
  if (linex0=linex1) and (liney0=liney1) then begin shift := []; exit; end;
  with xygraphdata[igraph] do
  if ( (linex0<x1) and (linex1<x1) ) or ( (linex0>x2) and (linex1>x2) ) or
     ( (liney0<y1) and (liney1<y1) ) or ( (liney0>y2) and (liney1>y2) )
   then begin shift := []; exit; end;

  with xygraphdata[igraph] do
  if (linex0=linex1) then
    begin xx1 := linex0; xx2 := xx1; yy1 := y2; yy2 := y1; end
  else if (liney0=liney1) then
    begin xx1 := x1; xx2 := x2; yy1 := liney0; yy2 := yy1; end
  else
  begin
    slope := (liney1-liney0)/(linex1-linex0);
    xx1 := x1; yy1 := liney0 + round((x1-linex0)*slope);
    if (yy1<y1) then
      begin yy1 := y1; xx1 := linex0 + round((y1-liney0)/slope); end;
    if (yy1>y2) then
      begin yy1 := y2; xx1 := linex0 + round((y2-liney0)/slope); end;

    xx2 := x2; yy2 := liney0 + round((x2-linex0)*slope);
    if (yy2<y1) then
      begin yy2 := y1; xx2 := linex0 + round((y1-liney0)/slope); end;
    if (yy2>y2) then
      begin yy2 := y2; xx2 := linex0 + round((y2-liney0)/slope); end;
  end;

  linex0 := xx1; liney0 := yy1; if cvmode then line(xx2,yy2,false);
  n := round(sqrt(sqr(xx2-xx1)+sqr(yy2-yy1)))+1; ok := true;
  makecrosssect(xwaarde(xx1),ywaarde(yy1,coordas),
                xwaarde(xx2),ywaarde(yy2,coordas),n,ok);
  if cvmode then line(xx2,yy2,false);
  if not ok then shift := [];
end;

{------------------------------------------------------------------------------}

procedure xycopytoclipboard; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{kopieert grafiek naar klembord}
begin
  screen.cursor := crhourglass; copyscreen(0);
  clipboard.assign(screencopy);
  screencopy.free; screen.cursor := crdefault;
end;

procedure xysaveasbitmap(s:string; showtxt:boolean; var ok:boolean); {XXXXXXXXX}
begin
  if (s='') then s := 'xygraph.bmp';
  s := changefileext(s,'.bmp'); ok := true;

  screen.cursor := crhourglass; copyscreen(1);
  try screencopy.savetofile(s) except ok := false end;
  screencopy.free; screen.cursor := crdefault;

  if showtxt then
  if not ok then
    begin
      beep;
      if (taal=1) then s := 'Fout bij schrijven bitmap '+s
                  else s := 'Error writing bitmap '+s;
      application.messagebox(@s[1],'File Error',mb_OK+mb_Iconerror);
    end
  else
   begin
     if (taal=1) then s := 'OK: bitmap '+s+' aangemaakt.'
                 else s := 'OK: bitmap '+s+' created.';
     showmessage(s);
   end;
end;

procedure initprinter; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var asp, pasp : single;
    pw,ph,prw,prh,fx,fy,f : integer;
const fillfac = 0.9;
begin
   asp := cheight/cwidth; prfac := 0;
   if (asp>1) then printer.orientation := poportrait
              else printer.orientation := polandscape;
   pw := printer.pagewidth; ph := printer.pageheight; pasp := ph/pw;
   if (pasp>asp) then
     begin prw := round(pw * fillfac); prh := round(prw * asp); end
   else
     begin prh := round(ph * fillfac); prw := round(prh / asp); end;
   fx := prw div cwidth; fy := prh div cheight; f := min(fx,fy);
   if (f=0) then exit;
   prw := cwidth * f; prh := cheight * f;
   prxoff := (pw-prw) div 2; pryoff := (ph-prh) div 2;
   prfac := f;
end;

procedure xyprint2(corr:single); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{ documentproperties getprinter openprinter }
var hasbmp : boolean;                 {grafiek heeft bmp}
begin
   if cvmode then exit;
   initprinter; if (prfac=0) then begin beep; exit; end;

   hasbmp := xybuffers[0].bmp;
   with printer do ffac := sqrt(pagewidth*pageheight)/1000;
   if (corr>0) then ffac := ffac / corr;

   prshow := false;

   screen.cursor := crhourglass;
   if hasbmp then copyscreen(2);
   xoff := prxoff; yoff := pryoff; res := prfac; doprint := true;
   try
     printer.begindoc;
     xypaintbox.repaint;
     printer.enddoc;
   except beep; end;
   res := 1; doprint := false; xoff := 0; yoff := 0;

   if hasbmp then
     begin restorefromprint; restorescreen; end
   else xypaintbox.repaint;
   screen.cursor := crdefault;
end;

procedure xyprint(show:boolean); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{print grafiek}
begin
  initprinter; if (prfac=0) then begin beep; exit; end;
  screen.cursor := crhourglass;
  copyscreen(2); prshow := show;

  try
    printer.begindoc;
    printpart(0,0,cwidth-1,cheight-1);
    printer.enddoc;
  except beep; end;

  if show then restorescreen else screencopy.free;
  screen.cursor := crdefault;
end;

procedure xysaveasmetafile2(s:string; showtxt:boolean; var ok:boolean; {XXXXXXX}
  scale : integer); {maakt metafile aan}
var hasbmp : boolean;                 {grafiek heeft bmp}
begin
  if cvmode {or xybuffers[0].str} then exit;
  if (scale<1) then scale := 1;
  hasbmp := xybuffers[0].bmp; openwmf(s,ok);
  if not ok then
   begin
     beep;
     if (taal=1) then s := 'Fout bij openen metafile '+s
                 else s := 'Error opening metafile '+s;
     if showtxt then application.messagebox(@s[1],'File Error',mb_OK+mb_Iconerror);
     exit;
   end;

  screen.cursor := crhourglass;
  if hasbmp then copyscreen(0);
  res := scale; xypaintbox.repaint;
  closewmf(ok);
  res := 1;
  if hasbmp then restorescreen else xypaintbox.repaint;
  screen.cursor := crdefault;

  if not ok then
    begin
      beep;
      if (taal=1) then s := 'Fout bij schrijven metafile '+s
                  else s := 'Error writing metafile '+s;
      if showtxt then application.messagebox(@s[1],'File Error',mb_OK+mb_Iconerror);
      exit;
    end;
  if (taal=1) then s := 'OK: metafile '+s+' aangemaakt.'
              else s := 'OK: metafile '+s+' created.';
  if showtxt then showmessage(s);
end;

procedure xysaveasmetafile(s:string; showtxt:boolean; var ok:boolean); {XXXXXXX}
begin xysaveasmetafile2(s,showtxt,ok,1); end;
procedure xysaveasmetafile(s:string; showtxt:boolean; var ok:boolean; scale:integer);
begin xysaveasmetafile2(s,showtxt,ok,scale); end;

{MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM}

procedure XYMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i,dx,dy,n : integer;
    yinc : single;
    rect : Trect;
    pnt : Tpoint;
begin
  if (istracking>0) then closetrack;
  crossmode := crossmode and 7;
  if (button=mbleft) then begin ndata := 0; track := 0; end;
  if not (crosson or blokon or lineon) then {zoek welke grafiek en yas}
    begin
      igraph := 0;
      for i := 1 to ngraph do with graphs[i] do if ok then
        if (x>=x1) and (x<=x2) and (y>=y1) and (y<=y2) then igraph := i;
      if (igraph=0) then exit;
      coordas := 0; crossrel := false;
      if (graphs[igraph].polar=0) then
      with graphs[igraph] do begin
        for i := 4 downto 1 do if graphs[igraph].as_[i].on
           then coordas := i;
        if (coordas=0) or not as_[0].on then exit;
        if (dat<>nil) and trackok then
          begin ndata := length(dat^)-1; nkol := length(dat^[0])-1;
       track := trck; trackas := tras; trackkl := trkl; end;
       doshowtrack := (track>0) and doshw; 
        xdata := 0; xdataold := 0; ydata := 0; ydataold := 0;
      end;
      with graphs3d[igraph] do
        begin empty := empt; hasempty := hasempt; end;
    end;

  with graphs[igraph] do
  if (button=mbleft) then
    if (ssCtrl in shift) and ( (not mode3d) or cont ) then
      begin blokon := true; blokx0 := x; bloky0 := y; blok(x,y,true); end
    else if (ssShift in shift) and cont and crsok then
      begin lineon := true; linex0 := x; liney0 := y; line(x,y,true); end
    else
     if (polar=0) and ( (x>as_[0].org) and (X<as_[0].org+as_[0].len)
      and (y<as_[coordas].org) and (y>as_[coordas].org-as_[coordas].len) )
      or ( (x>xpc-rp) and (x<xpc+rp) and (y>ypc-rp) and (y<ypc+rp) ) then
      begin

        crosson := true; crossrel := false;
        rul3d := cont and checkrul3d;
        if (polar>0) then begin snapx := 0; snapy := 0; end
        else
          begin
            with as_[0] do
              if snapxok then snapx := incr else snapx := 0;
            n := 0; yinc := 0;
            for i := 1 to 4 do with as_[i] do
              if on then begin inc(n); yinc := incr; end;
            if (n=1) and snapyok then snapy := yinc else snapy := 0;
          end;

        if (crossmode<4) then
        if (track>0) then screen.cursor := crhandpoint
        else if (polar>0) or (crossmode=3) then screen.cursor := crcross
           else screen.cursor := crnone;
        if (track>0) and (crossmode=3) then crossmode := 11;
        if (crossmode>2) and (track=1) then track := 2;

        getcursorpos(pnt); dx := pnt.x-x; dy := pnt.y-y;
        with rect do with graphs[igraph] do if (polar=0) then
         begin
         with as_[coordas] do begin top := dy+org-len; bottom := dy+org+1; end;
         with as_[0] do begin left := dx+org; right := dx+org+len+1; end; end
         else
           begin
             top := dy+ypc-rp; bottom := dy+ypc+rp;
             left := dx+xpc-rp; right := dx+xpc+rp;
           end;
        clipcursor(@rect);
        snap(x,y); cross(x,y,true);
      end;

  if (button=mbleft) and (crosson or blokon or lineon) then
    begin fillexport(x,y,xyexporta); fillexport(x,y,xyexportb);
     fillexport(x,y,xyexportc); fillexport(x,y,xyexportd); end;

  if (button=mbright) and coordok and (crosson or blokon or lineon) then
    begin
      if lineon then line(linex1,liney1,false) else
      if crosson then cross(crossx,crossy,false)
                 else blok(blokx1,bloky1,false);
      repeat
       inc(coordas);
       if (coordas=5) then coordas := 1;
       until graphs[igraph].as_[coordas].on;
      if lineon then line(linex1,liney1,true) else
      if crosson then cross(crossx,crossy,true)
                 else blok(blokx1,bloky1,true);
    end;

  if (button=mbmiddle) and crosson and (graphs[igraph].polar=0)
    then setcrossrel;
end;

procedure XYMouseMove(Shift: TShiftState;  X, Y: Integer); {XXXXXXXXXXXXXXXXXXX}
begin
  if (ssleft in shift) then
    begin
      if crosson then
         begin snap(x,y); if (x=crossx) and (y=crossy) then exit;
            cross(crossx,crossy,false); fillexport(x,y,xyexportd); cross(x,y,true);
            xdataold := xdata; ydataold := ydata;
         end;
      if blokon then begin blok(blokx1,bloky1,false); fillexport(x,y,xyexportd); blok(x,y,true); end;
      if lineon then begin line(linex1,liney1,false); fillexport(x,y,xyexportd); line(x,y,true); end;
    end;
end;

procedure XYMouseUp(Button: TMouseButton; var Shift: TShiftState; X, Y: Integer);
begin
  if (button=mbleft) then
   if (track>0) and (ssshift in shift) then 
    begin
      istracking := igraph; crosson := false; blokon := false; lineon := false;
    end
  else
    begin
      xdataold := xdata; ydataold := ydata;
      if crosson then  begin cross(crossx,crossy,false);
        crosson := false; crossx := 0; crossy := 0;
        if crossrel then tekencross(crossx0,crossy0);
        screen.cursor := crdefault; fillexport(x,y,xyexportb);
       end;
      if blokon then  begin
        blok(blokx1,bloky1,false); blokon := false; fillexport(x,y,xyexportb);
        if (ssCtrl in shift) then zoom;
       end;
      if lineon then begin
        line(linex1,liney1,false); lineon := false; fillexport(x,y,xyexportb);
        if (ssShift in shift) then initcrosssect(shift);
       end;
    end;
  clipcursor(nil);
end;

procedure XYKeyDown(var Key: Word; Shift: TShiftState); {XXXXXXXXXXXXXXXXXXXXXX}
var dxf : double; dyf : single; scr : boolean;
begin
  if (istracking>0) then
    begin
    scr := ssctrl in shift;
    case key of
     VK_RIGHT, VK_NUMPAD6 :  begin stattrack(1,scr); end;
     VK_LEFT, VK_NUMPAD4 :   begin stattrack(2,scr); end;
     VK_UP, VK_NUMPAD8 :     begin stattrack(3,scr); end;
     VK_DOWN, VK_NUMPAD2 :   begin stattrack(4,scr); end;
     VK_RETURN : begin closetrack; end;
    end;
    key := 0; exit;
    end;

  if (key=32) and (graphs[igraph].polar=0) then
    begin setcrossrel; key := 0; exit; end;

  if cvmode or mode3d then exit;

  if (izoom=0) and (zgraph>0) then izoom := zgraph;
  if (izoom<=0) or not graphs[izoom].ok then exit;

  with graphs[izoom] do begin

  if (zmxy<zmax) then zmxy := zmax;

  dxf := xf1-xf0; dyf := yf1-yf0;
  if ssctrl in shift then
    case key of
      VK_ADD :
        begin
          xf0 := xf0 + dxf / 4; xf1 := xf1 - dxf / 4;
          yf0 := yf0 + dyf / 4; yf1 := yf1 - dyf / 4;
          if (xf1-xf0)<zmxx then
            begin xf0 := (xf1+xf0)/2-zmxx/2; xf1 := xf0+zmxx; end;
          if (yf1-yf0)<zmxy then
            begin yf0 := (yf1+yf0)/2-zmxy/2; yf1 := yf0+zmxy; end;
          xypaintbox.repaint;
        end;
     VK_SUBTRACT :
        begin
          xf0 := xf0 - dxf / 2; xf1 := xf1 + dxf / 2;
          yf0 := yf0 - dyf / 2; yf1 := yf1 + dyf / 2;
          if (xf0<0) then xf0 := 0; if (xf1>1) then xf1 := 1;
          if (yf0<0) then yf0 := 0; if (yf1>1) then yf1 := 1;
          xypaintbox.repaint;
        end;
     VK_RIGHT, VK_NUMPAD6 :
        begin
          xf1 := xf1 + dxf / 4; if (xf1>1) then xf1 := 1;
          xf0 := xf1 - dxf;
          xypaintbox.repaint;
        end;
     VK_LEFT, VK_NUMPAD4 :
        begin
          xf0 := xf0 - dxf / 4; if (xf0<0) then xf0 := 0;
          xf1 := xf0 + dxf;
          xypaintbox.repaint;
        end;
     VK_UP, VK_NUMPAD8 :
        begin
          yf1 := yf1 + dyf / 4; if (yf1>1) then yf1 := 1;
          yf0 := yf1 - dyf;
          xypaintbox.repaint;
        end;
     VK_DOWN, VK_NUMPAD2 :
        begin
          yf0 := yf0 - dyf / 4; if (yf0<0) then yf0 := 0;
          yf1 := yf0 + dyf;
          xypaintbox.repaint;
        end;
     VK_RETURN :
        begin
          xyunzoom(izoom);
          xypaintbox.repaint;
        end;
    end;
  end;
end;

procedure xyclearbuffer; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  bufcount := 0; xyclearbuffer3d;
  setlength(xybuffers,1); setlength(buffer,1);
end;

procedure xyputbuffer(n:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var i,t : integer;
begin
  if (doprint or dowmf) then exit;
  t := -1;
  for i := 0 to bufcount-1 do if (buffer[i].labl=n) then t := i;
  if (t=-1) then
    begin inc(bufcount); setlength(buffer,bufcount);
    setlength(xybuffers,bufcount+1); t := bufcount-1; end;
  with buffer[t] do
    begin
      labl := n; pblabel := labl;
      graphdat1 := graphs;
      graphdat2 := xygraphdata;
      pixx := xypixx; pixy := xypixy;
      charh := xycharheight; charw := xycharwidth;
      zoomx := xyzoomx; zoomy := xyzoomy;
      legendcnt := xylegendcount;
      canw := cwidth; canh := cheight;
      izm := izoom; zgrph := zgraph;
      crx := crossxp; cry := crossyp; crj := crossj; crok := coordok;
      ccol := coordcolor; fcol := frontcolor; bcol := backcolor;
      cvm := cvmode; m3d := mode3d;
      if cvm then canvas := xycanvas
          else paintbox := xypaintbox;
    end;
  xyputbuffer3d(t);
  with xybuffers[t+1] do begin lbl := n;
    form := xybuffers[0].form; ngr := xybuffers[0].ngr;
    cw :=  xybuffers[0].cw;     ch := xybuffers[0].ch;
    cvm := xybuffers[0].cvm;   bmp := xybuffers[0].bmp;
    str := xybuffers[0].str;   b4d := xybuffers[0].b4d; end;
end;

procedure xygetbuffer(n:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var i,t : integer;
begin
  t := -1; if (istracking>0) then closetrack;
  for i := 0 to bufcount-1 do if (buffer[i].labl=n) then t := i;
  if (t=-1) then exit;
  with buffer[t] do
    begin
      pblabel := labl;
      graphs := graphdat1;
      xygraphdata := graphdat2;
      xypixx := pixx; xypixy := pixy;
      xycharheight := charh; xycharwidth := charw;
      xyzoomx := zoomx; xyzoomy := zoomy;
      xylegendcount := legendcnt;
      izoom := izm; zgraph := zgrph;
      cwidth := canw; cheight := canh;
      crossxp := crx; crossyp := cry; crossj := crj; coordok := crok;
      coordcolor := ccol; frontcolor := fcol; backcolor := bcol;
      cvmode := cvm; mode3d := m3d;
      if cvm then xycanvas := canvas
        else begin xypaintbox := paintbox; xycanvas := paintbox.canvas; end;
    end;
  xypen := xycanvas.pen; xybrush := xycanvas.brush; xyfont := xycanvas.font;
  xygetbuffer3d(t);
  with xybuffers[t+1] do begin
     xybuffers[0].lbl := lbl; xybuffers[0].ngr := ngr;
     xybuffers[0].cw := cw;   xybuffers[0].ch := ch;
     xybuffers[0].cvm := cvm; xybuffers[0].bmp := bmp;
     xybuffers[0].str := str; xybuffers[0].b4d := b4d; end;
end;

procedure copyzoom(n:integer;x0,x1,y0,y1:single); {XXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  graphs[n].xf0 := x0; graphs[n].xf1 := x1;
  graphs[n].yf0 := y0; graphs[n].yf1 := y1;
end;

procedure initxygraph; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var versioninfo : osversioninfo;
begin
  xyunzoom(0); taal := 0; pblabel := 0;
  track := 0; istracking := 0; init := false;
  res := 1; xoff := 0; yoff := 0; status := 0;
  oldpen := Tpen.create; oldbrush := Tbrush.create;
  versioninfo.dwOSVersionInfoSize := sizeof(osversioninfo);
  if getversionex(VersionInfo) then winnt :=
    (VersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT) {=2}
    else winnt := false;
  xyclearbuffer; initdate;
  initxycommon;  initxygraph3d;
end;

begin
  initxygraph;
  {for C++ users: discard these two lines and call initxygraph from your
   application at program start-up}
end.

