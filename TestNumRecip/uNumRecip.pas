unit uNumRecip;
{-Numerical recipes data types}

interface

Uses
  SysUtils, Vcl.Dialogs, math;

Type
  // glnarray = Array of Double;
  char12 = string[12];
  FUNCTION sngl(x:Double):Double;

type
  {-1 based dynamic integer array}
  TDynamicInteger = class
  private
    FArray: Array of Integer;
    function GetValue(Index: Integer): Integer;
    procedure SetValue(Index, Value: Integer);
    function GetLength: Integer;
    procedure SetLength(const Value: Integer);
  public
    constructor Create(Size: Integer);

    destructor Destroy; override;
  public
    property Length: Integer read GetLength write SetLength;
    property Value[index: integer]: Integer read GetValue write SetValue; default;
  end; (* TDynamicInteger *)

//Example usage:
// var
//  X: TDynamicInteger;
//initialization
//  X := TDynamicInteger.Create(4);
//  X[1] := 1;
//  X[2] := 2;
//  X[3] := 3;
//  X[4] := 4;
//finalization
//  X.Free
//end.

  {-1 based dynamic double array}
  TDynamicDouble = class
  private
    FArray: Array of Double;
    pvMissingValue: Double;
    function GetValue(Index: Integer): Double;
    procedure SetValue(Index: Integer; Value: Double);
    function GetLength: Integer;
    procedure SetLength(const Value: Integer);
    function GetMissingValue: Double;
    Procedure SetMissingValue( const aValue: Double );
  public
    property MissingValue: Double Read GetMissingValue Write SetMissingValue;
    constructor Create( const Size: Integer); overload;
    Constructor Create( const Size:Integer; const aValue: Double ); overload;
    Constructor Create( const Size:Integer; const aValue, aMissingValue: Double ); overload;
    destructor Destroy; override;
    Procedure WriteToTextFile( const aFileName: String ); overload;
    Procedure WriteToTextFile( const aFileName: String; const NrValues: Integer ); overload;
    Function GetMedian: Double;
    Function AllValuesAreTheSame( var theValue: Double ): Boolean;
    Function MaxValue( var aIndex: Integer ): Double;
  public
    property Length: Integer read GetLength write SetLength;
    property Value[index: integer]: Double read GetValue write SetValue; default;
  end; (* TDynamicDouble *)

var
 gliset: integer;
 glgset: Double;
 glinext,glinextp: integer;
 glma: ARRAY [1..55] OF double;

{-Fit a streight line y=a+bx to a given set of points x[1..ndata], y[1..ndata]
 with standard deviations sig[1..ndata] (=measurement error of y[]),
 by minimizing Gi-squared.
 Returned are a, b and ther respective probable uncertainties siga and sigb, the
 Chi-square chi2 and the goodness of fit probability q (that the fit would have
 Chi-square this large, or larger). If q is larger then, say, 0.1, then the
 goodness of fit is believable; If it is larger then, say 0.001 then the fit may
 be acceptable if the errors are nonnormal or have moderately underestimated.
 if a is less than 0.001 then the model and/or estimation procedure can rightly
 be called into question.
 If mwt=0 on input, then the standard deviations are assumed to be unavailable;
 q is returned as 1.0 and the normalization of chi2 is to unit standard deviation
 on all points.
}
  Function fit( x,y, sig: TDynamicDouble;
              mwt: integer; VAR a,b,siga,sigb,chi2,q: Double): Boolean;

  PROCEDURE glopen(VAR infile:textfile; filename:string);
  {-'Incomplete Gamma Function'}
  FUNCTION gammq(a,x: Double; var gammqResult: Double ): Boolean;
  {-Returns the incomplete gamma function P(a,x) evaluated by its series
    representation as gamser. Also returns gln(a)}
  Function gser(a,x: Double; VAR gamser,gln: Double): Boolean;
  FUNCTION gasdev(VAR idum: integer): Double;
  FUNCTION ran3(VAR idum: integer): Double;
  FUNCTION gammln(xx: Double): Double;
  Function gcf( a,x: Double; VAR gammcf,gln: Double ): Boolean;

implementation

FUNCTION sngl(x:Double):Double;
BEGIN sngl := x END;

PROCEDURE glopen(VAR infile:textfile; filename:string);
BEGIN assignFile(infile,filename); reset(infile) END;

FUNCTION ran3(VAR idum: integer): Double;
(* Programs using RAN3 must declare the following variables
VAR
   glinext,glinextp: integer;
   glma: ARRAY [1..55] OF Double;
in the main routine. Machines with 4-byte integers can use the integer
implementation of this routine, substituting glma of type integer, the
commented CONST and VAR declarations, and the MOD function in the third
line after the BEGIN. *)
(* CONST
   mbig=1000000000;
   mseed=161803398;
   mz=0;
   fac=1.0e-9;
VAR
   i,ii,k,mj,mk: integer; *)
CONST
   mbig=4.0e6;
   mseed=1618033.0;
   mz=0.0;
   fac=2.5e-7; (* 1/mbig *)
VAR
   i,ii,k: integer;
   mj,mk: Double;
BEGIN
   IF (idum < 0) THEN BEGIN
      mj := mseed+idum;
      (* The following IF block is mj := mj MOD mbig; for Double variables. *)
      IF mj>=0.0 THEN mj := mj-mbig*trunc(mj/mbig)
         ELSE mj := mbig-abs(mj)+mbig*trunc(abs(mj)/mbig);
      glma[55] := mj;
      mk := 1;
      FOR i := 1 TO 54 DO BEGIN
         ii := 21*i MOD 55;
         glma[ii] := mk;
         mk := mj-mk;
         IF (mk < mz) THEN mk := mk+mbig;
         mj := glma[ii]
      END;
      FOR k := 1 TO 4 DO BEGIN
         FOR i := 1 TO 55 DO BEGIN
            glma[i] := glma[i]-glma[1+((i+30) MOD 55)];
            IF (glma[i] < mz) THEN glma[i] := glma[i]+mbig
         END
      END;
      glinext := 0;
      glinextp := 31;
      idum := 1
   END;
   glinext := glinext+1;
   IF (glinext = 56) THEN glinext := 1;
   glinextp := glinextp+1;
   IF (glinextp = 56) THEN glinextp := 1;
   mj := glma[glinext]-glma[glinextp];
   IF (mj < mz) THEN mj := mj+mbig;
   glma[glinext] := mj;
   ran3 := mj*fac
END;


FUNCTION gasdev(VAR idum: integer): Double;
(* Programs using GASDEV must declare the variables
VAR
   gliset: integer;
   glgset: Double;
in the main routine and must intialize gliset to
   gliset := 0;   *)
VAR
   fac,r,v1,v2: Double;
BEGIN
   IF  (gliset = 0)  THEN BEGIN
      REPEAT
         v1 := 2.0*ran3(idum)-1.0;
         v2 := 2.0*ran3(idum)-1.0;
         r := sqr(v1)+sqr(v2);
      UNTIL (r < 1.0);
      fac := sqrt(-2.0*ln(r)/r);
      glgset := v1*fac;
      gasdev := v2*fac;
      gliset := 1
   END ELSE BEGIN
      gasdev := glgset;
      gliset := 0
   END
END;

FUNCTION gammln(xx: Double): Double;
CONST
   stp = 2.50662827465;
   half = 0.5;
   one = 1.0;
   fpf = 5.5;
VAR
   x,tmp,ser: double;
   j: integer;
   cof: ARRAY [1..6] OF double;
BEGIN
   cof[1] := 76.18009173;
   cof[2] := -86.50532033;
   cof[3] := 24.01409822;
   cof[4] := -1.231739516;
   cof[5] := 0.120858003e-2;
   cof[6] := -0.536382e-5;
   x := xx-one;
   tmp := x+fpf;
   tmp := (x+half)*ln(tmp)-tmp;
   ser := one;
   FOR j := 1 TO 6 DO BEGIN
      x := x+one;
      ser := ser+cof[j]/x
   END;
   gammln := sngl(tmp+ln(stp*ser));
END;


Function gcf( a,x: Double; VAR gammcf,gln: Double ): Boolean;
LABEL 1;
CONST
   itmax=100;
   eps=3.0e-7;
VAR
   n: integer;
   gold,g,fac,b1,b0,anf,ana,an,a1,a0: Double;
BEGIN
  Result := false;
  Try

// ********* Original code ***************************
   gln := gammln(a);
   gold := 0.0;
   a0 := 1.0;
   a1 := x;
   b0 := 0.0;
   b1 := 1.0;
   fac := 1.0;
   FOR n := 1 TO itmax DO BEGIN
      an := 1.0*n;
      ana := an-a;
      a0 := (a1+a0*ana)*fac;
      b0 := (b1+b0*ana)*fac;
      anf := an*fac;
      a1 := x*a0+anf*a1;
      b1 := x*b0+anf*b1;
      IF (a1 <> 0.0) THEN BEGIN
         fac := 1.0/a1;
         g := b1*fac;
         IF (abs((g-gold)/g) < eps) THEN GOTO 1;
         gold := g
      END
   END;
   // writeln('pause in GCF - a too large, itmax too small'); readln;
   raise Exception.Create('in GCF - a too large, itmax too small.');
1:   gammcf := exp(-x+a*ln(x)-gln)*g;
// ********* Original code ***************************

  Result := true;
  Except
    On E: Exception do begin
      // ShowMessage( E.Message );
    end;
  End;
END;

{-Returns the incomplete gamma function P(a,x) evaluated by its series
  representation as gamser. Also returns gln(a)}
Function gser(a,x: Double; VAR gamser,gln: Double): Boolean;
LABEL 1;
CONST
   itmax=100;
   eps=3.0e-7;
VAR
   n: integer;
   sum,del,ap: Double;
BEGIN
  Result := false;
  Try
   gln := gammln(a);
// ********* Original code ***************************
   IF (x <= 0.0) THEN BEGIN
      IF (x < 0.0) THEN BEGIN
         // writeln('pause in GSER - x less than 0'); readln
         raise Exception.Create('In GSER - x less than 0.');
      END;
      gamser := 0.0
   END ELSE BEGIN
      ap := a;
      sum := 1.0/a;
      del := sum;
      FOR n := 1 TO itmax DO BEGIN
         ap := ap+1.0;
         del := del*x/ap;
         sum := sum+del;
         IF (abs(del) < abs(sum)*eps) THEN GOTO 1
      END;
      raise Exception.Create('GSER - a too large, itmax too small');
      // writeln('pause in GSER - a too large, itmax too small'); readln;
1:      gamser := sum*exp(-x+a*ln(x)-gln)
   END;
// ********* Original code ***************************

   Result := true;
  Except
    On E: Exception do begin
      ShowMessage( E.Message );
    end;
  End;
END;

{-'Incomplete Gamma Function, complementary. p'}
FUNCTION gammq(a,x: Double; var gammqResult: Double ): Boolean;
VAR
   gamser,gammcf, gln: Double;
BEGIN
  Result := false;
  gammqResult := 0;
  Try

   IF ((x < 0.0) OR (a <= 0.0)) THEN Raise
     Exception.create( 'Invalid arguments in GAMMQ.' );

// ********* Original code ***************************

   IF (x < a+1.0) THEN BEGIN
      if not gser(a,x,gamser,gln) then
        raise Exception.Create('Error Message gser');
      gammqResult := 1.0-gamser
   END ELSE BEGIN
      if not gcf(a,x,gammcf,gln) then
        raise Exception.Create('Error in gammq.');
      gammqResult := gammcf;
   END;
// ********* Original code ***************************

    Result := true;
  Except
    On E: Exception do begin
      ShowMessage( E.Message );
    end;
  End;
END;

Function fit( x, y, sig: TDynamicDouble;
              mwt: integer; VAR a, b, siga, sigb, chi2, q: Double): Boolean;

VAR
   i, ndata: integer;
   wt, t, sy, sxoss, sx, st2, ss, sigdat: Double;
BEGIN
  Try
    Result := false;
    ndata := x.Length;
    if not ( ndata > 1 ) and
           ( ndata = y.Length ) and
           ( ndata = sig.Length ) then
      raise Exception.Create('Invalid length of x and/or y arrays in function "fit".');

// ********* Original code ***************************

   sx  := 0.0;
   sy  := 0.0;
   st2 := 0.0;
   b   := 0.0;
   IF (mwt <> 0)THEN BEGIN
      ss := 0.0;
      FOR i := 1 TO ndata DO BEGIN
         wt := 1.0/sqr(sig[i]);
         ss := ss+wt;
         sx := sx+x[i]*wt;
         sy := sy+y[i]*wt
      END
   END ELSE BEGIN
      FOR i := 1 TO ndata DO BEGIN
         sx := sx+x[i];
         sy := sy+y[i]
      END;
      ss := ndata
   END;
   sxoss := sx/ss;
   IF (mwt <> 0)THEN BEGIN
      FOR i := 1 TO ndata DO BEGIN
         t := (x[i]-sxoss)/sig[i];
         st2 := st2+t*t;
         b := b+t*y[i]/sig[i]
      END
   END ELSE BEGIN
      FOR i := 1 TO ndata DO BEGIN
         t := x[i]-sxoss;
         st2 := st2+t*t;
         b := b+t*y[i]
      END
   END;
   b := b/st2;
   a := (sy-sx*b)/ss;
   siga := sqrt((1.0+sx*sx/(ss*st2))/ss);
   sigb := sqrt(1.0/st2);
   chi2 := 0.0;
   IF (mwt = 0)THEN BEGIN
      FOR i := 1 TO ndata DO BEGIN
         chi2 := chi2+sqr(y[i]-a-b*x[i])
      END;
      q := 1.0;
      sigdat := sqrt(chi2/(ndata-2));
      siga := siga*sigdat;
      sigb := sigb*sigdat
   END ELSE BEGIN
      FOR i := 1 TO ndata DO BEGIN
         chi2 := chi2+sqr((y[i]-a-b*x[i])/sig[i])
      END;
      if not gammq(0.5*(ndata-2),0.5*chi2, q ) then
        raise Exception.Create('Error calculation q in "fit".');
      // q := gammq(0.5*(ndata-2),0.5*chi2)
   END;

// ********* Original code ***************************

   Result := True;

  Except
    On E: Exception do begin
      ShowMessage( E.Message );
    end;
  End;

END;

// ********************** TDynamicInteger array *****************************

constructor TDynamicInteger.Create(Size: Integer);
begin
  inherited Create;
  System.SetLength(FArray, Size)
end;

destructor TDynamicInteger.Destroy;
begin
  FArray := nil;
  inherited;
end;



function TDynamicInteger.GetLength: Integer;
begin
  Result := System.Length(FArray)
end;

procedure TDynamicInteger.SetLength(const Value: Integer);
begin
  System.SetLength(FArray, Value)
end;

function TDynamicInteger.GetValue(Index: Integer): Integer;
begin
  Result := FArray[Index-1] // 1-based array
end;

procedure TDynamicInteger.SetValue(Index, Value: Integer);
begin
  FArray[Index-1] := Value // 1-based array
end;


// ********************** TDynamicDouble array *****************************

constructor TDynamicDouble.Create( const Size: Integer);
begin
  inherited Create;
  System.SetLength(FArray, Size);
  pvMissingValue := -1.7E+308;
end;

Constructor  TDynamicDouble.Create( const Size:Integer; const aValue: Double );
var
  i,n: Integer;
begin
  Create( Size );
  n:= Length;
  for i := 1 to n do
    Value[ i ] := aValue;
end;

Constructor TDynamicDouble.Create( const Size:Integer; const aValue, aMissingValue: Double );
begin
  Create( Size, aValue );
  MissingValue := aMissingValue;
end;

function TDynamicDouble.GetMissingValue: Double;
begin
  Result := pvMissingValue;
end;

Procedure TDynamicDouble.SetMissingValue( const aValue: Double );
begin
  pvMissingValue := aValue;
end;

destructor TDynamicDouble.Destroy;
begin
  FArray := nil;
  inherited;
end;

Procedure TDynamicDouble.WriteToTextFile( const aFileName: String; const NrValues: Integer );
var
  f: TextFile;
  i, n: Integer;
begin
  AssignFile( f, aFileName ); Rewrite( f );
  n := min( Length, NrValues );
  for i := 1 to n do begin
    if Value[i] <> MissingValue then
      Writeln( f, floattostrf(Value[i], ffGeneral, 8, 2) )
    else
      Writeln( f, 'NA' );
  end;
  CloseFile( f );
end;

Function TDynamicDouble.MaxValue( var aIndex: Integer ): Double;
var
  i: Integer;
begin
  Result := MissingValue;
  aIndex := 0;
  if Length > 0 then begin
    Result := Value[ 1 ];
    aIndex := 1;
    if Length > 1 then
      for i := 1 to Length do begin
        if Value[ i ] > Result then begin
          Result := Value[ i ];
          aIndex := i;
        end;
      end;
  end;
end;

Procedure TDynamicDouble.WriteToTextFile( const aFileName: String );
begin
  WriteToTextFile( aFileName, Length );
end;

function TDynamicDouble.GetLength: Integer;
begin
  Result := System.Length(FArray)
end;

procedure TDynamicDouble.SetLength(const Value: Integer);
begin
  System.SetLength(FArray, Value)
end;

function TDynamicDouble.GetValue(Index: Integer): Double;
begin
  Result := FArray[Index-1] // 1-based array
end;

procedure TDynamicDouble.SetValue(Index: Integer; Value: Double);
begin
  FArray[Index-1] := Value // 1-based array
end;

Function TDynamicDouble.AllValuesAreTheSame( var theValue: Double ): Boolean;
var
  i, n: Integer;
  GeenVerschilGevonden: Boolean;
begin
  n        := Length;
  Result   := true;
  TheValue := 0;
  if ( n >= 1 ) then begin
    TheValue := Value[ 1 ];
    if ( n > 1 ) then begin
      i := 1 ;
      GeenVerschilGevonden := true;
      while ( ( i < n ) and GeenVerschilGevonden ) do begin
        Inc( i );
        GeenVerschilGevonden := ( TheValue = Value[ i ] );
      end;
    end;
  end else begin {-n < 1}
  end;
end;

Function  TDynamicDouble.GetMedian: Double;
LABEL 1;
CONST
   big=1.0e30;
   afac=1.5;
   amp=1.5;
VAR
   np,nm,j,n: integer;
   xx,xp,xm,sumx,sum,eps: double;
   stemp,dum,ap,am,aa,a: double;
begin
n := Length;

if AllValuesAreTheSame( Result ) then
  Exit;

// ********* Original code ***************************

BEGIN
   a := 0.5*(Value[1]+Value[n]);
   eps := abs(Value[n]-Value[1]);
   ap := big;
   am := -big;
1:   sum := 0.0; sumx := 0.0; np := 0; nm := 0; xp := big; xm := -big;
   FOR j := 1 TO n DO BEGIN
      xx := Value[j];
      IF (xx <> a) THEN BEGIN
         IF (xx > a) THEN BEGIN
            np := np+1;
            IF (xx < xp) THEN xp := xx END
         ELSE IF  (xx < a) THEN BEGIN
            nm := nm+1;
            IF (xx > xm) THEN xm := xx
         END;
         dum := 1.0/(eps+abs(xx-a));
         sum := sum+dum;
         sumx := sumx+xx*dum
      END
   END;
   stemp := (sumx/sum)-a;
   IF ((np-nm) >= 2) THEN BEGIN
      am := a;
      IF (stemp < 0.0) THEN aa := xp
      ELSE aa := xp+stemp*amp;
      IF (aa > ap) THEN aa := 0.5*(a+ap);
      eps := afac*abs(aa-a);
      a := aa;
      GOTO 1 END
   ELSE IF ((nm-np) >= 2) THEN BEGIN
      ap := a;
      IF (stemp > 0.0) THEN aa := xm
      ELSE aa := xm+stemp*amp;
      IF (aa < am) THEN aa := 0.5*(a+am);
      eps := afac*abs(aa-a);
      a := aa;
      GOTO 1 END
   ELSE IF (n MOD 2) = 0 THEN BEGIN
      IF (np = nm) THEN Result := 0.5*(xp+xm)
      ELSE IF (np > nm) THEN Result := 0.5*(a+xp)
      ELSE Result := 0.5*(xm+a) END
   ELSE BEGIN
      IF (np = nm) THEN Result := a
      ELSE IF (np > nm) THEN Result := xp
      ELSE Result := xm
   END
END;

// ********* Original code ***************************

end;

end.
