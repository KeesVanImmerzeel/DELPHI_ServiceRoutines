unit uPolInt;

interface

uses
  SysUtils, Vcl.Dialogs, uNumRecip;

CONST
   cNR_PolInt_Max_np = 10;   (* maximum value for n *)

Function polint( xa,ya: TDynamicDouble; x: Double; VAR y,dy: Double): Boolean;

implementation

Function polint( xa,ya: TDynamicDouble;
       x: Double; VAR y,dy: Double): Boolean;
VAR
   n, ns,m,i: integer;
   w,hp,ho,dift,dif,den: Double;
   c,d: TDynamicDouble;
BEGIN
   Try
      Result := false;
      n := xa.Length;
      if not ( n > 1 ) and
             ( n <= cNR_PolInt_Max_np ) and
             ( n = ya.Length ) then
        raise Exception.Create('Invalid length of xa and/or ya arrays in polint.');
      c:= TDynamicDouble.Create( n );
      d:= TDynamicDouble.Create( n );

// ********* Original code ***************************
   ns := 1;
   dif := abs(x-xa[1]);
   FOR i := 1 TO n DO BEGIN
      dift := abs(x-xa[i]);
      IF (dift < dif) THEN BEGIN
         ns := i;
         dif := dift
      END;
      c[i] := ya[i];
      d[i] := ya[i]
   END;
   y := ya[ns];
   ns := ns-1;
   FOR m := 1 TO n-1 DO BEGIN
      FOR i := 1 TO n-m DO BEGIN
         ho := xa[i]-x;
         hp := xa[i+m]-x;
         w := c[i+1]-d[i];
         den := ho-hp;
         IF (den = 0.0) THEN
           raise Exception.Create('den=0.0 in polint.');
         den := w/den;
         d[i] := hp*den;
         c[i] := ho*den
      END;
      IF ((2*ns) < (n-m)) THEN BEGIN
         dy := c[ns+1]
      END ELSE BEGIN
         dy := d[ns];
         ns := ns-1
      END;
      y := y+dy
   END;
// ********* Original code ***************************

   c.Free;
   d.Free;
   Result := True;

  Except
    On E: Exception do begin
      // ShowMessage( E.Message );
    end;
  End;

END;

end.
