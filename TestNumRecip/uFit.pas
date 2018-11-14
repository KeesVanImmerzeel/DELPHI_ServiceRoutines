unit uFit;

interface

uses
  SysUtils, Vcl.Dialogs, uNumRecip;

  Function GetLinearEstimate( x,y: TDynamicDouble; aXvalue: Double ): Double;

  procedure LinearLeastSquares( x,y: TDynamicDouble; var dB, dM, dR: Double);
  {- dB = intercept; dM= slope }

implementation

Function GetLinearEstimate( x,y: TDynamicDouble; aXvalue: Double ): Double;
//var dM,dB, dR: Double;
VAR a, b, siga, sigb, chi2, q: Double;
begin
//  LinearLeastSquares( x,y,dB,dM, dR );
//    ShowMessage( FloatTostr( dM )+ ' '+ FloatToStr( dB ) );
//  Result := dM * aXvalue + dB;
//x.WriteToTextFile( 'd:\tmp\x.txt');
//y.WriteToTextFile( 'd:\tmp\y.txt');

  if fit(x,y,nil,0,a,b,siga,sigb,chi2,q) then;
  Result := a + b * aXvalue;
end;

{************* LinearLeastSquares *******************}
 procedure LinearLeastSquares( x,y: TDynamicDouble; var dB, dM, dR: Double);
 {Line "Y = mX + b" is linear least squares line for the input array, "data",
  of TRealPoint}
var
  SumX, SumY, SumX2, SumY2, SumXY, a, b, r: extended;
  Sx,Sy :extended;
  n, i: Integer;

begin
  n := x.Length; {number of points}
  SumX := 0.0;  SumY := 0.0;
  SumX2 := 0.0;  SumY2:=0.0;
  SumXY := 0.0;

  for i := 0 to n - 1 do
  begin
    SumX := SumX + X[i];
    SumY := SumY + Y[i];
    SumX2 := SumX2 + X[i]*X[i];
    SumY2 := SumY2 + Y[i]*Y[i];
    SumXY := SumXY + X[i]*Y[i];
  end;

  if (n*SumX2=SumX*SumX) or (n*SumY2=SumY*SumY)
  then
  begin
    showmessage('LeastSquares() Error - X or Y  values cannot all be the same');
    a:=0;
    b:=0;
  end
  else
  begin
    a:=((n * SumXY) - (SumX * SumY)) / ((n * SumX2) - (SumX * SumX));  {Slope M}
    B:=(sumy-a*sumx)/n;  {Intercept a}
    Sx:=sqrt(Sumx2-sqr(sumx)/n);
    Sy:=Sqrt(Sumy2-sqr(Sumy)/n);
    r:=(Sumxy-Sumx*sumy/n)/(Sx*sy);
    //RSquared:=r*r;
  end;
  ShowMessage( FloatTostr( a )+ ' '+ FloatToStr( b ) );
  dM := a;
  dB := B;
  dR := r;
end;

end.
