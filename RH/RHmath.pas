unit RHmath;

interface

uses SysUtils, Windows, System.Math, RHsystem;

{Mathematical functions}
procedure DivMod(Dividend: Integer; Divisor: Word; var Result, Remainder: Word);
Function ABCFormula(a, b, c : Double; var s1, s2 : Double) : byte;
Function Interpolate(X2, X1, t, t2, t1 : Double) : Double;
Function MinI(I : array of LongInt) : LongInt;
Function MaxI(I : array of LongInt) : LongInt;
Function Average(A : array of Double) : Double;
Function Sign(X : Double) : ShortInt;
Function InvLog(X : Double) : Double;
Function TanH(X: extended) : extended;
Function Tan(X : Double) : Double;
Function CalcAngle(P1, P2 : TPoint) : Double;
Procedure Swap(var A, B : Double);
Procedure SwapW(var A, B : Double);
Function Order(R : Double) : SmallInt;
Function RoundOrderHigh( R : Double) : Double;
Function RoundOrder( R : Double) : Double;
Function Faculty(X : LongInt) : LongInt;
Function Poisson(Value, Av : LongInt) : Double;
Function CumPoisson(Value, Av : LongInt) : Double;
Function NormDist(X, Mean, StDev : Double) : Double;
Function CumNormDist(X, Mean, StDev : Double) : Double;
Function InvCumNormDist(Kans, Mean, StDev : double) : double;
Function OutsideRange(Mean, StDev, Min, Max : Double; Left : boolean) : Double;
Function CalcAge(BirthDate : TDateTime) : LongInt;
Function RoundDownDec(V : double; D : integer) : double;
Function RoundDec(V : double; D : integer) : double;
Function Deler(Min, Max, Interval : LongInt) : LongInt;

Procedure CalcPercICG(A1, A2, A3, A4, A5: integer; var P1, P2, P3, P4, P5 : integer);
Procedure CalcPerc(Aantallen : array of double; var Perc : array of double);
Function Rang(Getal : double; Arr : array of double; Oplopend : boolean) : integer;
Procedure QuickSortArray(var Arr : array of double);

{Physics}
Function CelciusToKelvin( C : Double) : Double;
Function KelvinToCelcius( K : Double) : Double;
Function CelciusToCal( C : Double) : Double;
Function CalToCelcius( C : Double) : Double;
Function KelvinToCal( K : Double) : Double;
Function CalToKelvin( C : Double) : Double;
Function WaterDensity( T : Double) : Double;

implementation

procedure DivMod(Dividend: Integer; Divisor: Word;
  var Result, Remainder: Word);
asm
        PUSH    EBX
        MOV     EBX,EDX
        MOV     EDX,EAX
        SHR     EDX,16
        DIV     BX
        MOV     EBX,Remainder
        MOV     [ECX],AX
        MOV     [EBX],DX
        POP     EBX
end;

Function ABCFormula( a, b, c : Double; var s1, s2 : Double) : byte;
var D : Double;
begin
  s1:= 0; s2:= 0;
  D:= b*b - 4*a*c;
  if D < 0 then Result:= 0
  else if D = 0 then Result:= 1
  else Result:= 2;
  if D = 0 then
  begin
    s1:= -b / (2*a); s2:= s1;
  end
  else if D > 0 then
  begin
    D:= SQRT(D);
    s1:= (-b - D) / (2 * a);
    s2:= (-b + D) / (2 * a);
  end;
end;

Function Interpolate( X2, X1, t, t2, t1 : Double) : Double;
begin
  if t2 <> t1 then Result:= (X2 - X1) * (t - t1) / (t2 - t1) + X1
  else Result:= X1;
end;

Function MinI(I : array of LongInt) : LongInt;
var counter : LongInt;
begin
  Result:= I[0];
  for counter:= Low(I) + 1 to High(I) do
    if I[counter] < Result then Result:= I[counter];
end;

Function MaxI(I : array of LongInt) : LongInt;
var counter : LongInt;
begin
  Result:= I[0];
  for counter:= Low(I) + 1 to High(I) do
    if I[counter] > Result then Result:= I[counter];
end;

Function Average(A : array of Double) : Double;
var i : LongInt;
begin
  Result:= 0;
  for i:= Low(A) to High(A) do
    Result:= Result + A[i];
  Result:= Result / (High(A) - Low(A) + 1);
end;

Function Sign( X : Double) : ShortInt;
begin
  if X = 0 then Result:= 0
  else if X < 0 then Result:= -1
  else Result:= 1;
end;

Function InvLog(X : Double) : Double;
begin
  Result:= Exp(X * ln(10));
end;

Function TanH( X: extended) : extended;
begin
  Result:= (1 - EXP(-2 * X)) / (1 + EXP(-2 * X));
end;

Function Tan( X : Double) : Double;
var S, C : extended;
begin
  SinCos(X, S, C);
  if C <> 0 then Result:= S / C
  else Result:= MaxReal;
end;

Function CalcAngle(P1, P2 : TPoint) : Double;
var H, W : LongInt;
begin
  H:= P1.Y - P2.Y;
  W:= P2.X - P1.X;
  if W <> 0 then
  begin
    Result:= ArcTan(H / W); {Angle in radians}
    if W < 0 then Result:= Result - PI;
  end
  else
  begin
    if H < 0 then Result:= -0.5 * PI else Result:= 0.5 * PI;
  end;
{  if Result < 0 then Result:= Result + 2 * PI;}
end;


Procedure Swap(var A, B : Double);
var C : Double;
begin
  C:= A;
  A:= B;
  B:= C;
end;

Procedure SwapW(var A, B : Double);
var C : single;
begin
  C:= A;
  A:= B;
  B:= C;
end;

Function RoundUp( R : Double) : LongInt;
begin
  Result:= Trunc(R);
  if R < 0 then Result:= Trunc(R) - 1
  else if R > 0 then Result:= Trunc(R) + 1;
end;

Function Order( R : Double) : SmallInt;
var Lg : Double;
begin
  if R = 0 then Result:= 0
  else
  begin
    Lg:= Log10(Abs(R));
    if Abs(R) >= 1 then
    begin
      Result:= Trunc(Log10(Abs(R)));
      {if Frac(Lg) = 0 then Result:= Trunc(Lg)
      else Result:= Trunc(Lg) + 1;}
    end
    else Result:= Trunc(Log10(Abs(R)))-1;
  end;
end;

Function OrderHigh( R : Double) : SmallInt;
begin
  if Frac(Log10(R)) = 0 then Result:= Order(R)
  else Result:= Order(R) + 1;
end;

Function RoundOrderHigh( R : Double) : Double;
begin
  Result:= RoundUp(R / InvLog(Order(R))) * InvLog(Order(R));
end;

Function RoundOrder( R : Double) : Double;
begin
  Result:= Int(R / InvLog(Order(R))) * InvLog(Order(R));
end;

Function Faculty(X : LongInt) : LongInt;
var i : LongInt;
begin
  Result:= 0;
  if X >= 1 then Result:= 1;
  for i:= 1 to X do Result:= Result * i;
end;

Function Poisson(Value, Av : LongInt) : Double;
begin
  if (Av > 0) and (Value > 0) then
    Result:= Exp(-Av) * Power(Av, Value) / Faculty(Value)
  else Result:= 0;
end;

Function CumPoisson(Value, Av : LongInt) : Double;
var i : LongInt;
    AmSteps : LongInt;
begin
  if Value > 0 then AmSteps:= Value
  else AmSteps:= 1;
  Result:= 0;
  if (Av > 0) and (Value > 0) then
    for i:= 1 to AmSteps do Result:= Result + Poisson(i, Av)
  else Result:= 0;
end;

Function InvCumPoisson(Value, Max, Av : LongInt) : Double;
var i : LongInt;
    AmSteps : LongInt;
begin
  if Value > 0 then AmSteps:= Max - Value + 1
  else AmSteps:= 1;
  Result:= 0;
  if (Av > 0) and (Value > 0) then
    for i:= 1 to AmSteps do Result:= Result + Poisson(i, Av)
  else Result:= 0;
end;

Function NormDist(X, Mean, StDev : Double) : Double;
var Sigma, Av : Double;
begin
  Result:= EXP( -(X - Mean) * (X - Mean) / (2 * StDev * StDev) )
           / (SQRT(2 * PI) * StDev);
end;

Function CumNormDist(X, Mean, StDev : Double) : Double;
var Min, Step : Double;
    i, MaxStep : LongInt;
begin
  Result:= 0;
  MaxStep:= 1000;
  Min:= Mean - 6 * StDev;
  Step:= (X - Min) / MaxStep;
  Result:= 0.5 * NormDist(Min, Mean, StDev) * 2 * Step;
  for i:= 1 to MaxStep do
    Result:= Result + NormDist((i - 0.5) * Step + Min, Mean, StDev) * Step;
end;

Function InvCumNormDist(Kans, Mean, StDev : double) : double;
var Cumm, Min, Step : Double;
    MaxStep : LongInt;
begin
  Result:= 0;
  if Kans = 0 then Result:= -5.0E324;
  if Kans = 1 then Result:= 1.7E308;
  MaxStep:= 1000;
  Min:= Mean - 10 * StDev;
  Step:= (Mean + 10 * StDev - Min) / MaxStep;
  Result:= Min;
  Cumm:= 0.5 * NormDist(Min - Step, Mean, StDev) * 2 * Step;
  Result:= Result - 0.5 * Step;
  while Cumm < Kans do
  begin
    Result:= Result + Step;
    Cumm:= Cumm + NormDist(Result, Mean, StDev) * Step;
  end;
end;

Function OutsideRange(Mean, StDev, Min, Max : Double; Left : boolean) : Double;
begin
  if Left then
  begin
    Result:= CumNormDist(Min, Mean, StDev);
  end
  else
  begin
    Result:= 1 - CumNormDist(Max, Mean, StDev);
  end;
end;

Function CalcAge(BirthDate : TDateTime) : LongInt;
var
  PresentDay: TDateTime;
  Y, M, D : Word;
  Y2, M2, D2 : word;
  i, Age : LongInt;
begin
  Y:= 0; M:= 0; D:= 0; Y2:= 0; M2:= 0; D2:= 0;
  PresentDay:= Date;
  DecodeDate(PresentDay, Y, M, D);
  DecodeDate(BirthDate, Y2, M2, D2);
  Result:= 0;
  if M > M2 then Result:= Y - Y2
  else if (M = M2) and (D >= D2) then Result:= Y - Y2
  else if (M = M2) and (D < D2) then Result:= Y - Y2 - 1
  else if M < M2 then Result:= Y - Y2 - 1;
end;


Procedure CalcPercICG(A1, A2, A3, A4, A5: integer; var P1, P2, P3, P4, P5 : integer);
var Tot : integer;
begin
  P1:= 0; P2:= 0; P3:= 0; P4:= 0; P5:= 0;
  Tot:= A1 + A2 + A3 + A4 + A5;
  if Tot > 0 then
  begin
    P1:= round(100 * A1 / Tot);
    P2:= round(100 * A2 / Tot);
    P4:= round(100 * A4 / Tot);
    P5:= round(100 * A5 / Tot);
    P3:= 100 - (P1 + P2 + P4 + P5);
    if P3 < 0 then
    begin
      P3:= 0;
      if P2 > P4 then P2:= P2 - 1 else P4:= P4 - 1;
    end;
  end;
end;

Function RoundDownDec(V : double; D : integer) : double;
var Va, Mult : double;
begin
  Mult:= Power(10, D);
  Va:= V * Mult;
  Va:= Int(Va);
  Va:= Va / Mult;
end;

Function RoundDec(V : double; D : integer) : double;
var Va, Mult : double;
begin
  Mult:= Power(10, D);
  Va:= V * Mult;
  Va:= Round(Va);
  Va:= Va / Mult;
end;

Function Deler(Min, Max, Interval : LongInt) : LongInt;
var i, Diff, M1, M2 : LongInt;
begin
  Result:= 0; M1:= 0;
  if Max > Min then
  begin
    Diff:= Max - Min;
    for i:= 1 to Diff do
    begin
      if (Diff mod i) = 0 then
      begin
        M2:= i;
        if (M1 < Interval) and (M2 >= Interval) then
        begin
          if (Interval - M1) < (M2 - Interval) then Result:= M1
          else Result:= M2;
          Exit;
        end;
        M1:= M2;
      end;
    end;
  end;
end;

Procedure CalcPerc(Aantallen : array of double; var Perc : array of double);
var i, j, k, l, Elm : integer;
    Tot, Rest : double;
    Prc, Rst : array of double;
    Rangen : array of integer;
begin
  {Rond eerst alle percentages naar beneden af en verdeel de rest naar gelang het
   hoogste getal achter de komma}
  Elm:= High(Aantallen);

  SetLength(Prc, Elm+1); SetLength(Rst, Elm+1); SetLength(Rangen, Elm+1);

  Tot:= 0;
  for i:= 0 to Elm do Tot:= Tot + Aantallen[i];
  if Tot > 0 then
  begin
    for i:= 0 to Elm do Prc[i]:= 100 * Aantallen[i] / Tot;
    for i:= 0 to Elm do Perc[i]:= Trunc(Prc[i]);
    Rest:= 0;
    for i:= 0 to Elm do
    begin
      Rst[i]:= Prc[i] - Perc[i];
      Rest:= Rest + Rst[i];
    end;
    Rest:= round(Rest);

    while Rest > 0 do
    begin
      for i:= 0 to Elm do
        Rangen[i]:= Rang(Rst[i], Rst, false);
      {Kijk of er meer elementen zijn met rangnummer 1}
      j:= 0;
      for i:= 0 to Elm do if Rangen[i] = 1 then Inc(j) else Rangen[i]:= 0;
      if j > 1 then
      begin
        k:= -1;
        while (Rest > 0) and (k < round(Elm/2)) do
        begin
          Inc(k);
          l:= round(Elm/2) - k;
          if (l >= 0) and (Rest > 0) then
          begin
            if (Rangen[l] = 1) then
            begin
              Rest:= Rest - 1;
              Rst[l]:= 0;
              Perc[l]:= Perc[l] + 1;
            end;
          end;
          l:= round(Elm/2) + k;
          if (l < Elm) and (k > 0) and (Rest > 0) then
          begin
            if (Rangen[l] = 1) then
            begin
              Rest:= Rest - 1;
              Rst[l]:= 0;
              Perc[l]:= Perc[l] + 1;
            end;
          end;
        end;
      end
      else if j = 1 then
      begin
        j:= 0;
        while Rangen[j] <> 1 do
          Inc(j);
        Rest:= Rest - 1;
        Rst[j]:= 0;
        Perc[j]:= Perc[j] + 1;
      end;
    end;
  end;
  Prc:= NIL; Rst:= NIL; Rangen:= NIL;
end;


Function Rang(Getal : double; Arr : array of double; Oplopend : boolean) : integer;
var A : array of double;
    i : LongInt;
begin
  Result:= 1;
  SetLength(A, High(Arr)+1);
  for i:= 0 to High(Arr) do A[i]:= Arr[i];
  QuickSortArray(A);
  if Oplopend then
  begin
    for i:= 0 to High(A) do
    begin
      if A[i] < Getal then Inc(Result);
    end;
  end
  else
  begin
    for i:= High(A) downto 0 do
    begin
      if A[i] > Getal then Inc(Result);
    end;
  end;
  A:= NIL;
end;

Procedure QuickSortArray(var Arr : array of double);
  procedure QuickSort(var A: array of double; iLo, iHi: Integer);
  var Lo, Hi: Integer;
      Mid, T : double;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := A[(Lo + Hi) div 2];
    repeat
      while A[Lo] < Mid do Inc(Lo);
      while A[Hi] > Mid do Dec(Hi);
      if Lo <= Hi then
      begin
        T := A[Lo];
        A[Lo] := A[Hi];
        A[Hi] := T;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then QuickSort(A, iLo, Hi);
    if Lo < iHi then QuickSort(A, Lo, iHi);
  end;
begin
  QuickSort(Arr, Low(Arr), High(Arr));
end;


{==============================================================================}
                                   {Physics}

Function CelciusToKelvin( C : Double) : Double;
begin
  Result:= C + 273.15;
end;

Function KelvinToCelcius( K : Double) : Double;
begin
  Result:= K - 273.15;
end;

Function CelciusToCal( C : Double) : Double;
begin
  Result:= 1000 * CelciusToKelvin(C);
end;

Function CalToCelcius( C : Double) : Double;
begin
  Result:=  KelvinToCelcius(C / 1000);
end;

Function KelvinToCal( K : Double) : Double;
begin
  Result:= K * 1000;
end;

Function CalToKelvin( C : Double) : Double;
begin
  Result:= C / 1000;
end;

Function WaterDensity( T : Double) : Double;
begin
  Result:= (999.83952 + 16.945176 * T - 7.9870401E-3 * T*T - 46.170461E-6 * T*T*T
            + 105.56302E-9 * T*T*T*T - 280.54253E-12 *T*T*T*T*T)
           / (1 + 16.879850E-3 * T);
end;

end.
