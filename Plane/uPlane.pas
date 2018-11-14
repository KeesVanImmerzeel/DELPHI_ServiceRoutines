unit uPlane;

interface

uses
  Math;

Type

T2dPoint =
  Record
    X, Y: Double;
  end;

T2dTriangle =
  record
    P1 : T2dPoint;
    P2 : T2dPoint;
    P3 : T2dPoint;
  end;

T3dVector =
  Record
    X, Y, Z: Double;
  end;

T3dPoint = T3dVector;

TPlaneCoefficients =
  Record
    a, b, c, d: Double;
  end;

Function PointIn2dTriangle(P : T2dPoint; Triangle : T2dTriangle) : Boolean;
Function CoefficientsOfPlane( P, Q, R: T3dPoint ): TPlaneCoefficients;
Function GetValueAt( Location: T2dPoint; PlaneCoefficients: TPlaneCoefficients ): Double;

implementation

Function PointIn2dTriangle(P : T2dPoint; Triangle : T2dTriangle) : Boolean;
var
  S : Integer;
begin
  S := Sign((Triangle.P1.X - P.X) * (Triangle.P2.Y - P.Y) - (Triangle.P2.X -
       P.X) * (Triangle.P1.Y - P.Y)) +
       Sign((Triangle.P2.X - P.X) * (Triangle.P3.Y - P.Y) - (Triangle.P3.X -
       P.X) * (Triangle.P2.Y - P.Y)) +
       Sign((Triangle.P3.X - P.X) * (Triangle.P1.Y - P.Y) - (Triangle.P1.X -
       P.X) * (Triangle.P3.Y - P.Y));
       Result := (S = 3) or (S = -3);
end;

function CrossProduct3dVector( a, b: T3dVector ): T3dVector;
begin
  Result.X := a.Y*b.Z - a.Z*b.Y;
  Result.Y := a.Z*b.X - a.X*b.Z;
  Result.Z := a.X*b.Y - a.Y*b.X;
end;

function Substract3dVectors( a, b: T3dVector ): T3dVector;
begin
  Result.X := b.X - a.X;
  Result.Y := b.Y - a.Y;
  Result.Z := b.Z - a.Z;
end;

// Bereken vector loodrecht op vlak door de punten P, Q en R
Function Perpendicular3dVector( P, Q, R: T3dPoint ): T3dVector;
var
  PQ, PR: T3dVector;
begin
  //Bepaal 2 vectoren die in het vlak liggen
  PQ := Substract3dVectors( P, Q );
  PR := Substract3dVectors( P, R );
  Result :=  CrossProduct3dVector( PQ, PR );
end;

// Bereken de coefficienten van het vlak ax + by + cz = d dat door de punten
// P, Q en R gaat
Function CoefficientsOfPlane( P, Q, R: T3dPoint ): TPlaneCoefficients;
var
  PerpVector: T3dVector;
begin
  PerpVector := Perpendicular3dVector( P, Q, R );
  Result.a := PerpVector.X;
  Result.b := PerpVector.Y;
  Result.c := PerpVector.Z;
  Result.d := Result.a * P.X + Result.b * P.Y + Result.c * P.Z;
end;

Function GetValueAt( Location: T2dPoint; PlaneCoefficients: TPlaneCoefficients ): Double;
begin
  with Location, PlaneCoefficients do
    Result := ( d - ( a*X + b*Y ) ) / c;
end;

end.
