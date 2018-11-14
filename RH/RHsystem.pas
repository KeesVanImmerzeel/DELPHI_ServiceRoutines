unit RHsystem;

interface
                                                                     
uses Windows, Classes, Vcl.Controls, Vcl.Grids, Vcl.Forms, SysUtils, Vcl.Dialogs, ShellApi,
  dUtils, System.UITypes;

Const
  Inch = 2.541;
  MaxReal: single = 1.7E38;
  MinReal: single = -1E36;
  MaxDouble : Double = 1.7E308;
  MinDouble : Double = 5.0E-324;
  MaxWord : longint = 65535;
  MinWord : longint = 0;
  MaxLongInt : LongInt = 2147483647;
  MinLongInt : LongInt = -2147483647;
  BlockSizeResult = 1;
  BlockSizeRestart = 1;

{Conversions}
Function DecimalsSc(S : Double) : LongInt;
Function GetOrdinals(S : extended) : LongInt;
Function GetDecimals(S : Double) : LongInt;
Function FormatString(S : extended; D : LongInt) : String;
Function StringFormat(S, X : Double; D : SmallInt) : string;
Function StrToReal(S : string) : Double;
Function RealToStr(S : Double) : string;
Function RealToStrSci(S : Double; Prec : LongInt) : string;
Function RealToStrDEC(S : Double; D : SmallInt) : string;
Function RealToStrSignif(S : Double; Signif : SmallInt) : string;
Function IsValidDate(S : string; var Da : TDateTime) : boolean;
Function GetAmountDays(Month, Year : LongInt) : LongInt;
Function Sinus(Value : double) : double;
Function Cosinus(Value : double) : double;
Function Tangens(Value : double) : double;

{String functions}
Function GetExtension(FileName : string) : string;
Function GetListSeparator : Char;
Function ReplaceStringPart(S, SearchString, ReplaceString : string) : string;
Function FindSubString(Line : string; Substrings : array of string; var Pos : Longint) : LongInt;
Function TranslateToOEM(S : string) : string;

Function GetCSVWord2(Line : string; Sep : Char; var Pos : LongInt) : string;
Function RemoveCharacter(Line : string; Ch : char) : string;
Function RemoveSigns(Value : string) : string;
Function RemovePath( FileName : string) : string;
Function GetPath(FileName : string; IncludeSlash : boolean) : string;
Function HasPath(FileName : string) : boolean;
Function ChangeExtension( FileName, Extension : string) : string;
Function RemoveExtension( FileName : string; WithPoint : boolean) : string;
Function ConcatenateFileName( FileName, Txt : string) : string;
Function AbsoluteToRelativePath(FileName, RelativeToPath : string) : string;
Function RemoveLastChar(Txt : string) : string;

Function MakeDateStr(Value : TDateTime) : string;
Function MakeInitialString(Value : string) : string;
Function NrColToChar(I : LongInt) : string;
Function CharToNrCol(S : string) : LongInt;

{I/O functions}
Function GetCurrentPath : string;
Function GetWindowsPath : string;
Function WindowsTempPath : string;
Procedure WriteCSV(var F : Text; S : string);
Procedure WriteLnCSV(var F : Text; S : string);
Procedure WriteTxt(var F : Text; S, Sep : string);
Procedure WriteLnTxt(var F : Text; S : string);
Procedure WriteLineCSV(var F : text; S : array of string);
Procedure ImportCSV(StrGr : TStringGrid; FN : string);
Procedure ExportCSV(StrGr : TStringGrid; FN, Comment : string);
Function GetCSVReal(Line : string; NrEntry : Longint) : double;
Function GetReal(Line : string; NrEntry : Longint) : double;
Function GetCSVEntry(Line : string; NrEntry : LongInt) : string;
Function GetEntry(Line : string; NrEntry : LongInt) : string;
Function GetCSVWord(S : string; var Pos : LongInt) : string;
Function GetWord( Line : string; var Pos : LongInt) : string;

Function GetINIcaption(Line : string) : string;
Function GetINIvalue(Line : string) : string;
Function GetINIint(Line : string) : LongInt;
Function GetINIreal(Line : string) : double;
Function Store(var F : File; var Wr; Size : longint) : longint;
Function Get(var F : File; var Re; Size : longint) : longint;

Procedure ExtrFileName(Value : string; var Path, FN, Ext : string);
Function CreateBackup(FileName : string) : boolean;
Function RestoreFromBackup(FileName : string) : boolean;
procedure CopyFile(const FileName, DestName: string);

{Other functions}
Procedure QuickSortArray(var Arr : array of double);

{Form functions}
Procedure SetCursor(Form : TForm; Crsor : TCursor);

{Error functions}
Function ShowLastError(Comment : string) : boolean;
Function GetLastErrorString : string;

{Shell API functions}
Procedure StartApplication(Name, LoadFile, Params : string; Show : boolean;
                           var PI : TProcessInformation);
Procedure StartAppl(Name, LoadFile : string);
Procedure Send_e_mail(sTo, sCC, sBCC, Subject : string);
Procedure OpenWebPage(Address : string);
Procedure OpenFile(FileName : string);

{Date and Time functions}
function Fixed_DateTimeToFileDate(DateTime: TDateTime): Integer;

implementation

uses Registry, Math, RHmath, RHdialogs;
                                   {Conversions}

Function Sinus(Value : double) : double;
begin
  Result:= Sin(2 * PI * Value/360);
end;

Function Cosinus(Value : double) : double;
begin
  Result:= Cos(2 * PI * Value/360);
end;

Function Tangens(Value : double) : double;
begin
  Result:= Sinus(Value) / Cosinus(Value);
end;

Function DecimalsSc( S: Double): LongInt;
begin
  if Abs(S) < 1 then Result:= Abs(Order(S))
  else Result:= 0;
end;

Function GetOrdinals( S : Extended) : LongInt;
var AbsS : Extended;
begin
  AbsS:= Abs(S);
  if AbsS < 1 then result:= 1  {0,..}
  else Result:= Trunc(Log10(AbsS)) + 1;
  if S < 0 then Inc(Result); {Minus-sign}
end;

Function GetDecimals( S : Double) : LongInt;
begin
  if (ABS(S) < 100) and (S <> 0) then
  begin
    if S >= 1 then Result:= 1 - Trunc( log10(ABS(S)) )
    else Result:= Abs(Order(S)) + 2;
  end
  else Result:= 0;
end;

Function FormatString(S : extended; D : LongInt) : String;
var O : LongInt;
begin
  O:= GetOrdinals(S);
  if D > 0 then O:= O + 1 + D;
  Result:= '%' + IntToStr(O) + '.' + IntToStr(D) + 'f';
  FmtStr(Result, Result, [S]);
end;

Function StringFormat( S, X : Double;  D : SmallInt) : string;
var Total, Whole, dec : LongInt;
begin
  if (ABS(S) < 100) and (S <> 0) and (D < 0) then
  begin
    if S >= 1 then Dec:= 1 - Trunc( log10(ABS(S)) )
    else Dec:= Abs(Order(S)) + 2;
  end
  else if (ABS(S) < 100) and (S <> 0) and (D >= 0) then
    Dec:= D
  else Dec:= 0;

  if X > 1 then Whole:= Trunc(log10(abs(X))) + 1
  else Whole:= 1;

  if Dec > 0 then Total:= Whole + Dec + 1
  else Total:= Whole;

  if X < 0 then Inc(Total);

  Result:= '%' + IntToStr(Total) + '.' + IntToStr(Dec) + 'f';
end;

Function RealToStr( S : Double) : string;
var StrF : string;
    Total, D : LongInt;
begin
  Total:= GetOrdinals(S);
  D:= GetDecimals(S);
  if D > 0 then Total:= Total + 1 + D;
  StrF:= '%' + IntToStr(Total) + '.' + IntToStr(D) + 'f';

  FmtStr(Result, StrF, [S]);
end;

Function RealToStrSci(S : Double; Prec : LongInt) : string;
var StrF, str : string;
    p : LongInt;
begin
  StrF:= '%.' + IntToStr(Prec) + 'e';
  FmtStr(Result, StrF, [S]);
  p:= pos('E', Result) + 1;
  str:= Copy(Result, p, Length(Result) - p + 1);
  result:= copy(result, 1, p - 1);
  result:= result + IntToStr(StrToInt(str));
end;

Function RealToStrDEC( S : Double;  D : SmallInt) : string;
var StrF : string;
    Total : LongInt;
begin
  Total:= GetOrdinals(S);
  if D > 0 then Total:= Total + 1 + D;
  StrF:= '%' + IntToStr(Total) + '.' + IntToStr(D) + 'f';

  FmtStr(Result, StrF, [S]);
end;

Function RealToStrSignif(S : Double; Signif : SmallInt) : string;
var O, D : LongInt;
begin
  O:= GetOrdinals(S);
  if O > Signif then Result:= RealToStrSci(S, Signif)
  else if GetDecimals(S) <= Signif then
  begin
    D:= Signif - O;
    Result:= RealToStrDec(S, D);
  end
  else Result:= RealToStrSci(S, Signif);
end;

Function StrToReal( S : string) : Double;
var Code : integer;
    SO : string;
    D : double;
begin
  Code:= 0;
  try
    S:= ReplaceStringPart(S, ',', '.');
    Val(S, D, Code);
    Result:= D;
  except
    SO:= Copy(S, Code, 1);
    Warning('Error in conversion from string to real (character ' + SO + ')');
    Result:= 0;
  end;
end;

Function IsValidDate(S : string; var Da : TDateTime) : boolean;
var Y, M, D, i, ii, Format : word;
    Parts : array[0..2] of String;
    Ch : char;
    LongYear : boolean;
begin
  Result:= TRUE;
  for i:= 0 to 2 do Parts[i]:= '';
  ii:= 0;

  with FormatSettings do begin {-Delphi XE6}

  if (UpperCase(ShortDateFormat[1]) = 'D') and
     (UpperCase(ShortDateFormat[Length(ShortDateFormat)]) = 'Y') then
    Format:= 0
  else if (UpperCase(ShortDateFormat[1]) = 'M') and
          (UpperCase(ShortDateFormat[Length(ShortDateFormat)]) = 'Y') then
    Format:= 1
  else if (UpperCase(ShortDateFormat[1]) = 'Y') and
          (UpperCase(ShortDateFormat[Length(ShortDateFormat)]) = 'D') then
    Format:= 2;

  i:= Pos('Y', Uppercase(ShortDateFormat));
  i:= i + 1;
  ii:= 1;
  while (UpperCase(ShortDateFormat[i]) = 'Y') and (i < Length(ShortDateFormat)) do
  begin
    Inc(ii); Inc(i);
  end;
  LongYear:= (ii > 2);
  ii:= 0;
  for i:= 1 to Length(S) do
  begin
    Ch:= S[i];
    if (Ch <> DateSeparator) and (ii <= 2) and (Ch >= '0') and (Ch <= '9') then
      Parts[ii]:= Parts[ii] + Ch
    else Inc(ii);
  end;
  if ii > 2 then Result:= false
  else
  begin
    Result:= (Parts[0] <> '') AND (Parts[1] <> '') and (Parts[2] <> '');
    if Result then
    begin
      if Format = 1 then
      begin
        try
          M:= StrToInt(Parts[0]);
          D:= StrToInt(Parts[1]);
          Y:= StrToInt(Parts[2]);
        except
          Result:= false;
        end;
      end
      else if Format = 0 then
      begin
        try
          M:= StrToInt(Parts[1]);
          D:= StrToInt(Parts[0]);
          Y:= StrToInt(Parts[2]);
        except
          Result:= false;
        end;
      end
      else if Format = 2 then
      begin
        try
          M:= StrToInt(Parts[1]);
          D:= StrToInt(Parts[2]);
          Y:= StrToInt(Parts[0]);
        except
          Result:= false;
        end;
      end
      else Result:= false;
      Result:= ((LongYear) AND (Y >= 1900)) or ((not LongYear));
      if Result then
        Result:= (D >= 1) AND (D <= 31) AND (M >= 1) AND (M <= 12);
      if Result then
        Result:= (D <= GetAmountDays(M, Y)) and (D >= 1);
      if Result then Da:= EncodeDate(Y, M, D);
      if Result then Result:= Da <= Date;
    end;
  end;

  end; {-Delphi XE6}
end;

Function GetAmountDays(Month, Year : LongInt) : LongInt;
begin
  Result:= 0;
  case Month of
  1, 3, 5, 7, 8, 10, 12 : Result:= 31;
  4, 6, 9, 11 : Result:= 30;
  2: if IsLeapYear(Year) then Result:= 29 else Result:= 28;
  end;
end;
{==============================================================================}
                                   {String functions}

Function GetExtension( FileName : string) : string;
begin
  Result:= ExtractFileExt(FileName);
end;

Function GetListSeparator : Char;
begin
  Result:= ',';
  with FormatSettings do begin {-Delphi XE6}
  if (DecimalSeparator = ',') or (ThousandSeparator = ',') then Result:= ';';
  end;
end;

{Function GetCSVWord( Line : string; var Pos : LongInt) : string;
var Ch : Char;
    counter, cnter2, count3 : integer;
    Res : string;
begin
  ListSeparator:= GetListSeparator;
  Result:= ''; Ch:= ' ';
  while (Pos <= Length(Line)) and (Ch <> ListSeparator) do
  begin
    Ch:= Line[Pos];
    if (Ch <> ListSeparator) and (Ch <> '"') then Result:= Result + Ch;
    Inc(Pos);
  end;
  if Length(Result) > 0 then
  begin
  {Remove beginblanks and endblanks:}
{    counter:= 1; Res:= '';
    while Result[counter] = ' ' do inc(counter);
    cnter2:= Length(Result);
    while Result[cnter2] = ' ' do
      inc(cnter2, -1);
    for count3:= counter to cnter2 do
      Res:= Res + Result[count3];
    Result:= Res;
  end;
end;}

Function GetCSVWord2(Line : string; Sep : Char; var Pos : LongInt) : string;
var Ch : Char;
    counter, cnter2, count3 : integer;
    Res : string;
begin
  Result:= ''; Ch:= Line[1];
  while (Pos <= Length(Line)) and (Ch <> Sep) do
  begin
    Ch:= Line[Pos];
    if (Ch <> Sep) and (Ch <> '"') then Result:= Result + Ch;
    Inc(Pos);
  end;
  if Length(Result) > 0 then
  begin
  {Remove beginblanks and endblanks:}
    counter:= 1; Res:= '';
    while Result[counter] = ' ' do inc(counter);
    cnter2:= Length(Result);
    while Result[cnter2] = ' ' do
      inc(cnter2, -1);
    for count3:= counter to cnter2 do
      Res:= Res + Result[count3];
    Result:= Res;
  end;
end;

Function FindSubString(Line : string; Substrings : array of string; var Pos : Longint) : LongInt;
var L, i : LongInt;
    Fnd : boolean;
begin
  Result:= 0;  Fnd:= false;
  while not Fnd do
  begin
    for i:= 0 to High(SubStrings) do
    begin
      L:= Length(SubStrings[i]);
      if (UpperCase(Copy(Line, Pos, L)) = Uppercase(SubStrings[i])) then
      begin
        Fnd:= TRUE;
        if (Pos > 1) AND (Line[Pos - 1] <> ' ') then Fnd:= false; //There must be a space before
        if (Pos > Length(Line) - 1) AND (Line[Pos + 1] <> ' ') then Fnd:= false; // there must be a space after
      end;
    end;
    if Pos = Length(Line) - L then Fnd:= TRUE;
    if not Fnd then Inc(pos);
  end;
  Pos:= Pos + L;
  if Pos > Length(Line) then Pos:= Length(Line);
  Result:= Pos;
end;

Function TranslateToOEM(S : string) : string;
var P1, P2 : PChar;
    i : LongInt;
begin
  Result:= S;
  i:= GetOEMCP;
  if (S <> '') and ((i = 437) or (i = 850)) then
  begin
    P1:= PChar(S);
    if CharToOEM( P1, PAnsiChar(P1)) then
      Result:= StrPas(P1);
  end;
end;

Function RemoveCharacter( Line : string;  Ch : char) : string;
var pos : integer;
begin
  Result:= ''; Pos:= 1;
  while Pos <= length(Line) do
  begin
    if Line[Pos] <> Ch then Result:= Result + Line[Pos];
    inc(Pos);
  end;
end;

Function RemoveSigns(Value : string) : string;
var pos : integer;
    O : integer;
begin
  Result:= ''; Pos:= 1;
  while Pos <= length(Value) do
  begin
    O:= Ord(Value[Pos]);
    if (O >= Ord(' ')) and (O <= Ord('ÿ')) then Result:= Result + Value[Pos];
    inc(Pos);
  end;
end;

Function ReplaceStringPart(S, SearchString, ReplaceString : string) : string;
var i : LongInt;
begin
   Result:= S;
   while Pos(SearchString, Result) > 0 do
   begin
     i:= Pos(SearchString, Result);
     if i > 0 then
     begin
       Delete(Result, i, Length(SearchString));
       Insert(ReplaceString, Result, i);
     end;
   end;
end;

Function RemovePath( FileName : string) : string;
begin
  Result:= ExtractFileName(FileName);
end;

Function GetPath( FileName : string; IncludeSlash : boolean) : string;
begin
  Result:= ExtractFilePath(FileName);
  if not IncludeSlash then Result:= Copy(Result, 1, Length(Result)-1);
end;

Function HasPath( FileName : string) : boolean;
const Slash : char = '\';
begin
  if Pos(Slash, FileName) > 0 then Result:= TRUE
  else Result:= FALSE;
end;

Function ChangeExtension( FileName, Extension : string) : string;
var WherePoint, Position : integer;
    Ch : string[1];
begin
  {Look for the last . in the FileName. This is the position where the extension starts}
  WherePoint:= 0;
  for Position:= 1 to Length(FileName) do
  begin
    Ch:= Copy(FileName, Position, 1);
    if Ch = '.' then WherePoint:= Position;
  end;
  {If there is een extension, remove it. If not, place a point behind the name}
  if WherePoint > 0 then
    Result:= Copy(FileName, 1, WherePoint)
  else
    Result:= FileName + '.';
 {Place the new extension}
  Result:= Result + Extension;
end;

Function RemoveExtension( FileName : string; WithPoint : boolean) : string;
var WherePoint, Position : integer;
    Ch : string[1];
begin
  {Look for the last . in the FileName. This is the position where the extension starts}
  WherePoint:= 0;
  for Position:= 1 to Length(FileName) do
  begin
    Ch:= Copy(FileName, Position, 1);
    if Ch = '.' then WherePoint:= Position;
  end;
  {If there is een extension, remove it. If not, place a point behind the name}
  if WherePoint > 0 then
  begin
    if WithPoint then Result:= Copy(FileName, 1, WherePoint)
    else Result:= Copy(FileName, 1, WherePoint - 1);
  end
  else
  begin
    if WithPoint then Result:= FileName + '.'
    else Result:= FileName;
  end;
end;

Function ConcatenateFileName( FileName, Txt : string) : string;
var Ext : string;
begin
  Ext:= GetExtension(FileName);
  if Ext <> '' then
    Result:= RemoveExtension(FileName, false) + Txt + '.' + Ext
  else
    Result:= RemoveExtension(FileName, false) + Txt;
end;

Function CountSlashes(Path : string) : LongInt;
var i : LongInt;
begin
  Result:= 0;
  for i:= 1 to Length(Path) do
    if Path[i] = '\' then Inc(Result);
end;

Procedure DisectPath(Path : string; var Dirs : array of string);
var i, n : LongInt;
    Part : string;
begin
  n:= 0;
  for i:= 1 to Length(Path) do
  begin
    if (Path[i] = '\') or (i = Length(Path)) then
    begin
      if Path[i] <> '\' then Part:= Part + Uppercase(Path[i]); //checken, voor als laatste teken geen '/' is
      Dirs[n]:= Part;  //eerste deel is de schijf
      Inc(n);
      Part:= '';
    end
    else
    begin
      Part:= Part + Uppercase(Path[i]);
    end;
  end;
end;

Function AbsoluteToRelativePath(FileName, RelativeToPath : string) : string;
var AbsPath, FileNm, AbsPath2 : string;
    Dirs1, Dirs2 : array of string;
    i, n, n1, n2, same : LongInt;
begin
  Result:= '';
  AbsPath:= GetPath(FileName, TRUE);
  FileNm:= RemovePath(FileName);
  AbsPath2:= GetPath(RelativeToPath, TRUE);
  n1:= CountSlashes(AbsPath);
  n2:= CountSlashes(AbsPath2);
  n:= Max(n1, n2);
  SetLength(Dirs1, n);
  SetLength(Dirs2, n);
  DisectPath(AbsPath, Dirs1);
  DisectPath(AbsPath2, Dirs2);

  if Dirs1[0] <> Dirs2[0] then //schijf is niet hetzelfde, geen relatief pad mogelijk
    Result:= FileName
  else
  begin
    same:= 1;
    //vind het laagste overeenkomende niveau
    i:= 2;
    while (Dirs1[i-1] = Dirs2[i-1]) and (i <= n) do
    begin
      same:= i;
      Inc(i);
    end;
{    for i:= 2 to n do
      if (Dirs1[i-1] = Dirs2[i-1]) and (i = same + 1) then
        same:= i;}

    for i:= same + 1 to n do
      if Dirs2[i-1] <> '' then
        Result:= Result + '..\';

    for i:= same + 1 to n do
      if Dirs1[i-1] <> '' then
        Result:= Result + Dirs1[i-1] + '\';

    Result:= Result + FileNm;    
  end;

  Dirs1:= NIL;
  Dirs2:= NIL;
end;

Function RemoveLastChar(Txt : string) : string;
var i : LongInt;
begin
  Result:= '';
  for i:= 1 to Length(Txt) - 1 do
    Result:= Result + Txt[i];
end;

Function NrColToChar(I : LongInt) : string;
var C1, C2 : Char;
    D, M : LongInt;
begin
  if (I > 0) and (I <= 26*26) then
  begin
    D:= I div 26;
    M:= I mod 26;
    if D = 0 then
    begin
      C2:= Chr(64+M);
      Result:= C2;
    end
    else if M = 0 then
    begin
      C2:= 'Z';
      if D = 1 then
      begin
        C1:= ' ';
        Result:= C2;
      end
      else
      begin
        C1:= Chr(63+D);
        Result:= C1+C2;
      end;
    end
    else
    begin
      C1:= Chr(64+D);
      C2:= Chr(64+M);
      Result:= C1+C2;
    end;
  end
  else Result:= '';
end;

Function CharToNr(Ch : Char) : LongInt;
begin
  case Ch of
  'a', 'A': Result:= 1;
  'b', 'B': Result:= 2;
  'c', 'C': Result:= 3;
  'd', 'D': Result:= 4;
  'e', 'E': Result:= 5;
  'f', 'F': Result:= 6;
  'g', 'G': Result:= 7;
  'h', 'H': Result:= 8;
  'i', 'I': Result:= 9;
  'j', 'J': Result:= 10;
  'k', 'K': Result:= 11;
  'l', 'L': Result:= 12;
  'm', 'M': Result:= 13;
  'n', 'N': Result:= 14;
  'o', 'O': Result:= 15;
  'p', 'P': Result:= 16;
  'q', 'Q': Result:= 17;
  'r', 'R': Result:= 18;
  's', 'S': Result:= 19;
  't', 'T': Result:= 20;
  'u', 'U': Result:= 21;
  'v', 'V': Result:= 22;
  'w', 'W': Result:= 23;
  'x', 'X': Result:= 24;
  'y', 'Y': Result:= 25;
  'z', 'Z': Result:= 26;
  end;
end;

Function CharToNrCol(S : string) : LongInt;
var C1, C2 : Char;
    A, B : LongInt;
begin
  C1:= ' '; C2:= ' ';
  if Length(S) = 1 then C1:= S[1]
  else begin C1:= S[1]; C2:= S[2]; end;
  if C2 = ' ' then
    Result:= CharToNr(C1)
  else
  begin
    A:= CharToNr(C1);
    B:= CharToNr(C2);
    Result:= A * 26 + B;
  end;
end;

Function MakeDateStr(Value : TDateTime) : string;
var Da2 : TDateTime;
    M, D, Y : word;
begin
  Da2:= EncodeDate(2000, 1, 1);
  DecodeDate(Value, Y, M, D);
  if Length(IntToStr(Y)) = 2 then
  begin
    if Value >= Da2 then Result:= IntToStr(20) + IntToStr(Y)
    else Result:= IntToStr(19) + IntToStr(Y);
  end
  else Result:= IntToStr(Y);

  if Length(IntToStr(M)) = 1 then Result:= Result + '0' + IntToStr(M)
  else Result:= Result + IntToStr(M);
  if Length(IntToStr(D)) = 1 then Result:= Result + '0' + IntToStr(D)
  else Result:= Result + IntToStr(D);
end;

Function MakeInitialString(Value : string) : string;
var i : integer;
begin
  Result:= '';
  for i:= 1 to Length(Value) do
  begin
    Result:= Result + Value[i];
    if (i < Length(Value))
    and (Ord(Value[i]) >= Ord('A')) and (Ord(Value[i]) <= Ord('Z'))
    and (Ord(Value[i+1]) >= Ord('A')) and (Ord(Value[i+1]) <= Ord('Z')) then
      Result:= Result + '.'
    else if i = Length(Value) then Result:= Result + '.';
  end;
end;

{==============================================================================}
                                   {I/O functions}

Function WindowsTempPath : string;
var  TempPath: array[0..MAX_PATH] of char;
begin
  GetTempPath(MAX_PATH,TempPath);
  Result:= StrPas(TempPath);
end;

Function GetCurrentPath : string;
var k: integer;
    buffer: array[1..256] of char;
    pbuf: PChar;
begin
  pbuf:=@buffer;
  GetCurrentDirectory(sizeof(buffer), pbuf);
  Result:= '';
  k:=1;
  while buffer[k]<>#0 do
  begin
    Result:= Result + buffer[k];
    k:= k+1;
  end;
end;

Function GetWindowsPath : string;
var k: integer;
    buffer: array[1..256] of char;
    pbuf: PChar;
begin
  pbuf:=@buffer;
  GetWindowsDirectory(pbuf, sizeof(buffer));
  Result:= '';
  k:=1;
  while buffer[k]<>#0 do
  begin
    Result:= Result + buffer[k];
    k:= k+1;
  end;
end;

Procedure WriteCSV(var F : Text; S : string);
begin
  with FormatSettings do begin {-Delphi XE6}
    Write(F, S + ListSeparator);
  end;
end;

Procedure WriteLnCSV(var F : Text; S : string);
begin
  WriteLn(F, S);
end;

Procedure WriteTxt(var F : Text; S, Sep : string);
begin
  Write(F, S + Sep);
end;

Procedure WriteLnTxt(var F : Text; S : string);
begin
  WriteLn(F, S);
end;

Procedure WriteLineCSV(var F : text; S : array of string);
var i : LongInt;
begin
  with FormatSettings do begin {-Delphi XE6}
    for i:= Low(S) to High(S) - 1 do Write(F, S[i] + ListSeparator);
    writeLn(F, S[High(S)]);
  end;
end;

Procedure ImportCSV( StrGr : TStringGrid;  FN : string);
var F : TextFile;
    X, Y, Pos : LongInt;
    Line : string;
begin
  {Open CZV-File}
  AssignFile(F, FN);
  Reset(F);
  {Read two lines of comment}
  ReadLn(F, Line);
  ReadLn(F, Line);
  Y:= 0;
  with StrGr do
    while not EOF(F) do
    begin
      Inc(Y);
      if RowCount - 1 < Y then RowCount:= RowCount + 1;
      ReadLn(F, Line);
      Pos:= 1;
      for X:= 1 to ColCount - 1 do
        Cells[X, Y]:= GetCSVWord(Line, Pos);
    end;
  CloseFile(F);
end;

Procedure ExportCSV( StrGr : TStringGrid;  FN, comment : string);
var F : TextFile;
    X, Y : longint;
begin
  {Open CZV-File}
  AssignFile(F, FN);
  Rewrite(F);
  {Write two lines of comment}
  WriteLnCSV(F, 'Exportfile OOMASWIN 3.0');
  WriteLnCSV(F, Comment);
  with StrGr do
    for Y:= 1 to RowCount - 1 do
    begin
      for X:= 1 to ColCount - 2 do
        WriteCSV(F, Cells[X, Y]);
      WriteLnCSV(F, Cells[ColCount - 1, Y]);
    end;
  CloseFile(F);
end;

Function GetWord( Line : string; var Pos : LongInt) : string;
var Ch : Char;
    counter, cnter2, count3 : integer;
    Res : string;
begin
  Result:= '';
  {Remove initial spaces}
  while Line[Pos] = ' ' do inc(Pos);
  while (Pos <= Length(Line)) and (Line[Pos] <> ' ') do
  begin
    Ch:= Line[Pos];
    if (Ch <> ' ') then Result:= Result + Ch;
    Inc(Pos);
  end;
  if Length(Result) > 0 then
  {Remove beginblanks and endblanks:}
    Result:= Trim(Result);
end;

Function GetCSVWord(S : string; var Pos : LongInt) : string;
var Ch : char;
const TextMark = '"';
begin
  Ch:= ' '; Result:= '';
  while (Ch <> ';') and (Pos <= Length(S)) do
  begin
    Ch:= S[Pos];
    if (Ch <> ';') and (Ch <> TextMark) then
      Result:= Result + Ch;
    Inc(Pos);
  end;
  Result:= TRIM(Result);
end;

Function GetEntry(Line : string; NrEntry : LongInt) : string;
var i, pos : LongInt;
begin
  Result:= ''; pos:= 1;
  for i:= 1 to NrEntry do
    Result:= GetWord(Line, Pos);
end;

Function GetCSVEntry(Line : string; NrEntry : LongInt) : string;
var i, pos : LongInt;
begin
  Result:= ''; pos:= 1;
  for i:= 1 to NrEntry do
    Result:= GetCSVWord(Line, Pos);
end;

Function GetReal(Line : string; NrEntry : Longint) : double;
begin
  Result:= StrToReal(GetCSVEntry(Line, NrEntry));
end;

Function GetCSVReal(Line : string; NrEntry : Longint) : double;
begin
  Result:= StrToReal(GetEntry(Line, NrEntry));
end;

Function GetINIcaption(Line : string) : string;
var i : LongInt;
    found : boolean;
begin
  found:= false; Result:= '';
  for i:= 1 to Length(Line) do
  begin
    if not found then
    begin
      if Line[i] = '=' then
      begin
        Found:= TRUE;
      end
      else Result:= Result + Line[i];
    end;
  end;
end;

Function GetINIvalue(Line : string) : string;
var i : LongInt;
    found : boolean;
begin
  found:= false; Result:= '';
  for i:= 1 to Length(Line) do
  begin
    if found then Result:= Result + Line[i]
    else
    begin
      if Line[i] = '=' then Found:= TRUE;
    end;
  end;
  Result:= Trim(Result);
end;

Function GetINIint(Line : string) : LongInt;
begin
  try
    Result:= StrToInt(GetINIvalue(Line));
  except
    Result:= 0;
    MessageDlg('Invalid value in INI-file', mtWarning, [mbOK], 0);
  end;
end;

Function GetINIreal(Line : string) : double;
begin
  try
    Result:= StrToFloat(GetINIvalue(Line));
  except
    Result:= 0;
    MessageDlg('Invalid value in INI-file', mtWarning, [mbOK], 0);
  end;
end;

Function Store(var F : File; var Wr;  Size : longint) : longint;
var NumWrite : integer;
begin
  BlockWrite(F, Wr, Size, NumWrite);
  Result:= NumWrite;
end;

Function Get(var F : File; var Re;  Size : longint) : longint;
var NumRead : integer;
begin
  BlockRead(F, Re, Size, NumRead);
  Result:= NumRead;
end;

Function ExtractFileNameWithoutExt(FileName : string) : string;
var Ch : Char;
    i : LongInt;
    FN : string;
begin
  FN:= ExtractFileName(FileName);
  Ch:= ' '; i:= 1; Result:= '';
  while (Ch <> '.') and (i <= Length(FN)) do
  begin
    Ch:= FN[i];
    if Ch <> '.' then Result:= Result + Ch;
    Inc(i);
  end;
end;

Procedure ExtrFileName(Value : string; var Path, FN, Ext : string);
begin
  Ext:= ExtractFileExt(Value);
  FN:= ExtractFileNameWithoutExt(Value);
  Path:= ExtractFilePath(Value);
end;

function HasAttr(const FileName: string; Attr: Word): Boolean;
var
 FileAttr: Integer;
begin
  FileAttr := FileGetAttr(FileName);
  if FileAttr = -1 then FileAttr := 0;
  Result := (FileAttr and Attr) = Attr;
end;

procedure CopyFile(const FileName, DestName: string);
var
  BytesCopied : LongInt;
  Len: Integer;
  Destination: TFileName; { holder for expanded destination name }
  Source, Dest : TFileStream;
const
  ChunkSize: Longint = 8192; { copy in 8K chunks }
begin
  Destination := ExpandFileName(DestName); { expand the destination path }
  if HasAttr(Destination, faDirectory) then { if destination is a directory... }
  begin
    Len :=  Length(Destination);
    if Destination[Len] = '\' then
      Destination := Destination + ExtractFileName(FileName) { ...clone file name }
    else
      Destination := Destination + '\' + ExtractFileName(FileName); { ...clone file name }
  end;
  try
    Source:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      Dest:= TFileStream.Create(Destination, fmCreate or fmShareExclusive);
      try
        BytesCopied:= Dest.CopyFrom(Source, 0);
      except
        ErrorMsg('Copy error');
      end;
    except
      EFCreateError.CreateFmt('Create file error', [Destination]);
    end;
  except
    EFOpenError.CreateFmt('Open file error', [FileName]);
  end;
  Source.Free;
  Dest.Free;
end;

Function CreateBackup(FileName : string) : boolean;
var Str, ONm, NNm, Path, FN, Ext : string;
begin
  ONm:= FileName;
  ExtrFileName(ONm, Path, FN, Ext);
  NNm:= Path + 'Copy of ' + FN + Ext;
  CopyFile(ONm, NNm);
  Result:= FileExists(NNm);
end;

Function RestoreFromBackup(FileName : string) : boolean;
var Str, ONm, NNm, Path, FN, Ext : string;
begin
  NNm:= FileName;
  ExtrFileName(NNm, Path, FN, Ext);
  ONm:= Path + 'Copy of ' + FN + Ext;
  if FileExists(ONm) then
  begin
    if FileExists(NNm) then
    begin
      CopyFile(ONm, NNm);
      Result:= FileExists(NNm);
    end
    else Result:= false;
  end
  else Result:= false;
end;

{Other functions}
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


{Form functions}

Procedure SetCursor(Form : TForm; Crsor : TCursor);
var i : LongInt;
begin
  with Form do
  begin
    Cursor:= Crsor;
    for I := 0 to ComponentCount - 1 do
      if Components[I] is TControl then
        TControl(Components[i]).Cursor:= Crsor;
  end;
  Application.ProcessMessages;
end;


{Error functions}

Function ShowLastError(Comment : string) : boolean;
var S : string;
begin
  S:= GetLastErrorString;
  if S <> '' then
  begin
    if MessageDlg(Comment + ': ' + S, mtError, [mbOK], 0) = idOK then Result:= TRUE
    else Result:= false;
  end;
end;

Function GetLastErrorString : string;
var ErrorValue : DWord;
begin
  ErrorValue:= GetLastError;
  case ErrorValue of
  0: Result:= '';
  ERROR_INVALID_FUNCTION : Result:= 'Invalid function';
  ERROR_FILE_NOT_FOUND : Result:= 'File not found';
  ERROR_PATH_NOT_FOUND : Result:= 'The system cannot find the path specified.';
  ERROR_TOO_MANY_OPEN_FILES : Result:= 'The system cannot open the file.';
  ERROR_ACCESS_DENIED : Result:= 'Access denied.';
  ERROR_INVALID_HANDLE : Result:= 'The handle is invalid';
  ERROR_ARENA_TRASHED  : Result:= 'The storage control blocks were destroyed.';
  ERROR_NOT_ENOUGH_MEMORY : Result:= 'Not enough storage is available to process this command.';
  ERROR_INVALID_BLOCK : Result:= 'The storage control block address is invalid.';
  ERROR_BAD_ENVIRONMENT : Result:= 'The environment is incorrect.';
  ERROR_BAD_FORMAT : Result:= 'An attempt was made to load a program with an incorrect format.';
  ERROR_INVALID_ACCESS : Result:= 'The access code is invalid.';
  ERROR_INVALID_DATA : Result:= 'The data is invalid.';
  ERROR_OUTOFMEMORY : Result:= 'Not enough storage is available to complete this operation.';
  ERROR_INVALID_DRIVE : Result:= 'The system cannot find the drive specified.';
  ERROR_CURRENT_DIRECTORY : Result:= 'The directory cannot be removed.';
  ERROR_NOT_SAME_DEVICE : Result:= 'The system cannot move the file to a different disk drive.';
  ERROR_NO_MORE_FILES : Result:= 'There are no more files.';
  ERROR_WRITE_PROTECT : Result:= 'The media is write protected.';
  ERROR_BAD_UNIT : Result:= 'The system cannot find the device specified.';
  ERROR_NOT_READY : Result:= 'The device is not ready.';
  ERROR_BAD_COMMAND : Result:= 'The device does not recognize the command.';
  ERROR_CRC : Result:= 'Data error (cyclic redundancy check).';
  ERROR_BAD_LENGTH : Result:= 'The program issued a command but the command length is incorrect.';
  ERROR_SEEK : Result:= 'The drive cannot locate a specific area or track on the disk.';
  ERROR_NOT_DOS_DISK : Result:= 'The specified disk or diskette cannot be accessed.';
  ERROR_SECTOR_NOT_FOUND : Result:= 'The drive cannot find the sector requested.';
  ERROR_OUT_OF_PAPER : Result:= 'The printer is out of paper.';
  ERROR_WRITE_FAULT : Result:= 'The system cannot write to the specified device.';
  ERROR_READ_FAULT : Result:= 'The system cannot read from the specified device.';
  ERROR_GEN_FAILURE : Result:= 'A device attached to the system is not functioning.';
  ERROR_SHARING_VIOLATION : Result:= 'The process cannot access the file because it is being used by another process.';
  ERROR_LOCK_VIOLATION : Result:= 'The process cannot access the file because another process has locked a portion of the file.';
  ERROR_WRONG_DISK : Result:= 'The wrong diskette is in the drive.';
  ERROR_SHARING_BUFFER_EXCEEDED : Result:= 'Too many files opened for sharing.';
  ERROR_HANDLE_EOF : Result:= 'Reached end of file.';
  ERROR_HANDLE_DISK_FULL : Result:= 'The disk is full.';
  ERROR_NOT_SUPPORTED : Result:= 'The network request is not supported.';
  ERROR_REM_NOT_LIST : Result:= 'The remote computer is not available.';
  ERROR_DUP_NAME : Result:= 'A duplicate name exists on the network.';
  ERROR_BAD_NETPATH : Result:= 'The network path was not found.';
  ERROR_NETWORK_BUSY : Result:= 'The network is busy.';
  ERROR_DEV_NOT_EXIST : Result:= 'The specified network resource or device is no longer available.';
  ERROR_TOO_MANY_CMDS : Result:= 'The network BIOS command limit has been reached.';
  ERROR_ADAP_HDW_ERR : Result:= 'A network adapter hardware error occurred.';
  ERROR_BAD_NET_RESP : Result:= 'The specified server cannot perform the requested operation.';
  ERROR_UNEXP_NET_ERR : Result:= 'An unexpected network error occurred.';
  ERROR_BAD_REM_ADAP : Result:= 'The remote adapter is not compatible.';
  ERROR_PRINTQ_FULL : Result:= 'The printer queue is full.';
  ERROR_NO_SPOOL_SPACE : Result:= 'Space to store the file waiting to be printed is not available on the server.';
  ERROR_PRINT_CANCELLED : Result:= 'Your file waiting to be printed was deleted.';
  ERROR_NETNAME_DELETED : Result:= 'The specified network name is no longer available.';
  ERROR_NETWORK_ACCESS_DENIED : Result:= 'Network access is denied.';
  ERROR_BAD_DEV_TYPE : Result:= 'The network resource type is not correct.';
  ERROR_BAD_NET_NAME : Result:= 'The network name cannot be found.';
  ERROR_TOO_MANY_NAMES : Result:= 'The name limit for the local computer network adapter card was exceeded.';
  ERROR_TOO_MANY_SESS : Result:= 'The network BIOS session limit was exceeded.';
  ERROR_SHARING_PAUSED : Result:= 'The remote server has been paused or is in the process of being started.';
  ERROR_REQ_NOT_ACCEP : Result:= 'No more connections can be made to this remote computer at this time because there are already as many connections as the computer can accept.';
  ERROR_REDIR_PAUSED : Result:= 'The specified printer or disk device has been paused.';
  ERROR_FILE_EXISTS : Result:= 'The file exists.';
  ERROR_CANNOT_MAKE : Result:= 'The directory or file cannot be created.';
  ERROR_FAIL_I24 : Result:= 'Fail on INT 24';
  ERROR_OUT_OF_STRUCTURES : Result:= 'Storage to process this request is not available.';
  ERROR_ALREADY_ASSIGNED : Result:= 'The local device name is already in use.';
  ERROR_INVALID_PASSWORD : Result:= 'The specified network password is not correct.';
  ERROR_INVALID_PARAMETER : Result:= 'The parameter is incorrect.';
  ERROR_NET_WRITE_FAULT : Result:= 'A write fault occurred on the network.';
  ERROR_NO_PROC_SLOTS : Result:= 'The system cannot start another process at this time.';
  ERROR_TOO_MANY_SEMAPHORES : Result:= 'Cannot create another system semaphore.';
  ERROR_EXCL_SEM_ALREADY_OWNED : Result:= 'The exclusive semaphore is owned by another process.';
  ERROR_SEM_IS_SET : Result:= 'The semaphore is set and cannot be closed.';
  ERROR_TOO_MANY_SEM_REQUESTS : Result:= 'The semaphore cannot be set again.';
  ERROR_INVALID_AT_INTERRUPT_TIME : Result:= 'Cannot request exclusive semaphores at interrupt time.';
  ERROR_SEM_OWNER_DIED : Result:= 'The previous ownership of this semaphore has ended.';
  ERROR_SEM_USER_LIMIT : Result:= 'Insert the diskette.';
  ERROR_DISK_CHANGE : Result:= 'Program stopped because alternate diskette was not inserted.';
  ERROR_DRIVE_LOCKED : Result:= 'The disk is in use or locked by another process.';
  ERROR_BROKEN_PIPE : Result:= 'The pipe has been ended.';
  ERROR_OPEN_FAILED : Result:= 'The system cannot open the device or file specified.';
  ERROR_BUFFER_OVERFLOW : Result:= 'The file name is too long.';
  ERROR_DISK_FULL : Result:= 'There is not enough space on the disk.';
  ERROR_NO_MORE_SEARCH_HANDLES : Result:= 'No more internal file identifiers available.';
  ERROR_INVALID_TARGET_HANDLE : Result:= 'he target internal file identifier is incorrect.';
  ERROR_INVALID_CATEGORY : Result:= 'The IOCTL call made by the application program is not correct.';
  ERROR_INVALID_VERIFY_SWITCH : Result:= 'The verify-on-write switch parameter value is not correct.';
  ERROR_BAD_DRIVER_LEVEL : Result:= 'The system does not support the command requested.';
  ERROR_CALL_NOT_IMPLEMENTED : Result:= 'This function is only valid in Windows NT mode.';
  ERROR_SEM_TIMEOUT : Result:= 'The semaphore timeout period has expired.';
  ERROR_INSUFFICIENT_BUFFER : Result:= 'The data area passed to a system call is too small.';
  ERROR_INVALID_NAME : Result:= 'The filename, directory name, or volume label syntax is incorrect.';
  ERROR_INVALID_LEVEL : Result:= 'The system call level is not correct.';
  ERROR_NO_VOLUME_LABEL : Result:= 'The disk has no volume label.';
  ERROR_MOD_NOT_FOUND : Result:= 'The specified module could not be found.';
  ERROR_PROC_NOT_FOUND : Result:= 'The specified procedure could not be found.';
  ERROR_WAIT_NO_CHILDREN : Result:= 'There are no child processes to wait for.';
  ERROR_CHILD_NOT_COMPLETE : Result:= 'The application cannot be run in Windows NT mode.';
  ERROR_DIRECT_ACCESS_HANDLE : Result:= 'Attempt to use a file handle to an open disk partition for an operation other than raw disk I/O.';
  ERROR_NEGATIVE_SEEK : Result:= 'An attempt was made to move the file pointer before the beginning of the file.';
  ERROR_SEEK_ON_DEVICE : Result:= 'The file pointer cannot be set on the specified device or file.';
  ERROR_IS_JOIN_TARGET : Result:= 'A JOIN or SUBST command cannot be used for a drive that contains previously joined drives.';
  ERROR_IS_JOINED : Result:= 'An attempt was made to use a JOIN or SUBST command on a drive that has already been joined.';
  ERROR_IS_SUBSTED : Result:= 'An attempt was made to use a JOIN or SUBST command on a drive that has already been substituted.';
  ERROR_NOT_JOINED : Result:= 'The system tried to delete the JOIN of a drive that is not joined.';
  ERROR_NOT_SUBSTED : Result:= 'The system tried to delete the substitution of a drive that is not substituted.';
  ERROR_JOIN_TO_JOIN : Result:= 'The system tried to join a drive to a directory on a joined drive.';
  ERROR_SUBST_TO_SUBST : Result:= 'The system tried to substitute a drive to a directory on a substituted drive.';
  ERROR_JOIN_TO_SUBST : Result:= 'The system tried to join a drive to a directory on a substituted drive.';
  ERROR_SUBST_TO_JOIN : Result:= 'The system tried to SUBST a drive to a directory on a joined drive.';
  ERROR_BUSY_DRIVE : Result:= 'The system cannot perform a JOIN or SUBST at this time.';
  ERROR_SAME_DRIVE : Result:= 'The system cannot join or substitute a drive to or for a directory on the same drive.';
  ERROR_DIR_NOT_ROOT : Result:= 'The directory is not a subdirectory of the root directory.';
  ERROR_DIR_NOT_EMPTY : Result:= 'The directory is not empty.';
  ERROR_IS_SUBST_PATH : Result:= 'The path specified is being used in a substitute.';
  ERROR_IS_JOIN_PATH : Result:= 'Not enough resources are available to process this command.';
  ERROR_PATH_BUSY : Result:= 'The path specified cannot be used at this time.';
  ERROR_IS_SUBST_TARGET : Result:= 'An attempt was made to join or substitute a drive for which a directory on the drive is the target of a previous substitute.';
  ERROR_SYSTEM_TRACE : Result:= 'System trace information was not specified in your CONFIG.SYS file, or tracing is disallowed.';
  ERROR_INVALID_EVENT_COUNT : Result:= 'The number of specified semaphore events for DosMuxSemWait is not correct.';
  ERROR_TOO_MANY_MUXWAITERS : Result:= 'DosMuxSemWait did not execute; too many semaphores are already set.';
  ERROR_INVALID_LIST_FORMAT : Result:= 'The DosMuxSemWait list is not correct.';
  ERROR_LABEL_TOO_LONG : Result:= 'The volume label you entered exceeds the label character limit of the target file system.';
  ERROR_TOO_MANY_TCBS : Result:= 'Cannot create another thread.';
  ERROR_SIGNAL_REFUSED : Result:= 'The recipient process has refused the signal.';
  ERROR_DISCARDED : Result:= 'The segment is already discarded and cannot be locked.';
  ERROR_NOT_LOCKED : Result:= 'The segment is already unlocked.';
  ERROR_BAD_THREADID_ADDR : Result:= 'The address for the thread ID is not correct.';
  ERROR_BAD_ARGUMENTS : Result:= 'The argument string passed to DosExecPgm is not correct.';
  ERROR_BAD_PATHNAME : Result:= 'The specified path is invalid.';
  ERROR_SIGNAL_PENDING : Result:= 'A signal is already pending.';
  ERROR_MAX_THRDS_REACHED : Result:= 'No more threads can be created in the system.';
  ERROR_LOCK_FAILED : Result:= 'Unable to lock a region of a file.';
  ERROR_BUSY : Result:= 'The requested resource is in use.';
  ERROR_CANCEL_VIOLATION : Result:= 'A lock request was not outstanding for the supplied cancel region.';
  ERROR_ATOMIC_LOCKS_NOT_SUPPORTED : Result:= 'The file system does not support atomic changes to the lock type.';
  ERROR_INVALID_SEGMENT_NUMBER : Result:= 'The system detected a segment number that was not correct.';
  ERROR_INVALID_ORDINAL : Result:= 'The operating system cannot run.';
  ERROR_ALREADY_EXISTS : Result:= 'Cannot create a file when that file already exists.';
  ERROR_INVALID_FLAG_NUMBER : Result:= 'The flag passed is not correct.';
  ERROR_SEM_NOT_FOUND : Result:= 'The specified system semaphore name was not found.';
  ERROR_INVALID_STARTING_CODESEG : Result:= 'The operating system cannot run.';
  ERROR_INVALID_STACKSEG : Result:= 'The operating system cannot run.';
  ERROR_INVALID_MODULETYPE : Result:= 'The operating system cannot run.';
  ERROR_INVALID_EXE_SIGNATURE : Result:= 'Cannot run in Windows NT mode.';
  ERROR_EXE_MARKED_INVALID : Result:= 'The operating system cannot run.';
  ERROR_BAD_EXE_FORMAT : Result:= 'Not a valid Windows NT application.';
  ERROR_ITERATED_DATA_EXCEEDS_64k : Result:= 'The operating system cannot run.';
  ERROR_INVALID_MINALLOCSIZE : Result:= 'The operating system cannot run.';
  ERROR_DYNLINK_FROM_INVALID_RING : Result:= 'The operating system cannot run this application program.';
  ERROR_IOPL_NOT_ENABLED : Result:= 'The operating system is not presently configured to run this application.';
  ERROR_INVALID_SEGDPL : Result:= 'The operating system cannot run.';
  ERROR_AUTODATASEG_EXCEEDS_64k : Result:= 'The operating system cannot run this application program.';
  ERROR_RING2SEG_MUST_BE_MOVABLE : Result:= 'The code segment cannot be greater than or equal to 64KB.';
  ERROR_RELOC_CHAIN_XEEDS_SEGLIM : Result:= 'The operating system cannot run.';
  ERROR_INFLOOP_IN_RELOC_CHAIN : Result:= 'The operating system cannot run.';
  ERROR_ENVVAR_NOT_FOUND : Result:= 'The system could not find the environment option that was entered.';
  ERROR_NO_SIGNAL_SENT : Result:= 'No process in the command subtree has a signal handler.';
  ERROR_FILENAME_EXCED_RANGE : Result:= 'The filename or extension is too long.';
  ERROR_RING2_STACK_IN_USE : Result:= 'The ring 2 stack is in use.';
  ERROR_META_EXPANSION_TOO_LONG : Result:= 'The global filename characters, * or ?, are entered incorrectly or too many global filename characters are specified.';
  ERROR_INVALID_SIGNAL_NUMBER : Result:= 'The signal being posted is not correct.';
  ERROR_THREAD_1_INACTIVE : Result:= 'The signal handler cannot be set.';
  ERROR_LOCKED : Result:= 'The segment is locked and cannot be reallocated.';
  ERROR_TOO_MANY_MODULES : Result:= 'Too many dynamic link modules are attached to this program or dynamic link module.';
  ERROR_NESTING_NOT_ALLOWED : Result:= 'Cannot nest calls to LoadModule.';
  ERROR_EXE_MACHINE_TYPE_MISMATCH : Result:= 'The image file is valid, but is for a machine type other than the current machine.';
  ERROR_BAD_PIPE : Result:= 'The pipe state is invalid.';
  ERROR_PIPE_BUSY : Result:= 'All pipe instances are busy.';
  ERROR_NO_DATA : Result:= 'The pipe is being closed.';
  ERROR_PIPE_NOT_CONNECTED : Result:= 'No process is on the other end of the pipe.';
  ERROR_MORE_DATA : Result:= 'More data is available.';
  ERROR_VC_DISCONNECTED : Result:= 'The session was cancelled.';
  ERROR_INVALID_EA_NAME : Result:= 'The specified extended attribute name was invalid.';
  ERROR_EA_LIST_INCONSISTENT : Result:= 'The extended attributes are inconsistent.';
  ERROR_NO_MORE_ITEMS : Result:= 'No more data is available.';
  ERROR_CANNOT_COPY : Result:= 'The Copy API cannot be used.';
  ERROR_DIRECTORY : Result:= 'The directory name is invalid.';
  ERROR_EAS_DIDNT_FIT : Result:= 'The extended attributes did not fit in the buffer.';
  ERROR_EA_FILE_CORRUPT : Result:= 'The extended attribute file on the mounted file system is corrupt.';
  ERROR_EA_TABLE_FULL : Result:= 'The extended attribute table file is full.';
  ERROR_INVALID_EA_HANDLE : Result:= 'The specified extended attribute handle is invalid.';
  ERROR_EAS_NOT_SUPPORTED : Result:= 'The mounted file system does not support extended attributes.';
  ERROR_NOT_OWNER : Result:= 'Attempt to release mutex not owned by caller.';
  ERROR_TOO_MANY_POSTS : Result:= 'Too many posts were made to a semaphore.';
  ERROR_PARTIAL_COPY : Result:= 'Only part of a Read/WriteProcessMemory request was completed.';
  ERROR_MR_MID_NOT_FOUND : Result:= 'The system cannot find message for message number in message file.';
  ERROR_INVALID_ADDRESS : Result:= 'Attempt to access invalid address.';
  ERROR_ARITHMETIC_OVERFLOW : Result:= 'Arithmetic result exceeded 32 bits.';
  ERROR_PIPE_CONNECTED : Result:= 'There is a process on other end of the pipe.';
  ERROR_PIPE_LISTENING : Result:= 'Waiting for a process to open the other end of the pipe';
  ERROR_EA_ACCESS_DENIED : Result:= 'Access to the extended attribute was denied.';
  ERROR_OPERATION_ABORTED : Result:= 'The I/O operation has been aborted because of either a thread exit or an application request.';
  ERROR_IO_INCOMPLETE : Result:= 'Overlapped I/O event is not in a signalled state.';
  ERROR_IO_PENDING : Result:= 'Overlapped I/O operation is in progress.';
  ERROR_NOACCESS : Result:= 'Invalid access to memory location.';
  ERROR_SWAPERROR : Result:= 'Error performing inpage operation.';
  ERROR_STACK_OVERFLOW : Result:= 'Recursion too deep, stack overflowed.';
  ERROR_INVALID_MESSAGE : Result:= 'The window cannot act on the sent message.';
  ERROR_CAN_NOT_COMPLETE : Result:= 'Cannot complete this function.';
  ERROR_INVALID_FLAGS : Result:= 'Invalid flags.';
  ERROR_UNRECOGNIZED_VOLUME : Result:= 'The volume does not contain a recognized file system. Please make sure that all required file system drivers are loaded and that the volume is not corrupt.';
  ERROR_FILE_INVALID : Result:= 'The volume for a file has been externally altered such that the opened file is no longer valid.';
  ERROR_FULLSCREEN_MODE : Result:= 'The requested operation cannot be performed in full-screen mode.';
  ERROR_NO_TOKEN : Result:= 'An attempt was made to reference a token that does not exist.';
  ERROR_BADDB : Result:= 'The configuration registry database is corrupt.';
  ERROR_BADKEY : Result:= 'The configuration registry key is invalid.';
  ERROR_CANTOPEN : Result:= 'The configuration registry key could not be opened.';
  ERROR_CANTREAD : Result:= 'The configuration registry key could not be read.';
  ERROR_CANTWRITE : Result:= 'The configuration registry key could not be written.';
  ERROR_REGISTRY_RECOVERED : Result:= 'One of the files in the Registry database had to be recovered by use of a log or alternate copy.  The recovery was successful.';
  ERROR_REGISTRY_CORRUPT : Result:= 'The Registry is corrupt. The structure of one of the files that contains Registry data is corrupt, or ' +
                                    'the systems image of the file in memory is corrupt, or the file could not be recovered because the alternate ' +
                                    'copy or log was absent or corrupt.';
  ERROR_REGISTRY_IO_FAILED : Result:= 'An I/O operation initiated by the Registry failed unrecoverably. ' +
                                      'The Registry could not read in, or write out, or flush, one of the files ' +
                                      'that contain the systems image of the Registry.';
  ERROR_NOT_REGISTRY_FILE : Result:= 'The system has attempted to load or restore a file into the Registry, but the ' +
                                      'specified file is not in a Registry file format.';
  ERROR_KEY_DELETED : Result:= 'Illegal operation attempted on a Registry key which has been marked for deletion.';
  ERROR_NO_LOG_SPACE : Result:= 'System could not allocate the required space in a Registry log.';
  ERROR_KEY_HAS_CHILDREN : Result:= 'Cannot create a symbolic link in a Registry key that already has subkeys or values.';
  ERROR_CHILD_MUST_BE_VOLATILE : Result:= 'Cannot create a stable subkey under a volatile parent key.';
  ERROR_NOTIFY_ENUM_DIR : Result:= 'A notify change request is being completed and the information is not being returned in the callers buffer. The caller now needs to enumerate the files to find the changes.';
  ERROR_DEPENDENT_SERVICES_RUNNING : Result:= 'A stop control has been sent to a service which other running services are dependent on.';
  ERROR_INVALID_SERVICE_CONTROL : Result:= 'The requested control is not valid for this service.';
  ERROR_SERVICE_REQUEST_TIMEOUT : Result:= 'The service did not respond to the start or control request in a timely fashion.';
  ERROR_SERVICE_NO_THREAD : Result:= 'A thread could not be created for the service.';
  ERROR_SERVICE_DATABASE_LOCKED : Result:= 'The service database is locked.';
  ERROR_SERVICE_ALREADY_RUNNING : Result:= 'An instance of the service is already running.';
  ERROR_INVALID_SERVICE_ACCOUNT : Result:= 'The account name is invalid or does not exist.';
  ERROR_SERVICE_DISABLED : Result:= 'The specified service is disabled and cannot be started.';
  ERROR_CIRCULAR_DEPENDENCY : Result:= 'Circular service dependency was specified.';
  ERROR_SERVICE_DOES_NOT_EXIST : Result:= 'The specified service does not exist as an installed service.';
  ERROR_SERVICE_CANNOT_ACCEPT_CTRL : Result:= 'The service cannot accept control messages at this time.';
  ERROR_SERVICE_NOT_ACTIVE : Result:= 'The service has not been started.';
  ERROR_FAILED_SERVICE_CONTROLLER_ : Result:= 'The service process could not connect to the service controller.';
  ERROR_EXCEPTION_IN_SERVICE : Result:= 'An exception occurred in the service when handling the control request.';
  ERROR_DATABASE_DOES_NOT_EXIST : Result:= 'The database specified does not exist.';
  ERROR_SERVICE_SPECIFIC_ERROR : Result:= 'The service has returned a service-specific error code.';
  ERROR_PROCESS_ABORTED : Result:= 'The process terminated unexpectedly.';
  ERROR_SERVICE_DEPENDENCY_FAIL : Result:= 'The dependency service or group failed to start.';
  ERROR_SERVICE_LOGON_FAILED : Result:= 'The service did not start due to a logon failure.';
  ERROR_SERVICE_START_HANG : Result:= 'After starting, the service hung in a start-pending state.';
  ERROR_INVALID_SERVICE_LOCK : Result:= 'The specified service database lock is invalid.';
  ERROR_SERVICE_MARKED_FOR_DELETE : Result:= 'The specified service has been marked for deletion.';
  ERROR_SERVICE_EXISTS : Result:= 'The specified service already exists.';
  ERROR_ALREADY_RUNNING_LKG : Result:= 'The system is currently running with the last-known-good configuration.';
  ERROR_SERVICE_DEPENDENCY_DELETED : Result:= 'The dependency service does not exist or has been marked for deletion.';
  ERROR_BOOT_ALREADY_ACCEPTED : Result:= 'The current boot has already been accepted for use as the last-known-good control set.';
  ERROR_SERVICE_NEVER_STARTED : Result:= 'No attempts to start the service have been made since the last boot.';
  ERROR_DUPLICATE_SERVICE_NAME : Result:= 'The name is already in use as either a service name or a service display name.';
  ERROR_DIFFERENT_SERVICE_ACCOUNT : Result:= 'The account specified for this service is different from the account specified for other services running in the same process.';
  ERROR_END_OF_MEDIA : Result:= 'The physical end of the tape has been reached.';
  ERROR_FILEMARK_DETECTED : Result:= 'A tape access reached a filemark.';
  ERROR_BEGINNING_OF_MEDIA : Result:= 'Beginning of tape or partition was encountered.';
  ERROR_SETMARK_DETECTED : Result:= 'A tape access reached the end of a set of files.';
  ERROR_NO_DATA_DETECTED : Result:= 'No more data is on the tape.';
  ERROR_PARTITION_FAILURE : Result:= 'Tape could not be partitioned.';
  ERROR_INVALID_BLOCK_LENGTH : Result:= 'When accessing a new tape of a multivolume partition, the current blocksize is incorrect.';
  ERROR_DEVICE_NOT_PARTITIONED : Result:= 'Tape partition information could not be found when loading a tape.';
  ERROR_UNABLE_TO_LOCK_MEDIA : Result:= 'Unable to lock the media eject mechanism.';
  ERROR_UNABLE_TO_UNLOAD_MEDIA : Result:= 'Unable to unload the media.';
  ERROR_MEDIA_CHANGED : Result:= 'Media in drive may have changed.';
  ERROR_BUS_RESET : Result:= 'The I/O bus was reset.';
  ERROR_NO_MEDIA_IN_DRIVE : Result:= 'No media in drive.';
  ERROR_NO_UNICODE_TRANSLATION : Result:= 'No mapping for the Unicode character exists in the target multi-byte code page.';
  ERROR_DLL_INIT_FAILED : Result:= 'A dynamic link library (DLL) initialization routine failed.';
  ERROR_SHUTDOWN_IN_PROGRESS : Result:= 'A system shutdown is in progress.';
  ERROR_NO_SHUTDOWN_IN_PROGRESS : Result:= 'Unable to abort the system shutdown because no shutdown was in progress.';
  ERROR_IO_DEVICE : Result:= 'The request could not be performed because of an I/O device error.';
  ERROR_SERIAL_NO_DEVICE : Result:= 'No serial device was successfully initialized.  The serial driver will unload.';
  ERROR_IRQ_BUSY : Result:= 'Unable to open a device that was sharing an interrupt request (IRQ) with other devices. At least one other device that uses that IRQ was already opened.';
  ERROR_MORE_WRITES : Result:= 'A serial I/O operation was completed by another write to the serial port. (The IOCTL_SERIAL_XOFF_COUNTER reached zero.)';
  ERROR_COUNTER_TIMEOUT : Result:= 'A serial I/O operation completed because the time-out period expired. (The IOCTL_SERIAL_XOFF_COUNTER did not reach zero.)';
  ERROR_FLOPPY_ID_MARK_NOT_FOUND : Result:= 'No ID address mark was found on the floppy disk.';
  ERROR_FLOPPY_WRONG_CYLINDER : Result:= 'Mismatch between the floppy disk sector ID field and the floppy disk controller track address.';
  ERROR_FLOPPY_UNKNOWN_ERROR : Result:= 'The floppy disk controller reported an error that is not recognized by the floppy disk driver.';
  ERROR_FLOPPY_BAD_REGISTERS : Result:= 'The floppy disk controller returned inconsistent results in its registers.';
  ERROR_DISK_RECALIBRATE_FAILED : Result:= 'While accessing the hard disk, a recalibrate operation failed, even after retries.';
  ERROR_DISK_OPERATION_FAILED : Result:= 'While accessing the hard disk, a disk operation failed even after retries.';
  ERROR_DISK_RESET_FAILED : Result:= 'While accessing the hard disk, a disk controller reset was needed, but even that failed.';
  ERROR_EOM_OVERFLOW : Result:= 'Physical end of tape encountered.';
  ERROR_NOT_ENOUGH_SERVER_MEMORY : Result:= 'Not enough server storage is available to process this command.';
  ERROR_POSSIBLE_DEADLOCK : Result:= 'A potential deadlock condition has been detected.';
  ERROR_MAPPED_ALIGNMENT : Result:= 'The base address or the file offset specified does not have the proper alignment.';
  { An attempt to change the system power state was vetoed by another }
  { application or driver. }
  ERROR_SET_POWER_STATE_VETOED : Result:= '';
  { The system BIOS failed an attempt to change the system power state. }
  ERROR_SET_POWER_STATE_FAILED : Result:= '';
  {  An attempt was made to create more links on a file than }
  {  the file system supports. }
  ERROR_TOO_MANY_LINKS : Result:= '';
  { The specified program requires a newer version of Windows. }
  ERROR_OLD_WIN_VERSION : Result:= '';
  { The specified program is not a Windows or MS-DOS program. }
  ERROR_APP_WRONG_OS : Result:= '';
  { Cannot start more than one instance of the specified program. }
  ERROR_SINGLE_INSTANCE_APP : Result:= '';
  {  The specified program was written for an older version of Windows. }
  ERROR_RMODE_APP : Result:= '';
  { One of the library files needed to run this application is damaged. }
  ERROR_INVALID_DLL : Result:= '';
  { No application is associated with the specified file for this operation. }
  ERROR_NO_ASSOCIATION : Result:= '';
  { An error occurred in sending the command to the application. }
  ERROR_DDE_FAIL : Result:= '';
  { One of the library files needed to run this application cannot be found. }
  ERROR_DLL_NOT_FOUND : Result:= '';
  { The specified username is invalid. }
  ERROR_BAD_USERNAME : Result:= '';
  { This network connection does not exist. }
  ERROR_NOT_CONNECTED : Result:= '';
  { This network connection has files open or requests pending. }
  ERROR_OPEN_FILES : Result:= '';
  { Active connections still exist. }
  ERROR_ACTIVE_CONNECTIONS : Result:= '';
  { The device is in use by an active process and cannot be disconnected. }
  ERROR_DEVICE_IN_USE : Result:= '';
  { The specified device name is invalid. }
  ERROR_BAD_DEVICE : Result:= '';
  { The device is not currently connected but it is a remembered connection. }
  ERROR_CONNECTION_UNAVAIL : Result:= '';
  { An attempt was made to remember a device that had previously been remembered. }
  ERROR_DEVICE_ALREADY_REMEMBERED : Result:= '';
  { No network provider accepted the given network path. }
  ERROR_NO_NET_OR_BAD_PATH : Result:= '';
  { The specified network provider name is invalid. }
  ERROR_BAD_PROVIDER : Result:= '';
  { Unable to open the network connection profile. }
  ERROR_CANNOT_OPEN_PROFILE : Result:= '';
  { The network connection profile is corrupt. }
  ERROR_BAD_PROFILE : Result:= '';
  { Cannot enumerate a non-container. }
  ERROR_NOT_CONTAINER : Result:= '';
  { An extended error has occurred. }
  ERROR_EXTENDED_ERROR : Result:= '';
  { The format of the specified group name is invalid. }
  ERROR_INVALID_GROUPNAME : Result:= '';
  { The format of the specified computer name is invalid. }
  ERROR_INVALID_COMPUTERNAME : Result:= '';
  { The format of the specified event name is invalid. }
  ERROR_INVALID_EVENTNAME : Result:= '';
  { The format of the specified domain name is invalid. }
  ERROR_INVALID_DOMAINNAME : Result:= '';
  { The format of the specified service name is invalid. }
  ERROR_INVALID_SERVICENAME : Result:= '';
  { The format of the specified network name is invalid. }
  ERROR_INVALID_NETNAME : Result:= '';
  { The format of the specified share name is invalid. }
  ERROR_INVALID_SHARENAME : Result:= '';
  { The format of the specified password is invalid. }
  ERROR_INVALID_PASSWORDNAME : Result:= '';
  { The format of the specified message name is invalid. }
  ERROR_INVALID_MESSAGENAME : Result:= '';
  { The format of the specified message destination is invalid. }
  ERROR_INVALID_MESSAGEDEST : Result:= '';
  { The credentials supplied conflict with an existing set of credentials. }
  ERROR_SESSION_CREDENTIAL_CONFLICT : Result:= '';
  { An attempt was made to establish a session to a network server, but there }
  { are already too many sessions established to that server. }
  ERROR_REMOTE_SESSION_LIMIT_EXCEEDED : Result:= '';
  { The workgroup or domain name is already in use by another computer on the }
  { network. }
  ERROR_DUP_DOMAINNAME : Result:= '';
  { The network is not present or not started. }
  ERROR_NO_NETWORK : Result:= '';
  { The operation was cancelled by the user. }
  ERROR_CANCELLED : Result:= '';
  { The requested operation cannot be performed on a file with a user mapped section open. }
  ERROR_USER_MAPPED_FILE : Result:= '';
  { The remote system refused the network connection. }
  ERROR_CONNECTION_REFUSED : Result:= '';
  { The network connection was gracefully closed. }
  ERROR_GRACEFUL_DISCONNECT : Result:= '';
  { The network transport endpoint already has an address associated with it. }
  ERROR_ADDRESS_ALREADY_ASSOCIATED : Result:= '';
  { An address has not yet been associated with the network endpoint. }
  ERROR_ADDRESS_NOT_ASSOCIATED : Result:= '';
  { An operation was attempted on a non-existent network connection. }
  ERROR_CONNECTION_INVALID : Result:= '';
  { An invalid operation was attempted on an active network connection. }
  ERROR_CONNECTION_ACTIVE : Result:= '';
  { The remote network is not reachable by the transport. }
  ERROR_NETWORK_UNREACHABLE : Result:= '';
  { The remote system is not reachable by the transport. }
  ERROR_HOST_UNREACHABLE : Result:= '';
  { The remote system does not support the transport protocol. }
  ERROR_PROTOCOL_UNREACHABLE : Result:= '';
  { No service is operating at the destination network endpoint }
  { on the remote system. }
  ERROR_PORT_UNREACHABLE : Result:= '';
  { The request was aborted. }
  ERROR_REQUEST_ABORTED : Result:= '';
  { The network connection was aborted by the local system. }
  ERROR_CONNECTION_ABORTED : Result:= '';
  { The operation could not be completed.  A retry should be performed. }
  ERROR_RETRY : Result:= '';
  { A connection to the server could not be made because the limit on the number of }
  { concurrent connections for this account has been reached. }
  ERROR_CONNECTION_COUNT_LIMIT : Result:= '';
  { Attempting to login during an unauthorized time of day for this account. }
  ERROR_LOGIN_TIME_RESTRICTION : Result:= '';
  { The account is not authorized to login from this station. }
  ERROR_LOGIN_WKSTA_RESTRICTION : Result:= '';
  { The network address could not be used for the operation requested. }
  ERROR_INCORRECT_ADDRESS : Result:= '';
  { The service is already registered. }
  ERROR_ALREADY_REGISTERED : Result:= '';
  { The specified service does not exist. }
  ERROR_SERVICE_NOT_FOUND : Result:= '';
  { The operation being requested was not performed because the user }
  { has not been authenticated. }
  ERROR_NOT_AUTHENTICATED : Result:= '';
  { The operation being requested was not performed because the user }
  { has not logged on to the network. }
  { The specified service does not exist. }
  ERROR_NOT_LOGGED_ON : Result:= '';
  { Return that wants caller to continue with work in progress. }
  ERROR_CONTINUE : Result:= '';
  { An attempt was made to perform an initialization operation when }
  { initialization has already been completed. }
  ERROR_ALREADY_INITIALIZED : Result:= '';
  { No more local devices. }
  ERROR_NO_MORE_DEVICES : Result:= '';
  { Not all privileges referenced are assigned to the caller. }
  ERROR_NOT_ALL_ASSIGNED : Result:= '';
  { Some mapping between account names and security IDs was not done. }
  ERROR_SOME_NOT_MAPPED : Result:= '';
  { No system quota limits are specifically set for this account. }
  ERROR_NO_QUOTAS_FOR_ACCOUNT : Result:= '';
  { No encryption key is available.  A well-known encryption key was returned. }
  ERROR_LOCAL_USER_SESSION_KEY : Result:= '';
  { The NT password is too complex to be converted to a LAN Manager }
  { password.  The LAN Manager password returned is a NULL string. }
  ERROR_NULL_LM_PASSWORD : Result:= '';
  { The revision level is unknown. }
  ERROR_UNKNOWN_REVISION : Result:= '';
  { Indicates two revision levels are incompatible. }
  ERROR_REVISION_MISMATCH : Result:= '';
  { This security ID may not be assigned as the owner of this object. }
  ERROR_INVALID_OWNER : Result:= '';
  { This security ID may not be assigned as the primary group of an object. }
  ERROR_INVALID_PRIMARY_GROUP : Result:= '';
  { An attempt has been made to operate on an impersonation token }
  { by a thread that is not currently impersonating a client. }
  ERROR_NO_IMPERSONATION_TOKEN : Result:= '';
  { The group may not be disabled. }
  ERROR_CANT_DISABLE_MANDATORY : Result:= '';
  { There are currently no logon servers available to service the logon }
  { request. }
  ERROR_NO_LOGON_SERVERS : Result:= '';
  {  A specified logon session does not exist.  It may already have }
  {  been terminated. }
  ERROR_NO_SUCH_LOGON_SESSION : Result:= '';
  {  A specified privilege does not exist. }
  ERROR_NO_SUCH_PRIVILEGE : Result:= '';
  {  A required privilege is not held by the client. }
  ERROR_PRIVILEGE_NOT_HELD : Result:= '';
  { The name provided is not a properly formed account name. }
  ERROR_INVALID_ACCOUNT_NAME : Result:= '';
  { The specified user already exists. }
  ERROR_USER_EXISTS : Result:= '';
  { The specified user does not exist. }
  ERROR_NO_SUCH_USER : Result:= '';
  { The specified group already exists. }
  ERROR_GROUP_EXISTS : Result:= '';
  { The specified group does not exist. }
  ERROR_NO_SUCH_GROUP : Result:= '';
  { Either the specified user account is already a member of the specified }
  { group, or the specified group cannot be deleted because it contains }
  { a member. }
  ERROR_MEMBER_IN_GROUP : Result:= '';
  { The specified user account is not a member of the specified group account. }
  ERROR_MEMBER_NOT_IN_GROUP : Result:= '';
  { The last remaining administration account cannot be disabled }
  { or deleted. }
  ERROR_LAST_ADMIN : Result:= '';
  { Unable to update the password.  The value provided as the current }
  { password is incorrect. }
  ERROR_WRONG_PASSWORD : Result:= '';
  { Unable to update the password.  The value provided for the new password }
  { contains values that are not allowed in passwords. }
  ERROR_ILL_FORMED_PASSWORD : Result:= '';
  { Unable to update the password because a password update rule has been }
  { violated. }
  ERROR_PASSWORD_RESTRICTION : Result:= '';
  { Logon failure: unknown user name or bad password. }
  ERROR_LOGON_FAILURE : Result:= '';
  { Logon failure: user account restriction. }
  ERROR_ACCOUNT_RESTRICTION : Result:= '';
  { Logon failure: account logon time restriction violation. }
  ERROR_INVALID_LOGON_HOURS : Result:= '';
  { Logon failure: user not allowed to log on to this computer. }
  ERROR_INVALID_WORKSTATION : Result:= '';
  { Logon failure: the specified account password has expired. }
  ERROR_PASSWORD_EXPIRED : Result:= '';
  { Logon failure: account currently disabled. }
  ERROR_ACCOUNT_DISABLED : Result:= '';
  { No mapping between account names and security IDs was done. }
  ERROR_NONE_MAPPED : Result:= '';
  { Too many local user identifiers (LUIDs) were requested at one time. }
  ERROR_TOO_MANY_LUIDS_REQUESTED : Result:= '';
  { No more local user identifiers (LUIDs) are available. }
  ERROR_LUIDS_EXHAUSTED : Result:= '';
  { The subauthority part of a security ID is invalid for this particular use. }
  ERROR_INVALID_SUB_AUTHORITY : Result:= '';
  { The access control list (ACL) structure is invalid. }
  ERROR_INVALID_ACL : Result:= '';
  { The security ID structure is invalid. }
  ERROR_INVALID_SID : Result:= '';
  { The security descriptor structure is invalid. }
  ERROR_INVALID_SECURITY_DESCR : Result:= '';
  { The inherited access control list (ACL) or access control entry (ACE) }
  { could not be built. }
  ERROR_BAD_INHERITANCE_ACL : Result:= '';
  { The server is currently disabled. }
  ERROR_SERVER_DISABLED : Result:= '';
  { The server is currently enabled. }
  ERROR_SERVER_NOT_DISABLED : Result:= '';
  { The value provided was an invalid value for an identifier authority. }
  ERROR_INVALID_ID_AUTHORITY : Result:= '';
  { No more memory is available for security information updates. }
  ERROR_ALLOTTED_SPACE_EXCEEDED : Result:= '';
  { The specified attributes are invalid, or incompatible with the }
  { attributes for the group as a whole. }
  ERROR_INVALID_GROUP_ATTRIBUTES : Result:= '';
  { Either a required impersonation level was not provided, or the }
  { provided impersonation level is invalid. }
  ERROR_BAD_IMPERSONATION_LEVEL : Result:= '';
  { Cannot open an anonymous level security token. }
  ERROR_CANT_OPEN_ANONYMOUS : Result:= '';
  { The validation information class requested was invalid. }
  ERROR_BAD_VALIDATION_CLASS : Result:= '';
  { The type of the token is inappropriate for its attempted use. }
  ERROR_BAD_TOKEN_TYPE : Result:= '';
  { Unable to perform a security operation on an object }
  { which has no associated security. }
  ERROR_NO_SECURITY_ON_OBJECT : Result:= '';
  { Indicates a Windows NT Server could not be contacted or that }
  { objects within the domain are protected such that necessary }
  { information could not be retrieved. }
  ERROR_CANT_ACCESS_DOMAIN_INFO : Result:= '';
  { The security account manager (SAM) or local security }
  { authority (LSA) server was in the wrong state to perform }
  { the security operation. }
  ERROR_INVALID_SERVER_STATE : Result:= '';
  { The domain was in the wrong state to perform the security operation. }
  ERROR_INVALID_DOMAIN_STATE : Result:= '';
  { This operation is only allowed for the Primary Domain Controller of the domain. }
  ERROR_INVALID_DOMAIN_ROLE : Result:= '';
  { The specified domain did not exist. }
  ERROR_NO_SUCH_DOMAIN : Result:= '';
  { The specified domain already exists. }
  ERROR_DOMAIN_EXISTS : Result:= '';
  { An attempt was made to exceed the limit on the number of domains per server. }
  ERROR_DOMAIN_LIMIT_EXCEEDED : Result:= '';
  { Unable to complete the requested operation because of either a }
  { catastrophic media failure or a data structure corruption on the disk. }
  ERROR_INTERNAL_DB_CORRUPTION : Result:= '';
  { The security account database contains an internal inconsistency. }
  ERROR_INTERNAL_ERROR : Result:= '';
  { Generic access types were contained in an access mask which should }
  { already be mapped to non-generic types. }
  ERROR_GENERIC_NOT_MAPPED : Result:= '';
  { A security descriptor is not in the right format (absolute or self-relative). }
  ERROR_BAD_DESCRIPTOR_FORMAT : Result:= '';
  { The requested action is restricted for use by logon processes }
  { only.  The calling process has not registered as a logon process. }
  ERROR_NOT_LOGON_PROCESS : Result:= '';
  { Cannot start a new logon session with an ID that is already in use. }
  ERROR_LOGON_SESSION_EXISTS : Result:= '';
  { A specified authentication package is unknown. }
  ERROR_NO_SUCH_PACKAGE : Result:= '';
  { The logon session is not in a state that is consistent with the }
  { requested operation. }
  ERROR_BAD_LOGON_SESSION_STATE : Result:= '';
  { The logon session ID is already in use. }
  ERROR_LOGON_SESSION_COLLISION : Result:= '';
  { A logon request contained an invalid logon type value. }
  ERROR_INVALID_LOGON_TYPE : Result:= '';
  { Unable to impersonate via a named pipe until data has been read }
  { from that pipe. }
  ERROR_CANNOT_IMPERSONATE : Result:= '';
  { The transaction state of a Registry subtree is incompatible with the }
  { requested operation. }
  ERROR_RXACT_INVALID_STATE : Result:= '';
  { An internal security database corruption has been encountered. }
  ERROR_RXACT_COMMIT_FAILURE : Result:= '';
  { Cannot perform this operation on built-in accounts. }
  ERROR_SPECIAL_ACCOUNT : Result:= '';
  { Cannot perform this operation on this built-in special group. }
  ERROR_SPECIAL_GROUP : Result:= '';
  { Cannot perform this operation on this built-in special user. }
  ERROR_SPECIAL_USER : Result:= '';
  { The user cannot be removed from a group because the group }
  { is currently the user's primary group. }
  ERROR_MEMBERS_PRIMARY_GROUP : Result:= '';
  { The token is already in use as a primary token. }
  ERROR_TOKEN_ALREADY_IN_USE : Result:= '';
  { The specified local group does not exist. }
  ERROR_NO_SUCH_ALIAS : Result:= '';
  { The specified account name is not a member of the local group. }
  ERROR_MEMBER_NOT_IN_ALIAS : Result:= '';
  { The specified account name is already a member of the local group. }
  ERROR_MEMBER_IN_ALIAS : Result:= '';
  { The specified local group already exists. }
  ERROR_ALIAS_EXISTS : Result:= '';
  { Logon failure: the user has not been granted the requested }
  { logon type at this computer. }
  ERROR_LOGON_NOT_GRANTED : Result:= '';
  { The maximum number of secrets that may be stored in a single system has been }
  { exceeded. }
  ERROR_TOO_MANY_SECRETS : Result:= '';
  { The length of a secret exceeds the maximum length allowed. }
  ERROR_SECRET_TOO_LONG : Result:= '';
  { The local security authority database contains an internal inconsistency. }
  ERROR_INTERNAL_DB_ERROR : Result:= '';
  { During a logon attempt, the user's security context accumulated too many }
  { security IDs. }
  ERROR_TOO_MANY_CONTEXT_IDS : Result:= '';
  { Logon failure: the user has not been granted the requested logon type }
  { at this computer. }
  ERROR_LOGON_TYPE_NOT_GRANTED : Result:= '';
  { A cross-encrypted password is necessary to change a user password. }
  ERROR_NT_CROSS_ENCRYPTION_REQUIRED : Result:= '';
  { A new member could not be added to a local group because the member does }
  { not exist. }
  ERROR_NO_SUCH_MEMBER : Result:= '';
  { A new member could not be added to a local group because the member has the }
  { wrong account type. }
  ERROR_INVALID_MEMBER : Result:= '';
  { Too many security IDs have been specified. }
  ERROR_TOO_MANY_SIDS : Result:= '';
  { A cross-encrypted password is necessary to change this user password. }
  ERROR_LM_CROSS_ENCRYPTION_REQUIRED : Result:= '';
  { Indicates an TACL contains no inheritable components }
  ERROR_NO_INHERITANCE : Result:= '';
  { The file or directory is corrupt and non-readable. }
  ERROR_FILE_CORRUPT : Result:= '';
  { The disk structure is corrupt and non-readable. }
  ERROR_DISK_CORRUPT : Result:= '';
  { There is no user session key for the specified logon session. }
  ERROR_NO_USER_SESSION_KEY : Result:= '';
  { The service being accessed is licensed for a particular number of connections. }
  { No more connections can be made to the service at this time }
  { because there are already as many connections as the service can accept. }
  ERROR_LICENSE_QUOTA_EXCEEDED : Result:= '';
  { Invalid window handle. }
  ERROR_INVALID_WINDOW_HANDLE : Result:= '';
  { Invalid menu handle. }
  ERROR_INVALID_MENU_HANDLE : Result:= '';
  { Invalid cursor handle. }
  ERROR_INVALID_CURSOR_HANDLE : Result:= '';
  { Invalid accelerator table handle. }
  ERROR_INVALID_ACCEL_HANDLE : Result:= '';
  { Invalid hook handle. }
  ERROR_INVALID_HOOK_HANDLE : Result:= '';
  { Invalid handle to a multiple-window position structure. }
  ERROR_INVALID_DWP_HANDLE : Result:= '';
  { Cannot create a top-level child window. }
  ERROR_TLW_WITH_WSCHILD : Result:= '';
  { Cannot find window class. }
  ERROR_CANNOT_FIND_WND_CLASS : Result:= '';
  { Invalid window, belongs to other thread. }
  ERROR_WINDOW_OF_OTHER_THREAD : Result:= '';
  { Hot key is already registered. }
  ERROR_HOTKEY_ALREADY_REGISTERED : Result:= '';
  { Class already exists. }
  ERROR_CLASS_ALREADY_EXISTS : Result:= '';
  { Class does not exist. }
  ERROR_CLASS_DOES_NOT_EXIST : Result:= '';
  { Class still has open windows. }
  ERROR_CLASS_HAS_WINDOWS : Result:= '';
  { Invalid index. }
  ERROR_INVALID_INDEX : Result:= '';
  { Invalid icon handle. }
  ERROR_INVALID_ICON_HANDLE : Result:= '';
  { Using private DIALOG window words. }
  ERROR_PRIVATE_DIALOG_INDEX : Result:= '';
  { The listbox identifier was not found. }
  ERROR_LISTBOX_ID_NOT_FOUND : Result:= '';
  { No wildcards were found. }
  ERROR_NO_WILDCARD_CHARACTERS : Result:= '';
  { Thread does not have a clipboard open. }
  ERROR_CLIPBOARD_NOT_OPEN : Result:= '';
  { Hot key is not registered. }
  ERROR_HOTKEY_NOT_REGISTERED : Result:= '';
  { The window is not a valid dialog window. }
  ERROR_WINDOW_NOT_DIALOG : Result:= '';
  { Control ID not found. }
  ERROR_CONTROL_ID_NOT_FOUND : Result:= '';
  { Invalid message for a combo box because it does not have an edit control. }
  ERROR_INVALID_COMBOBOX_MESSAGE : Result:= '';
  { The window is not a combo box. }
  ERROR_WINDOW_NOT_COMBOBOX : Result:= '';
  { Height must be less than 256. }
  ERROR_INVALID_EDIT_HEIGHT : Result:= '';
  { Invalid device context (DC) handle. }
  ERROR_DC_NOT_FOUND : Result:= '';
  { Invalid hook procedure type. }
  ERROR_INVALID_HOOK_FILTER : Result:= '';
  { Invalid hook procedure. }
  ERROR_INVALID_FILTER_PROC : Result:= '';
  { Cannot set non-local hook without a module handle. }
  ERROR_HOOK_NEEDS_HMOD : Result:= '';
  { This hook procedure can only be set globally. }
  ERROR_GLOBAL_ONLY_HOOK : Result:= '';
 { The journal hook procedure is already installed. }
  ERROR_JOURNAL_HOOK_SET : Result:= '';
  { The hook procedure is not installed. }
  ERROR_HOOK_NOT_INSTALLED : Result:= '';
  { Invalid message for single-selection listbox. }
  ERROR_INVALID_LB_MESSAGE : Result:= '';
  { LB_SETCOUNT sent to non-lazy listbox. }
  ERROR_SETCOUNT_ON_BAD_LB : Result:= '';
  { This list box does not support tab stops. }
  ERROR_LB_WITHOUT_TABSTOPS : Result:= '';
  { Cannot destroy object created by another thread. }
  ERROR_DESTROY_OBJECT_OF_OTHER_THREAD : Result:= '';
  { Child windows cannot have menus. }
  ERROR_CHILD_WINDOW_MENU : Result:= '';
  { The window does not have a system menu. }
  ERROR_NO_SYSTEM_MENU : Result:= '';
  { Invalid message box style. }
  ERROR_INVALID_MSGBOX_STYLE : Result:= '';
  { Invalid system-wide (SPI_*) parameter. }
  ERROR_INVALID_SPI_VALUE : Result:= '';
  { Screen already locked. }
  ERROR_SCREEN_ALREADY_LOCKED : Result:= '';
  { All handles to windows in a multiple-window position structure must }
  { have the same parent. }
  ERROR_HWNDS_HAVE_DIFF_PARENT : Result:= '';
  { The window is not a child window. }
  ERROR_NOT_CHILD_WINDOW : Result:= '';
  { Invalid GW_* command. }
  ERROR_INVALID_GW_COMMAND : Result:= '';
  { Invalid thread identifier. }
  ERROR_INVALID_THREAD_ID : Result:= '';
  { Cannot process a message from a window that is not a multiple document }
  { interface (MDI) window. }
  ERROR_NON_MDICHILD_WINDOW : Result:= '';
  { Popup menu already active. }
  ERROR_POPUP_ALREADY_ACTIVE : Result:= '';
  { The window does not have scroll bars. }
  ERROR_NO_SCROLLBARS : Result:= '';
  { Scroll bar range cannot be greater than $7FFF. }
  ERROR_INVALID_SCROLLBAR_RANGE : Result:= '';
  { Cannot show or remove the window in the way specified. }
  ERROR_INVALID_SHOWWIN_COMMAND : Result:= '';
  { Insufficient system resources exist to complete the requested service. }
  ERROR_NO_SYSTEM_RESOURCES : Result:= '';
  { Insufficient system resources exist to complete the requested service. }
  ERROR_NONPAGED_SYSTEM_RESOURCES : Result:= '';
  { Insufficient system resources exist to complete the requested service. }
  ERROR_PAGED_SYSTEM_RESOURCES : Result:= '';
  { Insufficient quota to complete the requested service. }
  ERROR_WORKING_SET_QUOTA : Result:= '';
  { Insufficient quota to complete the requested service. }
  ERROR_PAGEFILE_QUOTA : Result:= '';
  { The paging file is too small for this operation to complete. }
  ERROR_COMMITMENT_LIMIT : Result:= '';
  { A menu item was not found. }
  ERROR_MENU_ITEM_NOT_FOUND : Result:= '';
  { Invalid keyboard layout handle. }
  ERROR_INVALID_KEYBOARD_HANDLE : Result:= '';
  { Hook type not allowed. }
  ERROR_HOOK_TYPE_NOT_ALLOWED : Result:= '';
  { This operation requires an interactive windowstation. }
  ERROR_REQUIRES_INTERACTIVE_WINDOWSTATION : Result:= '';
  { This operation returned because the timeout period expired. }
  ERROR_TIMEOUT : Result:= '';
  { The event log file is corrupt. }
  ERROR_EVENTLOG_FILE_CORRUPT : Result:= '';
  { No event log file could be opened, so the event logging service did not start. }
  ERROR_EVENTLOG_CANT_START : Result:= '';
  { The event log file is full. }
  ERROR_LOG_FILE_FULL : Result:= '';
  { The event log file has changed between reads. }
  ERROR_EVENTLOG_FILE_CHANGED : Result:= '';
  { The string binding is invalid. }
  RPC_S_INVALID_STRING_BINDING : Result:= '';
  { The binding handle is not the correct type. }
  RPC_S_WRONG_KIND_OF_BINDING : Result:= '';
  { The binding handle is invalid. }
  RPC_S_INVALID_BINDING : Result:= '';
  { The RPC protocol sequence is not supported. }
  RPC_S_PROTSEQ_NOT_SUPPORTED : Result:= '';
  { The RPC protocol sequence is invalid. }
  RPC_S_INVALID_RPC_PROTSEQ : Result:= '';
  { The string universal unique identifier (UUID) is invalid. }
  RPC_S_INVALID_STRING_UUID : Result:= '';
  { The endpoint format is invalid. }
  RPC_S_INVALID_ENDPOINT_FORMAT : Result:= '';
  { The network address is invalid. }
  RPC_S_INVALID_NET_ADDR : Result:= '';
  { No endpoint was found. }
  RPC_S_NO_ENDPOINT_FOUND : Result:= '';
  { The timeout value is invalid. }
  RPC_S_INVALID_TIMEOUT : Result:= '';
  { The object universal unique identifier (UUID) was not found. }
  RPC_S_OBJECT_NOT_FOUND : Result:= '';
  { The object universal unique identifier (UUID) has already been registered. }
  RPC_S_ALREADY_REGISTERED : Result:= '';
  { The type universal unique identifier (UUID) has already been registered. }
  RPC_S_TYPE_ALREADY_REGISTERED : Result:= '';
  { The RPC server is already listening. }
  RPC_S_ALREADY_LISTENING : Result:= '';
  { No protocol sequences have been registered. }
  RPC_S_NO_PROTSEQS_REGISTERED : Result:= '';
  { The RPC server is not listening. }
  RPC_S_NOT_LISTENING : Result:= '';
  { The manager type is unknown. }
  RPC_S_UNKNOWN_MGR_TYPE : Result:= '';
  { The interface is unknown. }
  RPC_S_UNKNOWN_IF : Result:= '';
  { There are no bindings. }
  RPC_S_NO_BINDINGS : Result:= '';
  { There are no protocol sequences. }
  RPC_S_NO_PROTSEQS : Result:= '';
  { The endpoint cannot be created. }
  RPC_S_CANT_CREATE_ENDPOINT : Result:= '';
  { Not enough resources are available to complete this operation. }
  RPC_S_OUT_OF_RESOURCES : Result:= '';
  { The RPC server is unavailable. }
  RPC_S_SERVER_UNAVAILABLE : Result:= '';
  { The RPC server is too busy to complete this operation. }
  RPC_S_SERVER_TOO_BUSY : Result:= '';
  { The network options are invalid. }
  RPC_S_INVALID_NETWORK_OPTIONS : Result:= '';
  { There is not a remote procedure call active in this thread. }
  RPC_S_NO_CALL_ACTIVE : Result:= '';
  { The remote procedure call failed. }
  RPC_S_CALL_FAILED : Result:= '';
  { The remote procedure call failed and did not execute. }
  RPC_S_CALL_FAILED_DNE : Result:= '';
  { A remote procedure call (RPC) protocol error occurred. }
  RPC_S_PROTOCOL_ERROR : Result:= '';
  { The transfer syntax is not supported by the RPC server. }
  RPC_S_UNSUPPORTED_TRANS_SYN : Result:= '';
  { The universal unique identifier (UUID) type is not supported. }
  RPC_S_UNSUPPORTED_TYPE : Result:= '';
  { The tag is invalid. }
  RPC_S_INVALID_TAG : Result:= '';
  { The array bounds are invalid. }
  RPC_S_INVALID_BOUND : Result:= '';
  { The binding does not contain an entry name. }
  RPC_S_NO_ENTRY_NAME : Result:= '';
  { The name syntax is invalid. }
  RPC_S_INVALID_NAME_SYNTAX : Result:= '';
  { The name syntax is not supported. }
  RPC_S_UNSUPPORTED_NAME_SYNTAX : Result:= '';
  { No network address is available to use to construct a universal }
  { unique identifier (UUID). }
  RPC_S_UUID_NO_ADDRESS : Result:= '';
  { The endpoint is a duplicate. }
  RPC_S_DUPLICATE_ENDPOINT : Result:= '';
  { The authentication type is unknown. }
  RPC_S_UNKNOWN_AUTHN_TYPE : Result:= '';
  { The maximum number of calls is too small. }
  RPC_S_MAX_CALLS_TOO_SMALL : Result:= '';
  { The string is too long. }
  RPC_S_STRING_TOO_LONG : Result:= '';
  { The RPC protocol sequence was not found. }
  RPC_S_PROTSEQ_NOT_FOUND : Result:= '';
  { The procedure number is out of range. }
  RPC_S_PROCNUM_OUT_OF_RANGE : Result:= '';
  { The binding does not contain any authentication information. }
  RPC_S_BINDING_HAS_NO_AUTH : Result:= '';
  { The authentication service is unknown. }
  RPC_S_UNKNOWN_AUTHN_SERVICE : Result:= '';
  { The authentication level is unknown. }
  RPC_S_UNKNOWN_AUTHN_LEVEL : Result:= '';
  { The security context is invalid. }
  RPC_S_INVALID_AUTH_IDENTITY : Result:= '';
  { The authorization service is unknown. }
  RPC_S_UNKNOWN_AUTHZ_SERVICE : Result:= '';
  { The entry is invalid. }
  EPT_S_INVALID_ENTRY : Result:= '';
  { The server endpoint cannot perform the operation. }
  EPT_S_CANT_PERFORM_OP : Result:= '';
  { There are no more endpoints available from the endpoint mapper. }
  EPT_S_NOT_REGISTERED : Result:= '';
  { No interfaces have been exported. }
  RPC_S_NOTHING_TO_EXPORT : Result:= '';
  { The entry name is incomplete. }
  RPC_S_INCOMPLETE_NAME : Result:= '';
  { The version option is invalid. }
  RPC_S_INVALID_VERS_OPTION : Result:= '';
  { There are no more members. }
  RPC_S_NO_MORE_MEMBERS : Result:= '';
  { There is nothing to unexport. }
  RPC_S_NOT_ALL_OBJS_UNEXPORTED : Result:= '';
  { The interface was not found. }
  RPC_S_INTERFACE_NOT_FOUND : Result:= '';
  { The entry already exists. }
  RPC_S_ENTRY_ALREADY_EXISTS : Result:= '';
  { The entry is not found. }
  RPC_S_ENTRY_NOT_FOUND : Result:= '';
  { The name service is unavailable. }
  RPC_S_NAME_SERVICE_UNAVAILABLE : Result:= '';
  { The network address family is invalid. }
  RPC_S_INVALID_NAF_ID : Result:= '';
  { The requested operation is not supported. }
  RPC_S_CANNOT_SUPPORT : Result:= '';
  { No security context is available to allow impersonation. }
  RPC_S_NO_CONTEXT_AVAILABLE : Result:= '';
  { An internal error occurred in a remote procedure call (RPC). }
  RPC_S_INTERNAL_ERROR : Result:= '';
  { The RPC server attempted an integer division by zero. }
  RPC_S_ZERO_DIVIDE : Result:= '';
  { An addressing error occurred in the RPC server. }
  RPC_S_ADDRESS_ERROR : Result:= '';
  { A floating-point operation at the RPC server caused a division by zero. }
  RPC_S_FP_DIV_ZERO : Result:= '';
  { A floating-point underflow occurred at the RPC server. }
  RPC_S_FP_UNDERFLOW : Result:= '';
  { A floating-point overflow occurred at the RPC server. }
  RPC_S_FP_OVERFLOW : Result:= '';
  { The list of RPC servers available for the binding of auto handles }
  { has been exhausted. }
  RPC_X_NO_MORE_ENTRIES : Result:= '';
  { Unable to open the character translation table file. }
  RPC_X_SS_CHAR_TRANS_OPEN_FAIL : Result:= '';
  { The file containing the character translation table has fewer than }
  { 512 bytes. }
  RPC_X_SS_CHAR_TRANS_SHORT_FILE : Result:= '';
  { A null context handle was passed from the client to the host during }
  { a remote procedure call. }
  RPC_X_SS_IN_NULL_CONTEXT : Result:= '';
  { The context handle changed during a remote procedure call. }
  RPC_X_SS_CONTEXT_DAMAGED : Result:= '';
  { The binding handles passed to a remote procedure call do not match. }
  RPC_X_SS_HANDLES_MISMATCH : Result:= '';
  { The stub is unable to get the remote procedure call handle. }
  RPC_X_SS_CANNOT_GET_CALL_HANDLE : Result:= '';
  { A null reference pointer was passed to the stub. }
  RPC_X_NULL_REF_POINTER : Result:= '';
  { The enumeration value is out of range. }
  RPC_X_ENUM_VALUE_OUT_OF_RANGE : Result:= '';
  { The byte count is too small. }
  RPC_X_BYTE_COUNT_TOO_SMALL : Result:= '';
  { The stub received bad data. }
  RPC_X_BAD_STUB_DATA : Result:= '';
  { The supplied user buffer is not valid for the requested operation. }
  ERROR_INVALID_USER_BUFFER : Result:= '';
  { The disk media is not recognized.  It may not be formatted. }
  ERROR_UNRECOGNIZED_MEDIA : Result:= '';
  { The workstation does not have a trust secret. }
  ERROR_NO_TRUST_LSA_SECRET : Result:= '';
  { The SAM database on the Windows NT Server does not have a computer }
  { account for this workstation trust relationship. }
  ERROR_NO_TRUST_SAM_ACCOUNT : Result:= '';
  { The trust relationship between the primary domain and the trusted }
  { domain failed. }
  ERROR_TRUSTED_DOMAIN_FAILURE : Result:= '';
  { The trust relationship between this workstation and the primary }
  { domain failed. }
  ERROR_TRUSTED_RELATIONSHIP_FAILURE : Result:= '';
  { The network logon failed. }
  ERROR_TRUST_FAILURE : Result:= '';
  { A remote procedure call is already in progress for this thread. }
  RPC_S_CALL_IN_PROGRESS : Result:= '';
  { An attempt was made to logon, but the network logon service was not started. }
  ERROR_NETLOGON_NOT_STARTED : Result:= '';
  { The user's account has expired. }
  ERROR_ACCOUNT_EXPIRED : Result:= '';
  { The redirector is in use and cannot be unloaded. }
  ERROR_REDIRECTOR_HAS_OPEN_HANDLES : Result:= '';
  { The specified printer driver is already installed. }
  ERROR_PRINTER_DRIVER_ALREADY_INSTALLED : Result:= '';
  { The specified port is unknown. }
  ERROR_UNKNOWN_PORT : Result:= '';
  { The printer driver is unknown. }
  ERROR_UNKNOWN_PRINTER_DRIVER : Result:= '';
  { The print processor is unknown. }
  ERROR_UNKNOWN_PRINTPROCESSOR : Result:= '';
  { The specified separator file is invalid. }
  ERROR_INVALID_SEPARATOR_FILE : Result:= '';
  { The specified priority is invalid. }
  ERROR_INVALID_PRIORITY : Result:= '';
  { The printer name is invalid. }
  ERROR_INVALID_PRINTER_NAME : Result:= '';
  { The printer already exists. }
  ERROR_PRINTER_ALREADY_EXISTS : Result:= '';
  { The printer command is invalid. }
  ERROR_INVALID_PRINTER_COMMAND : Result:= '';
  { The specified datatype is invalid. }
  ERROR_INVALID_DATATYPE : Result:= '';
  { The Environment specified is invalid. }
  ERROR_INVALID_ENVIRONMENT : Result:= '';
  { There are no more bindings. }
  RPC_S_NO_MORE_BINDINGS : Result:= '';
  { The account used is an interdomain trust account.  Use your global user account or local user account to access this server. }
  ERROR_NOLOGON_INTERDOMAIN_TRUST_ACCOUNT : Result:= '';
  { The account used is a Computer Account.  Use your global user account or local user account to access this server. }
  ERROR_NOLOGON_WORKSTATION_TRUST_ACCOUNT : Result:= '';
  { The account used is an server trust account.  Use your global user account or local user account to access this server. }
  ERROR_NOLOGON_SERVER_TRUST_ACCOUNT : Result:= '';
  { The name or security ID (SID) of the domain specified is inconsistent }
  { with the trust information for that domain. }
  ERROR_DOMAIN_TRUST_INCONSISTENT : Result:= '';
  { The server is in use and cannot be unloaded. }
  ERROR_SERVER_HAS_OPEN_HANDLES : Result:= '';
  { The specified image file did not contain a resource section. }
  ERROR_RESOURCE_DATA_NOT_FOUND : Result:= '';
  { The specified resource type can not be found in the image file. }
  ERROR_RESOURCE_TYPE_NOT_FOUND : Result:= '';
  { The specified resource name can not be found in the image file. }
  ERROR_RESOURCE_NAME_NOT_FOUND : Result:= '';
  { The specified resource language ID cannot be found in the image file. }
  ERROR_RESOURCE_LANG_NOT_FOUND : Result:= '';
  { Not enough quota is available to process this command. }
  ERROR_NOT_ENOUGH_QUOTA : Result:= '';
  { No interfaces have been registered. }
  RPC_S_NO_INTERFACES : Result:= '';
  { The server was altered while processing this call. }
  RPC_S_CALL_CANCELLED : Result:= '';
  { The binding handle does not contain all required information. }
  RPC_S_BINDING_INCOMPLETE : Result:= '';
  { Communications failure. }
  RPC_S_COMM_FAILURE : Result:= '';
  { The requested authentication level is not supported. }
  RPC_S_UNSUPPORTED_AUTHN_LEVEL : Result:= '';
  { No principal name registered. }
  RPC_S_NO_PRINC_NAME : Result:= '';
  { The error specified is not a valid Windows NT RPC error code. }
  RPC_S_NOT_RPC_ERROR : Result:= '';
  { A UUID that is valid only on this computer has been allocated. }
  RPC_S_UUID_LOCAL_ONLY : Result:= '';
  { A security package specific error occurred. }
  RPC_S_SEC_PKG_ERROR : Result:= '';
  { Thread is not cancelled. }
  RPC_S_NOT_CANCELLED : Result:= '';
  { Invalid operation on the encoding/decoding handle. }
  RPC_X_INVALID_ES_ACTION : Result:= '';
  { Incompatible version of the serializing package. }
  RPC_X_WRONG_ES_VERSION : Result:= '';
  { Incompatible version of the RPC stub. }
  RPC_X_WRONG_STUB_VERSION : Result:= '';
  { The idl pipe object is invalid or corrupted. }
  RPC_X_INVALID_PIPE_OBJECT : Result:= '';
  { The operation is invalid for a given idl pipe object. }
  RPC_X_INVALID_PIPE_OPERATION : Result:= '';
  { The idl pipe version is not supported. }
  RPC_X_WRONG_PIPE_VERSION : Result:= '';
  { The group member was not found. }
  RPC_S_GROUP_MEMBER_NOT_FOUND : Result:= '';
  { The endpoint mapper database could not be created. }
  EPT_S_CANT_CREATE : Result:= '';
  { The object universal unique identifier (UUID) is the nil UUID. }
  RPC_S_INVALID_OBJECT : Result:= '';
  { The specified time is invalid. }
  ERROR_INVALID_TIME : Result:= '';
  { The specified Form name is invalid. }
  ERROR_INVALID_FORM_NAME : Result:= '';
  { The specified Form size is invalid }
  ERROR_INVALID_FORM_SIZE : Result:= '';
  { The specified Printer handle is already being waited on }
  ERROR_ALREADY_WAITING : Result:= '';
  { The specified Printer has been deleted }
  ERROR_PRINTER_DELETED : Result:= '';
  { The state of the Printer is invalid }
  ERROR_INVALID_PRINTER_STATE : Result:= '';
  { The user must change his password before he logs on the first time. }
  ERROR_PASSWORD_MUST_CHANGE : Result:= '';
  { Could not find the domain controller for this domain. }
  ERROR_DOMAIN_CONTROLLER_NOT_FOUND : Result:= '';
  { The referenced account is currently locked out and may not be logged on to. }
  ERROR_ACCOUNT_LOCKED_OUT : Result:= '';
  { The object exporter specified was not found. }
  OR_INVALID_OXID : Result:= '';
  { The object specified was not found. }
  OR_INVALID_OID : Result:= '';
  { The object resolver set specified was not found. }
  OR_INVALID_SET : Result:= '';
  { Some data remains to be sent in the request buffer. }
  RPC_S_SEND_INCOMPLETE : Result:= '';
  { The list of servers for this workgroup is not currently available }
  ERROR_NO_BROWSER_SERVERS_FOUND : Result:= '';
  { The pixel format is invalid. }
  ERROR_INVALID_PIXEL_FORMAT : Result:= '';
  { The specified driver is invalid. }
  ERROR_BAD_DRIVER : Result:= '';
  { The window style or class attribute is invalid for this operation. }
  ERROR_INVALID_WINDOW_STYLE : Result:= '';
  { The requested metafile operation is not supported. }
  ERROR_METAFILE_NOT_SUPPORTED : Result:= '';
  { The requested transformation operation is not supported. }
  ERROR_TRANSFORM_NOT_SUPPORTED : Result:= '';
  { The requested clipping operation is not supported. }
  ERROR_CLIPPING_NOT_SUPPORTED : Result:= '';
  { The specified print monitor is unknown. }
  ERROR_UNKNOWN_PRINT_MONITOR : Result:= '';
  { The specified printer driver is currently in use. }
  ERROR_PRINTER_DRIVER_IN_USE : Result:= '';
  { The spool file was not found. }
  ERROR_SPOOL_FILE_NOT_FOUND : Result:= '';
  { A StartDocPrinter call was not issued. }
  ERROR_SPL_NO_STARTDOC : Result:= '';
  { An AddJob call was not issued. }
  ERROR_SPL_NO_ADDJOB : Result:= '';
  { The specified print processor has already been installed. }
  ERROR_PRINT_PROCESSOR_ALREADY_INSTALLED : Result:= '';
  { The specified print monitor has already been installed. }
  ERROR_PRINT_MONITOR_ALREADY_INSTALLED : Result:= '';
  { The specified print monitor does not have the required functions. }
  ERROR_INVALID_PRINT_MONITOR : Result:= '';
  { The specified print monitor is currently in use. }
  ERROR_PRINT_MONITOR_IN_USE : Result:= '';
  { The requested operation is not allowed when there are jobs queued to the printer. }
  ERROR_PRINTER_HAS_JOBS_QUEUED : Result:= '';
  { The requested operation is successful.  Changes will not be effective until the system is rebooted. }
  ERROR_SUCCESS_REBOOT_REQUIRED : Result:= '';
  { The requested operation is successful.  Changes will not be effective until the service is restarted. }
  ERROR_SUCCESS_RESTART_REQUIRED : Result:= '';
  { WINS encountered an error while processing the command. }
  ERROR_WINS_INTERNAL : Result:= '';
  { The local WINS can not be deleted. }
  ERROR_CAN_NOT_DEL_LOCAL_WINS : Result:= '';
  { The importation from the file failed. }
  ERROR_STATIC_INIT : Result:= '';
  { The backup Failed.  Was a full backup done before ? }
  ERROR_INC_BACKUP : Result:= '';
  { The backup Failed.  Check the directory that you are backing the database to. }
  ERROR_FULL_BACKUP : Result:= '';
  { The name does not exist in the WINS database. }
  ERROR_REC_NON_EXISTENT : Result:= '';
  { Replication with a non-configured partner is not allowed. }
  ERROR_RPL_NOT_ALLOWED : Result:= '';
  else Result:= 'Unknown error.';
  end;
end;

Procedure StartApplication(Name, LoadFile, Params : string; Show : boolean;
                           var PI : TProcessInformation);
var SI: TStartupInfo;
    Path, FN, Ext, FileName : string;
    ExeName, Prms : PChar;
    PCPath, PCFN : PChar;
    res : integer;
    OK : boolean;
begin
  PCPath:= nil;
  if LoadFile <> '' then
  begin
    ExtrFileName(LoadFile, Path, FN, Ext);
    if Length(FN) > 8 then FN:= Copy(FN, 1, 6) + '~1';
    PCPath:= PChar(Path);
    res:= GetShortPathName(PCPath, PCPath, 255);
    if res = 0 then
    begin
      MessageDlg(GetLastErrorString, mtError, [mbOK], 0);
      Exit;
    end
    else
    begin
      FileName:= '"' + StrPas(PCPath) + FN + Ext + '"';
      PCFN:= PChar(FileName);
    end;
  end;
  FillChar(SI, SizeOf(SI), 0);
  with SI do
  begin
    lpReserved:= nil;
    lpDesktop:= nil;
    lpTitle:= nil;
    dwX:= 0; dwY:= 0;
    dwXSize:= 600; dwYSize:= 725;
    dwXCountChars:= 0; dwYCountChars:= 0;
    dwFillAttribute:= 0;
    dwFlags:= STARTF_USESHOWWINDOW;
    if Show then wShowWindow:= SW_SHOWNORMAL
    else wShowWindow:= sw_Hide;
    cbReserved2:= 0;
    lpReserved2:= nil;
    hStdInput:= 0;
    hStdOutput:= 0;
    hStdError:= 0;
  end;
  SI.cb := SizeOf(SI);

  ExeName:= PChar(Name);
  if (LoadFile <> '') and (Params <> '') then StrCat(PCFN, PChar(' ' + Params))
  else if (LoadFile <> '') then Prms:= PChar(PCFN)
  else if (Params <> '') then Prms:= PChar(Params)
  else Prms:= nil;

  OK:= CreateProcess(ExeName, Prms,
                      nil, nil, True, NORMAL_PRIORITY_CLASS, nil, PCPath, SI, PI);
  if not OK then MessageDlg(GetLastErrorString, mtError, [mbOK], 0);
end;

Procedure StartAppl(Name, LoadFile : string);
var res : integer;
    Reg : TRegistry;
    ExeName, FileName, Param : string;
    Path, FN, Ext : string;
    PCPath : PChar;
begin
  Param:= '';
  if LoadFile <> '' then
  begin
    FileName:= '"' + LoadFile + '"';

    ExtrFileName(LoadFile, Path, FN, Ext);
    Reg:= TRegistry.Create;
    with Reg do
    begin
      try
        RootKey:= HKEY_CLASSES_ROOT;
        if UpperCase(Ext) = '.TXT' then
          ExeName:= 'WordPad.Document.1\DefaultIcon'
        else if (UpperCase(Ext) = '.CSV') or (Uppercase(Ext) = '.XLS') then
          ExeName:= 'Excel.CSV\Shell\Open\command'
        else if UpperCase(Ext) = '.DOC' then
          ExeName:= 'Word.Document.8\Shell\Open\command'
        else if UpperCase(Ext) = '.MDB' then
          ExeName:= 'Access.Application.8\Shell\Open\command';

        OpenKey(ExeName, false);
        ExeName:= ReadString('');

        if UpperCase(Ext) = '.TXT' then
        begin
          res:= Pos(',', ExeName);
          if Res > 0 then ExeName:= Copy(ExeName, 1, Res - 1);
          if Pos('.EXE', UpperCase(ExeName)) = 0 then ExeName:= ExeName + '.exe';
        end
        else
        begin
          res:= Pos('/', ExeName);
          if Res > 0 then Param:= copy(ExeName, res, Length(ExeName) - res + 1);
          res:= Pos('" ', ExeName);
          if Res > 0 then ExeName:= Copy(ExeName, 1, Res);
          if ExeName[1] = '"' then ExeName:= copy(ExeName, 2, Length(ExeName) - 2);
        end;
      except
        MessageDlg(GetLastErrorString, mtError, [mbOK], 0);
      end;
      free;
    end;
  end;
  Path:= ExtractFilePath(ExeName);
  if Path[1] = '"' then Path:= Copy(Path, 2, Length(Path) - 1);
  if Path[Length(Path)] = '"' then Path:= Copy(Path, 1, Length(Path) - 1);
  PCPath:= PChar(Path);

  if Param <> '' then FileName:= FileName + ' ' + Param;
  try
    res:= ShellExecute(Application.Handle, PChar('open'),
                       PChar(ExeName), PChar(FileName), PCPath, SW_Show);
    if res <= 32 then MessageDlg(GetLastErrorString, mtError, [mbOK], 0);
  except
    MessageDlg(GetLastErrorString, mtError, [mbOK], 0);
  end;
end;

Procedure Send_e_mail(sTo, sCC, sBCC, Subject : string);
var S : string;
begin
  S:= '';
  if sTo <> '' then
  begin
    S:= 'mailto:' + sTo;
    S:= S + '?Subject=' + Subject;
    if sCC <> '' then S:= S + '&cc=' + sCC;
    if sBCC <> '' then S:= S + '&bcc=' + sBCC;
    ShellExecute(0, nil, PChar(S), nil, nil, SW_SHOWDEFAULT);
  end;
end;

Procedure OpenWebPage(Address : string);
begin
  if (Lowercase(copy(Address, 1, 3)) = 'www') or
     (lowercase(copy(address, 1, 7)) = 'http://') then
  begin
    ShellExecute(0, nil, PChar(Address), nil, nil, SW_SHOWDEFAULT);
  end;
end;

Procedure OpenFile(FileName : string);
begin
  ShellExecute(0, nil, PChar(FileName), nil, nil, SW_SHOWDEFAULT);
end;


function Fixed_DateTimeToFileDate(DateTime: TDateTime): Integer;
{ same as DateTimeToFileDate, but year limit changed to 2107 }
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  DecodeDate(DateTime, Year, Month, Day);
  if (Year < 1980) or (Year > 2107) then Result := 0 else
  begin
    DecodeTime(DateTime, Hour, Min, Sec, MSec);
    LongRec(Result).Lo := (Sec shr 1) or (Min shl 5) or (Hour shl 11);
    LongRec(Result).Hi := Day or (Month shl 5) or ((Year - 1980) shl 9);
  end;
end;

begin

end.



