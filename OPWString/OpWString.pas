Unit OpWString;

// 271099 Eerste versie.

Interface

Uses SysUtils, System.StrUtils, Math,  RegularExpressions, System.classes;

Type
  CharSet = TSysCharSet;
   Str19 = String[19];

Function ExtractWord(N: Byte; S: String; WordDelims: CharSet;
  var Len: Integer): String;
{ -Given a set of word delimiters, return the N'th word in S.
  On returen Len is the lenght of the N'th word in S (0 if not found) }

Function WordPosition(N: Byte; S: String; WordDelims: CharSet): Integer;
{ -Given a set of word delimiters, return start position of N'th word in S }

function WordCount(S: string; WordDelims: CharSet): Byte;
{ -Given a set of word delimiters, return number of words in S }

function JustName(PathName: string): string;

Function GetAdoGetal(r: Real): Str19;

function Str2Int(S : string; var I : Integer) : Boolean;
  {-Convert a string to an integer, returning true if successful}

function Str2Word(S : string; var I : Word) : Boolean;
  {-Convert a string to a word, returning true if successful}

function Str2Long(S : string; var I : LongInt) : Boolean;
  {-Convert a string to an longint, returning true if successful}

function Str2Double(S : string; var R : Double) : Boolean;
  {-Convert a string to a Double, returning true if successful}

Function GetSubString( const InString, delim1S, delim2S: string; var S: String ): Boolean;
  {-True if a substring could be extracted from InString. Uses delimiters delim1S en delim2. Result is in S}

Function GetNextText( var f: TextFile; delim1S, delim2S: string;
  var i: integer; var InString, S: String ): Boolean;
  {-Ref. GetSubString. Text is retreived from file f. LineNr i is increased.}

Function GetMonthNr( const S: String  ): Integer;
  {-Returns month number (1-12); Dutch/English Language codes are recognised.
  https://msdn.microsoft.com/en-us/library/ee825488(v=cs.20).aspx}

Function OStripAccents(const aStr: String): String;
  {-Convert Hi-Ansi chars to Ascii equivalent (é -> e)}

Function HTML2USASCIIString(const aStr: String): String;

function CountChar(const s: string; const c: char): integer;

{ Returns a count of the number of occurences of SubText in Text }
function CountOccurences( const SubText: string;
                          const Text: string): Integer;

Procedure RemoveEmptyLinesFromStringList( var aList: TStringList );

Implementation

Function GetAdoGetal(r: Real): Str19;
var
  e, i, j: Integer;
  s2: String[2];
  w: Char;
  s17: String[17];
begin
  if r < 0 then
    w := '-'
  else
    w := ' '; { bepaal teken }

  r := abs(r); { bepaal waarde en e-macht }
  e := 0;
  if r <> 0 then
  begin
    while r >= 0.1 do
    begin
      r := r / 10;
      inc(e);
    end;
    while r < 0.1 do
    begin
      r := r * 10;
      dec(e);
    end;
  end;
  str(r: 1: 12, s17);

  val(copy(s17, 1, 1), i, j); { test i.v.m. mogelijke afrondingsfout }
  if i >= 1 then
  begin
    inc(e);
    r := r / 10;
    str(r: 1: 12, s17);
  end;

  if e < 0.0 then
    s2 := 'E-'
  else
    s2 := 'E+'; { bepaal teken e-macht }

  e := abs(e); { vul string }
  if e > 99 then
  begin
    e := 99;
    s17 := concat('.999999999999', s2);
  end
  else
    s17 := concat('.', copy(s17, 3, 12), s2);
  str(e: 2, s2);
  if e < 10 then
    s2 := concat('0', copy(s2, 2, 1));
  s17 := concat(s17, s2);

  GetAdoGetal := w + '0' + s17;
end;

function WordPosition(N: Byte; S: string; WordDelims: CharSet): Integer;
{ -Given a set of word delimiters, return start position of N'th word in S }
var
  i, Count, SLen: Integer;
begin
  SLen := Length(S);
  Count := 0;
  i := 1;
  WordPosition := 0;

  while (i <= SLen) and (Count <> N) do
  begin
    { skip over delimiters }
    while (i <= SLen) and CharInSet(S[i], WordDelims) do
      inc(i);

    { if we're not beyond end of S, we're at the start of a word }
    if i <= SLen then
      inc(Count);

    { if not finished, find the end of the current word }
    if Count <> N then
      while (i <= SLen) and not(CharInSet(S[i], WordDelims)) do
        inc(i)
    else
      WordPosition := i;
  end;
end;

function ExtractWord(N: Byte; S: string; WordDelims: CharSet;
  var Len: Integer): string;
{ -Given a set of word delimiters, return the N'th word in S }
var
  i, SLen: Integer;
  ResultStr: String;
begin
  SLen := Length(S);
  SetLength(ResultStr, SLen);
  Len := 0;
  i := WordPosition(N, S, WordDelims);

  if i <> 0 then
    { find the end of the current word }
    while (i <= SLen) and not(CharInSet(S[i], WordDelims)) do
    begin
      { add the I'th character to result }
      inc(Len);
      ResultStr[Len] := S[i];
      inc(i);
    end;
  SetLength(ResultStr, Len);
  ExtractWord := ResultStr;
end;

function JustName(PathName: string): string;
{ -Return just the name (no extension, no path) of a pathname }
var
  DotPos: Byte;
begin
  PathName := ExtractFileName(PathName);
  DotPos := Pos('.', PathName);
  if DotPos > 0 then
    PathName := copy(PathName, 1, DotPos - 1);
  JustName := PathName;
end;

function WordCount(S: string; WordDelims: CharSet): Byte;
{ -Given a set of word delimiters, return number of words in S }
var
  i, Count: Byte;
  SLen: Byte absolute S;
begin
  Count := 0;
  i := 1;

  while i <= SLen do
  begin
    { skip over delimiters }
    while (i <= SLen) and CharInSet(S[i], WordDelims) do
      inc(i);

    { if we're not beyond end of S, we're at the start of a word }
    if i <= SLen then
      inc(Count);

    { find the end of the current word }
    while (i <= SLen) and not(CharInSet(S[i], WordDelims)) do
      inc(i);
  end;

  WordCount := Count;
end;

  function Str2Int(S : string; var I : Integer) : Boolean;
    {-Convert a string to an integer, returning true if successful}
  var
    code : Integer;
    SLen : Byte absolute S;
  begin
    while S[SLen] = ' ' do
      Dec(SLen);
    Val(S, I, code);
    if code <> 0 then begin
      I := code;
      Str2Int := False;
    end else
      Str2Int := True;
  end;

  function Str2Word(S : string; var I : Word) : Boolean;
    {-Convert a string to a word, returning true if successful}
  var
    code : Integer;
    SLen : Byte absolute S;
  begin
    while S[SLen] = ' ' do
      Dec(SLen);
    Val(S, I, code);
    if code <> 0 then begin
      I := code;
      Str2Word := False;
    end else
      Str2Word := True;
  end;

  function Str2Long(S : string; var I : LongInt) : Boolean;
    {-Convert a string to a longint, returning true if successful}
  var
    code : Integer;
    SLen : Byte absolute S;
  begin
    while S[SLen] = ' ' do
      Dec(SLen);
    Val(S, I, code);
    if code <> 0 then begin
      I := code;
      Str2Long := False;
    end else
      Str2Long := True;
  end;

  function Str2Double(S : string; var R : Double) : Boolean;
    {-Convert a string to a real, returning true if successful}
  var
    Code : Integer;
    SLen : Byte absolute S;
  begin
    {trim trailing blanks}
    while S[SLen] = ' ' do
      Dec(SLen);

    Val(S, R, Code);
    if Code <> 0 then begin
      R := Code;
      Str2Double := False;
    end else
      Str2Double := True;
  end;

Function GetSubString( const InString, delim1S, delim2S: string; var S: String ): Boolean;
var
  p1, p2: Integer;
begin
  Result := False; S := '';
  p1 := pos( delim1S, InString );
  if p1  <> 0  then begin
    p1 := p1 + length( delim1S );
    if delim2S <> '' then
      p2 := pos( delim2S, InString )
    else
      p2 := Length( InString ) + 1;
    if p2 <> 0  then begin
      p2 := p2;
      if p2 > p1 then begin
        S := copy( InString, p1, p2-p1 );
        Result := True;
        Exit;
      end;
    end;
  end;
end;

Function GetNextText( var f: TextFile; delim1S, delim2S: string;
  var i: integer; var InString, S: String ): Boolean;
begin
  Result := false;
  repeat
    Readln( f, InString ); inc( i );
    Result := GetSubString( InString, delim1S, delim2S, S );
  until ( Result or EOF( f ) {( i > Count-1)} );
end;

Function GetMonthNr( const S: String  ): Integer;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := TFormatSettings.Create( 'en-US' );
  Result := max( IndexText( S,FormatSettings.LongMonthNames)+1, IndexText( S,FormatSettings.ShortMonthNames)+1 );
  if Result = 0 then begin
    FormatSettings := TFormatSettings.Create( 'nl-NL' );
    Result := max( IndexText( S,FormatSettings.LongMonthNames)+1, IndexText( S,FormatSettings.ShortMonthNames)+1 );
  end;
end;

Function HTML2USASCIIString(const aStr: String): String;
begin
  Result := StringReplace( aStr, 'Ã©', 'é', [rfReplaceAll,rfIgnoreCase] );
  Result := StringReplace( Result, 'Ã³', 'ó', [rfReplaceAll,rfIgnoreCase] );
  Result := OStripAccents( Result );
end;

Function OStripAccents(const aStr: String): String;
type
  USASCIIString = type AnsiString(20127);//20127 = us ascii
begin
  Result := String(USASCIIString(aStr));
  //StringReplace( aStr, 'é', 'e', [rfReplaceAll,rfIgnoreCase] );
end;

function CountChar(const s: string; const c: char): integer;
begin
 Result:= TRegEx.Matches(s, c).Count
end;

{ Returns a count of the number of occurences of SubText in Text }
function CountOccurences( const SubText: string;
                          const Text: string): Integer;
begin
  Result := Pos(SubText, Text);
  if Result > 0 then
    Result := (Length(Text) - Length(StringReplace(Text, SubText, '', [rfReplaceAll]))) div  Length(subtext);
end;  { CountOccurences }

Procedure RemoveEmptyLinesFromStringList( var aList: TStringList );
var
  i: Integer;
begin
  for i := aList.count - 1 downto 0 do
  if Trim( aList[i]) = '' then
    aList.Delete(i);
end;

end.
