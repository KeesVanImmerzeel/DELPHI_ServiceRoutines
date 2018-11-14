Unit DUtils;
  {-Utility routines}

interface

Uses
  Windows, SysUtils, SHELLAPI, system.classes, Vcl.Grids,  Vcl.OleCtrls, SHDocVw,
  ActiveX, Vcl.Dialogs, Vcl.Forms{, sfexec.};

Type
  Str12 = String[12];
  TDirection = ( FrWrd, BckWrd );

  TProcessType = ( Divide, Multiply, Substract, Add, SetValue );
  TStatInfoType = Record
    Sum,
    Average,
    Median,
    Perc5,
    Perc10,
    Perc25,
    Perc75,
    Perc90,
    Perc95: Double;
  end;
  TSortOrder = ( Ascending, Descending );

  TMode = ( Batch, Interactive, Unknown );

var
  Stars: String[80];
  FormatSettings: TFormatSettings;  {-Delphi XE6}


Function WordToStr( i: Word ): String;
  {-Converteert integer naar een string}

  {- =1 als A>=0; =-1 als A< 0}
Procedure RSwap( var x1, x2: Real );
Procedure ISwap( var x1, x2: Integer );
Function RemoveSpaces( const InputStr: String): String;
Function ISign( A: Real ): Integer;    {- =1 als A>=0; =-1 als A< 0}
procedure DelFilesFromDir( Wnd: HWND; const Directory, FileMask: string; const DelSubDirs: Boolean );
// delete a folder and all of its sub-folders
procedure DeleteDir(const DirName: string);

Function LoadCSV( Filename: string; HasColumnHeaders: Boolean;
  sep: Char; var sg: TStringGrid): Boolean;

{Function htmlTab( const aWebBrowser: TWebBrowser;
  const sfAppExec1: TsfAppExec;   // tbv uitvoeren Rscript.exe
  const
  url,                   // Web adres
  NextPageText,          // Tekst op button "next page" van webpagina's
  tmpFolder,             // Folder voor plaatsing tijdelijke bestanden
  RscriptExe,            // Verwijzing naar Rscript.exe
  htmltab_r: String;     // RscriptFileNam om HTML om te zetten naar tabel (csv formaat)
  const table_nr: Integer; // Tabel nummer op webpagina
  var sg: TStringGrid): Boolean;}  // Resultaat tabel wordt vertaald naar TStringGrid formaat

implementation

Uses
  OpWString;

Function WordToStr( i: Word ): String;
var s:string[11];
begin
  str(i,s); WordToStr := s;
end;

Procedure RSwap( var x1, x2: Real );
var help: Real;
begin help := x2; x2 := x1; x1 := help; end;

Procedure ISwap( var x1, x2: Integer );
var help: Integer;
begin help := x2; x2 := x1; x1 := help; end;

Function RemoveSpaces( const InputStr: String): String;
var i: Integer;
    S: String;
begin
  S := InputStr;
  Trim( S );
  if ( Pos( ' ', S ) > 0 ) then begin
    Result := '';
    for i:=1 to Length( S ) do begin
      if S[ i ] <> ' ' then
        Result := Result + S[ i ]
    end; {-for}
  end else
    Result := S;
end;

Function ISign( A: Real ): Integer;
  {- =1 als A>=0; =-1 als A< 0}
begin
  if ( A >=0 ) then ISign := 1 else ISign := -1;
end;

// delete a folder and all of its sub-folders
procedure DeleteDir(const DirName: string);
var
  Path: string;
  F: TSearchRec;
begin
  Path:= DirName + '\*.*';
  if FindFirst(Path, faAnyFile, F) = 0 then begin
    try
      repeat
        if (F.Attr and faDirectory <> 0) then begin
          if (F.Name <> '.') and (F.Name <> '..') then begin
            DeleteDir(DirName + '\' + F.Name);
          end;
        end
        else
          DeleteFile(DirName + '\' + F.Name);
      until FindNext(F) <> 0;
    finally
      FindClose(F);
    end;
  end;
  RemoveDir(DirName);
end;

procedure DelFilesFromDir( Wnd: HWND; const Directory, FileMask: string; const DelSubDirs: Boolean );
var
  SourceLst: string;
  FOS: TSHFileOpStruct;
begin
  FillChar(FOS, SizeOf(FOS), 0);
  FOS.Wnd := Wnd;
  FOS.wFunc := FO_DELETE;
  SourceLst := Directory + '\' + FileMask + #0;
  FOS.pFrom := PChar(SourceLst);
  if not DelSubDirs then
    FOS.fFlags := FOS.fFlags OR FOF_FILESONLY;
  // Remove the next line if you want a confirmation dialog box
  FOS.fFlags := FOS.fFlags OR FOF_NOCONFIRMATION;
  // Add the next line for a "silent operation" (no progress box)
  // FOS.fFlags := FOS.fFlags OR FOF_SILENT;
  SHFileOperation(FOS);
end;

Function LoadCSV( Filename: string; HasColumnHeaders: Boolean;
  sep: Char; var sg: TStringGrid): Boolean;
var
   i, j, Position, count, edt1: integer;
   temp, tempField : string;
   //FieldDel: char;
   Data: TStringList;
begin
  Result := false;
  Try
    Try
      Data := TStringList.Create;
      //FieldDel := ',';
      Data.LoadFromFile(Filename);
      temp :=  Data[1];
      count := 0;
      for i:= 1 to length(temp) do
        if copy(temp,i,1) = sep{ FieldDel} then
          inc(count);
      edt1 := count+1;
      sg.RowCount := Data.Count +1;
      sg.ColCount := 1 + CountChar( Data[1], sep );
      sg.FixedCols := 0;
      if HasColumnHeaders then
        sg.FixedRows := 1
      else
        sg.FixedRows := 0;
      for i := 0 to Data.Count - 1 do
        begin;
          temp :=  Data[i];
          if copy(temp,length(temp),1) <> sep {FieldDel} then
            temp := temp + sep{FieldDel};
          while Pos('"', temp) > 0 do
            begin
              Delete(temp,Pos('"', temp),1);
            end;
          for j := 1 to edt1 do
          begin
            Position := Pos( sep{FieldDel},temp);
            tempField := copy(temp,0,Position-1);
            sg.Cells[j-1,i] := tempField;
            Delete(temp,1,length(tempField)+1);
          end;
        end;
      Data.Free;
      Result := true;
    Except
    End;
  Finally
  End;
end;





// Lees table_nr van de web pagina('s) in sg (=TStringGrid)

// BELANGRIJK: Definieer in de form (Tform) met de web browser (TWebBrowser):
//  private
//    CurDispatch: IDispatch;
// En vervolgens de volgende events functies van form:

// BeforeNavigate2
// begin
//   CurDispatch := nil;
//   aWebBrowser.Tag := 0;
// end;

// DocumentComplete
// if (pDisp = CurDispatch) then
// begin
//   aWebBrowser.Tag := 1;
//   CurDispatch := nil;
// end;

// NavigateComplete2
// begin
//   if CurDispatch = nil then
//   CurDispatch := pDisp;
// end;


{Function htmlTab( const aWebBrowser: TWebBrowser; const sfAppExec1: TsfAppExec;
  const url, NextPageText, tmpFolder, RscriptExe, htmltab_r: String; const table_nr: Integer;
  var sg: TStringGrid ): Boolean;
const
  HasColumnHeaders = true;
  sep = ';';
var
  CSVfilename, HTMLfileName: String;
  NextPageFound: Boolean;

  Procedure WaitUntilDocumentLoaded;
  begin
    repeat
      Application.ProcessMessages
    until ( aWebBrowser.Tag > 0 );
  end;

  Function SaveCurrenWebpageToHTMLfile: Boolean;
  var
    PersistStream: IPersistStreamInit;
    Stream: IStream;
    FileStream: TFileStream;
  begin
    Result := false;
    WaitUntilDocumentLoaded;
    Try
      // 1. Controleer of er wel een webpagina is geladen.
      if not Assigned( aWebBrowser.Document ) then
        raise Exception.Create('No web page loaded (assigned).');
      // 2. Verwijder evt. bestaand HTML bestand
      if fileExists( HTMLfileName ) then
        SysUtils.DeleteFile(  HTMLfileName );
      // 3. Save webpage to HTML bestand.
      PersistStream := aWebBrowser.Document as IPersistStreamInit;
      FileStream := TFileStream.Create( HTMLfileName, fmCreate );
      Stream := TStreamAdapter.Create(FileStream, soReference) as IStream;
      if Failed( PersistStream.Save( Stream, True ) ) then
        Raise Exception.Create( 'SaveAs HTML fail!' );
      FileStream.Free;
      Result := true;
    Except
    End;
  end; // Function SaveCurrenHTMLpage

  Function ClickNextPage: Boolean;
  var
    Buttons: OleVariant;
    Button: OleVariant;
    i: Integer;
  begin
    Result := false;
    Buttons := aWebBrowser.OleObject.Document.all.tags('A'); ;
    for i := 0 to Buttons.Length - 1 do begin
      Button := Buttons.item( i );
      Result := pos( NextPageText, Button.innerText ) <> 0;
      if Result then begin
        Button.click();
        //WaitUntilDocumentLoaded;
        Break;
      end; // if
    end; // for
  end; // Function ClickNextPage

  Procedure ClearStringGrid;
  var
    i: Integer;
  begin
    for i := 0 to sg.ColCount - 1 do
      sg.Cols[i].Clear;
  end;

  Function HTML2CSV( const append_flag: integer // 0: maak nieuw csv bestand; 1: append
    ): Boolean;
  begin
    Result := false;
    Try
      Try
        with sfAppExec1 do begin
          ApplicationName :=  RscriptExe;
          Parameters :=  '--vanilla ' + htmltab_r + ' ' +
              HTMLfilename + ' ' + IntToStr(  table_nr ) + ' ' +
              CSVfilename + ' ' +
              intToStr( append_flag );
          ShowType        := sstHide;
          Wait            := true;
          Execute;
          Result := ( ResultCode < 0 ) and FileExists( CSVfilename );
        end; //-with sfAppExec1

        Result := true;
      Except
      End;
    Finally
    End;
  end; // Function HTML2CSV

begin
  Result := false;
  ClearStringGrid;

  Try
    Try
      // 0. Surf naar webpagina
      aWebBrowser.Navigate(url);

      // 0. Controleer of tmpFolder bestaat; zo niet maak aan.
      if not DirectoryExists( tmpFolder ) then
        if not CreateDir( tmpFolder ) then
          raise Exception.Create('Cannot create output folder in htmlTab.');

      // 0. Controleer of htmltab.r bestaat
      if not FileExists( htmltab_r ) then
        raise Exception.Create('htmltab.r does not exists.');

      CSVfilename := tmpFolder + '\tmp.csv';
      // 1. Verwijder evt bestaande *.csv file
      if fileExists( CSVfilename ) then
        SysUtils.DeleteFile(  CSVfilename );

      HTMLfilename := tmpFolder + '\tmp.html';

      // 2. Schrijf huidige webpagina weg in HTML vorm naar tmpFolder
      if not SaveCurrenWebpageToHTMLfile then
        raise Exception.Create('Cannot save current web page to HTML file.');

      // 3. Maak op basis van HTML-file een *.CSV file in tmpFolder (met header);
      //    Gooi HTML-file daarna weg.
      if not HTML2CSV( 0 ) then
        raise Exception.Create('Error creating csv-file.');
      SysUtils.deleteFile( HTMLfilename  );

      //  Herhaal 4 en 5 totdat geen nieuwe pagina is geladen

      repeat
        NextPageFound := ClickNextPage;
        if NextPageFound then begin
          // 4. Schrijf huidige webpagina weg in HTML vorm naar tmpFolder
          if not SaveCurrenWebpageToHTMLfile then
            raise Exception.Create('Cannot save current web page to HTML file.');

          // 5.Vul het bestaande CSV bestand aan met info uit het laatste HTML bestand.
          // (APPEND). Gooi HTML-file daarna weg.
         if not HTML2CSV( 1 ) then
           raise Exception.Create('Error creating csv-file.');
         SysUtils.deleteFile( HTMLfilename  );

        end; // if NextPageFound
      until not NextPageFound;

      // 6. Lees *.csv bestand en zet in sg (=TStringGrid)
      if not LoadCSV( CSVfilename, HasColumnHeaders, sep, sg ) then
        raise Exception.Create('Cannot create sg (TStringGrid).');

      // 7. Verwijder *.csv bestand
      //SysUtils.deleteFile( CSVfilename );

      Result := true;
    Except
    End;
  Finally
  End;
end;}

initialization
  FillChar(Stars, SizeOf(Stars), '*'); { Set a string to all stars }
  Stars[0] := #80;  { Set length byte }
  FormatSettings := TFormatSettings.Create;
  FormatSettings.DecimalSeparator := '.';
finalization

end.
