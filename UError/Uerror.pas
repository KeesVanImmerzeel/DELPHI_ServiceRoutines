unit Uerror;
  {-Mapping of error-codes}

interface

uses
  Vcl.dialogs, System.sysutils, System.UITypes, Vcl.Forms;

Const
  {-General error-codes: -99...0}
  cNoError = 0;
  cUnknownError = -1;

  {-UAlgRout: -149...-100 }

  {-ExtParU: -199...-150 }

  {-AlgRout: -299...-200}

  {-UStepRout: -399...-300 }

  {-StepRout: -499...-400}

  {-UDriver: -599...-500 }

  {-Driver: -699...-600}

  {-uINTodeCLASS: -799...-700}

  {-UDSmodel: -899...-800}

  {-UDSmodelS: -999...-900}

  {-DSmodelS: -1099...-1000}

  {-AdoSets: -1199...-1100}

  {-DSmodel101: -9099..-9000}

  {-DSmodel102: -9199..-9100}

  {-DSmodel103: -9299..-9200}

  {-DSmodel104: -9399..-9300}

  {-DSmodel105: -9499..-9400}

  {-DSmodel106: -9599..-9500}

  {-DSmodel107: -9699..-9600}

  {-DSmodel108: -9799..-9700}

  {-DSmodel109: -9809..-9800}

  {-DSmodel110: -9819..-9810}

  {-DSmodel111: -9829..-9820}

  {-DSmodel112: -9839..-9830}

  {-DSmodel113: -9849..-9840}

  {-DSmodel301: -9910..-9900}

  {-DSmodel201: -9911..-9950}

  {-DSmodel202: -9951..-9999}

  {-DSmodel203: -8051..-8099}

  {-DSmodel204: -8001..-8050}

  {-DSmodel1001 tot DSmodel1100: -10099..-10000}

  {-uDCfunc: -10199..-10100}

  {-uDCstepRout: -10299..-10200}

  {-LargeArrays: -10399..-10300}

  {-uTabstractESRIgrid: -10409 .. -10400}

//  Procedure HandleError( var lf: TextFile; const ErrMsgStr: String;
//    const ShowMessageDialog: Boolean );

  Procedure InitialiseLogFile;
  Procedure FinaliseLogFile;

  Procedure HandleError( const ErrMsgStr: String;
    const ShowMessageDialog: Boolean );
  Procedure HandleErrorFmt( const FormatStr: String; const Args: array of const;
    const ShowMessageDialog: Boolean );

  Procedure WriteToLogFile( const aMsgStr: String );
  Procedure WriteToLogFileFmt( const FormatStr: String; const Args: array of const );

  Procedure WriteToLogFile_No_CR( const aMsgStr: String );
  Procedure WriteToLogFile_No_CRFmt( const FormatStr: String; const Args: array of const );

// Function LogFileName: String;

Type
  EUnableToCreateLogFile = class( Exception );
  EUnableToCloseLogFile = class( Exception );
  EInput_File_DoesNotExist = class( Exception );
  EUnableToOpenFile = class( Exception );
  EReadErrorInLine = class( Exception );

ResourceString
  sUnableToCreateLogFile = 'Unable to create log-file: [%s].';
  sUnableToCloseLogFile = 'Unable to close log-file: [%s].';
  sInputFileDoesNotExist = 'Input-file [%s] does not exist.';
  sUnableToOpenFile = 'Unable to open file [%s].';
  sReadErrorInLine = 'Read error in line: %d';

implementation

var
  LogFileName: String;

Procedure InitialiseLogFile;
begin
  LogFileName := ChangeFileExt ( Application.Exename, '.log' );
    if fileExists( LogFileName ) then
    DeleteFile( LogFileName );
  WriteToLogFile( Format( 'Starting application [%s] at %s.',
    [Application.Exename, DateTimeToStr (Now)] ) );
end;

Procedure FinaliseLogFile;
begin
  if fileExists( LogFileName ) then
    WriteToLogFile( Format( 'Closing application [%s] at %s.',
    [Application.Exename, DateTimeToStr (Now)] ) );
end;

Procedure OldHandleError( var lf: TextFile; const ErrMsgStr: String;
    const ShowMessageDialog: Boolean );
begin
  Writeln( lf, ErrMsgStr );
  if ShowMessageDialog then
    MessageDlg( ErrMsgStr, mtError, [mbOk], 0);
end;

Procedure PrivateHandleError( const ErrMsgStr: String;
  const ShowMessageDialog, CR: Boolean );
var
  Filename: string;
  LogFile: TextFile;
begin
  // prepares log file
  Filename := LogFileName;
  AssignFile ( LogFile, Filename );
  if FileExists ( FileName ) then
    Append (LogFile) // open existing file
  else
    Rewrite (LogFile); // create a new one
  try
    // write to the file and show error
    if CR then
      Writeln ( LogFile, {DateTimeToStr (Now) + ':' +} ErrMsgStr )
    else
      Write ( LogFile, {DateTimeToStr (Now) + ':' +} ErrMsgStr );

    if ShowMessageDialog then
      MessageDlg( ErrMsgStr, mtError, [mbOk], 0 );
  finally
    CloseFile (LogFile);
  end;
end;

Procedure HandleError( const ErrMsgStr: String;
  const ShowMessageDialog: Boolean );
begin
  PrivateHandleError( ErrMsgStr, ShowMessageDialog, true );
end;

Procedure HandleErrorFmt( const FormatStr: String; const Args: array of const;
    const ShowMessageDialog: Boolean );
begin
  HandleError( Format( FormatStr, Args ), ShowMessageDialog );
end;

Procedure WriteToLogFile( const aMsgStr: String );
begin
  PrivateHandleError( aMsgStr, false, true );
end;

Procedure WriteToLogFileFmt( const FormatStr: String; const Args: array of const );
begin
  PrivateHandleError( Format( FormatStr, Args ), false, true );
end;


Procedure WriteToLogFile_No_CR( const aMsgStr: String );
begin
  PrivateHandleError( aMsgStr, false, false );
end;

Procedure WriteToLogFile_No_CRFmt( const FormatStr: String; const Args: array of const );
begin
  WriteToLogFile_No_CR( Format( FormatStr, Args ) );
end;

begin
  LogFileName := 'Default.log';
end.

