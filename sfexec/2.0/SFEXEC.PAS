{$F+ S-}
unit sfexec;

interface

uses
  Windows,
  Messages, SysUtils, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

const
  WM_TASKEXIT = $7DEF;

type

  TsfShowType = (sstHide, sstMinimize, sstRestore, sstShow, sstShowMaximized,
                 sstShowMinimized, sstShowMinNoActive, sstShowNA,
                 sstShowNoActivate, sstShowNormal);

  TsfAppExec = class(TComponent)
  private
    FParentForm: TForm;
    FApplicationName: String;
    FParameters: String;
    FShowType: TsfShowType;
    FWait: Boolean;
    FResultCode: Integer;
    FExitCode: DWORD;
    FInstance: THandle;

    FOnBeforeExecute: TNotifyEvent;
    FOnAfterExecute: TNotifyEvent;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Execute;
  published
    property ApplicationName: String read FApplicationName write FApplicationName;
    property Parameters: String read FParameters write FParameters;
    property ShowType: TsfShowType read FShowType write FShowType;
    property Wait: Boolean read FWait write FWait;
    property ResultCode: Integer read FResultCode;
    property ExitCode: DWORD read FExitCode;

    property BeforeExecute: TNotifyEvent read FOnBeforeExecute write FOnBeforeExecute;
    property AfterExecute: TNotifyEvent read FOnAfterExecute write FOnAfterExecute;
  end;

procedure Register;

implementation

constructor TsfAppExec.Create(AOwner: TComponent);
{var
  FindFormControl: TComponent;}
begin
  inherited Create(AOwner);

  if AOwner is TForm then FParentForm := TForm(AOwner) else
    begin
    Application.MessageBox('TsfExec can only be placed in a form', 'Error', mb_OK);
    {Fail;} Exit;
    end;
  FApplicationName := '';
  FParameters := '';
  FShowType := sstShowNormal;
  FWait := True;
  FResultCode := 0;
  FExitCode := 0;
end;

procedure TsfAppExec.Execute;
const
  CmdShowArray: array [0..9] of integer =
                (SW_HIDE, SW_MINIMIZE, SW_RESTORE, SW_SHOW,
                 SW_SHOWMAXIMIZED, SW_SHOWMINIMIZED, SW_SHOWMINNOACTIVE,
                 SW_SHOWNA, SW_SHOWMINNOACTIVE, SW_SHOWNORMAL);
var
  StartUpInfo: TStartUpInfo;
  ProcessInfo: TProcessInformation;
  CommandLine: PChar;
  WaitCode: LongInt;
begin
  if Assigned(FOnBeforeExecute) then FOnBeforeExecute(Self);
  FillChar(StartUpInfo, SizeOf(StartUpInfo), 0);
  StartUpInfo.cb := SizeOf(StartUpInfo);
  StartUpInfo.wShowWindow := CmdShowArray[Ord(FShowType)];
  StartUpInfo.dwFlags := STARTF_USESHOWWINDOW;
  CommandLine := StrAlloc(Length(FApplicationName) + Length(FParameters) + 2);
  StrPCopy(CommandLine, FApplicationName + ' ' + FParameters);
  try
    if (CreateProcess(nil, CommandLine, nil, nil, FALSE, 0, nil, nil, StartUpInfo, ProcessInfo)) then
      begin
      FResultCode := -1;
      try
        CloseHandle(ProcessInfo.hThread);
        if FWait then
          begin
          WaitCode := WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
          if WaitCode = WAIT_FAILED then
            FExitCode := 0
          else
            GetExitCodeProcess(ProcessInfo.hProcess, FExitCode);
          end
        else
      finally
        CloseHandle(ProcessInfo.hProcess);
        PostMessage(FParentForm.Handle, WM_TASKEXIT, 0, FExitCode);
        end
      end
    else
      FResultCode := GetLastError;
  finally
    StrDispose(CommandLine);
    end;
  if Assigned(FOnAfterExecute) then FOnAfterExecute(Self);
end;

procedure Register;
begin
  RegisterComponents('MyComponents', [TsfAppExec]);
end;

end.
