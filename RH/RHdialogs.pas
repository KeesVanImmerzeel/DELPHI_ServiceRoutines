unit RHdialogs;

interface

uses SysUtils, Windows, Classes, Vcl.Dialogs, System.UITypes;

Procedure StateDemo;
Procedure ShowError(Txt, Pas : string; LineNr : LongInt);
Function AskForAction(Value : string) : boolean;
Function Question(Value : string) : boolean;
Function Warning(Value : string) : boolean;
Function ErrorMsg(Value : string) : boolean;

implementation

Procedure StateDemo;
begin
  MessageDlg('This is a demo version. The selected option does not work',
             mtWarning, [mbOK], 0);
end;

Function AskForAction(Value : string) : boolean;
begin
  Result:= (MessageDlg(Value, mtConfirmation, [mbOK, mbCancel], 0) = idOK);
end;

Function Question(Value : string) : boolean;
begin
  Result:= (MessageDlg(Value, mtConfirmation, [mbYes, mbNo], 0) = idYes);
end;

Function Warning(Value : string) : boolean;
begin
  Result:= (MessageDlg(Value, mtWarning, [mbOK], 0) = idOK);
end;

Function ErrorMsg(Value : string) : boolean;
begin
  Result:= (MessageDlg(Value, mtError, [mbOK], 0) = idOK);
end;

Procedure ShowError(Txt, Pas : string; LineNr : LongInt);
begin
  MessageDlg(Txt + ' (' + Pas + ': ' + IntToStr(LineNr) + ')', mtError, [mbOK], 0);
end;

Procedure Information( Txt : string);
begin
  MessageDlg(Txt, mtWarning, [mbOK], 0);
end;

end.
