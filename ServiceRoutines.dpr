program ServiceRoutines;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  OpWString in 'OPWString\OpWString.pas',
  DUtils in 'DUtils\DUtils.pas',
  Uerror in 'UError\Uerror.pas',
  DSiWin32 in 'Dsi\DSiWin32.pas',
  HotLog in 'HotLog\HotLog.pas',
  uPlane in 'Plane\uPlane.pas',
  DirWatch in 'DirWatch\DirWatch.pas',
  uFit in 'TestNumRecip\uFit.pas',
  uNumRecip in 'TestNumRecip\uNumRecip.pas',
  uPolInt in 'TestNumRecip\uPolInt.pas',
  RHdialogs in 'RH\RHdialogs.pas',
  RHmath in 'RH\RHmath.pas',
  RHsystem in 'RH\RHsystem.pas',
  AbstractTypedList in 'gentypedlist\AbstractTypedList.pas',
  xyun in 'xygr212\xyun.pas' {Form1},
  xygraph3d in 'xygr212\xygraph3d.pas',
  xygraph in 'xygr212\xygraph.pas',
  xycopy in 'xygr212\xycopy.pas' {CopyForm},
  xycommon in 'xygr212\xycommon.pas',
  shpAPI129 in 'lib.shp-1.2.9.r\Delphi\shpAPI129.pas';

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
