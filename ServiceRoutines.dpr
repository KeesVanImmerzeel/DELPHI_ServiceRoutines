program ServiceRoutines;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  OpWString in '..\ServiceRoutines\OPWString\OpWString.pas',
  DUtils in '..\ServiceRoutines\DUtils\DUtils.pas',
  Uerror in '..\ServiceRoutines\UError\Uerror.pas',
  DSiWin32 in '..\ServiceRoutines\Dsi\DSiWin32.pas',
  HotLog in '..\ServiceRoutines\HotLog\HotLog.pas',
  uPlane in '..\ServiceRoutines\Plane\uPlane.pas',
  SFEXEC in '..\ServiceRoutines\sfexec\2.0\SFEXEC.PAS',
  DirWatch in '..\ServiceRoutines\DirWatch\DirWatch.pas',
  uFit in '..\ServiceRoutines\TestNumRecip\uFit.pas',
  uNumRecip in '..\ServiceRoutines\TestNumRecip\uNumRecip.pas',
  uPolInt in '..\ServiceRoutines\TestNumRecip\uPolInt.pas',
  RHdialogs in '..\ServiceRoutines\RH\RHdialogs.pas',
  RHmath in '..\ServiceRoutines\RH\RHmath.pas',
  RHsystem in '..\ServiceRoutines\RH\RHsystem.pas',
  UDoubleMatrixList in '..\ServiceRoutines\DoubleMatrixList\UDoubleMatrixList.pas',
  AbstractTypedList in '..\ServiceRoutines\gentypedlist\AbstractTypedList.pas',
  xyun in '..\ServiceRoutines\xygr212\xyun.pas' {Form1},
  xygraph3d in '..\ServiceRoutines\xygr212\xygraph3d.pas',
  xygraph in '..\ServiceRoutines\xygr212\xygraph.pas',
  xycopy in '..\ServiceRoutines\xygr212\xycopy.pas' {CopyForm},
  xycommon in '..\ServiceRoutines\xygr212\xycommon.pas';

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
