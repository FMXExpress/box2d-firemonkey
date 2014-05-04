program MSTimerTest;

uses
  Vcl.Forms,
  Unit29 in 'Unit29.pas' {Form29},
  MSTimer in '..\..\..\Physics2D\MSTimer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm29, Form29);
  Application.Run;
end.
