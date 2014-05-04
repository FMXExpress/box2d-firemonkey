program XCadencerTest;

uses
  FMX.Forms,
  Unit28 in 'Unit28.pas' {Form28},
  uXCadencer in '..\..\..\Physics2D\uXCadencer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm28, Form28);
  Application.Run;
end.
