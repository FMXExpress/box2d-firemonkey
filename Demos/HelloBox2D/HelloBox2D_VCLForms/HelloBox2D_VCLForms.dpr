program HelloBox2D_VCLForms;

uses
  Forms,
  UPhysicsDebug in 'UPhysicsDebug.pas' {frmDebug},
  UPhysics2D in '..\..\..\Physics2D\UPhysics2D.pas',
  UPhysics2DTypes in '..\..\..\Physics2D\UPhysics2DTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  //Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmDebug, frmDebug);
  Application.Run;
end.
