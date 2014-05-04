program HelloBox2D_FMForms;

uses
  FMX.Forms,
  uFormFMPhysicsDebug in 'uFormFMPhysicsDebug.pas' {FormFMPhysicsDebug},
  UPhysics2D in '..\..\..\Physics2D\UPhysics2D.pas',
  UPhysics2DTypes in '..\..\..\Physics2D\UPhysics2DTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormFMPhysicsDebug, FormFMPhysicsDebug);
  Application.Run;
end.
