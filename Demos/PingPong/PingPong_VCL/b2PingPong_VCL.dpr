program b2PingPong_VCL;

uses
  Forms,
  UfrmMain in 'UfrmMain.pas' {frmMain},
  UOpenGLCanvas in '..\..\..\OpenGL Canvas\UOpenGLCanvas.pas',
  UPhysics2D in '..\..\..\Physics2D\UPhysics2D.pas',
  UPhysics2DTypes in '..\..\..\Physics2D\UPhysics2DTypes.pas',
  MSTimer in '..\..\..\Physics2D\MSTimer.pas',
  UPhysics2DHelper in '..\..\..\Physics2D\UPhysics2DHelper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
