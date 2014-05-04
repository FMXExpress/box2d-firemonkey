program HelloBox2D_Mobile;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  uFormSimpleDBox2D in 'uFormSimpleDBox2D.pas' {Form28},
  UPhysics2D in '..\..\..\Physics2D\UPhysics2D.pas',
  UPhysics2DTypes in '..\..\..\Physics2D\UPhysics2DTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm28, Form28);
  Application.Run;
end.
