program b2PingPong_Mobile;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  uFormPingPong in 'uFormPingPong.pas' {FormPingPong},
  UGame in '..\PingPong_Shared\UGame.pas',
  UGame2D in '..\PingPong_Shared\UGame2D.pas',
  UGamePingPong in '..\PingPong_Shared\UGamePingPong.pas',
  UPingPongTypes in '..\PingPong_Shared\UPingPongTypes.pas',
  UPhysics2D in '..\..\..\Physics2D\UPhysics2D.pas',
  UPhysics2DTypes in '..\..\..\Physics2D\UPhysics2DTypes.pas',
  UPhysics2DHelper in '..\..\..\Physics2D\UPhysics2DHelper.pas',
  uFMDrawUtils in '..\PingPong_Shared\uFMDrawUtils.pas',
  uXCadencer in '..\..\..\Physics2D\uXCadencer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soPortrait, TFormOrientation.soInvertedPortrait];
  Application.CreateForm(TFormPingPong, FormPingPong);
  Application.Run;
end.
