unit uFormDBox2DPyramidSample;

interface

{$I ..\..\Physics2D\Physics2D.inc}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Edit, FMX.ListBox, FMX.Objects,
  uXCadencer, uDebugDrawerFM, uSimulation, uPhysics2D, uPhysics2DTypes;

type
  TFormDBox2DSample = class(TForm, IGUIForm)
    LayoutButtons: TLayout;
    btnPause: TButton;
    btnSingleStep: TButton;
    btnReset: TButton;
    pntbxDrawPanel: TPaintBox;
    btnLaunchBomb: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SimulationOptionsChanged(Sender: TObject);
    procedure VisibilityOptionsChanged(Sender: TObject);
    procedure ModeOptionsChanged(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnSingleStepClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pntbxDrawPanelPaint(Sender: TObject; Canvas: TCanvas);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure pntbxDrawPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure pntbxDrawPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure pntbxDrawPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure pntbxDrawPanelMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure btnLaunchBombClick(Sender: TObject);
  private
    FSimulation: TSimulation;
    lastp: TPointF;
    dumpPool: TStringList;
    FCadencer: TXCadencer;
    FDrawer: TDebugDrawerFM;
    FDeltaTime, FNewTime: Double;
    procedure TimerProgress(const deltaTime, newTime: Double);
    procedure UpdatePauseButton;
    procedure ResetView;
    procedure UpdateDrawerTranslation;
  public
    procedure SimulationRecreateAndRun;
  end;

var
  FormDBox2DSample: TFormDBox2DSample;

implementation

{$R *.fmx}

uses System.Character, uMySimulation;

procedure TFormDBox2DSample.FormCreate(Sender: TObject);
begin
  FDeltaTime := 0;
  FNewTime := 0;

  Randomize;

  VisibilityOptionsChanged(self);
  SimulationOptionsChanged(self);
  ModeOptionsChanged(self);

  FCadencer := TXCadencer.Create;
  FCadencer.OnProgress := TimerProgress;

  FDrawer := TDebugDrawerFM.Create;
  FDrawer.Canvas := pntbxDrawPanel.Canvas;
end;

procedure TFormDBox2DSample.FormShow(Sender: TObject);
begin
  ResetView;
  VisibilityOptionsChanged(nil);
  SimulationOptionsChanged(nil);
  SimulationRecreateAndRun;
end;

procedure TFormDBox2DSample.FormDestroy(Sender: TObject);
begin
  FCadencer.Enabled := False;
  FCadencer.Free;
  FSimulation.Free;
  FDrawer.Free;
end;

procedure TFormDBox2DSample.SimulationRecreateAndRun;
begin
  ResetView;
  if Assigned(FSimulation) then
     FSimulation.Free;
  FSimulation := TMySimulation.Create;
  FSimulation.m_DrawDebugInfo := False;
  FSimulation.m_InvertedY := False;
  FSimulation.SetDebugDrawer(FDrawer);
  FSimulation.SetGUIForm(self);
  SimulationOptionsChanged(self);
  FCadencer.Reset;
  FCadencer.Enabled := True;
end;

procedure TFormDBox2DSample.TimerProgress(const deltaTime, newTime: Double);
begin
  FDeltaTime := deltaTime;
  FNewTime := newTime;

  self.Invalidate;

  if not Assigned(FSimulation) then
    FCadencer.Enabled := False;
end;

procedure TFormDBox2DSample.pntbxDrawPanelPaint(Sender: TObject;
  Canvas: TCanvas);
const DEFAULT_OPACITY: Double = 100;
var mRect: TRectF;
begin
  Canvas.BeginScene;
  try
    Canvas.Fill.Color := TAlphaColorRec.Black;
    mRect := pntbxDrawPanel.BoundsRect;
    Canvas.FillRect(mRect,0,0,[],DEFAULT_OPACITY);

    if Assigned(FSimulation) then
    begin
      FDrawer.Canvas := Canvas;
      FSimulation.Step(Settings, FDeltaTime);
    end;

  finally
    Canvas.EndScene;
  end;
end;

procedure TFormDBox2DSample.SimulationOptionsChanged(Sender: TObject);
begin
  Settings.enableSleep := True;
  Settings.enableWarmStarting := True;
  Settings.enableContinuousPhysics := True;
  Settings.enableSubStepping := False;
  if Assigned(FSimulation) then
  begin
    FSimulation.m_world.AllowSleeping := Settings.enableSleep;
    FSimulation.m_world.WarmStarting := Settings.enableWarmStarting;
    FSimulation.m_world.ContinuousPhysics := Settings.enableContinuousPhysics;
    FSimulation.m_world.SubStepping := Settings.enableSubStepping;
  end;
end;

procedure TFormDBox2DSample.VisibilityOptionsChanged(Sender: TObject);
var
  flag: Tb2DrawBitsSet;
begin
  Settings.drawShapes := True;
  Settings.drawJoints := False;
  Settings.drawAABBs := False;
  Settings.drawPairs := False;
  Settings.drawContactPoints := False;
  Settings.drawContactNormals := False;
  Settings.drawContactImpulse := False;
  Settings.drawFrictionImpulse := False;
  Settings.drawCOMs := False;
  Settings.drawStats := False;
  Settings.drawKeyInfo := False;

  flag := [];

  if Settings.drawShapes then
     Include(flag, e_shapeBit);

  if Settings.drawJoints then
     Include(flag, e_jointBit);

  if Settings.drawAABBs then
     Include(flag, e_aabbBit);

  if Settings.drawPairs then
     Include(flag, e_pairBit);

  if Settings.drawCOMs then
     Include(flag, e_centerOfMassBit);

  if Assigned(FDrawer) then
    FDrawer.m_drawFlags := flag;
end;

procedure TFormDBox2DSample.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
var s: string;
begin
  if Key = vkEscape then
    Close

  else if Key = vkLeft then
   if ssCtrl in Shift then
     FSimulation.ShiftOrigin(MakeVector(2.0, 0))
    else
     begin
      FDrawer.OffsetX := FDrawer.OffsetX - 1;
     end

  else if Key = vkRight then
   if ssCtrl in Shift then
     FSimulation.ShiftOrigin(MakeVector(-2.0, 0))
    else
     begin
       FDrawer.OffsetX := FDrawer.OffsetX + 1;
     end

  else if Key = vkUp then
   if ssCtrl in Shift then
     FSimulation.ShiftOrigin(MakeVector(0.0, -2.0))
    else
     begin
       FDrawer.OffsetY := FDrawer.OffsetY + 1;
     end

  else if Key = vkDown then
   if ssCtrl in Shift then
     FSimulation.ShiftOrigin(MakeVector(0.0, 2.0))
    else
     begin
       FDrawer.OffsetY := FDrawer.OffsetY - 1;
     end


  else if Key = vkHome then
    ResetView

  else if (KeyChar = 'P') or (KeyChar = 'p') then
    btnPauseClick(nil)

  else if Assigned(FSimulation) then
  begin
    if KeyChar = ' ' then
      FSimulation.LaunchBomb
    else
      FSimulation.Keyboard(Ord(KeyChar.ToUpper));
  end;
end;

procedure TFormDBox2DSample.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Assigned(FSimulation) then
    FSimulation.KeyboardUp(Ord(KeyChar.ToUpper));
end;

procedure TFormDBox2DSample.FormResize(Sender: TObject);
begin
  ResetView;
end;

procedure TFormDBox2DSample.UpdatePauseButton;
begin
  if Settings.pause then
    btnPause.Text := 'Start'
  else
    btnPause.Text := 'Pause';
end;

procedure TFormDBox2DSample.ModeOptionsChanged(Sender: TObject);
begin
  Settings.realTime := True;
end;

procedure TFormDBox2DSample.ResetView;
begin
  UpdateDrawerTranslation;
  FDrawer.ScaleX := 10;
  FDrawer.ScaleY := 10;
  FDrawer.CanvasHeight := pntbxDrawPanel.Height;

  FDrawer.OffsetY := FDrawer.OffsetY / 2; // move view centre lower
end;

procedure TFormDBox2DSample.pntbxDrawPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  pv: TVector2;
begin
  if Button = TMouseButton.mbLeft then
  begin
    if Assigned(FSimulation) then
    begin
      pv := FDrawer.ConvertScreenToWorld(X, Y);
      //if ssShift in Shift then
        FSimulation.ShiftMouseDown(pv)
      //else
      //  FSimulation.MouseDown(pv);
    end;
  end
  else if Button = TMouseButton.mbRight then
  begin
    lastp.X := X;
    lastp.Y := Y;
  end;
end;

procedure TFormDBox2DSample.pntbxDrawPanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
var pv, diff: TVector2;
begin
  pv := FDrawer.ConvertScreenToWorld(X, Y);
  if Assigned(FSimulation) then
    FSimulation.MouseMove(pv);

  if ssRight in Shift then
  begin
    diff.x := lastp.x - X;
    diff.y := lastp.y - Y;

    // span view
    FDrawer.OffsetX := FDrawer.OffsetX - diff.x;
    FDrawer.OffsetY := FDrawer.OffsetY + diff.y;
    lastp.x := X;
    lastp.y := Y;
  end;
end;

procedure TFormDBox2DSample.pntbxDrawPanelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  pv: TVector2;
begin
  if Assigned(FSimulation) then
  begin
    pv := FDrawer.ConvertScreenToWorld(X, Y);
    if Button = TMouseButton.mbLeft then
      FSimulation.MouseUp(pv);
  end;
end;

procedure TFormDBox2DSample.pntbxDrawPanelMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
var aScale: Double;
begin
  aScale := FDrawer.ScaleX;

  if WheelDelta < 0 then
    aScale := b2Max(aScale * 0.9, 0.01)
  else
    aScale := b2Min(aScale * 1.1, 1000.0);

  FDrawer.ScaleX := aScale;
  FDrawer.ScaleY := aScale;
end;

procedure TFormDBox2DSample.UpdateDrawerTranslation;
begin
  if Assigned(FDrawer) then
  begin
    FDrawer.OffsetX := pntbxDrawPanel.Width / 2;
    FDrawer.OffsetY := pntbxDrawPanel.Height / 2;
  end;
end;

procedure TFormDBox2DSample.btnLaunchBombClick(Sender: TObject);
begin
  FSimulation.LaunchBomb;
end;

procedure TFormDBox2DSample.btnPauseClick(Sender: TObject);
begin
  Settings.pause := not Settings.pause;
  UpdatePauseButton;
end;

procedure TFormDBox2DSample.btnResetClick(Sender: TObject);
begin
  SimulationRecreateAndRun;
end;

procedure TFormDBox2DSample.btnSingleStepClick(Sender: TObject);
begin
  Settings.pause := True;
  Settings.singleStep := True;
  UpdatePauseButton;
end;

end.
