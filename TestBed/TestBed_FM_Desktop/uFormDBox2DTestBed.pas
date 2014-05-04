unit uFormDBox2DTestBed;

interface

{$I ..\..\Physics2D\Physics2D.inc}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Edit, FMX.ListBox, FMX.Objects,
  uXCadencer, uDebugDrawerFM, uTestBed, uPhysics2D, uPhysics2DTypes;

type
  TFormDBox2DTestBed = class(TForm, ITesterForm)
    PanelRight: TPanel;
    PanelClient: TPanel;
    LayoutRight: TLayout;
    LayoutButtons: TLayout;
    btnPause: TButton;
    btnSingleStep: TButton;
    btnReset: TButton;
    btnDumpWorld: TButton;
    grpbxGravity: TGroupBox;
    grpbxMode: TGroupBox;
    rdoRealTime: TRadioButton;
    rdoFixedStep: TRadioButton;
    editGravityX: TEdit;
    lblGravityX: TLabel;
    editGravityY: TEdit;
    lblGravityY: TLabel;
    btnConfirmGravity: TButton;
    LayoutSimulationOptions: TLayout;
    chkEnableSleep: TCheckBox;
    chkSubStepping: TCheckBox;
    chkWarmStarting: TCheckBox;
    chkContinuousPhysics: TCheckBox;
    listTestEntries: TListBox;
    lblTests: TLabel;
    pntbxDrawPanel: TPaintBox;
    lblSimulationOptions: TLabel;
    LayoutVisibilityOptions: TLayout;
    lblVisibility: TLabel;
    chkKeyInfo: TCheckBox;
    chkStats: TCheckBox;
    chkCOMs: TCheckBox;
    chkFrictionImpulse: TCheckBox;
    chkContactImpulse: TCheckBox;
    chkContactNormals: TCheckBox;
    chkContactPoints: TCheckBox;
    chkPairs: TCheckBox;
    chkAABBs: TCheckBox;
    chkShapes: TCheckBox;
    chkJoints: TCheckBox;
    chkAntialiasing: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SimulationOptionsChanged(Sender: TObject);
    procedure VisibilityOptionsChanged(Sender: TObject);
    procedure ModeOptionsChanged(Sender: TObject);
    procedure btnConfirmGravityClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnSingleStepClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnDumpWorldClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pntbxDrawPanelPaint(Sender: TObject; Canvas: TCanvas);
    procedure listTestEntriesChange(Sender: TObject);
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
    procedure chkAntialiasingChange(Sender: TObject);
  private
    lastp: TPointF;
    dumpPool: TStringList;
    FCadencer: TXCadencer;
    FDrawer: TDebugDrawerFM;
    FDeltaTime, FNewTime: Double;
    procedure TimerProgress(const deltaTime, newTime: Double);
    procedure AddTests;
    procedure UpdatePauseButton;
    procedure Dump(Indent: Integer; const Format: string; const Args: array of const);
    procedure TestChanged;
    procedure ResetView;
    procedure UpdateDrawerTranslation;
  public
    // ITesterForm
    procedure UpdateGravityText(AGravityTextX, AGravityTextY: string);
    procedure ResetTest;
  end;

var
  FormDBox2DTestBed: TFormDBox2DTestBed;

implementation

{$R *.fmx}

uses uFormDump, System.Character;

procedure TFormDBox2DTestBed.FormCreate(Sender: TObject);
begin
  FDeltaTime := 0;
  FNewTime := 0;

  randomize;

  AddTests;

  VisibilityOptionsChanged(self);
  SimulationOptionsChanged(self);
  ModeOptionsChanged(self);

  FCadencer := TXCadencer.Create;
  FCadencer.OnProgress := TimerProgress;

  FDrawer := TDebugDrawerFM.Create;
  FDrawer.Canvas := pntbxDrawPanel.Canvas;
end;

procedure TFormDBox2DTestBed.FormDestroy(Sender: TObject);
begin
  FCadencer.Enabled := False;
  if Assigned(Test) then
    Test.Free;
  FCadencer.Free;
  FDrawer.Free;
end;

procedure TFormDBox2DTestBed.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
var s: string;
begin
  if Key = vkEscape then
    Close

  else if Key = vkLeft then
   if ssCtrl in Shift then
     Test.ShiftOrigin(MakeVector(2.0, 0))
    else
     begin
      FDrawer.OffsetX := FDrawer.OffsetX - 1;
     end

  else if Key = vkRight then
   if ssCtrl in Shift then
     Test.ShiftOrigin(MakeVector(-2.0, 0))
    else
     begin
       FDrawer.OffsetX := FDrawer.OffsetX + 1;
     end

  else if Key = vkUp then
   if ssCtrl in Shift then
     Test.ShiftOrigin(MakeVector(0.0, -2.0))
    else
     begin
       FDrawer.OffsetY := FDrawer.OffsetY + 1;
     end

  else if Key = vkDown then
   if ssCtrl in Shift then
     Test.ShiftOrigin(MakeVector(0.0, 2.0))
    else
     begin
       FDrawer.OffsetY := FDrawer.OffsetY - 1;
     end


  else if Key = vkHome then
    ResetView

  else if (KeyChar = 'P') or (KeyChar = 'p') then
    btnPauseClick(nil)

  else if Assigned(Test) then
  begin
    if KeyChar = ' ' then
      Test.LaunchBomb
    else
      Test.Keyboard(Ord(KeyChar.ToUpper));
  end;
end;

procedure TFormDBox2DTestBed.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Assigned(Test) then
    Test.KeyboardUp(Ord(KeyChar.ToUpper));
end;

procedure TFormDBox2DTestBed.FormResize(Sender: TObject);
begin
  ResetView;
end;

procedure TFormDBox2DTestBed.FormShow(Sender: TObject);
begin
  ResetView;
  VisibilityOptionsChanged(nil);
  listTestEntries.ItemIndex := 0;
  TestChanged;
  SimulationOptionsChanged(nil);
end;

procedure TFormDBox2DTestBed.listTestEntriesChange(Sender: TObject);
begin
  if listTestEntries.ItemIndex <> ActiveEntryIndex then
    TestChanged;
end;

procedure TFormDBox2DTestBed.AddTests;
var i: Integer;
begin
  for i := 0 to TestCount - 1 do
    listTestEntries.Items.Add(TestEntries[i].Name);
end;

procedure TFormDBox2DTestBed.UpdatePauseButton;
begin
  if Settings.pause then
    btnPause.Text := 'Start'
  else
    btnPause.Text := 'Pause';
end;

procedure TFormDBox2DTestBed.VisibilityOptionsChanged(Sender: TObject);
var
  flag: Tb2DrawBitsSet;
begin

  Settings.drawShapes := chkShapes.IsChecked;
  Settings.drawJoints := chkJoints.IsChecked;
  Settings.drawAABBs := chkAABBs.IsChecked;
  Settings.drawPairs := chkPairs.IsChecked;
  Settings.drawContactPoints := chkContactPoints.IsChecked;
  Settings.drawContactNormals := chkContactNormals.IsChecked;
  Settings.drawContactImpulse := chkContactImpulse.IsChecked;
  Settings.drawFrictionImpulse := chkFrictionImpulse.IsChecked;
  Settings.drawCOMs := chkCOMs.IsChecked;
  Settings.drawStats := chkStats.IsChecked;
  Settings.drawKeyInfo := chkKeyInfo.IsChecked;

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

procedure TFormDBox2DTestBed.ModeOptionsChanged(Sender: TObject);
begin
  Settings.realTime := rdoRealTime.IsChecked;
end;

procedure TFormDBox2DTestBed.ResetTest;
begin
  TestChanged;
end;

procedure TFormDBox2DTestBed.ResetView;
begin
  UpdateDrawerTranslation;
  FDrawer.ScaleX := 10;
  FDrawer.ScaleY := 10;
  FDrawer.CanvasHeight := pntbxDrawPanel.Height;
end;

procedure TFormDBox2DTestBed.TestChanged;
begin
  if listTestEntries.ItemIndex = -1 then
    ActiveEntry := nil
  else
    ActiveEntry := @TestEntries[listTestEntries.ItemIndex];

  if Assigned(ActiveEntry) then
  begin
    ResetView;
    if Assigned(Test) then
       Test.Free;
    Test := ActiveEntry^.ClassType.Create;
    Test.m_InvertedY := False;
    Test.SetDebugDrawer(FDrawer);
    Test.SetTesterForm(self);
    Test.UpdateGravityText; // moved from the constructor
    SimulationOptionsChanged(self);
    FCadencer.Reset;
    FCadencer.Enabled := True;
    ActiveEntryIndex := listTestEntries.ItemIndex;
  end
  else
    ActiveEntryIndex := -1;

  // Do not let listTestEntries get focused.
//  listTestEntries.CanFocus := False;
end;

procedure TFormDBox2DTestBed.TimerProgress(const deltaTime, newTime: Double);
begin
  FDeltaTime := deltaTime;
  FNewTime := newTime;

  self.Invalidate;

  if not Assigned(Test) then
    FCadencer.Enabled := False;
end;

procedure TFormDBox2DTestBed.pntbxDrawPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  pv: TVector2;
begin
  if Button = TMouseButton.mbLeft then
  begin
    if Assigned(Test) then
    begin
      pv := FDrawer.ConvertScreenToWorld(X, Y);
      if ssShift in Shift then
        Test.ShiftMouseDown(pv)
      else
        Test.MouseDown(pv);
    end;
  end
  else if Button = TMouseButton.mbRight then
  begin
    lastp.X := X;
    lastp.Y := Y;
  end;
end;

procedure TFormDBox2DTestBed.pntbxDrawPanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
var pv, diff: TVector2;
begin
  pv := FDrawer.ConvertScreenToWorld(X, Y);
  if Assigned(Test) then
    Test.MouseMove(pv);

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

procedure TFormDBox2DTestBed.pntbxDrawPanelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  pv: TVector2;
begin
  if Assigned(Test) then
  begin
    pv := FDrawer.ConvertScreenToWorld(X, Y);
    if Button = TMouseButton.mbLeft then
      test.MouseUp(pv);
  end;
end;

procedure TFormDBox2DTestBed.pntbxDrawPanelMouseWheel(Sender: TObject;
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

procedure TFormDBox2DTestBed.pntbxDrawPanelPaint(Sender: TObject;
  Canvas: TCanvas);
const DEFAULT_OPACITY: Double = 100;
var mRect: TRectF;
begin
  Canvas.BeginScene;
  try
    Canvas.Fill.Color := TAlphaColorRec.Black;
    mRect := pntbxDrawPanel.BoundsRect;
    Canvas.FillRect(mRect,0,0,[],DEFAULT_OPACITY);

    if Assigned(Test) then
    begin
      FDrawer.Canvas := Canvas;
      Test.m_textLine := 15;
      FDrawer.SetDefaultFontColor(TAlphaColorRec.Yellow);
      Test.DrawText(ActiveEntry^.Name);
      Test.NextLine; // A space line

      FDrawer.SetDefaultFontColor(TAlphaColorRec.Aqua);
{$IFDEF COMPUTE_PHYSICS_TIME}
      Test.DrawText(Format('Delta Time: %.5fs  Physics Time: %.5fs', [FDeltaTime, Test.m_world.Profile.step]));
      Test.DrawText(Format('Collide Time: %.5fs  Solve Time: %.5fs  SolveTOI Time: %.5fs',
        [Test.m_world.Profile.collide, Test.m_world.Profile.solve, Test.m_world.Profile.solveTOI]));
      Test.DrawText(Format('Solve Velocity Time: %.5fs  Solve Position Time: %.5fs',
        [Test.m_world.Profile.solveVelocity, Test.m_world.Profile.solvePosition]));
      Test.DrawText('');
{$ELSE}
      Test.DrawText(Format('Delta Time: %.4fs', [FDeltaTime]));
{$ENDIF}

      Test.Step(Settings, FDeltaTime);
    end;

  finally
    Canvas.EndScene;
  end;
end;

procedure TFormDBox2DTestBed.UpdateDrawerTranslation;
begin
  if Assigned(FDrawer) then
  begin
    FDrawer.OffsetX := pntbxDrawPanel.Width / 2;
    FDrawer.OffsetY := pntbxDrawPanel.Height / 2;
  end;
end;

procedure TFormDBox2DTestBed.UpdateGravityText(AGravityTextX,
  AGravityTextY: string);
begin
  editGravityX.Text := AGravityTextX;
  editGravityY.Text := AGravityTextY;
end;

procedure TFormDBox2DTestBed.SimulationOptionsChanged(Sender: TObject);
begin
  Settings.enableSleep := chkEnableSleep.IsChecked;
  Settings.enableWarmStarting := chkWarmStarting.IsChecked;
  Settings.enableContinuousPhysics := chkContinuousPhysics.IsChecked;
  Settings.enableSubStepping := chkSubStepping.IsChecked;
  if Assigned(Test) then
  begin
    Test.m_world.AllowSleeping := Settings.enableSleep;
    Test.m_world.WarmStarting := Settings.enableWarmStarting;
    Test.m_world.ContinuousPhysics := Settings.enableContinuousPhysics;
    Test.m_world.SubStepping := Settings.enableSubStepping;
  end;
end;

procedure TFormDBox2DTestBed.btnConfirmGravityClick(Sender: TObject);
var
  v: TVector2;
begin
  if Assigned(Test) then
  begin
    v.x := StrToFloatDef(editGravityX.Text, 0.0);
    v.y := StrToFloatDef(editGravityY.Text, -10.0);
    editGravityX.Text := FloatToStr(v.x);
    editGravityY.Text := FloatToStr(v.y);
    Test.m_world.SetGravity(v);
    Test.m_world.WakeAllSleepingBodies;
  end;
end;

procedure TFormDBox2DTestBed.btnDumpWorldClick(Sender: TObject);
var
  dumpForm: TFormDump;
begin
  if Test <> nil then
  begin
    Settings.pause := True;
    UpdatePauseButton;
    dumpPool := TStringList.Create;
    try
      dumpPool.BeginUpdate;

      dumpPool.Add('===========================');
      dumpPool.Add('Test: ' + Test.ClassName);
      dumpPool.Add('Dumped at ' + TimeToStr(Now));
      dumpPool.Add('===========================');

      Test.m_world.SetDumpMethod(Self.Dump);
      Test.m_world.Dump;
      dumpPool.EndUpdate;

      dumpForm := TFormDump.Create(Self);
      dumpForm.memoDump.Lines.Assign(dumpPool);
      dumpForm.ShowModal;
    finally
      dumpPool.Free;
    end;
  end;
end;

procedure TFormDBox2DTestBed.btnPauseClick(Sender: TObject);
begin
  Settings.pause := not Settings.pause;
  UpdatePauseButton;
end;

procedure TFormDBox2DTestBed.btnResetClick(Sender: TObject);
begin
  TestChanged;
end;

procedure TFormDBox2DTestBed.btnSingleStepClick(Sender: TObject);
begin
  Settings.pause := True;
  Settings.singleStep := True;
  UpdatePauseButton;
end;

procedure TFormDBox2DTestBed.chkAntialiasingChange(Sender: TObject);
begin
// no direct equivalent in FireMonkey
end;

procedure TFormDBox2DTestBed.Dump(Indent: Integer; const Format: string;
  const Args: array of const);
begin
  case Indent of
    0: dumpPool.Add(System.SysUtils.Format(Format, Args));
    1: dumpPool.Add('   ' + System.SysUtils.Format(Format, Args));
    2: dumpPool.Add('      ' + System.SysUtils.Format(Format, Args));
    3: dumpPool.Add('         ' + System.SysUtils.Format(Format, Args));
    4: dumpPool.Add('            ' + System.SysUtils.Format(Format, Args));
  end;
end;

end.
