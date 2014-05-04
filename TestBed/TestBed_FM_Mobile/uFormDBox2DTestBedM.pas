unit uFormDBox2DTestBedM;

interface

{$I ..\..\Physics2D\Physics2D.inc}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Edit, FMX.TreeView, FMX.ListBox, FMX.Objects,
  uXCadencer, uDebugDrawerFM, uTestBed, FMX.TabControl, Math, uPhysics2D, uPhysics2DTypes,
  FMX.Gestures;

type
  TFormDBox2DTestBedM = class(TForm, ITesterForm)
    listTestEntries: TListBox;
    ToolBar1: TToolBar;
    BackBTN: TButton;
    PanelClient: TPanel;
    pntbxDrawPanel: TPaintBox;
    Label1: TLabel;
    btnPause: TButton;
    btnSingleStep: TButton;
    btnDumpWorld: TButton;
    TabControl: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    LayoutVisibilityOptions: TLayout;
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
    LayoutSimulationOptions: TLayout;
    chkEnableSleep: TCheckBox;
    chkSubStepping: TCheckBox;
    chkWarmStarting: TCheckBox;
    chkContinuousPhysics: TCheckBox;
    lblSimulationOptions: TLabel;
    TabItem4: TTabItem;
    grpbxMode: TGroupBox;
    rdoRealTime: TRadioButton;
    rdoFixedStep: TRadioButton;
    grpbxGravity: TGroupBox;
    editGravityX: TEdit;
    lblGravityX: TLabel;
    editGravityY: TEdit;
    lblGravityY: TLabel;
    btnConfirmGravity: TButton;
    chkAntialiasing: TCheckBox;
    LayoutButtons: TLayout;
    btnReset: TButton;
    btnLaunchBomb: TButton;
    GestureManager1: TGestureManager;
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
    procedure chkAntialiasingClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pntbxDrawPanelPaint(Sender: TObject; Canvas: TCanvas);
    procedure listTestEntriesChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure pntbxDrawPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure pntbxDrawPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure pntbxDrawPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure BackBTNClick(Sender: TObject);
    procedure pntbxDrawPanelGesture(Sender: TObject;
      const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure btnLaunchBombClick(Sender: TObject);
  private
    { Private declarations }
    lastp: TPointF;
    dumpPool: TStringList;
    FCadencer: TXCadencer;
    FDrawer: TDebugDrawerFM;
    FDeltaTime, FNewTime: Double;
    FLastPosition: System.Types.TPointF;
    FLastDistance: Integer;
    procedure TimerProgress(const deltaTime, newTime: Double);
    procedure AddTests;
    procedure UpdatePauseButton;
    procedure Dump(Indent: Integer; const Format: string; const Args: array of const);
    procedure TestChanged;
    procedure ResetView;
    procedure UpdateDrawerTranslation;
    procedure handlePan(EventInfo: TGestureEventInfo);
    procedure handleRotate(eventInfo: TGestureEventInfo);
    procedure handleZoom(EventInfo: TGestureEventInfo);
    procedure handlePressAndTap(EventInfo: TGestureEventInfo);
  public
    // ITesterForm
    procedure UpdateGravityText(AGravityTextX, AGravityTextY: string);
    procedure ResetTest;
  end;

var
  FormDBox2DTestBedM: TFormDBox2DTestBedM;

implementation

{$R *.fmx}

uses uFormDumpM, System.Character;

procedure TFormDBox2DTestBedM.FormCreate(Sender: TObject);
begin
  FDeltaTime := 0;
  FNewTime := 0;

  randomize;

  AddTests;

  ActiveEntryIndex := -1;

  VisibilityOptionsChanged(self);
  SimulationOptionsChanged(self);
  ModeOptionsChanged(self);

  FCadencer := TXCadencer.Create;
  FCadencer.OnProgress := TimerProgress;

  FDrawer := TDebugDrawerFM.Create;
  FDrawer.Canvas := pntbxDrawPanel.Canvas;

  ResetView;
end;

procedure TFormDBox2DTestBedM.FormDestroy(Sender: TObject);
begin
  FCadencer.Enabled := False;
  if Assigned(Test) then
    Test.Free;
  FCadencer.Free;
  FDrawer.Free;
end;

procedure TFormDBox2DTestBedM.FormKeyDown(Sender: TObject; var Key: Word;
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

procedure TFormDBox2DTestBedM.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
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

procedure TFormDBox2DTestBedM.FormResize(Sender: TObject);
begin
  ResetView;
end;

procedure TFormDBox2DTestBedM.FormShow(Sender: TObject);
begin
  ResetView;
  VisibilityOptionsChanged(nil);
  //listTestEntries.ItemIndex := 0;
  //TestChanged;
  SimulationOptionsChanged(nil);
end;

procedure TFormDBox2DTestBedM.listTestEntriesChange(Sender: TObject);
begin
  if listTestEntries.ItemIndex <> ActiveEntryIndex then
    TestChanged;
  TabControl.Visible := False;
  PanelClient.Visible := True;
  BackBTN.Visible := True;
end;

procedure TFormDBox2DTestBedM.AddTests;
var i: Integer;
begin
  for i := 0 to TestCount - 1 do
    listTestEntries.Items.Add(TestEntries[i].Name);
end;

procedure TFormDBox2DTestBedM.UpdatePauseButton;
begin
  if Settings.pause then
   begin
    btnPause.Text := 'Start';
    btnPause.StyleLookup := 'playtoolbutton';
   end
  else
   begin
    btnPause.Text := 'Pause';
    btnPause.StyleLookup := 'stoptoolbutton';
   end;
end;

procedure TFormDBox2DTestBedM.VisibilityOptionsChanged(Sender: TObject);
var
  flag: Tb2DrawBitsSet;
begin
  // some visibility options are aparently ignored here

  Settings.drawShapes := chkShapes.IsChecked;
  Settings.drawJoints := chkJoints.IsChecked;
  Settings.drawAABBs := chkAABBs.IsChecked;
  Settings.drawPairs := chkPairs.IsChecked;
  Settings.drawContactPoints := chkContactPoints.IsChecked; // ignored
  Settings.drawContactNormals := chkContactNormals.IsChecked; // ignored
  Settings.drawContactImpulse := chkContactImpulse.IsChecked; // ignored
  Settings.drawFrictionImpulse := chkFrictionImpulse.IsChecked; // ignored
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

procedure TFormDBox2DTestBedM.ResetTest;
begin
  TestChanged;
end;

procedure TFormDBox2DTestBedM.ResetView;
begin
  UpdateDrawerTranslation;
  FDrawer.ScaleX := 10;
  FDrawer.ScaleY := 10;
  FDrawer.CanvasHeight := pntbxDrawPanel.Height+(pntbxDrawPanel.Height/4);
end;

procedure TFormDBox2DTestBedM.TestChanged;
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

procedure TFormDBox2DTestBedM.TimerProgress(const deltaTime, newTime: Double);
begin
  FDeltaTime := deltaTime;
  FNewTime := newTime;

  self.Invalidate;

  if not Assigned(Test) then
    FCadencer.Enabled := False;
end;

procedure TFormDBox2DTestBedM.pntbxDrawPanelGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  if EventInfo.GestureID = igiPan then
    handlePan(EventInfo)
  else if EventInfo.GestureID = igiZoom then
    handleZoom(EventInfo)
  else if EventInfo.GestureID = igiRotate then
    handleRotate(EventInfo)
  else if EventInfo.GestureID = igiPressAndTap then
    handlePressAndTap(EventInfo);
end;

procedure TFormDBox2DTestBedM.handlePan(EventInfo: TGestureEventInfo);
begin
end;

procedure TFormDBox2DTestBedM.handleRotate(eventInfo: TGestureEventInfo);
begin
end;

procedure TFormDBox2DTestBedM.handlePressAndTap(EventInfo: TGestureEventInfo);
begin
end;

procedure TFormDBox2DTestBedM.handleZoom(EventInfo: TGestureEventInfo);
var aScale: Double;
begin
  if (not(TInteractiveGestureFlag.gfBegin in EventInfo.Flags)) and
    (not(TInteractiveGestureFlag.gfEnd in EventInfo.Flags)) then
  begin
    aScale := FDrawer.ScaleX;

    if EventInfo.Distance - FLastDistance > 0 then
      aScale := b2Max(aScale * 0.9, 0.01)
    else
      aScale := b2Min(aScale * 1.1, 1000.0);

    FDrawer.ScaleX := aScale;
    FDrawer.ScaleY := aScale;
  end;
  FLastDistance := EventInfo.Distance;
end;

procedure TFormDBox2DTestBedM.pntbxDrawPanelMouseDown(Sender: TObject;
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

procedure TFormDBox2DTestBedM.pntbxDrawPanelMouseMove(Sender: TObject;
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

procedure TFormDBox2DTestBedM.pntbxDrawPanelMouseUp(Sender: TObject;
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

procedure TFormDBox2DTestBedM.pntbxDrawPanelPaint(Sender: TObject;
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
    //InvalidateRect(pntbxDrawPanel.BoundsRect);
  end;
end;

procedure TFormDBox2DTestBedM.UpdateDrawerTranslation;
begin
  if Assigned(FDrawer) then
  begin
    FDrawer.OffsetX := pntbxDrawPanel.Width / 2;
    FDrawer.OffsetY := pntbxDrawPanel.Height / 2;
  end;
end;

procedure TFormDBox2DTestBedM.UpdateGravityText(AGravityTextX,
  AGravityTextY: string);
begin
  editGravityX.Text := AGravityTextX;
  editGravityY.Text := AGravityTextY;
end;

procedure TFormDBox2DTestBedM.SimulationOptionsChanged(Sender: TObject);
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

procedure TFormDBox2DTestBedM.ModeOptionsChanged(Sender: TObject);
begin
  Settings.realTime := rdoRealTime.IsChecked;
end;

procedure TFormDBox2DTestBedM.btnConfirmGravityClick(Sender: TObject);
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

procedure TFormDBox2DTestBedM.btnDumpWorldClick(Sender: TObject);
//var
//  dumpForm: TFormDump;
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

      {$IFDEF ENABLE_DUMP}
      Test.m_world.SetDumpMethod(Self.Dump);
      Test.m_world.Dump;
      {$ENDIF}
      dumpPool.EndUpdate;

     // dumpForm := TFormDump.Create(Self);
      FormDump.memoDump.Lines.Assign(dumpPool);
      {$IFDEF MSWINDOWS}
      FormDump.ShowModal;
      {$ELSE}
      FormDump.Show;
      {$ENDIF}
    finally
      dumpPool.Free;
    end;
  end;
end;

procedure TFormDBox2DTestBedM.btnLaunchBombClick(Sender: TObject);
begin
  if Assigned(Test) then
    Test.LaunchBomb;
end;

procedure TFormDBox2DTestBedM.btnPauseClick(Sender: TObject);
begin
  Settings.pause := not Settings.pause;
  UpdatePauseButton;
end;

procedure TFormDBox2DTestBedM.btnResetClick(Sender: TObject);
begin
  ResetTest;
end;

procedure TFormDBox2DTestBedM.btnSingleStepClick(Sender: TObject);
begin
  Settings.pause := True;
  Settings.singleStep := True;
  UpdatePauseButton;
end;

procedure TFormDBox2DTestBedM.BackBTNClick(Sender: TObject);
begin
  PanelClient.Visible := False;
  TabControl.Visible := True;
  BackBTN.Visible := False;
end;

procedure TFormDBox2DTestBedM.chkAntialiasingClick(Sender: TObject);
begin
// no direct equivalent in FM canvas
//  GLCanvas.Antialiasing := chkAntialiasing.Checked;
if chkAntialiasing.IsChecked then
 begin
  Quality := TCanvasQuality.ccHighQuality;
 end
else
 begin
  Quality := TCanvasQuality.ccHighPerformance;
 end;

end;

procedure TFormDBox2DTestBedM.Dump(Indent: Integer; const Format: string;
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
