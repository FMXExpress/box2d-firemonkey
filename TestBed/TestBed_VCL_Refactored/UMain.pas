unit UMain;

interface

{$I ..\..\Physics2D\Physics2D.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, ExtCtrls, Math,
  UPhysics2DTypes, UPhysics2D,
  uTestBed, MSTimer,
  UOpenGLCanvas, uDebugDrawerOpenGL;

type
  TDrawPanel = class(TWinControl)
  published
     property OnResize;
     property OnMouseDown;
     property OnMouseMove;
     property OnMouseUp;
  end;

  TfrmMain = class(TForm, ITesterForm)
    Panel1: TPanel;
    Label1: TLabel;
    chkWarmStarting: TCheckBox;
    chkContinuousPhysics: TCheckBox;
    Label2: TLabel;
    chklstVisibility: TCheckListBox;
    btnPause: TButton;
    btnSingleStep: TButton;
    GroupBox1: TGroupBox;
    editGravityX: TEdit;
    editGravityY: TEdit;
    btnConfirmGravity: TButton;
    Label3: TLabel;
    Label4: TLabel;
    btnReset: TButton;
    GroupBox2: TGroupBox;
    rdoRealTime: TRadioButton;
    rdoFixedStep: TRadioButton;
    chkAntialiasing: TCheckBox;
    chkSubStepping: TCheckBox;
    btnDumpWorld: TButton;
    Bevel1: TBevel;
    listTestEntries: TListBox;
    chkEnableSleep: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure chklstVisibilityClickCheck(Sender: TObject);
    procedure SimulationOptionsChanged(Sender: TObject);
    procedure btnSingleStepClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure btnConfirmGravityClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnResetClick(Sender: TObject);
    procedure rdoRealTimeClick(Sender: TObject);
    procedure chkAntialiasingClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnDumpWorldClick(Sender: TObject);
    procedure listTestEntriesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure listTestEntriesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure listTestEntriesKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure rdoFixedStepClick(Sender: TObject);
  private
    { Private declarations }
	   lastp: TGLPointF;
     DrawPanel: TDrawPanel;
     dumpPool: TStringList;

     GLCanvas: TGLCanvas;
     Drawer: TDebugDrawerOpenGL;

     procedure DrawPanelResize(Sender: TObject);
     procedure DrawPanelMouseDown(Sender: TObject; Button: TMouseButton;
       Shift: TShiftState; X, Y: Integer);
     procedure DrawPanelMouseUp(Sender: TObject; Button: TMouseButton;
       Shift: TShiftState; X, Y: Integer);
     procedure DrawPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
       Y: Integer);

     procedure AddTests;
     procedure TestChanged;
     procedure Dump(Indent: Integer; const Format: string; const Args: array of const);
     procedure UpdateGUISettings;
     procedure UpdatePauseButton;
     procedure ResetView;
     procedure TimerProgress(const deltaTime, newTime: Double);
  public
   // ITesterForm
     procedure ResetTest;
     procedure UpdateGravityText(AGravityTextX, AGravityTextY: string);
  end;

var
  frmMain: TfrmMain;

implementation

uses
  UDump, System.UITypes;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var
   i: Integer;
begin
   //ReportMemoryLeaksOnShutdown := True;
  Randomize;

  DrawPanel := TDrawPanel.Create(Self);
  DrawPanel.Parent := Self;
  DrawPanel.Align := alClient;
  DrawPanel.OnMouseDown := DrawPanelMouseDown;
  DrawPanel.OnMouseMove := DrawPanelMouseMove;
  DrawPanel.OnMouseUp := DrawPanelMouseUp;
  DrawPanel.OnResize := DrawPanelResize;

  AddTests;

  Settings.drawShapes := True;
  Settings.drawJoints := True;
  Settings.drawAABBs := False;
  Settings.drawPairs := False;
  Settings.drawContactPoints := False;
  Settings.drawContactNormals := False;
  Settings.drawContactImpulse := False;
  Settings.drawFrictionImpulse := False;
  Settings.drawCOMs := False;
  Settings.drawStats := True;
  Settings.drawKeyInfo := True;
  Settings.enableSleep := True;
  Settings.enableWarmStarting := True;
  Settings.enableContinuousPhysics := True;
  Settings.enableSubStepping := False;
  Settings.pause := False;
  Settings.singleStep := False;
  Settings.realTime := True;
  Settings.customedStep := False;

  UpdateGUISettings;

  MSCadencer := TMSTimer.Create;
  MSCadencer.OnProgress := TimerProgress;

  GLCanvas := TGLCanvas.Create(DrawPanel, False, True, False, True);
  GLCanvas.DefaultFont.WinColor := TColors.White;

  Drawer := TDebugDrawerOpenGL.Create;
  Drawer.Canvas := GLCanvas;
end;

procedure TfrmMain.UpdateGUISettings;
begin
  chklstVisibility.Checked[0] := Settings.drawShapes;
  chklstVisibility.Checked[1] := Settings.drawJoints;
  chklstVisibility.Checked[2] := Settings.drawAABBs;
  chklstVisibility.Checked[3] := Settings.drawPairs;
  chklstVisibility.Checked[4] := Settings.drawContactPoints;
  chklstVisibility.Checked[5] := Settings.drawContactNormals;
  chklstVisibility.Checked[6] := Settings.drawContactImpulse;
  chklstVisibility.Checked[7] := Settings.drawFrictionImpulse;
  chklstVisibility.Checked[8] := Settings.drawCOMs;
  chklstVisibility.Checked[9] := Settings.drawStats;
  chklstVisibility.Checked[10] := Settings.drawKeyInfo;

  chkEnableSleep.Checked := Settings.enableSleep;
  chkWarmStarting.Checked := Settings.enableWarmStarting;
  chkContinuousPhysics.Checked := Settings.enableContinuousPhysics;
  chkSubStepping.Checked := Settings.enableSubStepping;

  // mutually exclusive
  if Settings.realTime then
    rdoRealTime.Checked := True
  else
    rdoFixedStep.Checked := True;

  UpdatePauseButton;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  MSCadencer.Enabled := False;
  if Assigned(Test) then
    Test.Free;
  MSCadencer.Free;
  GLCanvas.Free;
  Drawer.Free;
end;

procedure TfrmMain.TimerProgress(const deltaTime, newTime: Double);
begin
  if Assigned(Test) then
  begin
    GLCanvas.RenderingBegin(clBlack);
    Test.m_textLine := DrawPanel.ClientHeight - 15;
    GLCanvas.DefaultFont.WinColor := clYellow;
    Test.DrawText(ActiveEntry^.Name);
    Test.NextLine; // A space line

    GLCanvas.DefaultFont.WinColor := clAqua;

{$IFDEF COMPUTE_PHYSICS_TIME}
    Test.DrawText(Format('Delta Time: %.5fs  Physics Time: %.5fs', [deltaTime, Test.m_world.Profile.step]));
    Test.DrawText(Format('Collide Time: %.5fs  Solve Time: %.5fs  SolveTOI Time: %.5fs',
      [Test.m_world.Profile.collide, Test.m_world.Profile.solve, Test.m_world.Profile.solveTOI]));
    Test.DrawText(Format('Solve Velocity Time: %.5fs  Solve Position Time: %.5fs',
      [Test.m_world.Profile.solveVelocity, Test.m_world.Profile.solvePosition]));
    Test.DrawText('');
{$ELSE}
    Test.DrawText(Format('Delta Time: %.4fs', [deltaTime]));
{$ENDIF}
    GLCanvas.DefaultFont.WinColor := clWhite;
    Test.Step(settings, deltaTime);
    GLCanvas.RenderingEnd;
  end
  else
    MSCadencer.Enabled := False;
end;

procedure TfrmMain.AddTests;
var i: Integer;
begin
  for i := 0 to TestCount - 1 do
    listTestEntries.Items.Add(TestEntries[i].Name);
end;

procedure TfrmMain.TestChanged;
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
    Test.SetDebugDrawer(Drawer);
    Test.SetTesterForm(frmMain);
    Test.UpdateGravityText; // moved from the constructor
//      Test.ResetView; // not needed, called already
    MSCadencer.Reset;
    MSCadencer.Enabled := True;
    ActiveEntryIndex := listTestEntries.ItemIndex;
  end
  else
    ActiveEntryIndex := -1;

  // Do not let listTestEntries get focused.
  DrawPanel.SetFocus;
end;

procedure TfrmMain.SimulationOptionsChanged(Sender: TObject);
begin
  Settings.enableSleep := chkEnableSleep.Checked;
  Settings.enableWarmStarting := chkWarmStarting.Checked;
  Settings.enableContinuousPhysics := chkContinuousPhysics.Checked;
  Settings.enableSubStepping := chkSubStepping.Checked;
  if Assigned(Test) then
  begin
    Test.m_world.AllowSleeping := Settings.enableSleep;
    Test.m_world.WarmStarting := Settings.enableWarmStarting;
    Test.m_world.ContinuousPhysics := Settings.enableContinuousPhysics;
    Test.m_world.SubStepping := Settings.enableSubStepping;
  end;
end;

procedure TfrmMain.btnConfirmGravityClick(Sender: TObject);
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

procedure TfrmMain.btnDumpWorldClick(Sender: TObject);
var
  dumpForm: TfrmDump;
begin
  if Test <> nil then
  begin
    Settings.pause := True;
    UpdatePauseButton;
    dumpPool := TStringList.Create;
    try
      dumpPool.BeginUpdate;
      Test.m_world.SetDumpMethod(Self.Dump);
      Test.m_world.Dump;
      dumpPool.EndUpdate;

      dumpForm := TfrmDump.Create(Self);
      dumpForm.memoDump.Lines.Assign(dumpPool);
      dumpForm.ShowModal;
    finally
      dumpPool.Free;
    end;
  end;
end;

procedure TfrmMain.Dump(Indent: Integer; const Format: string; const Args: array of const);
begin
  case Indent of
    0: dumpPool.Add(SysUtils.Format(Format, Args));
    1: dumpPool.Add('   ' + SysUtils.Format(Format, Args));
    2: dumpPool.Add('      ' + SysUtils.Format(Format, Args));
    3: dumpPool.Add('         ' + SysUtils.Format(Format, Args));
    4: dumpPool.Add('            ' + SysUtils.Format(Format, Args));
  end;
end;

procedure TfrmMain.btnPauseClick(Sender: TObject);
begin
  Settings.pause := not Settings.pause;
  UpdatePauseButton;
end;

procedure TfrmMain.UpdatePauseButton;
begin
  if Settings.pause then
    btnPause.Caption := 'Start'
  else
    btnPause.Caption := 'Pause';
end;

procedure TfrmMain.btnResetClick(Sender: TObject);
begin
  ResetTest;
end;

procedure TfrmMain.btnSingleStepClick(Sender: TObject);
begin
  Settings.pause := True;
  Settings.singleStep := True;
  UpdatePauseButton;
end;

procedure TfrmMain.chkAntialiasingClick(Sender: TObject);
begin
  GLCanvas.Antialiasing := chkAntialiasing.Checked;
end;

procedure TfrmMain.chklstVisibilityClickCheck(Sender: TObject);
var
  flag: Tb2DrawBitsSet;
begin
// pg: some visibility options are aparently ignored here

  Settings.drawShapes := chklstVisibility.Checked[0];
  Settings.drawJoints := chklstVisibility.Checked[1];
  Settings.drawAABBs := chklstVisibility.Checked[2];
  Settings.drawPairs := chklstVisibility.Checked[3];
  Settings.drawContactPoints := chklstVisibility.Checked[4]; // ignored
  Settings.drawContactNormals := chklstVisibility.Checked[5]; // ignored
  Settings.drawContactImpulse := chklstVisibility.Checked[6]; // ignored
  Settings.drawFrictionImpulse := chklstVisibility.Checked[7]; // ignored
  Settings.drawCOMs := chklstVisibility.Checked[8];
  Settings.drawStats := chklstVisibility.Checked[9];
  Settings.drawKeyInfo := chklstVisibility.Checked[10];

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

  Drawer.m_drawFlags := flag;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
    Ord('P'): btnPauseClick(nil);
    VK_SPACE:
      if Assigned(Test) then
        Test.LaunchBomb;
    VK_LEFT: GLCanvas.SetTranslateX(GLCanvas.TranslateX - 1.0);
    VK_RIGHT: GLCanvas.SetTranslateX(GLCanvas.TranslateX + 1.0);
    VK_UP: GLCanvas.SetTranslateY(GLCanvas.TranslateY + 1.0);
    VK_DOWN: GLCanvas.SetTranslateY(GLCanvas.TranslateY - 1.0);
    VK_HOME: ResetView;
  else
    if Assigned(Test) then
      Test.Keyboard(Key);
  end;
end;

procedure TfrmMain.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(Test) then
    Test.KeyboardUp(Key);
end;

procedure TfrmMain.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if not listTestEntries.Focused then
    GLCanvas.SetEqualScale(b2Max(GLCanvas.ScaleX * 0.9, 0.01));
end;

procedure TfrmMain.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if not listTestEntries.Focused then
    GLCanvas.SetEqualScale(b2Min(GLCanvas.ScaleX * 1.1, 1000.0));
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  ResetView;
  chklstVisibilityClickCheck(nil);
  listTestEntries.ItemIndex := 0;
  TestChanged;
  SimulationOptionsChanged(nil);
end;

procedure TfrmMain.listTestEntriesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if listTestEntries.ItemIndex <> ActiveEntryIndex then
    TestChanged;
end;

procedure TfrmMain.listTestEntriesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if listTestEntries.ItemIndex <> ActiveEntryIndex then
    TestChanged;
end;

procedure TfrmMain.listTestEntriesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if listTestEntries.ItemIndex <> ActiveEntryIndex then
    TestChanged;
end;

procedure TfrmMain.rdoFixedStepClick(Sender: TObject);
begin
  Settings.realTime := False;
end;

procedure TfrmMain.rdoRealTimeClick(Sender: TObject);
begin
  Settings.realTime := True;
end;

procedure TfrmMain.DrawPanelResize(Sender: TObject);
begin
  if Assigned(GLCanvas) then
    ResetView;
end;

procedure TfrmMain.ResetTest;
begin
  if Assigned(Test) then
  begin
    FreeAndNil(Test);
    if Assigned(ActiveEntry) then
    begin
      Test := ActiveEntry^.ClassType.Create;
      MSCadencer.Reset;
    end;
  end;
end;

procedure TfrmMain.ResetView;
begin
  GLCanvas.BeginUpdateTransformation.ResetTransformation.
    SetTranslateX(DrawPanel.Width div 2).SetTranslateY(DrawPanel.Height div 2 - 20).SetEqualScale(10).
  EndUpdateTransformation;
end;

procedure TfrmMain.DrawPanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p: TGLPointF;
  pv: TVector2;
begin
  ActiveControl := nil;
  if Button = mbLeft then
  begin
    if Assigned(Test) then
    begin
      p := GLCanvas.ConvertScreenToWorld(X, Y);
      pv.x := p.X;
      pv.y := p.Y;
      if ssShift in Shift then
        Test.ShiftMouseDown(pv)
      else
        Test.MouseDown(pv);
    end;
  end
  else if Button = mbRight then
  begin
    lastp.X := X;
    lastp.Y := Y;
  end;
end;

procedure TfrmMain.DrawPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  p: TGLPointF;
  pv, diff: TVector2;
begin
  p := GLCanvas.ConvertScreenToWorld(X, Y);
  pv.x := p.X;
  pv.y := p.Y;
  if Assigned(Test) then
    Test.MouseMove(pv);

  if ssRight in Shift then
  begin
    diff.x := lastp.X - X;
    diff.y := lastp.Y - Y;

    // Span view
    GLCanvas.BeginUpdateTransformation;
    GLCanvas.TranslateX := GLCanvas.TranslateX - diff.x;
    GLCanvas.TranslateY := GLCanvas.TranslateY + diff.y;
    GLCanvas.EndUpdateTransformation;
    lastp.X := X;
    lastp.Y := Y;
  end;
end;

procedure TfrmMain.DrawPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p: TGLPointF;
  pv: TVector2;
begin
  p := GLCanvas.ConvertScreenToWorld(X, Y);
  pv.x := p.X;
  pv.y := p.Y;
  if Button = mbLeft then
    test.MouseUp(pv);
end;

procedure TfrmMain.UpdateGravityText(AGravityTextX, AGravityTextY: string);
begin
  editGravityX.Text := AGravityTextX;
  editGravityY.Text := AGravityTextY;
end;

initialization
   RegisterClass(TDrawPanel);

finalization

end.



