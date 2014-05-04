unit UMain;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, ExtCtrls, UOpenGLCanvas, MSTimer, UPhysics2D,
  UPhysics2DTypes, Math, OpenGL;

const
   k_maxContactPoints = 2048;

type
  TDrawPanel = class(TWinControl)
  published
     property OnResize;
     property OnMouseDown;
     property OnMouseMove;
     property OnMouseUp;
  end;

  TfrmMain = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    chkWarmStarting: TCheckBox;
    chkTimeOfImpact: TCheckBox;
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
    procedure rdoFixedStepClick(Sender: TObject);
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
  private
    { Private declarations }
	   lastp: TGLPointF;
     DrawPanel: TDrawPanel;
     dumpPool: TStringList;

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
  public
    { Public declarations }
     procedure TimerProgress(const deltaTime, newTime: Double);
     procedure ResetView;
  end;

  TTester = class;

  TDrawer = class(Tb2Draw)
  public
      Canvas: TGLCanvas;

      procedure DrawPolygon(const vertices: Tb2PolyVertices; vertexCount: Int32; const color: RGBA); override;
      procedure DrawPolygon4(const vertices: TVectorArray4; vertexCount: Int32; const color: RGBA); override;
      procedure DrawSolidPolygon(const vertices: Tb2PolyVertices; vertexCount: Int32; const color: RGBA); override;
      procedure DrawCircle(const center: TVector2; radius: PhysicsFloat; const color: RGBA); override;
      procedure DrawSolidCircle(const center, axis: TVector2; radius: PhysicsFloat; const color: RGBA); override;
      procedure DrawSegment(const p1, p2: TVector2; const color: RGBA); override;
      procedure DrawTransform(const xf: Tb2Transform); override;

      procedure DrawPoint(const p: TVector2; size: PhysicsFloat; const color: RGBA);
      procedure DrawAABB(const aabb: Tb2AABB; const color: RGBA);
  end;

  TDestructionListener = class(Tb2DestructionListener)
  public
     test: TTester;
     procedure SayGoodbye(fixture: Tb2Fixture); overload; override;
     procedure SayGoodbye(joint: Tb2Joint); overload; override;
  end;

  TContactPoint = record
     fixtureA, fixtureB: Tb2Fixture;
     normal, position: TVector2;
     id: Tb2ContactID;
     state: Tb2PointState;
     normalImpulse: PhysicsFloat;
     tangentImpulse: PhysicsFloat;
  end;

  TSettings = record
     drawShapes, drawJoints, drawAABBs, drawPairs, drawContactPoints, drawContactNormals,
     drawContactImpulse, drawFrictionImpulse, drawCOMs, drawStats, drawKeyInfo,
     enableSleep, enableWarmStarting, enableContinuous, enableSubStepping,
     pause, singleStep, realTime, customedStep: Boolean;
  end;

  TTestClass = class of TTester;
  TTester = class(Tb2ContactListener)
  protected
     m_RemainTime: PhysicsFloat;
  public
     m_stepCount: Integer;
     m_groundBody: Tb2Body;
     m_worldAABB: Tb2AABB;
     m_points: array[0..k_maxContactPoints - 1] of TContactPoint;
     m_pointCount: Int32;
     m_destructionListener: TDestructionListener;

     m_world: Tb2World;
     m_bomb: Tb2Body;
     m_mouseJoint: Tb2MouseJoint;
     m_bombSpawnPoint: TVector2;
     m_bombSpawning: Boolean;
     m_mouseWorld: TVector2;

     m_debugDraw: TDrawer;
     m_textLine: Int32;

     constructor Create; virtual;
     destructor Destroy; override;

     procedure NextLine;
     procedure Step(var settings: TSettings; timeStep: PhysicsFloat); virtual;
     procedure Keyboard(key: Byte); virtual;
     procedure KeyboardUp(key: Byte); virtual;
     procedure MouseDown(const p: TVector2); virtual;
     procedure ShiftMouseDown(const p: TVector2);
     procedure MouseUp(const p: TVector2); virtual;
     procedure MouseMove(const p: TVector2);
     procedure LaunchBomb(velocity_factor: PhysicsFloat = 1.0); overload; virtual;
     procedure LaunchBomb(const position, velocity: TVector2); overload;
	   procedure SpawnBomb(const worldPt: TVector2);
	   procedure CompleteBombSpawn(const p: TVector2);

     procedure SetCanvasTranslation(x, y: PhysicsFloat);
     procedure SetCanvasTranslationOffset(dx, dy: PhysicsFloat);
     procedure DrawText(const text: string);

     // Let derived tests know that a joint was destroyed.
     procedure JointDestroyed(joint: Tb2Joint); virtual;

     // Callbacks for derived classes.
     procedure PreSolve(var contact: Tb2Contact; const oldManifold: Tb2Manifold); override;

     procedure UpdateGravityText;
  end;

const
   DefaultStep = 1 / 60;
   velocityIterations = 8;
   positionIterations = 3;

var
  frmMain: TfrmMain;

procedure RegisterTestEntry(name: ShortString; ClassType: TTestClass);

var
   Test: TTester;

implementation
uses
   UDump;

{$R *.dfm}

type
   PTestEntry = ^TTestEntry;
   TTestEntry = record
      Name: ShortString;
      ClassType: TTestClass;
   end;

var
   Settings: TSettings;
   Drawer: TDrawer;
   GLCanvas: TGLCanvas;

   TestEntries: array of TTestEntry;
   TestCount: Integer;
   ActiveEntry: PTestEntry;
   ActiveEntryIndex: Integer;

procedure RegisterTestEntry(name: ShortString; ClassType: TTestClass);
var
   i: Integer;
   found: Int32;
begin
   found := TestCount;
   for i := 0 to TestCount - 1 do // Sort by name
      if CompareText(name, TestEntries[i].Name) < 0 then
      begin
         found := i;
         Break;
      end;

   SetLength(TestEntries, TestCount + 1);
   if found < TestCount then
      for i := TestCount downto found + 1 do
         TestEntries[i] := TestEntries[i - 1];
   TestEntries[found].Name := name;
   TestEntries[found].ClassType := ClassType;
   Inc(TestCount);
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
   Settings.pause := True;
   dumpPool := TStringList.Create;
   dumpPool.BeginUpdate;
   Test.m_world.SetDumpMethod(Self.Dump);
   Test.m_world.Dump;
   dumpPool.EndUpdate;

   dumpForm := TfrmDump.Create(Self);
   dumpForm.memoDump.Lines.Assign(dumpPool);
   dumpForm.ShowModal;
   dumpPool.Free;
end;

procedure TfrmMain.btnPauseClick(Sender: TObject);
begin
   Settings.pause := not Settings.pause;
end;

procedure TfrmMain.btnResetClick(Sender: TObject);
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

procedure TfrmMain.btnSingleStepClick(Sender: TObject);
begin
	 Settings.pause := True;
	 Settings.singleStep := True;
end;

procedure TfrmMain.chkAntialiasingClick(Sender: TObject);
begin
   GLCanvas.Antialiasing := chkAntialiasing.Checked;
end;

procedure TfrmMain.chklstVisibilityClickCheck(Sender: TObject);
type
   TSettingArray = array[0..SizeOf(TSettings) div SizeOf(Boolean) - 1] of Boolean;
var
   flag: Tb2DrawBitsSet;
   i: Integer;
begin
   for i := 0 to High(TSettingArray) - 8 do
      TSettingArray(Settings)[i] := chklstVisibility.Checked[i];

   flag := [];
   with Settings do
   begin
      if drawShapes then
         Include(flag, e_shapeBit);
      if drawJoints then
         Include(flag, e_jointBit);
      if drawAABBs then
         Include(flag, e_aabbBit);
      if drawPairs then
         Include(flag, e_pairBit);
      if drawCOMs then
         Include(flag, e_centerOfMassBit);
   end;
   Drawer.m_drawFlags := flag;
end;

procedure TfrmMain.AddTests;
var
   i: Integer;
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
      MSCadencer.Reset;
      MSCadencer.Enabled := True;
      ActiveEntryIndex := listTestEntries.ItemIndex;
   end
   else
      ActiveEntryIndex := -1;

   // Do not let listTestEntries get focused.
   DrawPanel.SetFocus;
end;

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

   FillChar(Settings, SizeOf(Settings), 0);
   with Settings do
   begin
	    drawShapes := True;
	 	  drawJoints := True;
      drawStats := True;
      drawKeyInfo := True;
      enableSleep := True;
		  enableWarmStarting := True;
		  enableContinuous := True;
      realTime := True;
   end;
   chklstVisibility.Checked[0] := True;
   chklstVisibility.Checked[1] := True;
   chklstVisibility.Checked[9] := True;
   chklstVisibility.Checked[10] := True;

   chkWarmStarting.Checked := True;
   chkTimeOfImpact.Checked := True;
   rdoRealTime.Checked := True;

   MSCadencer := TMSTimer.Create;
   MSCadencer.OnProgress := TimerProgress;
   GLCanvas := TGLCanvas.Create(DrawPanel, False, True, False, True);
   GLCanvas.DefaultFont.WinColor := clWhite;
   Drawer := TDrawer.Create;
   Drawer.Canvas := GLCanvas;
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

procedure TfrmMain.ResetView;
begin
   GLCanvas.BeginUpdateTransformation.ResetTransformation.
      SetTranslateX(DrawPanel.Width div 2).SetTranslateY(DrawPanel.Height div 2 - 20).SetEqualScale(10).
   EndUpdateTransformation;
end;

procedure TfrmMain.SimulationOptionsChanged(Sender: TObject);
begin
   with Settings do
   begin
      enableSleep := chkEnableSleep.Checked;
      enableWarmStarting := chkWarmStarting.Checked;
      enableContinuous := chkTimeOfImpact.Checked;
      enableSubStepping := chkSubStepping.Checked;
      if Assigned(Test) then
      begin
         Test.m_world.AllowSleeping := enableSleep;
         Test.m_world.WarmStarting := enableWarmStarting;
         Test.m_world.ContinuousPhysics := enableContinuous;
         Test.m_world.SubStepping := enableSubStepping;
      end;
   end;
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

{ TDrawer }

procedure TDrawer.DrawPolygon(const vertices: Tb2PolyVertices;
  vertexCount: Int32; const color: RGBA);
{$IFNDEF SINGLE_PRECISION}
var
   i: Integer;
{$ENDIF}
begin
   {$IFDEF SINGLE_PRECISION}
   Canvas.SetPenColor(TColorVector(color)).Polygon(TGLPointsF(@vertices[0]), vertexCount);
   {$ELSE}
   Canvas.SetPenColor(TColorVector(color));
   glBegin(GL_LINE_LOOP);
   for i := 0 to vertexCount - 1 do
      glVertex2d(vertices[i].X, vertices[i].Y);
   glEnd;
   {$ENDIF}
end;

procedure TDrawer.DrawPolygon4(const vertices: TVectorArray4;
   vertexCount: Int32; const color: RGBA);
{$IFNDEF SINGLE_PRECISION}
var
   i: Integer;
{$ENDIF}
begin
   {$IFDEF SINGLE_PRECISION}
   Canvas.SetPenColor(TColorVector(color)).Polygon(TGLPointsF(@vertices[0]), 4);
   {$ELSE}
   Canvas.SetPenColor(TColorVector(color));
   glBegin(GL_LINE_LOOP);
   for i := 0 to 3 do
      glVertex2d(vertices[i].X, vertices[i].Y);
   glEnd;
   {$ENDIF}
end;

procedure TDrawer.DrawSolidPolygon(const vertices: Tb2PolyVertices;
   vertexCount: Int32; const color: RGBA);
var
   tmp: TColorVector;
{$IFNDEF SINGLE_PRECISION}
   i: Integer;
{$ENDIF}
begin
   with TRGBA(color) do
   begin
      tmp[0] := red / 2;
      tmp[1] := green / 2;
      tmp[2] := blue / 2;
      tmp[3] := alpha / 2;
   end;
   {$IFDEF SINGLE_PRECISION}
   Canvas.SetPenColor(TColorVector(color)).SetBrushColor(tmp).
      FillPolygon(TGLPointsF(@vertices[0]), vertexCount, True);
   {$ELSE}
   Canvas.SetPenColor(tmp);
   glBegin(GL_POLYGON);
   for i := 0 to vertexCount - 1 do
      glVertex2d(vertices[i].X, vertices[i].Y);
   glEnd;
   Canvas.SetPenColor(TColorVector(color));
   glBegin(GL_LINE_LOOP);
   for i := 0 to vertexCount - 1 do
      glVertex2d(vertices[i].X, vertices[i].Y);
   glEnd;
   {$ENDIF}
end;

procedure TDrawer.DrawCircle(const center: TVector2; radius: PhysicsFloat; const color: RGBA);
begin
   Canvas.SetPenColor(TColorVector(color)).Ellipse(center.x, center.y, radius, radius);
end;

procedure TDrawer.DrawSolidCircle(const center, axis: TVector2; radius: PhysicsFloat; const color: RGBA);
var
   tmp: TColorVector;
   p: TVector2;
begin
   Canvas.SetPenColor(TColorVector(color));
   with TRGBA(color) do
   begin
      tmp[0] := red / 2;
      tmp[1] := green / 2;
      tmp[2] := blue / 2;
      tmp[3] := alpha / 2;
   end;
   Canvas.SetBrushColor(tmp).FillEllipse(center.x, center.y, radius, radius, True);

   {$IFDEF OP_OVERLOAD}
   p := center + radius * axis;
   {$ELSE}
   p := Add(center, Multiply(axis, radius));
   {$ENDIF}
   Canvas.Line(center.x, center.y, p.x, p.y);
end;

procedure TDrawer.DrawSegment(const p1, p2: TVector2; const color: RGBA);
begin
   Canvas.SetPenColor(TColorVector(color)).Line(p1.x, p1.y, p2.x, p2.y);
end;

procedure TDrawer.DrawTransform(const xf: Tb2Transform);
const
   k_axisScale = 0.4;
   clRed: TColorVector = (1.0, 0.0, 0.0, 1.0);
   clGreen: TColorVector = (0.0, 1.0, 0.0, 1.0);
var
   p2: TVector2;
   xAxis, yAxis: TVector2;
begin
   with xf do
   begin
      {$IFDEF OP_OVERLOAD}
      xAxis := q.GetXAxis;
      yAxis := q.GetYAxis;
      {$ELSE}
      xAxis := GetXAxis(q);
      yAxis := GetYAxis(q);
      {$ENDIF}
      p2.x := p.x + k_axisScale * xAxis.x;
      p2.y := p.y + k_axisScale * xAxis.y;
      Canvas.SetPenColor(clRed).Line(p.x, p.y, p2.x, p2.y);

      p2.x := p.x + k_axisScale * yAxis.x;
      p2.y := p.y + k_axisScale * yAxis.y;
      Canvas.SetPenColor(clGreen).Line(p.x, p.y, p2.x, p2.y);
   end;
end;

procedure TDrawer.DrawPoint(const p: TVector2; size: PhysicsFloat; const color: RGBA);
begin
   glPointSize(size);
   glColor3f(color[0], color[1], color[2]);
   glBegin(GL_POINTS);
   glVertex2f(p.x, p.y);
   glEnd;
   glPointSize(1.0);
end;

procedure TDrawer.DrawAABB(const aabb: Tb2AABB; const color: RGBA);
begin
   glColor3f(color[0], color[1], color[2]);
   glBegin(GL_LINE_LOOP);
   with aabb do
   begin
      glVertex2f(lowerBound.x, lowerBound.y);
      glVertex2f(upperBound.x, lowerBound.y);
      glVertex2f(upperBound.x, upperBound.y);
      glVertex2f(lowerBound.x, upperBound.y);
   end;
   glEnd;
end;

{ TDestructionListener }

procedure TDestructionListener.SayGoodbye(fixture: Tb2Fixture);
begin
end;

procedure TDestructionListener.SayGoodbye(joint: Tb2Joint);
begin
   if test.m_mouseJoint = joint then
      test.m_mouseJoint := nil
   else
      test.JointDestroyed(joint);
end;

{ TTester }

constructor TTester.Create;
const
   WorldLowerBound: TVector2 = (x: -200.0; y: -100.0);
   WorldUpperBound: TVector2 = (x: 200.0; y: 200.0);
var
   gravity: TVector2;
   bodyDef: Tb2BodyDef;
begin
   m_RemainTime := 0.0;

   m_worldAABB.lowerBound := WorldLowerBound;
   m_worldAABB.upperBound := WorldUpperBound;

   gravity.x := 0.0;
   gravity.y := -10.0;
   m_world := Tb2World.Create(gravity);

   UpdateGravityText;

   m_bomb := nil;
   m_textLine := 30;
   m_mouseJoint := nil;
   m_pointCount := 0;

   m_destructionListener := TDestructionListener.Create;
   m_destructionListener.test := Self;
   m_world.DestructionListener := m_destructionListener;
   m_world.SetContactListener(Self);

   m_debugDraw := Drawer;
   m_world.Draw := m_debugDraw;

   m_bombSpawning := False;
	 m_stepCount := 0;
   bodyDef := Tb2BodyDef.Create;
	 m_groundBody := m_world.CreateBody(bodyDef);

   m_world.WarmStarting := Settings.enableWarmStarting;
   m_world.ContinuousPhysics := Settings.enableContinuous;
end;

destructor TTester.Destroy;
begin
   m_world.Free;
   m_destructionListener.Free;
   inherited;
end;

procedure TTester.NextLine;
begin
   m_textLine := m_textLine - 15;
end;

procedure TTester.Step(var settings: TSettings; timeStep: PhysicsFloat);
const
   k_impulseScale = 0.1;
   k_axisScale = 0.4;
   clPoint: RGBA = (0.0, 1.0, 0.0, 1.0);
   clLine: RGBA = (0.8, 0.8, 0.8, 1.0);
   clAdd: RGBA = (0.3, 0.95, 0.3, 1.0);
   clPersist: RGBA = (0.3, 0.3, 0.95, 1.0);
   clContactNormal: RGBA = (0.9, 0.9, 0.9, 1.0);
   clContactImpulse: RGBA = (0.9, 0.9, 0.3, 1.0);
   clFrictionImpulse: RGBA = (0.9, 0.2, 0.2, 1.0);
   clBomb: RGBA = (0.0, 0.0, 1.0, 1.0);

var
   i: Integer;
   p1, p2: TVector2;
begin
   if not Settings.customedStep then
   begin
      m_pointCount := 0;
      if not settings.realTime then
         timeStep := DefaultStep;

      if settings.pause then
      begin
         m_RemainTime := 0.0;
         if settings.singleStep then
         begin
            m_world.Step(DefaultStep, velocityIterations, positionIterations, True);
            settings.singleStep := False;
            Inc(m_stepCount);
         end
         else
            m_world.Step(0, 8, 3, True);
            //m_world.DrawDebugData;
         m_debugDraw.Canvas.DefaultFont.WinColor := clRed;
         DrawText('**** PAUSED ****');
         m_debugDraw.Canvas.DefaultFont.WinColor := clWhite;
      end
      else
      begin
         if settings.realTime then // Make sure that every frame is processed using a time step of 1/60s.
         begin
            timeStep := timeStep + m_RemainTime;
            while timeStep > DefaultStep do
            begin
               m_world.Step(DefaultStep, velocityIterations, positionIterations);
               timeStep := timeStep - DefaultStep;
            end;
            m_RemainTime := timeStep;
            m_world.DrawDebugData;
         end
         else
            m_world.Step(timeStep, velocityIterations, positionIterations, True);

         Inc(m_stepCount);
      end;
   end;

   if settings.drawKeyInfo then
   begin
      m_debugDraw.Canvas.DefaultFont.WinColor := clRed;
      DrawText('Space: Launch bomb   Arrows: Move view   Home: Reset view');
      DrawText('Right Mouse: Span   Wheel: Scale');
      DrawText('Hold Shift and drag the mouse to spawn a bullet.');
      m_debugDraw.Canvas.DefaultFont.WinColor := clWhite;
   end;

   if settings.drawStats then
   begin
      m_debugDraw.Canvas.DefaultFont.WinColor := clLime;
      NextLine; // space line
      DrawText(Format('bodies/contacts/joints = %d/%d/%d',
         [m_world.GetBodyCount, m_world.GetContactCount, m_world.GetJointCount]));
      DrawText(Format('proxies/height/balance/quality = %d/%d/%d/%f',
         [m_world.GetProxyCount, m_world.GetTreeHeight, m_world.GetTreeBalance, m_world.GetTreeQuality]));
      NextLine; // space line
      m_debugDraw.Canvas.DefaultFont.WinColor := clWhite;
   end;

   if Assigned(m_mouseJoint) then
   begin
		  p1 := m_mouseJoint.GetAnchorB;
		  p2 := m_mouseJoint.GetTarget;

      m_debugDraw.DrawPoint(p1, 4.0, clPoint);
      m_debugDraw.DrawPoint(p2, 4.0, clPoint);
      m_debugDraw.DrawSegment(p1, p2, clLine);
   end;

   if m_bombSpawning then
   begin
      m_debugDraw.DrawPoint(m_bombSpawnPoint, 4.0, clBomb);
      m_debugDraw.DrawSegment(m_mouseWorld, m_bombSpawnPoint, clLine);
   end;

   if settings.drawContactPoints then
   begin
      for i := 0 to m_pointCount - 1 do
         with m_points[i] do
         begin
            if state = b2_addState then // Add
               m_debugDraw.DrawPoint(position, 10.0, clAdd)
            else if state = b2_persistState then // Persist
               m_debugDraw.DrawPoint(position, 5.0, clPersist);

            if settings.drawContactNormals then
               {$IFDEF OP_OVERLOAD}
               m_debugDraw.DrawSegment(position, position + k_axisScale * normal, clContactNormal)
               {$ELSE}
               m_debugDraw.DrawSegment(position, Add(position, Multiply(normal, k_axisScale)), clContactNormal)
               {$ENDIF}
            else if settings.drawContactImpulse then
               {$IFDEF OP_OVERLOAD}
               m_debugDraw.DrawSegment(position, position + k_impulseScale * normalImpulse * normal, clContactImpulse)
               {$ELSE}
               m_debugDraw.DrawSegment(position, Add(position, Multiply(normal, k_impulseScale * normalImpulse)), clContactImpulse)
               {$ENDIF}
            else if settings.drawFrictionImpulse then
               {$IFDEF OP_OVERLOAD}
               m_debugDraw.DrawSegment(position, position + k_impulseScale * tangentImpulse * b2Cross(normal, 1.0), clFrictionImpulse);
               {$ELSE}
               m_debugDraw.DrawSegment(position, Add(position, Multiply(b2Cross(normal, 1.0), k_impulseScale * tangentImpulse)), clFrictionImpulse);
               {$ENDIF}
         end;
   end;
end;

procedure TTester.Keyboard(key: Byte);
begin
end;

procedure TTester.KeyboardUp(key: Byte);
begin
end;

type
   TQueryCallback = class(Tb2QueryCallback)
   public
      m_point: TVector2;
      m_fixture: Tb2Fixture;

      procedure Initizlize(const point: TVector2); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      function ReportFixture(fixture: Tb2Fixture): Boolean; override;
   end;

procedure TQueryCallback.Initizlize(const point: TVector2);
begin
   m_point := point;
   m_fixture := nil;
end;

function TQueryCallback.ReportFixture(fixture: Tb2Fixture): Boolean;
begin
   if fixture.GetBody.GetType = b2_dynamicBody then
   begin
      if fixture.TestPoint(m_point) then
      begin
         m_fixture := fixture;
         // We are done, terminate the query.
         Result := False;
         Exit;
      end;
   end;

   // Continue the query.
   Result := True;
end;

var
   _QueryCallback: TQueryCallback;
procedure TTester.MouseDown(const p: TVector2);
const
   k_maxCount = 10;
var
   aabb: Tb2AABB;
   d: TVector2;
   body: Tb2Body;
   md: Tb2MouseJointDef;
begin
   m_mouseWorld := p;
   if Assigned(m_mouseJoint) then
      Exit;

   // Make a small box.
   {$IFDEF OP_OVERLOAD}
   d.SetValue(0.001, 0.001);
   aabb.lowerBound := p - d;
   aabb.upperBound := p + d;
   {$ELSE}
   SetValue(d, 0.001, 0.001);
   aabb.lowerBound := Subtract(p, d);
   aabb.upperBound := Add(p, d);
   {$ENDIF}

   // Query the world for overlapping shapes.
   _QueryCallback.Initizlize(p);
   m_world.QueryAABB(_QueryCallback, aabb);

   if Assigned(_QueryCallback.m_fixture) then
   begin
      body := _QueryCallback.m_fixture.GetBody;
      md := Tb2MouseJointDef.Create;
      md.bodyA := m_groundBody;
      md.bodyB := body;
      md.target := p;
      md.maxForce := 1000.0 * body.GetMass;
      m_mouseJoint := Tb2MouseJoint(m_world.CreateJoint(md));
      body.SetAwake(True);
   end;
end;

procedure TTester.ShiftMouseDown(const p: TVector2);
begin
   m_mouseWorld := p;
   if Assigned(m_mouseJoint) then
      Exit;

   SpawnBomb(p);
end;

procedure TTester.MouseUp(const p: TVector2);
begin
   if Assigned(m_mouseJoint) then
   begin
      m_world.DestroyJoint(m_mouseJoint);
      m_mouseJoint := nil;
   end;

   if m_bombSpawning then
      CompleteBombSpawn(p);
end;

procedure TTester.MouseMove(const p: TVector2);
begin
   m_mouseWorld := p;
	 if Assigned(m_mouseJoint) then
      m_mouseJoint.SetTarget(p);
end;

procedure TTester.LaunchBomb(velocity_factor: PhysicsFloat = 1.0);
var
   p, v: TVector2;
begin
   {$IFDEF OP_OVERLOAD}
	 p.SetValue(RandomFloat(-15, 15), 30.0);
	 v := -5.0 * velocity_factor * p;
   {$ELSE}
	 SetValue(p, RandomFloat(-15, 15), 30.0);
	 v := Multiply(p, -5.0 * velocity_factor);
   {$ENDIF}
	 LaunchBomb(p, v);
end;

procedure TTester.LaunchBomb(const position, velocity: TVector2);
var
   bd: Tb2BodyDef;
   circle: Tb2CircleShape;
   fd: Tb2FixtureDef;
begin
   if Assigned(m_bomb) then
   begin
      m_world.DestroyBody(m_bomb);
      m_bomb := nil;
   end;

   bd := Tb2BodyDef.Create;
   bd.bodyType := b2_dynamicBody;
   bd.position := position;
   bd.bullet := True;
   m_bomb := m_world.CreateBody(bd);
   m_bomb.SetLinearVelocity(velocity);

   circle := Tb2CircleShape.Create;
   circle.m_radius := 0.3;

   fd := Tb2FixtureDef.Create;
   fd.shape := circle;
   fd.density := 20.0;
   fd.restitution := 0.0;

   {b2AABB aabb;
   aabb.lowerBound := position - b2Vec2(0.3f,0.3f);
   aabb.upperBound := position + b2Vec2(0.3f,0.3f);     }
   m_bomb.CreateFixture(fd);
end;

procedure TTester.SpawnBomb(const worldPt: TVector2);
begin
	 m_bombSpawnPoint := worldPt;
	 m_bombSpawning := True;
end;

procedure TTester.CompleteBombSpawn(const p: TVector2);
const
   multiplier = 30.0;
var
   vel: TVector2;
begin
   if not m_bombSpawning then
      Exit;

   {$IFDEF OP_OVERLOAD}
   vel := m_bombSpawnPoint - p;
   vel.MultiplyBy(multiplier);
   {$ELSE}
   vel := Subtract(m_bombSpawnPoint, p);
   MultiplyBy(vel, multiplier);
   {$ENDIF}
   LaunchBomb(m_bombSpawnPoint, vel);
   m_bombSpawning := False;
end;

procedure TTester.SetCanvasTranslation(x, y: PhysicsFloat);
begin
   with m_debugDraw.Canvas do
   begin
      BeginUpdateTransformation;
      SetTranslateX(x);
      SetTranslateX(y);
      EndUpdateTransformation;
   end;
end;

procedure TTester.SetCanvasTranslationOffset(dx, dy: PhysicsFloat);
begin
   with m_debugDraw.Canvas do
   begin
      BeginUpdateTransformation;
      TranslateX := TranslateX + dx;
      TranslateY := TranslateY + dy;
      EndUpdateTransformation;
   end;
end;

procedure TTester.DrawText(const text: string);
begin
   m_debugDraw.Canvas.TextOutASCII(text, 5, m_textLine);
   NextLine;
end;

procedure TTester.JointDestroyed(joint: Tb2Joint);
begin
end;

procedure TTester.PreSolve(var contact: Tb2Contact; const oldManifold: Tb2Manifold);
var
   i: Integer;
   state1, state2: Tb2PointStateArray;
   worldManifold: Tb2WorldManifold;
begin
   if contact.m_manifold.pointCount = 0 then
      Exit;

   b2GetPointStates(state1, state2, oldManifold, contact.m_manifold);
   {$IFDEF OP_OVERLOAD}
   contact.GetWorldManifold(worldManifold);
   {$ELSE}
   GetWorldManifold(contact, worldManifold);
   {$ENDIF}

   i := 0;
   while (i < contact.m_manifold.pointCount) and (m_pointCount < k_maxContactPoints) do
      with m_points[m_pointCount] do
      begin
         fixtureA := contact.m_fixtureA;
         fixtureB := contact.m_fixtureB;
         position := worldManifold.points[i];
         normal := worldManifold.normal;
         state := state2[i];
         normalImpulse := contact.m_manifold.points[i].normalImpulse;
         tangentImpulse := contact.m_manifold.points[i].tangentImpulse;
         Inc(m_pointCount);
         Inc(i);
      end;
end;

procedure TTester.UpdateGravityText;
begin
   frmMain.editGravityX.Text := FloatToStr(m_world.Gravity.x);
   frmMain.editGravityY.Text := FloatToStr(m_world.Gravity.y);
end;

initialization
   RegisterClass(TDrawPanel);
   TestCount := 0;
   ActiveEntry := nil;
   _QueryCallback := TQueryCallback.Create;

finalization
   _QueryCallback.Free;

end.



