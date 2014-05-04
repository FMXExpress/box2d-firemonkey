unit UfrmMain;

interface
{$I ..\..\..\Physics2D\Physics2D.inc}

uses
   Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
   Dialogs, StdCtrls, ExtCtrls, Math, UOpenGLCanvas, UPhysics2D, UPhysics2DTypes,
   UPhysics2DHelper;

type
   TGameState = (gsNotLaunched, gsPlaying, gsPaused, gsGameOver,
      gsGameFinished);
   // 模式：未开始、运行中、暂停、游戏结束、游戏成功

   TBlock = class;
   TBlockDisappearEvent = procedure(Block: TBlock) of object;
   TBlockType = (btUnbreakable, btMud, btWood, btSteel);
   // 砖块的种类：泥土、木头、钢铁（木头需要撞两次，钢铁三次）
   TBlock = class
   public
      X, Y: Integer; // 位置
      Color: TColor; // 颜色
      BlockType: TBlockType; // 种类
      HP: Integer; // 血量，泥土初始为1，钢铁为3，撞一次减一；降为0后砖块消失
      b2Body: Tb2Body; // 每个砖块在box2D里的刚体
      OnDisappeared: TBlockDisappearEvent;

      procedure RestoreHP; // 恢复生命值
      procedure Knocked; // 被球撞击一次
   end;

   TPlate = record // 接盘
      X: Integer;
      Width: Integer; // 宽度
      DrawPoints: TPointsF; // 用于绘制接盘曲线的点
      DrawPointCount: Integer;
   end;

   TBall = record // 球
      X, Y: Double;
   end;

   // 由于接盘与球的反弹不符合物体规律，需要特殊处理
   TMyb2ContactListener = class(Tb2ContactListener)
   public
      procedure BeginContact(var contact: Tb2Contact); override;
   end;

   TfrmMain = class(TForm)
      imgDisplay: TImage;
      btnNewGame: TButton;
      clrBlockColor: TColorBox;
      cboBlockType: TComboBox;
      Bevel1: TBevel;
      btnPauseResume: TButton;
      btnLoadMap: TButton;
      btnSaveMap: TButton;
      chkEditMode: TCheckBox;
      OpenDialog: TOpenDialog;
      SaveDialog: TSaveDialog;
      procedure FormCreate(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure chkEditModeClick(Sender: TObject);
      procedure btnNewGameClick(Sender: TObject);
      procedure btnPauseResumeClick(Sender: TObject);
      procedure imgDisplayMouseDown(Sender: TObject; Button: TMouseButton;
         Shift: TShiftState; AX, AY: Integer);
      procedure clrBlockColorChange(Sender: TObject);
      procedure cboBlockTypeChange(Sender: TObject);
      procedure btnSaveMapClick(Sender: TObject);
      procedure btnLoadMapClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
   private
      { Private declarations }
      GLCanvas: TGLCanvas;

      Life: Integer; // 剩余生命数
      MapFileName: string;
      EditingMap: Boolean; // 是否在编辑地图状态
      GameState: TGameState; // 游戏状态
      Blocks: TList; // 所有砖块
      LeftBlockCount: Integer;
      Ball: TBall; // 球
      Plate: TPlate;
      SelectedBlock: TBlock; // 编辑状态时被选中的砖块

      b2World: Tb2World; // box2D物理空间
      b2WallBody: Tb2Body; // 边框的刚体
      b2BallBody: Tb2Body; // 球的刚体
      b2PlateBody: Tb2Body; // 接盘的刚体
      b2ContactListener: TMyb2ContactListener;

      procedure InitializePhysics; // 初始化物理
      procedure Display; // 重绘游戏场景
      procedure FreeAllBlocks; // 销毁所有砖块
      procedure SetEditingMap(Editing: Boolean);
      procedure BlockKnockOut(Block: TBlock); // 当一个砖块消失时由砖块回调
      procedure ShowSelectedBlockProperty;

      procedure TimerProgress(const deltaTime, newTime: Double); // 计时器
   public
      { Public declarations }
   end;

var
   frmMain: TfrmMain;

implementation
uses
   MSTimer;

{$R *.dfm}

const
   DefaultPlateWidth = 70;
   DefaultPlateHeight = 15;
   DefaultBallRadius = 4;
   MapWidth = 600; // 地图宽度
   MapHeight = 500; // 地图高度
   BlockWidth = 40; // 砖块长度
   BlockHeight = 20; // 砖块宽度
   BlockMinTop = 50; // 砖块的最低高度

   PlateColor = clMaroon; // 接盘的颜色
   BallColor = clRed; // 球的颜色

   BallVelocity = 180; // 球的速度
   BallInitialVelocity: TVector2 = (X: 0; Y: BallVelocity);

var
   Map: array[0..MapWidth div BlockHeight - 1] of array[0..MapHeight div
      BlockHeight - 1] of TBlock;

procedure SetMap(X, Y: Integer; Block: TBlock);
begin
   Map[X div BlockHeight][Y div BlockHeight] := Block;
   Map[X div BlockHeight + 1][Y div BlockHeight] := Block;
end;

{ TBlock }

procedure TBlock.RestoreHP;
begin
   HP := Ord(BlockType);
   if BlockType = btUnbreakable then
      HP := MaxInt;
end;

procedure TBlock.Knocked;
begin
   Dec(HP);
   if HP = 0 then
      if Assigned(OnDisappeared) then
         OnDisappeared(Self);
   // 其实可以直接调用frmMain.BlockKnockOut，这里展示类的低耦合性以及如何使用方法指针
end;

{ TMyb2ContactListener }

procedure TMyb2ContactListener.BeginContact(var contact: Tb2Contact);
var
   bodyA, bodyB: Tb2Body;
begin
   bodyA := contact.m_fixtureA.GetBody;
   bodyB := contact.m_fixtureB.GetBody;

   if (bodyA = frmMain.b2BallBody) and Assigned(bodyB.UserData) then
      TBlock(bodyB.UserData).Knocked
   else if (bodyB = frmMain.b2BallBody) and Assigned(bodyA.UserData) then
      TBlock(bodyA.UserData).Knocked;
end;

{ TfrmMain }

procedure TfrmMain.btnLoadMapClick(Sender: TObject);
var
   i, blockCount: Integer;
   stream: TMemoryStream;
   block: TBlock;
begin
   if OpenDialog.Execute then
   begin
      FreeAllBlocks;
      MapFileName := OpenDialog.FileName;
      stream := TMemoryStream.Create;
      try
         try
            stream.LoadFromFile(MapFileName);
            stream.Read(blockCount, SizeOf(blockCount));
            for i := 0 to blockCount - 1 do
            begin
               block := TBlock.Create;
               with block do
               begin
                  stream.Read(X, SizeOf(X));
                  stream.Read(Y, SizeOf(Y));
                  stream.Read(Color, SizeOf(Color));
                  stream.Read(BlockType, SizeOf(BlockType));
               end;
               Blocks.Add(block);
            end;
            if EditingMap then
               SetEditingMap(True)
            else
               btnNewGameClick(nil);
         except
            ShowMessage('Corrupted map file.');
            FreeAllBlocks;
            if EditingMap then
               SetEditingMap(True)
            else
               btnNewGameClick(nil);
         end;
      finally
         stream.Free;
      end;
   end;
end;

procedure TfrmMain.btnNewGameClick(Sender: TObject);
var
   i, t: Integer;
   ax, ay: Integer;
   ABlock: TBlock;
begin
   GameState := gsNotLaunched;
   Life := 3;
   Plate.Width := DefaultPlateWidth;
   Plate.X := MapWidth div 2; // 将接盘旋转在中间
   Ball.X := Plate.X;
   Ball.Y := DefaultBallRadius + DefaultPlateHeight;
   btnPauseResume.Caption := 'Pause';

   if MapFileName = '' then // 创建默认的砖块
   begin
      if Assigned(b2World) then
         FreeAndNil(b2World);
      FreeAllBlocks;
      ay := 280;
      for i := 0 to 4 do
      begin
         ax := (MapWidth - BlockWidth * 9) div 2;
         for t := 0 to 8 do
         begin
            ABlock := TBlock.Create;
            with ABlock do
            begin
               X := ax;
               Y := ay;
               Color := clBlue;
               BlockType := btMud;
            end;
            Blocks.Add(ABlock); // 不需要排序，创建顺序就是从左到右，从下到上
            ax := ax + BlockWidth;
         end;
         ay := ay + BlockHeight;
      end;
      MapFileName := 'default';
   end;

   // 计算不可击碎砖块的数量
   LeftBlockCount := Blocks.Count;
   for i := 0 to Blocks.Count - 1 do
      if TBlock(Blocks[i]).BlockType = btUnbreakable then
         Dec(LeftBlockCount);

   InitializePhysics;
   Display;
   MSCadencer.Enabled := True;
end;

procedure TfrmMain.btnPauseResumeClick(Sender: TObject);
begin
   if EditingMap or (GameState in [gsNotLaunched, gsGameOver]) then
      Exit;
   if GameState = gsPaused then
   begin
      GameState := gsPlaying;
      btnPauseResume.Caption := 'Pause';
   end
   else
   begin
      GameState := gsPaused;
      btnPauseResume.Caption := 'Resume';
   end;
   MSCadencer.Enabled := GameState = gsPlaying;
end;

procedure TfrmMain.btnSaveMapClick(Sender: TObject);
var
   i: Integer;
   fn: string;
   stream: TMemoryStream;
begin
   if SaveDialog.Execute then
   begin
      fn := SaveDialog.FileName;
      stream := TMemoryStream.Create;
      try
         i := Blocks.Count;
         stream.Write(i, SizeOf(i));
         for i := 0 to Blocks.Count - 1 do
            with TBlock(Blocks[i]) do
            begin
               stream.Write(X, SizeOf(X));
               stream.Write(Y, SizeOf(Y));
               stream.Write(Color, SizeOf(Color));
               stream.Write(BlockType, SizeOf(BlockType));
            end;
         stream.SaveToFile(fn);
      finally
         stream.Free;
      end;
   end;
end;

procedure TfrmMain.cboBlockTypeChange(Sender: TObject);
begin
   if EditingMap and Assigned(SelectedBlock) then
      SelectedBlock.BlockType := TBlockType(cboBlockType.ItemIndex);
end;

procedure TfrmMain.chkEditModeClick(Sender: TObject);
begin
   SetEditingMap(chkEditMode.Checked);
end;

procedure TfrmMain.clrBlockColorChange(Sender: TObject);
begin
   if EditingMap and Assigned(SelectedBlock) then
   begin
      SelectedBlock.Color := clrBlockColor.Selected;
      Display;
   end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
   MapFileName := '';
   Blocks := TList.Create;
   // 创建计时器
   MSCadencer := TMSTimer.Create;
   MSCadencer.OnProgress := TimerProgress;
   // 创建opengl画布
   GLCanvas := TGLCanvas.Create(imgDisplay, True, True, False, True);
   GLCanvas.DefaultFont.WinColor := clBlack;

   b2ContactListener := TMyb2ContactListener.Create;

   OpenDialog.InitialDir := ExtractFileDir(Application.ExeName);
   SaveDialog.InitialDir := OpenDialog.InitialDir;

   SetEditingMap(False); // 设置为游戏状态，同时开始新游戏
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
   if Assigned(b2World) then
      b2World.Free;
   FreeAllBlocks;
   MSCadencer.Free;
   GLCanvas.Free;
   b2ContactListener.Free;
   Blocks.Free;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if EditingMap and Assigned(SelectedBlock) and (Key = 46) and
      (MessageDlg('Really want to delete selected block?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then  // delete
   begin
      SetMap(SelectedBlock.X, SelectedBlock.Y, nil);
      Blocks.Remove(SelectedBlock);
      FreeAndNil(SelectedBlock);
      ShowSelectedBlockProperty;
      Display;
   end;
end;

procedure TfrmMain.InitializePhysics;
var
   i: Integer;
   bd: Tb2BodyDef;
   fix: Tb2FixtureDef;
   ballShape: Tb2CircleShape;
   blockShape: Tb2PolygonShape;
   Walls: array[0..4] of TPointF; // 边框的顶点
   PlatePoints: array[0..4] of TPointF; // 接盘的顶点
begin
   if Assigned(b2World) then
      b2World.Free; // box2D只需销毁world即可，其中的物体也会被销毁

   b2World := Tb2World.Create(b2Vec2_Zero);
   b2World.SetContactListener(b2ContactListener);

   bd := Tb2BodyDef.Create;
   // 创建边框
   Walls[0].x := 0;
   Walls[0].y := -5;
   Walls[1].x := 0;
   Walls[1].y := MapHeight;
   Walls[2].x := MapWidth;
   Walls[2].y := MapHeight;
   Walls[3].x := MapWidth;
   Walls[3].y := -5;
   bd.bodyType := b2_staticBody;
   b2WallBody := b2World.CreateBody(bd, False);
   BuildEdgeShapeCurve(@Walls[0], 4, b2WallBody, 100, 0);

   // 创建球
   fix := Tb2FixtureDef.Create;
   ballShape := Tb2CircleShape.Create;
   ballShape.m_radius := DefaultBallRadius;
   fix.shape := ballShape;
   fix.restitution := 1.0;
   fix.density := 1.0;
   bd.bodyType := b2_dynamicBody; // 球为动态物体
   bd.position.x := Ball.X;
   bd.position.y := Ball.Y;
   b2BallBody := b2World.CreateBody(bd, False);
   b2BallBody.CreateFixture(fix);

   // 创建接盘，近似圆弧
   PlatePoints[0].x := -DefaultPlateWidth / 2;
   PlatePoints[0].y := 0;
   PlatePoints[1].x := -DefaultPlateWidth / 4;
   PlatePoints[1].y := DefaultPlateHeight / 1.4;
   PlatePoints[2].x := 0;
   PlatePoints[2].y := DefaultPlateHeight;
   PlatePoints[3].x := DefaultPlateWidth / 4;
   PlatePoints[3].y := DefaultPlateHeight / 1.4;
   PlatePoints[4].x := DefaultPlateWidth / 2;
   PlatePoints[4].y := 0;
   bd.bodyType := b2_staticBody; // 接盘为静态物体，用程序控制它的位置
   bd.position.x := Plate.X;
   bd.position.y := 0;
   b2PlateBody := b2World.CreateBody(bd, False);
   BuildEdgeShapeCurve(@PlatePoints[0], 5, b2PlateBody, 0.5, 0);
   with Plate do
   begin
      DrawPointCount := LastGeneratedPointCount;
      SetLength(DrawPoints, LastGeneratedPointCount);
      Move(LastGeneratedPoints[0], DrawPoints[0], SizeOf(TPointF) *
         LastGeneratedPointCount);
   end;

   // 创建砖块物体
   blockShape := Tb2PolygonShape.Create;
   blockShape.SetAsBox(BlockWidth / 2, BlockHeight / 2);
   bd.bodyType := b2_staticBody; // 砖块也为静态物体
   for i := 0 to Blocks.Count - 1 do
      with TBlock(Blocks[i]) do
      begin
         OnDisappeared := BlockKnockOut;
         bd.position.x := X + BlockWidth / 2;
         bd.position.y := Y + BlockHeight / 2;
         b2Body := b2World.CreateBody(bd, False);
         b2Body.CreateFixture(blockShape, 1, False);
         b2Body.UserData := Blocks[i];
         RestoreHP;
      end;
   bd.Free;
   blockShape.Free;
end;

function GetReverseColor(Color: TColor): TColor; // 获得一个颜色的反色
begin
   Result := RGB(255 - GetRValue(Color), 255 - GetGValue(Color), 255 -
      GetBValue(Color));
end;

procedure TfrmMain.Display;
var
   i: Integer;
   tmpX: Single;
begin
   GLCanvas.RenderingBegin(clWhite);
   // 绘制所有砖块
   for i := 0 to Blocks.Count - 1 do
      with TBlock(Blocks[i]) do
      begin
         if HP <= 0 then
            Continue;

         GLCanvas.SetBrushColorWin(Color, 255, False).
            FillRect(X + 1, Y + 1, X + BlockWidth - 1, Y + BlockHeight - 1);

         if SelectedBlock = Blocks[i] then // 画一个框表示被选中
         begin
            GLCanvas.SetPenWidth(1).SetPenColorWin(GetReverseColor(Color), 255,
               False).
               Line(X + 1, Y + BlockHeight - 1, X + BlockWidth - 1, Y + 1).
               Line(X + 1, Y + 1, X + BlockWidth - 1, Y + BlockHeight - 1);
         end;
      end;

   // 绘制接盘
   if not EditingMap then
   begin
      with Plate do
      begin
         GLCanvas.TranslateX := X;
         GLCanvas.SetPenWidth(3).SetPenColorWin(PlateColor, 255, False).
            Polyline(TGLPointsF(DrawPoints), DrawPointCount);
         GLCanvas.TranslateX := 0;
      end;

      // 绘制球
      with Ball do
         GLCanvas.SetBrushColorWin(BallColor, 255, False).
            FillEllipse(X, Y, DefaultBallRadius, DefaultBallRadius);

      // 绘制还剩几个球
      tmpX := MapWidth - 7;
      for i := 1 to Life - 1 do
      begin
         GLCanvas.FillEllipse(tmpX, MapHeight - DefaultBallRadius - 2,
            DefaultBallRadius, DefaultBallRadius);
         tmpX := tmpX - DefaultBallRadius * 2 - 3;
      end;
   end;

   // 输出信息
   if not EditingMap then
      if GameState = gsNotLaunched then
         GLCanvas.TextOut('Launch the ball by left clicking.', 2, MapHeight - 11)
      else if GameState = gsGameOver then
         GLCanvas.TextOut('Game Over', 2, MapHeight - 11)
      else if GameState = gsGameFinished then
         GLCanvas.TextOut('Game Success', 2, MapHeight - 11);

   GLCanvas.RenderingEnd;
end;

procedure TfrmMain.FreeAllBlocks;
var
   i: Integer;
begin
   for i := Blocks.Count - 1 downto 0 do
      TObject(Blocks[i]).Free;
   Blocks.Clear;
   SelectedBlock := nil;
   MapFileName := '';
end;

procedure TfrmMain.imgDisplayMouseDown(Sender: TObject; Button: TMouseButton;
   Shift: TShiftState; AX, AY: Integer);
var
   i: Integer;
begin
   if EditingMap then
   begin
      AY := MapHeight - AY;
      SelectedBlock := Map[AX div BlockHeight, AY div BlockHeight];

      if not Assigned(SelectedBlock) then // 如果没有选择砖块，则在空白处新建一个砖块
      begin
         // 判断鼠标单击的位置能否创建砖块
         if AY < BlockMinTop then
            ShowMessage('The brick cannot be too low.')
         else if (AX >= MapWidth - BlockWidth / 2) or
            Assigned(Map[AX div BlockHeight + 1][AY div BlockHeight]) then
            ShowMessage('Cannot add a block here.')
         else
         begin
            SelectedBlock := TBlock.Create;
            with SelectedBlock do
            begin
               X := (AX div BlockHeight) * BlockHeight;
               Y := (AY div BlockHeight) * BlockHeight;
               Color := clBlue;
               BlockType := btMud;
               RestoreHP;
            end;
            Blocks.Add(SelectedBlock);
            SetMap(AX, AY, SelectedBlock);
         end;
      end;

      ShowSelectedBlockProperty;
      Display;
   end
   else
   begin
      if GameState = gsNotLaunched then // 发球
      begin
         b2BallBody.SetLinearVelocity(BallInitialVelocity);
         GameState := gsPlaying;
         MSCadencer.Enabled := True;
      end;
   end;
end;

procedure TfrmMain.SetEditingMap(Editing: Boolean);
var
   i: Integer;
begin
   EditingMap := Editing;
   SelectedBlock := nil;

   if chkEditMode.Checked <> Editing then
      chkEditMode.Checked := Editing;
   clrBlockColor.Enabled := Editing;
   cboBlockType.Enabled := Editing;
   btnSaveMap.Enabled := Editing;
   btnPauseResume.Enabled := not Editing;
   btnNewGame.Enabled := not Editing;

   if Editing then
   begin
      MSCadencer.Enabled := False;
      FillChar(Map, SizeOf(Map), 0);
      // 由游戏切换到编辑模式，将所有砖块的生命值恢复
      for i := 0 to Blocks.Count - 1 do
         with TBlock(Blocks[i]) do
         begin
            RestoreHP;
            SetMap(X, Y, TBlock(Blocks[i]));
         end;

      Display;
   end
   else
   begin
      // 由编辑切换到游戏模式，调用新游戏函数
      btnNewGameClick(nil);
   end;
end;

procedure TfrmMain.BlockKnockOut(Block: TBlock);
begin
   Block.b2Body.SetIgnoreColliding(True);
   Dec(LeftBlockCount);
   if LeftBlockCount = 0 then // 所有砖块都消失，游戏成功
      GameState := gsGameFinished;
end;

procedure TfrmMain.ShowSelectedBlockProperty;
begin
   if Assigned(SelectedBlock) then // 显示该砖块属性
   begin
      cboBlockType.ItemIndex := Ord(SelectedBlock.BlockType);
      clrBlockColor.Selected := SelectedBlock.Color;
   end
   else
   begin
      cboBlockType.ItemIndex := -1;
      clrBlockColor.Selected := clNone;
   end;
end;

procedure TfrmMain.TimerProgress(const deltaTime, newTime: Double);
const
   FixedStep = 1 / 100; // 100分一秒
var
   sp, cp: TPoint;
   vel: TVector2;
   dt: Double;
begin
   if EditingMap or (GameState in [gsPaused, gsGameOver]) then
      Exit;

   GetCursorPos(sp); // 获得鼠标位置
   cp := imgDisplay.ScreenToClient(sp); // cp为鼠标在imgDisplay中的水平位置，即使鼠标移出也有效
   // 更新接盘的位置
   Plate.X := Max(cp.X, Plate.Width div 2);
   Plate.X := Min(Plate.X, imgDisplay.Width - Plate.Width div 2);
   b2PlateBody.SetTransform(MakeVector(Plate.X, 0), 0); // 更新接盘的位置

   // 如果球仍未发出，则更新球的位置
   if GameState = gsNotLaunched then
   begin
      with Ball do
      begin
         X := Plate.X;
         Y := DefaultBallRadius + DefaultPlateHeight;
         b2BallBody.SetTransform(MakeVector(X, Y), 0);
      end;
   end
   else if GameState = gsPlaying then
   begin
      // 设置球的速度为恒定，也许不需要，球的速度在整个游戏过程中基本不变，没有能量损失
      vel := b2BallBody.GetLinearVelocity;
      {$IFDEF OP_OVERLOAD}
      vel.SetLength(BallVelocity);
      {$ELSE}
      SetLengthVec(vel, BallVelocity);
      {$ENDIF}
      b2BallBody.SetLinearVelocity(vel);

      // 细分化时间
      if deltaTime < 1.2 * FixedStep then
         b2World.Step(deltaTime, 8, 3)
      else
      begin
         dt := deltaTime;
         while dt > 0 do
         begin
            b2World.Step(FixedStep, 8, 3);
            dt := dt - FixedStep;
         end;
      end;

      // 根据球的Y坐标判断是否没接到球
      Ball.X := b2BallBody.GetPosition.x;
      Ball.Y := b2BallBody.GetPosition.y;
      if Ball.Y < 0 then
      begin
         Dec(Life);
         if Life = 0 then // 接盘用完，游戏结束
            GameState := gsGameOver
         else
            GameState := gsNotLaunched; // 重新发球
      end;
   end;

   Display;
end;

end.

