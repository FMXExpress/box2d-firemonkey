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
   // ģʽ��δ��ʼ�������С���ͣ����Ϸ��������Ϸ�ɹ�

   TBlock = class;
   TBlockDisappearEvent = procedure(Block: TBlock) of object;
   TBlockType = (btUnbreakable, btMud, btWood, btSteel);
   // ש������ࣺ������ľͷ��������ľͷ��Ҫײ���Σ��������Σ�
   TBlock = class
   public
      X, Y: Integer; // λ��
      Color: TColor; // ��ɫ
      BlockType: TBlockType; // ����
      HP: Integer; // Ѫ����������ʼΪ1������Ϊ3��ײһ�μ�һ����Ϊ0��ש����ʧ
      b2Body: Tb2Body; // ÿ��ש����box2D��ĸ���
      OnDisappeared: TBlockDisappearEvent;

      procedure RestoreHP; // �ָ�����ֵ
      procedure Knocked; // ����ײ��һ��
   end;

   TPlate = record // ����
      X: Integer;
      Width: Integer; // ���
      DrawPoints: TPointsF; // ���ڻ��ƽ������ߵĵ�
      DrawPointCount: Integer;
   end;

   TBall = record // ��
      X, Y: Double;
   end;

   // ���ڽ�������ķ���������������ɣ���Ҫ���⴦��
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

      Life: Integer; // ʣ��������
      MapFileName: string;
      EditingMap: Boolean; // �Ƿ��ڱ༭��ͼ״̬
      GameState: TGameState; // ��Ϸ״̬
      Blocks: TList; // ����ש��
      LeftBlockCount: Integer;
      Ball: TBall; // ��
      Plate: TPlate;
      SelectedBlock: TBlock; // �༭״̬ʱ��ѡ�е�ש��

      b2World: Tb2World; // box2D����ռ�
      b2WallBody: Tb2Body; // �߿�ĸ���
      b2BallBody: Tb2Body; // ��ĸ���
      b2PlateBody: Tb2Body; // ���̵ĸ���
      b2ContactListener: TMyb2ContactListener;

      procedure InitializePhysics; // ��ʼ������
      procedure Display; // �ػ���Ϸ����
      procedure FreeAllBlocks; // ��������ש��
      procedure SetEditingMap(Editing: Boolean);
      procedure BlockKnockOut(Block: TBlock); // ��һ��ש����ʧʱ��ש��ص�
      procedure ShowSelectedBlockProperty;

      procedure TimerProgress(const deltaTime, newTime: Double); // ��ʱ��
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
   MapWidth = 600; // ��ͼ���
   MapHeight = 500; // ��ͼ�߶�
   BlockWidth = 40; // ש�鳤��
   BlockHeight = 20; // ש����
   BlockMinTop = 50; // ש�����͸߶�

   PlateColor = clMaroon; // ���̵���ɫ
   BallColor = clRed; // �����ɫ

   BallVelocity = 180; // ����ٶ�
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
   // ��ʵ����ֱ�ӵ���frmMain.BlockKnockOut������չʾ��ĵ�������Լ����ʹ�÷���ָ��
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
   Plate.X := MapWidth div 2; // ��������ת���м�
   Ball.X := Plate.X;
   Ball.Y := DefaultBallRadius + DefaultPlateHeight;
   btnPauseResume.Caption := 'Pause';

   if MapFileName = '' then // ����Ĭ�ϵ�ש��
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
            Blocks.Add(ABlock); // ����Ҫ���򣬴���˳����Ǵ����ң����µ���
            ax := ax + BlockWidth;
         end;
         ay := ay + BlockHeight;
      end;
      MapFileName := 'default';
   end;

   // ���㲻�ɻ���ש�������
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
   // ������ʱ��
   MSCadencer := TMSTimer.Create;
   MSCadencer.OnProgress := TimerProgress;
   // ����opengl����
   GLCanvas := TGLCanvas.Create(imgDisplay, True, True, False, True);
   GLCanvas.DefaultFont.WinColor := clBlack;

   b2ContactListener := TMyb2ContactListener.Create;

   OpenDialog.InitialDir := ExtractFileDir(Application.ExeName);
   SaveDialog.InitialDir := OpenDialog.InitialDir;

   SetEditingMap(False); // ����Ϊ��Ϸ״̬��ͬʱ��ʼ����Ϸ
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
   Walls: array[0..4] of TPointF; // �߿�Ķ���
   PlatePoints: array[0..4] of TPointF; // ���̵Ķ���
begin
   if Assigned(b2World) then
      b2World.Free; // box2Dֻ������world���ɣ����е�����Ҳ�ᱻ����

   b2World := Tb2World.Create(b2Vec2_Zero);
   b2World.SetContactListener(b2ContactListener);

   bd := Tb2BodyDef.Create;
   // �����߿�
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

   // ������
   fix := Tb2FixtureDef.Create;
   ballShape := Tb2CircleShape.Create;
   ballShape.m_radius := DefaultBallRadius;
   fix.shape := ballShape;
   fix.restitution := 1.0;
   fix.density := 1.0;
   bd.bodyType := b2_dynamicBody; // ��Ϊ��̬����
   bd.position.x := Ball.X;
   bd.position.y := Ball.Y;
   b2BallBody := b2World.CreateBody(bd, False);
   b2BallBody.CreateFixture(fix);

   // �������̣�����Բ��
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
   bd.bodyType := b2_staticBody; // ����Ϊ��̬���壬�ó����������λ��
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

   // ����ש������
   blockShape := Tb2PolygonShape.Create;
   blockShape.SetAsBox(BlockWidth / 2, BlockHeight / 2);
   bd.bodyType := b2_staticBody; // ש��ҲΪ��̬����
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

function GetReverseColor(Color: TColor): TColor; // ���һ����ɫ�ķ�ɫ
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
   // ��������ש��
   for i := 0 to Blocks.Count - 1 do
      with TBlock(Blocks[i]) do
      begin
         if HP <= 0 then
            Continue;

         GLCanvas.SetBrushColorWin(Color, 255, False).
            FillRect(X + 1, Y + 1, X + BlockWidth - 1, Y + BlockHeight - 1);

         if SelectedBlock = Blocks[i] then // ��һ�����ʾ��ѡ��
         begin
            GLCanvas.SetPenWidth(1).SetPenColorWin(GetReverseColor(Color), 255,
               False).
               Line(X + 1, Y + BlockHeight - 1, X + BlockWidth - 1, Y + 1).
               Line(X + 1, Y + 1, X + BlockWidth - 1, Y + BlockHeight - 1);
         end;
      end;

   // ���ƽ���
   if not EditingMap then
   begin
      with Plate do
      begin
         GLCanvas.TranslateX := X;
         GLCanvas.SetPenWidth(3).SetPenColorWin(PlateColor, 255, False).
            Polyline(TGLPointsF(DrawPoints), DrawPointCount);
         GLCanvas.TranslateX := 0;
      end;

      // ������
      with Ball do
         GLCanvas.SetBrushColorWin(BallColor, 255, False).
            FillEllipse(X, Y, DefaultBallRadius, DefaultBallRadius);

      // ���ƻ�ʣ������
      tmpX := MapWidth - 7;
      for i := 1 to Life - 1 do
      begin
         GLCanvas.FillEllipse(tmpX, MapHeight - DefaultBallRadius - 2,
            DefaultBallRadius, DefaultBallRadius);
         tmpX := tmpX - DefaultBallRadius * 2 - 3;
      end;
   end;

   // �����Ϣ
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

      if not Assigned(SelectedBlock) then // ���û��ѡ��ש�飬���ڿհ״��½�һ��ש��
      begin
         // �ж���굥����λ���ܷ񴴽�ש��
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
      if GameState = gsNotLaunched then // ����
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
      // ����Ϸ�л����༭ģʽ��������ש�������ֵ�ָ�
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
      // �ɱ༭�л�����Ϸģʽ����������Ϸ����
      btnNewGameClick(nil);
   end;
end;

procedure TfrmMain.BlockKnockOut(Block: TBlock);
begin
   Block.b2Body.SetIgnoreColliding(True);
   Dec(LeftBlockCount);
   if LeftBlockCount = 0 then // ����ש�鶼��ʧ����Ϸ�ɹ�
      GameState := gsGameFinished;
end;

procedure TfrmMain.ShowSelectedBlockProperty;
begin
   if Assigned(SelectedBlock) then // ��ʾ��ש������
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
   FixedStep = 1 / 100; // 100��һ��
var
   sp, cp: TPoint;
   vel: TVector2;
   dt: Double;
begin
   if EditingMap or (GameState in [gsPaused, gsGameOver]) then
      Exit;

   GetCursorPos(sp); // ������λ��
   cp := imgDisplay.ScreenToClient(sp); // cpΪ�����imgDisplay�е�ˮƽλ�ã���ʹ����Ƴ�Ҳ��Ч
   // ���½��̵�λ��
   Plate.X := Max(cp.X, Plate.Width div 2);
   Plate.X := Min(Plate.X, imgDisplay.Width - Plate.Width div 2);
   b2PlateBody.SetTransform(MakeVector(Plate.X, 0), 0); // ���½��̵�λ��

   // �������δ��������������λ��
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
      // ��������ٶ�Ϊ�㶨��Ҳ����Ҫ������ٶ���������Ϸ�����л������䣬û��������ʧ
      vel := b2BallBody.GetLinearVelocity;
      {$IFDEF OP_OVERLOAD}
      vel.SetLength(BallVelocity);
      {$ELSE}
      SetLengthVec(vel, BallVelocity);
      {$ENDIF}
      b2BallBody.SetLinearVelocity(vel);

      // ϸ�ֻ�ʱ��
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

      // �������Y�����ж��Ƿ�û�ӵ���
      Ball.X := b2BallBody.GetPosition.x;
      Ball.Y := b2BallBody.GetPosition.y;
      if Ball.Y < 0 then
      begin
         Dec(Life);
         if Life = 0 then // �������꣬��Ϸ����
            GameState := gsGameOver
         else
            GameState := gsNotLaunched; // ���·���
      end;
   end;

   Display;
end;

end.

