unit UPingPongTypes;

interface

{$I ..\..\..\Physics2D\Physics2D.inc}

uses

  UGame,
  System.Generics.Collections, System.Types,
  System.UITypes, UPhysics2D, UPhysics2DTypes;

const
//   PlateColor = Maroon; // 接盘的颜色
//   BallColor = clRed; // 球的颜色

   BallVelocity = 180; // 球的速度
   BallInitialVelocity: TVector2 = (X: 0; Y: BallVelocity);


type
//  TGameState = (gsNotLaunched, gsPlaying, gsPaused, gsGameOver, gsGameFinished);

  TBlock = class;
  TBlockDisappearEvent = procedure(Block: TBlock); // of object;
  TBlockType = (btUnbreakable, btMud, btWood, btSteel);

  TBlock = class
  public
    X, Y: Integer;
    Color: TColor;
    BlockType: TBlockType;
    HP: Integer;
    b2Body: Tb2Body;
    OnDisappeared: TBlockDisappearEvent;
    procedure RestoreHP;
    procedure Knocked;
  end;

  TPlate = record
    X: Integer;
    Width: Integer;
    DrawPoints: TPointsF;
    DrawPointCount: Integer;
  end;

  TBall = record
    X, Y: Double;
  end;

  TMyb2ContactListener = class(Tb2ContactListener)
  public
    procedure BeginContact(var contact: Tb2Contact); override;
  end;

var
  DefaultPlateWidth: Integer;// = 70;
  DefaultPlateHeight: Integer;// = 15;
  DefaultBallRadius: Integer;// = 4;
  MapWidth: Integer;// = 600; // 地图宽度
  MapHeight: Integer;// = 500; // 地图高度
  BlockWidth: Integer;// = 40; // 砖块长度
  BlockHeight: Integer;// = 20; // 砖块宽度
  BlockMinTop: Integer;// = 50; // 砖块的最低高度

  //Map: array[0..MapWidth div BlockHeight - 1] of
   // array[0..MapHeight div BlockHeight - 1] of TBlock;
   Map: array of array of TBlock;

  Life: Integer;
  MapFileName: string;
  EditingMap: Boolean;
  GameState: TGameState;
  Blocks: TList<TBlock>;
  LeftBlockCount: Integer;
  Ball: TBall;
  Plate: TPlate;
  SelectedBlock: TBlock;

  b2World: Tb2World;
  b2WallBody: Tb2Body;
  b2BallBody: Tb2Body;
  b2PlateBody: Tb2Body;

  b2ContactListener: TMyb2ContactListener;

procedure SetMap(X, Y: Integer; Block: TBlock);
procedure FreeAllBlocks;
procedure InitializePhysics;

procedure DoGameProgress(deltaTime: double; cp: TPoint; ADisplayWidth: integer);

procedure GameInit(DPW, DPH, DBR, MW, MH, BW, BH, BMT: Integer);
procedure GameFree;

function GetReverseColor(Color: TColor): TColor;

implementation

uses
  System.Math,
  uPhysics2DHelper;

function GetReverseColor(Color: TColor): TColor;
begin
  Result := Color;
  TAlphaColorRec(Result).R := 255 - TAlphaColorRec(Color).R;
  TAlphaColorRec(Result).G := 255 - TAlphaColorRec(Color).G;
  TAlphaColorRec(Result).B := 255 - TAlphaColorRec(Color).B;
end;

procedure GameInit(DPW, DPH, DBR, MW, MH, BW, BH, BMT: Integer);
begin
  DefaultPlateWidth := DPW;//70;
  DefaultPlateHeight := DPH;//15;
  DefaultBallRadius := DBR;//4;
  MapWidth := MW;//600;
  MapHeight := MH;//500;
  BlockWidth := BW;//40;
  BlockHeight := BH;//20;
  BlockMinTop := BMT;//50;

  SetLength(Map,MapHeight div BlockHeight - 1,MapWidth div BlockHeight - 1);

  MapFileName := '';
  Blocks := TList<TBlock>.Create;
  b2ContactListener := TMyb2ContactListener.Create;
end;

procedure GameFree;
begin
  if Assigned(b2World) then
    b2World.Free;
  FreeAllBlocks;
  b2ContactListener.Free;
  Blocks.Free;
end;

procedure DoGameProgress(deltaTime: double; cp: TPoint; ADisplayWidth: integer);
const
   FixedStep = 1 / 100;
var
  vel: TVector2; dt: Double;
begin
   if EditingMap or (GameState in [gsPaused, gsGameOver]) then
      Exit;

   Plate.X := Max(cp.X, Plate.Width div 2);
   Plate.X := Min(Plate.X, ADisplayWidth - Plate.Width div 2);
   b2PlateBody.SetTransform(MakeVector(Plate.X, 0), 0);

   if GameState = gsNotLaunched then
   begin
     Ball.X := Plate.X;
     Ball.Y := DefaultBallRadius + DefaultPlateHeight;
     b2BallBody.SetTransform(MakeVector(Ball.X, Ball.Y), 0);
   end

   else if GameState = gsPlaying then
   begin
      vel := b2BallBody.GetLinearVelocity;
      {$IFDEF OP_OVERLOAD}
      vel.SetLength(BallVelocity);
      {$ELSE}
      SetLengthVec(vel, BallVelocity);
      {$ENDIF}
      b2BallBody.SetLinearVelocity(vel);

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

      Ball.X := b2BallBody.GetPosition.x;
      Ball.Y := b2BallBody.GetPosition.y;
      if Ball.Y < 0 then
      begin
         Dec(Life);
         if Life = 0 then
           GameState := gsGameOver
         else
           GameState := gsNotLaunched;
      end;
   end;

end;

procedure BlockKnockOut(Block: TBlock);
begin
  Block.b2Body.SetIgnoreColliding(True);
  Dec(LeftBlockCount);
  if LeftBlockCount = 0 then
    GameState := gsGameFinished;
end;

procedure SetMap(X, Y: Integer; Block: TBlock);
begin
  Map[X div BlockHeight][Y div BlockHeight] := Block;
  Map[X div BlockHeight + 1][Y div BlockHeight] := Block;
end;

procedure FreeAllBlocks;
var i: Integer;
begin
  for i := Blocks.Count - 1 downto 0 do
    TObject(Blocks[i]).Free;
  Blocks.Clear;
  SelectedBlock := nil;
  MapFileName := '';
end;

procedure InitializePhysics;
var
  i: Integer;
  bd: Tb2BodyDef;
  fix: Tb2FixtureDef;
  ballShape: Tb2CircleShape;
  blockShape: Tb2PolygonShape;
  Walls: array[0..4] of TPointF;
  PlatePoints: array[0..4] of TPointF;
  b: TBlock;
begin
  if Assigned(b2World) then
    b2World.Free;

  b2World := Tb2World.Create(b2Vec2_Zero);
  b2World.SetContactListener(b2ContactListener);

  bd := Tb2BodyDef.Create;

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

  fix := Tb2FixtureDef.Create;
  ballShape := Tb2CircleShape.Create;
  ballShape.m_radius := DefaultBallRadius;
  fix.shape := ballShape;
  fix.restitution := 1.0;
  fix.density := 1.0;
  bd.bodyType := b2_dynamicBody;
  bd.position.x := Ball.X;
  bd.position.y := Ball.Y;
  b2BallBody := b2World.CreateBody(bd, False);
  b2BallBody.CreateFixture(fix);

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
  bd.bodyType := b2_staticBody;
  bd.position.x := Plate.X;
  bd.position.y := 0;
  b2PlateBody := b2World.CreateBody(bd, False);
  BuildEdgeShapeCurve(@PlatePoints[0], 5, b2PlateBody, 0.5, 0);

  Plate.DrawPointCount := LastGeneratedPointCount;
  SetLength(Plate.DrawPoints, LastGeneratedPointCount);
  Move(LastGeneratedPoints[0], Plate.DrawPoints[0], SizeOf(TPointF) *
    LastGeneratedPointCount);

  blockShape := Tb2PolygonShape.Create;
  blockShape.SetAsBox(BlockWidth / 2, BlockHeight / 2);
  bd.bodyType := b2_staticBody;

  for i := 0 to Blocks.Count - 1 do
  begin
    b := TBlock(Blocks[i]);
    b.OnDisappeared := BlockKnockOut;
    bd.position.x := b.X + BlockWidth / 2;
    bd.position.y := b.Y + BlockHeight / 2;
    b.b2Body := b2World.CreateBody(bd, False);
    b.b2Body.CreateFixture(blockShape, 1, False);
    b.b2Body.UserData := b;
    b.RestoreHP;
  end;
  bd.Free;
  blockShape.Free;
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
end;

{ TMyb2ContactListener }

procedure TMyb2ContactListener.BeginContact(var contact: Tb2Contact);
var bodyA, bodyB: Tb2Body;
begin
  bodyA := contact.m_fixtureA.GetBody;
  bodyB := contact.m_fixtureB.GetBody;

  if (bodyA = b2BallBody) and Assigned(bodyB.UserData) then
    TBlock(bodyB.UserData).Knocked
  else if (bodyB = b2BallBody) and Assigned(bodyA.UserData) then
    TBlock(bodyA.UserData).Knocked;
end;

end.
