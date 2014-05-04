unit UGraviation;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, UPhysics2DControllers, SysUtils, Math;

{$IFDEF CONTROLLERS}
type
   TGraviation = class(TTester)
   private
      procedure CreateBall;
   public
      gc: Tb2GravityController;

      constructor Create; override;
      destructor Destroy; override;
      procedure Keyboard(key: Byte); override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
   end;
{$ENDIF}

implementation

{$IFDEF CONTROLLERS}
{ TGraviation }

constructor TGraviation.Create;
var
   i: Integer;
   bd: Tb2BodyDef;
   ground: Tb2Body;
   shape: Tb2EdgeShape;
begin
   inherited;
   m_world.SetGravity(b2Vec2_Zero);
   UpdateGravityText;

   begin
      bd := Tb2BodyDef.Create;
      ground := m_world.CreateBody(bd);

      shape := Tb2EdgeShape.Create;

      // Floor
      shape.SetVertices(MakeVector(-10.0, 0.0), MakeVector(10.0, 0.0));
      ground.CreateFixture(shape, 0.0, False, False);

      // Left wall
      shape.SetVertices(MakeVector(-10.0, 0.0), MakeVector(-10.0, 20.0));
      ground.CreateFixture(shape, 0.0, False, False);

      // Right wall
      shape.SetVertices(MakeVector(10.0, 0.0), MakeVector(10.0, 20.0));
      ground.CreateFixture(shape, 0.0, False, False);

      // Roof
      shape.SetVertices(MakeVector(-10.0, 20.0), MakeVector(10.0, 20.0));
      ground.CreateFixture(shape, 0.0);
   end;

   gc := Tb2GravityController.Create;
   m_world.AddController(gc);

   for i := 0 to 10 do
      CreateBall;
end;

procedure TGraviation.CreateBall;
var
   cshape: Tb2CircleShape;
   bd: Tb2BodyDef;
   body: Tb2Body;
begin
   cshape := Tb2CircleShape.Create;
   cshape.m_radius := 0.5;
   bd := Tb2BodyDef.Create;
   bd.bodyType := b2_dynamicBody;
   SetValue(bd.position, RandomFloat(-9, 9), RandomFloat(1, 19));
   body := m_world.CreateBody(bd);
   body.CreateFixture(cshape, 0.0);
   gc.AddBody(body);
end;

destructor TGraviation.Destroy;
begin
   gc.Free;
   inherited;
end;

procedure TGraviation.Keyboard(key: Byte);
begin
   if key = Ord('R') then
      gc.reject := not gc.reject
   else if key = Ord('C') then
      CreateBall;
end;

procedure TGraviation.Step(var settings: TSettings; timeStep: PhysicsFloat);
begin
   inherited;
   DrawText('Press ''R'' to toggle rejection or graviation. ''C'' to create a ball.');
end;

initialization
   RegisterTestEntry('Graviation Controller', TGraviation);
{$ENDIF}

end.

