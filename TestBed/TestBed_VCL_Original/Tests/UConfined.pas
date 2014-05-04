unit UConfined;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TConfined = class(TTester)
   private
      procedure CreateCircle;
   public
      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
   end;

implementation

{ TConfined }

constructor TConfined.Create;
const
   radius = 0.5;
   e_columnCount = 0;
   e_rowCount = 0;
var
   i, j: Integer;
   bd: Tb2BodyDef;
   ground: Tb2Body;
   shape: Tb2EdgeShape;
   cshape: Tb2CircleShape;
   fd: Tb2FixtureDef;
begin
   inherited;
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

   cshape := Tb2CircleShape.Create;
   cshape.m_p := b2Vec2_Zero;
   cshape.m_radius := radius;

   fd := Tb2FixtureDef.Create;
   fd.shape := cshape;
   fd.density := 1.0;
   fd.friction := 0.1;

   bd := Tb2BodyDef.Create;
   bd.bodyType := b2_dynamicBody;
   for j := 0 to e_columnCount - 1 do
     for i := 0 to e_rowCount - 1 do
     begin
        SetValue(bd.position, -10.0 + (2.1 * j + 1.0 + 0.01 * i) * radius, (2.0 * i + 1.0) * radius);
        m_world.CreateBody(bd, False).CreateFixture(fd, False, False);
     end;
   bd.Free;
   fd.Free;
   cshape.Free;

   m_world.SetGravity(b2Vec2_Zero);
   UpdateGravityText;
end;

procedure TConfined.Step(var settings: TSettings; timeStep: PhysicsFloat);
var
   b: Tb2Body;
   sleeping: Boolean;
   p: TVector2;
begin
   sleeping := True;
   b := m_world.GetBodyList;
   while Assigned(b) do
   begin
      if b.GetType <> b2_dynamicBody then
      begin
         b := b.GetNext;
         Continue;
      end;

      if b.IsAwake then
         sleeping := False;

      b := b.GetNext;
   end;

   if m_stepCount = 180 then
      m_stepCount := m_stepCount + 0;

   //if (sleeping)
   //{
   //	CreateCircle();
   //}

   inherited;

   b := m_world.GetBodyList;
   while Assigned(b) do
   begin
      if b.GetType <> b2_dynamicBody then
      begin
         b := b.GetNext;
         Continue;
      end;

      p := b.GetPosition;
      if (p.x <= -10.0) or (10.0 <= p.x) or (p.y <= 0.0) or (20.0 <= p.y) then
         p.x := p.x + 0.0;
      b := b.GetNext;
   end;

   DrawText('Press ''c'' to create a circle.');
end;

procedure TConfined.CreateCircle;
const
   radius = 2.0;
var
   shape: Tb2CircleShape;
   fd: Tb2FixtureDef;
   bd: Tb2BodyDef;
   body: Tb2Body;
   p: TVector2;
begin
   shape := Tb2CircleShape.Create;
   shape.m_p := b2Vec2_Zero;
   shape.m_radius := radius;

   fd := Tb2FixtureDef.Create;
   fd.shape := shape;
   fd.density := 1.0;
   fd.friction := 0.0;

   SetValue(p, RandomFloat, 3 + RandomFloat);
   bd := Tb2BodyDef.Create;
   bd.bodyType := b2_dynamicBody;
	 bd.position := p;
   body := m_world.CreateBody(bd);
   body.CreateFixture(fd);
end;

procedure TConfined.Keyboard(key: Byte);
begin
   case key of
      Ord('C'): CreateCircle;
   end;
end;

initialization
   RegisterTestEntry('Confined', TConfined);

end.

