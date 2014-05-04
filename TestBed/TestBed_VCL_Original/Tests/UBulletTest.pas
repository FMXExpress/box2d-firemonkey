unit UBulletTest;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TBulletTest = class(TTester)
   private
      procedure Launch;
   public
      m_body, m_bullet: Tb2Body;
      m_x: PhysicsFloat;

      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
   end;

implementation

{ TBulletTest }

constructor TBulletTest.Create;
var
   bd: Tb2BodyDef;
   body: Tb2Body;
   box: Tb2PolygonShape;
   edge: Tb2EdgeShape;
begin
   inherited;
   begin
      bd := Tb2BodyDef.Create;
      SetValue(bd.position, 0.0, 0.0);
      body := m_world.CreateBody(bd);

      edge := Tb2EdgeShape.Create;
      edge.SetVertices(MakeVector(-10.0, 0.0), MakeVector(10.0, 0.0));
      body.CreateFixture(edge, 0.0);

      box := Tb2PolygonShape.Create;
      box.SetAsBox(0.2, 1.0, MakeVector(0.5, 1.0), 0.0);
      body.CreateFixture(box, 0.0);
   end;

   begin
      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      SetValue(bd.position, 0.0, 4.0);

      box := Tb2PolygonShape.Create;
      box.SetAsBox(2.0, 0.1);

      m_body := m_world.CreateBody(bd, False);
      m_body.CreateFixture(box, 1.0, False);

      box.SetAsBox(0.25, 0.25);

      //m_x := RandomFloat(-1.0, 1.0);
      m_x := 0.20352793;
      SetValue(bd.position, m_x, 10.0);
      bd.bullet := True;

      m_bullet := m_world.CreateBody(bd);
      m_bullet.CreateFixture(box, 100.0);

      m_bullet.SetLinearVelocity(MakeVector(0.0, -50.0));
   end;
end;

procedure TBulletTest.Launch;
begin
   m_body.SetTransform(MakeVector(0.0, 4.0), 0.0);
   m_body.SetLinearVelocity(b2Vec2_zero);
   m_body.SetAngularVelocity(0.0);

   m_x := RandomFloat(-1.0, 1.0);
   m_bullet.SetTransform(MakeVector(m_x, 10.0), 0.0);
   m_bullet.SetLinearVelocity(MakeVector(0.0, -50.0));
   m_bullet.SetAngularVelocity(0.0);

   b2_gjkCalls := 0;
   b2_gjkIters := 0;
   b2_gjkMaxIters := 0;

   b2_toiCalls := 0;
   b2_toiIters := 0;
   b2_toiMaxIters := 0;
   b2_toiRootIters := 0;
   b2_toiMaxRootIters := 0;
end;

procedure TBulletTest.Step(var settings: TSettings; timeStep: PhysicsFloat);
begin
   inherited;

   if b2_gjkCalls > 0 then
      DrawText(Format('gjk calls := %d, ave gjk iters := %3.1f, max gjk iters := %d',
         [b2_gjkCalls, b2_gjkIters / b2_gjkCalls, b2_gjkMaxIters]));

   if b2_toiCalls > 0 then
   begin
      DrawText(Format('toi calls := %d, ave toi iters := %3.1f, max toi iters := %d',
         [b2_toiCalls, b2_toiIters / b2_toiCalls, b2_toiMaxRootIters]));

      DrawText(Format('ave toi root iters := %3.1f, max toi root iters := %d',
         [b2_toiRootIters / b2_toiCalls, b2_toiMaxRootIters]));
   end;

   if m_stepCount mod 60 = 0 then
      Launch;
end;

initialization
   RegisterTestEntry('Bullet Test', TBulletTest);

end.

