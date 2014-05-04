unit UTumbler;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TTumbler = class(TTester)
   public
      m_joint: Tb2RevoluteJoint;
      m_count: Int32;

      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
   end;


implementation

{ UTumbler }

const
   e_count = 800;

constructor TTumbler.Create;
var
   ground, body: Tb2Body;
   bd: Tb2BodyDef;
   shape: Tb2PolygonShape;
   jd: Tb2RevoluteJointDef;
begin
   inherited;

   bd := Tb2BodyDef.Create;
   ground := m_world.CreateBody(bd);

   begin
      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      bd.allowSleep := False;
      SetValue(bd.position, 0.0, 10.0);
      body := m_world.CreateBody(bd);

      shape := Tb2PolygonShape.Create;
      shape.SetAsBox(0.5, 10.0, MakeVector( 10.0, 0.0), 0.0);
      body.CreateFixture(shape, 5.0, False);
      shape.SetAsBox(0.5, 10.0, MakeVector(-10.0, 0.0), 0.0);
      body.CreateFixture(shape, 5.0, False);
      shape.SetAsBox(10.0, 0.5, MakeVector(0.0, 10.0), 0.0);
      body.CreateFixture(shape, 5.0, False);
      shape.SetAsBox(10.0, 0.5, MakeVector(0.0, -10.0), 0.0);
      body.CreateFixture(shape, 5.0);

      jd := Tb2RevoluteJointDef.Create;
      jd.bodyA := ground;
      jd.bodyB := body;
      SetValue(jd.localAnchorA, 0.0, 10.0);
      SetValue(jd.localAnchorB, 0.0, 0.0);
      jd.referenceAngle := 0.0;
      jd.motorSpeed := 0.05 * Pi;
      jd.maxMotorTorque := 1e8;
      jd.enableMotor := True;
      m_joint := Tb2RevoluteJoint(m_world.CreateJoint(jd));
   end;

   m_count := 0;
end;

procedure TTumbler.Step(var settings: TSettings; timeStep: PhysicsFloat);
var
   bd: Tb2BodyDef;
   body: Tb2Body;
   shape: Tb2PolygonShape;
begin
   inherited;

   if m_count < e_count then
   begin
      bd := Tb2BodyDef.Create;
			bd.bodyType := b2_dynamicBody;
      SetValue(bd.position, 0.0, 10.0);
			body := m_world.CreateBody(bd);

      shape := Tb2PolygonShape.Create;
			shape.SetAsBox(0.125, 0.125);
			body.CreateFixture(shape, 1.0);

			Inc(m_count);
   end;
end;

initialization
   RegisterTestEntry('Tumbler(stress test)', TTumbler);
end.
