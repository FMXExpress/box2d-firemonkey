unit UMotorsAndLimits;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TMotorsAndLimits = class(TTester)
   public
      m_joint1, m_joint2: Tb2RevoluteJoint;
      m_joint3: Tb2PrismaticJoint;

      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
   end;

implementation

{ TMotorsAndLimits }

constructor TMotorsAndLimits.Create;
const
   y = 8.0;
var
   ground, body, prevBody: Tb2Body;
   sd: Tb2PolygonShape;
   bd: Tb2BodyDef;
   fd: Tb2FixtureDef;
   rjd: Tb2RevoluteJointDef;
   pjd: Tb2PrismaticJointDef;
begin
   inherited;
   begin
      sd := Tb2PolygonShape.Create;
      sd.SetAsBox(50.0, 10.0);

      bd := Tb2BodyDef.Create;
      SetValue(bd.position, 0.0, -10.0);
      ground := m_world.CreateBody(bd);
      ground.CreateFixture(sd, 0.0);
   end;

   begin
      sd := Tb2PolygonShape.Create;
      sd.SetAsBox(2.0, 0.5);

      fd := Tb2FixtureDef.Create;
      fd.shape := sd;
      fd.density := 5.0;
      fd.friction := 0.05;

      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      rjd := Tb2RevoluteJointDef.Create;

      prevBody := ground;

      SetValue(bd.position, 3.0, y);
      body := m_world.CreateBody(bd, False);
      body.CreateFixture(fd, False, False);

      rjd.Initialize(prevBody, body, MakeVector(0.0, y));
      rjd.motorSpeed := 1.0 * Pi;
      rjd.maxMotorTorque := 10000.0;
      rjd.enableMotor := True;
      m_joint1 := Tb2RevoluteJoint(m_world.CreateJoint(rjd, False));

      prevBody := body;

      SetValue(bd.position, 9.0, y);
      body := m_world.CreateBody(bd, False);
      body.CreateFixture(fd, False, False);

      rjd.Initialize(prevBody, body, MakeVector(6.0, y));
      rjd.motorSpeed := 0.5 * Pi;
      rjd.maxMotorTorque := 2000.0;
      rjd.enableMotor := True;
      rjd.lowerAngle := -0.5 * Pi;
      rjd.upperAngle := 0.5 * Pi;
      rjd.enableLimit := True;
      m_joint2 := Tb2RevoluteJoint(m_world.CreateJoint(rjd));

      SetValue(bd.position, -10.0, 10.0);
      bd.angle := 0.5 * Pi;
      body := m_world.CreateBody(bd);
      body.CreateFixture(fd);

      pjd := Tb2PrismaticJointDef.Create;
      pjd.Initialize(ground, body, MakeVector(-10.0, 10.0), MakeVector(1.0, 0.0));
      pjd.motorSpeed := 10.0;
      pjd.maxMotorForce := 1000.0;
      pjd.enableMotor := True;
      pjd.lowerTranslation := 0.0;
      pjd.upperTranslation := 20.0;
      pjd.enableLimit := True;

      m_joint3 := Tb2PrismaticJoint(m_world.CreateJoint(pjd));
   end;
end;

procedure TMotorsAndLimits.Step(var settings: TSettings; timeStep: PhysicsFloat);
begin
   inherited;
   DrawText('Keys: (L) limits, (M) motors, (P) prismatic speed');
   DrawText(Format('Motor Torque = %4.0f, %4.0f : Motor Force = %4.0f',
      [m_joint1.GetMotorTorque(DefaultStep), m_joint2.GetMotorTorque(DefaultStep), m_joint3.GetMotorForce(DefaultStep)]));
end;

procedure TMotorsAndLimits.Keyboard(key: Byte);
begin
   case key of
      76{L}:
         begin
            m_joint2.EnableLimit(not m_joint2.IsLimitEnabled);
            m_joint3.EnableLimit(not m_joint3.IsLimitEnabled);
            m_joint2.GetBodyA.SetAwake(True);
            m_joint3.GetBodyB.SetAwake(True);
         end;
      77{M}:
         begin
            m_joint1.EnableMotor(not m_joint1.IsMotorEnabled);
            m_joint2.EnableMotor(not m_joint2.IsMotorEnabled);
            m_joint3.EnableMotor(not m_joint3.IsMotorEnabled);
            m_joint2.GetBodyA.SetAwake(True);
            m_joint3.GetBodyB.SetAwake(True);
         end;
      80{P}:
         begin
			      m_joint3.GetBodyB.SetAwake(True);
			      m_joint3.SetMotorSpeed(-m_joint3.GetMotorSpeed);
         end;
   end;
end;

initialization
   RegisterTestEntry('Motors & Limits', TMotorsAndLimits);

end.

