unit UCar;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TCar = class(TTester)
   public
      m_car: Tb2Body;
      m_wheel1: Tb2Body;
      m_wheel2: Tb2Body;

      m_hz: PhysicsFloat;
      m_zeta: PhysicsFloat;
      m_speed: PhysicsFloat;

      m_spring1: Tb2WheelJoint;
      m_spring2: Tb2WheelJoint;

      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
   end;

implementation

{ TCar }

constructor TCar.Create;
const
   axis: TVector2 = (X: 0.0; Y: 1.0);
   hs: array[0..9] of PhysicsFloat = (0.25, 1.0, 4.0, 0.0, 0.0, -1.0, -2.0, -2.0, -1.25, 0.0);
var
   shape: Tb2EdgeShape;
   bd: Tb2BodyDef;
   fd: Tb2FixtureDef;
   ground, body, prevBody: Tb2Body;
   box, pShape, chassis: Tb2PolygonShape;
   circle: Tb2CircleShape;
   jd: Tb2WheelJointDef;
   rjd: Tb2RevoluteJointDef;
   anchor: TVector2;
   vertices: array[0..7] of TVector2;

   i: Integer;
   N: Int32;
   x, y1, y2, dx: PhysicsFloat;
begin
   inherited;

   m_hz := 4.0;
   m_zeta := 0.7;
   m_speed := 50.0;

   begin
      bd := Tb2BodyDef.Create;
      ground := m_world.CreateBody(bd);

      shape := Tb2EdgeShape.Create;

      fd := Tb2FixtureDef.Create;
      fd.shape := shape;
      fd.density := 0.0;
      fd.friction := 0.6;

      shape.SetVertices(MakeVector(-20.0, 0.0), MakeVector(20.0, 0.0));
      ground.CreateFixture(fd, False, False);

      x := 20.0;
      y1 := 0.0;
      dx := 5.0;

      for i := 0 to 9 do
      begin
         y2 := hs[i];
         shape.SetVertices(MakeVector(x, y1), MakeVector(x + dx, y2));
         ground.CreateFixture(fd, False, False);
         y1 := y2;
         x := x + dx;
      end;

      for i := 0 to 9 do
      begin
         y2 := hs[i];
         shape.SetVertices(MakeVector(x, y1), MakeVector(x + dx, y2));
         ground.CreateFixture(fd, False, False);
         y1 := y2;
         x := x + dx;
      end;

      shape.SetVertices(MakeVector(x, 0.0), MakeVector(x + 40.0, 0.0));
      ground.CreateFixture(fd, False, False);

      x := x + 80.0;
      shape.SetVertices(MakeVector(x, 0.0), MakeVector(x + 40.0, 0.0));
      ground.CreateFixture(fd, False, False);

      x := x + 40.0;
      shape.SetVertices(MakeVector(x, 0.0), MakeVector(x + 10.0, 5.0));
      ground.CreateFixture(fd, False, False);

      x := x + 20.0;
      shape.SetVertices(MakeVector(x, 0.0), MakeVector(x + 40.0, 0.0));
      ground.CreateFixture(fd, False, False);

      x := x + 40.0;
      shape.SetVertices(MakeVector(x, 0.0), MakeVector(x, 20.0));
      ground.CreateFixture(fd);
   end;

   // Teeter
   begin
      bd := Tb2BodyDef.Create;
      SetValue(bd.position, 140.0, 1.0);
      bd.bodyType := b2_dynamicBody;
      body := m_world.CreateBody(bd);

      box := Tb2PolygonShape.Create;
      box.SetAsBox(10.0, 0.25);
      body.CreateFixture(box, 1.0);

      rjd := Tb2RevoluteJointDef.Create;
      rjd.Initialize(ground, body, body.GetPosition);
      rjd.lowerAngle := -8.0 * Pi / 180.0;
      rjd.upperAngle := 8.0 * Pi / 180.0;
      rjd.enableLimit := True;
      m_world.CreateJoint(rjd);

      body.ApplyAngularImpulse(100.0);
   end;

   // Bridge
   begin
      N := 20;
      pShape := Tb2PolygonShape.Create;
      pShape.SetAsBox(1.0, 0.125);

      fd := Tb2FixtureDef.Create;
      fd.shape := pShape;
      fd.density := 1.0;
      fd.friction := 0.6;

      rjd := Tb2RevoluteJointDef.Create;
      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;

      prevBody := ground;
      for i := 0 to N - 1 do
      begin
         SetValue(bd.position, 161.0 + 2.0 * i, -0.125);
         body := m_world.CreateBody(bd, False);
         body.CreateFixture(fd, False, False);

         anchor := MakeVector(160.0 + 2.0 * i, -0.125);
         rjd.Initialize(prevBody, body, anchor);
         m_world.CreateJoint(rjd, False);

         prevBody := body;
      end;

      anchor := MakeVector(160.0 + 2.0 * N, -0.125);
      rjd.Initialize(prevBody, ground, anchor);
      m_world.CreateJoint(rjd);

      fd.Free;
      bd.Free;
      pShape.Free;
   end;

   // Boxes
   begin
      box := Tb2PolygonShape.Create;
      box.SetAsBox(0.5, 0.5);

      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;

      SetValue(bd.position, 230.0, 0.5);
      body := m_world.CreateBody(bd, False);
      body.CreateFixture(box, 0.5, False);

      SetValue(bd.position, 230.0, 1.5);
      body := m_world.CreateBody(bd, False);
      body.CreateFixture(box, 0.5, False);

      SetValue(bd.position, 230.0, 2.5);
      body := m_world.CreateBody(bd, False);
      body.CreateFixture(box, 0.5);

      SetValue(bd.position, 230.0, 3.5);
			body := m_world.CreateBody(bd, False);
			body.CreateFixture(box, 0.5, False);

			SetValue(bd.position, 230.0, 4.5);
			body := m_world.CreateBody(bd);
			body.CreateFixture(box, 0.5);
   end;

   // Car
   begin
      chassis := Tb2PolygonShape.Create;
      SetValue(vertices[0], -1.5, -0.5);
      SetValue(vertices[1], 1.5, -0.5);
      SetValue(vertices[2], 1.5, 0.0);
      SetValue(vertices[3], 0.0, 0.9);
      SetValue(vertices[4], -1.15, 0.9);
      SetValue(vertices[5], -1.5, 0.2);
      chassis.SetVertices(@vertices[0], 6);

      circle := Tb2CircleShape.Create;
      circle.m_radius := 0.4;

      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      SetValue(bd.position, 0.0, 1.0);
      m_car := m_world.CreateBody(bd, False);
      m_car.CreateFixture(chassis, 1.0);

      fd := Tb2FixtureDef.Create;
      fd.shape := circle;
      fd.density := 1.0;
      fd.friction := 0.9;

      SetValue(bd.position, -1.0, 0.35);
      m_wheel1 := m_world.CreateBody(bd, False);
      m_wheel1.CreateFixture(fd, False, False);

      SetValue(bd.position, 1.0, 0.35);
      m_wheel2 := m_world.CreateBody(bd);
      m_wheel2.CreateFixture(fd);

      jd := Tb2WheelJointDef.Create;
      jd.Initialize(m_car, m_wheel1, m_wheel1.GetPosition, axis);
      jd.motorSpeed := 0.0;
      jd.maxMotorTorque := 20.0;
      jd.enableMotor := True;
      jd.frequencyHz := m_hz;
      jd.dampingRatio := m_zeta;
      m_spring1 := Tb2WheelJoint(m_world.CreateJoint(jd, False));

      jd.Initialize(m_car, m_wheel2, m_wheel2.GetPosition, axis);
      jd.motorSpeed := 0.0;
      jd.maxMotorTorque := 10.0;
      jd.enableMotor := False;
      jd.frequencyHz := m_hz;
      jd.dampingRatio := m_zeta;
      m_spring2 := Tb2WheelJoint(m_world.CreateJoint(jd));
   end;
end;

procedure TCar.Keyboard(key: Byte);
begin
   case key of
      Ord('A'): m_spring1.SetMotorSpeed(m_speed);
      Ord('S'): m_spring1.SetMotorSpeed(0.0);
      Ord('D'): m_spring1.SetMotorSpeed(-m_speed);
      Ord('Q'):
         begin
            m_hz := b2Max(0.0, m_hz - 1.0);
            m_spring1.SpringFrequencyHz := m_hz;
            m_spring2.SpringFrequencyHz := m_hz;
         end;
      Ord('E'):
         begin
            m_hz := m_hz + 1.0;
            m_spring1.SpringFrequencyHz := m_hz;
            m_spring2.SpringFrequencyHz := m_hz;
         end;
   end;
end;

procedure TCar.Step(var settings: TSettings; timeStep: PhysicsFloat);
begin
   inherited;
   DrawText('Keys: left = a, brake = s, right = d, hz down = q, hz up = e');
   DrawText(Format('frequency = %f hz, damping ratio = %f', [m_hz, m_zeta]));
end;

initialization
   RegisterTestEntry('Car', TCar);

end.
