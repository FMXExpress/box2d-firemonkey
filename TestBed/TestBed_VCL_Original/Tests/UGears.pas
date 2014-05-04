unit UGears;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TGears = class(TTester)
   public
      m_joint1, m_joint2: Tb2RevoluteJoint;
      m_joint3: Tb2PrismaticJoint;
      m_joint4, m_joint5: Tb2GearJoint;

      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
   end;

implementation

{ TGears }

constructor TGears.Create;
var
   ground, body1, body2, body3: Tb2Body;
   bd, bd1, bd2, bd3: Tb2BodyDef;
   edge: Tb2EdgeShape;
   box: Tb2PolygonShape;
   circle1, circle2: Tb2CircleShape;
   jd1, jd2: Tb2RevoluteJointDef;
   jd3: Tb2PrismaticJointDef;
   jd4, jd5: Tb2GearJointDef;
   joint1, joint2: Tb2Joint;
begin
   inherited;
   begin
      bd := Tb2BodyDef.Create;
      ground := m_world.CreateBody(bd);

      edge := Tb2EdgeShape.Create;
      edge.SetVertices(MakeVector(50.0, 0.0), MakeVector(-50.0, 0.0));
      ground.CreateFixture(edge, 0.0);
   end;

   begin
      circle1 := Tb2CircleShape.Create;
      circle1.m_radius := 1.0;
      circle2 := Tb2CircleShape.Create;
      circle2.m_radius := 2.0;

      box := Tb2PolygonShape.Create;
      box.SetAsBox(0.5, 5.0);

      bd1 := Tb2bodyDef.Create;
			bd1.bodyType := b2_staticBody;
			SetValue(bd1.position, 10.0, 9.0);
			body1 := m_world.CreateBody(bd1);
			body1.CreateFixture(circle1, 5.0, False);

      bd2 := Tb2BodyDef.Create;
			bd2.bodyType := b2_dynamicBody;
			SetValue(bd2.position, 10.0, 8.0);
      body2 := m_world.CreateBody(bd2);
			body2.CreateFixture(box, 5.0);

      bd3 := Tb2BodyDef.Create;
			bd3.bodyType := b2_dynamicBody;
			SetValue(bd3.position, 10.0, 6.0);
			body3 := m_world.CreateBody(bd3);
			body3.CreateFixture(circle2, 5.0, False);

      jd1 := Tb2RevoluteJointDef.Create;
			jd1.Initialize(body2, body1, bd1.position);
			joint1 := m_world.CreateJoint(jd1);

			jd2 := Tb2RevoluteJointDef.Create;
			jd2.Initialize(body2, body3, bd3.position);
			joint2 := m_world.CreateJoint(jd2);

      jd4 := Tb2GearJointDef.Create;
			jd4.bodyA := body1;
			jd4.bodyB := body3;
			jd4.joint1 := joint1;
			jd4.joint2 := joint2;
			jd4.ratio := circle2.m_radius / circle1.m_radius;
			m_world.CreateJoint(jd4);

      /////////////////////////
      box := Tb2PolygonShape.Create;
      box.SetAsBox(0.5, 5.0);

      bd1 := Tb2BodyDef.Create;
      bd1.bodyType := b2_dynamicBody;
      SetValue(bd1.position, -3.0, 12.0);
      body1 := m_world.CreateBody(bd1);
      body1.CreateFixture(circle1, 5.0, False);

      jd1 := Tb2RevoluteJointDef.Create;
      jd1.bodyA := ground;
      jd1.bodyB := body1;
      jd1.localAnchorA := ground.GetLocalPoint(bd1.position);
      jd1.localAnchorB := body1.GetLocalPoint(bd1.position);
      jd1.referenceAngle := body1.GetAngle - ground.GetAngle;
      m_joint1 := Tb2RevoluteJoint(m_world.CreateJoint(jd1));

      bd2 := Tb2BodyDef.Create;
      bd2.bodyType := b2_dynamicBody;
      SetValue(bd2.position, 0.0, 12.0);
      body2 := m_world.CreateBody(bd2);
      body2.CreateFixture(circle2, 5.0, False);

      jd2 := Tb2RevoluteJointDef.Create;
      jd2.Initialize(ground, body2, bd2.position);
      m_joint2 := Tb2RevoluteJoint(m_world.CreateJoint(jd2));

      bd3 := Tb2BodyDef.Create;
      bd3.bodyType := b2_dynamicBody;
      SetValue(bd3.position, 2.5, 12.0);
      body3 := m_world.CreateBody(bd3);
      body3.CreateFixture(box, 5.0);

      jd3 := Tb2PrismaticJointDef.Create;
      jd3.Initialize(ground, body3, bd3.position, MakeVector(0.0, 1.0));
      jd3.lowerTranslation := -5.0;
      jd3.upperTranslation := 5.0;
      jd3.enableLimit := True;
      m_joint3 := Tb2PrismaticJoint(m_world.CreateJoint(jd3));

      jd4 := Tb2GearJointDef.Create;
      jd4.bodyA := body1;
      jd4.bodyB := body2;
      jd4.joint1 := m_joint1;
      jd4.joint2 := m_joint2;
      jd4.ratio := circle2.m_radius / circle1.m_radius;
      m_joint4 := Tb2GearJoint(m_world.CreateJoint(jd4));

      jd5 := Tb2GearJointDef.Create;
      jd5.bodyA := body2;
      jd5.bodyB := body3;
      jd5.joint1 := m_joint2;
      jd5.joint2 := m_joint3;
      jd5.ratio := -1.0 / circle2.m_radius;
      m_joint5 := Tb2GearJoint(m_world.CreateJoint(jd5));

      circle1.Free;
      circle2.Free;
   end;
end;

procedure TGears.Step(var settings: TSettings; timeStep: PhysicsFloat);
var
   value: PhysicsFloat;
begin
   inherited;
   value := m_joint1.GetJointAngle + m_joint4.Ratio * m_joint2.GetJointAngle;
   DrawText(Format('theta1 + %4.2f * theta2 := %4.2f', [m_joint4.Ratio, value]));

   value := m_joint2.GetJointAngle + m_joint5.Ratio * m_joint3.GetJointTranslation;
   DrawText(Format('theta2 + %4.2f * delta := %4.2f', [m_joint5.Ratio, value]));
end;

initialization
   RegisterTestEntry('Gears', TGears);
end.

