unit URevolute;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TRevolute = class(TTester)
   public
      m_ball: Tb2Body;
      m_joint: Tb2RevoluteJoint;

      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
   end;

implementation

{ TRevolute }

constructor TRevolute.Create;
var
   ground: Tb2Body;
   bd: Tb2BodyDef;
   shape: Tb2EdgeShape;
   cshape: Tb2CircleShape;
   polyshape: Tb2PolygonShape;
   rjd: Tb2RevoluteJointDef;
   fd: Tb2FixtureDef;
   body: Tb2Body;
   verts: array[0..2] of TVector2;
begin
   inherited;
   begin
      bd := Tb2BodyDef.Create;
      ground := m_world.CreateBody(bd);

      shape := Tb2EdgeShape.Create;
      shape.SetVertices(MakeVector(-40.0, 0.0), MakeVector(40.0, 0.0));
      ground.CreateFixture(shape, 0.0);
   end;

   begin
      cshape := Tb2CircleShape.Create;
      cshape.m_radius := 0.5;

      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;

      rjd := Tb2RevoluteJointDef.Create;

      SetValue(bd.position, -10.0, 20.0);
      body := m_world.CreateBody(bd);
      body.CreateFixture(cshape, 5.0);

      body.SetAngularVelocity(100.0);
      body.SetLinearVelocity(MakeVector(-8.0 * 100.0, 0.0));

      rjd.Initialize(ground, body, MakeVector(-10.0, 12.0));
      rjd.motorSpeed := 1.0 * Pi;
      rjd.maxMotorTorque := 10000.0;
      rjd.enableMotor := False;
      rjd.lowerAngle := -0.25 * Pi;
      rjd.upperAngle := 0.5 * Pi;
      rjd.enableLimit := true;
      rjd.collideConnected := true;

      m_joint := Tb2RevoluteJoint(m_world.CreateJoint(rjd));
   end;

   begin
			cshape := Tb2CircleShape.Create;
			cshape.m_radius := 3.0;

      bd := Tb2BodyDef.Create;
			bd.bodyType := b2_dynamicBody;
      SetValue(bd.position, 5.0, 30.0);

      fd := Tb2FixtureDef.Create;
      fd.density := 5.0;
			fd.filter.maskBits := 1;
			fd.shape := cshape;

			m_ball := m_world.CreateBody(bd);
			m_ball.CreateFixture(fd);

      polyshape := Tb2PolygonShape.Create;
			polyshape.SetAsBox(10.0, 0.2, MakeVector(-10.0, 0.0), 0.0);

      bd := Tb2BodyDef.Create;
      SetValue(bd.position, 20.0, 10.0);
			bd.bodyType := b2_dynamicBody;
			bd.bullet := True;
			body := m_world.CreateBody(bd);
			body.CreateFixture(polyshape, 2.0);

      rjd := Tb2RevoluteJointDef.Create;
			rjd.Initialize(ground, body, MakeVector(20.0, 10.0));
			rjd.lowerAngle := -0.25 * Pi;
			rjd.upperAngle := 0.0 * Pi;
			rjd.enableLimit := True;
			m_world.CreateJoint(rjd);
   end;

   // Tests mass computation of a small object far from the origin
   begin
      bd := Tb2BodyDef.Create;
			bd.bodyType := b2_dynamicBody;
			body := m_world.CreateBody(bd);

      polyshape := Tb2PolygonShape.Create;
      verts[0] := MakeVector(17.63, 36.31);
      verts[1] := MakeVector(17.52, 36.69);
      verts[2] := MakeVector(17.19, 36.36);
      polyshape.SetVertices(@verts[0], 3);

      fd := Tb2FixtureDef.Create;
			fd.shape := polyshape;
			fd.density := 1;

			body.CreateFixture(fd);	//assertion hits inside here
   end;
end;

procedure TRevolute.Keyboard(key: Byte);
begin
   case key of
      Ord('L'): m_joint.EnableLimit(not m_joint.IsLimitEnabled);
      Ord('M'): m_joint.EnableMotor(not m_joint.IsMotorEnabled);
   end;
end;

procedure TRevolute.Step(var settings: TSettings; timeStep: PhysicsFloat);
begin
   inherited;
   DrawText('Keys: (l) limits, (m) motor');
end;

initialization
   RegisterTestEntry('Revolute', TRevolute);

end.

