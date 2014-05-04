unit UBodyTypes;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TBodyTypes = class(TTester)
   public
      m_attachment, m_platform: Tb2Body;
	    m_speed: PhysicsFloat;

      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
   end;

implementation

{ TBodyTypes }

constructor TBodyTypes.Create;
var
   ground, body: Tb2Body;
   bd: Tb2BodyDef;
   shape: Tb2PolygonShape;
   edge: Tb2EdgeShape;
   fd: Tb2FixtureDef;
   rjd: Tb2RevoluteJointDef;
   pjd: Tb2PrismaticJointDef;
begin
   inherited;
   begin
      bd := Tb2BodyDef.Create;
      ground := m_world.CreateBody(bd);

      edge := Tb2EdgeShape.Create;
      edge.SetVertices(MakeVector(-20.0, 0.0), MakeVector(20.0, 0.0));

      fd := Tb2FixtureDef.Create;
      fd.shape := edge;

      ground.CreateFixture(fd);
   end;

   // Define attachment
   begin
      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      SetValue(bd.position, 0.0, 3.0);
      m_attachment := m_world.CreateBody(bd);

      shape := Tb2PolygonShape.Create;
      shape.SetAsBox(0.5, 2.0);
      m_attachment.CreateFixture(shape, 2.0);
   end;

   // Define platform
   begin
      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      SetValue(bd.position, -4.0, 5.0);
      m_platform := m_world.CreateBody(bd);

      shape := Tb2PolygonShape.Create;
      shape.SetAsBox(0.5, 4.0, MakeVector(4.0, 0.0), 0.5 * Pi);

      fd := Tb2FixtureDef.Create;
      fd.shape := shape;
      fd.friction := 0.6;
      fd.density := 2.0;
      m_platform.CreateFixture(fd);

      rjd := Tb2RevoluteJointDef.Create;
      rjd.Initialize(m_attachment, m_platform, MakeVector(0.0, 5.0));
      rjd.maxMotorTorque := 50.0;
      rjd.enableMotor := True;
      m_world.CreateJoint(rjd);

      pjd := Tb2PrismaticJointDef.Create;
      pjd.Initialize(ground, m_platform, MakeVector(0.0, 5.0), MakeVector(1.0, 0.0));
      pjd.maxMotorForce := 1000.0;
      pjd.enableMotor := True;
      pjd.lowerTranslation := -10.0;
      pjd.upperTranslation := 10.0;
      pjd.enableLimit := True;

      m_world.CreateJoint(pjd);
      m_speed := 3.0;
   end;

   // Create a payload
   begin
      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      SetValue(bd.position, 0.0, 8.0);
      body := m_world.CreateBody(bd);

      shape := Tb2PolygonShape.Create;
      shape.SetAsBox(0.75, 0.75);

      fd := Tb2FixtureDef.Create;
      fd.shape := shape;
      fd.friction := 0.6;
      fd.density := 2.0;

      body.CreateFixture(fd);
   end;
end;

procedure TBodyTypes.Step(var settings: TSettings; timeStep: PhysicsFloat);
var
   p, v: TVector2;
begin
   // Drive the kinematic body.
   if m_platform.GetType = b2_kinematicBody then
   begin
      p := m_platform.GetTransform^.p;
      v := m_platform.GetLinearVelocity;

      if ((p.x < -10.0) and (v.x < 0.0)) or ((p.x > 10.0) and (v.x > 0.0)) then
      begin
         v.x := -v.x;
         m_platform.SetLinearVelocity(v);
      end;
   end;

   inherited;
   DrawText('Keys: (d) dynamic, (s) static, (k) kinematic');
end;

procedure TBodyTypes.Keyboard(key: Byte);
begin
   case key of
      Ord('D'): m_platform.SetType(b2_dynamicBody);
      Ord('S'): m_platform.SetType(b2_staticBody);
      Ord('K'):
         begin
            m_platform.SetType(b2_kinematicBody);
            m_platform.SetLinearVelocity(MakeVector(-m_speed, 0.0));
            m_platform.SetAngularVelocity(0.0);
         end;
   end;
end;

initialization
   RegisterTestEntry('Body Types', TBodyTypes);

end.

