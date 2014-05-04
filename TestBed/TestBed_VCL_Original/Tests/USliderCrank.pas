unit USliderCrank;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TSliderCrank = class(TTester)
   public
      m_joint1: Tb2RevoluteJoint;
    	m_joint2: Tb2PrismaticJoint;

      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
   end;

implementation

{ TSliderCrank }

constructor TSliderCrank.Create;
var
   ground, prevBody, body: Tb2Body;
   bd: Tb2BodyDef;
   rjd: Tb2RevoluteJointDef;
   pjd: Tb2PrismaticJointDef;
   edge: Tb2EdgeShape;
   shape: Tb2PolygonShape;
begin
   inherited;
   begin
     bd := Tb2BodyDef.Create;
     ground := m_world.CreateBody(bd);

     edge := Tb2EdgeShape.Create;
     edge.SetVertices(MakeVector(-40.0, 0.0), MakeVector(40.0, 0.0));
     ground.CreateFixture(edge, 0.0);
   end;

   begin
      prevBody := ground;

      // Define crank.
      begin
         shape := Tb2PolygonShape.Create;
         shape.SetAsBox(0.5, 2.0);

         bd := Tb2BodyDef.Create;
         bd.bodyType := b2_dynamicBody;
         SetValue(bd.position, 0.0, 7.0);
         body := m_world.CreateBody(bd);
         body.CreateFixture(shape, 2.0);

         rjd := Tb2RevoluteJointDef.Create;
         rjd.Initialize(prevBody, body, MakeVector(0.0, 5.0));
         rjd.motorSpeed := 1.0 * Pi;
         rjd.maxMotorTorque := 10000.0;
         rjd.enableMotor := True;
         m_joint1 := Tb2RevoluteJoint(m_world.CreateJoint(rjd));

         prevBody := body;
      end;

      // Define follower.
      begin
         shape := Tb2PolygonShape.Create;
         shape.SetAsBox(0.5, 4.0);

         bd := Tb2BodyDef.Create;
         bd.bodyType := b2_dynamicBody;
         SetValue(bd.position, 0.0, 13.0);
         body := m_world.CreateBody(bd);
         body.CreateFixture(shape, 2.0);

         rjd := Tb2RevoluteJointDef.Create;
         rjd.Initialize(prevBody, body, MakeVector(0.0, 9.0));
         rjd.enableMotor := False;
         m_world.CreateJoint(rjd);

         prevBody := body;
      end;

      // Define piston
      begin
         shape := Tb2PolygonShape.Create;
         shape.SetAsBox(1.5, 1.5);

         bd := Tb2BodyDef.Create;
         bd.bodyType := b2_dynamicBody;
         bd.fixedRotation := True;
         SetValue(bd.position, 0.0, 17.0);
         body := m_world.CreateBody(bd);
         body.CreateFixture(shape, 2.0);

         rjd := Tb2RevoluteJointDef.Create;
         rjd.Initialize(prevBody, body, MakeVector(0.0, 17.0));
         m_world.CreateJoint(rjd);

         pjd := Tb2PrismaticJointDef.Create;
         pjd.Initialize(ground, body, MakeVector(0.0, 17.0), MakeVector(0.0, 1.0));

         pjd.maxMotorForce := 1000.0;
         pjd.enableMotor := True;

         m_joint2 := Tb2PrismaticJoint(m_world.CreateJoint(pjd));
      end;

      // Create a payload
      begin
         shape := Tb2PolygonShape.Create;
         shape.SetAsBox(1.5, 1.5);

         bd := Tb2BodyDef.Create;
         bd.bodyType := b2_dynamicBody;
         SetValue(bd.position, 0.0, 23.0);
         body := m_world.CreateBody(bd);
         body.CreateFixture(shape, 2.0);
      end;
   end;
end;

procedure TSliderCrank.Step(var settings: TSettings; timeStep: PhysicsFloat);
begin
   inherited;
   DrawText('Keys: (f) toggle friction, (m) toggle motor');
   DrawText(Format('Motor Torque = %5.0f', [m_joint1.GetMotorTorque(DefaultStep)]));
end;

procedure TSliderCrank.Keyboard(key: Byte);
begin
   case key of
      70{F}:
         begin
			      m_joint2.EnableMotor(not m_joint2.IsMotorEnabled);
			      m_joint2.GetBodyB.SetAwake(True);
         end;
      77{M}:
         begin
			      m_joint1.EnableMotor(not m_joint1.IsMotorEnabled);
			      m_joint1.GetBodyB.SetAwake(True);
         end;
   end;
end;

initialization
   RegisterTestEntry('Slider Crank', TSliderCrank);

end.

