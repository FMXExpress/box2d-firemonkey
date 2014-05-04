unit UPrismatic;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TPrismatic = class(TTester)
   public
      m_joint: Tb2PrismaticJoint;

      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
   end;

implementation

// The motor in this test gets smoother with higher velocity iterations.

{ TPrismatic }

constructor TPrismatic.Create;
var
   bd: Tb2BodyDef;
   ground, body: Tb2Body;
   edge: Tb2EdgeShape;
   shape: Tb2PolygonShape;
   pjd: Tb2PrismaticJointDef;
   axis: TVector2;
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
      shape := Tb2PolygonShape.Create;
      shape.SetAsBox(2.0, 0.5);

      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      SetValue(bd.position, -10.0, 10.0);
      bd.angle := 0.5 * Pi;
      bd.allowSleep := False;
      body := m_world.CreateBody(bd);
      body.CreateFixture(shape, 5.0);

      pjd := Tb2PrismaticJointDef.Create;

      // Bouncy limit
      SetValue(axis, 2.0, 1.0);
      {$IFDEF OP_OVERLOAD}
      axis.Normalize;
      {$ELSE}
      Normalize(axis);
      {$ENDIF}
      pjd.Initialize(ground, body, MakeVector(0.0, 0.0), axis);

      // Non-bouncy limit
      //pjd.Initialize(ground, body, MakeVector(-10.0, 10.0), MakeVector(1.0, 0.0));

      pjd.motorSpeed := 10.0;
      pjd.maxMotorForce := 10000.0;
      pjd.enableMotor := True;
      pjd.lowerTranslation := 0.0;
      pjd.upperTranslation := 20.0;
      pjd.enableLimit := True;

      m_joint := Tb2PrismaticJoint(m_world.CreateJoint(pjd));
   end;
end;

procedure TPrismatic.Keyboard(key: Byte);
begin
   case key of
      Ord('L'): m_joint.EnableLimit(not m_joint.IsLimitEnabled);
      Ord('M'): m_joint.EnableMotor(not m_joint.IsMotorEnabled);
      Ord('S'): m_joint.SetMotorSpeed(-m_joint.GetMotorSpeed);
   end;
end;

procedure TPrismatic.Step(var settings: TSettings; timeStep: PhysicsFloat);
begin
   inherited;
   DrawText('Keys: (l) limits, (m) motors, (s) speed');
   DrawText(Format('Motor Force := %4.0f', [m_joint.GetMotorForce(DefaultStep)]));
end;

initialization
   RegisterTestEntry('Prismatic', TPrismatic);

end.

