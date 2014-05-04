unit UWheelJoint;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TWheelJoint = class(TTester)
   public
      constructor Create; override;
   end;


implementation

{ TWheelJoint }

constructor TWheelJoint.Create;
var
   ground, body: Tb2Body;
   edge: Tb2EdgeShape;
   shape: Tb2PolygonShape;
   bd: Tb2BodyDef;
   jd: Tb2WheelJointDef;
   axis: TVector2;
begin
   inherited;
   begin
      edge := Tb2EdgeShape.Create;
      edge.SetVertices(MakeVector(-40.0, 0.0), MakeVector(40.0, 0.0));

      bd := Tb2BodyDef.Create;
      ground := m_world.CreateBody(bd);
      ground.CreateFixture(edge, 0.0);
   end;

   begin
      shape := Tb2PolygonShape.Create;
      shape.SetAsBox(0.5, 2.0);

      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      SetValue(bd.position, 0.0, 7.0);
      body := m_world.CreateBody(bd);
      body.CreateFixture(shape, 1.0);

      jd := Tb2WheelJointDef.Create;
      SetValue(axis, 2.0, 1.0);
      {$IFDEF OP_OVERLOAD}
      axis.Normalize;
      {$ELSE}
      Normalize(axis);
      {$ENDIF}
      jd.Initialize(ground, body, MakeVector(0.0, 8.5), axis);
			jd.motorSpeed := 1.0;
			jd.maxMotorTorque := 1000.0;
			jd.enableMotor := True;
			jd.frequencyHz := 1.0;
			jd.dampingRatio := 0.2;
      m_world.CreateJoint(jd);
   end;
end;

initialization
   RegisterTestEntry('Wheel Joint', TWheelJoint);
end.

