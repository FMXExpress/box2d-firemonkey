unit UPinBall;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TPinBall = class(TTester)
   public
	    m_leftJoint: Tb2RevoluteJoint;
	    m_rightJoint: Tb2RevoluteJoint;
	    m_ball: Tb2Body;
	    m_button: Boolean;

      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
      procedure KeyboardUp(key: Byte); override;
   end;

implementation

{ TPinBall }

constructor TPinBall.Create;
const
   vs: array[0..4] of TVector2 = ((X: 0.0; Y: -2.0), (X: 8.0; Y: 6.0),
      (X: 8.0; Y: 20.0), (X: -8.0; Y: 20.0), (X: -8.0; Y: 6.0));
   p1: TVector2 = (X: -2.0; Y: 0.0);
   p2: TVector2 = (X: 2.0; Y: 0.0);
var
   ground, leftFlipper, rightFlipper: Tb2Body;
   bd: Tb2BodyDef;
   fd: Tb2FixtureDef;
   loop: Tb2ChainShape;
   box: Tb2PolygonShape;
   shape: Tb2CircleShape;
   jd: Tb2RevoluteJointDef;
begin
   inherited;
   // Ground body
   begin
      bd := Tb2BodyDef.Create;
      ground := m_world.CreateBody(bd);

      loop := Tb2ChainShape.CreateLoop(@vs[0], 5);

      fd := Tb2FixtureDef.Create;
      fd.shape := loop;
      fd.density := 0.0;
      ground.CreateFixture(fd);
   end;

   // Flippers
   begin
      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;

      bd.position := p1;
      leftFlipper := m_world.CreateBody(bd, False);

      bd.position := p2;
      rightFlipper := m_world.CreateBody(bd);

      box := Tb2PolygonShape.Create;
      box.SetAsBox(1.75, 0.1);

      fd := Tb2FixtureDef.Create;
      fd.shape := box;
      fd.density := 1.0;

      leftFlipper.CreateFixture(fd, False, False);
      rightFlipper.CreateFixture(fd);

      jd := Tb2RevoluteJointDef.Create;
      jd.bodyA := ground;
      jd.localAnchorB := b2Vec2_Zero;
      jd.enableMotor := True;
      jd.maxMotorTorque := 1000.0;
      jd.enableLimit := True;

      jd.motorSpeed := 0.0;
      jd.localAnchorA := p1;
      jd.bodyB := leftFlipper;
      jd.lowerAngle := -30.0 * Pi / 180.0;
      jd.upperAngle := 5.0 * Pi / 180.0;
      m_leftJoint := Tb2RevoluteJoint(m_world.CreateJoint(jd, False));

      jd.motorSpeed := 0.0;
      jd.localAnchorA := p2;
      jd.bodyB := rightFlipper;
      jd.lowerAngle := -5.0 * Pi / 180.0;
      jd.upperAngle := 30.0 * Pi / 180.0;
      m_rightJoint := Tb2RevoluteJoint(m_world.CreateJoint(jd));
   end;

   // Circle character
   begin
      bd := Tb2BodyDef.Create;
      SetValue(bd.position, 1.0, 15.0);
      bd.bodyType := b2_dynamicBody;
      bd.bullet := True;

      m_ball := m_world.CreateBody(bd);

      shape := Tb2CircleShape.Create;
      shape.m_radius := 0.2;

      fd := Tb2FixtureDef.Create;
      fd.shape := shape;
      fd.density := 1.0;
      m_ball.CreateFixture(fd);
   end;

   m_button := False;
end;

procedure TPinBall.Keyboard(key: Byte);
begin
   if key = Ord('A') then
      m_button := True;
end;

procedure TPinBall.KeyboardUp(key: Byte);
begin
   if key = Ord('A') then
      m_button := False;
end;

procedure TPinBall.Step(var settings: TSettings; timeStep: PhysicsFloat);
begin
   if m_button then
   begin
      m_leftJoint.SetMotorSpeed(20.0);
      m_rightJoint.SetMotorSpeed(-20.0);
   end
   else
   begin
      m_leftJoint.SetMotorSpeed(-10.0);
      m_rightJoint.SetMotorSpeed(10.0);
   end;

   inherited;
   DrawText('Press ''a'' to control the flippers.');
end;

initialization
   RegisterTestEntry('Pin Ball', TPinBall);

end.

