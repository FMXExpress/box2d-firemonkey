unit UBreakable;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TBreakable = class(TTester)
   private
      procedure Break;
      procedure PostSolve(var contact: Tb2Contact; const impulse: Tb2ContactImpulse); override;
   public
      m_body1: Tb2Body;
	    m_velocity: TVector2;
	    m_angularVelocity: PhysicsFloat;
	    m_shape1, m_shape2: Tb2PolygonShape;
      m_piece1, m_piece2: Tb2Fixture;
      m_broke, m_break: Boolean;

      constructor Create; override;
      destructor Destroy; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
   end;

implementation

{ TBreakable }

constructor TBreakable.Create;
var
   bd: Tb2BodyDef;
   ground: Tb2Body;
   shape: Tb2EdgeShape;
begin
   inherited;
   // Ground body
   begin
      bd := Tb2BodyDef.Create;
      ground := m_world.CreateBody(bd);

      shape := Tb2EdgeShape.Create;
      shape.SetVertices(MakeVector(-40.0, 0.0), MakeVector(40.0, 0.0));
      ground.CreateFixture(shape, 0.0);
   end;

   // Breakable dynamic body
   begin
      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      SetValue(bd.position, 0.0, 40.0);
      bd.angle := 0.25 * Pi;
      m_body1 := m_world.CreateBody(bd);

      m_shape1 := Tb2PolygonShape.Create;
      m_shape2 := Tb2PolygonShape.Create;
      m_shape1.SetAsBox(0.5, 0.5, MakeVector(-0.5, 0.0), 0.0);
      m_piece1 := m_body1.CreateFixture(m_shape1, 1.0, False, False);
      m_shape2.SetAsBox(0.5, 0.5, MakeVector(0.5, 0.0), 0.0);
      m_piece2 := m_body1.CreateFixture(m_shape2, 1.0, False);
   end;

   m_break := False;
   m_broke := False;
end;

destructor TBreakable.Destroy;
begin
   m_shape1.Free;
   m_shape2.Free;
   inherited;
end;

procedure TBreakable.Break;
var
   body1, body2: Tb2Body;
   bd: Tb2BodyDef;
   center, velocity1, velocity2: TVector2;
begin
   // Create two bodies from one.
   body1 := m_piece1.GetBody;
   center := body1.GetWorldCenter;

   body1.DestroyFixture(m_piece2);

   bd := Tb2BodyDef.Create;
   bd.bodyType  := b2_dynamicBody;
   bd.position := body1.GetPosition;
   bd.angle := body1.GetAngle;

   body2 := m_world.CreateBody(bd);
   m_piece2 := body2.CreateFixture(m_shape2, 1.0, False);

   // Compute consistent velocities for new bodies based on cached velocity.
   {$IFDEF OP_OVERLOAD}
   velocity1 := m_velocity + b2Cross(m_angularVelocity, body1.GetWorldCenter - center);
   velocity2 := m_velocity + b2Cross(m_angularVelocity, body2.GetWorldCenter - center);
   {$ELSE}
   velocity1 := Add(m_velocity, b2Cross(m_angularVelocity, Subtract(body1.GetWorldCenter, center)));
   velocity2 := Add(m_velocity, b2Cross(m_angularVelocity, Subtract(body2.GetWorldCenter, center)));
   {$ENDIF}

   body1.SetAngularVelocity(m_angularVelocity);
   body1.SetLinearVelocity(velocity1);

   body2.SetAngularVelocity(m_angularVelocity);
   body2.SetLinearVelocity(velocity2);
end;

procedure TBreakable.PostSolve(var contact: Tb2Contact; const impulse: Tb2ContactImpulse);
var
   i: Integer;
   maxImpulse: PhysicsFloat;
begin
   if m_broke then // The body already broke.
      Exit;

   // Should the body break?
   maxImpulse := 0.0;
   for i := 0 to contact.m_manifold.pointCount - 1 do
      maxImpulse := b2Max(maxImpulse, impulse.normalImpulses[i]);

   if maxImpulse > 40.0 then // Flag the body for breaking.
      m_break := True;
end;

procedure TBreakable.Step(var settings: TSettings; timeStep: PhysicsFloat);
begin
   if m_break then
   begin
      Break;
      m_broke := True;
      m_break := False;
   end;

   // Cache velocities to improve movement on breakage.
   if not m_broke then
   begin
      m_velocity := m_body1.GetLinearVelocity;
      m_angularVelocity := m_body1.GetAngularVelocity;
   end;
   inherited;
end;

initialization
   RegisterTestEntry('Breakable', TBreakable);

end.

