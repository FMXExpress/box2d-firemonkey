unit UOneSidedPlatform;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TState = (e_unknown, e_above, e_below);

   TOneSidedPlatform = class(TTester)
   private
      procedure PreSolve(var contact: Tb2Contact; const oldManifold: Tb2Manifold); override;
   public
      m_radius, m_top, m_bottom: PhysicsFloat;
      m_state: TState;
      m_platform, m_character: Tb2Fixture;

      constructor Create; override;
   end;

implementation

{ TOneSidedPlatform }

constructor TOneSidedPlatform.Create;
var
   bd: Tb2BodyDef;
   ground, body: Tb2Body;
   edge: Tb2EdgeShape;
   shape: Tb2PolygonShape;
   cshape: Tb2CircleShape;
begin
   inherited;
   // Ground
   begin
      bd := Tb2BodyDef.Create;
      ground := m_world.CreateBody(bd);

      edge := Tb2EdgeShape.Create;
      edge.SetVertices(MakeVector(-20.0, 0.0), MakeVector(20.0, 0.0));
      ground.CreateFixture(edge, 0.0);
   end;

   // Platform
   begin
      bd := Tb2BodyDef.Create;
      SetValue(bd.position, 0.0, 10.0);
      body := m_world.CreateBody(bd);

      shape := Tb2PolygonShape.Create;
      shape.SetAsBox(3.0, 0.5);
      m_platform := body.CreateFixture(shape, 0.0);

      m_bottom := 10.0 - 0.5;
      m_top := 10.0 + 0.5;
   end;

   // Actor
   begin
      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      SetValue(bd.position, 0.0, 12.0);
      body := m_world.CreateBody(bd);

      m_radius := 0.5;
      cshape := Tb2CircleShape.Create;
      cshape.m_radius := m_radius;
      m_character := body.CreateFixture(cshape, 20.0);
      body.SetLinearVelocity(MakeVector(0.0, -50.0));

      m_state := e_unknown;
   end;
end;

procedure TOneSidedPlatform.PreSolve(var contact: Tb2Contact;
   const oldManifold: Tb2Manifold);
begin
   inherited;
   with contact do
   begin
      if (m_fixtureA <> m_platform) and (m_fixtureA <> m_character) then
         Exit;

      if (m_fixtureB <> m_platform) and (m_fixtureB <> m_character) then
         Exit;

      if m_character.GetBody.GetPosition.y < m_top + m_radius - 3.0 * b2_linearSlop then
         {$IFDEF OP_OVERLOAD}
         contact.SetEnabled(False);
         {$ELSE}
         SetEnabled(contact, False);
         {$ENDIF}
   end;
end;

initialization
   RegisterTestEntry('One Sided Platform', TOneSidedPlatform);

end.

