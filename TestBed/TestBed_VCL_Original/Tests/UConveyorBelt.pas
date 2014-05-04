unit UConveyorBelt;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TConveyorBelt = class(TTester)
   public
      m_platform: Tb2Fixture;

      constructor Create; override;
      procedure PreSolve(var contact: Tb2Contact; const oldManifold: Tb2Manifold); override;
   end;

implementation

{ TConveyorBelt }

constructor TConveyorBelt.Create;
var
   i: Integer;
   bd: Tb2BodyDef;
   ground, body: Tb2Body;
   shape: Tb2EdgeShape;
   pshape: Tb2PolygonShape;
   fd: Tb2FixtureDef;
begin
   inherited;

   // Ground
   bd := Tb2BodyDef.Create;
   ground := m_world.CreateBody(bd);

   shape := Tb2EdgeShape.Create;
   shape.SetVertices(MakeVector(-20.0, 0.0), MakeVector(20.0, 0.0));
   ground.CreateFixture(shape, 0.0);

   // Platform
   bd := Tb2BodyDef.Create;
   SetValue(bd.position, -5.0, 5.0);
   body := m_world.CreateBody(bd);

   pshape := Tb2PolygonShape.Create;
   pshape.SetAsBox(10.0, 0.5);

   fd := Tb2FixtureDef.Create;
   fd.shape := pshape;
   fd.friction := 0.8;
   m_platform := body.CreateFixture(fd);

   // Boxes
   bd := Tb2BodyDef.Create;
   bd.bodyType := b2_dynamicBody;
   pshape := Tb2PolygonShape.Create;
   pshape.SetAsBox(0.5, 0.5);
   for i := 0 to 4 do
   begin
      SetValue(bd.position, -10.0 + 2.0 * i, 7.0);
      body := m_world.CreateBody(bd, False);
      body.CreateFixture(pshape, 20.0, False);
   end;
   bd.Free;
   pshape.Free;
end;

procedure TConveyorBelt.PreSolve(var contact: Tb2Contact;
  const oldManifold: Tb2Manifold);
var
   fixtureA, fixtureB: Tb2Fixture;
begin
   inherited;

   fixtureA := contact.m_fixtureA;
   fixtureB := contact.m_fixtureB;

   if fixtureA = m_platform then
      contact.m_tangentSpeed := 5.0;

   if fixtureB = m_platform then
      contact.m_tangentSpeed := -5.0;
end;

initialization
   RegisterTestEntry('Conveyor Belt', TConveyorBelt);

end.
