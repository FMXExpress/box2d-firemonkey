unit UAddPair;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TAddPair = class(TTester)
   public
      constructor Create; override;
   end;


implementation

{ TAddPair }

constructor TAddPair.Create;
const
   max_body_count = 150;
var
   i: Integer;
   minX, maxX, minY, maxY: PhysicsFloat;
   shape: Tb2CircleShape;
   bd: Tb2BodyDef;
   body: Tb2Body;
   pshape: Tb2PolygonShape;
begin
   inherited;

   m_world.SetGravity(b2Vec2_Zero);
   UpdateGravityText;

   shape := Tb2CircleShape.Create;
   shape.m_p := b2Vec2_Zero;
   shape.m_radius := 0.1;

   minX := -6.0;
   maxX := 0.0;
   minY := 4.0;
   maxY := 6.0;

   bd := Tb2BodyDef.Create;
   bd.bodyType := b2_dynamicBody;
   for i := 0 to max_body_count - 1 do
   begin
      bd.position := MakeVector(RandomFloat(minX, maxX), RandomFloat(minY, maxY));
      body := m_world.CreateBody(bd, False);
      body.CreateFixture(shape, 0.01, False);
   end;
   bd.Free;
   shape.Free;

   pshape := Tb2PolygonShape.Create;
   pshape.SetAsBox(1.5, 1.5);

   bd := Tb2BodyDef.Create;
   bd.bodyType := b2_dynamicBody;
   SetValue(bd.position, -40.0, 5.0);
   bd.bullet := True;
   body := m_world.CreateBody(bd);
   body.CreateFixture(pshape, 1.0);
   body.SetLinearVelocity(MakeVector(150.0, 0.0));
end;

initialization
   RegisterTestEntry('Add Pair(stress test)', TAddPair);
end.
