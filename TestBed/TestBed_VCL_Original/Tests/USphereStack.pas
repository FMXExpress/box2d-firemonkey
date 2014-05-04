unit USphereStack;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

const
   e_count = 15;

type
   TSphereStack = class(TTester)
   public
      m_bodies: array[0..e_count - 1] of Tb2Body;

      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
   end;

implementation

{ TSphereStack }

constructor TSphereStack.Create;
var
   i: Integer;
   bd: Tb2BodyDef;
   ground: Tb2Body;
   shape: Tb2EdgeShape;
   cshape: Tb2CircleShape;
begin
   inherited;
   bd := Tb2BodyDef.Create;
   ground := m_world.CreateBody(bd);

   shape := Tb2EdgeShape.Create;
   shape.SetVertices(MakeVector(-40.0, 0.0), MakeVector(40.0, 0.0));
   ground.CreateFixture(shape, 0.0);

   cshape := Tb2CircleShape.Create;
   cshape.m_radius := 1.0;
   bd := Tb2BodyDef.Create;
   bd.bodyType := b2_dynamicBody;
   for i := 0 to e_count - 1 do
   begin
      SetValue(bd.position, 0.0, 4.0 + 3.0 * i);
      m_bodies[i] := m_world.CreateBody(bd, False);
      m_bodies[i].CreateFixture(cshape, 1.0, False);
      m_bodies[i].SetLinearVelocity(MakeVector(0.0, -50.0));
   end;
   cshape.Free;
   bd.Free;
end;

procedure TSphereStack.Step(var settings: TSettings; timeStep: PhysicsFloat);
begin
   inherited;
end;

initialization
   RegisterTestEntry('Sphere Stack', TSphereStack);

end.

