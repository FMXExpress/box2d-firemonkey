unit UVaryingRestitution;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TVaryingRestitution = class(TTester)
   public
      constructor Create; override;
   end;

implementation

// Note: even with a restitution of 1.0, there is some energy change
// due to position correction.

{ TVaryingRestitution }

constructor TVaryingRestitution.Create;
const
   restitution: array[0..6] of Single = (0.0, 0.1, 0.3, 0.5, 0.75, 0.9, 1.0);
var
   i: Integer;
   sd: Tb2PolygonShape;
   bd: Tb2BodyDef;
   ground: Tb2Body;
   cd: Tb2CircleShape;
   fd: Tb2FixtureDef;
   body: Tb2Body;
begin
   inherited;
   begin
      sd := Tb2PolygonShape.Create;
      sd.SetAsBox(50.0, 10.0);

      bd := Tb2BodyDef.Create;
      SetValue(bd.position, 0.0, -10.0);

      ground := m_world.CreateBody(bd);
      ground.CreateFixture(sd, 0.0);
   end;

   cd := Tb2CircleShape.Create;
   cd.m_radius := 1.0;

   fd := Tb2FixtureDef.Create;
   fd.shape := cd;
   fd.density := 1.0;
   bd := Tb2BodyDef.Create;
   bd.bodyType := b2_dynamicBody;
   for i := 0 to 6 do
   begin
      SetValue(bd.position, -10.0 + 3.0 * i, 20.0);
      body := m_world.CreateBody(bd, False);
      fd.restitution := restitution[i];
      body.CreateFixture(fd, False, False);
   end;
   fd.Free;
   cd.Free;
   bd.Free;
end;

initialization
   RegisterTestEntry('Varying Restitution', TVaryingRestitution);
end.
