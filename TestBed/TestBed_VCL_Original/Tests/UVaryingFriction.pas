unit UVaryingFriction;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TVaryingFriction = class(TTester)
   public
      constructor Create; override;
   end;

implementation

{ TVaryingFriction }

constructor TVaryingFriction.Create;
const
   friction: array[0..4] of Single = (0.75, 0.5, 0.35, 0.1, 0.0);
var
   i: Integer;
   ed: Tb2EdgeShape;
   sd: Tb2PolygonShape;
   bd: Tb2BodyDef;
   body: Tb2Body;
   fd: Tb2FixtureDef;
begin
   inherited;
   ed := Tb2EdgeShape.Create;
   ed.SetVertices(MakeVector(-40.0, 0.0), MakeVector(40.0, 0.0));
   bd := Tb2BodyDef.Create;
   m_world.CreateBody(bd).CreateFixture(ed, 0.0);

   sd := Tb2PolygonShape.Create;
   sd.SetAsBox(13.0, 0.25);
   bd := Tb2BodyDef.Create;
   SetValue(bd.position, -4.0, 22.0);
   bd.angle := -0.25;
   m_world.CreateBody(bd).CreateFixture(sd, 0.0, False);

   sd.SetAsBox(0.25, 1.0);
   bd := Tb2BodyDef.Create;
   SetValue(bd.position, 10.5, 19.0);
   m_world.CreateBody(bd).CreateFixture(sd, 0.0, False);

   sd.SetAsBox(13.0, 0.25);
   bd := Tb2BodyDef.Create;
   SetValue(bd.position, 4.0, 14.0);
   bd.angle := 0.25;
   m_world.CreateBody(bd).CreateFixture(sd, 0.0, False);

   sd.SetAsBox(0.25, 1.0);
   bd := Tb2BodyDef.Create;
   SetValue(bd.position, -10.5, 11.0);
   m_world.CreateBody(bd).CreateFixture(sd, 0.0, False);

   sd.SetAsBox(13.0, 0.25);
   bd := Tb2BodyDef.Create;
   SetValue(bd.position, -4.0, 6.0);
   bd.angle := -0.25;
   m_world.CreateBody(bd).CreateFixture(sd, 0.0, False);

   fd := Tb2FixtureDef.Create;
   fd.density := 25;
   fd.shape := sd;
   sd.SetAsBox(0.5, 0.5);
   bd := Tb2BodyDef.Create;
   bd.bodyType := b2_dynamicBody;
   for i := 0 to 4 do
   begin
      SetValue(bd.position, -15.0 + 4.0 * i, 28.0);
      body := m_world.CreateBody(bd, False);
      fd.friction := friction[i];
      body.CreateFixture(fd, False, False);
   end;
   fd.Free;
   bd.Free;
   sd.Free;
end;

initialization
   RegisterTestEntry('Varying Friction', TVaryingFriction);

end.
