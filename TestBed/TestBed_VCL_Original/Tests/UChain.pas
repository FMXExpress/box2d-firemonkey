unit UChain;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TChain = class(TTester)
   public
      constructor Create; override;
   end;

implementation

{ TChain }

constructor TChain.Create;
const
   y = 25.0;
var
   i: Integer;
   ground, prevBody, body: Tb2Body;
   bd: Tb2BodyDef;
   edge: Tb2EdgeShape;
   shape: Tb2PolygonShape;
   fd: Tb2FixtureDef;
   jd: Tb2RevoluteJointDef;
   anchor: TVector2;
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
      shape.SetAsBox(0.6, 0.125);

      fd := Tb2FixtureDef.Create;
      fd.shape := shape;
      fd.density := 20.0;
      fd.friction := 0.2;

      jd := Tb2RevoluteJointDef.Create;
      jd.collideConnected := False;

      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;

      prevBody := ground;
      for i := 0 to 29 do
      begin
         SetValue(bd.position, 0.5 + i, y);
         body := m_world.CreateBody(bd, False);
         body.CreateFixture(fd, False, False);

         SetValue(anchor, i, y);
         jd.Initialize(prevBody, body, anchor);
         m_world.CreateJoint(jd, False);

         prevBody := body;
      end;
      jd.Free;
      bd.Free;
      fd.Free;
      shape.Free;
   end;
end;

initialization
   RegisterTestEntry('Chain', TChain);

end.
