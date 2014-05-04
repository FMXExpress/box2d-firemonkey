unit UBridge;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TBridge = class(TTester)
   public
      m_middle: Tb2Body;
      constructor Create; override;
   end;

implementation

{ TBridge }

constructor TBridge.Create;
const
   e_count = 30;
var
   i: Integer;
   ground, prevBody, body: Tb2Body;
   bd: Tb2BodyDef;
   eshape: Tb2EdgeShape;
   shape: Tb2PolygonShape;
   cshape: Tb2CircleShape;
   fd: Tb2FixtureDef;
   jd: Tb2RevoluteJointDef;
   anchor: TVector2;
   vertices: array[0..2] of TVector2;
begin
   inherited;
   begin
      bd := Tb2BodyDef.Create;
      ground := m_world.CreateBody(bd);

      eshape := Tb2EdgeShape.Create;
      eshape.SetVertices(MakeVector(-40.0, 0.0), MakeVector(40.0, 0.0));
      ground.CreateFixture(eshape, 0.0);
   end;

   begin
      shape := Tb2PolygonShape.Create;
      shape.SetAsBox(0.5, 0.125);

      fd := Tb2FixtureDef.Create;
      fd.shape := shape;
      fd.density := 20.0;
      fd.friction := 0.2;

      jd := Tb2RevoluteJointDef.Create;
      bd := Tb2BodyDef.Create;

      prevBody := ground;
      for i := 0 to e_count - 1 do
      begin
         bd.bodyType := b2_dynamicBody;
         SetValue(bd.position, -14.5 + 1.0 * i, 5.0);
         body := m_world.CreateBody(bd, False);
         body.CreateFixture(fd, False, False);

         SetValue(anchor, -15.0 + 1.0 * i, 5.0);
         jd.Initialize(prevBody, body, anchor);
         m_world.CreateJoint(jd, False);

         if i = (e_count shr 1) then
            m_middle := body;
         prevBody := body;
      end;

      SetValue(anchor, -15.0 + 1.0 * e_count, 5.0);
      jd.Initialize(prevBody, ground, anchor);
      m_world.CreateJoint(jd);
      bd.Free;
      fd.Free;
      shape.Free;
   end;

   vertices[0] := MakeVector(-0.5, 0.0);
   vertices[1] := MakeVector(0.5, 0.0);
   vertices[2] := MakeVector(0.0, 1.5);
   shape := Tb2PolygonShape.Create;
   shape.SetVertices(@vertices[0], 3);
   fd := Tb2FixtureDef.Create;
   fd.shape := shape;
   fd.density := 1.0;
   bd := Tb2BodyDef.Create;
   bd.bodyType := b2_dynamicBody;
   for i := 0 to 1 do
   begin
      SetValue(bd.position, -8.0 + 8.0 * i, 12.0);
      m_world.CreateBody(bd, False).CreateFixture(fd, False, False)
   end;
   shape.Free;
   fd.Free;
   bd.Free;

   cshape := Tb2CircleShape.Create;
   cshape.m_radius := 0.5;
   fd := Tb2FixtureDef.Create;
   fd.shape := cshape;
   fd.density := 1.0;
   bd := Tb2BodyDef.Create;
   bd.bodyType := b2_dynamicBody;
   for i := 0 to 2 do
   begin
      SetValue(bd.position, -6.0 + 6.0 * i, 10.0);
      m_world.CreateBody(bd, False).CreateFixture(fd, False, False);
   end;
   bd.Free;
   fd.Free;
   cshape.Free;
end;

initialization
   RegisterTestEntry('Bridge', TBridge);

end.
