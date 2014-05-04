unit UCantilever;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TCantilever = class(TTester)
   public
      constructor Create; override;
   end;

implementation

// It is difficult to make a cantilever made of links completely rigid with weld joints.
// You will have to use a high number of iterations to make them stiff.
// So why not go ahead and use soft weld joints? They behave like a revolute
// joint with a rotational spring.

{ TCantilever }

constructor TCantilever.Create;
const
   e_count = 8;
var
   i: Integer;
   ground, body: Tb2Body;
   bd: Tb2BodyDef;
   edge: Tb2EdgeShape;
   shape: Tb2PolygonShape;
   fd: Tb2FixtureDef;
   jd: Tb2WeldJointDef;
   prevBody: Tb2Body;
   anchor: TVector2;
   vertices: array[0..2] of TVector2;
   cshape: Tb2CircleShape;
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
      shape.SetAsBox(0.5, 0.125);

      fd := Tb2FixtureDef.Create;
      fd.shape := shape;
      fd.density := 20.0;

      jd := Tb2WeldJointDef.Create;

      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      prevBody := ground;
      for i := 0 to e_count - 1 do
      begin
         SetValue(bd.position, -8.5 + 1.0 * i, 5.0);
         body := m_world.CreateBody(bd, False);
         body.CreateFixture(fd, False, False);

         SetValue(anchor, -9.0 + 1.0 * i, 5.0);
         jd.Initialize(prevBody, body, anchor);
         m_world.CreateJoint(jd, False);

         prevBody := body;
      end;
      shape.Free;
      bd.Free;
   end;

   begin
      shape := Tb2PolygonShape.Create;
      shape.SetAsBox(1.0, 0.125);

      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;

      prevBody := ground;

      jd.frequencyHz := 5.0;
			jd.dampingRatio := 0.7;
      for i := 0 to 3 do
      begin
         SetValue(bd.position, -14.0 + 2.0 * i, 15.0);
         body := m_world.CreateBody(bd, False);
         body.CreateFixture(fd, False, False);

         SetValue(anchor, -15.0 + 2.0 * i, 15.0);
         jd.Initialize(prevBody, body, anchor);
         m_world.CreateJoint(jd, False);

         prevBody := body;
      end;
      jd.Free;
      bd.Free;
   end;

   begin
      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      prevBody := ground;

      jd := Tb2WeldJointDef.Create;
      for i := 0 to e_count - 1 do
      begin
         SetValue(bd.position, -4.5 + 1.0 * i, 8.0);
         body := m_world.CreateBody(bd, False);
         body.CreateFixture(fd, False, False);

         if i > 0 then
         begin
            SetValue(anchor, -5.0 + 1.0 * i, 8.0);
            jd.Initialize(prevBody, body, anchor);
            m_world.CreateJoint(jd, False);
         end;

         prevBody := body;
      end;
      bd.Free;
   end;

   begin
      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;

      jd.frequencyHz := 8.0;
			jd.dampingRatio := 0.7;
      prevBody := ground;
      for i := 0 to e_count - 1 do
      begin
         SetValue(bd.position, -15.5 + 1.0 * i, 20.0);
         body := m_world.CreateBody(bd, False);
         body.CreateFixture(fd, False, False);

         if i > 0 then
         begin
            SetValue(anchor, -15.0 + 1.0 * i, 20.0);
            jd.Initialize(prevBody, body, anchor);
            m_world.CreateJoint(jd, False);
         end;

         prevBody := body;
      end;
      bd.Free;
      jd.Free;
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
      SetValue(bd.position, -13.0 + 8.0 * i, 18.0);
      m_world.CreateBody(bd, False).CreateFixture(fd, False, False);
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
   for i := 0 to 1 do
   begin
      SetValue(bd.position, -6.0 + 6.0 * i, 10.0);
      m_world.CreateBody(bd, False).CreateFixture(fd, False, False);
   end;
   bd.Free;
   fd.Free;
   cshape.Free;
end;

initialization
   RegisterTestEntry('Cantilever', TCantilever);

end.
