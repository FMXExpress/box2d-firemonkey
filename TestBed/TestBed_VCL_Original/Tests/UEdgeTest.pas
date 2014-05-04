unit UEdgeTest;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TEdgeTest = class(TTester)
   public
      constructor Create; override;
   end;

implementation

{ TChain }

constructor TEdgeTest.Create;
const
   v1: TVector2 = (X: -10.0; Y: 0.0);
   v2: TVector2 = (X: -7.0; Y: -2.0);
   v3: TVector2 = (X: -4.0; Y: 0.0);
   v4: TVector2 = (X: -0.0; Y: 0.0);
   v5: TVector2 = (X: 4.0; Y: 0.0);
   v6: TVector2 = (X: 7.0; Y: 2.0);
   v7: TVector2 = (X: 10.0; Y: 0.0);
var
   bd: Tb2BodyDef;
   ground, body: Tb2Body;
   shape: Tb2EdgeShape;
   cshape: Tb2CircleShape;
   pshape: Tb2PolygonShape;
begin
   inherited;
   begin
      bd := Tb2BodyDef.Create;
      ground := m_world.CreateBody(bd);

      shape := Tb2EdgeShape.Create;
      shape.SetVertices(v1, v2);
      shape.m_hasVertex3 := True;
      shape.m_vertex3 := v3;
      ground.CreateFixture(shape, 0.0, False);

      shape.SetVertices(v2, v3);
      shape.m_hasVertex0 := True;
      shape.m_hasVertex3 := True;
      shape.m_vertex0 := v1;
      shape.m_vertex3 := v4;
      ground.CreateFixture(shape, 0.0, False);

      shape.SetVertices(v3, v4);
      shape.m_hasVertex0 := True;
      shape.m_hasVertex3 := True;
      shape.m_vertex0 := v2;
      shape.m_vertex3 := v5;
      ground.CreateFixture(shape, 0.0, False);

      shape.SetVertices(v4, v5);
      shape.m_hasVertex0 := True;
      shape.m_hasVertex3 := True;
      shape.m_vertex0 := v3;
      shape.m_vertex3 := v6;
      ground.CreateFixture(shape, 0.0, False);

      shape.SetVertices(v5, v6);
      shape.m_hasVertex0 := True;
      shape.m_hasVertex3 := True;
      shape.m_vertex0 := v4;
      shape.m_vertex3 := v7;
      ground.CreateFixture(shape, 0.0, False);

      shape.SetVertices(v6, v7);
      shape.m_hasVertex0 := True;
      shape.m_vertex0 := v5;
      ground.CreateFixture(shape, 0.0);
   end;

   begin
      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      SetValue(bd.position, -0.5, 0.6);
      bd.allowSleep := False;
      body := m_world.CreateBody(bd);

      cshape := Tb2CircleShape.Create;
      cshape.m_radius := 0.5;

      body.CreateFixture(cshape, 1.0);
   end;

   begin
      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      SetValue (bd.position, 1.0, 0.6);
      bd.allowSleep := False;
      body := m_world.CreateBody(bd);

      pshape := Tb2PolygonShape.Create;
      pshape.SetAsBox(0.5, 0.5);

      body.CreateFixture(pshape, 1.0);
   end;
end;

initialization
   RegisterTestEntry('Edge Test', TEdgeTest);

end.
