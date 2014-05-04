unit UPolyCollision;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TPolyCollision = class(TTester)
   public
      m_polygonA, m_polygonB: Tb2PolygonShape;
      m_transformA, m_transformB: Tb2Transform;
      m_positionB: TVector2;
      m_angleB: PhysicsFloat;

      constructor Create; override;
      destructor Destroy; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
   end;

implementation

{ TPolyCollision }

constructor TPolyCollision.Create;
begin
   inherited;
   m_polygonA := Tb2PolygonShape.Create;
   m_polygonA.SetAsBox(0.2, 0.4);
   {$IFDEF OP_OVERLOAD}
   m_transformA.SetValue(MakeVector(0.0, 0.0), 0.0);
   {$ELSE}
   SetValue(m_transformA, MakeVector(0.0, 0.0), 0.0);
   {$ENDIF}

   m_polygonB := Tb2PolygonShape.Create;
   m_polygonB.SetAsBox(0.5, 0.5);
   SetValue(m_positionB, 19.345284, 1.5632932);
   m_angleB := 1.9160721;
   {$IFDEF OP_OVERLOAD}
   m_transformB.SetValue(m_positionB, m_angleB);
   {$ELSE}
   SetValue(m_transformB, m_positionB, m_angleB);
   {$ENDIF}
end;

destructor TPolyCollision.Destroy;
begin
   m_polygonA.Free;
   m_polygonB.Free;
   inherited;
end;

procedure TPolyCollision.Step(var settings: TSettings; timeStep: PhysicsFloat);
const
   color: RGBA = (0.9, 0.9, 0.9, 1.0);
   color2: RGBA = (0.9, 0.3, 0.3, 1.0);
var
   i: Integer;
   manifold: Tb2Manifold;
   worldManifold: Tb2WorldManifold;
   v: Tb2PolyVertices;
begin
   //B2_NOT_USED(settings);

   b2CollidePolygons(nil, manifold, m_polygonA, m_polygonB, m_transformA, m_transformB, False);
   {$IFDEF OP_OVERLOAD}
   worldManifold.Initialize(manifold, m_transformA, m_transformB, m_polygonA.m_radius, m_polygonB.m_radius);
   {$ELSE}
   Initialize(worldManifold, manifold, m_transformA, m_transformB, m_polygonA.m_radius, m_polygonB.m_radius);
   {$ENDIF}

   DrawText('Use A/S/D/W to move and Q/E to rotate.');
   DrawText(Format('point count := %d', [manifold.pointCount]));

   begin
      for i := 0 to m_polygonA.m_count - 1 do
        v[i] := b2Mul(m_transformA, m_polygonA.m_vertices[i]);
      m_debugDraw.DrawPolygon(v, m_polygonA.m_count, color);

      for i := 0 to m_polygonB.m_count - 1 do
         v[i] := b2Mul(m_transformB, m_polygonB.m_vertices[i]);
      m_debugDraw.DrawPolygon(v, m_polygonB.m_count, color);
   end;

   for i := 0 to manifold.pointCount - 1 do
      m_debugDraw.DrawPoint(worldManifold.points[i], 4.0, color2);
end;

procedure TPolyCollision.Keyboard(key: Byte);
begin
   case key of
      Ord('A'): m_positionB.x := m_positionB.x - 0.1;
      Ord('D'): m_positionB.x := m_positionB.x + 0.1;
      Ord('S'): m_positionB.y := m_positionB.y - 0.1;
      Ord('W'): m_positionB.y := m_positionB.y + 0.1;
      Ord('Q'): m_angleB := m_angleB + 0.1 * Pi;
      Ord('E'): m_angleB := m_angleB - 0.1 * Pi;
   end;
   {$IFDEF OP_OVERLOAD}
   m_transformB.SetValue(m_positionB, m_angleB);
   {$ELSE}
   SetValue(m_transformB, m_positionB, m_angleB);
   {$ENDIF}
end;

initialization
   RegisterTestEntry('PolyCollision', TPolyCollision);

end.

