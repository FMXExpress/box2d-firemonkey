unit UDistanceTest;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils, OpenGL;

type
   TDistanceTest = class(TTester)
   public
      m_positionB: TVector2;
      m_angleB: PhysicsFloat;
      m_transformA, m_transformB: Tb2Transform;
      m_polygonA, m_polygonB: Tb2PolygonShape;

      constructor Create; override;
      destructor Destroy; override;

      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
   end;

implementation

{ TDistanceTest }

constructor TDistanceTest.Create;
begin
   inherited;
   begin
      {$IFDEF OP_OVERLOAD}
      m_transformA.SetIdentity;
      {$ELSE}
      SetIdentity(m_transformA);
      {$ENDIF}
      SetValue(m_transformA.p, 0.0, -0.2);
      m_polygonA := Tb2PolygonShape.Create;
      m_polygonA.SetAsBox(10.0, 0.2);
   end;

   begin
      SetValue(m_positionB, 12.017401, 0.13678508);
      m_angleB := -0.0109265;
      {$IFDEF OP_OVERLOAD}
      m_transformB.SetValue(m_positionB, m_angleB);
      {$ELSE}
      SetValue(m_transformB, m_positionB, m_angleB);
      {$ENDIF}
      m_polygonB := Tb2PolygonShape.Create;
      m_polygonB.SetAsBox(2.0, 0.1);
   end;
end;

destructor TDistanceTest.Destroy;
begin
   m_polygonA.Free;
   m_polygonB.Free;
   inherited;
end;

procedure TDistanceTest.Step(var settings: TSettings; timeStep: PhysicsFloat);
const
   color1: RGBA = (0.9, 0.9, 0.9, 1.0);
   color2: RGBA = (1.0, 0.0, 0.0, 1.0);
   color3: RGBA = (1.0, 1.0, 0.0, 1.0);
var
   i: Integer;
   input: Tb2DistanceInput;
   output: Tb2DistanceOutput;
   cache: Tb2SimplexCache;
   v: Tb2PolyVertices;
begin
   inherited;

   {$IFDEF OP_OVERLOAD}
   input.proxyA.SetShape(m_polygonA, 0);
   input.proxyB.SetShape(m_polygonB, 0);
   {$ELSE}
   SetShape(input.proxyA, m_polygonA, 0);
   SetShape(input.proxyB, m_polygonB, 0);
   {$ENDIF}
   input.transformA := m_transformA;
   input.transformB := m_transformB;
   input.useRadii := True;
   cache.count := 0;
   UPhysics2D.b2Distance(output, cache, input);

   DrawText('Use A/D/S/W/Q/E to move and rotate.');
   DrawText(Format('distance := %f', [output.distance]));
   DrawText(Format('iterations := %d', [output.iterations]));

   begin
      for i := 0 to m_polygonA.m_count - 1 do
         v[i] := b2Mul(m_transformA, m_polygonA.m_vertices[i]);
      m_debugDraw.DrawPolygon(v, m_polygonA.m_count, color1);

      for i := 0 to m_polygonB.m_count - 1 do
         v[i] := b2Mul(m_transformB, m_polygonB.m_vertices[i]);
      m_debugDraw.DrawPolygon(v, m_polygonB.m_count, color1);
   end;

   m_debugDraw.DrawPoint(output.pointA, 4.0, color2);
   m_debugDraw.DrawPoint(output.pointB, 4.0, color3);
end;

procedure TDistanceTest.Keyboard(key: Byte);
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
   RegisterTestEntry('Distance Test', TDistanceTest);

end.


