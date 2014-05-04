unit URope;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TRope = class(TTester)
   public
     	m_rope: Tb2Rope;
	    m_angle: PhysicsFloat;

      constructor Create; override;
      destructor Destroy; override;

      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
   end;


implementation

{ TRope }

constructor TRope.Create;
const N = 40;

var
   i: Integer;
   vertices: TVectorArray;
   masses: TPhysicsFloatArray;
   def: Tb2RopeDef;
begin
   inherited;

   SetLength(vertices, N);
   SetLength(masses, N);

   for i := 0 to N - 1 do
   begin
      SetValue(vertices[i], 0.0, 20.0 - 0.25 * i);
			masses[i] := 1.0;
   end;

   masses[0] := 0.0;
   masses[1] := 0.0;

   def := Tb2RopeDef.Create;
   def.vertices := vertices;
   def.count := N;
   SetValue(def.gravity, 0.0, -10.0);
   def.masses := masses;
   def.damping := 0.1;
   def.k2 := 1.0;
   def.k3 := 0.5;

   m_rope := Tb2Rope.Create(def);

   m_angle := 0.05 * Pi;
   m_rope.SetAngle(m_angle);
end;

destructor TRope.Destroy;
begin
   m_rope.Free;
   inherited;
end;

procedure TRope.Keyboard(key: Byte);
begin
   if key = Ord('Q') then
   begin
      m_angle := b2Max(-Pi, m_angle - 0.05 * Pi);
			m_rope.SetAngle(m_angle);
   end
   else if key = Ord('E') then
   begin
			m_angle := b2Min(Pi, m_angle + 0.05 * Pi);
			m_rope.SetAngle(m_angle);
   end;
end;

procedure TRope.Step(var settings: TSettings; timeStep: PhysicsFloat);
begin
   m_rope.Step(timeStep, 1);
   inherited;

   m_rope.Draw(m_debugDraw);
   DrawText('Press (q, e) to adjust target angle');
   DrawText(Format('Target angle = %g degrees', [m_angle * 180.0 / Pi]));
end;

initialization
   RegisterTestEntry('Free Rope', TRope);

end.
