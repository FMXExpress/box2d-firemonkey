unit UPyramid;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TPyramid = class(TTester)
   private
      m_bodyCount: Integer;
   public

      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
      procedure LaunchBomb(velocity_factor: PhysicsFloat = 1.0); override;
   end;

implementation
var
   m_level: Integer;

{ TPyramid }

constructor TPyramid.Create;
const
   deltaX: TVector2 = (X: 0.5625; Y: 2.0);
   deltaY: TVector2 = (X: 1.125; Y: 0.0);
var
   i, j: Integer;
   ed: Tb2EdgeShape;
   sd: Tb2PolygonShape;
   bd: Tb2BodyDef;
   ground: Tb2Body;
   body: Tb2Body;
   x, y: TVector2;
begin
   inherited;

   begin
			bd := Tb2BodyDef.Create;
			ground := m_world.CreateBody(bd);

			ed := Tb2EdgeShape.Create;
			ed.SetVertices(MakeVector(-40.0, 0.0), MakeVector(40.0, 0.0));
			ground.CreateFixture(ed, 0.0);
   end;

   begin
      sd := Tb2PolygonShape.Create;
      sd.SetAsBox(0.5, 0.5);
      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;

      SetValue(x, -10.0, 0.75);
      m_bodyCount := 0;
      for i := 0 to m_level do
      begin
         y := x;

         for j := i to m_level do
         begin
            bd.position := y;
            body := m_world.CreateBody(bd, False);
            body.CreateFixture(sd, 5.0, False);
            Inc(m_bodyCount);

            {$IFDEF OP_OVERLOAD}
            y.AddBy(deltaY);
            {$ELSE}
            AddBy(y, deltaY);
            {$ENDIF}
         end;
         {$IFDEF OP_OVERLOAD}
         x.AddBy(deltaX);
         {$ELSE}
         AddBy(x, deltaX);
         {$ENDIF}
      end;
      sd.Free;
      bd.Free;
   end;
end;

procedure TPyramid.Step(var settings: TSettings; timeStep: PhysicsFloat);
begin
   inherited;
   DrawText(Format('Use +/- to change box count. Box Count: %d', [m_bodyCount]));
end;

procedure TPyramid.Keyboard(key: Byte);
begin
   inherited;
   case key of
      187{+}:
         if m_level < 30 then
            Inc(m_level);
      189{-}:
         if m_level > 1 then
            Dec(m_level);
   else
      Exit;
   end;
   frmMain.btnResetClick(nil);
end;

procedure TPyramid.LaunchBomb(velocity_factor: PhysicsFloat);
begin
   inherited LaunchBomb(15.0);
end;

initialization
   m_level := 25;
   RegisterTestEntry('Pyramid', TPyramid);

end.

