unit uMySimulation1;

interface

uses
  uCustomSimulation;

type
  TMySimulation1 = class(TCustomSimulation)
  private
    m_bodyCount: Integer;
    class var m_level: Integer;
    procedure Keyboard(key: Byte);
  public
    class constructor Create;
    constructor Create; override;
    destructor Destroy; override;
    procedure Step(var settings: TCustomSimulationSettings; timeStep: TSimulationTime);
    procedure UserInput(aUserInput: TSimulationUserInput); override;
  end;

implementation

uses
  System.SysUtils,
  UPhysics2DTypes,
  UPhysics2D;

{ TMySimulation1 }

class constructor TMySimulation1.Create;
begin
  inherited;
  m_level := 5;
end;

constructor TMySimulation1.Create;
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

  bd := Tb2BodyDef.Create;

  ground := m_world.CreateBody(bd);

  ed := Tb2EdgeShape.Create;
  ed.SetVertices(MakeVector(-40.0, 0.0), MakeVector(40.0, 0.0));

  ground.CreateFixture(ed, 0.0);

  sd := Tb2PolygonShape.Create;
  sd.SetAsBox(0.5, 0.5);

  bd := Tb2BodyDef.Create;
  bd.bodyType := b2_dynamicBody;

  try
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

  finally
    sd.Free;
    bd.Free;
  end;
end;

destructor TMySimulation1.Destroy;
begin

  inherited;
end;

procedure TMySimulation1.Step(var settings: TCustomSimulationSettings; timeStep: TSimulationTime);
begin
  inherited;
  DrawText(Format('Use +/- to change box count. Box Count: %d', [m_bodyCount]));
end;

procedure TMySimulation1.UserInput(aUserInput: TSimulationUserInput);
begin
  inherited;

  Keyboard(aUserInput.KeyboardKey);
end;

procedure TMySimulation1.Keyboard(key: Byte);
begin
   inherited;

   if key > 0 then
   begin
     case key of
        187, 43{+}:
           if m_level < 30 then
              Inc(m_level);
        189, 95{-}:
           if m_level > 1 then
              Dec(m_level);
     else
        Exit;
     end;

     DoResetTest;
   end;
end;


initialization
   RegisterSimulationEntry('AMySimulation1', TMySimulation1);

end.
