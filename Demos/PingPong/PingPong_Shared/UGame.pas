unit UGame;

interface

type
  TGameState = (gsNotLaunched, gsPlaying, gsPaused, gsGameOver, gsGameFinished);

type
  TGame = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure DoProgress(const deltaTime, newTime: TTime); virtual; abstract;
  end;

implementation

{ TGame }

constructor TGame.Create;
begin
//
end;

destructor TGame.Destroy;
begin

  inherited;
end;

end.
