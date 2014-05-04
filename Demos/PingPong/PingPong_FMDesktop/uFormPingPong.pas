unit uFormPingPong;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListBox, FMX.Colors, FMX.Objects,
  UGame, UGamePingPong, uXCadencer, UPingPongTypes;

type
  TFormPingPong = class(TForm)
    pnlBottom: TPanel;
    btnNewGame: TButton;
    btnPauseResume: TButton;
    btnLoadMap: TButton;
    chkEditMode: TCheckBox;
    clrBlockColor: TColorComboBox;
    cboBlockType: TComboBox;
    btnSaveMap: TButton;
    pntbxGame: TPaintBox;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    btnDeleteBlock: TButton;
    procedure pntbxGamePaint(Sender: TObject; Canvas: TCanvas);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnNewGameClick(Sender: TObject);
    procedure btnPauseResumeClick(Sender: TObject);
    procedure btnLoadMapClick(Sender: TObject);
    procedure clrBlockColorChange(Sender: TObject);
    procedure cboBlockTypeChange(Sender: TObject);
    procedure btnSaveMapClick(Sender: TObject);
    procedure pntbxGameMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure btnDeleteBlockClick(Sender: TObject);
    procedure chkEditModeChange(Sender: TObject);
  private
    FGamePingPong: TGamePingPong;
    FCadencer: TXCadencer;
    procedure DoOnProgress(const deltaTime, newTime: Double);
    procedure SetEditingMap(Editing: Boolean);
    procedure DoNewGame;
    procedure Display;
    function GetClientMousePosition: TPointF;
    procedure ShowSelectedBlockProperty;
  public
    { Public declarations }
  end;

var
  FormPingPong: TFormPingPong;

implementation

uses
  FMX.Platform, uFMDrawUtils;

{$R *.fmx}

procedure TFormPingPong.Display;
begin
  self.Invalidate;
end;

procedure TFormPingPong.DoNewGame;
var
  i, t: Integer;
  ax, ay: Integer;
  ABlock: TBlock;
begin
  GameState := gsNotLaunched;
  Life := 3;
  Plate.Width := DefaultPlateWidth;
  Plate.X := MapWidth div 2;
  Ball.X := Plate.X;
  Ball.Y := DefaultBallRadius + DefaultPlateHeight;
  btnPauseResume.Text := 'Pause';

  if MapFileName = '' then
  begin
    if Assigned(b2World) then
      FreeAndNil(b2World);

    FreeAllBlocks;

    ay := 280;
    for i := 0 to 4 do
    begin
      ax := (MapWidth - BlockWidth * 9) div 2;
      for t := 0 to 8 do
      begin
        ABlock := TBlock.Create;
        ABlock.X := ax;
        ABlock.Y := ay;
        ABlock.Color := TAlphaColorRec.Blue;
        ABlock.BlockType := btMud;
        Blocks.Add(ABlock);
        ax := ax + BlockWidth;
      end;
      ay := ay + BlockHeight;
    end;

    MapFileName := 'default';
  end;

  LeftBlockCount := Blocks.Count;
  for i := 0 to Blocks.Count - 1 do
    if TBlock(Blocks[i]).BlockType = btUnbreakable then
      Dec(LeftBlockCount);

  InitializePhysics;
  Display;

  FCadencer.Enabled := True;
end;

procedure TFormPingPong.DoOnProgress(const deltaTime, newTime: Double);
var cp, sp: TPoint; mpos: TPointF;
begin
  mpos := GetClientMousePosition;

  cp.X := round(mpos.X);
  cp.Y := round(mpos.Y);

  DoGameProgress(deltaTime,cp,round(pntbxGame.Width));

  Display;
end;

procedure TFormPingPong.FormCreate(Sender: TObject);
begin
  FGamePingPong := TGamePingPong.Create;

  FCadencer:= TXCadencer.Create;
  FCadencer.OnProgress := DoOnProgress;
  FCadencer.Enabled := False;

  GameInit(
  70, //DefaultPlateWidth
  15, //DefaultPlateHeight
  4, //DefaultBallRadius
  600, //MapWidth
  500, //MapHeight
  40, //BlockWidth
  20, //BlockHeight
  50 //BlockMinTop
  );

  SetEditingMap(False);
end;

procedure TFormPingPong.FormDestroy(Sender: TObject);
begin
  FCadencer.Free;
  FGamePingPong.Free;
end;

procedure TFormPingPong.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
// For compatibility with mobile devices, this code moved to "btnDeleteClick"
end;

function TFormPingPong.GetClientMousePosition: TPointF;
var
  MouseService: IFMXMouseService;
  mpos: TPointF;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXMouseService, IInterface(MouseService)) then
  begin
    mpos := MouseService.GetMousePos;
    Result := ScreenToClient(mpos);
  end
  else
    raise Exception.Create('Current platform does not support IFMXMouseService');
end;

procedure TFormPingPong.pntbxGameMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  i: Integer; AX, AY: integer;
begin
  if EditingMap then
  begin
    AX := round(X);
    AY := MapHeight - round(Y);
    SelectedBlock := Map[AX div BlockHeight, AY div BlockHeight];

    if not Assigned(SelectedBlock) then
    begin

      if AY < BlockMinTop then
        ShowMessage('The brick cannot be too low.')
      else if (AX >= MapWidth - BlockWidth / 2) or Assigned(Map[AX div BlockHeight + 1][AY div BlockHeight]) then
        ShowMessage('Cannot add a block here.')
      else
      begin
        SelectedBlock := TBlock.Create;
        with SelectedBlock do
        begin
          X := (AX div BlockHeight) * BlockHeight;
          Y := (AY div BlockHeight) * BlockHeight;
          Color := TAlphaColorRec.Blue;
          BlockType := btMud;
          RestoreHP;
        end;
        Blocks.Add(SelectedBlock);
        SetMap(AX, AY, SelectedBlock);
      end;
    end;

    ShowSelectedBlockProperty;
    Display;
  end

  else
  begin
    if GameState = gsNotLaunched then
    begin
      b2BallBody.SetLinearVelocity(BallInitialVelocity);
      GameState := gsPlaying;
      FCadencer.Enabled := True;
    end;
  end;
end;

procedure TFormPingPong.ShowSelectedBlockProperty;
begin
  if Assigned(SelectedBlock) then
  begin
    cboBlockType.ItemIndex := Ord(SelectedBlock.BlockType);
    clrBlockColor.Color := SelectedBlock.Color;
  end
  else
  begin
    cboBlockType.ItemIndex := -1;
    clrBlockColor.Color := TAlphaColorRec.Null;
  end;
end;


procedure TFormPingPong.pntbxGamePaint(Sender: TObject; Canvas: TCanvas);
begin
  FGamePingPong.DoRender(Canvas, pntbxGame.BoundsRect);
end;

procedure TFormPingPong.SetEditingMap(Editing: Boolean);
var i: Integer; b: TBlock;
begin
  EditingMap := Editing;
  SelectedBlock := nil;

  if chkEditMode.IsChecked <> Editing then
    chkEditMode.IsChecked := Editing;

  clrBlockColor.Enabled := Editing;
  cboBlockType.Enabled := Editing;
  btnSaveMap.Enabled := Editing;
  btnPauseResume.Enabled := not Editing;
  btnNewGame.Enabled := not Editing;
  btnDeleteBlock.Enabled := not Editing;

  if Editing then
  begin
    FCadencer.Enabled := False;
    FillChar(Map, SizeOf(Map), 0);

    for i := 0 to Blocks.Count - 1 do
    begin
      b := TBlock(Blocks[i]);
      b.RestoreHP;
      SetMap(b.X, b.Y, b);
    end;

    Display;
  end

  else
    DoNewGame;
end;

procedure TFormPingPong.btnDeleteBlockClick(Sender: TObject);
begin
  if EditingMap and Assigned(SelectedBlock) and
    (MessageDlg('Really want to delete selected block?',
      TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes) then
  begin
    SetMap(SelectedBlock.X, SelectedBlock.Y, nil);
    Blocks.Remove(SelectedBlock);
    FreeAndNil(SelectedBlock);
    ShowSelectedBlockProperty;
    Display;
  end;
end;

procedure TFormPingPong.btnLoadMapClick(Sender: TObject);
var
  i, blockCount: Integer;
  stream: TMemoryStream;
  block: TBlock;
  prevGameState: TGameState;
  s: integer;
begin
  prevGameState := GameState;
  GameState := gsPaused;

  if OpenDialog1.Execute then
  begin
    FreeAllBlocks;
    MapFileName := OpenDialog1.FileName;

    stream := TMemoryStream.Create;
    try
      try
        stream.LoadFromFile(MapFileName);
        stream.Read(blockCount, SizeOf(blockCount));
        for i := 0 to blockCount - 1 do
        begin
          block := TBlock.Create;
          stream.Read(block.X, SizeOf(block.X));
          stream.Read(block.Y, SizeOf(block.Y));
          stream.Read(block.Color, SizeOf(block.Color));

          block.Color := VCLToFMColor(block.Color);

          stream.Read(block.BlockType, SizeOf(block.BlockType));

          Blocks.Add(block);
        end;

        if EditingMap then
          SetEditingMap(True)
        else
          DoNewGame;

      except
        ShowMessage('Corrupted map file.');
        FreeAllBlocks;
        if EditingMap then
          SetEditingMap(True)
        else
          DoNewGame;
      end;

    finally
      stream.Free;
    end;
  end
  else
    GameState := prevGameState;

end;

procedure TFormPingPong.btnNewGameClick(Sender: TObject);
begin
  DoNewGame;
end;

procedure TFormPingPong.btnPauseResumeClick(Sender: TObject);
begin
  if EditingMap or (GameState in [gsNotLaunched, gsGameOver]) then
    Exit;

  if GameState = gsPaused then
  begin
    GameState := gsPlaying;
    btnPauseResume.Text := 'Pause';
  end
  else
  begin
    GameState := gsPaused;
    btnPauseResume.Text := 'Resume';
  end;

  FCadencer.Enabled := GameState = gsPlaying;
end;

procedure TFormPingPong.btnSaveMapClick(Sender: TObject);
var
   i: Integer;
   fn: string;
   stream: TMemoryStream;
   block: TBlock;
begin
  if SaveDialog1.Execute then
  begin
    fn := SaveDialog1.FileName;
    stream := TMemoryStream.Create;
    try
      i := Blocks.Count;
      stream.Write(i, SizeOf(i));
      for i := 0 to Blocks.Count - 1 do
      begin
        block := TBlock(Blocks[i]);
        stream.Write(block.X, SizeOf(block.X));
        stream.Write(block.Y, SizeOf(block.Y));
        stream.Write(block.Color, SizeOf(block.Color));
        stream.Write(block.BlockType, SizeOf(block.BlockType));
      end;
      stream.SaveToFile(fn);
    finally
      stream.Free;
    end;
  end;
end;

procedure TFormPingPong.chkEditModeChange(Sender: TObject);
begin
  SetEditingMap(chkEditMode.IsChecked);
end;

procedure TFormPingPong.clrBlockColorChange(Sender: TObject);
begin
  if EditingMap and Assigned(SelectedBlock) then
  begin
    SelectedBlock.Color := clrBlockColor.Color;
    Display;
  end;
end;

procedure TFormPingPong.cboBlockTypeChange(Sender: TObject);
begin
  if EditingMap and Assigned(SelectedBlock) then
    SelectedBlock.BlockType := TBlockType(cboBlockType.ItemIndex);
end;

end.
