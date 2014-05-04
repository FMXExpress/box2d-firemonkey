unit Unit28;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uXCadencer, FMX.Layouts, FMX.Memo;

type
  TForm28 = class(TForm)
    ckbxEnabled: TCheckBox;
    Panel1: TPanel;
    MemoLog: TMemo;
    btnReset: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ckbxEnabledChange(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
  private
    FCadencer: TXCadencer;
    procedure DoOnProgress(const deltaTime, newTime: Double);
    procedure Log(msg: string);
  public
    { Public declarations }
  end;

var
  Form28: TForm28;

implementation

{$R *.fmx}

procedure TForm28.btnResetClick(Sender: TObject);
begin
  Log('===================================');
  Log('RESET');
  Log('===================================');

  FCadencer.Reset;
end;

procedure TForm28.ckbxEnabledChange(Sender: TObject);
begin
  FCadencer.Enabled := ckbxEnabled.IsChecked;
end;

procedure TForm28.DoOnProgress(const deltaTime, newTime: Double);
begin
  Log('delta: ' + deltaTime.ToString + ' new: ' + newTime.ToString);
end;

procedure TForm28.FormCreate(Sender: TObject);
begin
  FCadencer:= TXCadencer.Create;
  FCadencer.OnProgress := DoOnProgress;
  FCadencer.Enabled := False;

  ckbxEnabled.IsChecked := FCadencer.Enabled;
end;

procedure TForm28.FormDestroy(Sender: TObject);
begin
  FCadencer.Free;
end;

procedure TForm28.Log(msg: string);
var h,m,s,ms: word;
begin
  DecodeTime(Now,h,m,s,ms);
  MemoLog.Lines.Add('[' + h.ToString +':' + m.ToString + ':' + s.ToString + ':' + ms.ToString + '] ' + msg);

end;

end.
