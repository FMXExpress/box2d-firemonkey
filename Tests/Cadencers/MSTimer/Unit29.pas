unit Unit29;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm29 = class(TForm)
    Panel1: TPanel;
    MemoLog: TMemo;
    chkbxEnabled: TCheckBox;
    btnReset: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure chkbxEnabledClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
  private
    procedure Log(msg: string);
    procedure TimerProgress(const deltaTime, newTime: Double);
  public
    { Public declarations }
  end;

var
  Form29: TForm29;

implementation

uses
  MSTimer;

{$R *.dfm}

{ TForm29 }

procedure TForm29.btnResetClick(Sender: TObject);
begin
  Log('===================================');
  Log('RESET');
  Log('===================================');

  MSCadencer.Reset;
end;

procedure TForm29.chkbxEnabledClick(Sender: TObject);
begin
  MSCadencer.Enabled := chkbxEnabled.Checked;
end;

procedure TForm29.FormCreate(Sender: TObject);
begin
  MSCadencer := TMSTimer.Create;
  MSCadencer.OnProgress := TimerProgress;
  MSCadencer.Enabled := False;

  chkbxEnabled.Checked := MSCadencer.Enabled;
end;

procedure TForm29.FormDestroy(Sender: TObject);
begin
  MSCadencer.Free;
end;

procedure TForm29.Log(msg: string);
var h,m,s,ms: word;
begin
  DecodeTime(Now,h,m,s,ms);
  MemoLog.Lines.Add('[' + h.ToString +':' + m.ToString + ':' + s.ToString + ':' + ms.ToString + '] ' + msg);
end;

procedure TForm29.TimerProgress(const deltaTime, newTime: Double);
begin
  Log('deltaTime: ' + deltaTime.ToString + ' - newTime: ' + newTime.ToString);
end;

end.
