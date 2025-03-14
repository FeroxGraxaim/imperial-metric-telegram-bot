unit getMeForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, IniPropStorage;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    BtnGetMe: TButton;
    EdtToken: TLabeledEdit;
    InPrpStrg: TIniPropStorage;
    MmBotInfo: TMemo;
    procedure BtnGetMeClick(Sender: TObject);
  private

  public

  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.lfm}

uses
  tgsendertypes, tgfclhttpclientbroker
  ;

{ TFrmMain }

procedure TFrmMain.BtnGetMeClick(Sender: TObject);
var
  ABot: TTelegramSender;
begin
  ABot:=TTelegramSender.Create(EdtToken.Text);
  try
    if ABot.getMe then
      MmBotInfo.Lines.Add(ABot.BotUser.AsString);
  finally
    ABot.Free;
  end;
end;

end.

