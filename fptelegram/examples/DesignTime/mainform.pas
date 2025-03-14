unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, DTTelegramBot, tgtypes, tgsendertypes, tgbot_dt
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnStart: TButton;
    BtnStop: TButton;
    DTLongPolBot1: TDTLongPollBot;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    procedure BtnStartClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure DTLongPollBot1ReceiveMessageUpdate(ASender: TObject; AMessage: TTelegramMessageObj);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses
  tgutils
  ;

{$R *.lfm}

{ TForm1 }

procedure TForm1.BtnStartClick(Sender: TObject);
begin
  BtnStart.Enabled:=False;
  try
    DTLongPolBot1.StartReceiver;
  except
    on E: EDTTelegramBot do
    begin
      BtnStart.Enabled:=True;
      BtnStop.Enabled:=False;
      raise;
    end;
  end;
  BtnStop.Enabled:=True;
end;

procedure TForm1.BtnStopClick(Sender: TObject);
begin
  BtnStop.Enabled:=False;
  DTLongPolBot1.StopReceiver;
  BtnStart.Enabled:=True;
end;

procedure TForm1.DTLongPollBot1ReceiveMessageUpdate(ASender: TObject; AMessage: TTelegramMessageObj);
var
  aReply, aMsg: String;
  aReplyMarkup: TReplyMarkup;
begin
  aReplyMarkup:=nil;
  try
    if AMessage.Text='/inlinekeyboard' then
    begin
      if DTLongPolBot1.ReplyMarkups.Count>=1 then
        aReplyMarkup:=DTLongPolBot1.ReplyMarkups.ReplyMarkups[0].GetJSONReplyMarkup;
      aMsg:=Format('Hi, %s', [CaptionFromUser(AMessage.From)])+'!'+LineEnding+'This message with inline keyboard';
      TTelegramSender(ASender).UpdateProcessed:=True;
    end;
    if AMessage.Text='/replykeyboard' then
    begin
      if DTLongPolBot1.ReplyMarkups.Count>=2 then
        aReplyMarkup:=DTLongPolBot1.ReplyMarkups.ReplyMarkups[1].GetJSONReplyMarkup;
      aMsg:=Format('Hi, %s', [CaptionFromUser(AMessage.From)])+'!'+LineEnding+'This message with reply keyboard';
      TTelegramSender(ASender).UpdateProcessed:=True;
    end;                            
    if AMessage.Text='/forcereply' then
    begin
      if DTLongPolBot1.ReplyMarkups.Count>=3 then
        aReplyMarkup:=DTLongPolBot1.ReplyMarkups.ReplyMarkups[2].GetJSONReplyMarkup;
      aMsg:=Format('Hi, %s', [CaptionFromUser(AMessage.From)])+'!'+LineEnding+'This message with ForceReply';
      TTelegramSender(ASender).UpdateProcessed:=True;
    end;
    if AMessage.Text='/removekeyboard' then
    begin
      if DTLongPolBot1.ReplyMarkups.Count>=2 then
        aReplyMarkup:=DTLongPolBot1.ReplyMarkups.ReplyMarkups[3].GetJSONReplyMarkup;
      aMsg:=Format('Hi, %s', [CaptionFromUser(AMessage.From)])+'!'+LineEnding+'This message remove replykeyboard';
      TTelegramSender(ASender).UpdateProcessed:=True;
    end;
    if not TTelegramSender(ASender).UpdateProcessed then
      aMsg:=Format('You sent the text: %s', [AMessage.Text]);
    TgBotSendMessage(DTLongPolBot1.Token, TTelegramSender(ASender).CurrentChatId, aMsg, aReply, pmDefault, False,
      aReplyMarkup);
  finally
    TTelegramSender(ASender).UpdateProcessed:=True;
    aReplyMarkup.Free;
    Memo1.Lines.Add(aReply);
  end;
end;

end.

