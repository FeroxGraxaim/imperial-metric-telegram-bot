unit CfgCommands;

{$mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, tgtypes, tgsendertypes, IniFiles, Main, BotMsgs,
  fptelegram, fpjson;

type

  { TConfigCommands }

  TConfigCommands = class(TMainClass)
    procedure ShutUp(ASender: TObject; const ACommand: string;
      AMessage: TTelegramMessageObj);
    procedure BeFun(ASender: TObject; const ACommand: string;
      AMessage: TTelegramMessageObj);
  end;

var
  ConfigCommands: TConfigCommands;

implementation

{ TConfigCommands }

procedure TConfigCommands.ShutUp(ASender: TObject; const ACommand: string;
  AMessage: TTelegramMessageObj);
var
  RepMsgId, ChatId, UserId: int64;
  BotMsg: string;
begin
  ChatId   := AMessage.ChatId;
  RepMsgId := AMessage.MessageId;
  UserId   := AMessage.From.ID;
  if not MainClass.CanBeFun(ChatId, True) then
    BotMsg := ALREADY_QUIET
  else begin
    if not Bot.CurrentIsAdminUser then
      BotMsg := NOT_ADM_SHTUP
    else begin
      MainClass.SetChatConfig(ChatId, False);
      BotMsg := SHUT_UP;
    end;
  end;

  Bot.SendMessage(BotMsg, pmDefault, False, nil, RepMsgId, False);
end;

procedure TConfigCommands.BeFun(ASender: TObject; const ACommand: string;
  AMessage: TTelegramMessageObj);
var
  RepMsgId, ChatId, UserId: int64;
  BotMsg: string;
begin
  ChatId   := AMessage.ChatId;
  RepMsgId := AMessage.MessageId;
  UserId   := AMessage.From.ID;
  if MainClass.CanBeFun(ChatId, True) then
    BotMsg := ALREADY_FUNNY
  else begin
    if not Bot.CurrentIsAdminUser then
      BotMsg := NOT_ADM_BEFUN
    else begin
      MainClass.SetChatConfig(ChatId, True);
      BotMsg := BE_FUNNY;
    end;
  end;

  Bot.SendMessage(BotMsg, pmDefault, False, nil, RepMsgId, False);
end;

end.
