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
    function IsAdmin(ChatID, UserID: int64): boolean;
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
    if not IsAdmin(ChatID, UserID) then
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
    if not IsAdmin(ChatID, UserID) then
      BotMsg := NOT_ADM_BEFUN
    else begin
      MainClass.SetChatConfig(ChatId, True);
      BotMsg := BE_FUNNY;
    end;
  end;

  Bot.SendMessage(BotMsg, pmDefault, False, nil, RepMsgId, False);
end;

function TConfigCommands.IsAdmin(ChatID, UserID: int64): boolean;
var
  ChatMember: TTelegramChatMember;
  IsTrue: boolean;
begin
  if ChatID > 0 then
    Exit(True);

  if not Bot.getChatMember(ChatID, UserID, ChatMember) then
    Exit(False)
    else
      IsTrue := (ChatMember.StatusType = msCreator) or (ChatMember.StatusType =
                msAdministrator);
  Exit(IsTrue);
end;

end.
