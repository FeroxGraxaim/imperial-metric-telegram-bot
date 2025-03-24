unit Main;

{$mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, FPHTTPClient, fpjson, jsonparser, RegExpr, opensslsockets,
  tgtypes, tgsendertypes, crncyFunctions, Math, IniFiles, aichatbot;

var
  TELEGRAM_TOKEN: string;
  CURRENCY_TOKEN: string;
  AI_TOKEN: string;
  AllowBeingFun: boolean;
  LastUpdate: integer = 0;
  Bot: TTelegramSender;
  ChatCfgFile: TIniFile;

type

  { TMainClass }

  TMainClass = class
    procedure SetChatConfig(ChatID: integer; NewConfigFile: boolean);
    function CanBeFun(ChatID: integer; NewConfigFile: boolean): boolean;
    function Mentioned(const MsgText, BotName: string): boolean;
    procedure StartBot;
    procedure DetectMessage(ASender: TObject; AMessage: TTelegramMessageObj);
    destructor Destroy; override;
  end;

  { TCommandMessages }

  TCommandMessages = class(TMainClass)
  private
    function CurrencyConvertMsg(MSG, REP: string): string;
  public
    procedure ShowStart(ASender: TObject; const ACommand: string;
      AMessage: TTelegramMessageObj);
    procedure ShowHelp(ASender: TObject; const ACommand: string;
      AMessage: TTelegramMessageObj);
    procedure ConvertCurrency(ASender: TObject; const ACommand: string;
      AMessage: TTelegramMessageObj);
  end;

var
  MainClass: TMainClass;
  CommandMessages: TCommandMessages;

implementation

uses
  msrConvert, BotMsgs, fun, CfgCommands;

  { MainClass }

procedure TMainClass.SetChatConfig(ChatID: integer; NewConfigFile: boolean);
var
  SectionName: string;
begin
  SectionName := 'Chat_' + IntToStr(ChatID);
  ChatCfgFile.WriteBool(SectionName, 'BE_FUN', NewConfigFile);
end;

function TMainClass.CanBeFun(ChatID: integer; NewConfigFile: boolean): boolean;
var
  SectionName: string;
  Value: boolean;
begin
  SectionName := 'Chat_' + IntToStr(ChatID);
  Value := ChatCfgFile.ReadBool(SectionName, 'BE_FUN', NewConfigFile);
  Exit(Value);
end;

function TMainClass.Mentioned(const MsgText, BotName: string): boolean;
begin
  Result := Pos('@' + BotName, MsgText) > 0;
end;

procedure TMainClass.StartBot;
var
  ChatID: int64;
  MsgText, RepText: string;
  ChatCfgLocation: string;
begin

  writeln('Initializing BOT');
  ChatCfgLocation := ExtractFilePath(ParamStr(0)) + 'impmetbot.ini';
  ChatCfgFile := TIniFIle.Create(ChatCfgLocation);
  Bot := TTelegramSender.Create(TELEGRAM_TOKEN);
  Bot.OnReceiveMessage := @DetectMessage;
  with BOT do
  try
    CommandHandlers['/start']    := @CommandMessages.ShowStart;
    CommandHandlers['/help']     := @CommandMessages.ShowHelp;
    CommandHandlers['/currency'] := @CommandMessages.ConvertCurrency;
    CommandHandlers['/shutup']   := @ConfigCommands.ShutUp;
    CommandHandlers['/befun']    := @ConfigCommands.BeFun;
  except
    On E: Exception do
      writeln('Error while creating commands: ' + E.Message);
  end;
  writeln('BOT initialized!');

  while True do
  begin
    Bot.getUpdatesEx(0, 10);
  end;
end;

procedure TMainClass.DetectMessage(ASender: TObject; AMessage: TTelegramMessageObj);
var
  MsgText, Response, AiReply: string;
  RepMsgID: integer;
  Request: TAiRequest;
  JsonResponse: TJSONData;
  AiMsg:   TJSONObject;
  choices: TJSONArray;
label
  Send;
begin
  MsgText  := AMessage.Text;
  RepMsgID := AMessage.MessageId;

  if CanBeFun(AMessage.ChatId, True) then
  begin
    Response := FunnyMsgs.FunnyMessage(MsgText);
    if Response <> 'null' then
      goto Send;
  end;

  Response := Mesurement.ConvertValueStr(MsgText);
  if Response <> 'null' then
    goto Send
  else
    Exit;

  Send:
    Bot.sendMessage(Response, pmDefault, False, nil, RepMsgID, True);
end;

destructor TMainClass.Destroy;
begin
  ChatCfgFile.Free;
  Bot.Free;
  inherited Destroy;
end;

{ TCommandMessages }

function TCommandMessages.CurrencyConvertMsg(MSG, REP: string): string;
var
  FromCurrency, ToCurrency, FromValue, ToValue: string;
  Value, NewValue: double;
const
  NoReply = 'no-reply-param';
  NoParam = 'no-param';
  Error   = 'error';
begin
  with TRegExpr.Create do
  try
    if (REP = 'null') then
    begin
      Expression := '\b([a-zA-Z]{3})\s+([a-zA-Z]{3})\s+(\d+([.,]\d+)?)\b';
      if not Exec(MSG) then
        Exit(NoParam)
      else begin
        FromCurrency := UpperCase(Match[1]);
        ToCurrency := UpperCase(Match[2]);
        FromValue := StringReplace(Match[3], ',', '.', [rfReplaceAll]);
        Value := StrToFloat(FromValue);
        Value := RoundTo(Value, -2);
      end;
    end
    else begin
      Expression := '\b(\d+([.,]\d+)?)\s*([a-zA-Z]{3})\b';
      if Exec(REP) then
      begin
        FromValue := StringReplace(Match[1], ',', '.', [rfReplaceAll]);
        Value     := StrToFloat(FromValue);
        Value     := RoundTo(Value, -2);
        FromCurrency := UpperCase(Match[3]);
      end
      else begin
        Expression := '\b(\$|R\$|US\$|€|£|CLP\$)\s*(\d+(?:[.,]\d+)?)\b';
        if not Exec(REP) then Exit(NoReply)
        else begin
          case Match[1] of
            '$', 'US$': FromCurrency := 'USD';
            'R$': FromCurrency   := 'BRL';
            '€': FromCurrency  := 'EUR';
            '£': FromCurrency   := 'GBP';
            'CLP$': FromCurrency := 'CLP';
          end;
          FromValue := StringReplace(Match[2], ',', '.', [rfReplaceAll]);
          Value     := StrToFloat(FromValue);
          Value     := RoundTo(Value, -2);
        end;

      end;
      Expression := '\b([A-Za-z]{3})\b';
      if not Exec(MSG) then
        Exit(NoParam)
      else
        ToCurrency := UpperCase(Match[1]);
    end;

    WriteLn('From Currency: ', FromCurrency);
    WriteLn('To Currency: ', ToCurrency);
    WriteLn('Value: ', Value);

    if Concat(FromCurrency, ToCurrency) = '' then
      Exit(NoParam)
    else
    try
      NewValue  := ConvertedCurrency(Value, FromCurrency, ToCurrency);
      FromValue := FromValue + ' ' + FromCurrency;
      NewValue  := RoundTo(NewValue, -2);
      ToValue   := FloatToStr(NewValue) + ' ' + ToCurrency;

      Result := Format(RESULT_MSG, [FromValue, ToValue]);
    except
      on E: Exception do
      begin
        WriteLn('Error while converting currency: ' + E.Message);
        Exit(Error);
      end;
    end;
  finally
    Free;
  end;
end;



procedure TCommandMessages.ShowStart(ASender: TObject; const ACommand: string;
  AMessage: TTelegramMessageObj);
begin
  Bot.sendMessage(START_MSG);
  { UpdateProcessed is a flag that the Update object is processed and there is no need for further processing
      and for calling the appropriate events }
  Bot.UpdateProcessed := True;
end;

procedure TCommandMessages.ShowHelp(ASender: TObject; const ACommand: string;
  AMessage: TTelegramMessageObj);
begin
  Bot.sendMessage(HELP_MSG);
  { UpdateProcessed is a flag that the Update object is processed and there is no need for further processing
      and for calling the appropriate events }
  Bot.UpdateProcessed := True;
end;

procedure TCommandMessages.ConvertCurrency(ASender: TObject;
  const ACommand: string; AMessage: TTelegramMessageObj);
var
  Reply, Msg, Response: string;
  RepMsgID: integer;
begin
  if Assigned(AMessage.ReplyToMessage) then
    Reply := AMessage.ReplyToMessage.Text
  else
    Reply := 'null';

  Msg      := AMessage.Text;
  Response := CurrencyConvertMsg(Msg, Reply);
  RepMsgID := AMessage.MessageId;

  case Response of
    'no-param':
      Bot.SendMessage(NOPARAM_MSG, pmDefault, False, nil, RepMsgId, False);
    'no-reply-param':
      Bot.SendMessage(NOREP_MSG, pmDefault, False, nil, RepMsgId, False);
    'error':
      Bot.SendMessage(ERR_MSG, pmDefault, False, nil, RepMsgId, False);
    else
      Bot.SendMessage(Response, pmDefault, False, nil, RepMsgId, False);
  end;
  Bot.UpdateProcessed := True;
end;


end.
