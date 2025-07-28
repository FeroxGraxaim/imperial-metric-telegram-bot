unit Main;

{$mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, FPHTTPClient, fpjson, jsonparser, RegExpr, opensslsockets,
  tgtypes, tgsendertypes, crncyFunctions, Math, IniFiles;

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
  JsonResponse: TJSONData;
  AiMsg: TJSONObject;
  choices: TJSONArray;
  i: integer;
label
  Send, Mesure;
begin
  try
    MsgText  := AMessage.Text;
    {WriteLn('Raw message: ', AMessage.Text);
    for i := 1 to Length(AMessage.Text) do
  writeln(Format('Char %d: %s (ord=%d hex=%x)', [i, AMessage.Text[i], Ord(AMessage.Text[i]), Ord(AMessage.Text[i])])); }
    RepMsgID := AMessage.MessageId;

    if CanBeFun(AMessage.ChatId, True) then
    begin
      Response := FunnyMsgs.FunnyMessage(MsgText);
      if Response <> 'null' then
        goto Send
      else
        goto Mesure;
    end;
    Mesure:
      Response := Mesurement.ConvertValueStr(MsgText);
    if Response <> 'null' then
      goto Send
    else
      Exit;

    Send:
      Bot.sendMessage(Response, pmMarkdown, False, nil, RepMsgID, True);
  except
    On E: Exception do
    begin
      writeln('Error: ' + E.Message);
      Bot.sendMessage(FATAL_ERR, pmDefault, False, nil, RepMsgID, True);
      Halt(1);
    end;
  end;
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
  i, ExecNumber: integer;
  IsRep: boolean;
const
  RepExec: array[1..2] of string = (
    '\b(\d+([.,]\d+)?)\s*([a-zA-Z]{3})\b',
    '\b(\$|R\$|US\$|€|£|CLP\$)\s*(\d+(?:[.,]\d+)?)\b'
    );
const
  NoReply = 'no-reply-param';
  NoParam = 'no-param';
  Error   = 'error';
begin
  with TRegExpr.Create do
  try
    for i := 1 to High(RepExec) do
    begin
      Expression := RepExec[i];
      if (REP = 'null') or not (Exec(REP)) then
        IsRep := False
      else begin
        IsRep      := True;
        ExecNumber := i;
        Break;
      end;
    end;
    if not IsRep then
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
      case ExecNumber of
        1: begin
          FromValue := StringReplace(Match[1], ',', '.', [rfReplaceAll]);
          Value     := StrToFloat(FromValue);
          Value     := RoundTo(Value, -2);
          FromCurrency := UpperCase(Match[3]);
        end;
        2: begin
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
  Bot.sendMessage(START_MSG, pmMarkdown, False, nil, 0, False);
  Bot.UpdateProcessed := True;
end;

procedure TCommandMessages.ShowHelp(ASender: TObject; const ACommand: string;
  AMessage: TTelegramMessageObj);
begin
  Bot.sendMessage(HELP_MSG, pmMarkdown, False, nil, 0, False);
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
      Bot.SendMessage(NOPARAM_MSG, pmMarkdownV2, False, nil, RepMsgId, False);
    'no-reply-param':
      Bot.SendMessage(NOREP_MSG, pmMarkdownV2, False, nil, RepMsgId, False);
    'error':
      Bot.SendMessage(ERR_MSG, pmMarkdownV2, False, nil, RepMsgId, False);
    else
      Bot.SendMessage(Response, pmMarkdownV2, False, nil, RepMsgId, False);
  end;
  Bot.UpdateProcessed := True;
end;


end.
