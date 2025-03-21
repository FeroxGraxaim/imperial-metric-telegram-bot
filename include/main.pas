unit Main;

{$mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, FPHTTPClient, fpjson, jsonparser, RegExpr, opensslsockets,
  tgtypes, tgsendertypes, crncyFunctions, Math;

var
  TELEGRAM_TOKEN: string;
  CURRENCY_TOKEN: string;
  LastUpdate: integer = 0;
  Bot: TTelegramSender;

type

  { TMainClass }

  TMainClass = class
    procedure StartBot;
    procedure SetCommands;
    procedure SendMessage(ChatID, Text: string; ReplyToMessageID: integer);
    function GetAPI: string;
    function ReadMessage: string;
    function UrlEncode(const S: string): string;

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
  msrConvert, BotMsgs;

  { MainClass }

{procedure TMainClass.DetectMessage;
var
  JSONData:    TJSONData;
  JSONArray:   TJSONArray;
  i, UpdateID: integer;
  ChatID, ReceivedText, ReplyText, Response: string;
  MessageData, ReplyData: TJSONData;
  MessageID:   integer;
begin
  JSONData := GetJSON(ReadMessage);
  try
    JSONArray := JSONData.FindPath('result') as TJSONArray;
    for i := 0 to JSONArray.Count - 1 do
    begin
      MessageData := JSONArray[i].FindPath('message');
      if MessageData = nil then
        Continue;

      if MessageData.FindPath('chat.id') = nil then
        Continue;
      ChatID := MessageData.FindPath('chat.id').AsString;

      if MessageData.FindPath('text') <> nil then
        ReceivedText := MessageData.FindPath('text').AsString
      else
        Continue;

      UpdateID  := JSONArray[i].FindPath('update_id').AsInteger;
      MessageID := MessageData.FindPath('message_id').AsInteger;

      if (ReceivedText <> '') and (Pos('/', ReceivedText) = 1) then
      begin
        ReplyData := MessageData.FindPath('reply_to_message');
        if ReplyData <> nil then
        begin
          if ReplyData.FindPath('text') <> nil then
            ReplyText := ReplyData.FindPath('text').AsString
          else
            ReplyText := '';
        end
        else
          ReplyText := '';

        case LowerCase(ReceivedText) of
          '/currency':
            Response := ProcessCurrency(ReceivedText, ReplyText, MessageData);
          '/help': Response := HelpMessage;
          else
            Response := 'Unknown command. Type /help for assistance.';
        end;
      end
      else
        Response := Mesurement.ConvertValue(ReceivedText);

      if Response <> '' then
        SendMessage(ChatID, Response, MessageID);

      if UpdateID > LastUpdate then
        LastUpdate := UpdateID;
    end;
  except
    on E: Exception do
      writeln('Error while detecting message: ' + E.Message);
  end;
  JSONData.Free;
end; }


procedure TMainClass.StartBot;
var
  ChatID: int64;
  MsgText, RepText: string;
begin
  writeln('Initializing BOT');
  Bot := TTelegramSender.Create(TELEGRAM_TOKEN);
  Bot.OnReceiveMessage := @Mesurement.DetectImperialMetric;
  with BOT do
  try
    CommandHandlers['/start']    := @CommandMessages.ShowStart;
    CommandHandlers['/help']     := @CommandMessages.ShowHelp;
    CommandHandlers['/currency'] := @CommandMessages.ConvertCurrency;
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

procedure TMainClass.SetCommands;
begin

end;

procedure TMainClass.SendMessage(ChatID, Text: string; ReplyToMessageID: integer);
begin

end;

function TMainClass.GetAPI: string;
begin
  Result := 'https://api.telegram.org/bot' + TELEGRAM_TOKEN + '/';
end;

function TMainClass.ReadMessage: string;
begin
  with TFPHTTPClient.Create(nil) do
  try
    Result := SimpleGet(GetAPI + 'getUpdates?offset=' + IntToStr(LastUpdate + 1));
  finally
    Free;
  end;
end;


function TMainClass.UrlEncode(const S: string): string;
const
  HexChars: array[0..15] of char = '0123456789ABCDEF';
var
  I: integer;
  CharCode: char;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    CharCode := S[I];
    if CharCode in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.', '~'] then
      Result := Result + CharCode
    else
    begin
      Result := Result + '%' + HexChars[Ord(CharCode) shr 4] +
        HexChars[Ord(CharCode) and $0F];
    end;
  end;
end;

{ TCommandMessages }

function TCommandMessages.CurrencyConvertMsg(MSG, REP: string): string;
var
  FromCurrency, ToCurrency, FromValue, ToValue: string;
  Value, NewValue: double;
const
  NoReply   = 'no-reply-param';
  NoParam   = 'no-param';
  Error = 'error';
begin
  with TRegExpr.Create do
  try
    if (REP = 'null') then
    begin
      Expression := '\b([a-zA-Z]{3})\s+([a-zA-Z]{3})\s+(\d+(\.\d+)?)\b';
      if not Exec(MSG) then
        Exit(NoParam)
      else begin
        FromCurrency := UpperCase(Match[1]);
        ToCurrency := UpperCase(Match[2]);
        Value := RoundTo(StrToFloat(Match[3]), -2);
      end;
    end
    else begin
      Expression := '\b(\d+(\.\d+)?)\s*([a-zA-Z]{3})\b';
      if Exec(REP) then
      begin
        Value := RoundTo(StrToFloat(Match[1]), -2);
        FromCurrency := UpperCase(Match[3]);
      end
      else begin
        Expression := '\b(\$|R\$|US\$|€|£|CLP\$)\s*(\d+(?:\.\d+)?)\b';
        if not Exec(REP) then Exit(NoReply)
        else begin
          case Match[1] of
            '$', 'US$': FromCurrency := 'USD';
            'R$': FromCurrency   := 'BRL';
            '€': FromCurrency  := 'EUR';
            '£': FromCurrency   := 'GBP';
            'CLP$': FromCurrency := 'CLP';
          end;
          Value := RoundTo(StrToFloat(Match[2]), -2);
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
      NewValue := ConvertedCurrency(Value, FromCurrency, ToCurrency);
      FromValue := FloatToStr(RoundTo(Value, -2)) + ' ' + FromCurrency;
      ToValue := FloatToStr(RoundTo(NewValue, -2)) + ' ' + ToCurrency;

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
