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
    function StartMessage: string;
    function HelpMessage: string;
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
  msrConvert;

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
  {with TBotThread.Create(Bot) do
    Start; }

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

function TCommandMessages.StartMessage: string;
begin
  Result :=
    'Hello! I''m WordWide Brotherhood BOT!' + #13#10 + #13#10 +
    'I can convert between imperial and metric mesurements, and also convert ' +
    'currency! You may add me to a group that has friends from different countries, ' +
    'so I will make your communication better! ' + #13#10 +
    'Type /help to see avaliable commands and resources!';
end;

function TCommandMessages.HelpMessage: string;
begin
  Result :=
    '*Help Message*' + #13#10 + '/help: Shows this help.' + #13#10 +
    '/currency: Used to convert currency. You can reply to a message that ' +
    'has a currency in the format [value] [currency] (Ex: 2.00 BRL) and type ' +
    'the currency you wanna convert to.' + #13#10 + 'Example: /currency BRL' +
    #13#10 + 'You can also ask to convert a specific value without needing to reply to '
    + 'another message.' + #13#10 + 'Example: `/currency USD BRL 25.00`' +
    #13#10 + 'Use `/help` to see this again.' + #13#10 + #13#10 +
    'Convertions between imperial and metric mesurements are automatic, you just ' +
    'type a value with its identifier (Ex: 35Kg) and I will convert it for you!';
end;

function TCommandMessages.CurrencyConvertMsg(MSG, REP: string): string;
var
  FromCurrency, ToCurrency: string;
  Value, NewValue: double;
begin
  with TRegExpr.Create do
  try
    Expression := '\b(\d+(\.\d+)?)\s*([a-zA-Z]{3})\b';
    if (REP <> 'null') and Exec(REP) then
    begin
      writeln('Reply detected!');
      Value := StrToFloat(Match[1]);
      FromCurrency := Match[3];
      writeln('From currency: ', FromCurrency);

      Expression := '\b([[a-zA-Z]{3})\b';
      if Exec(MSG) then
        ToCurrency := Match[1]
      else begin
        Result := 'no-param';
        Exit;
      end;
      writeln('To currency: ', ToCurrency);
    end
    else
    begin
      Expression := '\b([a-zA-Z]{3})\s+([a-zA-Z]{3})\s+(\d+(\.\d+)?)\b';
      if Exec(MSG) then
      begin
        writeln('Not detected reply, checking the message');
        FromCurrency := Match[1];
        writeln('From currency: ', FromCurrency);
        ToCurrency := Match[2];
        writeln('To currency: ', ToCurrency);
        Value := StrToFloat(Match[3]);
      end
      else begin
        Result := 'no-param';
        Exit;
      end;
    end;
  finally
    Free;
  end;

  try
    FromCurrency := UpperCase(FromCurrency);
    ToCurrency := UpperCase(ToCurrency);
    NewValue := ConvertedCurrency(Value, FromCurrency, ToCurrency);
  except
    On E: Exception do
    begin
      Result := 'Error while converting currency: ' + E.Message;
      Exit;
    end;
  end;
  Result := FloatToStr(Value) + ' ' + FromCurrency + ' is the same as ' +
    FloatToStr(RoundTo(NewValue, -2)) + ' ' + ToCurrency;
end;

procedure TCommandMessages.ShowStart(ASender: TObject; const ACommand: string;
  AMessage: TTelegramMessageObj);
begin
  Bot.sendMessage(StartMessage);
  { UpdateProcessed is a flag that the Update object is processed and there is no need for further processing
      and for calling the appropriate events }
  Bot.UpdateProcessed := True;
end;

procedure TCommandMessages.ShowHelp(ASender: TObject; const ACommand: string;
  AMessage: TTelegramMessageObj);
begin
  Bot.sendMessage(HelpMessage);
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
  if Response = 'no-param' then begin
    Bot.SendMessage('You need to specify the parameters for this command!' +
    #13#10 + 'You can reply to a message with a currency value like this: ' +
    '"25 USD" with the command + the currency you want (Ex: /currency BRL' +
    #13#10 + 'Or you can just type the command + original currency + currency ' +
    'you want (Ex: /currency USD BRL 22)', pmDefault, False, nil, RepMsgId, False);
    Exit;
  end;
  Bot.SendMessage(Response, pmDefault, False, nil, RepMsgId, False);
  Bot.UpdateProcessed := True;
end;


end.
