unit Main;

{$mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, FPHTTPClient, fpjson, jsonparser, RegExpr, opensslsockets;

var
  TOKEN:      string;
  LastUpdate: integer = 0;

type

  { MainClass }

  { TMainClass }

  TMainClass = class
  public
    procedure SendMessage(ChatID, Text: string; ReplyToMessageID: integer);
    procedure DetectMessage;
    function GetAPI: string;
    function ReadMessage: string;
    function UrlEncode(const S: string): string;
  end;

var
  MainClass: TMainClass;

implementation

uses msrConvert;

  { MainClass }

procedure TMainClass.SendMessage(ChatID, Text: string; ReplyToMessageID: integer);
var
  Response: string;
begin
  with TFPHTTPClient.Create(nil) do
  try
    if ReplyToMessageID > 0 then
      Response := GetAPI + 'sendMessage?chat_id=' + ChatID + '&text=' +
        URLEncode(Text) + '&reply_to_message_id=' + IntToStr(ReplyToMessageID) +
        '&disable_notification=true'
    else
      Response := GetAPI + 'sendMessage?chat_id=' + ChatID + '&text=' +
        URLEncode(Text) + '&disable_notification=true';

    Get(Response);
  finally
    Free;
  end;
end;

procedure TMainClass.DetectMessage;
var
  JSONData:    TJSONData;
  JSONArray:   TJSONArray;
  i, UpdateID: integer;
  ChatID, ReceivedText, Response: string;
  MessageData: TJSONData;
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

      UpdateID := JSONArray[i].FindPath('update_id').AsInteger;

      MessageID := MessageData.FindPath('message_id').AsInteger;

      Response := Mesurement.ConvertValue(ReceivedText);
      if (Response <> '') and (Response <> 'null') then
      begin
        SendMessage(ChatID, Response, MessageID);
        writeln('Message sent: "' + Response + '"');
      end;

      if UpdateID > LastUpdate then
        LastUpdate := UpdateID;
    end;
  finally
    JSONData.Free;
  end;
end;

function TMainClass.GetAPI: string;
begin
  Result := 'https://api.telegram.org/bot' + TOKEN + '/';
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

end.
