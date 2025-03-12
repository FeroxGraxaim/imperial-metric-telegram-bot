unit Main;

{$mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, FPHTTPClient, fpjson, jsonparser, RegExpr, opensslsockets;
var
  TOKEN: string;
  LastUpdate: integer = 0;

type

  { MainClass }

  { TMainClass }

  TMainClass = class
  public
    procedure SendMessage(ChatID, Text: string; ReplyToMessageID: integer);
    procedure DetectMessage;
    function GetAPI: string;
    function ConvertValue(Message: string): string;
    function ReadMessage: string;
    function UrlEncode(const S: string): string;
  end;

var
  MainClass: TMainClass;

implementation

{ MainClass }

procedure TMainClass.SendMessage(ChatID, Text: string; ReplyToMessageID: integer);
var
  HTTP:     TFPHTTPClient;
  Response: string;
begin
  HTTP := TFPHTTPClient.Create(nil);
  try
    if ReplyToMessageID > 0 then
      Response := GetAPI + 'sendMessage?chat_id=' + ChatID + '&text=' +
        URLEncode(Text) + '&reply_to_message_id=' + IntToStr(ReplyToMessageID)
    else
      Response := GetAPI + 'sendMessage?chat_id=' + ChatID + '&text=' +
      URLEncode(Text);

    HTTP.Get(Response);
  finally
    HTTP.Free;
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

      Response := ConvertValue(ReceivedText);
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

function TMainClass.ConvertValue(Message: string): string;
var
  LB, KG, OZ, G, L, GAL, ML, FLOZ, KM, MI, M, FT, CM, INCH, C, F, MHP, HP: double;
  ExprID, i: integer;
const
  Patterns: array[1..18] of string = (
    '\b(\d+(\.\d+)?)\s*(lb|lbs|pounds|pound)\b',               //Pounds
    '\b(\d+(\.\d+)?)\s*(kg|kilo|kilogram|kilograms)\b',        //Kilograms
    '\b(\d+(\.\d+)?)\s*(oz|ounce, ounces)\b',                  //Ounces
    '\b(\d+(\.\d+)?)\s*(g|gram|grams)\b',                      //Grams
    '\b(\d+(\.\d+)?)\s*(l|liter|liters)\b',                    //Liters
    '\b(\d+(\.\d+)?)\s*(gal|gallon|gallons)\b',                //Gallons
    '\b(\d+(\.\d+)?)\s*(ml|mililiter|mililiters)\b',           //Mililiters
    '\b(\d+(\.\d+)?)\s*(fl|floz|fl oz|oz fl)\b',               //Fluid ounces
    '\b(\d+(\.\d+)?)\s*(km|kilometer|kilometers|km/h|kmph)\b', //Kilometers
    '\b(\d+(\.\d+)?)\s*(mi|miles|mile|mi/h|mph)\b',            //Miles
    '\b(\d+(\.\d+)?)\s*(m|meter|meters|m/s)\b',                //Meters
    '\b(\d+(\.\d+)?)\s*(ft|foot|feet)\b',                      //feet
    '\b(\d+(\.\d+)?)\s*(cm|centimeter|centimeters)\b',         //Centimeters
    '\b(\d+(\.\d+)?)\s*(in|inch|inches)\b',                    //Inches
    '\b(\d+(\.\d+)?)\s*(C|\xB0C|\xBAC|celsius)\b',             //Celsius
    '\b(\d+(\.\d+)?)\s*(F|\xB0F|\xBAF|fahrenheit)\b',          //Fahrenheit
    '\b(\d+(\.\d+)?)\s*(mhp|cv|ps)\b',                         //Metric HorsePower
    '\b(\d+(\.\d+)?)\s*(hp|Hp|HP)\b');                         //Imperial HorsePower
begin
  Result := 'null';
  ExprID := 0;
  with TRegExpr.Create do
  try
    ModifierI := True;
    for i := Low(Patterns) to High(Patterns) do
    begin
      Expression := Patterns[i];
      if Exec(Message) then
      begin
        ExprID := i;
        Break;
      end;
    end;
    case ExprID of
      1: //Pounds to Kilograms
      begin
        LB     := StrToFLoat(Match[1]);
        KG     := LB * 0.453592;
        Result := Format('%0.2flb is the same as %0.2fKg.', [LB, KG]);
      end;
      2: //Kilograms to pounds
      begin
        KG     := StrToFLoat(Match[1]);
        LB     := KG * 2.20462;
        Result := Format('%0.2fKg is the same as %0.2flb.', [KG, LB]);
      end;
      3: //Ounces to grams
      begin
        OZ     := StrToFLoat(Match[1]);
        G      := OZ * 28.35;
        Result := Format('%0.2fOz is the same as %0.2fg.', [OZ, G]);
      end;
      4: //Grams to ounces
      begin
        G      := StrToFLoat(Match[1]);
        OZ     := G / 28.35;
        Result := Format('%0.2fg is the same as %0.2fOz.', [G, OZ]);
      end;
      5: //Liters to gallons
      begin
        L      := StrToFLoat(Match[1]);
        GAL    := L / 3.785;
        Result := Format('%0.2fL is the same as %0.2fGal.', [L, GAL]);
      end;
      6: //Gallons to liters
      begin
        GAL    := StrToFLoat(Match[1]);
        L      := GAL * 3.785;
        Result := Format('%0.2fGal is the same as %0.2fL.', [GAL, L]);
      end;
      7: //Mililiters to Fluid Ounces
      begin
        ML     := StrToFLoat(Match[1]);
        FLOZ   := ML / 29.5735;
        Result := Format('%0.2fmL is the same as %0.2ffl oz.', [ML, FLOZ]);
      end;
      8: //Fluid ounces to mililiters
      begin
        FLOZ   := StrToFLoat(Match[1]);
        ML     := FLOZ * 29.5735;
        Result := Format('%0.2ffl oz is the same as %0.2fmL.', [FLOZ, ML]);
      end;
      9: //Kilometers to miles
      begin
        KM     := StrToFLoat(Match[1]);
        MI     := KM / 1.609;
        Result := Format('%0.2fKm is the same as %0.2fmi.', [KM, MI]);
      end;
      10: //Miles to kilometers
      begin
        MI     := StrToFLoat(Match[1]);
        KM     := MI * 1.609;
        Result := Format('%0.2fmi is the same as %0.2fKm.', [MI, KM]);
      end;
      11: //Meters to feet
      begin
        M      := StrToFLoat(Match[1]);
        FT     := M * 3.281;
        Result := Format('%0.2fm is the same as %0.2fft.', [M, FT]);
      end;
      12: //Feet to meters
      begin
        FT     := StrToFLoat(Match[1]);
        M      := FT / 3.281;
        Result := Format('%0.2fft is the same as %0.2fm.', [FT, M]);
      end;
      13: //Centimeters to inches
      begin
        CM     := StrToFLoat(Match[1]);
        INCH   := CM / 2.54;
        Result := Format('%0.2fcm is the same as %0.2fin.', [CM, INCH]);
      end;
      14: //Inches to centimeters
      begin
        INCH   := StrToFLoat(Match[1]);
        CM     := INCH * 2.54;
        Result := Format('%0.2fin is the same as %0.2fcm.', [INCH, CM]);
      end;
      15: //Celsius to fahrenheit
      begin
        C      := StrToFLoat(Match[1]);
        F      := (C * 9 / 5) + 32;
        Result := Format('%0.2fºC is the same as %0.2fºF.', [C, F]);
      end;
      16: //Fahrenheit to celsius
      begin
        F      := StrToFLoat(Match[1]);
        C      := (F - 32) * 5 / 9;
        Result := Format('%0.2fºF is the same as %0.2fºC.', [F, C]);
      end;
      17: //Metric HorsePower to Imperial HorsePower
      begin
        MHP    := StrToFLoat(Match[1]);
        HP     := MHP / 1.014;
        Result := Format('%0.2fmhp is the same as %0.2fhp.', [MHP, HP]);
      end;
      18: //Imperial HorsePower to Metric HorsePower
      begin
        HP     := StrToFLoat(Match[1]);
        MHP    := HP * 1.014;
        Result := Format('%0.2fhp is the same as %0.2fmhp.', [HP, MHP]);
      end;
      else
        Result := 'null';
    end;
  finally
    Free;
  end;
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
