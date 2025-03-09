unit untMain;

{$mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, FPHTTPClient, fpjson, jsonparser, RegExpr, opensslsockets;

const
  TOKEN = '7562929498:AAG1MMJQILkjk8hX6yi--5uUkwmpSlsBNt0'; //Main token
  //TOKEN = '7345917722:AAFoWLfAzekmL-KuIR2wtMHbc2fYafQLeB0'; //Test token
  API   = 'https://api.telegram.org/bot' + TOKEN + '/';

var
  LastUpdate: integer = 0;

type

  { MainClass }

  { TMainClass }

  TMainClass = class
  public

    procedure SendMessage(ChatID, Text: string; ReplyToMessageID: integer);
    procedure DetectMessage;

    function ReadMessage: string;
    function DetectPounds(Message: string; var LB: double): boolean;
    function DetectKilograms(Message: string; var KG: double): boolean;
    function DetectOz(Message: string; var OZ: double): boolean;
    function DetectGrams(Message: string; var G: double): boolean;
    function DetectLiters(Message: string; var L: double): boolean;
    function DetectGallons(Message: string; var GAL: double): boolean;
    function DetectML(Message: string; var ML: double): boolean;
    function DetectFLOZ(Message: string; var FLOZ: double): boolean;
    function DetectKilometers(Message: string; var KM: double): boolean;
    function DetectMiles(Message: string; var MI: double): boolean;
    function DetectMeters(Message: string; var M: double): boolean;
    function DetectFeet(Message: string; var FT: double): boolean;
    function DetectCentimeters(Message: string; var CM: double): boolean;
    function DetectInches(Message: string; var INCH: double): boolean;
    function DetectCelsius(message: string; var C: double): boolean;
    function DetectFahrenheit(Message: string; var F: double): boolean;

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
      Response := API + 'sendMessage?chat_id=' + ChatID + '&text=' +
        URLEncode(Text) + '&reply_to_message_id=' + IntToStr(ReplyToMessageID)
    else
      Response := API + 'sendMessage?chat_id=' + ChatID + '&text=' + URLEncode(Text);

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
  LB, KG, L, GAL, ML, FLOZ, OZ, G, KM, MI, M, FT, INCH, CM, C, F: double;
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

      //Pounds to kilograms
      if DetectPounds(ReceivedText, LB) then
      begin
        KG := LB * 0.453592;
        Response := Format('%0.2flb is the same as %0.2fKg.', [LB, KG]);
        SendMessage(ChatID, Response, MessageID);
      end;

      //Kilograms to pounds
      if DetectKilograms(ReceivedText, KG) then
      begin
        LB := KG * 2.20462;
        Response := Format('%0.2fKg is the same as %0.2flb.', [KG, LB]);
        SendMessage(ChatID, Response, MessageID);
      end;

      //OZ to Grams
      if DetectOz(ReceivedText, OZ) then
      begin
        G := OZ / 28.35;
        Response := Format('%0.2fOz is the same as %0.2fg.', [OZ, G]);
        SendMessage(ChatID, Response, MessageID);
      end;

      //Grams to Oz
      if DetectGrams(ReceivedText, G) then
      begin
        OZ := G * 28.35;
        Response := Format('%0.2fg is the same as %0.2fOz.', [G, OZ]);
        SendMessage(ChatID, Response, MessageID);
      end;

      //Liters to galoons
      if DetectLiters(ReceivedText, L) then
      begin
        GAL      := L / 3.785;
        Response := Format('%0.2fL is the same as %0.2fGal.', [L, GAL]);
        SendMessage(ChatID, Response, MessageID);
      end;

      //Gallons to liters
      if DetectGallons(ReceivedText, GAL) then
      begin
        L := GAL * 3.785;
        Response := Format('%0.2fGal is the same as %0.2fL.', [GAL, L]);
        SendMessage(ChatID, Response, MessageID);
      end;

      //ML to fl oz
      if DetectML(ReceivedText, ML) then
      begin
        FLOZ     := ML / 29.5735;
        Response := Format('%0.2fmL is the same as %0.2ffl oz.', [ML, FLOZ]);
        SendMessage(ChatID, Response, MessageID);
      end;

      //FL OZ to ML
      if DetectFLOZ(ReceivedText, FLOZ) then
      begin
        ML := FLOZ * 29.5735;
        Response := Format('%0.2ffl oz is the same as %0.2fmL.', [FLOZ, ML]);
        SendMessage(ChatID, Response, MessageID);
      end;

      //Kilometers to miles:
      if DetectKilometers(ReceivedText, KM) then
      begin
        MI := KM / 1.609;
        Response := Format('%0.2fKm is the same as %0.2fmi.', [KM, MI]);
        SendMessage(ChatID, Response, MessageID);
      end;

      //Miles to kilometers
      if DetectMiles(ReceivedText, MI) then
      begin
        KM := MI * 1.609;
        Response := Format('%0.2fmi is the same as %0.2fKm.', [MI, KM]);
        SendMessage(ChatID, Response, MessageID);
      end;

      //Meters to feet
      if DetectMeters(ReceivedText, M) then
      begin
        FT := M * 3.281;
        Response := Format('%0.2fm is the same as %0.2fft.', [M, FT]);
        SendMessage(ChatID, Response, MessageID);
      end;

      //Feet to meter
      if DetectFeet(ReceivedText, FT) then
      begin
        M := FT / 3.281;
        Response := Format('%0.2fft is the same as %0.2fm.', [FT, M]);
        SendMessage(ChatID, Response, MessageID);
      end;

      //Centimeters to inches
      if DetectCentimeters(ReceivedText, CM) then
      begin
        INCH := CM / 2.54;
        Response := Format('%0.2fcm is the same as %0.2fin.', [CM, INCH]);
        SendMessage(ChatID, Response, MessageID);
      end;

      //Inches to centimeters
      if DetectInches(ReceivedText, INCH) then
      begin
        CM := INCH * 2.54;
        Response := Format('%0.2fin is the same as %0.2fcm.', [CM, INCH]);
        SendMessage(ChatID, Response, MessageID);
      end;

      //Celsius to Fahrenheit
      if DetectCelsius(ReceivedText, C) then
      begin
        F := (C * 9 / 5) + 32;
        Response := Format('%0.2fºC is the same as %0.2fºF.', [C, F]);
        SendMessage(ChatID, Response, MessageID);
      end;

      //Fahrenheit to Celsius
       if DetectFahrenheit(ReceivedText, F) then
      begin
        C := (F - 32) * 5/9;
        Response := Format('%0.2fºF is the same as %0.2fºC.', [F, C]);
        SendMessage(ChatID, Response, MessageID);
      end;

      if Response <> '' then
        writeln('Message sent: "' + Response + '"');

      if UpdateID > LastUpdate then
        LastUpdate := UpdateID;
    end;
  finally
    JSONData.Free;
  end;
end;



function TMainClass.ReadMessage: string;
begin
  with TFPHTTPClient.Create(nil) do
  try
    Result := SimpleGet(API + 'getUpdates?offset=' + IntToStr(LastUpdate + 1));
  finally
    Free;
  end;
end;

function TMainClass.DetectPounds(Message: string; var LB: double): boolean;
begin
  with TRegExpr.Create do
  try
    Expression := '\b(\d+(\.\d+)?)\s*(lb|pounds)\b';
    if Exec(Message) then begin
      LB := StrToFLoat(Match[1]);
      Exit(True);
    end;
    Exit(False);
  finally
    Free;
  end;
end;

function TMainClass.DetectKilograms(Message: string; var KG: double): boolean;
begin
  with TRegExpr.Create do
  try
    Expression := '\b(\d+(\.\d+)?)\s*(kg|Kg|kilos|Kilos|kilo|Kilo|kilograms|Kilograms|kilogram|Kilogram)\b';
    if Exec(Message) then begin
      writeln('Detected KG: ' + Match[0]);
      KG := StrToFloat(Match[1]);
      Exit(True);
    end;
    Exit(False);
  finally
    Free;
  end;
end;


function TMainClass.DetectOz(Message: string; var OZ: double): boolean;
begin
  with TRegExpr.Create do
  try
    Expression := '\b(\d+(\.\d+)?)\s*(oz|Oz|OZ)\b';
    if Exec(Message) then begin
      writeln('Detected: ' + Message);
      OZ := StrToFloat(Match[1]);
      Exit(True);
    end;
    Exit(False);
  finally
    Free;
  end;
end;

function TMainClass.DetectGrams(Message: string; var G: double): boolean;
begin
  with TRegExpr.Create do
  try
    Expression := '\b(\d+(\.\d+)?)\s*(g|G)\b';
    if Exec(Message) then begin
      writeln('Detected: ' + Message);
      G := StrToFloat(Match[1]);
      Exit(True);
    end;
    Exit(False);
  finally
    Free;
  end;
end;

function TMainClass.DetectLiters(Message: string; var L: double): boolean;
begin
  with TRegExpr.Create do
  try
    Expression := '\b(\d+(\.\d+)?)\s*(l|L|liter|Liter|liters|Liters|LITER' +
    '|LITERS)\b';
    if Exec(Message) then begin
      writeln('Detected: ' + Message);
      L := StrToFloat(Match[1]);
      Exit(True);
    end;
    Exit(False);
  finally
    Free;
  end;
end;

function TMainClass.DetectGallons(Message: string; var GAL: double): boolean;
begin
  with TRegExpr.Create do
  try
    Expression := '\b(\d+(\.\d+)?)\s*(gal|Gal|gallon|Gallon|gallons|Gallons' +
    '|GALLONS|GALLON)\b';
    if Exec(Message) then begin
      writeln('Detected: ' + Message);
      GAL := StrToFloat(Match[1]);
      Exit(True);
    end;
    Exit(False);
  finally
    Free;
  end;
end;

function TMainClass.DetectML(Message: string; var ML: double): boolean;
begin
  with TRegExpr.Create do
  try
    Expression := '\b(\d+(\.\d+)?)\s*(ml|mL|ML|mililiters|Mililiters' +
    'MILILITERS|mililiter|Mililiter|MILILITER)\b';
    if Exec(Message) then begin
      writeln('Detected: ' + Message);
      ML := StrToFloat(Match[1]);
      Exit(True);
    end;
    Exit(False);
  finally
    Free;
  end;
end;

function TMainClass.DetectFLOZ(Message: string; var FLOZ: double): boolean;
begin
  with TRegExpr.Create do
  try
    Expression := '\b(\d+(\.\d+)?)\s*(fl oz|FL OZ|floz|Floz| Fl oz |FLOZ)\b';
    if Exec(Message) then begin
      writeln('Detected: ' + Message);
      FLOZ := StrToFloat(Match[1]);
      Exit(True);
    end;
    Exit(False);
  finally
    Free;
  end;
end;

function TMainClass.DetectKilometers(Message: string; var KM: double): boolean;
begin
  with TRegExpr.Create do
  try
    Expression := '\b(\d+(\.\d+)?)\s*(Km|km|kilometers|Kilometers|Km/h)\b';
    if Exec(Message) then begin
      writeln('Detected: ' + Message);
      KM := StrToFloat(Match[1]);
      Exit(True);
    end;
    Exit(False);
  finally
    Free;
  end;
end;

function TMainClass.DetectMiles(Message: string; var MI: double): boolean;
begin
  with TRegExpr.Create do
  try
    Expression := '\b(\d+(\.\d+)?)\s*(mi|Mi|mph|Mph|mi/h|Mi/h)\b';
    if Exec(Message) then begin
      writeln('Detected: ' + Message);
      MI := StrToFloat(Match[1]);
      Exit(True);
    end;
    Exit(False);
  finally
    Free;
  end;

end;

function TMainClass.DetectMeters(Message: string; var M: double): boolean;
begin
  with TRegExpr.Create do
  try
    Expression := '\b(\d+(\.\d+)?)\s*(m|M|Meters|meters)\b';
    if Exec(Message) then begin
      writeln('Detected: ' + Message);
      M := StrToFloat(Match[1]);
      Exit(True);
    end;
    Exit(False);
  finally
    Free;
  end;
end;

function TMainClass.DetectFeet(Message: string; var FT: double): boolean;
begin
  with TRegExpr.Create do
  try
    Expression := '\b(\d+(\.\d+)?)\s*(ft|Ft|feet|Feet|\'')\b';
    if Exec(Message) then begin
      writeln('Detected: ' + Message);
      FT := StrToFloat(Match[1]);
      Exit(True);
    end;
    Exit(False);
  finally
    Free;
  end;
end;

function TMainClass.DetectCentimeters(Message: string; var CM: double): boolean;
begin
  with TRegExpr.Create do
  try
    Expression := '\b(\d+(\.\d+)?)\s*(cm|Cm|centimeters|Centimeters)\b';
    if Exec(Message) then begin
      writeln('Detected: ' + Message);
      CM := StrToFloat(Match[1]);
      Exit(True);
    end;
    Exit(False);
  finally
    Free;
  end;
end;

function TMainClass.DetectInches(Message: string; var INCH: double): boolean;
begin
  with TRegExpr.Create do
  try
    Expression := '\b(\d+(\.\d+)?)\s*(in|In|inch|Inch|inches|Inches|\")\b';
    if Exec(Message) then begin
      writeln('Detected: ' + Message);
      INCH := StrToFloat(Match[1]);
      Exit(True);
    end;
    Exit(False);
  finally
    Free;
  end;
end;

function TMainClass.DetectCelsius(Message: string; var C: double): boolean;
begin
  with TRegExpr.Create do
  try
    Expression := '\b(\d+(\.\d+)?)\s*(C|\xB0C|\xBAC|Celsius|celsius)\b';
    if Exec(Message) then begin
      writeln('Detected Celsius: ' + Match[0]);
      C := StrToFloat(Match[1]);
      Exit(True);
    end;
    Exit(False);
  finally
    Free;
  end;
end;

function TMainClass.DetectFahrenheit(Message: string; var F: double): boolean;
begin
  with TRegExpr.Create do
  try
    Expression := '\b(\d+(\.\d+)?)\s*(F|\xB0F|\xBAF|Fahrenheit|fahrenheit)\b';
    if Exec(Message) then begin
      writeln('Detected Fahrenheit: ' + Match[0]);
      F := StrToFloat(Match[1]);
      Exit(True);
    end;
    Exit(False);
  finally
    Free;
  end;
end;


function TMainClass.UrlEncode(const S: string): string;
const
  HexChars: array[0..15] of char = '0123456789ABCDEF';
var
  I, Value: integer;
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
