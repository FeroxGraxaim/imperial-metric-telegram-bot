unit msrConvert;

{$mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, RegExpr, opensslsockets,
  Main, tgtypes, tgsendertypes, Math;

type

  { TMesurement }

  TMesurement = class(TMainClass)
    procedure DetectImperialMetric({%H-}ASender: TObject;
    {%H-}AMessage: TTelegramMessageObj);
    function ConvertValueStr(Message: string): string;
  end;

var
  Mesurement: TMesurement;

implementation

uses Convertions, BotMsgs;

procedure TMesurement.DetectImperialMetric({%H-}ASender: TObject;
 {%H-}AMessage: TTelegramMessageObj);
var
  Msg:      TTelegramMessageObj;
  MsgText, Reptext, Response: string;
  RepMsgID: integer;
begin
  MsgText  := AMessage.Text;
  Response := ConvertValueStr(MsgText);
  RepMsgID := AMessage.MessageId;

  if Response <> 'null' then
    Bot.sendMessage(Response, pmDefault, False, nil, RepMsgID, True);
end;

function TMesurement.ConvertValueStr(Message: string): string;
var
  ValueFloat: double;
  ValueStr, Convertion: string;
  ExprID, i:  integer;
const
  Patterns: array[1..18] of string = (
    '(?:^|\s)(\d+([.,]\d+)?)\s*(lb|lbs|pounds|pound)\b',               // Pounds
    '(?:^|\s)(\d+([.,]\d+)?)\s*(kg|kilo|kilogram|kilograms)\b',        // Kilograms
    '(?:^|\s)(\d+([.,]\d+)?)\s*(oz|ounce|ounces)\b',                   // Ounces
    '(?:^|\s)(\d+([.,]\d+)?)\s*(g|gram|grams)\b',                      // Grams
    '(?:^|\s)(\d+([.,]\d+)?)\s*(l|liter|liters)\b',                    // Liters
    '(?:^|\s)(\d+([.,]\d+)?)\s*(gal|gallon|gallons)\b',                // Gallons
    '(?:^|\s)(\d+([.,]\d+)?)\s*(ml|mililiter|mililiters)\b',           // Milliliters
    '(?:^|\s)(\d+([.,]\d+)?)\s*(fl|floz|fl oz|oz fl)\b',               // Fluid ounces
    '(?:^|\s)(\d+([.,]\d+)?)\s*(km|kilometer|kilometers|km/h|kmph)\b', // Kilometers
    '(?:^|\s)(\d+([.,]\d+)?)\s*(mi|miles|mile|mi/h|mph)\b',            // Miles
    '(?:^|\s)(\d+([.,]\d+)?)\s*(m|meter|meters|m/s)\b',                // Meters
    '(?:^|\s)(\d+([.,]\d+)?)\s*(ft|foot|feet)\b',                      // Feet
    '(?:^|\s)(\d+([.,]\d+)?)\s*(cm|centimeter|centimeters)\b',         // Centimeters
    '(?:^|\s)(\d+([.,]\d+)?)\s*(in|inch|inches)\b',                    // Inches
    '(?:^|\s)(\d+([.,]\d+)?)\s*(C|\xB0C|\xBAC|celsius)\b',             // Celsius
    '(?:^|\s)(\d+([.,]\d+)?)\s*(F|\xB0F|\xBAF|fahrenheit)\b',          // Fahrenheit
    '(?:^|\s)(\d+([.,]\d+)?)\s*(mhp|cv|ps)\b',                         // Metric HorsePower
    '(?:^|\s)(\d+([.,]\d+)?)\s*(hp|Hp|HP)\b');                         // Imperial HorsePower

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
    if Match[1] = '' then
      Exit('null')
    else
    begin
      ValueStr   := StringReplace(Match[1], ',', '.', [rfReplaceAll]);
      ValueFloat := StrToFloat(ValueStr);
      ValueFloat := RoundTo(ValueFloat, -2);
      ValueStr   := FloatToStr(ValueFloat);
      case ExprID of
        1: begin //Pounds to kilograms
          ValueStr   := ValueStr + 'lb';
          Convertion := LbToKg(ValueFloat);
        end;
        2: begin //Kilograms to pounds
          ValueStr   := ValueStr + 'Kg';
          Convertion := KgToLb(ValueFloat);
        end;
        3: begin //Ounces to grams
          ValueStr   := ValueStr + 'Oz';
          Convertion := OzToG(ValueFloat);
        end;
        4: begin //Grams to ounces
          ValueStr   := ValueStr + 'g';
          Convertion := GToOz(ValueFloat);
        end;
        5: begin //Liters to gallons
          ValueStr   := ValueStr + 'L';
          Convertion := LToGal(ValueFloat);
        end;
        6: begin //Gallons to liters
          ValueStr   := ValueStr + 'gal';
          Convertion := GalToL(ValueFloat);
        end;
        7: begin //Mililiters to Fluid Ounces
          ValueStr   := ValueStr + 'mL';
          Convertion := MlToFloz(ValueFloat);
        end;
        8: begin //Fluid ounces to mililiters
          ValueStr   := ValueStr + 'mL';
          Convertion := FlozToMl(ValueFloat);
        end;
        9: begin //Kilometers to miles
          ValueStr   := ValueStr + 'Km';
          Convertion := KmToMi(ValueFloat);
        end;
        10: //Miles to kilometers
        begin
          ValueStr   := ValueStr + 'mi';
          Convertion := MiToKm(ValueFloat);
        end;
        11: begin //Meters to feet
          ValueStr   := ValueStr + 'm';
          Convertion := MToFt(ValueFloat);
        end;
        12: begin //Feet to meters
          ValueStr   := ValueStr + 'ft';
          Convertion := FtToM(ValueFloat);
        end;
        13: begin //Centimeters to inches
          ValueStr   := ValueStr + 'cm';
          Convertion := CmToIn(ValueFloat);
        end;
        14: begin //Inches to centimeters
          ValueStr   := ValueStr + 'in';
          Convertion := InToCm(ValueFloat);
        end;
        15: begin //Celsius to fahrenheit
          ValueStr   := ValueStr + '°C';
          Convertion := CToF(ValueFloat);
        end;
        16: begin //Fahrenheit to celsius
          ValueStr   := ValueStr + '°F';
          Convertion := FToC(ValueFloat);
        end;
        17: begin //Metric HorsePower to Imperial HorsePower
          ValueStr   := ValueStr + 'mhp';
          Convertion := MhpToHp(ValueFloat);
        end;
        18: begin //Imperial HorsePower to Metric HorsePower
          ValueStr   := ValueStr + 'hp';
          Convertion := HpToMhp(ValueFloat);
        end;
        else
          Exit('null');
      end;
    end;
  finally
    Free;
  end;
  Result := Format(RESULT_MSG, [ValueStr, Convertion]);
end;

end.
