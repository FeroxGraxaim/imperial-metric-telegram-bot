unit msrConvert;

{$mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, FPHTTPClient, fpjson, jsonparser, RegExpr, opensslsockets,
  Main, tgtypes, tgsendertypes, Math;

type

  { TMesurement }

  TMesurement = class(TMainClass)
    procedure DetectImperialMetric({%H-}ASender: TObject;
    {%H-}AMessage: TTelegramMessageObj);
    function ConvertValue(Message: string): string;
  end;

var
  Mesurement: TMesurement;

implementation

uses Convertions, BotMsgs;

procedure TMesurement.DetectImperialMetric({%H-}ASender: TObject;
 {%H-}AMessage: TTelegramMessageObj);
var
  Msg: TTelegramMessageObj;
  MsgText, Reptext, Response: string;
  RepMsgID:   integer;
begin
  MsgText  := AMessage.Text;
  Response := ConvertValue(MsgText);
  RepMsgID := AMessage.MessageId;

  if Response <> 'null' then
    Bot.sendMessage(Response, pmDefault, False, nil, RepMsgID, true);
end;

function TMesurement.ConvertValue(Message: string): string;
var
  FoundValue: double;
  Value, Convertion: string;
  ExprID, i:  integer;
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
    if Match[1] <> '' then
    FoundValue := StrToFloat(Match[1])
    else
    Exit('null');
    case ExprID of
      1: begin //Pounds to kilograms
        Value      := FloatToStr(RoundTo(FoundValue, -2)) + 'lb';
        Convertion := LbToKg(FoundValue);
      end;
      2: begin //Kilograms to pounds
        Value      := FloatToStr(RoundTo(FoundValue, -2)) + 'Kg';
        Convertion := KgToLb(FoundValue);
      end;
      3: begin //Ounces to grams
        Value      := FloatToStr(RoundTo(FoundValue, -2)) + 'Oz';
        Convertion := OzToG(FoundValue);
      end;
      4: begin //Grams to ounces
        Value      := FloatToStr(RoundTo(FoundValue, -2)) + 'g';
        Convertion := GToOz(FoundValue);
      end;
      5: begin //Liters to gallons
        Value      := FloatToStr(RoundTo(FoundValue, -2)) + 'L';
        Convertion := LToGal(FoundValue);
      end;
      6: begin //Gallons to liters
        Value      := FloatToStr(RoundTo(FoundValue, -2)) + 'gal';
        Convertion := GalToL(FoundValue);
      end;
      7: begin //Mililiters to Fluid Ounces
        Value      := FloatToStr(RoundTo(FoundValue, -2)) + 'mL';
        Convertion := MlToFloz(FoundValue);
      end;
      8: begin //Fluid ounces to mililiters
        Value      := FloatToStr(RoundTo(FoundValue, -2)) + 'mL';
        Convertion := FlozToMl(FoundValue);
      end;
      9: begin //Kilometers to miles
        Value      := FloatToStr(RoundTo(FoundValue, -2)) + 'Km';
        Convertion := KmToMi(FoundValue);
      end;
      10: //Miles to kilometers
      begin
        Value      := FloatToStr(RoundTo(FoundValue, -2)) + 'mi';
        Convertion := MiToKm(FoundValue);
      end;
      11: begin //Meters to feet
        Value      := FloatToStr(RoundTo(FoundValue, -2)) + 'm';
        Convertion := MToFt(FoundValue);
      end;
      12: begin //Feet to meters
        Value      := FloatToStr(RoundTo(FoundValue, -2)) + 'ft';
        Convertion := FtToM(FoundValue);
      end;
      13: begin //Centimeters to inches
        Value      := FloatToStr(RoundTo(FoundValue, -2)) + 'cm';
        Convertion := CmToIn(FoundValue);
      end;
      14: begin //Inches to centimeters
        Value      := FloatToStr(RoundTo(FoundValue, -2)) + 'in';
        Convertion := InToCm(FoundValue);
      end;
      15: begin //Celsius to fahrenheit
        Value      := FloatToStr(RoundTo(FoundValue, -2)) + '°C';
        Convertion := CToF(FoundValue);
      end;
      16: begin //Fahrenheit to celsius
        Value      := FloatToStr(RoundTo(FoundValue, -2)) + '°F';
        Convertion := FToC(FoundValue);
      end;
      17: begin //Metric HorsePower to Imperial HorsePower
        Value      := FloatToStr(RoundTo(FoundValue, -2)) + 'mhp';
        Convertion := MhpToHp(FoundValue);
      end;
      18: begin //Imperial HorsePower to Metric HorsePower
        Value      := FloatToStr(RoundTo(FoundValue, -2)) + 'hp';
        Convertion := HpToMhp(FoundValue);
      end;
      else
        Result := 'null';
    end;
    Free;
  except
    writeln('Some problem making the function ConvertValue detect non-mesurement ' +
    'messages.');
    Free;
    Exit('null');
  end;
  Result := Format(RESULT_MSG, [Value, Convertion]);
end;

end.
