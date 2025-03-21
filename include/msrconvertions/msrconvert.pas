unit msrConvert;

{$mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, FPHTTPClient, fpjson, jsonparser, RegExpr, opensslsockets,
  Main, tgtypes, tgsendertypes;

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

uses Convertions;

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
  begin
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
    else begin
      Result := 'null';
      Exit;
    end;
    case ExprID of
      1: begin //Pounds to kilograms
        Value      := FormatFloat('0.00', FoundValue) + 'lb';
        Convertion := LbToKg(FoundValue);
      end;
      2: begin //Kilograms to pounds
        Value      := FormatFloat('0.00', FoundValue) + 'Kg';
        Convertion := KgToLb(FoundValue);
      end;
      3: begin //Ounces to grams
        Value      := FormatFloat('0.00', FoundValue) + 'Oz';
        Convertion := OzToG(FoundValue);
      end;
      4: begin //Grams to ounces
        Value      := FormatFloat('0.00', FoundValue) + 'g';
        Convertion := GToOz(FoundValue);
      end;
      5: begin //Liters to gallons
        Value      := FormatFloat('0.00', FoundValue) + 'L';
        Convertion := LToGal(FoundValue);
      end;
      6: begin //Gallons to liters
        Value      := FormatFloat('0.00', FoundValue) + 'gal';
        Convertion := GalToL(FoundValue);
      end;
      7: begin //Mililiters to Fluid Ounces
        Value      := FormatFloat('0.00', FoundValue) + 'mL';
        Convertion := MlToFloz(FoundValue);
      end;
      8: begin //Fluid ounces to mililiters
        Value      := FormatFloat('0.00', FoundValue) + 'mL';
        Convertion := FlozToMl(FoundValue);
      end;
      9: begin //Kilometers to miles
        Value      := FormatFloat('0.00', FoundValue) + 'Km';
        Convertion := KmToMi(FoundValue);
      end;
      10: //Miles to kilometers
      begin
        Value      := FormatFloat('0.00', FoundValue) + 'mi';
        Convertion := MiToKm(FoundValue);
      end;
      11: begin //Meters to feet
        Value      := FormatFloat('0.00', FoundValue) + 'm';
        Convertion := MToFt(FoundValue);
      end;
      12: begin //Feet to meters
        Value      := FormatFloat('0.00', FoundValue) + 'ft';
        Convertion := FtToM(FoundValue);
      end;
      13: begin //Centimeters to inches
        Value      := FormatFloat('0.00', FoundValue) + 'cm';
        Convertion := CmToIn(FoundValue);
      end;
      14: begin //Inches to centimeters
        Value      := FormatFloat('0.00', FoundValue) + 'in';
        Convertion := InToCm(FoundValue);
      end;
      15: begin //Celsius to fahrenheit
        Value      := FormatFloat('0.00', FoundValue) + '°C';
        Convertion := CToF(FoundValue);
      end;
      16: begin //Fahrenheit to celsius
        Value      := FormatFloat('0.00', FoundValue) + '°F';
        Convertion := FToC(FoundValue);
      end;
      17: begin //Metric HorsePower to Imperial HorsePower
        Value      := FormatFloat('0.00', FoundValue) + 'mhp';
        Convertion := MhpToHp(FoundValue);
      end;
      18: begin //Imperial HorsePower to Metric HorsePower
        Value      := FormatFloat('0.00', FoundValue) + 'hp';
        Convertion := HpToMhp(FoundValue);
      end;
      else
        Result := 'null';
    end;
    Result := Value + ' is the same as ' + Convertion;
  except
    Result := 'null';
    writeln('Some problem making the function ConvertValue detect non-mesurement ' +
    'messages.');
  end;
  Free;
  end;
end;

end.
