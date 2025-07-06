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
    function ReplaceUnicodeFractions(Message: string): string;
    function UTF8CHar(const Bytes: array of Byte): string;
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
  ValueFloat, SecondFloat, FinalFloat: double;
  ValueStr, SecondStr, Convertion, SecondConvertion, TotalFtIn: string;
  ExprID, i: integer;
  ResultList: TStringList;
  Reg, Reg2: TRegExpr;
  S: string;
label
  Final;
const
  Patterns: array[1..22] of string = (
  // Compound feet and inches
    '(?:^|\s)(?:(\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?)''(\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?))#',

  // Pounds
    '(?:^|\s)((?:\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?))\s*(lb|lbs|pounds|pound)\b',

  // Kilograms
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(kg|kilo|kilogram|kilograms)\b',

  // Ounces
    '(?:^|\s)((?:\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?))\s*(oz|ounce|ounces)\b',

  // Grams
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(g|gram|grams)\b',

  // Liters
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(l|liter|liters)\b',

  // Gallons
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(gal|gallon|gallons)\b',

  // Milliliters
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(ml|mililiter|mililiters)\b',

  // Fluid ounces
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(fl|floz|fl oz|oz fl)\b',

  // Kilometers
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(km|kilometer|kilometers|km/h|kmph)\b',

  // Miles
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(mi|miles|mile|mi/h|mph)\b',

  // Meters
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(m|meter|meters|m/s)\b',

  // Feet
    '(?:^|\s)((?:\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?))(ft|foot|feet|\$27)\b',

  // Centimeters
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(cm|centimeter|centimeters)\b',

  // Inches
    '(?:^|\s)((?:\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?))(in|inch|inches|\$22)\b',

  // Celsius
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(C|\xB0C|\xBAC|celsius)\b',

  // Fahrenheit
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(F|\xB0F|\xBAF|fahrenheit)\b',

  // Metric HorsePower
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(mhp|cv|ps)\b',

  // Imperial HorsePower
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(hp|Hp|HP)\b',

  // Feet and inches
    '\b(?:^|\s)((?:\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?))''(\s|$|\d)\b',

  // Inches
    '\b((?:\d+\s(1/8|1/4|3/8|1/2|5/8|3/4|7/8))|(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|(?:\d+(?:[.,]\d+)?))#\b',

  // Milimiters
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(mm|milimiters|milimiter)\b');
begin
  Message := ReplaceUnicodeFractions(Message);
  Result := 'null';
  ExprID := 0;
  ResultList := TStringList.Create;
  Reg := TRegExpr.Create;
  try
    with Reg do
    begin
      ModifierI := True;
      Message   := StringReplace(Message, '"', '#', [rfReplaceAll]);
      for i := Low(Patterns) to High(Patterns) do
      begin
        Expression := Patterns[i];
        if Exec(Message) then
          repeat
            ExprID := i;
            if Match[1] = '' then
              continue
            else
            begin
              ValueStr := StringReplace(Match[1], ',', '.', [rfReplaceAll]);
              writeln('Detected value: ', ValueStr);
              begin
                try
                  Reg2 := TRegExpr.Create;
                  Reg2.ModifierI := True;
                  Reg2.Expression :=
                    '^(?:(\d+)\s+(1/8|1/4|3/8|1/2|5/8|3/4|7/8)|(1/8|1/4|3/8|1/2|5/8|3/4|7/8))$';
                  if Reg2.Exec(ValueStr) then
                  begin
                    if Reg2.Match[1] <> '' then
                      ValueFloat :=
                        StrToFloat(Reg2.Match[1]) + DetectFraction(Reg2.Match[2])
                    else
                      ValueFloat := DetectFraction(ValueStr);
                    WriteLn('Value float: ', ValueFloat);
                  end
                  else
                    ValueFloat := StrToFloat(ValueStr);
                except
                  on E: Exception do
                    raise Exception.Create('Error while detecting number and ' +
                      'fraction: ' + E.Message);
                end;
                Reg2.Free;
              end;

              ValueFloat := RoundTo(ValueFloat, -2);
              case ExprID of
                1: begin
                  Reg2 := TRegExpr.Create;
                  writeln('feet and inches');
                  //WriteLn('Value Float: ', ValueFloat);
                  SecondStr   := StringReplace(Match[2], ',', '.', [rfReplaceAll]);
                  SecondFloat := StrToFLoat(SecondStr);
                  SecondFloat := SecondFloat / 100;
                  //WriteLn('Second Float: ', SecondFloat);
                  Convertion  := FtToM(ValueFloat);
                  //WriteLn('Convertion feet: ', Convertion);
                  SecondConvertion := InToCm(SecondFloat);
                 //WriteLn('Convertion Inches: ', SecondConvertion);
                  ValueStr    := ValueStr + '''' + SecondStr + '"';
                  Delete(Convertion, Length(Convertion), 1);
                  Delete(SecondConvertion, Length(SecondConvertion) - 1, 2);
                  FinalFloat := StrToFloat(Convertion) + StrToFloat(SecondConvertion);
                  if FinalFloat >= 1 then
                    Convertion := FloatToStr(FinalFloat) + 'm'
                  else begin
                    FinalFloat := FinalFloat * 10;
                    Convertion := FloatToStr(FinalFloat) + 'cm';
                  end;
                  goto Final;
                end;
                2: begin //Pounds to kilograms
                  ValueStr   := ValueStr + 'lb';
                  Convertion := LbToKg(ValueFloat);
                end;
                3: begin //Kilograms to pounds
                  ValueStr   := ValueStr + 'Kg';
                  Convertion := KgToLb(ValueFloat);
                end;
                4: begin //Ounces to grams
                  ValueStr   := ValueStr + 'Oz';
                  Convertion := OzToG(ValueFloat);
                end;
                5: begin //Grams to ounces
                  ValueStr   := ValueStr + 'g';
                  Convertion := GToOz(ValueFloat);
                end;
                6: begin //Liters to gallons
                  ValueStr   := ValueStr + 'L';
                  Convertion := LToGal(ValueFloat);
                end;
                7: begin //Gallons to liters
                  ValueStr   := ValueStr + 'gal';
                  Convertion := GalToL(ValueFloat);
                end;
                8: begin //Mililiters to Fluid Ounces
                  ValueStr   := ValueStr + 'mL';
                  Convertion := MlToFloz(ValueFloat);
                end;
                9: begin //Fluid ounces to mililiters
                  ValueStr   := ValueStr + 'mL';
                  Convertion := FlozToMl(ValueFloat);
                end;
                10: begin //Kilometers to miles
                  ValueStr   := ValueStr + 'Km';
                  Convertion := KmToMi(ValueFloat);
                end;
                11: //Miles to kilometers
                begin
                  ValueStr   := ValueStr + 'mi';
                  Convertion := MiToKm(ValueFloat);
                end;
                12: begin //Meters to feet
                  ValueStr   := ValueStr + 'm';
                  Convertion := MToFt(ValueFloat);
                end;
                13, 20: begin //Feet to meters
                  ValueStr   := ValueStr + '''';
                  Convertion := FtToM(ValueFloat);
                end;
                14: begin //Centimeters to inches
                  ValueStr   := ValueStr + 'cm';
                  Convertion := CmToIn(ValueFloat);
                end;
                15, 21: begin //Inches to centimeters
                  ValueStr   := ValueStr + '"';
                  Convertion := InToCm(ValueFloat);
                end;
                16: begin //Celsius to fahrenheit
                  ValueStr   := ValueStr + '°C';
                  Convertion := CToF(ValueFloat);
                end;
                17: begin //Fahrenheit to celsius
                  ValueStr   := ValueStr + '°F';
                  Convertion := FToC(ValueFloat);
                end;
                18: begin //Metric HorsePower to Imperial HorsePower
                  ValueStr   := ValueStr + 'mhp';
                  Convertion := MhpToHp(ValueFloat);
                end;
                19: begin //Imperial HorsePower to Metric HorsePower
                  ValueStr   := ValueStr + 'hp';
                  Convertion := HpToMhp(ValueFloat);
                end;
                22: begin //Milimiters to inch fractions
                  ValueStr   := ValueStr + 'mm';
                  Convertion := MmToFracIn(ValueFloat);
                end;
                else
                  Exit('null');
              end;
              Final:
                ValueStr := StringReplace(ValueStr, '1/4', '¼', [rfReplaceAll]);
              ValueStr   := StringReplace(ValueStr, '1/2', '½', [rfReplaceAll]);
              ValueStr   := StringReplace(ValueStr, '3/4', '¾', [rfReplaceAll]);
              ValueStr   := StringReplace(ValueStr, '1/8', '⅛', [rfReplaceAll]);
              ValueStr   := StringReplace(ValueStr, '3/8', '⅜', [rfReplaceAll]);
              ValueStr   := StringReplace(ValueStr, '5/8', '⅝', [rfReplaceAll]);
              ValueStr   := StringReplace(ValueStr, '7/8', '⅞', [rfReplaceAll]);
              ResultList.Add(Format(RESULT_MSG, [ValueStr, Convertion]));
            end;
          until not ExecNext;
      end;
      Result := ResultList.Text;
    end;
  finally
    Reg.Free;
    ResultList.Free;
  end;
end;

function TMesurement.ReplaceUnicodeFractions(Message: string): string;
begin
  Result := Message;
  Result := StringReplace(Result, WideChar($00BC), ' 1/4', [rfReplaceAll]); // ¼
  Result := StringReplace(Result, WideChar($00BD), ' 1/2', [rfReplaceAll]); // ½
  Result := StringReplace(Result, WideChar($00BE), ' 3/4', [rfReplaceAll]); // ¾
  Result := StringReplace(Result, WideChar($215B), ' 1/8', [rfReplaceAll]); // ⅛
  Result := StringReplace(Result, WideChar($215C), ' 3/8', [rfReplaceAll]); // ⅜
  Result := StringReplace(Result, WideChar($215D), ' 5/8', [rfReplaceAll]); // ⅝
  Result := StringReplace(Result, WideChar($215E), ' 7/8', [rfReplaceAll]); // ⅞
end;

function TMesurement.UTF8Char(const Bytes: array of Byte): string;
begin
  SetString(Result, PAnsiChar(@Bytes[0]), Length(Bytes));
end;

end.
