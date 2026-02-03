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
    function ConvertValueStr(Message: string; IsUK: boolean): string;
    function ReplaceUnicodeFractions(Message: string): string;
    function UTF8CHar(const Bytes: array of byte): string;
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
  Response := ConvertValueStr(MsgText, False);
  RepMsgID := AMessage.MessageId;

  if Response <> 'null' then
    Bot.sendMessage(Response, pmDefault, False, nil, RepMsgID, True);
end;

function TMesurement.ConvertValueStr(Message: string; IsUK: boolean): string;
var
  ValueFloat, SecondFloat, FinalFloat: double;
  ValueStr, SecondStr, Convertion, SecondConvertion, TotalFtIn, MCM: string;
  ExprID, i: integer;
  ResultList: TStringList;
  Reg, Reg2: TRegExpr;
  S: string;
const
  SAME    = ' (Same as USA)';
  NO_NEED = #10 + #10 +
    'From metric to imperial I already convert to both! No command needed.' +
    ' But anyway... Here it is!';
  Patterns: array[1..27] of string = (
  // 1 Compound feet and inches
    '(?:^|\s)(?:(\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?)''(\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?))#',

  // 2 Pounds
    '(?:^|\s)((?:\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?))\s*(lb|lbs|pounds|pound)\b',

  // 3 Kilograms
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(kg|kilo|kilogram|kilograms)\b',

  // 4 Ounces
    '(?:^|\s)((?:\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?))\s*(oz|ounce|ounces)\b',

  // 5 Grams
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(g|gram|grams)\b',

  // 6 Liters
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(l|liter|liters)\b',

  // 7 Gallons
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(gal|gallon|gallons)\b',

  // 8 Milliliters
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(ml|mililiter|mililiters)\b',

  // 9 Fluid ounces
    '(?:^|\s)((?:\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?))\s*(fl|floz|fl oz|oz fl)\b',

  // 10 Kilometers
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(km|kilometer|kilometers|km/h|kmph)\b',

  // 11 Miles
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(mi|miles|mile|mi/h|mph)\b',

  // 12 Meters
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(m|meter|meters|m/s)\b',

  // 13 Feet                                                                          {\$27}
    '(?:^|\s)((?:\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?))(ft|foot|feet|'')',

  // 14 Centimeters
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(cm|centimeter|centimeters)\b',

  // 15 Inches
    //'(?:^|\s)((?:\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?))(in|inch|inches|#)\b',
    '\b(\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?)(in|inch|inches|#)',
  // 16 Celsius
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(C|\xB0C|\xBAC|celsius)\b',

  // 17 Fahrenheit
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(F|\xB0F|\xBAF|fahrenheit)\b',

  // 18 Metric HorsePower
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(mhp|cv|ps)\b',

  // 19 Imperial HorsePower
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(hp|Hp|HP)\b',

  {// 20 Feet and inches
    '',

  // 21 Inches legacy
    'changed to 15',}

  // 20 Milimiters
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(mm|milimiters|milimiter)\b',

  // 21 Yards
    '(?:^|\s)((?:\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?))(yd|yard|yards)',

  // 22 Leagues
    '(?:^|\s)((?:\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?))(lg|lea|league|leagues)',

  // 23 Fathoms
    '(?:^|\s)((?:\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?))(ftm|fathom|fathoms)',

  // 24 Furlongs
    '(?:^|\s)((?:\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?))(furl|furlong|furlongs)',

  // 25 Pints
    '(?:^|\s)((?:\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?))\s*(pt|pint|pints)\b',

  // 26 Tea spoons
    '(?:^|\s)((?:\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?))\s*(tsp|tea spoon|tea spoons)\b',

  //27  Table spoons
    '(?:^|\s)((?:\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?))\s*(tbsp|table spoon|table spoons)\b'
    );
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
              writeln('Detected value: ', ValueStr, ' on ID: ', i);
              begin
                try
                  //WriteLn('Starting do tetect fraction...');
                  Reg2 := TRegExpr.Create;
                  Reg2.ModifierI := True;
                  Reg2.Expression :=
                    '^(?:(\d+)\s+(1/8|1/4|3/8|1/2|5/8|3/4|7/8)|(1/8|1/4|3/8|1/2|5/8|3/4|7/8))$';
                  if Reg2.Exec(ValueStr) then
                  begin
                    //WriteLn('Fraction detected!');
                    if Reg2.Match[1] <> '' then
                      ValueFloat :=
                        StrToFloat(Reg2.Match[1]) + DetectFraction(Reg2.Match[2])
                    else
                    begin
                      //WriteLn('Fraction not detected!');
                      ValueFloat := DetectFraction(ValueStr);
                    end;
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
              //WriteLn('Finished to detect fraction!');
              ValueFloat := RoundTo(ValueFloat, -2);
              case ExprID of
                1: begin //Feet and inches to meters
                  Reg2 := TRegExpr.Create;
                  writeln('feet and inches');
                  WriteLn('Value Float: ', ValueFloat);
                  SecondStr   := StringReplace(Match[2], ',', '.', [rfReplaceAll]);
                  SecondFloat := StrToFLoat(SecondStr);
                  WriteLn('Second Float: ', SecondFloat);
                  if ValueFloat >= 3.28 then begin
                    Convertion := FtToM(ValueFloat);
                    Delete(Convertion, Length(Convertion), 1);
                    SecondFloat := SecondFloat / 100;
                    MCM := 'm';
                  end
                  else begin
                    Convertion := FtToCm(ValueFloat);
                    Delete(Convertion, Length(Convertion) - 1, 2);
                    MCM := 'cm';
                  end;
                  SecondConvertion := InToCm(SecondFloat);
                  ValueStr := ValueStr + '''' + SecondStr + '"';
                  if IsUK then
                    ValueStr := ValueStr + SAME;
                  Delete(SecondConvertion, Length(SecondConvertion) - 1, 2);
                  FinalFloat := StrToFloat(Convertion) + StrToFloat(SecondConvertion);
                  WriteLn('Final float: ', FinalFloat);
                  Convertion := FloatToStr(FinalFloat) + MCM;
                  Message    :=
                    StringReplace(Message, Match[0], '§§§', [rfReplaceAll]);
                end;
                2: begin //Pounds to kilograms
                  ValueStr   := ValueStr + 'lb';
                  ValueStr   := ValueStr + SAME;
                  Convertion := LbToKg(ValueFloat);
                end;
                3: begin //Kilograms to pounds
                  ValueStr   := ValueStr + 'Kg';
                  Convertion := KgToLb(ValueFloat);
                  if IsUK then
                    Convertion := Convertion + SAME;
                end;
                4: begin //Ounces to grams
                  ValueStr := ValueStr + 'oz';
                  if IsUK then begin
                    ValueStr := ValueStr + SAME;
                    Message  :=
                      StringReplace(Message, Match[0], '§§§', [rfReplaceAll]);
                  end;
                  Convertion := OzToG(ValueFloat);
                end;
                5: begin //Grams to ounces
                  ValueStr   := ValueStr + 'g';
                  Convertion := GToOz(ValueFloat);
                  if IsUK then
                    Convertion := Convertion + SAME;
                end;
                6: begin //Liters to gallons

                  ValueStr := ValueStr + 'L';
                  if IsUK then
                    Convertion := LToGal(ValueFloat) + NO_NEED
                  else
                    Convertion := LToGal(ValueFloat);
                end;
                7: begin //Gallons to liters
                  ValueStr := ValueStr + 'gal';
                  if IsUK then
                    Convertion := UKGalToL(ValueFloat)
                  else
                    Convertion := GalToL(ValueFloat);
                end;
                8: begin //Mililiters to Fluid Ounces
                  ValueStr := ValueStr + 'mL';
                  if IsUK then
                    Convertion := MlToFloz(ValueFloat) + NO_NEED
                  else
                    Convertion := MlToFloz(ValueFloat);
                end;
                9: begin //Fluid ounces to mililiters
                  ValueStr := ValueStr + 'fl oz';
                  if IsUK then
                    Convertion := UKFlozToMl(ValueFloat)
                  else
                    Convertion := FlozToMl(ValueFloat);
                end;
                10: begin //Kilometers to miles
                  ValueStr   := ValueStr + 'Km';
                  Convertion := KmToMi(ValueFloat);
                  if IsUK then
                    Convertion := Convertion + SAME;
                end;
                11: //Miles to kilometers
                begin
                  ValueStr := ValueStr + 'mi';
                  if IsUK then
                    ValueStr := ValueStr + SAME;
                  Convertion := MiToKm(ValueFloat);
                end;
                12: begin //Meters to feet
                  ValueStr   := ValueStr + 'm';
                  Convertion := MToFt(ValueFloat);
                  if IsUK then
                    Convertion := Convertion + SAME;
                end;
                13: begin //Feet to meters
                  ValueStr := ValueStr + '''';
                  if IsUK then
                    ValueStr := ValueStr + SAME;
                  if ValueFloat >= 3.28 then
                    Convertion := FtToM(ValueFloat)
                  else
                    Convertion := FtToCm(ValueFloat);
                end;
                14: begin //Centimeters to inches
                  ValueStr   := ValueStr + 'cm';
                  Convertion := CmToIn(ValueFloat);
                  if IsUK then
                    Convertion := Convertion + SAME;
                end;
                15: begin //Inches to centimeters
                  ValueStr := ValueStr + '"';
                  if IsUK then ValueStr := ValueStr + SAME;
                  Convertion := InToCm(ValueFloat);
                end;
                16: begin //Celsius to fahrenheit
                  ValueStr   := ValueStr + '°C';
                  Convertion := CToF(ValueFloat);
                  if IsUK then
                    Convertion := Convertion + SAME;
                end;
                17: begin //Fahrenheit to celsius
                  ValueStr := ValueStr + '°F';
                  if IsUK then
                    ValueStr := ValueStr + SAME;
                  Convertion := FToC(ValueFloat);
                end;
                18: begin //Metric HorsePower to Imperial HorsePower
                  ValueStr   := ValueStr + 'mhp';
                  Convertion := MhpToHp(ValueFloat);
                  if IsUK then
                    Convertion := Convertion + SAME;
                end;
                19: begin //Imperial HorsePower to Metric HorsePower
                  ValueStr := ValueStr + 'hp';
                  if IsUK then ValueStr := ValueStr + SAME;
                  Convertion := HpToMhp(ValueFloat);
                end;
                20: begin //Milimiters to inch fractions
                  ValueStr   := ValueStr + 'mm';
                  Convertion := MmToFracIn(ValueFloat);
                  if IsUK then Convertion := Convertion + SAME;
                end;
                21: begin //yards to meters
                  ValueStr := ValueStr + 'yd';
                  if IsUK then ValueStr := ValueStr + SAME;
                  ValueFloat := ValueFloat * 3;
                  if ValueFloat >= 3.28 then
                    Convertion := FtToM(ValueFloat)
                  else
                    Convertion := FtToCm(ValueFloat);
                end;
                22: begin  //leagues to kilometers
                  ValueStr := ValueStr + 'lea';
                  if IsUK then
                    ValueStr := ValueStr + SAME;
                  ValueFloat := ValueFloat * 3;
                  Convertion := MiToKm(ValueFloat);
                end;
                23: begin //Fathoms to Meters
                  ValueStr := ValueStr + 'ftm';
                  if IsUK then
                    ValueStr := ValueStr + SAME;
                  ValueFloat := ValueFloat * 6;
                  Convertion := FtToM(ValueFloat);
                end;
                24: begin //Furlongs to Meters
                  ValueStr := ValueStr + 'furl';
                  if IsUK then
                    ValueStr := ValueStr + SAME;
                  ValueFloat := ValueFloat * 660;
                  Convertion := FtToM(ValueFloat);
                end;
                25: begin //Pints to Mililiters
                  ValueStr := ValueStr + 'pt';
                  if IsUK then
                    Convertion := UKPintToMl(ValueFloat)
                  else
                    Convertion := PintToMl(ValueFloat);
                end;
                26: begin //Tea Spoons to Mililiters
                  ValueStr := ValueStr + 'tsp';
                  if IsUK then
                    Convertion := UKTspToMl(ValueFloat)
                  else
                    Convertion := TspToMl(ValueFloat);
                end;
                27: begin //Table Spoons to Mililiters
                  ValueStr := ValueStr + 'tbsp';
                  if IsUK then
                    Convertion := UKTbspToMl(ValueFloat)
                  else
                    Convertion := TbspToMl(ValueFloat);
                end;
                {else
                  Exit('null');}
              end;
              if IsUK then
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
      //WriteLn('Result');
    end;
  finally
    Reg.Free;
    ResultList.Free;
  end;
end;

function TMesurement.ReplaceUnicodeFractions(Message: string): string;
begin
  Result := Message;
  Result := StringReplace(Result, widechar($00BC), ' 1/4', [rfReplaceAll]); // ¼
  Result := StringReplace(Result, widechar($00BD), ' 1/2', [rfReplaceAll]); // ½
  Result := StringReplace(Result, widechar($00BE), ' 3/4', [rfReplaceAll]); // ¾
  Result := StringReplace(Result, widechar($215B), ' 1/8', [rfReplaceAll]); // ⅛
  Result := StringReplace(Result, widechar($215C), ' 3/8', [rfReplaceAll]); // ⅜
  Result := StringReplace(Result, widechar($215D), ' 5/8', [rfReplaceAll]); // ⅝
  Result := StringReplace(Result, widechar($215E), ' 7/8', [rfReplaceAll]); // ⅞
end;

function TMesurement.UTF8Char(const Bytes: array of byte): string;
begin
  SetString(Result, pansichar(@Bytes[0]), Length(Bytes));
end;

end.
