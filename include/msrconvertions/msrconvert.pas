unit msrConvert;

{$mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, StrUtils, fpjson, jsonparser, RegExpr, opensslsockets,
  Main, tgtypes, tgsendertypes, Math;

type

  { TMesurement }

  TMesurement = class(TMainClass)
    procedure DetectImperialMetric({%H-}ASender: TObject;
    {%H-}AMessage: TTelegramMessageObj);
    function ConvertValueStr(Message: string): string;
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
  Response := ConvertValueStr(MsgText);
  RepMsgID := AMessage.MessageId;

  if Response <> 'null' then
    Bot.sendMessage(Response, pmDefault, False, nil, RepMsgID, True);
end;

function TMesurement.ConvertValueStr(Message: string): string;
var
  ValueFloat, SecondFloat, FinalFloat: double;
  ValueStr, SecondStr, Convertion, SecondConvertion, TotalFtIn, Fraction: string;
  ExprID, i, ValueInt, SecondInt: integer;
  ResultList: TStringList;
  Reg, Reg2: TRegExpr;
  S:      string;
  IsFull: boolean;
label
  Final;
const
  Patterns: array[1..20] of string = (
  // feet
    '(?:^|\s)((?:\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?))' +
    '(ft|foot|foots|feet|'')(?:\s|[.,;:!?)]|$)\b',

  // Pounds
    '(?:^|\s)((?:\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?))\s*' +
    '(lb|lbs|pounds|pound)\b',

  // Kilograms
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(kg|kilo|kilogram|kilograms)\b',

  // Ounces
    '(?:^|\s)((?:\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?))\s*' +
    '(oz|ounce|ounces)\b',

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

  // Feet and inches
    '(\d+)''((?:\d+\s(1/8|1/4|3/8|1/2|5/8|3/4|7/8))|(?:\d+)|' +
    '(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|(\d+)[.,](\d+))#',

  // Centimeters
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(cm|centimeter|centimeters)\b',

  // Inches (deprecated)
    {'(?:^|\s)((?:\d+\s?(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|\d+(?:[.,]\d+)?))' +
    '(in|inch|inches|#)(?:\s|[.,;:!?)]|$)', }

  // Celsius
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(C|\xB0C|\xBAC|celsius)\b',

  // Fahrenheit
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(F|\xB0F|\xBAF|fahrenheit)\b',

  // Metric HorsePower
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(mhp|cv|ps)\b',

  // Imperial HorsePower
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(hp|Hp|HP)\b',

  // Inches
    '\b(?:\s|[.,;:!?)]|$)((?:\d+\s(1/8|1/4|3/8|1/2|5/8|3/4|7/8))|' +
    '(?:1/8|1/4|3/8|1/2|5/8|3/4|7/8)|(?:\d+(?:[.,]\d+)?))(in|inch|inches|#)' +
    '(?:\s|[.,;:!?)]|$)\b',

  // Milimiters
    '(?:^|\s)((?:\d+(?:[.,]\d+)?))\s*(mm|milimiters|milimiter)\b');
begin
  Convertion := '';
  {for i := 1 to Length(Message) do
    WriteLn(IntToHex(Ord(Message[i]), 4));}
  Message    := ReplaceUnicodeFractions(Message);
  //WriteLn(Message);
  Result := 'null';
  ExprID := 0;
  ResultList := TStringList.Create;
  Reg := TRegExpr.Create;
  try
    with Reg do
    begin
      ModifierI := True;
      Message   := StringReplace(Message, widechar($2018), '''', [rfReplaceAll]);
      Message   := StringReplace(Message, widechar($2019), '''', [rfReplaceAll]);
      Message   := StringReplace(Message, '"', '#', [rfReplaceAll]);
      Message   := StringReplace(Message, widechar($201C), '#', [rfReplaceAll]);
      Message   := StringReplace(Message, widechar($201D), '#', [rfReplaceAll]);
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
              writeln('Detected value: ', ValueStr, ' in expression: ', ExprID);
              begin
                try
                  Reg2 := TRegExpr.Create;
                  Reg2.ModifierI := True;
                  Reg2.Expression :=
                    '(?:(\d+)\s+(1/8|1/4|3/8|1/2|5/8|3/4|7/8)|(1/8|1/4|3/8|1/2|5/8|3/4|7/8))';
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

              //ValueFloat := RoundTo(ValueFloat, -2);
              case ExprID of
                1: begin //feet to meters
                  ValueStr   := ValueStr + '''';
                  Convertion := FtToM(ValueFloat);
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
                13: begin //Feet and inches to meters or centimeters
                  Reg2 := TRegExpr.Create;
                  writeln('feet and inches');
                  //WriteLn('Value Float: ', ValueFloat);
                  SecondStr := StringReplace(Match[2], ',', '.', [rfReplaceAll]);
                  if not TryStrToFloat(SecondStr, SecondFloat) then
                    SecondFloat := DetectFraction(SecondStr);
                  Fraction      := DecimalToFraction(SecondFloat, IsFull);
                  if IsFull then
                    SecondInt := Round(SecondFloat)
                  else
                    SecondInt := Trunc(SecondFloat);
                  if SecondInt <> 0 then
                    SecondStr := Format('%d%s', [SecondInt, Fraction])
                  else
                    SecondStr := Fraction;
                  SecondFloat := SecondFloat / 100;
                  //WriteLn('Second Float: ', SecondFloat);
                  Convertion := FtToM(ValueFloat);
                  //WriteLn('Convertion feet: ', Convertion);
                  SecondConvertion := InToCm(SecondFloat);
                 //WriteLn('Convertion Inches: ', SecondConvertion);
                  ValueStr   := ValueStr + '''' + SecondStr + '"';
                  Delete(Convertion, Length(Convertion), 1);
                  Delete(SecondConvertion, Length(SecondConvertion) - 1, 2);
                  FinalFloat := StrToFloat(Convertion) + StrToFloat(SecondConvertion);
                  if FinalFloat >= 1 then
                    Convertion := FloatToStr(FinalFloat) + 'm'
                  else begin
                    FinalFloat := FinalFloat * 10;
                    Convertion := FloatToStr(FinalFloat) + 'cm';
                  end;
                  writeln('Debug:', Convertion);
                  //goto Final;
                end;
                14: begin //Centimeters to inches
                  ValueStr   := ValueStr + 'cm';
                  Convertion := CmToIn(ValueFloat);
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
                19: begin //Inches to centimeters
                  ValueStr   := ValueStr + '"';
                  Convertion := InToCm(ValueFloat);
                end;
                20: begin //Milimiters to inch fractions
                  ValueStr   := ValueStr + 'mm';
                  Convertion := MmToFracIn(ValueFloat);
                end;
                else
                  if Convertion = '' then
                    Exit('null');
              end;
              Final:
                ValueStr := StringReplace(ValueStr, ' 1/4', '¼', [rfReplaceAll]);
              ValueStr   := StringReplace(ValueStr, ' 1/2', '½', [rfReplaceAll]);
              ValueStr   := StringReplace(ValueStr, ' 3/4', '¾', [rfReplaceAll]);
              ValueStr   := StringReplace(ValueStr, ' 1/8', '⅛', [rfReplaceAll]);
              ValueStr   := StringReplace(ValueStr, ' 3/8', '⅜', [rfReplaceAll]);
              ValueStr   := StringReplace(ValueStr, ' 5/8', '⅝', [rfReplaceAll]);
              ValueStr   := StringReplace(ValueStr, ' 7/8', '⅞', [rfReplaceAll]);
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
var
  ResultStr, Convertion: string;
  i, PosFound, minPos, minIndex, ConvInt: integer;
  ConvFloat: double;
  chBefore:  char;
  IsUnicode: boolean;
const
  Fractions: array[0..6] of record
      Unicode: widechar;
      Text:    string;
      Value:   string;
      end
  = (
    (Unicode: widechar($215B); Text: '1/8'; Value: '0.125'),
    (Unicode: widechar($00BC); Text: '1/4'; Value: '0.25'),
    (Unicode: widechar($215C); Text: '3/8'; Value: '0.375'),
    (Unicode: widechar($00BD); Text: '1/2'; Value: '0.5'),
    (Unicode: widechar($215D); Text: '5/8'; Value: '0.625'),
    (Unicode: widechar($00BE); Text: '3/4'; Value: '0.75'),
    (Unicode: widechar($215E); Text: '7/8'; Value: '0.875')
    );
begin
  ResultStr := Message;
  minPos    := Length(ResultStr) + 1;
  minIndex  := -1;
  for i := 0 to High(Fractions) do
  begin
    IsUnicode := ((Pos(Fractions[i].Unicode, ResultStr) > 0) and not
      (Pos(Fractions[i].Text, ResultStr) > 0));
    if IsUnicode then
      PosFound := Pos(Fractions[i].Unicode, ResultStr)
    else
      PosFound := Pos(Fractions[i].Text, ResultStr);

    if PosFound > 0 then
      if PosFound < minPos then
      begin
        minPos   := PosFound;
        minIndex := i;
      end;
  end;

  if minIndex <> -1 then
  begin
    Convertion := Fractions[minIndex].Value;
    ConvFloat  := StrToFloat(Convertion);

    if (minPos > 1) then
      chBefore := ResultStr[minPos - 1]
    else
      chBefore := #0;

    if Pos(Fractions[minIndex].Unicode, ResultStr) > 0 then
    begin
      if (chBefore in ['0'..'9']) then
      begin
        ConvInt   := Trunc(ConvFloat * 1000);
        ResultStr := StringReplace(ResultStr, Fractions[minIndex].Unicode,
          '.' + IntToStr(ConvInt), []);
      end
      else
        ResultStr := StringReplace(ResultStr, Fractions[minIndex].Unicode,
          Convertion, []);
    end
    else begin
      if chBefore = ' ' then
      begin
        ConvInt   := Trunc(ConvFloat * 1000);
        ResultStr := StringReplace(ResultStr, ' ' + Fractions[minIndex].Text,
          '.' + IntToStr(ConvInt), []);
      end
      else
        ResultStr := StringReplace(ResultStr, Fractions[minIndex].Text,
          Convertion, []);
    end;
  end
  else
    Exit(Message);

  Result := ResultStr;
end;

function TMesurement.UTF8Char(const Bytes: array of byte): string;
begin
  SetString(Result, pansichar(@Bytes[0]), Length(Bytes));
end;

end.
