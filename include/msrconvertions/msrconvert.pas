unit msrConvert;

{$mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, FPHTTPClient, fpjson, jsonparser, RegExpr, opensslsockets;

type

  { TMesurement }

  TMesurement = class(TMainClass)
    function ConvertValue(Message: string): string;
    function LbToKg(LB: double): string;
    function KgToLb(KG: double): string;
    function OzToG(OZ: double): string;
    function GToOz(G: double): string;
    function LToGal(L: double): string;
    function GalToL(GAL: double): string;
    function MlToFloz(ML: double): string;
    function FlozToMl(FLOZ: double): string;
    function KmToMi(KM: double): string;
    function MiToKm(MI: double): string;
  end;

var
  Mesurement: TMesurement;

implementation

uses Main, Convertions;

function TMesurement.ConvertValue(Message: string): string;
var
  LB, KG, OZ, G, L, GAL, ML, FLOZ, KM, MI, M, FT, CM, INCH, C, F, MHP, HP: double;
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

    FoundValue := StrToFloat(Match[1]);
    case ExprID of
      1: begin //Pounds to kilograms
        Value      := FormatFloat('0.00', FoundValue + 'lb');
        Convertion := LbToKg(StrToFLoat(Match[1]));
      end;
      2: begin //Kilograms to pounds
        Value      := FormatFloat('0.00', FoundValue + 'Kg');
        Convertion := KgToLb(StrToFloat(Match[1]));
      end;
      3: begin //Ounces to grams
        Value      := FormatFloat('0.00', FoundValue + 'Oz');
        Convertion := OzToG(StrToFloat(Match[1]));
      end;
      4: begin //Grams to ounces
        Value      := FormatFloat('0.00', FoundValue + 'g');
        Convertion := GToOz(StrToFloat(Match[1]));
      end;
      5: begin //Liters to gallons
        Value      := FormatFloat('0.00', FoundValue + 'L');
        Convertion := LToGal(StrToFloat(Match[1]));
      end;
      6: begin //Gallons to liters
        Value      := FormatFloat('0.00', FoundValue + 'gal');
        Convertion := GalToL(StrToFloat(Match[1]));
      end;
      7: begin //Mililiters to Fluid Ounces
        Value      := FormatFloat('0.00', FoundValue + 'mL');
        Convertion := MlToFloz(StrToFloat(Match[1]));
      end;
      8: begin //Fluid ounces to mililiters
        Value      := FormatFloat('0.00', FoundValue + 'mL');
        Convertion := FlozToMl(StrToFloat(Match[1]));
      end;
      9: //Kilometers to miles
      begin
        Value      := FormatFloat('0.00', FoundValue + 'Km');
        Convertion := KmToMi(StrToFloat(Match[1]));
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
    Result := Value + ' is the same as ' + Convertion;
  finally
    Free;
  end;
end;

end.
