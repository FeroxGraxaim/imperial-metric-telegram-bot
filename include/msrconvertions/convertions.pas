unit Convertions;

{$mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, Math;

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
function MToFt(M: double): string;
function FtToM(FT: double): string;
function CmToIn(CM: double): string;
function InToCm(INCH: double): string;
function CToF(C: double): string;
function FToC(F: double): string;
function MhpToHp(MHP: double): string;
function HpToMhp(HP: double): string;
function MmToFracIn(MM: double): string;

function FractionSymbol(Numerator: integer): string;
function DetectFraction(Fraction: string): double;
function PreciseFractionSymbol(Numerator: integer): string;
function DetectPreciseFraction(Fraction: string): integer;

implementation

uses
  msrConvert;

function LbToKg(LB: double): string;
var
  KG: double;
begin
  KG     := LB * 0.453592;
  if Kg < 1 then
  begin
    KG := KG * 1000;
    Exit(Format('%dg', [Trunc(KG)]));
  end;
  Result := FloatToStr(RoundTo(KG, -2)) + 'Kg';
end;

function KgToLb(KG: double): string;
var
  LB: double;
  Numerator, IntLB: integer;
  Fraction: string;
begin
  LB     := KG * 2.20462;
  IntLB := Trunc(LB);
  Numerator := Round((LB - IntLB) * 8);
  Fraction := FractionSymbol(Numerator);
  Result := Format('%d%slb', [IntLB, Fraction]);
end;

function OzToG(OZ: double): string;
var
  G: double;
begin
  G      := OZ * 28.35;
  Result := FloatToStr(RoundTo(G, -2)) + 'g';
end;

function GToOz(G: double): string;
var
  OZ: double;
  OzStr, Fraction: string;
  IntOZ, Numerator: integer;
begin
  OZ    := G / 28.35;
  IntOZ := Trunc(OZ);
  Numerator := Round((OZ - IntOZ) * 8);
  if OZ < (1 / 8) then
    if OZ >= (1 / 16) then
      Exit('⅟₁₆oz')
    else
      Exit('less than ⅟₁₆oz');
  Fraction := FractionSymbol(Numerator);
  Result   := Format('%d%soz', [IntOZ, Fraction]);
end;

function LToGal(L: double): string;
var
  GAL, ML, FLOZ:   double;
  OzStr, Fraction: string;
  IntGAL, IntFLOZ, Numerator: integer;
begin
  GAL := L / 3.785;
  if GAL < 1 then
  begin
    ML      := L * 1000;
    FLOZ    := ML / 29.5735;
    IntFLOZ := Trunc(FLOZ);
    Numerator := Round((FLOZ - IntFLOZ) * 8);
    Fraction := FractionSymbol(Numerator);
    OzStr   := Format('%d%sfl oz', [IntFLOZ, Fraction]);
    Exit(OzStr);
  end;
  IntGAL    := Trunc(GAL);
  Numerator := Round((GAL - Trunc(GAL)) * 8);
  writeln('Gallon: ', GAL);
  writeln('Numerator of gallon: ', Numerator);
  Fraction  := FractionSymbol(Numerator);
  Result    := Format('%d%sgal', [IntGAL, Fraction]);
end;

function GalToL(GAL: double): string;
var
  L: double;
begin
  L      := GAL * 3.785;
  Result := FloatToStr(RoundTo(L, -2)) + 'L';
end;

function MlToFloz(ML: double): string;
var
  FLOZ, TSP: double;
  TspStr, Fraction: string;
  IntTSP, IntFLOZ, Numerator: integer;
begin
  FLOZ := ML / 29.5735;
  if FLOZ < 0.125 then
  begin
    TSP      := ML / 4.9289;
    IntTSP   := Trunc(TSP);
    Numerator := Round((TSP - IntTSP) * 8);
    Fraction := FractionSymbol(Numerator);
    if IntTSP = 0 then
      TspStr := Fraction + 'tsp'
    else
      TspStr := Format('%d%stsp', [IntTSP, Fraction]);
    Exit(TspStr);
  end;
  IntFLOZ   := Trunc(FLOZ);
  Numerator := Round((FLOZ - IntFLOZ) * 8);
  Fraction  := FractionSymbol(Numerator);
  if IntFLOZ = 0 then
    Result := Fraction + 'fl oz'
  else
    Result := Format('%d%sfl oz', [IntFLOZ, Fraction]);
end;

function FlozToMl(FLOZ: double): string;
var
  ML: double;
begin
  ML     := FLOZ * 29.5735;
  Result := FloatToStr(RoundTo(ML, -2)) + 'mL';
end;

function KmToMi(KM: double): string;
var
  MI: double;
begin
  MI     := KM / 1.609;
  Result := FloatToStr(RoundTo(MI, -2)) + 'mi';
end;

function MiToKm(MI: double): string;
var
  KM: double;
begin
  KM     := MI * 1.609;
  Result := FloatToStr(RoundTo(KM, -2)) + 'Km';
end;

function MToFt(M: double): string;
var
  FT, FTFrac, INCH, INCHFrac: double;
  FTInt, INCHInt, INCHNum: integer;
begin
  FT      := M * 3.281;
  FTInt   := Trunc(FT);
  FTFrac  := FT - FTInt;
  INCH    := FTFrac * 12;
  INCHInt := Trunc(INCH);
  INCHFrac := INCH - INCHInt;
  INCHNum := Round(INCHFrac * 8);

  if (INCHInt > 0) and (INCHNum > 0) then
    Exit(Format('%d''%d%s"', [FTInt, INCHInt, FractionSymbol(INCHNum)]))
  else if INCHInt > 0 then
    Exit(Format('%d''%d"', [FTInt, INCHInt]))
  else if INCHNum > 0 then
    Exit(Format('%d''%s"', [FTInt, FractionSymbol(INCHNum)]))
  else
    Result := Format('%d''', [FTInt]);
end;

function FtToM(FT: double): string;
var
  M: double;
begin
  M      := FT / 3.281;
  Result := FloatToStr(RoundTo(M, -2)) + 'm';
end;

function CmToIn(CM: double): string;
var
  INCH:      double;
  inteiras:  integer;
  decimal:   double;
  numerador: integer;
begin
  INCH     := CM / 2.54;
  inteiras := Trunc(INCH);

  // Parte decimal
  decimal   := INCH - inteiras;
  numerador := Round(decimal * 8);

  // Se arredondar para 8/8, vira mais uma polegada inteira
  if numerador = 8 then
  begin
    Inc(inteiras);
    numerador := 0;
  end;

  // Compor o resultado
  if numerador = 0 then
    Result := Format('%d"', [inteiras])
  else begin
    if inteiras = 0 then
      Result := FractionSymbol(numerador) + '"'
    else
      Result := Format('%d%s"', [inteiras, FractionSymbol(numerador)]);
  end;
end;


function InToCm(INCH: double): string;
var
  CM: double;
begin
  CM     := INCH * 2.54;
  Result := FloatToStr(RoundTo(CM, -2)) + 'cm';
end;

function CToF(C: double): string;
var
  F: double;
begin
  F      := (C * 9 / 5) + 32;
  Result := FloatToStr(RoundTo(F, -2)) + '°F';
end;

function FToC(F: double): string;
var
  C: double;
begin
  C      := (F - 32) * 5 / 9;
  Result := FloatToStr(RoundTo(C, -2)) + '°C';
end;

function MhpToHp(MHP: double): string;
var
  HP: double;
begin
  HP     := MHP / 1.014;
  Result := FloatToStr(RoundTo(HP, -2)) + 'hp';
end;

function HpToMhp(HP: double): string;
var
  MHP: double;
begin
  MHP    := HP * 1.014;
  Result := FloatToStr(RoundTo(MHP, -2)) + 'mhp';
end;

function MmToFracIn(MM: double): string;
var
  INCH:     double;
  Numerator, IntINCH: integer;
  Fraction: string;
begin
  INCH      := MM / 25.4;
  IntINCH   := Trunc(INCH);
  Numerator := Round((INCH - IntINCH) * 16);
  Fraction  := PreciseFractionSymbol(Numerator);
  if IntINCH = 0 then
    if Numerator = 0 then
      Exit('0"')
    else
      Exit(Fraction + '"');
  Result := Format('%d%s"', [IntINCH, Fraction]);
end;

function FractionSymbol(Numerator: integer): string;
begin
  case Numerator of
    1: Result := '⅛';
    2: Result := '¼';
    3: Result := '⅜';
    4: Result := '½';
    5: Result := '⅝';
    6: Result := '¾';
    7: Result := '⅞';
    else
      Result := '';
  end;
end;

function DetectFraction(Fraction: string): double;
begin
  case Fraction of
    '⅛', '1/8': Exit(0.125);
    '¼', '1/4': Exit(0.25);
    '⅜', '3/8': Exit(0.375);
    '½', '1/2': Exit(0.5);
    '⅝', '5/8': Exit(0.625);
    '¾', '3/4': Exit(0.75);
    '⅞', '7/8': Exit(0.875);
    else
      Exit(0);
  end;
end;

function PreciseFractionSymbol(Numerator: integer): string;
begin
  case Numerator of
    1: Exit('¹⁄₁₆');
    2: Exit('⅛');
    3: Exit('³⁄₁₆');
    4: Exit('¼');
    5: Exit('⁵⁄₁₆');
    6: Exit('⅜');
    7: Exit('⁷⁄₁₆');
    8: Exit('½');
    9: Exit('⁹⁄₁₆');
    10: Exit('⅝');
    11: Exit('¹¹⁄₁₆');
    12: Exit('¾');
    13: Exit('¹³⁄₁₆');
    14: Exit('⅞');
    15: Exit('¹⁵⁄₁₆');
    else
      Exit('');
  end;
end;

function DetectPreciseFraction(Fraction: string): integer;
begin
   case Fraction of
    '¹⁄₁₆', '1/16': Exit(1);
    '⅛', '1/8': Exit(2);
    '³⁄₁₆', '3/16': Exit(3);
    '¼', '1/4': Exit(4);
    '⁵⁄₁₆', '5/16': Exit(5);
    '⅜', '3/8': Exit(6);
    '⁷⁄₁₆', '7/16': Exit(7);
    '½', '1/2': Exit(8);
    '⁹⁄₁₆', '9/16': Exit(9);
    '⅝', '5/8': Exit(10);
    '¹¹⁄₁₆', '11/16': Exit(11);
    '¾', '3/4': Exit(12);
    '¹³⁄₁₆', '13/16': Exit(13);
    '⅞', '7/8': Exit(14);
    '¹⁵⁄₁₆', '15/16': Exit(15);
    else
      Exit(0);
  end;
end;

end.
