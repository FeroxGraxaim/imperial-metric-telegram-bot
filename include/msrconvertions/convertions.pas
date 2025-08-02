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
function GToDr(G: double): string;
function GToGr(G: double): string;
function LToGal(L: double): string;
function GalToL(GAL: double): string;
function MlToFloz(ML: double): string;
function MlToTsp(ML: double): string;
function FlozToMl(FLOZ: double): string;
function MlToPint(ML: double): string;
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

function FtToIn(FT: double): string;
function InToFT(INCH: double): string;

function DetectMLToImperial(ML: double): string;
function DecimalToPreciseFraction(Value: double; out IsPreciseFull: boolean): string;
function DecimalToFraction(Value: double; out IsNormalFull: boolean): string;
function FractionSymbol(Numerator: integer): string;
function DetectFraction(Fraction: string): double;
function PreciseFractionSymbol(Numerator: integer): string;
function DetectPreciseFraction(Fraction: string): integer;

implementation

uses
  msrConvert;

var
  IsFull: boolean;

function LbToKg(LB: double): string;
var
  KG: double;
begin
  KG := LB * 0.453592;
  if Kg < 1 then
  begin
    KG := KG * 1000;
    Exit(Format('%dg', [Round(KG)]));
  end;
  Result := FloatToStr(RoundTo(KG, -2)) + 'Kg';
end;

function KgToLb(KG: double): string;
var
  LB, G: double;
  IntLB, IntOZ: integer;
  OzStr: string;
begin
  LB := KG * 2.20462;
  if LB < 1 then
    Exit(GToOz(KG));
  IntLB := Trunc(LB);
  G     := LB - (IntLB / 2.20462);
  if G <> 0 then
    OzStr := GToOz(G)
  else
    OzStr := '';
  Result := Format('%dlb %s', [IntLB, OzStr]);
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
  OZ:     double;
  OzStr, Fraction: string;
  IntOZ:  integer;
  IsFull: boolean;
begin
  OZ := G / 28.35;
  if OZ >= 16 then
  begin
    G := G / 1000;
    Exit(KgToLb(G));
  end;
  if OZ < (1 / 16) then
    Exit(GToDr(G));
  IntOZ := Trunc(OZ);
  if OZ = (1 / 16) then
    Fraction := '¹⁄₁₆'
  else
    Fraction := DecimalToFraction(OZ, IsFull);
  if IsFull then
    IntOZ := Round(OZ);
  if IntOZ = 0 then
    Exit(Fraction + 'oz');
  Result := Format('%d%soz', [IntOZ, Fraction]);
end;

function GToDr(G: double): string;
var
  DR:    double;
  IntDR: integer;
  Fraction: string;
begin
  DR := G * 1.7718451953125;
  Fraction := DecimalToPreciseFraction(DR, IsFull);
  if IsFull then
    IntDR := Round(DR)
  else
    IntDR := Trunc(DR);

  if DR < (1 / 16) then
    Exit(GToGr(G));

  //WriteLn('Decimal drain: ', DR);
  Result := Format('%d%sdr', [IntDR, Fraction]);
end;

function GToGr(G: double): string;
var
  GR: double;
  IntGR, Numerator: integer;
  Fraction: string;
begin
  GR := (G * 1000) / 64.79891;
  Fraction := DecimalToPreciseFraction(GR, IsFull);
  if IsFull then
    IntGR := Round(GR)
  else
    IntGR := Trunc(GR);

  {if GR < (1 / 16) then
    Exit('less than ¹⁄₁₆gr');}

  if IntGR = 0 then
    Exit(Fraction + 'gr');
  Result := Format('%d%sgr', [IntGR, Fraction]);
end;

function LToGal(L: double): string;
var
  GAL, ML, ExcML: double;
  StrPint: string;
  IntGAL:  integer;
begin
  GAL := L / 3.785;
  ML  := L * 1000;
  if GAL < 1 then
    Exit(MlToPint(ML));
  ExcML  := Frac(GAL) * 3785.41;
  IntGAL := Trunc(GAL);
  if ExcML <> 0.00 then
    StrPint := MlToPint(ExcML)
  else
    StrPint := '';
  Result := Format('%dgal %s', [IntGAL, StrPint]);
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
  FLOZ:     double;
  Fraction: string;
  IntFLOZ:  integer;
  IsFull:   boolean;
begin
  FLOZ     := ML / 29.5735;
  Fraction := DecimalToFraction(FLOZ, IsFull);
  if IsFull then
    IntFLOZ := Round(FLOZ)
  else
    IntFLOZ := Trunc(FLOZ);
  WriteLn(IntFLOZ);
  if FLOZ < 1 then
    Exit(MlToTsp(ML));
  if IntFLOZ > 15 then
    Exit(MlToPint(ML));
  Result := Format('%d%sfl oz', [IntFLOZ, Fraction]);
end;

function MlToTsp(ML: double): string;
var
  TSP:      double;
  IntTSP:   integer;
  Fraction: string;
begin
  TSP      := ML / 4.92892;
  Fraction := DecimalToFraction(TSP, IsFull);
  if IsFull then
    IntTSP := Round(TSP)
  else
    IntTSP := Trunc(TSP);
  if (IntTSP = 0) and not (Fraction = '') then
    Exit(Fraction + 'tsp');
  if (IntTSP = 0) and (Fraction = '') then
    Exit('');
  Result := Format('%d%stsp', [IntTSP, Fraction]);
end;

function FlozToMl(FLOZ: double): string;
var
  ML: double;
begin
  ML     := FLOZ * 29.5735;
  Result := FloatToStr(RoundTo(ML, -2)) + 'mL';
end;

function MlToPint(ML: double): string;
var
  PT:    double;
  IntPT: integer;
  Fraction: string;
begin
  PT := ML * 0.00211337642;
  Fraction := DecimalToPreciseFraction(PT, IsFull);
  if IsFull then
    IntPT := Round(PT)
  else
    IntPT := Trunc(PT);
  Result := Format('%d%spt', [IntPT, Fraction]);
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
  FT, FTFrac, INCH: double;
  FTInt, INCHInt: integer;
  Fraction: string;
begin
  FT     := M * 3.280839895013123;
  FTInt  := Trunc(FT);
  FTFrac := FT - FTInt;
  INCH   := FTFrac * 12;
  Fraction := DecimalToFraction(INCH, IsFull);
  if IsFull then
    INCHInt := Round(INCH)
  else
    INCHInt := Trunc(INCH);

  if (INCHInt = 0) and (Fraction = '') then
    Exit(Format('%d''', [FTInt]));
  if (INCHInt = 0) and (Fraction <> '') then
    Exit(Format('%d''%s"', [FTInt, Fraction]));
  Result := Format('%d''%d%s"', [FTInt, INCHInt, Fraction]);
end;

function FtToM(FT: double): string;
var
  M: double;
begin
  M      := FT * 0.3048;
  Result := FloatToStr(RoundTo(M, -2)) + 'm';
end;

function CmToIn(CM: double): string;
var
  INCH:     double;
  INCHInt:  integer;
  Fraction: string;
begin
  INCH     := CM / 2.54;
  INCHInt  := Trunc(INCH);
  Fraction := DecimalToFraction(INCH, IsFull);
  if IsFull then
    INCHInt := Round(INCH)
  else
    INCHInt := Trunc(INCH);
  if INCHInt = 0 then
    Exit(Fraction + '"');
  Result := Format('%d%s"', [INCHInt, Fraction]);
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
  IntINCH:  integer;
  Fraction: string;
begin
  INCH     := MM / 25.4;
  IntINCH  := Trunc(INCH);
  Fraction := DecimalToFraction(INCH, IsFull);
  if IsFull then
    IntINCH := Round(INCH)
  else
    IntINCH := Trunc(INCH);
  if INtINCH = 0 then
    Exit(Fraction + '"');
  Result := Format('%d%s"', [IntINCH, Fraction]);
end;

function FtToIn(FT: double): string;
var
  INCH:     double;
  IntINCH:  integer;
  Fraction: string;
begin
  INCH     := FT / 12;
  Fraction := DecimalToFraction(INCH, IsFull);
  if IsFull then
    IntINCH := Round(INCH)
  else
    IntINCH := Trunc(INCH);
  Result := Format('%d%s"', [IntINCH, Fraction]);
end;

function InToFT(INCH: double): string;
var
  FT, AddINCH: double;
  IntFT, IntINCH: integer;
  Fraction: string;
begin
  FT      := INCH * 12;
  IntFT   := Trunc(FT);
  AddINCH := (FT - IntFT) * 12;
  Fraction := DecimalToFraction(AddINCH, IsFull);
  if IsFull then
    IntINCH := Round(AddINCH)
  else
    IntINCH := Trunc(AddINCH);
  if (IntINCH = 0) and (Fraction <> '') then
    Exit(Format('%d''%s"', [IntFT, Fraction]));
  if (IntINCH = 0) and (Fraction = '') then
    Exit(IntToStr(IntFT) + '''');
  Result := Format('%d''%d%s"', [IntFT, IntINCH, Fraction]);
end;

function DetectMLToImperial(ML: double): string;
var
  L: double;
begin
  L := ML / 1000;
  if L >= 3.78541 then
    Exit(LToGal(L));
  if (L < 3.78541) and (ML >= 473.176) then
    Exit(MLToPint(ML));
  if (ML < 473.176) and (ML >= 4.92892) then
    Exit(MlToFloz(ML));

  Result := MlToTsp(ML);
end;

function DecimalToPreciseFraction(Value: double; out IsPreciseFull: boolean): string;
var
  IntValue, Numerator: integer;
begin
  IntValue := Trunc(Value);
  Numerator := Round((Value - IntValue) * 16);
  IsPreciseFull := (Numerator = 16);
  Result := PreciseFractionSymbol(Numerator);
end;

function DecimalToFraction(Value: double; out IsNormalFull: boolean): string;
var
  IntValue, Numerator: integer;
begin
  IsFull    := False;
  IntValue  := Trunc(Value);
  Numerator := Round((Value - IntValue) * 8);
  IsNormalFull := (Numerator = 8);
  Result    := FractionSymbol(Numerator);
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
