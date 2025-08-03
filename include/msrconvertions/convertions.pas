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
function LToGal(L: double): string;
function GalToL(GAL: double): string;
function MlToFloz(ML: double): string;
function MlToTsp(ML: double): string;
function FlozToMl(FLOZ: double): string;
function MlToPint(ML: double; out IsFull: boolean): string;
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

function DecimalToPreciseFraction(Value: double; out IsPreciseFull: boolean): string;
function DecimalToFraction(Value: double; out IsNormalFull: boolean): string;
function FractionSymbol(Numerator: integer): string;
function DetectFraction(Fraction: string): double;
function PreciseFractionSymbol(Numerator: integer): string;
function DetectPreciseFraction(Fraction: string): integer;

function PoundToOunce(LB: double): string;
function OunceToDram(OZ: double): string;

function OunceToPound(OZ: double): string;
function DramToOunce(DR: double): string;

function GramToKg(G: double): string;
function KgToGram(KG: double): string;

function GalToPint(GAL: double): string;
function PintToGal(PT: double): string;

function FlozToPint(FLOZ: double): string;
function PintToFloz(PT: double): string;

function FlozToTsp(FLOZ: double): string;
function TspToFloz(TSP: double): string;

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
    Exit(KgToGram(KG));
  Result := Format('%.2fKg', [KG]);
end;

function KgToLb(KG: double): string;
var
  LB, G: double;
  IntLB, IntOZ: integer;
  OzStr: string;
begin
  LB := KG * 2.20462;
  if (LB < 1) and (LB <= (15 / 16)) then
    Exit(PoundToOunce(LB))
  else if (LB < 1) and (LB >= (15 / 16)) then
    IntLB := Round(LB)
  else
    IntLB := Trunc(LB);
  G := LB - (IntLB / 2.20462);
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
  Result := Format('%.2fg', [G]);
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
    Exit(OunceToPound(OZ));
  if OZ < (1 / 16) then
    Exit(OunceToDram(OZ));
  Fraction := DecimalToPreciseFraction(OZ, IsFull);
  if IsFull then
    IntOZ := Round(OZ)
  else
    IntOZ := Trunc(OZ);
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

  //WriteLn('Decimal drain: ', DR);
  if IntDR = 0 then
    Exit(Format('%sdr', [Fraction]));
  Result := Format('%d%sdr', [IntDR, Fraction]);
end;

function LToGal(L: double): string;
var
  GAL, ML, ExcML: double;
  StrPint:  string;
  IntGAL:   integer;
  FullPint: boolean;
begin
  GAL := L / 3.785;
  ML  := L * 1000;
  if GAL < 1 then
    Exit(GalToPint(GAL));
  ExcML  := Frac(GAL) * 3785.41;
  IntGAL := Trunc(GAL);
  if ExcML <> 0.00 then
  begin
    StrPint := MlToPint(ExcML, FullPint);
    if FullPint then
    begin
      IntGAL  := Round(GAL);
      StrPint := '';
    end;
  end
  else
    StrPint := '';
  Result    := Format('%dgal %s', [IntGAL, StrPint]);
end;

function GalToL(GAL: double): string;
var
  L: double;
begin
  L      := GAL * 3.785;
  Result := Format('%.2fL', [L]);
end;

function MlToFloz(ML: double): string;
var
  FLOZ:     double;
  Fraction: string;
  IntFLOZ:  integer;
  IsFull:   boolean;
begin
  FLOZ     := ML / 29.5735;
  Fraction := DecimalToPreciseFraction(FLOZ, IsFull);
  if IsFull then
    IntFLOZ := Round(FLOZ)
  else
    IntFLOZ := Trunc(FLOZ);
  WriteLn(IntFLOZ);
  if FLOZ < 1 then
    Exit(MlToTsp(ML));
  if IntFLOZ >= 16 then
    Exit(MlToPint(ML, IsFull));
  Result := Format('%d%sfl oz', [IntFLOZ, Fraction]);
end;

function MlToTsp(ML: double): string;
var
  TSP:      double;
  IntTSP:   integer;
  Fraction: string;
begin
  TSP      := ML / 4.92892;
  Fraction := DecimalToPreciseFraction(TSP, IsFull);
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

function MlToPint(ML: double; out IsFull: boolean): string;
var
  PT:    double;
  IntPT: integer;
  Fraction: string;
  FullFraction: boolean;
begin
  PT := ML * 0.00211337642;
  Fraction := DecimalToPreciseFraction(PT, FullFraction);
  if FullFraction then
    IntPT := Round(PT)
  else
    IntPT := Trunc(PT);
  if (IntPT = 0) and (Fraction = '') then
  Exit('');
  IsFull := (IntPT >= 8);
  if IntPT = 0 then
  Exit(Fraction + 'pt');
  Result := Format('%d%spt', [IntPT, Fraction]);
end;

function KmToMi(KM: double): string;
var
  MI: double;
begin
  MI     := KM / 1.609;
  Result := Format('%.2fmi', [MI]);
end;

function MiToKm(MI: double): string;
var
  KM: double;
begin
  KM     := MI * 1.609;
  Result := Format('%.2fKm', [KM]);
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
  Result := Format('%.2fm', [M]);
end;

function CmToIn(CM: double): string;
var
  INCH:     double;
  INCHInt:  integer;
  Fraction: string;
begin
  INCH     := CM / 2.54;
  INCHInt  := Trunc(INCH);
  Fraction := DecimalToPreciseFraction(INCH, IsFull);
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
  Result := Format('%.2fcm', [CM]);
end;

function CToF(C: double): string;
var
  F: double;
begin
  F      := (C * 9 / 5) + 32;
  Result := Format('%.2f°F', [F]);
end;

function FToC(F: double): string;
var
  C: double;
begin
  C      := (F - 32) * 5 / 9;
  Result := Format('%.2f°C', [C]);
end;

function MhpToHp(MHP: double): string;
var
  HP: double;
begin
  HP     := MHP / 1.014;
  Result := Format('%.2fhp', [HP]);
end;

function HpToMhp(HP: double): string;
var
  MHP: double;
begin
  MHP    := HP * 1.014;
  Result := Format('%.2fmhp', [MHP]);
end;

function MmToFracIn(MM: double): string;
var
  INCH:     double;
  IntINCH:  integer;
  Fraction: string;
begin
  INCH     := MM / 25.4;
  IntINCH  := Trunc(INCH);
  Fraction := DecimalToPreciseFraction(INCH, IsFull);
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
  Fraction := DecimalToPreciseFraction(INCH, IsFull);
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
  Fraction := DecimalToPreciseFraction(AddINCH, IsFull);
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

function PoundToOunce(LB: double): string;
var
  OZ:     double;
  IntOZ:  integer;
  Fraction: string;
  IsFull: boolean;
begin
  OZ := LB * 16;
  if OZ < (1 / 16) then
    Exit(OunceToDram(OZ));

  Fraction := DecimalToPreciseFraction(OZ, IsFull);
  if IsFull then
    IntOZ := Round(OZ)
  else
    IntOZ := Trunc(OZ);
  Result := Format('%d%soz', [IntOZ, Fraction]);
end;

function OunceToDram(OZ: double): string;
var
  DR:     double;
  IntDR:  integer;
  Fraction: string;
  IsFull: boolean;
begin
  DR := OZ * 16;
  if DR < (1 / 16) then
    Exit('Less than ⅟₁₆dr');
  Fraction := DecimalToPreciseFraction(DR, IsFull);
  if IsFull then
    IntDR := Round(DR)
  else
    IntDR := Trunc(DR);
  if IntDR = 0 then
  Exit(Fraction + 'dr');
  Result := Format('%d%sdr', [IntDR, Fraction]);
end;

function OunceToPound(OZ: double): string;
var
  LB, AddOZ: double;
  IntLB, IntOZ: integer;
  Fraction: string;
  IsFull: boolean;
begin
  LB    := OZ / 16;
  AddOZ := (LB - Trunc(LB)) * 16;
  Fraction := DecimalToPreciseFraction(AddOZ, IsFull);
  if IsFull then
    IntOZ := Round(AddOZ)
  else
    IntOZ := Trunc(AddOZ);
  if IntOZ < 16 then
    IntLB := Trunc(LB)
  else begin
    IntLB := Round(LB);
    Exit(Format('%dlb', [IntLB]));
  end;
  Result := Format('%dlb %d%soz', [IntLB, IntOZ, Fraction]);
end;

function DramToOunce(DR: double): string;
var
  OZ:     double;
  IntOZ:  integer;
  Fraction: string;
  IsFull: boolean;
begin
  OZ := DR / 16;
  Fraction := DecimalToPreciseFraction(OZ, IsFull);
  if IsFull then
    IntOZ := Round(OZ)
  else
    IntOZ := Trunc(OZ);
  Result := Format('%d%soz', [IntOZ, Fraction]);
end;

function GramToKg(G: double): string;
begin
  Result := Format('%2.fKg', [G / 1000]);
end;

function KgToGram(KG: double): string;
begin
  Result := Format('%.2fg', [KG * 1000]);
end;

function GalToPint(GAL: double): string;
var
  PT:     double;
  IntPT:  integer;
  Fraction: string;
  IsFull: boolean;
begin
  PT := GAL * 8;
  Fraction := DecimalToPreciseFraction(PT, IsFull);
  if IsFull then
    IntPT := Round(PT)
  else
    IntPT := Trunc(PT);
  if (IntPT = 0) and (Fraction <> '') then
  Exit(Fraction + 'pt');
  Result := Format('%d%spt', [IntPT, Fraction]);
end;

function PintToGal(PT: double): string;
var
  GAL:      double;
  IntGAL:   integer;
  Fraction: string;
  IsFull:   boolean;
begin
  GAL      := PT / 8;
  Fraction := DecimalToFraction(GAL, IsFull);
  if IsFull then
    IntGAL := Round(GAL)
  else
    IntGAL := Trunc(GAL);
  Result := Format('%d%sgal', [IntGAL, Fraction]);
end;

function FlozToPint(FLOZ: double): string;
var
  PT:     double;
  IntPT:  integer;
  Fraction: string;
  IsFull: boolean;
begin
  PT := FLOZ / 16;
  Fraction := DecimalToPreciseFraction(PT, IsFull);
  if IsFull then
    IntPT := Round(PT)
  else
    IntPT := Trunc(PT);
  Result := Format('%d%spt', [IntPT, Fraction]);
end;

function PintToFloz(PT: double): string;
var
  FLOZ:     double;
  IntFLOZ:  integer;
  Fraction: string;
  IsFull:   boolean;
begin
  FLOZ     := PT * 16;
  Fraction := DecimalToPreciseFraction(FLOZ, IsFull);
  if IsFull then
    IntFLOZ := Round(FLOZ)
  else
    IntFLOZ := Trunc(FLOZ);
  Result := Format('%d%sfl oz', [IntFLOZ, Fraction]);
end;

function FlozToTsp(FLOZ: double): string;
var
  TSP:      double;
  IntTSP:   integer;
  Fraction: string;
  IsFull:   boolean;
begin
  TSP      := FLOZ * 6;
  Fraction := DecimalToPreciseFraction(TSP, IsFull);
  if IsFull then
    IntTSP := Round(TSP)
  else
    IntTSP := Trunc(TSP);
  Result := Format('%d%stsp', [IntTSP, Fraction]);
end;

function TspToFloz(TSP: double): string;
var
  FLOZ: double;
  IntFLOZ: integer;
  Fraction: string;
  IsFull: boolean;
begin
  FLOZ := TSP / 6;
  Fraction := DecimalToPreciseFraction(FLOZ, IsFull);
  if IsFull then
  IntFLOZ := Round(FLOZ)
  else
    IntFLOZ := Trunc(FLOZ);
  Result := Format('%d%sfl oz', [IntFLOZ, Fraction]);
end;

end.
