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

function FractionSymbol(Numerator: integer): string;

implementation

uses
  msrConvert;

function LbToKg(LB: double): string;
var
  KG: double;
begin
  KG     := LB * 0.453592;
  Result := FloatToStr(RoundTo(KG, -2)) + 'Kg';
end;

function KgToLb(KG: double): string;
var
  LB: double;
begin
  LB     := KG * 2.20462;
  Result := FloatToStr(RoundTo(LB, -2)) + 'lb';
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
begin
  OZ     := G / 28.35;
  Result := FloatToStr(RoundTo(OZ, -2)) + 'Oz';
end;

function LToGal(L: double): string;
var
  GAL: double;
begin
  GAL    := L / 3.785;
  Result := FloatToStr(RoundTo(GAL, -2)) + 'gal';
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
  FLOZ: double;
begin
  FLOZ   := ML / 29.5735;
  Result := FloatToStr(RoundTo(FLOZ, -2)) + 'fl oz';
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
  FT, INCH, FTFrac, INCHNum: double;
  FTInt, INCHNumInt: integer;
begin
  FT      := M * 3.281;
  FTInt   := Trunc(FT);
  FTFrac  := FT - FTInt;
  INCH    := FTFrac * 12;
  INCHNum := INCH - Trunc(INCH);
  INCHNumInt := Trunc(INCHNum);
  if INCHNumInt = 0 then
    Result := Format('%d''', [FTInt])
  else
    Result := Format('%d''%d%s"', [FTInt, Trunc(INCH), FractionSymbol(INCHNumInt)]);
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

end.
