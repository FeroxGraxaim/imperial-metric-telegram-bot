unit Convertions;

{$mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils;

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

implementation

uses
  msrConvert;

function LbToKg(LB: double): string;
var
  KG: double;
begin
  KG     := LB * 0.453592;
  Result := FormatFloat('0.00', KG) + 'Kg';
end;

function KgToLb(KG: double): string;
var
  LB: double;
begin
  LB     := KG * 2.20462;
  Result := FormatFloat('0.00', LB) + 'lb';
end;

function OzToG(OZ: double): string;
var
  G: double;
begin
  G      := OZ * 28.35;
  Result := FormatFloat('0.00', G) + 'g';
end;

function GToOz(G: double): string;
var
  OZ: double;
begin
  OZ     := G / 28.35;
  Result := FormatFloat('0.00', OZ) + 'Oz';
end;

function LToGal(L: double): string;
var
  GAL: double;
begin
  GAL    := L / 3.785;
  Result := FormatFloat('0.00', GAL) + 'gal';
end;

function GalToL(GAL: double): string;
var
  L: double;
begin
  L      := GAL * 3.785;
  Result := FormatFloat('0.00', L) + 'L';
end;

function MlToFloz(ML: double): string;
var
  FLOZ: double;
begin
  FLOZ   := ML / 29.5735;
  Result := FormatFloat('0.00', FLOZ) + 'fl oz';
end;

function FlozToMl(FLOZ: double): string;
var
  ML: double;
begin
  ML     := FLOZ * 29.5735;
  Result := FormatFloat('0.00', ML) + 'mL';
end;

function KmToMi(KM: double): string;
var
  MI: double;
begin
  MI     := KM / 1.609;
  Result := FormatFloat('0.00', MI) + 'mi';
end;

function MiToKm(MI: double): string;
var
  KM: double;
begin
  KM     := MI * 1.609;
  Result := FormatFloat('0.00', KM) + 'Km';
end;

function MToFt(M: double): string;
var
  FT: double;
begin
  FT     := M * 3.281;
  Result := FormatFloat('0.00', FT) + 'ft';
end;

function FtToM(FT: double): string;
var
  M: double;
begin
  M      := FT / 3.281;
  Result := FormatFloat('0.00', M) + 'm';
end;

function CmToIn(CM: double): string;
var
  INCH: double;
begin
  INCH   := CM / 2.54;
  Result := FormatFloat('0.00', INCH) + 'in';
end;

function InToCm(INCH: double): string;
var
  CM: double;
begin
  CM     := INCH * 2.54;
  Result := FormatFloat('0.00', CM) + 'cm';
end;

function CToF(C: double): string;
var
  F: double;
begin
  F      := (C * 9 / 5) + 32;
  Result := FormatFloat('0.00', F) + '°F';
end;

function FToC(F: double): string;
var
  C: double;
begin
  C      := (F - 32) * 5 / 9;
  Result := FormatFloat('0.00', C) + '°C';
end;

function MhpToHp(MHP: double): string;
var
  HP: double;
begin
  HP     := MHP / 1.014;
  Result := FormatFloat('0.00', HP) + 'hp';
end;

function HpToMhp(HP: double): string;
var
  MHP: double;
begin
  MHP    := HP * 1.014;
  Result := FormatFloat('0.00', MHP) + 'mhp';
end;

end.
