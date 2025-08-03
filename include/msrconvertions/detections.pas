unit Detections;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function GramsToImperial(G: double): string;
function OuncesToMetric(OZ: double): string;
function KilosToImperial(KG: double): string;
function PoundsToMetric(OZ: double): string;

implementation

function GramsToImperial(G: double): string;
var
 GR, DRM, OZ, LB, TONS: double;
  Fraction: string;
begin
   OZ := GToOz(G);
  if OZ <= (1/16) then
end;

function OuncesToMetric(OZ: double): string;
begin

end;

function KilosToImperial(KG: double): string;
begin

end;

function PoundsToMetric(OZ: double): string;
begin

end;

end.

