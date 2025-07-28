unit Currency;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson, jsonparser;

type

  { CurrencyConversion }

  TCurrencyConversion = class
  function GetAPI: string;
end;

var
  CurrencyConversion: TCurrencyConversion;

implementation

uses
  Main, crncyFunctions;

{ CurrencyConversion }

function TCurrencyConversion.GetAPI: string;
begin
  Result := 'https://api.exchangeratesapi.io/v1/latest?access_key=' +
  CURRENCY_TOKEN;
end;

end.

