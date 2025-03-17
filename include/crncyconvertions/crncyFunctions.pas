unit crncyFunctions;

{$mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson, RegExpr, jsonparser, currency;

function ConvertedCurrency(Value: double; OC, FC: string): double;
//function ProcessCurrency(const ReceivedText, ReplyText: string; MessageData: TJSONData): string;

implementation

uses
  Main;

function ConvertedCurrency(Value: double; OC, FC: string): double;
var
  Response: TStringStream;
  URL:      string;
  JSON:     TJSONData;
  Rates:    TJSONObject;
  RateOC, RateFC: double;
begin
  Response := TStringStream.Create('');
  with TFPHTTPClient.Create(nil) do
  try
    URL := Format('https://api.exchangeratesapi.io/v1/latest?access_key=%s',
    [CURRENCY_TOKEN]);

    Get(URL, Response);
    JSON  := GetJSON(Response.DataString);
    Rates := JSON.FindPath('rates') as TJSONObject;

    if OC = 'EUR' then
      RateOC := 1.0
    else
      RateOC := Rates.FindPath(OC).AsFloat;

    if FC = 'EUR' then
      RateFC := 1.0
    else
      RateFC := Rates.FindPath(FC).AsFloat;

    Result := (Value / RateOC) * RateFC;
  finally
    Response.Free;
    Free;
  end;
end;


{function ProcessCurrency: string;
var
  ChatID: Int64;
  MsgText, RepText: string;
begin

end;}


end.
