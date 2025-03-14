unit crncyFunctions;

{$mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson, RegExpr, jsonparser, currency;

function ConvertCurrency(Value: double; OC, FC: string): double;
function ProcessCurrency(const ReceivedText, ReplyText: string; MessageData: TJSONData): string;

implementation

uses
  Main;

function ConvertCurrency(Value: double; OC, FC: string): double;
var
  Response: TStringStream;
  URL:      string;
  JSON:     TJSONData;
  ExchRate: double;
begin
  with TFPHTTPClient.Create(nil) do
  try
    URL := Format('https://api.exchangeratesapi.io/v1/convert?access_key=%s&' +
      'from=%s&to=%s&amount=%f', [CURRENCY_TOKEN, OC, FC, Value]);

    Get(URL, Response);
    JSON     := GetJSON(Response.DataString);
    ExchRate := JSON.FindPath('result').AsFloat;

    Result := ExchRate;
  finally
    Response.Free;
    Free;
  end;
end;

function ProcessCurrency: string;
var
  ChatID: Int64;
  MsgText, RepText: string;
begin

end;


end.
