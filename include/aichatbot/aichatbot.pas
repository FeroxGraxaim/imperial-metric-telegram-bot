unit aichatbot;

{$mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, fphttpclient;

const
  URL = 'https://api.openai.com/v1/chat/completions';

type

  { TAiRequest }

  TAiRequest = object
  private
    Client: TFPHttpClient;
  public
    JsonRequest: string;
    procedure InitAi;
    function SendRequest: string;
    constructor Create;
    destructor Destroy;
  end;

implementation

uses
  Main;

procedure TAiRequest.InitAi;
begin
  Client := TFPHttpClient.Create(nil);
  Client.AddHeader('Content-Type', 'application/json');
  Client.AddHeader('Authorization', 'Bearer ' + AI_TOKEN);
end;

function TAiRequest.SendRequest: string;
var
  RequestBody, ResponseBody: TStringStream;
begin
  Result := '';

  RequestBody := TStringStream.Create(JsonRequest, TEncoding.UTF8);
  ResponseBody := TStringStream.Create('');
  try
    Client.Post(URL, RequestBody);
    Result := ResponseBody.DataString;
  finally
    RequestBody.Free;
    ResponseBody.Free;
  end;
end;

constructor TAiRequest.Create;
begin
  InitAi;
end;

destructor TAiRequest.Destroy;
begin
  Client.Free;
  //inherited Destroy;
end;

end.

