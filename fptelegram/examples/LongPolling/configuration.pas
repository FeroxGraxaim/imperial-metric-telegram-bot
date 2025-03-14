unit configuration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TAppConfig = record
    TelegramToken: String;
    TelegramEndPoint: String;
  end;


var
  Conf: TAppConfig;

implementation

uses
  IniFiles
  ;

function ConfigFile: String;
begin
  Result:=ChangeFileExt(ParamStr(0), '.ini');
end;

procedure LoadConfig(var aConf: TAppConfig);
var
  aIni: TMemIniFile;
begin
  aIni:=TMemIniFile.Create(ConfigFile);
  aConf.TelegramToken:=aIni.ReadString('Telegram', 'Token', EmptyStr);
  aConf.TelegramEndPoint:=aIni.ReadString('Telegram', 'EndPoint', aConf.TelegramEndPoint); // Optional line and config. Can be default
  aIni.Free;
end;

initialization
  LoadConfig(Conf);

end.

