unit testbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, tgsendertypes, IniFiles, fpjson;

type

  { TTestTelegramBase }

  TTestTelegramBase= class(TTestCase)
  private
    FConf: TIniFile;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    property Conf: TIniFile read FConf;
    procedure SaveJSONData(AData: TJSONData; const AFileName: String);
    procedure SaveString(const AString, AFileName: String);
  end;

  { TTestTelegramClass }

  TTestTelegramClass= class(TTestTelegramBase)
  private
    FBot: TTelegramSender;
    FChatID: Int64;
    FUserID: Int64;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    property Bot: TTelegramSender read FBot;
    property ChatID: Int64 read FChatID;
    property UserID: Int64 read FUserID;
  end;

implementation

uses
  eventlog
  ;

{ TTestTelegramClass }

procedure TTestTelegramClass.SetUp;
begin
  inherited SetUp;
  FBot:=TTelegramSender.Create(FConf.ReadString('Bot', 'Token', EmptyStr));
  FBot.Logger:=TEventLog.Create(nil);
  FBot.LogDebug:=True;
  with FBot.Logger do
  begin
    LogType:=ltFile;
    FileName:=ChangeFileExt(ParamStr(0), '.log');
    AppendContent:=True;
  end;
  if FBot.Token=EmptyStr then
    Fail('Please, specify bot token in testtelegram.ini! See readme.md');
  FChatID:=Conf.ReadInt64('Chat', 'ID', 0);
  if FChatID=0 then
    Fail('Please, specify chat ID in testtelegram.ini! See readme.md');
  FUserID:=Conf.ReadInt64('User', 'ID', 0);
end;

procedure TTestTelegramClass.TearDown;
begin
  FBot.Logger.Free;
  FBot.Logger:=nil;
  FreeAndNil(FBot);
  inherited TearDown;
end;

{ TTestTelegramBase }

procedure TTestTelegramBase.SetUp;
begin
  FConf:=TMemIniFile.Create('testtelegram.ini');
  TelegramAPI_URL:=FConf.ReadString('API', 'Endpoint', TelegramAPI_URL); // For Russian specific case
end;

procedure TTestTelegramBase.TearDown;
begin
  FreeAndNil(FConf);
end;

procedure TTestTelegramBase.SaveJSONData(AData: TJSONData;
  const AFileName: String);
begin
  if not Assigned(AData) then
  begin
    Fail('JSON data is empty!');
    Exit;
  end;
  SaveString(AData.FormatJSON, AFileName);
end;

procedure TTestTelegramBase.SaveString(const AString, AFileName: String);
var
  AStrings: TStringList;
begin
  AStrings:=TStringList.Create;
  try
    AStrings.Text:=AString;
    AStrings.SaveToFile(AFileName);
  finally
    AStrings.Free;
  end;
end;

end.

