program ImpMetBot;

{$mode objfpc}
{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  Main,
  IniFiles,
  Convertions,
  currency,
  crncyFunctions,
  FPHTTPClient,
  fpjson,
  jsonparser,
  RegExpr,
  opensslsockets,
  //Telegram package:
  fptelegram,
  tgtypes,
  tgsendertypes, BotMsgs;

type

  { TImpMetBot }

  TImpMetBot = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure UpdateToken;
    procedure ReadConfig;
    procedure ShowToken;
  end;

{ TImpMetBot }

  procedure TImpMetBot.DoRun;
  var
    ErrorMsg: string;
  begin
  // quick check parameters
    ErrorMsg := CheckOptions('h u s', 'help update-token show-token');
    if ErrorMsg <> '' then begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

  // parse parameters
    if HasOption('h', 'help') then begin
      WriteHelp;
      Terminate;
      Exit;
    end
    else if HasOption('u', 'update-token') then begin
      UpdateToken;
      Terminate;
      Exit;
    end
    else if HasOption('s', 'show-token') then begin
      ShowToken;
      Terminate;
      Exit;
    end;

    ReadConfig;

    if TELEGRAM_TOKEN = 'null' then
    begin
      writeln('Token not found!');
      UpdateToken;
    end;

    MainClass.StartBot;
  // stop program loop
    Terminate;
  end;

  constructor TImpMetBot.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := False;
  end;

  destructor TImpMetBot.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TImpMetBot.WriteHelp;
  begin
    writeln('Usage: ', ExeName, ' -h');
    writeln('-h, --help: shows this help.');
    Writeln('-u, --update-token: Changes the BOT Telegram token.');
    WriteLn('-s, --show-token: Shows the current token registered.');
  end;

  procedure TImpMetBot.UpdateToken;
  begin
    TELEGRAM_TOKEN := '';
    CURRENCY_TOKEN := '';
    Write('Insert your Telegram token (or press just Enter to skip): ');
    readln(TELEGRAM_TOKEN);
    Write('Insert your Exchangerates API token (or press just Enter to skip): ');
    readln(CURRENCY_TOKEN);
    with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'impmetbot.ini') do
    try
      if Trim(TELEGRAM_TOKEN) <> '' then
        WriteString('Config', 'TELEGRAM_TOKEN', TELEGRAM_TOKEN);
      if Trim(CURRENCY_TOKEN) <> '' then
        WriteString('Config', 'CURRENCY_TOKEN', CURRENCY_TOKEN);
      writeln('Tokens updated!');
    finally
      Free;
    end;
  end;

  procedure TImpMetBot.ReadConfig;
  begin
    writeln('Reading config file');
    with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'impmetbot.ini') do
    try
      TELEGRAM_TOKEN := ReadString('Config', 'TELEGRAM_TOKEN', 'null');
      CURRENCY_TOKEN := ReadString('Config', 'CURRENCY_TOKEN', 'null');
    finally
      Free;
    end;
  end;

  procedure TImpMetBot.ShowToken;
  var
    Key: char;
    ReadTGTokeN, ReadCRToken: string;
  begin
    with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'impmetbot.ini') do
    try
      ReadTGToken := ReadString('Config', 'TELEGRAM_TOKEN', 'null');
      ReadCRToken := ReadString('Config', 'CURRENCY_TOKEN', 'null');
      if (ReadTGToken = 'null') and (ReadCRToken = 'null') then
      begin
        writeln('No tokens registered. Please run the program with "--update-token' +
          '" parameter and try again.');
        Exit;
      end;
    finally
      Free;
    end;

    repeat
      Write('WARNING: You may not show this token to anyone! Continue? (Y/N)');
      ReadLn(Key);
      Key := UpCase(Key);
    until (Key = 'Y') or (Key = 'N');

    case Key of
      'Y': begin
        WriteLn('Telegram Token: ', ReadTGToken);
        WriteLn('Currency Token: ', ReadCRToken);
      end;
      'N': Exit;
    end;
  end;

var
  Application: TImpMetBot;
begin
  Application := TImpMetBot.Create(nil);
  Application.Title := 'WordWide Brotherhood BOT';
  Application.Run;
  Application.Free;
end.
