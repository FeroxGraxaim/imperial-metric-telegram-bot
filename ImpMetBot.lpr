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
  untMain,
  IniFiles;

var
  tgtoken: string;

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
  end;

{ TImpMetBot }

  procedure TImpMetBot.DoRun;
  var
    ErrorMsg: string;
  begin
  // quick check parameters
    ErrorMsg := CheckOptions('h', 'help');
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
    end;

    ReadConfig;

    if TOKEN = 'null' then
    begin
      writeln('Token not found!');
      UpdateToken;
    end;
    writeln('BOT initialized');
    while True do
    try
      MainClass.DetectMessage;
      Sleep(1000);
    except
      on E: Exception do
      begin
        if Pos('404', E.Message) > 0 then
          writeln('Error: Invalid or missing token! Check your configuration or ' +
            'change your token with "impmetbot --update-token"')
        else
          writeln('Error: ' + E.Message);
        Halt(1);
      end;
    end;
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
    writeln('-h, -help: shows this help.');
    Writeln('-u, -update-token: Changes the BOT Telegram Token.');
  end;

  procedure TImpMetBot.UpdateToken;
  begin
    Write('Insert your token: ');
    readln(TOKEN);
    with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'impmetbot.ini') do
    try
      WriteString('Config', 'TOKEN', TOKEN);
      writeln('Token updated!');
    finally
      Free;
    end;
  end;

  procedure TImpMetBot.ReadConfig;
  begin
    writeln('Reading config file');
    with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'impmetbot.ini') do
    try
      TOKEN := ReadString('Config', 'TOKEN', 'null');
    finally
      Free;
    end;
  end;

var
  Application: TImpMetBot;
begin
  Application := TImpMetBot.Create(nil);
  Application.Title := 'Brotherhood Imperial/Metric Converter';
  Application.Run;
  Application.Free;
end.
