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
  untMain { you can add units after this };

type

  { TImpMetBot }

  TImpMetBot = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
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
    end;

    writeln('BOT initialized');
    while True do begin
      MainClass.DetectMessage;
      Sleep(1000);
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
  { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
  end;

var
  Application: TImpMetBot;
begin
  Application := TImpMetBot.Create(nil);
  Application.Title := 'Brotherhood Imperial/Metric Converter';
  Application.Run;
  Application.Free;
end.
