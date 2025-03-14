program tg_longpolling;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  tgsendertypes, configuration, eventlog, tgtypes
  ;

type

  { TtgLPApplication }

  TtgLPApplication = class(TCustomApplication)
  private
    FBot: TTelegramSender;
    procedure BotReceiveMessage({%H-}ASender: TObject; AMessage: TTelegramMessageObj);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TtgLPApplication }

procedure TtgLPApplication.BotReceiveMessage(ASender: TObject;
  AMessage: TTelegramMessageObj);
begin
  WriteLn(AMessage.AsString);
end;

procedure TtgLPApplication.DoRun;
begin
  while not Terminated do
    FBot.getUpdatesEx(0, 10);
  Terminate;
end;

constructor TtgLPApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FBot:=TTelegramSender.Create(Conf.TelegramToken);
  with FBot do
  begin
    Logger:=TEventLog.Create(nil);
    Logger.LogType:=ltFile;
    Logger.Active:=True;
    LogDebug:=True;
    OnReceiveMessage:=@BotReceiveMessage;
    APIEndPoint:=Conf.TelegramEndPoint;
  end;
end;

destructor TtgLPApplication.Destroy;
begin
  FBot.Logger.Free;
  FBot.Free;
  inherited Destroy;
end;

var
  Application: TtgLPApplication;
begin
  Application:=TtgLPApplication.Create(nil);
  Application.Title:='Test Telegram LongPolling App';
  Application.Run;
  Application.Free;
end.

