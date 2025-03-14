unit ReceiveUpdatesThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tgtypes, tgsendertypes, eventlog
  ;

type

  TOnAppendMessage = procedure (const aMessage: String) of object;

  { TReceiverThread }

  TReceiverThread=class(TThread)
  private
    FBot: TTelegramSender;
    FOnAppendMessage: TOnAppendMessage;
    FLPTimeout: Integer;
    procedure BotReceiveMessage({%H-}ASender: TObject; {%H-}AMessage: TTelegramMessageObj);
    procedure BotStartCommandHandler({%H-}ASender: TObject; const {%H-}ACommand: String;
      {%H-}AMessage: TTelegramMessageObj);
    procedure SendMsgToMainThread;
  public
    constructor Create(const AToken: String);
    destructor Destroy; override;
    procedure Execute; override;
    property Bot: TTelegramSender read FBot write FBot;
    property OnAppendMessage: TOnAppendMessage read FOnAppendMessage write FOnAppendMessage;
  end;

implementation

{ TReceiverThread }

procedure TReceiverThread.BotReceiveMessage(ASender: TObject; AMessage: TTelegramMessageObj);
begin
  { You must synchronize (or make other thread-safe manipulations) due
      BotReceiveMessage is called from ReceiverThread) }
  Synchronize(@SendMsgToMainThread);
end;

procedure TReceiverThread.BotStartCommandHandler(ASender: TObject; const ACommand: String;
  AMessage: TTelegramMessageObj);
begin
  FBot.sendMessage('Hi! This is HelloWorld bot (Long Polling) developed in Lazarus');
end;

procedure TReceiverThread.SendMsgToMainThread;
begin
  if Assigned(FOnAppendMessage) then
    FOnAppendMessage(FBot.CurrentUpdate.AsString);
end;

constructor TReceiverThread.Create(const AToken: String);
begin
  inherited Create(True);
  FreeOnTerminate:=False;
  FBot:=TTelegramSender.Create(AToken);
  FBot.APIEndPoint:=TelegramAPI_URL;   // no need
  FBot.Logger:=TEventLog.Create(nil);
  FBot.Logger.LogType:=ltFile;
  FBot.LogDebug:=True;
  FBot.CommandHandlers['/start']:=@BotStartCommandHandler;
  FBot.OnReceiveMessage:=@BotReceiveMessage;
  FLPTimeout:=1; // LongPolling timeout
end;

destructor TReceiverThread.Destroy;
begin
  FBot.Logger.Free;
  FreeAndNil(FBot);
  inherited Destroy;
end;

procedure TReceiverThread.Execute;
begin
  while not Terminated do
    FBot.getUpdatesEx(0, FLPTimeout);
end;

end.

