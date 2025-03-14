unit receiverthread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, tgtypes, tgsendertypes, eventlog, tgbot
  ;

type

  TLongPollingThread = class;

  { TThreadedBot }

  TThreadedBot=class(TTelegramBot)
  private
    FThread: TLongPollingThread;
    FCurrentCallback: TCallbackQueryObj;
    procedure SynDoReceiveMessage;
    procedure SynDoReceiveCallback;
    procedure SynDoReceiveUpdate;  
    procedure SynDoReceiveEditedMessage;
  public
    procedure DoReceiveMessageUpdate(AMessage: TTelegramMessageObj); override;
    procedure DoReceiveCallbackQuery(ACallback: TCallbackQueryObj); override;
    procedure DoReceiveUpdate(AnUpdate: TTelegramUpdateObj); override;
    procedure DoReceiveEditedMessage(AMessage: TTelegramMessageObj); override;
  end;

  { TLongPollingThread }

  TLongPollingThread=class(TThread)
  private
    FLogger: TEventLog;
    FBot: TThreadedBot;
    FOnReceiveCallack: TCallbackEvent;
    FOnReceiveEditedMessage: TMessageEvent;
    FOnReceiveMessageUpdate: TMessageEvent;
    FOnReceiveUpdate: TOnUpdateEvent;
    FTerminated: Boolean;
    FLPTimeout: Integer;
    function GetLogger: TEventLog;
    procedure SetLogger(AValue: TEventLog);
    property Logger: TEventLog read GetLogger write SetLogger;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    property Bot: TThreadedBot read FBot;
    property LongpollingTimeout: Integer read FLPTimeout write FLPTimeout;
    property OnReceiveCallack: TCallbackEvent read FOnReceiveCallack write FOnReceiveCallack;
    property OnReceiveMessageUpdate: TMessageEvent read FOnReceiveMessageUpdate write FOnReceiveMessageUpdate;
    property OnReceiveUpdate: TOnUpdateEvent read FOnReceiveUpdate write FOnReceiveUpdate;      
    property OnReceiveEditedMessage: TMessageEvent read FOnReceiveEditedMessage write FOnReceiveEditedMessage;
  end;

implementation

uses
  LazFileUtils, FileUtil, LazUTF8, ssockets
  ;

{ TThreadedBot }

procedure TThreadedBot.SynDoReceiveMessage;
begin
  FThread.FOnReceiveMessageUpdate(Self, CurrentMessage);
end;

procedure TThreadedBot.SynDoReceiveCallback;
begin
  FThread.FOnReceiveCallack(Self, FCurrentCallback)
end;

procedure TThreadedBot.SynDoReceiveUpdate;
begin
  FThread.FOnReceiveUpdate(Self, CurrentUpdate);
end;

procedure TThreadedBot.SynDoReceiveEditedMessage;
begin
  FThread.FOnReceiveEditedMessage(Self, CurrentMessage);
end;

procedure TThreadedBot.DoReceiveMessageUpdate(AMessage: TTelegramMessageObj);
begin
  inherited DoReceiveMessageUpdate(AMessage);
  if Assigned(FThread.FOnReceiveMessageUpdate) and not UpdateProcessed then
    FThread.Synchronize(@SynDoReceiveMessage);
end;

procedure TThreadedBot.DoReceiveCallbackQuery(ACallback: TCallbackQueryObj);
begin
  inherited DoReceiveCallbackQuery(ACallback);
  if Assigned(FThread.FOnReceiveCallack) and not UpdateProcessed then
  begin
    FCurrentCallback:=ACallback;
    FThread.Synchronize(@SynDoReceiveCallback);
  end;
end;

procedure TThreadedBot.DoReceiveUpdate(AnUpdate: TTelegramUpdateObj);
begin
  inherited DoReceiveUpdate(AnUpdate);
  if Assigned(FThread.FOnReceiveUpdate) and not UpdateProcessed then
    FThread.Synchronize(@SynDoReceiveUpdate);
end;

procedure TThreadedBot.DoReceiveEditedMessage(AMessage: TTelegramMessageObj);
begin
  inherited DoReceiveEditedMessage(AMessage);
  if Assigned(FThread.FOnReceiveEditedMessage) and not UpdateProcessed then
    FThread.Synchronize(@SynDoReceiveEditedMessage);
end;

{ TLongPollingThread }

procedure TLongPollingThread.SetLogger(AValue: TEventLog);
begin
  if FLogger=AValue then Exit;
  FLogger.Free;
  FLogger:=AValue;
end;

function TLongPollingThread.GetLogger: TEventLog;
begin
  if not Assigned(FLogger) then
  begin
    FLogger:=TEventLog.Create(nil);
    FLogger.Identification:='Receiver thread';
    FLogger.LogType:=ltFile;
    FLogger.Active:=True;
    FLogger.AppendContent:=True;
  end;
  Result:=FLogger;
end;

constructor TLongPollingThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate:=True;
  FBot:=TThreadedBot.Create(EmptyStr);
  FBot.FThread:=self;
  FBot.Logger:=Logger;
  FTerminated:=False;
  FLPTimeout:=4;
  Logger.Info('Log started');
end;

destructor TLongPollingThread.Destroy;
begin
  FreeAndNil(FBot);
  FreeAndNil(FLogger);
  inherited Destroy;
end;

procedure TLongPollingThread.Execute;
begin
  while not Terminated do
  try
    FBot.getUpdatesEx(0, FLPTimeout);
  except
    on E: ESocketError do
      begin
      end;
  {  Yes, it is doubtful.
    It is necessary that if the connection to the network fails, the stream continues to work
    It is possible to add more specialized exception handling
    }
  end;
end;

end.


