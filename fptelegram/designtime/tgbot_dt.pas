unit tgbot_dt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, receiverthread, tgsendertypes, fpjson
  ;

type

  { TInlineButtonItem }

  TInlineButtonItem = class(TCollectionItem)
  private
    FCallbackData: String;
    FSwitchInlineQuery: String;
    FText: String;
    FUrl: String;
  public
    procedure AssignTo(Dest: TPersistent); override;
  published
    property Text: String read FText write FText;
    property Url: String read FUrl write FUrl;
    property CallbackData: String read FCallbackData write FCallbackData;
    property SwitchInlineQuery: String read FSwitchInlineQuery write FSwitchInlineQuery;
  end;

  TInlineButtonRowItem = class;

  { TInlineButtonCollection }

  TInlineButtonCollection = class(TOwnedCollection)
  public
    constructor Create(aOwner: TInlineButtonRowItem);
  end;

  { TInlineButtonRowItem }

  TInlineButtonRowItem = class(TCollectionItem)
  private
    FButtonRows: TInlineButtonCollection;
    procedure SetButtonRows(AValue: TInlineButtonCollection);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;
  published
    property ButtonRows: TInlineButtonCollection read FButtonRows write SetButtonRows;
  end;

  TReplyMarkupItem = class;

  { TInlineKeyboardCollection }

  TInlineKeyboardCollection = class(TOwnedCollection)
  public
    constructor Create(aOwner: TReplyMarkupItem);
  end;

  { TReplyButtonItem }

  TReplyButtonItem = class(TCollectionItem)
  private
    Frequest_contact: Boolean;
    Frequest_location: Boolean;
    FText: String;
  public
    procedure AssignTo(Dest: TPersistent); override;
  published
    property text: String read FText write FText;
    property request_contact: Boolean read Frequest_contact write Frequest_contact;
    property request_location: Boolean read Frequest_location write Frequest_location;
  end;

  TReplyButtonRowItem = class;

  { TReplyButtonCollection }

  TReplyButtonCollection = class(TOwnedCollection)
  public
    constructor Create(AOwner: TReplyButtonRowItem);
  end;

  { TReplyButtonRowItem }

  TReplyButtonRowItem = class(TCollectionItem)
  private
    FButtonRows: TReplyButtonCollection;
    procedure SetButtonRows(AValue: TReplyButtonCollection);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;
  published
    property ButtonRows: TReplyButtonCollection read FButtonRows write SetButtonRows;
  end;

  { TReplyKeyboardCollection }

  TReplyKeyboardCollection = class(TOwnedCollection)
  public
    constructor Create(aOwner: TReplyMarkupItem);
  end;

  { TReplyMarkupItem }

  TReplyMarkupItem = class(TCollectionItem)
  private
    FForceReply: Boolean;
    FInlineKeyboard: TInlineKeyboardCollection;
    FInputFieldPlaceholder: String;
    FOneTimeKeyboard: Boolean;
    FRemoveKeyboard: Boolean;
    FReplyKeyboard: TReplyKeyboardCollection;
    FResizeKeyboard: Boolean;
    FSelective: Boolean;
    procedure SetInlineKeyBoard(AValue: TInlineKeyboardCollection);
    procedure SetReplyKeyboard(AValue: TReplyKeyboardCollection);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;
    { Caution: this is an object constructor.
        While there is no way for streaming to JSON nested array (or is there one?)
        we manually create jsonreplymarkup }
    function GetJSONReplyMarkup: TReplyMarkup;
  published
    property ForceReply: Boolean read FForceReply write FForceReply;
    property InlineKeyboard: TInlineKeyboardCollection read FInlineKeyBoard write SetInlineKeyBoard;
    property ReplyKeyboard: TReplyKeyboardCollection read FReplyKeyboard write SetReplyKeyboard;
    property RemoveKeyboard: Boolean read FRemoveKeyboard write FRemoveKeyboard;
    property ResizeKeyboard: Boolean read FResizeKeyboard write FResizeKeyboard;
    property OneTimeKeyboard: Boolean read FOneTimeKeyboard write FOneTimeKeyboard;
    property InputFieldPlaceholder: String read FInputFieldPlaceholder write FInputFieldPlaceholder;
    property Selective: Boolean read FSelective write FSelective;
  end;

  TReplyMarkupItemClass = class of TReplyMarkupItem;

  TCustomDTTelegramBot = class;

  { TReplyMarkupCollection }

  TReplyMarkupCollection = class(TOwnedCollection)
  private
    function GetReplyMarkup(aIndex: Integer): TReplyMarkupItem;
  public
    constructor Create(aOwner: TCustomDTTelegramBot);
    property ReplyMarkups[aIndex: Integer]: TReplyMarkupItem read GetReplyMarkup;
  end;

  TDTLongPollingThread = class;

  TTelegramBotEvent = procedure (aSender: TCustomDTTelegramBot) of object;

  { TCustomDTTelegramBot }

  TCustomDTTelegramBot = class(TComponent)
  private
    FActive: Boolean;
    FBotUsername: String;
    FHelpStrings: TStringList;
    FLongPollingTime: Integer;
    FOnReceiveCallback: TCallbackEvent;
    FOnReceiveEditedMessage: TMessageEvent;
    FOnReceiveMessageUpdate: TMessageEvent;
    FOnReceiveUpdate: TOnUpdateEvent;
    FOnDisconnectReceiver: TTelegramBotEvent;
    FReceiver: TDTLongPollingThread;
    FReceiverLogActive: Boolean;
    FReceiverLogDebug: Boolean;
    FReceiverLogFilename: String;
    FReplyMarkup: TReplyMarkupCollection;
    FSenderBot: TTelegramSender;
    FStartStrings: TStringList;
    FToken: String;
    function GetHelpText: TStrings;
    function GetSenderBot: TTelegramSender;
    function GetStartText: TStrings;
    procedure RaiseIfNoToken;
    procedure SetHelpText(AValue: TStrings);
    procedure SetReplyMarkups(AValue: TReplyMarkupCollection);
    procedure SetStartText(AValue: TStrings);
    procedure SetActive(AValue: Boolean);
  protected
    procedure DoDisconnectReceiver;
  public
    procedure BotgetMe;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartReceiver;
    procedure StopReceiver;
    property SenderBot: TTelegramSender read GetSenderBot;
    property Active: Boolean read FActive write SetActive;
    { It can be arrived with getMe command for example or specified manually.
      Required for some commands }
    property BotUsername: String read FBotUsername write FBotUsername;
    property Token: String read FToken write FToken;
    property StartText: TStrings read GetStartText write SetStartText;
    property HelpText: TStrings read GetHelpText write SetHelpText;
    property LongPollingTime: Integer read FLongPollingTime write FLongPollingTime; 
    property OnDisconnectReceiver: TTelegramBotEvent read FOnDisconnectReceiver write FOnDisconnectReceiver;
    property OnReceiveCallback: TCallbackEvent read FOnReceiveCallback write FOnReceiveCallback;
    property OnReceiveMessageUpdate: TMessageEvent read FOnReceiveMessageUpdate write FOnReceiveMessageUpdate; 
    property OnReceiveEditedMessage: TMessageEvent read FOnReceiveEditedMessage write FOnReceiveEditedMessage;
    { This event will be triggered if none of the other events have been handled (UpdateProcessed = True) after
      receiving the update from the telegram server. }
    property OnReceiveUpdate: TOnUpdateEvent read FOnReceiveUpdate write FOnReceiveUpdate;
    property ReceiverLogFilename: String read FReceiverLogFilename write FReceiverLogFilename;
    property ReceiverLogActive: Boolean read FReceiverLogActive write FReceiverLogActive;    
    property ReceiverLogDebug: Boolean read FReceiverLogDebug write FReceiverLogDebug;
    property ReplyMarkups: TReplyMarkupCollection read FReplyMarkup write SetReplyMarkups;
  end;

  { TDTLongPollingThread }

  TDTLongPollingThread = class(TLongPollingThread)
  private
    FDTBot: TCustomDTTelegramBot;
    FOnDisconnectReceiver: TTelegramBotEvent;
    procedure CallOnDisconnectReceiver;
  protected
    procedure DoTerminate; override;
  public
    constructor Create(aDTTelegramBot: TCustomDTTelegramBot);
  end;

  { TDTLongPollBot }

  TDTLongPollBot = class(TCustomDTTelegramBot)
  published
    property BotUsername;
    property Token;
    property StartText;
    property HelpText;
    property LongPollingTime;
    property OnReceiveMessageUpdate;
    property OnReceiveCallback;
    property OnReceiveUpdate;
    property OnDisconnectReceiver;
    property ReplyMarkups;
  end;

  EDTTelegramBot = class(Exception);

implementation

uses
  Dialogs
  ;

const
  _StartText = 'Welcome! This is start text for `/start` command.'+LineEnding+
    'You can change this text by the property `TDTLongPolBot.StartText`.'+LineEnding+
    'Caution: it is markdown markup';
  _HelpText = 'This is help text for `/help` command.'+LineEnding+
    'You can change this text by the property `TDTLongPolBot.HelpText`.'+LineEnding+
    'Caution: it is markdown markup';

{ TReplyButtonItem }

procedure TReplyButtonItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TReplyButtonItem then
  begin
    TReplyButtonItem(Dest).Ftext:=Ftext;
    TReplyButtonItem(Dest).Frequest_location:=Frequest_location;
    TReplyButtonItem(Dest).Frequest_contact:=Frequest_contact;
  end
  else
    inherited AssignTO(Dest);
end;

{ TReplyButtonCollection }

constructor TReplyButtonCollection.Create(AOwner: TReplyButtonRowItem);
begin
  inherited Create(aOwner, TReplyButtonItem);
end;

{ TReplyButtonRowItem }

procedure TReplyButtonRowItem.SetButtonRows(AValue: TReplyButtonCollection);
begin
  FButtonRows.Assign(AValue);
end;

constructor TReplyButtonRowItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FButtonRows:=TReplyButtonCollection.Create(Self);
end;

destructor TReplyButtonRowItem.Destroy;
begin
  FButtonRows.Free;
  inherited Destroy;
end;

procedure TReplyButtonRowItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TReplyButtonRowItem then
    TReplyButtonRowItem(Dest).FButtonRows.Assign(FButtonRows)
  else
    inherited AssignTO(Dest);
end;

{ TReplyKeyboardCollection }

constructor TReplyKeyboardCollection.Create(aOwner: TReplyMarkupItem);
begin
  inherited Create(aOwner, TReplyButtonRowItem);
end;

{ TInlineButtonItem }

procedure TInlineButtonItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TInlineButtonItem then
  begin                                                        
    TInlineButtonItem(Dest).Text:=Text;
    TInlineButtonItem(Dest).CallbackData:=CallbackData;
    TInlineButtonItem(Dest).SwitchInlineQuery:=SwitchInlineQuery;
    TInlineButtonItem(Dest).Url:=Url;
  end
  else
    inherited AssignTO(Dest);
end;

{ TInlineButtonRowItem }

procedure TInlineButtonRowItem.SetButtonRows(AValue: TInlineButtonCollection);
begin
  FButtonRows.Assign(AValue);
end;

constructor TInlineButtonRowItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FButtonRows:=TInlineButtonCollection.Create(Self);
end;

destructor TInlineButtonRowItem.Destroy;
begin
  FButtonRows.Free;
  inherited Destroy;
end;

procedure TInlineButtonRowItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TInlineButtonRowItem then
    TInlineButtonRowItem(Dest).FButtonRows.Assign(FButtonRows)
  else
    inherited AssignTO(Dest);
end;

{ TInlineButtonCollection }

constructor TInlineButtonCollection.Create(aOwner: TInlineButtonRowItem);
begin
  inherited Create(aOwner, TInlineButtonItem);
end;

{ TInlineKeyboardCollection }

constructor TInlineKeyboardCollection.Create(aOwner: TReplyMarkupItem);
begin
  inherited Create(aOwner, TInlineButtonRowItem);
end;

{ TReplyMarkupItem }

function TReplyMarkupItem.GetJSONReplyMarkup: TReplyMarkup;
var
  aRowItem, aButtonItem: TCollectionItem;
  aInlineKeyboard: TInlineKeyboard;
  aInlineButtonRow: TInlineKeyboardButtons;
  aReplyButtonRow: TKeyboardButtons;
  aReplyKeyboard: TKeyboardButtonArray;

  procedure AddKeyboardButton(aButton: TInlineButtonItem);
  begin
    if aButton.CallbackData<>EmptyStr then
    begin
      aInlineButtonRow.AddButton(aButton.Text, aButton.CallbackData);
      Exit;
    end;
    if aButton.Url<>EmptyStr then
    begin
      aInlineButtonRow.AddButtonUrl(aButton.Text, aButton.Url);
      Exit;
    end;   
    if aButton.SwitchInlineQuery<>EmptyStr then
      aInlineButtonRow.AddButtonInline(aButton.Text, aButton.SwitchInlineQuery)
  end;
  procedure AddKeyboardButton(aButton: TReplyButtonItem);
  var
    aJSONBtn: TKeyboardButton;
  begin
    aJSONBtn:=TKeyboardButton.Create(aButton.text);
    if aButton.request_contact then
      aJSONBtn.RequestContact:=aButton.request_contact;
    if aButton.request_location then
      aJSONBtn.RequestLocation:=aButton.request_location;
    aReplyButtonRow.Add(aJSONBtn);
  end;

begin
  Result:=TReplyMarkup.Create;
  if FForceReply then
  begin
    Result.ForceReply:=True;
    Result.InputFieldPlaceholder:=FInputFieldPlaceholder;
    Result.Selective:=FSelective;
    Exit;
  end;
  if FInlineKeyBoard.Count>0 then
  begin
    aInlineKeyboard:=Result.CreateInlineKeyBoard;
    for aRowItem in FInlineKeyBoard do
    begin
      aInlineButtonRow:=aInlineKeyboard.Add;
      for aButtonItem in (aRowItem as TInlineButtonRowItem).ButtonRows do
        AddKeyboardButton(aButtonItem as TInlineButtonItem);
    end;
  end;
  if FReplyKeyboard.Count>0 then
  begin
    aReplyKeyboard:=Result.CreateReplyKeyboard;
    for aRowItem in FReplyKeyboard do
    begin
      aReplyButtonRow:=aReplyKeyboard.Add;
      for aButtonItem in (aRowItem as TReplyButtonRowItem).ButtonRows do
        AddKeyboardButton(aButtonItem as TReplyButtonItem);
    end;
  end;
end;

procedure TReplyMarkupItem.SetInlineKeyBoard(AValue: TInlineKeyboardCollection);
begin
  FInlineKeyBoard.Assign(AValue);
end;

procedure TReplyMarkupItem.SetReplyKeyboard(AValue: TReplyKeyboardCollection);
begin
  FReplyKeyboard.Assign(AValue);
end;

constructor TReplyMarkupItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FInlineKeyBoard:=TInlineKeyboardCollection.Create(Self);
  FReplyKeyboard:=TReplyKeyboardCollection.Create(Self);
end;

destructor TReplyMarkupItem.Destroy;
begin
  FReplyKeyboard.Free;
  FInlineKeyBoard.Free;
  inherited Destroy;
end;

procedure TReplyMarkupItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TReplyMarkupItem then
  begin
    TReplyMarkupItem(Dest).FForceReply:=FForceReply;
    TReplyMarkupItem(Dest).FInlineKeyBoard.Assign(FInlineKeyBoard);
    TReplyMarkupItem(Dest).FReplyKeyboard.Assign(FReplyKeyboard);
    TReplyMarkupItem(Dest).FOneTimeKeyboard:=FOneTimeKeyboard;
    TReplyMarkupItem(Dest).FRemoveKeyboard:=FRemoveKeyboard;
    TReplyMarkupItem(Dest).FResizeKeyboard:=FResizeKeyboard;
    TReplyMarkupItem(Dest).FInputFieldPlaceholder:=FInputFieldPlaceholder;
    TReplyMarkupItem(Dest).FSelective:=FSelective;
  end
  else
    inherited AssignTO(Dest);
end;

{ TReplyMarkupCollection }

function TReplyMarkupCollection.GetReplyMarkup(aIndex: Integer): TReplyMarkupItem;
begin
  Result:=TReplyMarkupItem(Items[aIndex]);
end;

constructor TReplyMarkupCollection.Create(aOwner: TCustomDTTelegramBot);
begin
  inherited Create(aOwner, TReplyMarkupItem);
end;

{ TCustomDTTelegramBot }

procedure TCustomDTTelegramBot.StartReceiver;
begin
  if FActive then
    Exit;
  RaiseIfNoToken;
  FActive:=True;
  FReceiver:=TDTLongPollingThread.Create(Self);
  FReceiver.FreeOnTerminate:=False;
  FReceiver.LongpollingTimeout:=LongPollingTime;
  FReceiver.Bot.Logger.FileName:=FReceiverLogFilename;   
  FReceiver.Bot.Logger.Active:=FReceiverLogActive;
  FReceiver.Bot.LogDebug:=FReceiverLogDebug;
  FReceiver.Bot.Token:=FToken;
  FReceiver.Bot.StartText:=FStartStrings.Text;
  FReceiver.Bot.HelpText:=FHelpStrings.Text;
  FReceiver.OnReceiveMessageUpdate:=FOnReceiveMessageUpdate;
  FReceiver.OnReceiveEditedMessage:=FOnReceiveEditedMessage;
  FReceiver.OnReceiveCallack:=FOnReceiveCallback;
  FReceiver.OnReceiveUpdate:=FOnReceiveUpdate;
  if Assigned(FOnDisconnectReceiver) then
    FReceiver.FOnDisconnectReceiver:=FOnDisconnectReceiver;
  FReceiver.Start;
end;

procedure TCustomDTTelegramBot.RaiseIfNoToken;
begin
  if FToken.IsEmpty then
    raise EDTTelegramBot.Create('Token for telegram bot is not specified!');
end;

function TCustomDTTelegramBot.GetHelpText: TStrings;
begin
  Result:=FHelpStrings;
end;

function TCustomDTTelegramBot.GetSenderBot: TTelegramSender;
begin
  if not Assigned(FSenderBot) then
    FSenderBot:=TTelegramSender.Create(FToken);
  Result:=FSenderBot;
end;

function TCustomDTTelegramBot.GetStartText: TStrings;
begin
  Result:=FStartStrings;
end;

procedure TCustomDTTelegramBot.SetHelpText(AValue: TStrings);
begin
  FHelpStrings.Assign(AValue);
end;

procedure TCustomDTTelegramBot.SetReplyMarkups(AValue: TReplyMarkupCollection);
begin
  FReplyMarkup.Assign(AValue);
end;

procedure TCustomDTTelegramBot.SetStartText(AValue: TStrings);
begin
  FStartStrings.Assign(AValue);
end;

procedure TCustomDTTelegramBot.StopReceiver;
begin
  if not FActive then
    Exit;
  FReceiver.Terminate;
  FReceiver.WaitFor;
  FActive:=False;
  FreeAndNil(FReceiver);
end;

procedure TCustomDTTelegramBot.SetActive(AValue: Boolean);
begin
  if FActive=AValue then Exit;
  if AValue then
    StartReceiver
  else
    StopReceiver;
end;

procedure TCustomDTTelegramBot.DoDisconnectReceiver;
begin
  if Assigned(FOnDisconnectReceiver) then
    FOnDisconnectReceiver(Self);
end;

constructor TCustomDTTelegramBot.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStartStrings:=TStringList.Create;
  FHelpStrings:=TStringList.Create;
  FLongPollingTime:=4; // secs! 0 is not recommended
  FStartStrings.Text:=_StartText;
  FHelpStrings.Text:= _HelpText;
  FReplyMarkup:=TReplyMarkupCollection.Create(Self);
end;

destructor TCustomDTTelegramBot.Destroy;
begin
  FReplyMarkup.Free;
  FSenderBot.Free;
  FHelpStrings.Free;
  FStartStrings.Free;
  StopReceiver;
  inherited Destroy;
end;

procedure TCustomDTTelegramBot.BotgetMe;
begin
  RaiseIfNoToken;
  if SenderBot.getMe then
  begin
    BotUsername:=FSenderBot.BotUsername;
    if (csDesigning in ComponentState) then
      with FSenderBot.BotUser do
        MessageDlg('Telegram response for getMe',
          Format('Bot username: %s. User_id: %d. First_Name: %s', [Username, ID, First_name]), mtInformation, [mbOK],
          '');
  end
  else
    raise EDTTelegramBot.Create('Unsuccessful request!');
end;

{ TDTLongPollingThread }

procedure TDTLongPollingThread.CallOnDisconnectReceiver;
begin
  FOnDisconnectReceiver(FDTBot);
end;

procedure TDTLongPollingThread.DoTerminate;
begin
  inherited DoTerminate;
  if Assigned(FOnDisconnectReceiver) then
    Synchronize(@CallOnDisconnectReceiver);
end;

constructor TDTLongPollingThread.Create(aDTTelegramBot: TCustomDTTelegramBot);
begin       
  FDTBot:=aDTTelegramBot;
  inherited Create;
end;

end.
