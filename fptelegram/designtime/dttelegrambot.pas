unit DTTelegramBot;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, tgbot_dt, ComponentEditors
  ;

type

  { TLPTelegramBotEditor }

  TLPTelegramBotEditor = class(TComponentEditor)
  private
    procedure getMe;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

uses
  LResources
  ;

procedure Register;
begin
  {$I dttelegrambot_icon.lrs}
  RegisterComponentEditor(TDTLongPollBot, TLPTelegramBotEditor);
  RegisterComponents('Misc', [TDTLongPollBot]);
end;

{ TLPTelegramBotEditor }

procedure TLPTelegramBotEditor.getMe;
begin
  (Component as TDTLongPollBot).BotgetMe;
end;

procedure TLPTelegramBotEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: getMe;
  else
    inherited;
  end;
end;

function TLPTelegramBotEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result:='getMe for the bot';
  else
    inherited;
  end;
end;

function TLPTelegramBotEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

end.
