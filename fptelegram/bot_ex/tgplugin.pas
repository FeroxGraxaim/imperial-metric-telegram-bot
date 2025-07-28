unit tgplugin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, tgtypes, tgsendertypes, tgbot
  ;

type

  { TCustomTelegramBotPlugin }

  TCustomTelegramBotPlugin = class
  private
    FBot: TTelegramBot;
    FDirectory: String;
  protected
    property Bot: TTelegramBot read FBot;
    procedure Register; virtual; abstract;
  public
    constructor Create(aOwner: TTelegramBot); virtual;
    property Directory: String read FDirectory write FDirectory;
  end;

implementation

uses
  DateUtils
  ;

{ TCustomTelegramBotPlugin }

constructor TCustomTelegramBotPlugin.Create(aOwner: TTelegramBot);
begin
  FBot:=aOwner;
  Register;
end;

end.

