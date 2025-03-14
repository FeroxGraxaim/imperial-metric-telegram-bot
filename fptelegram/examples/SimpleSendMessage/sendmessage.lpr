program sendmessage;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  tgsendertypes;

type

  { TMyBot }
  { You do not have to create a chilf to class TelegramSender.
    This is only necessary for logging in our sample }
  TMyBot = class(TTelegramSender)
  protected
    procedure DebugMessage(const Msg: String); override;
    procedure InfoMessage(const Msg: String); override;
    procedure ErrorMessage(const Msg: String); override;
  end;


var
  ABot: TMyBot;
  AToken, AMsg: String;
  Q: AnsiChar;
  AChatID: Int64;

{ TMyBot }

procedure TMyBot.DebugMessage(const Msg: String);
begin
  inherited DebugMessage(Msg);
  WriteLn('Debug: '+Msg);
end;

procedure TMyBot.InfoMessage(const Msg: String);
begin
  inherited InfoMessage(Msg);
  WriteLn('Information: '+Msg);
end;

procedure TMyBot.ErrorMessage(const Msg: String);
begin
  inherited ErrorMessage(Msg);
  WriteLn('Error: '+Msg);
end;

begin
  Writeln('This is simple console app without getting updates from telegram bot API');
  Writeln('Sample of using sendMessage from Telegram bot. fp-telegram lib is using');
  Write('Please enter bot telegram token: ');
  Readln(AToken);
  ABot:=TMyBot.Create(AToken);
  try
    Write('Now found out your (or other known to the bot user) user private or group ID');
    Write('And after please enter this user/chat ID (it is integer value [Int64]): ');
    Readln(AChatID);
    //ABot.APIEndPoint:='https://api.telegram.org/bot';
    repeat
      Write('Please enter message string: ');
      Readln(AMsg);
      ABot.sendMessage(AChatID, AMsg);
      Write('Do you want to send another message [Y/N]?: ');
      Readln(Q);
    until lowercase(Q)<>'y';
  finally
    ABot.Free;
  end;
end.

