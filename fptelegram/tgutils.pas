unit tgutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, tgtypes, tgsendertypes
  ;

const
  mdCode='`';

function MarkdownEscape(const S: String): String; 
function MarkdownEscapeV2(const S: String): String;
function CaptionFromChat(aChat: TTelegramChatObj): String;
function CaptionFromUser(AUser: TTelegramUserObj): String;
function BuildLink(const aCaption, aLink: String; aMarkup: TParseMode = pmDefault): String;
function ValidateUTF8(const S: String): String;

{ Like a tg://user?id=123456789 }
function UserLink(aUserID: Int64): String;

implementation

const
  MarkdownSpChars: array[0..3] of AnsiChar = ('\', '_', '*', '`');
  MarkdownSpCharsV2: array[0..17] of AnsiChar = ('_', '*', '[', ']', '(', ')', '~', '`', '>', '#', '+', '-', '=', '|',
    '{', '}', '.', '!');

  _tguserLink='tg://user?id=%d';

function MarkdownEscape(const S: String): String;
var
  a: AnsiChar;
begin
  Result:=S;
  for a in MarkdownSpChars do
    Result:=StringReplace(Result, a, '\'+a, [rfReplaceAll]);
end;

function MarkdownEscapeV2(const S: String): String;
var
  a: AnsiChar;
begin
  Result:=S;
  for a in MarkdownSpCharsV2 do
    Result:=StringReplace(Result, a, '\'+a, [rfReplaceAll]);
end;

function CaptionFromChat(aChat: TTelegramChatObj): String;
begin
  Result:=EmptyStr;
  with aChat do
  begin
    if First_name<>EmptyStr then
      Result+=First_name+' ';
    if Last_name<>EmptyStr then
      Result+=Last_name+' ';
    if Title<>EmptyStr then
      Result+=Title+' ';
    if Username<>EmptyStr then
      Result+='@'+Username;
  end;
  Result:=ValidateUTF8(Trim(Result));
end;

function CaptionFromUser(AUser: TTelegramUserObj): String;
begin
  Result:=EmptyStr;
  with AUser do
  begin
    if First_name<>EmptyStr then
      Result+=First_name+' ';
    if Last_name<>EmptyStr then
      Result+=Last_name+' ';
    if Username<>EmptyStr then
      Result+='@'+Username;
  end;
  Result:=Trim(Result);
end;

function BuildLink(const aCaption, aLink: String; aMarkup: TParseMode): String;
begin
  case aMarkup of
    pmMarkdown: Result:=Format('[%s](%s)', [aCaption, aLink]);
    pmHTML:     Result:=Format('<a href="%s">%s</a>', [aLink, aCaption]);
  else
    Result:=aLink;
  end;
end;

{ Hack function due of problem some utf8 entities }
function ValidateUTF8(const S: String): String;
begin
  Result:=UTF8Encode(UTF8Decode(S));
end;

function UserLink(aUserID: Int64): String;
begin
  Result:=Format(_tguserLink, [aUserID]);
end;

end.

