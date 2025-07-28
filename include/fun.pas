unit fun;

{$mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, Main, tgtypes, tgsendertypes, RegExpr, BotMsgs;

type

  { TFunnyMsgs }

  TFunnyMsgs = class(TMainClass)
    function FunnyMessage(Message: string): string;
  end;

var
  FunnyMsgs: TFunnyMsgs;

implementation

{ TFunnyMsgs }

function TFunnyMsgs.FunnyMessage(Message: string): string;
var
  ExprID, i: integer;
const
  Patterns: array[1..2] of string = (
    '\b(?i)(OwO)\b',
    '(?i)(What\?|What\s+do\s+you\s+mean\?|Wht\s+do\s+u\s+mean\?|Idk|' +
    'I\s+didn''t\s+get\s+it|Idk\s+wut\s+u\s+mean|I\s+didn''t\s+understand|' +
    'I\s+don''t\s+understand|I\s+dunno|Dunno)');
begin
  Result := 'null';
  ExprID := 0;

  with TRegExpr.Create do
  try
    ModifierI := True;
    for i := Low(Patterns) to High(Patterns) do
    begin
      Expression := Patterns[i];
      if Exec(Message) then
      begin
        ExprID := i;
        Break;
      end;
    end;
    if Match[1] = '' then
      Exit('null');

    case ExprID of
      1: Exit(OWO_MSG);
      2: Exit(NOTGET_MSG);
      else
        Exit('null');
    end;
  finally
    Free;
  end;
end;

end.

