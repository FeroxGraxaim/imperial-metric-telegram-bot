unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, IniPropStorage, ReceiveUpdatesThread;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    BtnStart: TButton;
    BtnStop: TButton;
    EdtToken: TLabeledEdit;
    InPrpStrg: TIniPropStorage;
    MmLog: TMemo;
    procedure BtnStartClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FReceiverThread: TReceiverThread;
    procedure BotAppendMessage(const aMsg: String);
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.lfm}

uses
  tgsendertypes, tgfclhttpclientbroker
  ;

{ TFrmMain }

procedure TFrmMain.BtnStartClick(Sender: TObject);
begin
  if EdtToken.Text=EmptyStr then
  begin
    ShowMessage('Please, enter telegram token!');
    Exit;
  end;
  BtnStart.Enabled:=False;
  FReceiverThread:=TReceiverThread.Create(EdtToken.Text);
  FReceiverThread.OnAppendMessage:=@BotAppendMessage;
  FReceiverThread.Start;
  BtnStop.Enabled:=True;
end;

procedure TFrmMain.BtnStopClick(Sender: TObject);
begin
  BtnStop.Enabled:=False;
  FReceiverThread.Terminate;
  FReceiverThread.WaitFor;
  FreeAndNil(FReceiverThread);
  BtnStart.Enabled:=True;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FReceiverThread) then
  begin
    FReceiverThread.Terminate;
    FReceiverThread.WaitFor;
    FreeAndNil(FReceiverThread);
  end;
end;

procedure TFrmMain.BotAppendMessage(const aMsg: String);
begin
  MmLog.Lines.Add(aMsg);
end;

initialization

end.

