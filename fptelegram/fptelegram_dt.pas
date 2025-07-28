{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fptelegram_dt;

{$warn 5023 off : no warning about unused units}
interface

uses
  tgbot_dt, DTTelegramBot, receiverthread, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('DTTelegramBot', @DTTelegramBot.Register);
end;

initialization
  RegisterPackage('fptelegram_dt', @Register);
end.
