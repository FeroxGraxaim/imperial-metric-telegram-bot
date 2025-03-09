# Telegram Imperial-Metric Converter BOT (bi-directional)

This very simple BOT is able to convert imperial to metric mesurements and vice-versa automatically when someone write a mesurement in the chat (that can be in a group or DM).
Perfect for Telegram groups that have people from diverse countries!
It's developed in FreePascal, so you will need Lazarus and FPC to compile.

## How to use it

* Windows: you need to download the installer [here.](https://slproweb.com/products/Win32OpenSSL.html  )
* Linux: you need to install libssl-dev for Debian-based systems, openssl for Arch-based systems, or openssl-devel for RedHat-based distros:
  * `sudo apt install libssl-dev`
  * `sudo dnf install openssl-devel`
  * `sudo zypper install openssl-devel`
  * `sudo yum install openssl-devel`
  * `sudo pacman -S openssl`
Then, you need to create a BOT in [BotFather](https://t.me/botfather) and put the generated token in the const `TOKEN` in untMain.pas. In the last, compile and run!
