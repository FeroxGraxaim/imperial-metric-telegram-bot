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

Then, you need to create a BOT in [BotFather](https://t.me/botfather). Afther that, you need to compile the program for your operating system.
* If you are on Windows, put the EXE file in a dedicated folder. And then, run it by double-clicking (it will open on Command Prompt).
* If you are on Linux, you can put in ~/.local/bin and run through terminal anytime!

Then, it will ask you to put the generated token, paste it and then your BOT will be working!

> [!TIP]
> You can change the token anytime by running the program with `impmetbot -u` or `impmetbot --update-token` through Windows CMD or Linux teminal. Try `-h` or `--help`to see all the parse parameters avaliable.
