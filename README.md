# WorldWide Brotherhood Telegram BOT

This very simple BOT add resources to make it easier to communicate between people from different countries, like Imperial/Metric bidirectional conversion and currency conversion! In the future more resources may be implemented.

## How to use it

You need to create a BOT in [BotFather](https://t.me/botfather). Afther that, you need to compile the program for your operating system.
* If you are on Windows, put the EXE file in a dedicated folder. And then, run it by double-clicking (it will open on Command Prompt).
* If you are on Linux, you can put in ~/.local/bin and run through terminal anytime with `impmetbot` command!

Then, it will ask you to put the generated token, paste it and then your BOT will be working!

**Note:** It will only ask to put tokens in the first run, after that it will create an .ini file containing the necessary tokens.

## Recommended stuff to do:

On Linux, you may wanna use `screen` to run the BOT, so you will not have an annoying open terminal window everytime. 
* Install the package `screen` for your distro and, after putting the BOT binary file in `~/.local/bin` you can run this command:
  ```sh
  screen -dmS your-screen impmetbot
  ```
  You can change `your-screen` for the name you want. So when you wanna see the program running, you type on terminal: 
  ```sh
  screen -r your-screen
  ```
  You can make a custom shortcut for it if you don't wanna type the entire command everytime to see its log, so create a shortcut with this command if you use gnome-terminal:
  ```sh
  gnome-terminal -- bash -c "screen -r your-screen; exec bash"
  ```
  

* If the BOT closes after sometime idle, you may make a script to keep it restarting everytime it closes.
   * **Linux:** create a SH file (that you can also put in `~/.local/bin`) with this content:
     ```sh
     #!/bin/bash

     while true; do
      impmetbot
      echo "BOT stopped, restarting..."
     done
     ```
  * **Windows:** Create a BAT file with this content:
    ```batch
    @echo off
    
    :loop
    impmetbot.exe
    echo BOT stopped, restarting...
    goto loop
    ```    

> [!TIP]
> You can change the token anytime by running the program with `impmetbot -u` or `impmetbot --update-token` through Windows CMD or Linux teminal. Try `-h` or `--help`to see all the parse parameters avaliable.
