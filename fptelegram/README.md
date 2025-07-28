# fp-telegram #

Library for working with Telegram bots API in FreePascal/Lazarus

FreePascal wrapper classes for Telegram bot's API. You can use it in your own working projects. Please join if you want to help of the development this library.

The library **does not use any third-party libraries**, besides the built-in FPC. You can use it both independently and together with other libraries, 
for example, as a [plug-in](https://github.com/Al-Muhandis/brook-telegram/) to Brook-framework/Brook4FreePascal

# Design-time

Library has two packages: run-time (`fptelegram.lpk`) and design-time (`fp-telegram_dt.lpk`). `fp-telegram_dt.lpk` contains component  `DTLongPolBot`. 
It is ready-made longpolling bot which you can use for the rapid developments of longpolling telegram bots. 
Look about longpolling [here](https://github.com/Al-Muhandis/fp-telegram/wiki/How-to-step-by-step.-Creation-telegram-bot-in-Lazarus-(longpolling)).
This component ca be used in GUI and non-GUI applications, daemons and services and 
even in web-server (however, in the latter case, it is preferable to use a webhook mechanism to receive updates. 
About webhook is [here](https://github.com/Al-Muhandis/fp-telegram/wiki/How-to-step-by-step.-Creation-telegram-bot-in-Lazarus-(webhook))

# Examples
You can find in the `examples` folder various architectures for using the library: 
long polling and webhook, 
single-threaded and multithreaded, 
using the designtime component and runtime classes
