Add some data so that you can test the telegram bots API wrapper
``` INI

[Bot]
;; Please specify bot token below!
Token=

[API]
;; Do not pay attention to this - it is needs for countries where telegram is blocked. (You can specify custom telegram endpoint)
;; Endpoint=
[Chat]
;; Enter Your user/chat ID
ID=

;; The section for payments tests
[Payment]

;; Get from the @BotFather
ProviderToken=
;; For example, USD or USD
Currency=USD
;; For example, $
PricePortionLabel=
;; for example: 10000, that is 100.00 [dollars/rub/etc]
PricePortionAmount=10000

[Send]
;; Url link to [short] video file (for example, http://sample.com/video.mp4)
videourl=
;; Url for setWebhook test
url=
;; Video filename for sendVideoBFileName
VideoFile=
;; Photo filename for sendPhotoByFileName
PhotoFile=


;; Proxy support is implemented only for the synapse HTTP Client broker. Not yet implemented in the native FPHTTPClient. 
;; You can specify HTTP proxy via Host/Port/UserName/Password name-value strings or via the Uri.
[Proxy]
Host=
Port=
Username=
Password=
Uri=
;;Uri=username:password@host:port
```
In order to test receiving updates via longpolling, send a message or other \[action\] to the bot just before testing (procedure `TTestReceiveLongPolling.Receive`).

In order to test payments wiring, run sendInvoice test. Test the payment via telegram than run the ReceivePreCheckOutQuery (You must run the test during 10 seconds after the payment test!).
The program will send answerPreCheckOutQuery as soon as it receives PreCheckOutQuery update from telegram API. 
At the end of the wiring the telegram API will send a successfulpayment update, the reception of which you can also run using ReceiveSuccessfulPayment test
