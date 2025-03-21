unit BotMsgs;

{$mode ObjFPC}{$H+}

interface

const
  START_MSG = 'Hello! I''m WordWide Brotherhood BOT!' + #13#10 + #13#10 +
              'I can convert between imperial and metric mesurements, and ' +
              'also convert currency! You may add me to a group that has ' +
              'friends from different countries, so I will make your ' +
              'communication better! ' + #13#10 +
              'Type /help to see avaliable commands and resources!';

  HELP_MSG = '*Help Message*' + #13#10 + '/help: Shows this help.' + #13#10 +
             '/currency: Used to convert currency. You can reply to a message ' +
             'that has a currency in the format [value] [currency] (Ex: 2.00 ' +
             'BRL) and type the currency you wanna convert to.' + #13#10 +
             'Example: /currency BRL' + #13#10 + 'You can also ask to convert ' +
             'a specific value without needing to reply to another message.' +
             #13#10 + 'Example: `/currency USD BRL 25.00`' + #13#10 + 'Use ' +
             '/help to see this again.' + #13#10 + #13#10 + 'Convertions ' +
             'between imperial and metric mesurements are automatic, you just ' +
             'type a value with its identifier (Ex: 35Kg) and I will convert ' +
             'it for you!';          

  ERR_MSG = 'Sorry, there was a problem and I could not convert this currency ' +
            'for you :c' + #13#10  + 'Try again, if problem persists tell ' +
            '@PampasFox to solve this.';

  NOREP_MSG = 'Reply currency not detected! If it''s not in ISO format (Ex: ' +
              'not being BRL, USD, etc), try to convert manually without ' +
            'replying to the message (Ex: /currency USD BRL [value]). ' +
            'Talk to @PampasFox for he to add this monetary format in me.';

  NOPARAM_MSG = 'You need to specify the parameters for this command!' + #13#10
                + 'You can reply to a message with a currency value like ' +
                'this: "25 USD" with the command + the currency you want ' +
                '(Ex: /currency BRL' + #13#10 + 'Or you can just type the ' +
                'command + original currency + currency you want (Ex: ' +
                '/currency USD BRL 22)';

  RESULT_MSG = '%s is the same as %s';

implementation

end.

