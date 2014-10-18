module CommandList (commands) where

import Command.Attach
import Command.Categories
import Command.Envelope
import Command.Help
import Command.Init
import Command.Migrate
import Command.Pull
import Command.Rule
import Command.Summary
import Command.Transfer
import CommandType


commands :: [Command]
commands = [ Command { name="init"
                     , function=initCommand
                     , docSummary="Initialize manila project in the current directory"
                     , doc="manila init\n\nInitialize manila project in the current directory."
                     }
           , Command { name="migrate"
                     , function=migrateCommand
                     , docSummary="Migrate the database to a newer version of manila"
                     , doc="manila migrate\n\nMigrate the database to a newer version of manila."
                     }
           , Command { name="help"
                     , function=helpCommand
                     , docSummary="Print this list"
                     , doc="manila help\n\nPrint this list."
                     }
           , Command { name="pull"
                     , function=pullCommand
                     , docSummary="Download and import data from Mint.com"
                     , doc="manila pull\n\nDownload and import transactions and account balances from Mint.com."
                     }
           , Command { name="summary"
                     , function=summaryCommand
                     , docSummary="Summary of envelope and account balances"
                     , doc="manila summary\n\nSummary of envelope and account balances."
                     }
           , Command { name="envelope"
                     , function=envelopeCommand
                     , docSummary="Add an envelope"
                     , doc="manila envelope <name>\n\nAdd an envelope. Use the 'summary' command to see current envelopes."
                     }
           , Command { name="transfer"
                     , function=transferCommand
                     , docSummary="Transfer money to an envelope"
                     , doc="manila transfer <amount> <envelope>\n\nTransfer money to an envelope. Use a negative amount to transfer from envelope."
                     }
           , Command { name="attach"
                     , function=attachCommand
                     , docSummary="Attach Mint.com category to envelope"
                     , doc="manila attach <envelope> <category>\n\nAttach Mint.com category to envelope. This means that all transactions from <category> will come out of (or go into) <envelope>."
                     }
           , Command { name="categories"
                     , function=categoriesCommand
                     , docSummary="List the categories that are attached to an envelope"
                     , doc="manila categories\n\nList the categories that are attached to an envelope."
                     }
           , Command { name="rule"
                     , function=addRuleCommand
                     , docSummary="Add a rule"
                     , doc="manila rule -c <envelope> <category> <percentage> <amount> [start_date]\nmanila rule -t <envelope> <frequence> <amount> [start_date]\n\nAdd a rule."
                     }
           , Command { name="rules"
                     , function=listRulesCommand
                     , docSummary="List all rules"
                     , doc="manila rules\n\nList all rules."
                     }
           ]
