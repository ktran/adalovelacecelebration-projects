import os
import requests
import time
from slackclient import SlackClient
from donald_generator import DonaldGen
from FetchTweets import post_tweet, post_media
import random


slack_client = SlackClient(os.environ.get('SLACK_BOT_TOKEN'))

class TheDonald():
    # starterbot's ID as an environment variable
    def __init__(self):
        self.BOT_ID = os.environ.get("BOT_ID")
        self.donald = DonaldGen()
        self.reactionlist = ['dollar', 'coffeeparrot', 'flag-mx', 'flag-us', 'flag-ru', 'trumpparrot']
        self.last_response = ''

        # constants
        self.AT_BOT = "<@" + self.BOT_ID + ">"
        #COMMANDS = ["do", "gif", "help"]

        self.COMMANDS = {"do" : "Sure...write some more code then I can do that!", "dance" : ":donald:"  }
        # instantiate Slack & Twilio clients

    def get_echo(self, sentence):
        response = sentence
        return response

    def get_response(self, sentence):
        response = self.donald.reply(sentence)
        return response

    def help_commands(self):
        helper_string = ""
        for command in self.COMMANDS.keys():
            helper_string = helper_string+ " " + command

        return ("Some commands" + helper_string)


    def handle_command(self, command, channel):
        """
        Receives commands directed at the bot and determines if they
        are valid commands. If so, then acts on the commands. If not,
        returns back what it needs for clarification.
        """
        sentence = command.split(" ", 1)
        command = sentence[0]
        if command == "thoughts":
            response = self.get_response(sentence[1])
            if response: self.last_response = response
        elif command == "echo":
            response = self.get_echo(sentence[1])
        elif command == "gif":
            response = self.parse_gif()
            if response: self.last_response = response
        elif command in self.COMMANDS.keys():
            response = self.COMMANDS[command]
        elif command == "help":
            response = self.help_commands()
        elif command == 'tweet' and self.last_response:
            try:
                if 'http' in self.last_response:
                    post_media(self.last_response, '#realDonaldTrump')
                else:
                    post_tweet(self.last_response  + ' #realDonaldTrump')
                self.last_response = ''
            except:
                pass

            response = "MAKING TWITTER GREAT AGAIN!!"
        else:
            response = "You are fired!!! :angry:"

        if response:
            slack_client.api_call("chat.postMessage", channel=channel,
                                  text=response, as_user=True)

    def handle_normal_text(self, text, channel):
        response = self.get_response(text)
        if response:
            slack_client.api_call("chat.postMessage", channel=channel,
                                  text=response, as_user=True)
            self.last_response = response



    def handle_reaction(self, timestamp, channel):
        slack_client.api_call("reactions.add", channel=channel, name=random.choice(self.reactionlist), timestamp=timestamp)



    def parse_gif(self):
        data = requests.get("https://api.giphy.com/v1/gifs/random?api_key="+ GIPHY_KEY "&tag=trump&rating=PG-13")
        preurl = data.text.split(',')[9]
        url = preurl.split(':', 1)[1]
        url = url.translate({ord(c): None for c in '\'\"\\'})

        return url

    def parse_slack_output(self, slack_rtm_output):
        """
        The Slack Real Time Messaging API is an events firehose.
        this parsing function returns None unless a message is
        directed at the Bot, based on its ID.
        """
        output_list = slack_rtm_output
        if output_list and len(output_list) > 0:
            for output in output_list:
                if output and 'text' in output and self.AT_BOT in output['text']:
                # return text after the @ mention, whitespace removed
                    return True, output['text'].split(self.AT_BOT)[1].strip().lower(), \
                        output['channel'], output['ts']
                elif output and 'text' in output:
                    if 'user' in output and output['user'] != self.BOT_ID:
                        return False, output['text'].strip().lower(), output['channel'], output['ts']
                    else:
                        continue

        return None, None, None, None

if __name__ == "__main__":
    READ_WEBSOCKET_DELAY = 1 # 1 second delay between reading from firehose
    thisBot = TheDonald()
    if slack_client.rtm_connect():
        print("StarterBot connected and running!")

        while True:
            is_command, command, channel, timestamp = thisBot.parse_slack_output(slack_client.rtm_read())

            if is_command and command and channel:
                thisBot.handle_command(command, channel)
            elif not is_command and command and channel:
                thisBot.handle_normal_text(command, channel)
                thisBot.handle_reaction(timestamp, channel)
            time.sleep(READ_WEBSOCKET_DELAY)
    else:
        print("Connection failed. Invalid Slack token or bot ID?")
