import json
from twython import Twython


def get_config(fname):
    config = open(fname, "r").read()
    return json.loads(config)


def get_twitter_client(config=None):
    if config:
        return Twython(twitter_token=config.get("CONSUMER_KEY"), 
                      twitter_secret=config.get("CONSUMER_SECRET"), 
                         oauth_token=config.get("ACCESS_TOKEN"), 
                  oauth_token_secret=config.get("ACCESS_TOKEN_SECRET"))
    else:
        return Twython()


def doifollow(config_fname, screen_name):
    config = get_config(config_fname)
    api    = get_twitter_client(config)
    rel    = api.showFriendship(target_screen_name=screen_name)
    try:
        return rel['relationship']['source']['following']
    except:
        return False
