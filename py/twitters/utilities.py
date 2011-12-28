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


def get_relationship(config_fname, screen_name):
    config = get_config(config_fname)
    api    = get_twitter_client(config)
    return api.showFriendship(target_screen_name=screen_name)


def doifollow(config_fname, screen_name):
    rel = get_relationship(config_fname, screen_name)
    try:
        return rel['relationship']['source']['following']
    except:
        return False


def followsme(config_fname, screen_name):
    rel = get_relationship(config_fname, screen_name)
    try:
        return rel['relationship']['source']['followed_by']
    except:
        return false


def get_friends(config_fname, screen_name=None, cursor=None):
    config = get_config(config_fname)
    api    = get_twitter_client(config)

    if cursor is None or cursor == unicode('0'):
        if screen_name:
            friends = api.getFriendsIDs(screen_name=screen_name)
        else:
            friends = api.getFriendsIDs()
    else:
        if screen_name:
            friends = api.getFriendsIDs(screen_name=screen_name, cursor=cursor)
        else:
            friends = api.getFriendsIDs(cursor=cursor)

    if friends and friends['next_cursor_str'] != unicode('0'):
        return friends['ids'] + get_friends(config_fname, screen_name, 
            friends['next_cursor_str'])
    elif friends:
        return friends['ids'] 
    else:
        return []


def friends_incommon(config_fname, screen_name):
    my_friends = get_friends(config_fname)
    other_friends = get_friends(config_fname, screen_name)
    return [x for x in my_friends if x in other_friends]


def get_followers(config_fname, screen_name=None, cursor=None):
    config = get_config(config_fname)
    api    = get_twitter_client(config)

    if cursor is None or cursor == unicode('0'):
        if screen_name:
            followers = api.getFollowersIDs(screen_name=screen_name)
        else:
            followers = api.getFollowersIDs()
    else:
        if screen_name:
            followers = api.getFollowersIDs(screen_name=screen_name, 
                cursor=cursor)
        else:
            followers = api.getFollowersIDs(cursor=cursor)

    if followers and followers['next_cursor_str'] != unicode('0'):
        return followers['ids'] + get_followers(config_fname, screen_name, 
            followers['next_cursor_str'])
    elif followers:
        return followers['ids'] 
    else:
        return []


def followers_incommon(config_fname, screen_name):
    my_followers = get_followers(config_fname)
    other_followers = get_followers(config_fname, screen_name)
    return [x for x in my_followers if x in other_followers]


def get_unfollowers(config_fname, screen_name=None):
    friends = get_friends(config_fname, screen_name)
    followers = get_followers(config_fname, screen_name)
    return [x for x in friends if x not in followers]

