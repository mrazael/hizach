import json
import re
from datetime import datetime
import string


def pause():
    programPause = input("Press something to continue.")

def json_to_csv():
    subreddits = {}
    users = {}
    k = 1000000
    i = 0
    n = 0
    totalposts = 0
    totalkarma = 0

    with open('reddit.csv', 'w', newline = '') as reddit:
        with open('RC_2015-01', 'r', encoding = 'utf-8') as file:
            reddit.write('\t'.join(["created_utc", "author", "body", "score", "subreddit" + '\n']))
            for line in file:
                data = json.loads(line)
                data['body'] = data['body'].replace('\t', '').replace('\r', '').replace('\n', '').lower()
                if data['author'] != "[deleted]" and data['body'] != "[deleted]":
                    if re.findall(r'([Aa][Uu][Tt][O0o])|([Bb][O0o][Tt])|([Mm][O0o][Dd])|([Tt]ranscriber)', data['author']):
                        continue

                    else:

                        if data['subreddit'] in subreddits:
                            subdata = (subreddits[data['subreddit']][0]+1, subreddits[data['subreddit']][1] + int(data['score']))
                            subreddits[data['subreddit']] = subdata
                            #print("Subreddit", subreddits[data['subreddit']][1])
                        if data['author'] in users:
                            #print("Author", users[data['author']][1])
                            userdata = (users[data['author']][0]+1, users[data['author']][1] + int(data['score']))
                            users[data['author']] = userdata
                        if data['subreddit'] not in subreddits:
                            subreddits[data['subreddit']] = (1, int(data['score']))
                        if data['author'] not in users:
                            users[data['author']] = (1, int(data['score']))
                        #
                        totalposts += 1
                        totalkarma += data['score']

                        # i += 1
                        # if i == 10000:
                        #      break

                        data['created_utc'] = datetime.utcfromtimestamp(int(data['created_utc'])).strftime('%Y-%m-%d %H:%M:%S')
                        reddit.write('\t'.join([data['created_utc'], data['author'], data['body'], str(data['score']), data['subreddit'] + '\n']))

                        n += 1
                        if k == n:
                            print(f'Processed {n} lines.')
                            k += 1000000

    print("Finished!")
    print(f"Processed {n} lines. Here are the summary statistics:")
    
    topsubs = [(k, v) for k, v in sorted(subreddits.items(), key=lambda item: item[1], reverse = True)]
    topusers = [(k, v) for k, v in sorted(users.items(), key=lambda item: item[1], reverse = True)]
    topkarma = [(k, v) for k, v in sorted(users.items(), key=lambda item: item[1][1], reverse = True)]
    
    groups = {"Superusers": (0, 0), "Contributors": (0, 0), "Lurkers": (0, 0)}
    
    subposts = 0
    subkarma = 0
    
    for i in range(16):
        subposts += topsubs[i][1][0]
        subkarma += topsubs[i][1][1]
    
    print("")
    print("Subreddits in total:", len(topsubs))
    print(f'Top 16 subreddits account for {format(subposts/totalposts*100, ".2f")}% of all posts and {format(subkarma/totalkarma*100, ".2f")}% of total karma')
    
    for i in range(len(topusers)):
        if i < round(len(topusers)*0.01):
            groups["Superusers"] = (groups["Superusers"][0] + topusers[i][1][0], groups["Superusers"][1] + topusers[i][1][1])
    
        elif i < round(len(topusers)*0.1):
            groups["Contributors"] = (groups["Contributors"][0] + topusers[i][1][0], groups["Contributors"][1] + topusers[i][1][1])
            #groups["Contributors"] += topusers[i][1][0]
    
        else:
            #groups["Lurkers"] += topusers[i][1][0]
            groups["Lurkers"] = (groups["Lurkers"][0] + topusers[i][1][0], groups["Lurkers"][1] + topusers[i][1][1])
    #####################
    
    print("Users in total:", len(topusers))
    print("")
    for index in groups:
        print(f'{index} have in total made {groups[index][0]} posts ({format(groups[index][0]/totalposts*100, ".2f")}% of all posts) and accumulated {groups[index][1]} karma ({format(groups[index][1]/totalkarma*100, ".2f")}% of total karma)')
    print("")
    ######################
    
    sampletopsubs = []
    
    print('================Top subreddits=========================')
    for i in range(16):
        sampletopsubs.append(topsubs[i][0])
        if i < 9:
            print(f'{"0" + str(i+1):02} {topsubs[i][0]: ^40} {str(topsubs[i][1][0]): ^5} {str(topsubs[i][1][1]): ^5}')
        else:
            print(f'{str(i+1)} {topsubs[i][0]: ^40} {str(topsubs[i][1][0]): ^5} {str(topsubs[i][1][1]): ^5}')
    
    print("")
    
    print('===================Top users==========================')
    for i in range(16):
        if i < 9:
            print(f'{"0" + str(i+1):02} {topusers[i][0]: ^40} {str(topusers[i][1][0]): ^5} {str(topusers[i][1][1]): ^5}')
        else:
            print(f'{str(i+1)} {topusers[i][0]: ^40} {str(topusers[i][1][0]): ^5} {str(topusers[i][1][1]): ^5}')
    
    ###################
    print("")
    # print("Systematic sample of 100 subreddits:")
    n = 0
    
    samplesubs = []
    
    while n <= len(topsubs):
        if topsubs[n][1][0] >= 1000:
            samplesubs.append(topsubs[n][0])
        n += round(len(topsubs)*0.001)
    
    print(samplesubs)
    print("")
    print(sampletopsubs)

# Lets get all the top 16 subs in one dataframe

def smallfiles():

    n = 0
    k = 1

    mainsubs = ["europe", "worldnews", "news", "AskReddit", "DebateReligion"]
    topsixteen = ['AskReddit', 'nfl', 'funny', 'leagueoflegends', 'pics', 'worldnews', 'DestinyTheGame', 'todayilearned', 'AdviceAnimals', 'videos', 'pcmasterrace', 'nba', 'SquaredCircle', 'hockey', 'news', 'CFB']
    sample = ['AskReddit', 'AskMen', 'rwbyRP', 'rupaulsdragrace', 'DebateReligion', 'Frugal', 'talesfromtechsupport', 'steroids', 'melbourne', 'GetMotivated', 'spikes', 'history', 'Coachella', 'jobs', 'mexico', 'MorbidReality', 'gardening', 'blog', 'community', 'fitnesscirclejerk', 'WWE', 'TrueAtheism', 'TalesFromYourServer', 'bostonceltics', 'PrivateFiction', 'saplings', 'freemasonry', 'gundeals', 'coins', 'Turkey', 'FallOutBoy', 'finance', 'HongKong', 'TheWire', 'Overwatch', 'peloton', 'MAA', 'BeautyBoxes', 'gaybrosgonemild', 'PerfectTiming', '52book', 'rccars', 'twinks', 'FarCry4', 'YouEnterADungeon', 'birthcontrol', 'ChronicPain', 'WebGames', 'MedievalEngineers', 'Comcast', 'bdsm', 'Hair', 'HaloPlayers', 'exo', 'autism', 'Ooer', 'latin', 'straya', 'FolkPunk', 'JuggaloTuggalo', 'BeardPorn', 'providence', 'ginger', 'ShittyLifeProTips', 'circojeca', 'awesome', 'Equestrian', 'changetip', 'nl_Kripparrian', 'MHOCPress']
    with open('top16.csv', 'w', newline = '') as top16, open('redditsyssample.csv', 'w', newline = '') as redditsample, open('mainsubs.csv', 'w', newline = '') as mains:
        with open('RC_2015-01', 'r', encoding = 'utf-8') as file:
            top16.write('\t'.join(["created_utc", "author", "body", "score", "subreddit" + '\n']))
            redditsample.write('\t'.join(["created_utc", "author", "body", "score", "subreddit" + '\n']))
            mains.write('\t'.join(["created_utc", "author", "body", "score", "subreddit" + '\n']))
            for line in file:
                data = json.loads(line)
                data['body'] = data['body'].replace('\r', '').replace('\n', '').replace('\t', '').lower()
                if data['author'] != "[deleted]" and data['body'] != "[deleted]":
                    if re.findall(r'([Aa][Uu][Tt][O0o])|([Bb][O0o][Tt])|([Mm][O0o][Dd])|([Tt]ranscriber)', data['author']):
                        continue

                    else:
                        #data['author'] = data['author'].translate(str.maketrans('', '', string.punctuation))
                        data['created_utc'] = datetime.utcfromtimestamp(int(data['created_utc'])).strftime('%Y-%m-%d %H:%M:%S')

                        if data['subreddit'] in topsixteen:
                            top16.write('\t'.join([data['created_utc'], data['author'], data['body'], str(data['score']), data['subreddit'] + '\n']))

                        if data['subreddit'] in sample:
                            redditsample.write('\t'.join([data['created_utc'], data['author'], data['body'], str(data['score']), data['subreddit'] + '\n']))

                        if data['subreddit'] in mainsubs:
                            mains.write('\t'.join([data['created_utc'], data['author'], data['body'], str(data['score']), data['subreddit'] + '\n']))

                    n += 1
                    if 8 ** k == n:
                        print(f'Processed {8 ** k} lines.')
                        k += 1

    print("Wrote 'top16.csv', 'redditsyssample.csv, and 'mainsubs.csv' into files.")
    print("Finished!")

json_to_csv()
