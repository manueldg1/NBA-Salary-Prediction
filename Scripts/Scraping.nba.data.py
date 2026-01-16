## scraping nba.com

import pandas as pd
import requests
pd.set_option('display.max_columns', None)
import time
import numpy as np

text_url = 'https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight='
years = ['2023-2024']
season_type = ['Regular Season']

headers  = {

    'Connection': 'keep-alive',
    'Accept': 'application/json, text/plain, */*',
    'x-nba-stats-token': 'true',
    'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36',
    'x-nba-stats-origin': 'stats',
    'Sec-Fetch-Site': 'same-origin',
    'Sec-Fetch-Mode': 'cors',
    'Referer': 'https://stats.nba.com/',
    'Accept-Encoding': 'gzip, deflate, br',
    'Accept-Language': 'en-US,en;q=0.9',
}

response = []
for i in years:
    for a in season_type:
        complete_url = text_url 
        response.append(requests.get(url = complete_url, headers = headers).json())

nba_df = []
for i in response:
    columns = i['resultSets'][0]['headers']
    player_list = i['resultSets'][0]['rowSet']
    year = i['parameters']['Season']
    season_type = i['parameters']['SeasonType']
    nba_df.append(pd.DataFrame(player_list, columns= columns))
    pd.DataFrame(player_list, columns= columns).to_csv(f'./planta_hustle_{year}_{season_type}.csv')

pd.DataFrame(player_list, columns= columns).to_csv(f"C:\\Users\\Domen\\Desktop\\Data science\\Exams\\Statistical learning\\RS_traditional_TOTALS.csv")

pd.DataFrame(player_list, columns= columns).head()