# -*- coding: utf-8 -*-
"""
Created on Wed Dec  2 19:42:13 2020

@author: Anonymous
"""

import pandas as pd 
from pandas import DataFrame
import spotipy 
sp = spotipy.Spotify() 
from spotipy.oauth2 import SpotifyClientCredentials 
cid ="######################" 
secret = "##########################" 
client_credentials_manager = SpotifyClientCredentials(client_id=cid, client_secret=secret) 
sp = spotipy.Spotify(client_credentials_manager=client_credentials_manager) 
sp.trace=False 

####### creating playlist dataframe with track varaibles (one iteration returns 100 tracks MAX)

playlist_name = "MGSC 401"
playlist_id = "1Lov8diS6axfhZVc6qEbCG"
#enter playlist name and ID
playlist = sp.user_playlist( playlist_name , playlist_id) 
songs = playlist["tracks"]["items"] 



ids = [] 
for i in range(len(songs)): 
    ids.append(songs[i]["track"]["id"]) 
    
data = sp.audio_data(ids) 

#missing varaible from playlist api call: artist, track name, popularity, year
#add the following variable to data df: track's name, artist, year release, popularity
for i in range(len(songs)):
    data[i].update(track_name=songs[i]["track"]["name"])
    artists=[]
    k = 0
    while k < len(songs[i]["track"]["artists"]):
        artists.append(songs[i]["track"]["artists"][k]["name"])
        k=k+1
    data[i].update(artist=artists)
    data[i].update(popularity=songs[i]["track"]["popularity"])
    data[i].update(year=songs[i]["track"]["album"]["release_date"].split('-')[0])
    data[i].update(explicit=songs[i]["track"]["explicit"])
    
    #adding genders to data of a track
    #take only the genres associated with the main artist
    artist_id = artist=songs[i]["track"]["artists"][0]["id"]
    artist_info = sp.artist(artist_id)
    if (artist_info['genres']):
        artist_genres = artist_info['genres'][0]
        data[i].update(genres=artist_genres)
 
df = pd.DataFrame(data)

###### end of creating playlist dataframe with track variables

likedSongs = pd.DataFrame(df)
likedSongs = pd.concat([likedSongs, df]).reset_index(drop=True)

likedSongs.to_csv(index=False, path_or_buf=r'C:\Users\Anonymous\OneDrive - McGill University\Fall 2020\MGSC 401\Final Project\LikedSongs.csv')


dislikedSongs = pd.DataFrame(df)
dislikedSongs = pd.concat([dislikedSongs, df]).reset_index(drop=True)

dislikedSongs.to_csv(index=False, path_or_buf=r'C:\Users\Anonymous\OneDrive - McGill University\Fall 2020\MGSC 401\Final Project\DislikedSongs.csv')

likedSongs = pd.read_csv(r'C:\Users\Anonymous\OneDrive - McGill University\Fall 2020\MGSC 401\Final Project\dataset\LikedSongs.csv')
dislikedSongs = pd.read_csv(r'C:\Users\Anonymous\OneDrive - McGill University\Fall 2020\MGSC 401\Final Project\dataset\DislikedSongs.csv')

songs_dataset = pd.concat([dislikedSongs, likedSongs]).reset_index(drop=True)
songs_dataset.to_csv(index=False, path_or_buf=r'C:\Users\Anonymous\OneDrive - McGill University\Fall 2020\MGSC 401\Final Project\dataset\songs.csv')

#droping all observations from the test dataset that are from before 1960
test_dataset = pd.read_csv(r'C:\Users\Anonymous\OneDrive - McGill University\Fall 2020\MGSC 401\Final Project\dataset\testing dataset\data.csv')
indexNames = test_dataset[ test_dataset['year'] < 1960 ].index
# Delete these row indexes from dataFrame
test_dataset.drop(indexNames , inplace=True)
test_dataset.reset_index(drop=True)
test_dataset.to_csv(index=False, path_or_buf=r'C:\Users\Anonymous\OneDrive - McGill University\Fall 2020\MGSC 401\Final Project\dataset\test_dataset.csv')
