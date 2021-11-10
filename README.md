# Spotify Data Science Project

- ## Project Structure

```
project
│   README.md
│   Report.pdf    
│
└───src
│   │   data-analysis.R
│   │   spotify_data_collection.py
│   
└───figures
    │   ...
│   
└───data
    │    dataset_variable_description.xlsx
    │    DislikedSongs.csv
    │    full_test_playlist.csv
    │    LikedSongs.csv
    │    songs.csv
```


- ## Project Description


> Summary of project report


The goal of this project is to analyse my spotify music with a data science driven approach. 
I will start by using spotify API to collect my data directly from my spotify account. I've organized the music into 2 playlists that will be the 2 datasets of the project. One 'liked-songs' playlist and one 'disliked-songs' playlist. I made multiple API calls to collect and then transform the data into the desired format I had in mind (added addtional variables).

- python script: src/spotify_data_collection.py

Using the datasets, I study the different variables that describe the music I love as well as the music I dislike. In order to get a better idea of each impact on the outcome of the music (like/dislike).
I apply those insights to build the best prediction model that predict if I like or not any songs that you give as input. 

- R scritp: src/data-analysis.R

More details in Report.pdf

