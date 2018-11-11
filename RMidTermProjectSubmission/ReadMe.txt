PART 1: DATA WRANGLING.

There are two files: RegularSeasonDetailedResults.csv contains the results of all NCAA regular season basketball games from 2003-2017. TourneyCompactResults.csv contains tournament results from 2003-2016.  Your first task will be to use the first file to create a dataset that contains for each team and each season and list of team’s overall statistics for that season. So each row in this dataset should contain a column for the season, a column for the team number, and then columns for the team’s statistics for that season (only do offensive statistics – i.e., do not include their opponents’ numbers).

 

 PART 2: CLUSTERING.

Your next task is to do a cluster analysis on the season statistics. I recommend you exclude the season and team number for this step. You can use K-means or H-Clustering. Try to find an optimal number of clusters for this data.

 

PART 3: REGRESSION.

Your final task is to build a model that will predict how many points a team will score in its game in the tournament. To do this you will need to create a dataset that includes the season, the team number, their statistics for that season, and the points they scored in the game of the tournament. Using this dataset, you should be able to use the regression techniques we have discussed to build a model. Use cross-validation to test your model’s accuracy.
