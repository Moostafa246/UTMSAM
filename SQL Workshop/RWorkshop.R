#Need to collect player data, teams, salary data, batting data, fielding and pitching data
library("RSQLite")
library("sqldf")
library("Lahman")
library("factoextra")

viewing = LahmanData

People = Lahman::People
Teams = Lahman::Teams
salaries = Lahman::Salaries
Batting = Lahman::Batting
Pitching = Lahman::Pitching
Fielding = Lahman::Fielding


#First grab a list of all players that changed teams in between 2015 and 2016 
#This is a list of players who are either free agents, or players who are willing to be traded by their
#respective orgs 

Players2015 = sqldf("SELECT s.yearID as yearID2015, t.teamID as teamID2015, p.playerID as playerID2015
                   
                   FROM Teams as t
                   
                   INNER JOIN salaries as s
                    ON t.teamID = s.teamID
                   
                   INNER JOIN People as p 
                    ON s.playerID = p.playerID
                    
                  WHERE t.yearID = 2015
                  AND s.yearID = 2015
                  ")

Players2016 = sqldf("SELECT s.yearID as yearID2016, t.teamID as teamID2016, p.playerID as playerID2016
                   
                   FROM Teams as t
                   
                   INNER JOIN salaries as s
                    ON t.teamID = s.teamID
                   
                   INNER JOIN People as p 
                    ON s.playerID = p.playerID
                    
                  WHERE t.yearID = 2016
                  AND s.yearID = 2016
                  ")

FreeAgents = sqldf("SELECT p1.playerID2015
                   
                   FROM Players2015 as p1
                   INNER JOIN Players2016 as p2
                    ON p1.playerID2015 = p2.playerID2016
                   
                   WHERE p1.teamID2015 != p2.teamID2016")


#In this scenario we are the LA Angels
#First lets consolidate a list of stats for "free agent players"

FreeAgentStats = sqldf("SELECT FA.playerID2015 as playerID, Bat.G, Bat.AB, Bat.R, Bat.H, Bat.X2B, Bat.X3B, 
                        Bat.HR, Bat.RBI, Bat.SB, Bat.CS, Bat.BB, Bat.SO, Bat.IBB, Bat.HBP, Bat.SH, 
                        Bat.SF, Bat.GIDP, 
                        
                       pitch.W, pitch.L, pitch.G, pitch.GS, pitch.CG, pitch.SHO, pitch.SV, pitch.IPouts,
                      pitch.H, pitch.ER, pitch.HR, pitch.BB, pitch.SO, pitch.BAOpp, pitch.ERA, pitch.IBB,
                      pitch.WP, pitch.HBP, pitch.BK, pitch.BFP, pitch.GF, pitch.R, pitch.SH, pitch.SF,
                      pitch.GIDP,

                        
                      field.POS
                        
                       FROM 
                       FreeAgents as FA
                       
                       LEFT JOIN Batting as Bat 
                        ON FA.playerID2015 = Bat.playerID
                        AND Bat.yearID = 2015
                        
                        LEFT JOIN Pitching as pitch 
                          ON FA.playerID2015 = pitch.playerID
                          AND pitch.yearID = 2015
                        
                        LEFT JOIN Fielding as field
                          ON FA.PlayerID2015 = field.playerID
                          AND field.yearID = 2015
                          
                       ")


#Now lets have stats of the angels 

AngelsStaffBat = sqldf("SELECT bat.playerID, Bat.yearID, Bat.G, Bat.AB, Bat.R, Bat.H, Bat.X2B, Bat.X3B, 
                        Bat.HR, Bat.RBI, Bat.SB, Bat.CS, Bat.BB, Bat.SO, Bat.IBB, Bat.HBP, Bat.SH, 
                        Bat.SF, Bat.GIDP
                        
                        FROM Batting as Bat

                        WHERE Bat.yearId = 2018
                          AND Bat.teamId = 'LAA'
                      
                    ")

AngelsStaffPitch = sqldf("SELECT pitch.playerID, pitch.W, pitch.L, pitch.G, pitch.GS, pitch.CG, pitch.SHO, pitch.SV, pitch.IPouts,
                      pitch.H, pitch.ER, pitch.HR, pitch.BB, pitch.SO, pitch.BAOpp, pitch.ERA, pitch.IBB,
                      pitch.WP, pitch.HBP, pitch.BK, pitch.BFP, pitch.GF, pitch.R, pitch.SH, pitch.SF,
                      pitch.GIDP

                         FROM Pitching as Pitch
                         
                         WHERE pitch.yearID = 2018 
                          AND pitch.teamID = 'LAA'
                         ")


AngelsStaffField = sqldf("SELECT field.playerID, field.POS

                         FROM Fielding as field
                         
                         WHERE field.yearID = 2018 
                          AND field.teamID = 'LAA'
                         ")

AngelsStaff = sqldf("SELECT *

                     FROM AngelsStaffBat as bat
                      LEFT JOIN AngelsStaffPitch as pitch
                        ON bat.playerID = pitch.playerID
                      
                      LEFT JOIN AngelsStaffField as field
                        ON bat.playerID = field.playerID
                     
                     ")

battersnewagent <- subset(FreeAgentStats, is.na(W))
pitchersnewagent <- subset(FreeAgentStats, !is.na(W))

#newagent_batters
battersnewagent_person<- data.frame(battersnewagent$playerID,battersnewagent$POS)
battersnewagent_person_POS <- apply(battersnewagent_person,1,paste,collapse="-")
battersnewagent_data <- battersnewagent[2:17]

#scaled
battersnewagent_scaled <- scale(battersnewagent_data)
battersnewagent_scaled
battersnewagent_data_dist <- dist(battersnewagent_data)
battersnewagent_data_dist

#calculate the number of clusters
#within sum squares
fviz_nbclust(battersnewagent_scaled, kmeans, method="wss") + labs(subtitle = "Elbow Method")
#k = 5

kmeans.out <- kmeans(battersnewagent_scaled, centers = 5, nstart = 1000)
print(kmeans.out)

#visualize the clustering algorithm
kmeans.clusters <- kmeans.out$cluster
rownames(battersnewagent_scaled) <- paste(battersnewagent_person_POS, 1:dim(battersnewagent)[1], sep = "_")
fviz_cluster(list(data=battersnewagent_scaled, cluster=kmeans.clusters))


#newagent_pitchers
pitchersnewagent_person<- data.frame(pitchersnewagent$playerID,pitchersnewagent$POS)
pitchersnewagent_person_POS <- apply(pitchersnewagent_person,1,paste,collapse="-")
pitchersnewagent_data <- pitchersnewagent[18:43]
pitchersnewagent_data

#scaled
pitchersnewagent_scaled <- scale(pitchersnewagent_data)
pitchersnewagent_scaled
pitchersnewagent_data_dist <- dist(pitchersnewagent_data)
pitchersnewagent_data_dist

#calculate the number of clusters
#within sum squares
fviz_nbclust(pitchersnewagent_scaled, kmeans, method="wss") + labs(subtitle = "Elbow Method")
#k = 6

kmeans1.out <- kmeans(pitchersnewagent_scaled, centers = 5, nstart = 1000)
print(kmeans1.out)

#visualize the clustering algorithm
kmeans1.clusters <- kmeans1.out$cluster
rownames(pitchersnewagent_scaled) <- paste(pitchersnewagent_person_POS, 1:dim(pitchersnewagent)[1], sep = "_")
fviz_cluster(list(data=pitchersnewagent_scaled, cluster=kmeans1.clusters))












