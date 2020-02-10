
# Load packages
library(twitteR)
library(httpuv)
library(curl)
library(openssl)

# Define parameters
# E is number of most recent ego tweets
# A is number of most recent alter tweets

# Number of ego tweets to be pulled
E = 100

# Number of alter tweets to be pulled
A = 100

# Get authorization from Twitter
setup_twitter_oauth("CONSUMERKEY","CONSUMERSECRET", "ACCESSTOKEN", "ACCESSTOKENSECRET")

# Load table of Twitter handles
handles <- as.matrix(read.csv("handles.csv", header=FALSE))
handlesD <- dim(handles)[1]

# Keep track of number of calls of Twitter API
callsUT = 0
callsRT = 0
callsGU = 0

# Set matrices for network data
posts <- matrix(0,0,6)
ptext <- matrix(0,0,2)
people <- matrix(0,0,8)
edges <- matrix(0,0,4)

# Set matrix to check retweeters captured
check <- matrix(0,0,5)

# Set minimum and maximum Tweet IDs
SID <- 760836150760050689
MID <- 795954831718498305

# Set variable for error message
error <- "Error in twInterface"

for(h in 1:handlesD)
{
	# Check to make sure limit on Timeline calls has not been reached
	if(callsUT == 900)
	{
		Sys.sleep(900)
		callsUT = 0
	}
	
	# Pull most recent E tweets from ego Timeline
	ego_raw <- try(userTimeline(handles[h,2], n=E, maxID = MID, sinceID = SID, includeRts=FALSE, excludeReplies=TRUE))
	
	# Update number of Timeline calls
	callsUT = callsUT + 1
	
	if(length(ego_raw)!=0 & substr(ego_raw[1],1,20) != error)
	{
		ego <- twListToDF(ego_raw)
		
		# Calculate effective number of most recent ego tweets
		# (in case this is less than E)
		egoD <- dim(ego)[1]
		
		# Collect retweeters for all ego tweets
		for(i in 1:egoD)
		{
			egoID <- ego[i,"id"]
			egoTX <- ego[i,"text"]
			egoRT <- ego[i,"retweetCount"]
			egoFC <- ego[i,"favoriteCount"]
			egoCR <- ego[i,"created"]
			
			newpost <- cbind(handles[h,1],egoID,egoRT,egoFC,egoCR,"egoPost")
			posts <- rbind(posts,newpost)
			
			newtext <- cbind(handles[h,1],egoTX)
			ptext <- rbind(ptext,newtext)
			
			if(egoRT > 0)
			{
				if(callsRT == 75)
				{
					Sys.sleep(900)
					callsRT = 0
				}
				
				egoRTers <- retweeters(egoID,n=100)
				callsRT = callsRT + 1
				
				newcheck <- cbind(handles[h,1],0,i,egoRT,length(egoRTers))
				check <- rbind(check,newcheck)
				
				newpeople <- cbind(t(t(egoRTers)),0,0,0,0,0,0,"egoRetweeter")
				people <- rbind(people,newpeople)
				
				newedges <- cbind(handles[h,1],egoID,t(t(egoRTers)),"egoPost/egoRetweeter")
				edges <- rbind(edges,newedges)
			}
		}
	}
}

people <- unique(people)
peopleD <- dim(people)[1]

write.table(posts, file = "posts.csv", sep = ",", col.names = FALSE, row.names = FALSE)
write.table(ptext, file = "ptext.csv", sep = ",", col.names = FALSE, row.names = FALSE)
write.table(edges, file = "edges.csv", sep = ",", col.names = FALSE, row.names = FALSE)
write.table(check, file = "check.csv", sep = ",", col.names = FALSE, row.names = FALSE)

for(j in 1:peopleD)
{
	if(callsUT == 900)
	{
		Sys.sleep(900)
		callsUT = 0
	}
	
	alter_raw <- try(userTimeline(people[j,2], n=A, maxID = MID, sinceID = SID, includeRts=FALSE, excludeReplies=TRUE))
	callsUT = callsUT + 1

	if(callsGU == 900)
	{
		Sys.sleep(900)
		callsGU = 0
	}
	
	alter_draw <- try(getUser(people[j,2]))
	callsGU = callsGU + 1
	
	if(length(slotNames(alter_draw))!=0)
	{
		people[j,2] <- alter_draw$statusesCount
		people[j,3] <- alter_draw$followersCount
		people[j,4] <- alter_draw$favoritesCount
		people[j,5] <- alter_draw$friendsCount
		people[j,6] <- alter_draw$created
		people[j,7] <- alter_draw$verified
	}
	
	write.table(t(people[j,]), file = "people.csv", sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
	
	if(length(alter_raw)!=0 & substr(alter_raw[1],1,20) != error)
	{
		alter <- twListToDF(alter_raw)
		alterD <- dim(alter)[1]
		
		for(k in 1:alterD)
		{
			alterID <- alter[k,"id"]
			alterTX <- alter[k,"text"]
			alterRT <- alter[k,"retweetCount"]
			alterFC <- alter[k,"favoriteCount"]
			alterCR <- alter[k,"created"]
			
			newpost <- cbind(people[j,1],alterID,alterRT,alterFC,alterCR,"alterPost")
			write.table(newpost, file = "posts.csv", sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)

			newtext <- cbind(people[j,1],alterTX)
			write.table(newtext, file = "ptext.csv", sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
			
			newedges <- cbind(0,people[j,1],alterID,"egoRetweeter/alterPost")
			write.table(newedges, file = "edges.csv", sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
			
			if(alterRT > 0)
			{
				if(callsRT == 75)
				{
					Sys.sleep(900)
					callsRT = 0
				}
				
				callsRT = callsRT + 1
				
				alterRTers <- retweeters(alterID,n=100)
				
				newcheck <- cbind(people[j,1],j,k,alterRT,length(alterRTers))
				write.table(newcheck, file = "check.csv", sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
				
				newpeople <- cbind(people[j,1],t(t(alterRTers)),0,0,0,0,0,0,"alterRetweeter")
				write.table(newpeople, file = "people.csv", sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
				
				newedges <- cbind(people[j,1],alterID,t(t(alterRTers)),"alterPost/alterRetweeter")
				write.table(newedges, file = "edges.csv", sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
			}
		}
	}
}
