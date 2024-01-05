##### script 4 for preparing Chimp&See videos for upload to Zooniverse
#split videoes into 15-second clips

#if a video is over 60 seconds then the fourth clip will have extra time added to it (if the total length is less than 61 seconds)
#or another clip will be created (if the total length is 61+ seconds)

#this is a general call to ffmpeg to split a video from 0 to 15 seconds
#"path/ffmpeg.exe" -ss 0 -t 15 -i "path/input.mp4" -vf format=yuv420p -vf scale=-1:720:flags=lanczos -c:v libx264 "D:/Desktop/output.mp4"


keyframe.videos<-sort(list.files("path", full.names=T))
length(keyframe.videos)
#

#connect paths with durations
videos.to.split<-read.table("path/sitename_videos_to_split.txt", sep="\t", header=T, stringsAsFactors=F)

nrow(videos.to.split)
#
videos.to.split<-videos.to.split[order(videos.to.split$path),]
videos.to.split$path<-gsub(videos.to.split$path, pattern="\\\\", replacement="/")

#make sure the names match up
parts<-strsplit(videos.to.split$path, split="/")

parts<-unlist(lapply(parts, function(x){
    paste(x[4:6], collapse="_")
}))
parts<-substr(parts, start=1, stop=nchar(parts)-4)
videos.to.split$match.id<-parts

keyframe.match.ids<-strsplit(keyframe.videos, split="/")
keyframe.match.ids<-unlist(lapply(keyframe.match.ids, function(x){
    x[length(x)]
}))
keyframe.match.ids<-strsplit(keyframe.match.ids, split="_")
keyframe.match.ids<-unlist(lapply(keyframe.match.ids, function(x){
    paste(x[1:(length(x)-1)], collapse="_")
}))

videos.to.split<-videos.to.split[videos.to.split$match.id %in% keyframe.match.ids,]
nrow(videos.to.split)
#
all.equal(videos.to.split$match.id, keyframe.match.ids)
#TRUE
videos.to.split$keyframe.path<-keyframe.videos


#Split the videos into 15-second splices that are mp4s, make them playable by dumb viewers, and set them to height=720p if they aren’t already
#save output videos as .mp4

(loop.start.time<-Sys.time())
for(i in 1:nrow(videos.to.split)){
	cat(i, ": ", round(i/nrow(videos.to.split)*100,2), "%\n", sep="")
	dur<-videos.to.split$duration[i]
	input.file<-videos.to.split$keyframe.path[i]

	#make the output mp4 files the same name as the input files minus "_keyframes"
	output.file.parts<-unlist(strsplit(videos.to.split$path[i], split="/", fixed=T))

	output.file.name<-paste(output.file.parts[4:6], collapse="_")
	
    output.file.name<-unlist(strsplit(output.file.name, split=".", fixed=T))
	output.file.name<-output.file.name[1]
	
    output.path<-paste("path", output.file.name, sep="/")
	
	#determine how many 15-second or less sections will be created
	num.full.clips<-floor(dur/15)
	partial.clip.dur<-dur %% 15
		
	################################################################
	#if the partial clip is enough to be its own clip
	if(partial.clip.dur>=1){
		for(j in 1:(num.full.clips+1)){
			start.time<-(j-1)*15
			output.file.mp4<-paste(output.path, "_", (j-1)*15, ".mp4", sep="")			
	
			#if the clip is prior to the last one
			if(j<=num.full.clips){				
				#to save the output as an mp4
				cat(paste(i, j, sep="-"), "\n")
				system(paste("path/ffmpeg.exe -ss ", start.time, " -t ", 15, " -i ", input.file, " -vf format=yuv420p -vf scale=-1:720:flags=lanczos -c:v libx264 ", output.file.mp4, sep=""), show.output.on.console=F) 
				flush.console()
							
			#if the clip is the last one
			} else {				
				cat(paste(i, j, sep="-"), "\n")
				#to save the output as an mp4
                system(paste("path/ffmpeg.exe -ss ", start.time, " -t ", partial.clip.dur, " -i ", input.file, " -vf format=yuv420p -vf scale=-1:720:flags=lanczos -c:v libx264 ", output.file.mp4, sep=""), show.output.on.console=F)
				flush.console()

			}
		}
		
	##################################################################	
	#if the partial clip is more than 0 but less than 1 second, add it to the final full clip
	} else if(partial.clip.dur>0){
		for(j in 1:num.full.clips){
			start.time<-(j-1)*15
			output.file.mp4<-paste(output.path, "_", (j-1)*15, ".mp4", sep="")
		
			#if the clip is prior to the last one
			if(j<num.full.clips){
				cat(paste(i, j, sep="-"), "\n")
				#to save the output as an mp4
                system(paste("path/ffmpeg.exe -ss ", start.time, " -t ", 15, " -i ", input.file, " -vf format=yuv420p -vf scale=-1:720:flags=lanczos -c:v libx264 ", output.file.mp4, sep=""), show.output.on.console=F) 
				flush.console()
				
			#if the clip is the last one
			} else {
				cat(paste(i, j, sep="-"), "\n")
				#to save the output as an mp4
                system(paste("path/ffmpeg.exe -ss ", start.time, " -t ", 15+partial.clip.dur, " -i ", input.file, " -vf format=yuv420p -vf scale=-1:720:flags=lanczos -c:v libx264 ", output.file.mp4, sep=""), show.output.on.console=F)
				flush.console()
			}
		
		}
	
	##################################################################
	#if the partial clips is 0, then just do the full clips
	} else{
		for(j in 1:num.full.clips){
			cat(paste(i, j, sep="-"), "\n")
			start.time<-(j-1)*15
			output.file.mp4<-paste(output.path, "_", (j-1)*15, ".mp4", sep="")
			
			#to save the output as an mp4
            system(paste("path/ffmpeg.exe -ss ", start.time, " -t ", 15, " -i ", input.file, " -vf format=yuv420p -vf scale=-1:720:flags=lanczos -c:v libx264 ", output.file.mp4, sep=""), show.output.on.console=F)
			flush.console()
		}
	}
	
}

(loop.end.time<-Sys.time())
loop.end.time - loop.start.time
#


