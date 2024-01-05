##### script 3 for preparing Chimp&See videos for upload to Zooniverse
#redo keyframes
#this is to make sure videos can be split at the desired timestamps

#example: ffmpeg -i <input> -c:v libx264 -x264-params keyint=15:scenecut=0 out.mp4

videos<-read.table("path/sitename_videos_to_split.txt", sep="\t", header=T, stringsAsFactors=F)
nrow(videos)
#

start.time<-Sys.time()
for(i in 1:nrow(videos)){
	cat(i, " ", round(i/nrow(videos),3)*100, "\n")
	file.parts<-unlist(strsplit(videos$path[i], split="/"))

    #this will vary based on your path
    #create the desired file name for the output file
    output.file.name<-paste(file.parts[4:6], collapse="_") 

	output.file.name<-unlist(strsplit(output.file.name, split=".", fixed=T))

	output.video<-paste("path/", output.file.name[1], "_keyframes.mp4", sep="")

	system(paste("path/ffmpeg.exe -i ", videos$path[i], " -c:v libx264 -x264-params keyint=15:scenecut=0 ", output.video, sep=""), show.output.on.console=F)

    flush.console()
}
end.time<-Sys.time()
end.time - start.time
#


