##### script 5 for preparing Chimp&See videos for upload to Zooniverse
#extract one frame per second
#this is not necessary but C&S shows the clip along with still images


#this is a general call
#"path/ffmpeg.exe" -i <input.mp4> -vf fps=1 output%02d.jpg -hide_banner


all.clips<-list.files("path", pattern="mp4", full.names=T)
length(all.clips)
#

all.clips<-data.frame(path=all.clips, size=NA)

#get output file names with no extension so the still number can be added at the end
clip.names<-strsplit(all.clips$path, split="/")
clip.names<-unlist(lapply(clip.names, function(x)x[4]))
clip.names<-substr(clip.names, start=1, stop=nchar(clip.names)-4)

#create output path
all.clips<-data.frame(input.clip=all.clips$path, output.path.no.ext=paste("path/", clip.names, sep=""))

nrow(all.clips)
#


start.time<-Sys.time()
for(i in 1:nrow(all.clips)){
	cat(i, " ", round(i/nrow(all.clips),4)*100, "\n")
	system(paste("path/ffmpeg.exe -i \"", all.clips$input.clip[i], "\" -vf fps=1 ", all.clips$output.path.no.ext[i],"_%02d.jpg -hide_banner", sep=""), show.output.on.console=F)
	flush.console()
}
end.time<-Sys.time()
end.time - start.time
#

########make sure all stills have a size, remove them if they don't
start.time<-Sys.time()
all.stills<-list.files("path")
length(all.stills)
#

stills<-data.frame(path=paste("path/", all.stills, sep=""), size=NA)

stills$size<-floor(file.size(stills$path)/1024)
sum(is.na(stills$size))
#
range(stills$size)
#
end.time<-Sys.time()
end.time - start.time
#


