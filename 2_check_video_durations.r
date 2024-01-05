##### script 2 for preparing Chimp&See videos for upload to Zooniverse
#check video durations
#this is to remove videos that have no duration for whatever reason as they can't be processed

durs<-read.table("path/sitename_video_lengths.csv", sep=",", header=T)
durs<-durs[,2:3]
colnames(durs)[1]<-"duration"
nrow(durs)
#

#how many videos had errors
sum(is.na(durs$duration))
#

range(durs$duration, na.rm=T)
#


################
#remove the videos that don't have a duration because something is wrong with it, plus videos under 1 second long

vids.to.split<-durs[!is.na(durs$duration) & durs$duration>=1,]
nrow(vids.to.split)
#
vids.to.split$path<-gsub(vids.to.split$path, pattern="\\\\", replacement="/")


write.table(vids.to.split, "path/sitename_videos_to_split.txt", sep="\t", row.names=F, col.names=T)


