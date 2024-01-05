##### script 7 for preparing Chimp&See videos for upload to Zooniverse
#downsize all clips to between 900kb and 1MB, or less if clips are originally below 1MB

#the downsizing itself is done by running batch files
#each step will produce one batch file, which you will then run before moving on to the next step in this script

#where clips are located
all.clips<-list.files("path", pattern="mp4", full.names=T)
length(all.clips)
#

#get output file names with no extension so we can add "downsized"
clip.names<-strsplit(all.clips, split="/")
clip.names<-unlist(lapply(clip.names, function(x){x[4]}))
clip.names<-substr(clip.names, start=1, stop=nchar(clip.names)-4)

#get file sizes, figure out what to resize everything to based on original file size
start.time<-Sys.time()
file.sizes<-data.frame(video.name=all.clips, output.path.no.ext=paste("path/", clip.names, sep=""), video.size=ceiling(file.size(all.clips)/1024))
end.time<-Sys.time()
end.time - start.time
#

file.sizes$video.name<-as.character(file.sizes$video.name)
file.sizes$output.path.no.ext<-as.character(file.sizes$output.path.no.ext)

range(file.sizes$video.size)
#

sum(file.sizes$video.size<=1000)
#xxx clips don't need to be downsized

file.sizes<-file.sizes[order(file.sizes$video.size),]
rownames(file.sizes)<-1:nrow(file.sizes)


#######connect clip lengths and sizes
clip.lengths<-read.table("path/sitename_clip_lengths.csv", sep=",", header=T, stringsAsFactors=F)
nrow(clip.lengths)
#
clip.lengths$path<-gsub(clip.lengths$path, pattern="\\\\", replacement="/")
#get matchable id
path.parts<-strsplit(clip.lengths$path, split="/")

#this will vary based on your path
path.parts<-unlist(lapply(path.parts, function(x){x[4]}))

clip.lengths$match.id<-substr(path.parts, start=1, stop=nchar(path.parts)-4)

#get match id for file.sizes
fs.parts<-strsplit(file.sizes$output.path.no.ext, split="/")

file.sizes$match.id<-unlist(lapply(fs.parts, function(x){x[4]}))

#put length into file.sizes
file.sizes$duration<-clip.lengths$X0[match(file.sizes$match.id, clip.lengths$match.id)]


########get bitrate = size/duration
#  Use this rate control mode if you are targeting a specific output file size, and if output quality from frame to frame is of less importance. This is best explained with an example. Your video is 10 minutes (600 seconds) long and an output of 200 MiB is desired. Since bitrate = file size / duration:

#example calculation
# (200 MiB * 8192 [converts MiB to kBit]) / 600 seconds = ~2730 kBit/s total bitrate
# 2730 - 128 kBit/s (desired audio bitrate) = 2602 kBit/s video bitrate

#example formulation
# ffmpeg -y -i input -c:v libx264 -b:v 2600k -pass 1 -an -f null NUL && ^
# ffmpeg -i input -c:v libx264 -b:v 2600k -pass 2 -c:a aac -b:a 128k output.mp4

#our example formulation in windows with 48k for audio
# ffmpeg -y -i input -c:v libx264 -b:v 428k -pass 1 -c:a aac -b:a 48k -f null NUL && ^
# ffmpeg -i input -c:v libx264 -b:v 428k -pass 2 -c:a aac -b:a 48k output.mp4

file.sizes$bitrate<-floor((1*8000)/file.sizes$duration - 48) #total kb/s minus audio
colnames(file.sizes)[1:2]<-c("input.name","output.name")
file.sizes$output.name<-paste(file.sizes$output.name, "_downsized.mp4", sep="")

write.table(file.sizes, "path/sitename_file_sizes.txt", sep="\t", row.names=F, col.names=T)

file.sizes<-read.table("path/sitename_file_sizes.txt", sep="\t", header=T, stringsAsFactors=F)
nrow(file.sizes)
#

#############################################################################
#this first run of downsizing uses the bitrates calculated above
#this will only be done for clips that are originally over 1MB
#the clips less than 1MB will be copied over to join the downsized clips at the end
#note: this version is for windows

clips.to.downsize<-file.sizes[file.sizes$video.size>1000,]
nrow(clips.to.downsize)
#
batch.file.orig<-data.frame(command=rep(NA, times=nrow(clips.to.downsize)))

ffmpeg.path<-"path/ffmpeg.exe"

for(i in 1:nrow(batch.file.orig)){
	batch.file.orig$command[i]<-paste(ffmpeg.path, " -y -i ", clips.to.downsize$input.name[i], " -c:v libx264 -b:v ", clips.to.downsize$bitrate[i],"k -pass 1 -c:a aac -b:a 48k -f null NUL && ^", ffmpeg.path, " -y -i ", clips.to.downsize$input.name[i], " -c:v libx264 -b:v ", clips.to.downsize$bitrate[i], "k -pass 2 -c:a aac -b:a 48k ", clips.to.downsize$output.name[i], sep="")
}

#change all forward slashes to backslashes
batch.file.orig$command<-gsub(batch.file.orig$command, pattern="/", replacement="\\\\")
batch.file.orig$command<-sort(batch.file.orig$command)
batch.file.orig<-rbind(batch.file.orig, "pause")
nrow(batch.file.orig)
#

#save the original batch file for downsizing
write.table(batch.file.orig, "path/sitename_batch_downsize_orig.bat", sep="\t", row.names=F, col.names=F, quote=F)

save.image("path/sitename_downsizing.RData")


###############################
#this creates a file that contains the bitrates for all clips
#because some videos will be too large or too small after the original downsizing attempt, we will need to use higher or lower bitrates in subsequent attempts
#this data frame will contain progressively higher and lower bitrates for use when needed for all clips
# for example, if the original attempt for video 1 results in a video that is too small (under 900kb), then we would use a bitrate that is 1.05x larger than the original bitrate and try the downsizing process again

load("path/sitename_downsizing.RData")

file.sizes<-read.table("path/sitename_file_sizes.txt", sep="\t", header=T, stringsAsFactors=F)
nrow(file.sizes)
#
bitrates<-file.sizes[,c("input.name","output.name","video.size","bitrate")]

bitrates$x1.05<-round(bitrates$bitrate*1.05,0)
bitrates$x1.025<-round(bitrates$bitrate*1.025,0)
bitrates$x1.10<-round(bitrates$bitrate*1.1,0)
bitrates$x1.075<-round(bitrates$bitrate*1.075,0)
bitrates$x1.15<-round(bitrates$bitrate*1.15,0)
bitrates$x1.125<-round(bitrates$bitrate*1.125,0)
bitrates$x1.20<-round(bitrates$bitrate*1.2,0)
bitrates$x1.175<-round(bitrates$bitrate*1.175,0)
bitrates$x1.25<-round(bitrates$bitrate*1.25,0)
bitrates$x1.225<-round(bitrates$bitrate*1.225,0)
bitrates$x1.30<-round(bitrates$bitrate*1.3,0)
bitrates$x1.275<-round(bitrates$bitrate*1.275,0)
bitrates$x1.35<-round(bitrates$bitrate*1.35,0)
bitrates$x1.325<-round(bitrates$bitrate*1.325,0)
bitrates$x1.40<-round(bitrates$bitrate*1.4,0)
bitrates$x1.375<-round(bitrates$bitrate*1.375,0)
bitrates$x1.45<-round(bitrates$bitrate*1.45,0)
bitrates$x1.425<-round(bitrates$bitrate*1.425,0)
bitrates$x1.50<-round(bitrates$bitrate*1.50,0)
bitrates$x1.475<-round(bitrates$bitrate*1.475,0)
bitrates$x1.55<-round(bitrates$bitrate*1.555,0)
bitrates$x1.525<-round(bitrates$bitrate*1.525,0)

bitrates$x0.95<-round(bitrates$bitrate*0.95,0)
bitrates$x0.975<-round(bitrates$bitrate*0.975,0)
bitrates$x0.90<-round(bitrates$bitrate*0.9,0)
bitrates$x0.925<-round(bitrates$bitrate*0.925,0)
bitrates$x0.85<-round(bitrates$bitrate*0.85,0)
bitrates$x0.875<-round(bitrates$bitrate*0.875,0)
bitrates$x0.80<-round(bitrates$bitrate*0.8,0)
bitrates$x0.825<-round(bitrates$bitrate*0.825,0)
bitrates$x0.75<-round(bitrates$bitrate*0.75,0)
bitrates$x0.775<-round(bitrates$bitrate*0.775,0)
bitrates$x0.70<-round(bitrates$bitrate*0.7,0)

write.table(bitrates, "path/sitename_bitrate_file.txt", sep="\t", row.names=F, col.names=T)



#******************************************************
# ***** at this point, run the original batch file to perform the downsizing process****
# do not continue this script before the batch file has finished running
#******************************************************


#update bitrate.file with sizes of the original downsized files
bitrate.file<-read.table("path/sitename_bitrate_file.txt", sep="\t", header=T, stringsAsFactors=F)
nrow(bitrate.file)
#


start.time<-Sys.time()
bitrate.file$orig.downsized.size<-ceiling(file.size(bitrate.file$output.name)/1024)
end.time<-Sys.time()
end.time - start.time
#
sum(is.na(bitrate.file$orig.downsized.size))
#xxx, the ones that don't need to be downsized

range(bitrate.file$orig.downsized.size, na.rm=T)
#


#################################################
#now get all videos between 900kb and 1MB
#use higher or lower bitrates to achieve this
#e.g. the first step below is for videos that are too small, and thus need to use a bitrate that is 5% larger than the original
#the 5% higher section is labeled as 1.05x and the 5% lower section is labeled as 0.95x, etc
#in the rare case that a video is too high after being downsized, but too low after using the adjusted bitrate, then an intermediate bitrate will be used
#for example, if a video is too small after the original downsizing, but too large after using the 1.05x bitrate, then a 1.025x bitrate will be used


#################
###   1.05x   ###
#################

###### subset the bitrate file to contain only the clips that are currently under 900kb, which also originally had a size of over 1000kb
bitrate.file<-read.table("path/sitename_bitrate_file.txt", sep="\t", header=T, stringsAsFactors=F)
nrow(bitrate.file)
#
bitrate.file.105<-bitrate.file[!is.na(bitrate.file$orig.downsized.size) & bitrate.file$orig.downsized.size<900,]
nrow(bitrate.file.105)
#

######create batch file to redo the downsizing using 1.05x bitrate
batch.file.105<-data.frame(command=rep(NA, times=nrow(bitrate.file.105)))

ffmpeg.path<-"path/ffmpeg.exe"


for(i in 1:nrow(batch.file.105)){
	batch.file.105$command[i]<-paste(ffmpeg.path, " -y -i ", bitrate.file.105$input.name[i], " -c:v libx264 -b:v ", bitrate.file.105$x1.05[i],"k -pass 1 -c:a aac -b:a 48k -f null NUL && ^", ffmpeg.path, " -y -i ", bitrate.file.105$input.name[i], " -c:v libx264 -b:v ", bitrate.file.105$x1.05[i], "k -pass 2 -c:a aac -b:a 48k ", bitrate.file.105$output.name[i], sep="")
}

#change all forward slashes to backslashes
batch.file.105$command<-gsub(batch.file.105$command, pattern="/", replacement="\\\\")
batch.file.105$command<-sort(batch.file.105$command)
batch.file.105<-rbind(batch.file.105, "pause")

write.table(batch.file.105, "path/sitename_batch_downsize_105.bat", sep="\t", row.names=F, col.names=F, quote=F)

#save the 1.05x bitrate file version and the workspace
write.table(bitrate.file.105, "path/sitename_bitrate_file_105.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")


############################
#      run batch file      #
############################


######check to see if any clips are now over 1000kb, because if so, then they need to be redone with 1.025x
bitrate.file.105<-read.table("path/sitename_bitrate_file_105.txt", sep="\t", header=T, stringsAsFactors=F)
bitrate.file.105$current.size<-ceiling(file.size(bitrate.file.105$output.name)/1024)
sum(bitrate.file.105$current.size>1000)
#0


#are there still any under 900kb
#if there are still videos that are too small, use the next step up bitrate for them ie 1.10x
sum(bitrate.file.105$current.size<900)
#



#################
###   1.025x   ###
#################
#only do this step if it's necessary

bitrate.file.105<-read.table("path/sitename_bitrate_file_105.txt", sep="\t", header=T, stringsAsFactors=F)
nrow(bitrate.file.105)
#

bitrate.file.1025<-bitrate.file.105[bitrate.file.105$current.size>1000,]
nrow(bitrate.file.1025)
#

batch.file.1025<-data.frame(command=rep(NA, times=nrow(bitrate.file.1025)))

ffmpeg.path<-"path/ffmpeg.exe"


for(i in 1:nrow(batch.file.1025)){
	batch.file.1025$command[i]<-paste(ffmpeg.path, " -y -i ", bitrate.file.1025$input.name[i], " -c:v libx264 -b:v ", bitrate.file.1025$x1.025[i],"k -pass 1 -c:a aac -b:a 48k -f null NUL && ^", ffmpeg.path, " -y -i ", bitrate.file.1025$input.name[i], " -c:v libx264 -b:v ", bitrate.file.1025$x1.025[i], "k -pass 2 -c:a aac -b:a 48k ", bitrate.file.1025$output.name[i], sep="")
}

#change all forward slashes to backslashes
batch.file.1025$command<-gsub(batch.file.1025$command, pattern="/", replacement="\\\\")
batch.file.1025<-rbind(batch.file.1025, "pause")

write.table(batch.file.1025, "path/sitename_batch_downsize_1025.bat", sep="\t", row.names=F, col.names=F, quote=F)

#run batch file

#put the sizes back into .105
bitrate.file.105$current.size<-ceiling(file.size(bitrate.file.105$output.name)/1024)
sum(bitrate.file.105$current.size>1000)
#
sum(bitrate.file.105$current.size<900)
#

bitrate.file.105[bitrate.file.105$current.size>1000,]

#save the .105x bitrate file version and the workspace
write.table(bitrate.file.105, "path/sitename_bitrate_file_105.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")



#################
###   1.10x   ###
#################

#starting with the bitrate.file from 1.05x, see which files still are below 900kb and redo them at 1.1x bitrate
bitrate.file.105<-read.table("path/sitename_bitrate_file_105.txt", sep="\t", header=T, stringsAsFactors=F)
nrow(bitrate.file.105)
#
bitrate.file.110<-bitrate.file.105[bitrate.file.105$current.size<900,]
nrow(bitrate.file.110)
#

######create batch file to redo the downsizing using 1.10x bitrate
batch.file.110<-data.frame(command=rep(NA, times=nrow(bitrate.file.110)))

ffmpeg.path<-"path/ffmpeg.exe"


for(i in 1:nrow(batch.file.110)){
	batch.file.110$command[i]<-paste(ffmpeg.path, " -y -i ", bitrate.file.110$input.name[i], " -c:v libx264 -b:v ", bitrate.file.110$x1.10[i],"k -pass 1 -c:a aac -b:a 48k -f null NUL && ^", ffmpeg.path, " -y -i ", bitrate.file.110$input.name[i], " -c:v libx264 -b:v ", bitrate.file.110$x1.10[i], "k -pass 2 -c:a aac -b:a 48k ", bitrate.file.110$output.name[i], sep="")
}

#change all forward slashes to backslashes
batch.file.110$command<-gsub(batch.file.110$command, pattern="/", replacement="\\\\")
batch.file.110<-rbind(batch.file.110, "pause")

write.table(batch.file.110, "path/sitename_batch_downsize_110.bat", sep="\t", row.names=F, col.names=F, quote=F)


#save the .110x bitrate file version and the workspace
write.table(bitrate.file.110, "path/sitename_bitrate_file_110.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")


######run batch file


######check to see if any clips are now over 1000kb, because if so, then they need to be redone with 1.075x
bitrate.file.110<-read.table("path/sitename_bitrate_file_110.txt", sep="\t", header=T, stringsAsFactors=F)
bitrate.file.110$current.size<-ceiling(file.size(bitrate.file.110$output.name)/1024)
sum(bitrate.file.110$current.size>1000)
#0
#save the .110x bitrate file version and the workspace
write.table(bitrate.file.110, "path/sitename_bitrate_file_110.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")

#are there still any under 900kb
#if so, continue with 1.15x
sum(bitrate.file.110$current.size<900)
#


#################
###   1.075x   ### - NOT NECESSARY
#################

# bitrate.file.110<-read.table("path/sitename_bitrate_file_110.txt", sep="\t", header=T, stringsAsFactors=F)
# nrow(bitrate.file.110)
# #

# bitrate.file.1075<-bitrate.file.110[bitrate.file.110$current.size>1000,]
# nrow(bitrate.file.1075)
# #

# ######create batch file to redo the downsizing using 1.075x bitrate
# batch.file.1075<-data.frame(command=rep(NA, times=nrow(bitrate.file.1075)))

# for(i in 1:nrow(batch.file.1075)){
# 	batch.file.1075$command[i]<-paste("path/ffmpeg.exe -y -i ", bitrate.file.1075$input.name[i], " -c:v libx264 -b:v ", bitrate.file.1075$x1.075[i],"k -pass 1 -c:a aac -b:a 48k -f null NUL && ^path/ffmpeg.exe -y -i ", bitrate.file.1075$input.name[i], " -c:v libx264 -b:v ", bitrate.file.1075$x1.075[i], "k -pass 2 -c:a aac -b:a 48k ", bitrate.file.1075$output.name[i], sep="")
# }

# #change all forward slashes to backslashes
# batch.file.1075$command<-gsub(batch.file.1075$command, pattern="/", replacement="\\\\")
# batch.file.1075<-rbind(batch.file.1075, "pause")

# write.table(batch.file.1075, "path/sitename_batch_downsize_1075.bat", sep="\t", row.names=F, col.names=F, quote=F)

# #run batch file

# #put the sizes back into .110
# bitrate.file.110$current.size<-ceiling(file.size(bitrate.file.110$output.name)/1024)
# sum(bitrate.file.110$current.size>1000)
# #0
# sum(bitrate.file.110$current.size<900)
# #34

# #save the .110x bitrate file version and the workspace
# write.table(bitrate.file.110, "path/sitename_bitrate_file_110.txt", sep="\t", row.names=F, col.names=T)
# save.image("path/sitename_downsizing.RData")


#################
###   1.15x   ###
#################

#starting with the bitrate.file from 1.10x, see which files still are below 900kb and redo them at 1.15x bitrate
bitrate.file.110<-read.table("path/sitename_bitrate_file_110.txt", sep="\t", header=T, stringsAsFactors=F)
nrow(bitrate.file.110)
#
bitrate.file.115<-bitrate.file.110[bitrate.file.110$current.size<900,]
nrow(bitrate.file.115)
#

######create batch file to redo the downsizing using 1.15x bitrate
batch.file.115<-data.frame(command=rep(NA, times=nrow(bitrate.file.115)))

ffmpeg.path<-"path/ffmpeg.exe"


for(i in 1:nrow(batch.file.115)){
	batch.file.115$command[i]<-paste(ffmpeg.path, " -y -i ", bitrate.file.115$input.name[i], " -c:v libx264 -b:v ", bitrate.file.115$x1.15[i],"k -pass 1 -c:a aac -b:a 48k -f null NUL && ^", ffmpeg.path, " -y -i ", bitrate.file.115$input.name[i], " -c:v libx264 -b:v ", bitrate.file.115$x1.15[i], "k -pass 2 -c:a aac -b:a 48k ", bitrate.file.115$output.name[i], sep="")
}

#change all forward slashes to backslashes
batch.file.115$command<-gsub(batch.file.115$command, pattern="/", replacement="\\\\")
batch.file.115<-rbind(batch.file.115, "pause")

write.table(batch.file.115, "path/sitename_batch_downsize_115.bat", sep="\t", row.names=F, col.names=F, quote=F)


#save the .115x bitrate file version and the workspace
write.table(bitrate.file.115, "path/sitename_bitrate_file_115.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")

######run batch file

######check to see if any clips are now over 1000kb, because if so, then they need to be redone with 1.125x
bitrate.file.115<-read.table("path/sitename_bitrate_file_115.txt", sep="\t", header=T, stringsAsFactors=F)
bitrate.file.115$current.size<-ceiling(file.size(bitrate.file.115$output.name)/1024)
sum(bitrate.file.115$current.size>1000)
#0
#save the .115x bitrate file version and the workspace
write.table(bitrate.file.115, "path/sitename_bitrate_file_115.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")

#are there still any under 900kb
#if so then redo at 1.20x
sum(bitrate.file.115$current.size<900)
#3

#################
###   1.125x   ### - NOT NECESSARY
#################

# bitrate.file.115<-read.table("path/sitename_bitrate_file_115.txt", sep="\t", header=T, stringsAsFactors=F)
# nrow(bitrate.file.115)
# #

# bitrate.file.1125<-bitrate.file.115[bitrate.file.115$current.size>1000,]
# nrow(bitrate.file.1125)
# #

# ######create batch file to redo the downsizing using 1.125x bitrate
# 
# batch.file.1125<-data.frame(command=rep(NA, times=nrow(bitrate.file.1125)))

# for(i in 1:nrow(batch.file.1125)){
# 	batch.file.1125$command[i]<-paste("path/ffmpeg.exe -y -i ", bitrate.file.1125$input.name[i], " -c:v libx264 -b:v ", bitrate.file.1125$x1.125[i],"k -pass 1 -c:a aac -b:a 48k -f null NUL && ^path/ffmpeg.exe -y -i ", bitrate.file.1125$input.name[i], " -c:v libx264 -b:v ", bitrate.file.1125$x1.125[i], "k -pass 2 -c:a aac -b:a 48k ", bitrate.file.1125$output.name[i], sep="")
# }

# #change all forward slashes to backslashes
# batch.file.1125$command<-gsub(batch.file.1125$command, pattern="/", replacement="\\\\")
# batch.file.1125<-rbind(batch.file.1125, "pause")

# write.table(batch.file.1125, "path/sitename_batch_downsize_1125.bat", sep="\t", row.names=F, col.names=F, quote=F)

# #run batch file

# #put the sizes back into .115
# bitrate.file.115$current.size<-ceiling(file.size(bitrate.file.115$output.name)/1024)
# sum(bitrate.file.115$current.size>1000)
# #
# sum(bitrate.file.115$current.size<900)
# #

# #save the .115x bitrate file version and the workspace
# write.table(bitrate.file.115, "path/sitename_bitrate_file_115.txt", sep="\t", row.names=F, col.names=T)
# save.image("path/sitename_downsizing.RData")


#################
###   1.20x   ###
#################

#starting with the bitrate.file from 1.15x, see which files still are below 900kb and redo them at 1.2x bitrate
bitrate.file.115<-read.table("path/sitename_bitrate_file_115.txt", sep="\t", header=T, stringsAsFactors=F)
nrow(bitrate.file.115)
#
bitrate.file.120<-bitrate.file.115[bitrate.file.115$current.size<900,]
nrow(bitrate.file.120)
#

######create batch file to redo the downsizing using 1.20x bitrate
batch.file.120<-data.frame(command=rep(NA, times=nrow(bitrate.file.120)))

ffmpeg.path<-"path/ffmpeg.exe"


for(i in 1:nrow(batch.file.120)){
	batch.file.120$command[i]<-paste(ffmpeg.path, " -y -i ", bitrate.file.120$input.name[i], " -c:v libx264 -b:v ", bitrate.file.120$x1.20[i],"k -pass 1 -c:a aac -b:a 48k -f null NUL && ^", ffmpeg.path, " -y -i ", bitrate.file.120$input.name[i], " -c:v libx264 -b:v ", bitrate.file.120$x1.20[i], "k -pass 2 -c:a aac -b:a 48k ", bitrate.file.120$output.name[i], sep="")
}

#change all forward slashes to backslashes
batch.file.120$command<-gsub(batch.file.120$command, pattern="/", replacement="\\\\")
batch.file.120<-rbind(batch.file.120, "pause")

write.table(batch.file.120, "path/sitename_batch_downsize_120.bat", sep="\t", row.names=F, col.names=F, quote=F)


#save the .120x bitrate file version and the workspace
write.table(bitrate.file.120, "path/sitename_bitrate_file_120.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")

######run batch file

######check to see if any clips are now over 1000kb, because if so, then they need to be redone with 1.175x
bitrate.file.120<-read.table("path/sitename_bitrate_file_120.txt", sep="\t", header=T, stringsAsFactors=F)
bitrate.file.120$current.size<-ceiling(file.size(bitrate.file.120$output.name)/1024)
sum(bitrate.file.120$current.size>1000)
#0
#save the .120x bitrate file version and the workspace
write.table(bitrate.file.120, "path/sitename_bitrate_file_120.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")

#are there still any under 900kb
#if so then redo downsizing with 1.25x
sum(bitrate.file.120$current.size<900)
#0


#################
###   1.175x   ### - NOT NECESSARY
#################

# bitrate.file.120<-read.table("path/sitename_bitrate_file_120.txt", sep="\t", header=T, stringsAsFactors=F)
# nrow(bitrate.file.120)
# #

# bitrate.file.1175<-bitrate.file.120[bitrate.file.120$current.size>1000,]
# nrow(bitrate.file.1175)
# #

# ######create batch file to redo the downsizing using 1.175x bitrate
# 
# batch.file.1175<-data.frame(command=rep(NA, times=nrow(bitrate.file.1175)))

# for(i in 1:nrow(batch.file.1175)){
# 	batch.file.1175$command[i]<-paste("path/ffmpeg.exe -y -i ", bitrate.file.1175$input.name[i], " -c:v libx264 -b:v ", bitrate.file.1175$x1.175[i],"k -pass 1 -c:a aac -b:a 48k -f null NUL && ^path/ffmpeg.exe -y -i ", bitrate.file.1175$input.name[i], " -c:v libx264 -b:v ", bitrate.file.1175$x1.175[i], "k -pass 2 -c:a aac -b:a 48k ", bitrate.file.1175$output.name[i], sep="")
# }

# #change all forward slashes to backslashes
# batch.file.1175$command<-gsub(batch.file.1175$command, pattern="/", replacement="\\\\")
# batch.file.1175<-rbind(batch.file.1175, "pause")

# write.table(batch.file.1175, "path/sitename_batch_downsize_1175.bat", sep="\t", row.names=F, col.names=F, quote=F)

# #run batch file

# #put the sizes back into .120
# bitrate.file.120$current.size<-ceiling(file.size(bitrate.file.120$output.name)/1024)
# sum(bitrate.file.120$current.size>1000)
# #
# sum(bitrate.file.120$current.size<900)
# #

# #save the .120x bitrate file version and the workspace
# write.table(bitrate.file.120, "path/sitename_bitrate_file_120.txt", sep="\t", row.names=F, col.names=T)
# save.image("path/sitename_downsizing.RData")



#################
###   1.25x   ###
#################

#starting with the bitrate.file from 1.20x, see which files still are below 900kb and redo them at 1.25x bitrate
bitrate.file.120<-read.table("path/sitename_bitrate_file_120.txt", sep="\t", header=T, stringsAsFactors=F)
nrow(bitrate.file.120)
#
bitrate.file.125<-bitrate.file.120[bitrate.file.120$current.size<900,]
nrow(bitrate.file.125)
#

######create batch file to redo the downsizing using 1.25x bitrate
batch.file.125<-data.frame(command=rep(NA, times=nrow(bitrate.file.125)))

ffmpeg.path<-"path/ffmpeg.exe"


for(i in 1:nrow(batch.file.125)){
	batch.file.125$command[i]<-paste(ffmpeg.path, " -y -i ", bitrate.file.125$input.name[i], " -c:v libx264 -b:v ", bitrate.file.125$x1.25[i],"k -pass 1 -c:a aac -b:a 48k -f null NUL && ^", ffmpeg.path, " -y -i ", bitrate.file.125$input.name[i], " -c:v libx264 -b:v ", bitrate.file.125$x1.25[i], "k -pass 2 -c:a aac -b:a 48k ", bitrate.file.125$output.name[i], sep="")
}

#change all forward slashes to backslashes
batch.file.125$command<-gsub(batch.file.125$command, pattern="/", replacement="\\\\")
batch.file.125<-rbind(batch.file.125, "pause")

write.table(batch.file.125, "path/sitename_batch_downsize_125.bat", sep="\t", row.names=F, col.names=F, quote=F)


#save the .125x bitrate file version and the workspace
write.table(bitrate.file.125, "path/sitename_bitrate_file_125.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")

######run batch file

######check to see if any clips are now over 1000kb, because if so, then they need to be redone with 1.225x
bitrate.file.125<-read.table("path/sitename_bitrate_file_125.txt", sep="\t", header=T, stringsAsFactors=F)
bitrate.file.125$current.size<-ceiling(file.size(bitrate.file.125$output.name)/1024)
sum(bitrate.file.125$current.size>1000)
#
#save the .125x bitrate file version and the workspace
write.table(bitrate.file.125, "path/sitename_bitrate_file_125.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")

#are there still any under 900kb
#if so then they need to be redon with 1.3x
sum(bitrate.file.125$current.size<900)
#

#################
###   1.225x   ### - NOT NECESSARY
#################

# bitrate.file.125<-read.table("path/sitename_bitrate_file_125.txt", sep="\t", header=T, stringsAsFactors=F)
# nrow(bitrate.file.125)
# #

# bitrate.file.1225<-bitrate.file.125[bitrate.file.125$current.size>1000,]
# nrow(bitrate.file.1225)
# #

# ######create batch file to redo the downsizing using 1.225x bitrate
# 
# batch.file.1225<-data.frame(command=rep(NA, times=nrow(bitrate.file.1225)))

# for(i in 1:nrow(batch.file.1225)){
# 	batch.file.1225$command[i]<-paste("path/ffmpeg.exe -y -i ", bitrate.file.1225$input.name[i], " -c:v libx264 -b:v ", bitrate.file.1225$x1.225[i],"k -pass 1 -c:a aac -b:a 48k -f null NUL && ^path/ffmpeg.exe -y -i ", bitrate.file.1225$input.name[i], " -c:v libx264 -b:v ", bitrate.file.1225$x1.225[i], "k -pass 2 -c:a aac -b:a 48k ", bitrate.file.1225$output.name[i], sep="")
# }

# #change all forward slashes to backslashes
# batch.file.1225$command<-gsub(batch.file.1225$command, pattern="/", replacement="\\\\")
# batch.file.1225<-rbind(batch.file.1225, "pause")

# write.table(batch.file.1225, "path/sitename_batch_downsize_1225.bat", sep="\t", row.names=F, col.names=F, quote=F)

# #run batch file

# #put the sizes back into .125
# bitrate.file.125$current.size<-ceiling(file.size(bitrate.file.125$output.name)/1024)
# sum(bitrate.file.125$current.size>1000)
# #
# sum(bitrate.file.125$current.size<900)
# #

# #save the .125x bitrate file version and the workspace
# write.table(bitrate.file.125, "path/sitename_bitrate_file_125.txt", sep="\t", row.names=F, col.names=T)
# save.image("path/sitename_downsizing.RData")


#################
###   1.30x   ###
#################

#starting with the bitrate.file from 1.25x, see which files still are below 900kb and redo them at 1.3x bitrate
bitrate.file.125<-read.table("path/sitename_bitrate_file_125.txt", sep="\t", header=T, stringsAsFactors=F)
nrow(bitrate.file.125)
#
bitrate.file.130<-bitrate.file.125[bitrate.file.125$current.size<900,]
nrow(bitrate.file.130)
#

######create batch file to redo the downsizing using 1.30x bitrate

batch.file.130<-data.frame(command=rep(NA, times=nrow(bitrate.file.130)))

ffmpeg.path<-"path/ffmpeg.exe"


for(i in 1:nrow(batch.file.130)){
	batch.file.130$command[i]<-paste(ffmpeg.path, " -y -i ", bitrate.file.130$input.name[i], " -c:v libx264 -b:v ", bitrate.file.130$x1.30[i],"k -pass 1 -c:a aac -b:a 48k -f null NUL && ^", ffmpeg.path, " -y -i ", bitrate.file.130$input.name[i], " -c:v libx264 -b:v ", bitrate.file.130$x1.30[i], "k -pass 2 -c:a aac -b:a 48k ", bitrate.file.130$output.name[i], sep="")
}

#change all forward slashes to backslashes
batch.file.130$command<-gsub(batch.file.130$command, pattern="/", replacement="\\\\")
batch.file.130<-rbind(batch.file.130, "pause")

write.table(batch.file.130, "path/sitename_batch_downsize_130.bat", sep="\t", row.names=F, col.names=F, quote=F)

#save the .130x bitrate file version and the workspace
write.table(bitrate.file.130, "path/sitename_bitrate_file_130.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")

######run batch file


######check to see if any clips are now over 1000kb, because if so, then they need to be redone with 1.275x
bitrate.file.130<-read.table("path/sitename_bitrate_file_130.txt", sep="\t", header=T, stringsAsFactors=F)
bitrate.file.130$current.size<-ceiling(file.size(bitrate.file.130$output.name)/1024)
sum(bitrate.file.130$current.size>1000)
#0
#save the .130x bitrate file version and the workspace
write.table(bitrate.file.130, "path/sitename_bitrate_file_130.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")

#are there still any under 900kb
#if so then they need to be redone wth 1.35x
sum(bitrate.file.130$current.size<900)
#3, fine


#################
###   1.275x   ### - NOT NECESSARY
#################

# bitrate.file.130<-read.table("path/sitename_bitrate_file_130.txt", sep="\t", header=T, stringsAsFactors=F)
# nrow(bitrate.file.130)
# #

# bitrate.file.1275<-bitrate.file.130[bitrate.file.130$current.size>1000,]
# nrow(bitrate.file.1275)
# #

# ######create batch file to redo the downsizing using 1.275x bitrate
# 
# batch.file.1275<-data.frame(command=rep(NA, times=nrow(bitrate.file.1275)))

# for(i in 1:nrow(batch.file.1275)){
# 	batch.file.1275$command[i]<-paste("path/ffmpeg.exe -y -i ", bitrate.file.1275$input.name[i], " -c:v libx264 -b:v ", bitrate.file.1275$x1.275[i],"k -pass 1 -c:a aac -b:a 48k -f null NUL && ^path/ffmpeg.exe -y -i ", bitrate.file.1275$input.name[i], " -c:v libx264 -b:v ", bitrate.file.1275$x1.275[i], "k -pass 2 -c:a aac -b:a 48k ", bitrate.file.1275$output.name[i], sep="")
# }

# #change all forward slashes to backslashes
# batch.file.1275$command<-gsub(batch.file.1275$command, pattern="/", replacement="\\\\")
# batch.file.1275<-rbind(batch.file.1275, "pause")

# write.table(batch.file.1275, "path/sitename_batch_downsize_1275.bat", sep="\t", row.names=F, col.names=F, quote=F)

# #run batch file

# #put the sizes back into .130
# bitrate.file.130$current.size<-ceiling(file.size(bitrate.file.130$output.name)/1024)
# sum(bitrate.file.130$current.size>1000)
# #
# sum(bitrate.file.130$current.size<900)
# #

# #save the .130x bitrate file version and the workspace
# write.table(bitrate.file.130, "path/sitename_bitrate_file_130.txt", sep="\t", row.names=F, col.names=T)
# save.image("path/sitename_downsizing.RData")


#################
###   1.35x   ###
#################

#starting with the bitrate.file from 1.30x, see which files still are below 900kb and redo them at 1.35x bitrate
bitrate.file.130<-read.table("path/sitename_bitrate_file_130.txt", sep="\t", header=T, stringsAsFactors=F)
nrow(bitrate.file.130)
#
bitrate.file.135<-bitrate.file.130[bitrate.file.130$current.size<900,]
nrow(bitrate.file.135)
#

######create batch file to redo the downsizing using 1.35x bitrate

batch.file.135<-data.frame(command=rep(NA, times=nrow(bitrate.file.135)))

ffmpeg.path<-"path/ffmpeg.exe"


for(i in 1:nrow(batch.file.135)){
	batch.file.135$command[i]<-paste(ffmpeg.path, " -y -i ", bitrate.file.135$input.name[i], " -c:v libx264 -b:v ", bitrate.file.135$x1.35[i],"k -pass 1 -c:a aac -b:a 48k -f null NUL && ^", ffmpeg.path, " -y -i ", bitrate.file.135$input.name[i], " -c:v libx264 -b:v ", bitrate.file.135$x1.35[i], "k -pass 2 -c:a aac -b:a 48k ", bitrate.file.135$output.name[i], sep="")
}

#change all forward slashes to backslashes
batch.file.135$command<-gsub(batch.file.135$command, pattern="/", replacement="\\\\")
batch.file.135<-rbind(batch.file.135, "pause")

write.table(batch.file.135, "path/sitename_batch_downsize_135.bat", sep="\t", row.names=F, col.names=F, quote=F)

#save the 1.35x bitrate file version and the workspace
write.table(bitrate.file.135, "path/sitename_bitrate_file_135.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")

######run batch file

######check to see if any clips are now over 1000kb, because if so, then they need to be redone with 1.325x
bitrate.file.135<-read.table("path/sitename_bitrate_file_135.txt", sep="\t", header=T, stringsAsFactors=F)
bitrate.file.135$current.size<-ceiling(file.size(bitrate.file.135$output.name)/1024)
sum(bitrate.file.135$current.size>1000)
#0
#save the 1.35x bitrate file version and the workspace
write.table(bitrate.file.135, "path/sitename_bitrate_file_135.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")

#are there still any under 900kb
sum(bitrate.file.135$current.size<900)
#0



############### continue on as need with larger bitrates



########################################################################################
########################################################################################
########### move on to reducing bitrate to get clips under 1000kb


#################
###   0.95x   ###
#################

###### subset the bitrate file to contain only the clips that are currently over 1000kb
bitrate.file<-read.table("path/sitename_bitrate_file.txt", sep="\t", header=T, stringsAsFactors=F)
nrow(bitrate.file)
#
bitrate.file$current.size<-ceiling(file.size(bitrate.file$output.name)/1024)
bitrate.file.095<-bitrate.file[!is.na(bitrate.file$orig.downsized.size) & bitrate.file$orig.downsized.size>1000,]
nrow(bitrate.file.095)
#

######create batch file to redo the downsizing using 0.95x bitrate

batch.file.095<-data.frame(command=rep(NA, times=nrow(bitrate.file.095)))

ffmpeg.path<-"path/ffmpeg.exe"


for(i in 1:nrow(batch.file.095)){
	batch.file.095$command[i]<-paste(ffmpeg.path, " -y -i ", bitrate.file.095$input.name[i], " -c:v libx264 -b:v ", bitrate.file.095$x0.95[i],"k -pass 1 -c:a aac -b:a 48k -f null NUL && ^", ffmpeg.path, " -y -i ", bitrate.file.095$input.name[i], " -c:v libx264 -b:v ", bitrate.file.095$x0.95[i], "k -pass 2 -c:a aac -b:a 48k ", bitrate.file.095$output.name[i], sep="")
}

#change all forward slashes to backslashes
batch.file.095$command<-gsub(batch.file.095$command, pattern="/", replacement="\\\\")
batch.file.095$command<-sort(batch.file.095$command)
batch.file.095<-rbind(batch.file.095, "pause")

write.table(batch.file.095, "path/sitename_batch_downsize_095.bat", sep="\t", row.names=F, col.names=F, quote=F)

#save the .095x bitrate file version and the workspace
write.table(bitrate.file.095, "path/sitename_bitrate_file_095.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")

######run batch file

######check to see if any clips are now under 900kb, because if so, then they need to be redone with 0.975x
bitrate.file.095<-read.table("path/sitename_bitrate_file_095.txt", sep="\t", header=T, stringsAsFactors=F)
bitrate.file.095$current.size<-ceiling(file.size(bitrate.file.095$output.name)/1024)
sum(bitrate.file.095$current.size<900)
#0

#save the .095x bitrate file version and the workspace
write.table(bitrate.file.095, "path/sitename_bitrate_file_095.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")

#are there still any over 1000kb
#if so, then they need to be redone at 0.90x
sum(bitrate.file.095$current.size>1000)
#0


#################
###   0.975x   ### unnecessary
#################

# bitrate.file.095<-read.table("path/sitename_bitrate_file_095.txt", sep="\t", header=T, stringsAsFactors=F)
# nrow(bitrate.file.095)
# #

# bitrate.file.0975<-bitrate.file.095[bitrate.file.095$current.size<900,]
# nrow(bitrate.file.0975)
# #

# ######create batch file to redo the downsizing using 1.075x bitrate

# batch.file.0975<-data.frame(command=rep(NA, times=nrow(bitrate.file.0975)))

# ffmpeg.path<-"path/ffmpeg.exe"


# for(i in 1:nrow(batch.file.0975)){
# 	batch.file.0975$command[i]<-paste(ffmpeg.path, " -y -i ", bitrate.file.0975$input.name[i], " -c:v libx264 -b:v ", bitrate.file.0975$x0.975[i],"k -pass 1 -c:a aac -b:a 48k -f null NUL && ^", ffmpeg.path, " -y -i ", bitrate.file.0975$input.name[i], " -c:v libx264 -b:v ", bitrate.file.0975$x0.975[i], "k -pass 2 -c:a aac -b:a 48k ", bitrate.file.0975$output.name[i], sep="")
# }

# #change all forward slashes to backslashes
# batch.file.0975$command<-gsub(batch.file.0975$command, pattern="/", replacement="\\\\")
# batch.file.0975<-rbind(batch.file.0975, "pause")

# write.table(batch.file.0975, "path/sitename_batch_downsize_0975.bat", sep="\t", row.names=F, col.names=F, quote=F)

# #run batch file

# #put the sizes back into .095
# bitrate.file.095$current.size<-ceiling(file.size(bitrate.file.095$output.name)/1024)
# sum(bitrate.file.095$current.size<900)
# #
# sum(bitrate.file.095$current.size>1000)
# #

# #save the .095x bitrate file version and the workspace
# write.table(bitrate.file.095, "path/sitename_bitrate_file_095.txt", sep="\t", row.names=F, col.names=T)
# save.image("path/sitename_downsizing.RData")



#################
###   0.90x   ###
#################

#starting with the bitrate.file from 0.95x, see which files still are above 1000kb and redo them at 0.90x bitrate
bitrate.file.095<-read.table("path/sitename_bitrate_file_095.txt", sep="\t", header=T, stringsAsFactors=F)
nrow(bitrate.file.095)
#
bitrate.file.090<-bitrate.file.095[bitrate.file.095$current.size>1000,]
nrow(bitrate.file.090)
#

######create batch file to redo the downsizing using 1.05x bitrate

batch.file.090<-data.frame(command=rep(NA, times=nrow(bitrate.file.090)))

ffmpeg.path<-"path/ffmpeg.exe"


for(i in 1:nrow(batch.file.090)){
	batch.file.090$command[i]<-paste(ffmpeg.path, " -y -i ", bitrate.file.090$input.name[i], " -c:v libx264 -b:v ", bitrate.file.090$x0.90[i],"k -pass 1 -c:a aac -b:a 48k -f null NUL && ^", ffmpeg.path, " -y -i ", bitrate.file.090$input.name[i], " -c:v libx264 -b:v ", bitrate.file.090$x0.90[i], "k -pass 2 -c:a aac -b:a 48k ", bitrate.file.090$output.name[i], sep="")
}

#change all forward slashes to backslashes
batch.file.090$command<-gsub(batch.file.090$command, pattern="/", replacement="\\\\")
batch.file.090$command<-sort(batch.file.090$command)
batch.file.090<-rbind(batch.file.090, "pause")

write.table(batch.file.090, "path/sitename_batch_downsize_090.bat", sep="\t", row.names=F, col.names=F, quote=F)

#save the .090x bitrate file version and the workspace
write.table(bitrate.file.090, "path/sitename_bitrate_file_090.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")

######run batch file

######check to see if any clips are now under 900kb, because if so, then they need to be redone with 0.925x
bitrate.file.090$current.size<-ceiling(file.size(bitrate.file.090$output.name)/1024)
sum(bitrate.file.090$current.size<900)
#0
#save the .090x bitrate file version and the workspace
write.table(bitrate.file.090, "path/sitename_bitrate_file_090.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")

#are there still any over 1000kb
#if so they need to be redone with 0.85x
sum(bitrate.file.090$current.size>1000)
#


#################
###   0.925x   ### - NOT NECESSARY
#################

# bitrate.file.090<-read.table("path/sitename_bitrate_file_090.txt", sep="\t", header=T, stringsAsFactors=F)
# nrow(bitrate.file.090)
# #

# bitrate.file.0925<-bitrate.file.090[bitrate.file.090$current.size<900,]
# nrow(bitrate.file.0925)
# #

# ######create batch file to redo the downsizing using 1.075x bitrate
# 
# batch.file.0925<-data.frame(command=rep(NA, times=nrow(bitrate.file.0925)))

# ffmpeg.path<-"path/ffmpeg.exe"


# for(i in 1:nrow(batch.file.0925)){
# 	batch.file.0925$command[i]<-paste(ffmpeg.path, " -y -i ", bitrate.file.0925$input.name[i], " -c:v libx264 -b:v ", bitrate.file.0925$x0.925[i],"k -pass 1 -c:a aac -b:a 48k -f null NUL && ^", ffmpeg.path, " -y -i ", bitrate.file.0925$input.name[i], " -c:v libx264 -b:v ", bitrate.file.0925$x0.925[i], "k -pass 2 -c:a aac -b:a 48k ", bitrate.file.0925$output.name[i], sep="")
# }

# #change all forward slashes to backslashes
# batch.file.0925$command<-gsub(batch.file.0925$command, pattern="/", replacement="\\\\")
# batch.file.0925<-rbind(batch.file.0925, "pause")

# write.table(batch.file.0925, "path/sitename_batch_downsize_0925.bat", sep="\t", row.names=F, col.names=F, quote=F)

# #run batch file

# #put the sizes back into .090
# bitrate.file.090$current.size<-ceiling(file.size(bitrate.file.090$output.name)/1024)
# sum(bitrate.file.090$current.size<900)
# #
# sum(bitrate.file.090$current.size>1000)
# #

# #save the .090x bitrate file version and the workspace
# write.table(bitrate.file.090, "path/sitename_bitrate_file_090.txt", sep="\t", row.names=F, col.names=T)
# save.image("path/sitename_downsizing.RData")


#################
###   0.85x   ###
#################

#starting with the bitrate.file from 0.90x, see which files still are above 1000kb and redo them at 0.85x bitrate
bitrate.file.090<-read.table("path/sitename_bitrate_file_090.txt", sep="\t", header=T, stringsAsFactors=F)
nrow(bitrate.file.090)
#
bitrate.file.085<-bitrate.file.090[bitrate.file.090$current.size>1000,]
nrow(bitrate.file.085)
#

######create batch file to redo the downsizing using 0.85x bitrate

batch.file.085<-data.frame(command=rep(NA, times=nrow(bitrate.file.085)))

ffmpeg.path<-"path/ffmpeg.exe"


for(i in 1:nrow(batch.file.085)){
	batch.file.085$command[i]<-paste(ffmpeg.path, " -y -i ", bitrate.file.085$input.name[i], " -c:v libx264 -b:v ", bitrate.file.085$x0.85[i],"k -pass 1 -c:a aac -b:a 48k -f null NUL && ^", ffmpeg.path, " -y -i ", bitrate.file.085$input.name[i], " -c:v libx264 -b:v ", bitrate.file.085$x0.85[i], "k -pass 2 -c:a aac -b:a 48k ", bitrate.file.085$output.name[i], sep="")
}

#change all forward slashes to backslashes
batch.file.085$command<-gsub(batch.file.085$command, pattern="/", replacement="\\\\")
batch.file.085$command<-sort(batch.file.085$command)
batch.file.085<-rbind(batch.file.085, "pause")

write.table(batch.file.085, "path/sitename_batch_downsize_085.bat", sep="\t", row.names=F, col.names=F, quote=F)

#save the .085x bitrate file version and the workspace
write.table(bitrate.file.085, "path/sitename_bitrate_file_085.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")

######run batch file

######check to see if any clips are now under 900kb, because if so, then they need to be redone with 0.925x
bitrate.file.085$current.size<-ceiling(file.size(bitrate.file.085$output.name)/1024)
sum(bitrate.file.085$current.size<900)
#0
#save the 0.85x bitrate file version and the workspace
write.table(bitrate.file.085, "path/sitename_bitrate_file_085.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")

#are there still any over 1000kb
#if so they need to be redone with 0.80x
sum(bitrate.file.085$current.size>1000)
#


#################
###   0.875x   ### unnecessary
#################

# bitrate.file.085<-read.table("path/sitename_bitrate_file_085.txt", sep="\t", header=T, stringsAsFactors=F)
# nrow(bitrate.file.085)
# #

# bitrate.file.0875<-bitrate.file.085[bitrate.file.085$current.size<900,]
# nrow(bitrate.file.0875)
# #

# ######create batch file to redo the downsizing using 1.05x bitrate
# 
# batch.file.0875<-data.frame(command=rep(NA, times=nrow(bitrate.file.0875)))

# ffmpeg.path<-"path/ffmpeg.exe"


# for(i in 1:nrow(batch.file.0875)){
# 	batch.file.0875$command[i]<-paste(ffmpeg.path, " -y -i ", bitrate.file.0875$input.name[i], " -c:v libx264 -b:v ", bitrate.file.0875$x0.875[i],"k -pass 1 -c:a aac -b:a 48k -f null NUL && ^", ffmpeg.path, " -y -i ", bitrate.file.0875$input.name[i], " -c:v libx264 -b:v ", bitrate.file.0875$x0.875[i], "k -pass 2 -c:a aac -b:a 48k ", bitrate.file.0875$output.name[i], sep="")
# }

# #change all forward slashes to backslashes
# batch.file.0875$command<-gsub(batch.file.0875$command, pattern="/", replacement="\\\\")
# batch.file.0875<-rbind(batch.file.0875, "pause")

# write.table(batch.file.0875, "path/sitename_batch_downsize_0875.bat", sep="\t", row.names=F, col.names=F, quote=F)

# #run batch file

# #put the sizes back into .085
# bitrate.file.085$current.size<-ceiling(file.size(bitrate.file.085$output.name)/1024)
# sum(bitrate.file.085$current.size<900)
# #
# sum(bitrate.file.085$current.size>1000)
# #

# #save the .085x bitrate file version and the workspace
# write.table(bitrate.file.085, "path/sitename_bitrate_file_085.txt", sep="\t", row.names=F, col.names=T)
# save.image("path/sitename_downsizing.RData")



#################
###   0.80x   ###
#################

#starting with the bitrate.file from 0.90x, see which files still are above 1000kb and redo them at 0.85x bitrate
bitrate.file.085<-read.table("path/sitename_bitrate_file_085.txt", sep="\t", header=T, stringsAsFactors=F)
nrow(bitrate.file.085)
#
bitrate.file.080<-bitrate.file.085[bitrate.file.085$current.size>1000,]
nrow(bitrate.file.080)
#

######create batch file to redo the downsizing using 0.85x bitrate

batch.file.080<-data.frame(command=rep(NA, times=nrow(bitrate.file.080)))

ffmpeg.path<-"path/ffmpeg.exe"


for(i in 1:nrow(batch.file.080)){
	batch.file.080$command[i]<-paste(ffmpeg.path, " -y -i ", bitrate.file.080$input.name[i], " -c:v libx264 -b:v ", bitrate.file.080$x0.80[i],"k -pass 1 -c:a aac -b:a 48k -f null NUL && ^", ffmpeg.path, " -y -i ", bitrate.file.080$input.name[i], " -c:v libx264 -b:v ", bitrate.file.080$x0.80[i], "k -pass 2 -c:a aac -b:a 48k ", bitrate.file.080$output.name[i], sep="")
}

#change all forward slashes to backslashes
batch.file.080$command<-gsub(batch.file.080$command, pattern="/", replacement="\\\\")
batch.file.080$command<-sort(batch.file.080$command)
batch.file.080<-rbind(batch.file.080, "pause")

write.table(batch.file.080, "path/sitename_batch_downsize_080.bat", sep="\t", row.names=F, col.names=F, quote=F)

#save the 0.80x bitrate file version and the workspace
write.table(bitrate.file.080, "path/sitename_bitrate_file_080.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")

######run batch file

######check to see if any clips are now under 900kb, because if so, then they need to be redone with 0.825x
bitrate.file.080$current.size<-ceiling(file.size(bitrate.file.080$output.name)/1024)
sum(bitrate.file.080$current.size<900)
#0
#save the .080x bitrate file version and the workspace
write.table(bitrate.file.080, "path/sitename_bitrate_file_080.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")

#are there still any over 1000kb
#if so they need to be redone with 0.75x
sum(bitrate.file.080$current.size>1000)
#


#################
###   0.75x   ###
#################

#starting with the bitrate.file from 0.80x, see which files still are above 1000kb and redo them at 0.80x bitrate
bitrate.file.080<-read.table("path/sitename_bitrate_file_080.txt", sep="\t", header=T, stringsAsFactors=F)
nrow(bitrate.file.080)
#
bitrate.file.075<-bitrate.file.080[bitrate.file.080$current.size>1000,]
nrow(bitrate.file.075)
#

#make the 0.75x column
bitrate.file.075$x0.75<-round(bitrate.file.075$bitrate*0.75,0)

######create batch file to redo the downsizing using 0.80x bitrate

batch.file.075<-data.frame(command=rep(NA, times=nrow(bitrate.file.075)))

ffmpeg.path<-"path/ffmpeg.exe"


for(i in 1:nrow(batch.file.075)){
	batch.file.075$command[i]<-paste(ffmpeg.path, " -y -i ", bitrate.file.075$input.name[i], " -c:v libx264 -b:v ", bitrate.file.075$x0.75[i],"k -pass 1 -c:a aac -b:a 48k -f null NUL && ^", ffmpeg.path, " -y -i ", bitrate.file.075$input.name[i], " -c:v libx264 -b:v ", bitrate.file.075$x0.75[i], "k -pass 2 -c:a aac -b:a 48k ", bitrate.file.075$output.name[i], sep="")
}

#change all forward slashes to backslashes
batch.file.075$command<-gsub(batch.file.075$command, pattern="/", replacement="\\\\")
batch.file.075$command<-sort(batch.file.075$command)
batch.file.075<-rbind(batch.file.075, "pause")

write.table(batch.file.075, "path/sitename_batch_downsize_075.bat", sep="\t", row.names=F, col.names=F, quote=F)

#save the .075x bitrate file version and the workspace
write.table(bitrate.file.075, "path/sitename_bitrate_file_075.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")

######run batch file

######check to see if any clips are now under 900kb, because if so, then they need to be redone with 0.925x
bitrate.file.075$current.size<-ceiling(file.size(bitrate.file.075$output.name)/1024)
sum(bitrate.file.075$current.size<900)
#0
#save the .075x bitrate file version and the workspace
write.table(bitrate.file.075, "path/sitename_bitrate_file_075.txt", sep="\t", row.names=F, col.names=T)
save.image("path/sitename_downsizing.RData")

#are there still any over 1000kb
sum(bitrate.file.075$current.size>1000)
#0


#continue as needed with smaller bitrates



################################################################
#now copy over the files that were originally under 1MB so that the downsized files and these small files are in the same folder

bitrate.file<-read.table("path/sitename_bitrate_file.txt", sep="\t", header=T, stringsAsFactors=F)
nrow(bitrate.file)
#
bitrate.file<-bitrate.file[bitrate.file$video.size<=1000,]
nrow(bitrate.file)
#

batch.file.copy<-data.frame(command=rep(NA, times=nrow(bitrate.file)))

for(i in 1:nrow(batch.file.copy)){
	batch.file.copy$command[i]<-paste("copy /Y ", bitrate.file$input.name[i], " ", bitrate.file$output.name[i], sep="")
}

batch.file.copy$command<-gsub(batch.file.copy$command, pattern="/", replacement="\\\\")
batch.file.copy$command<-gsub(batch.file.copy$command, pattern="\\Y", replacement="/Y", fixed=T)
batch.file.copy<-rbind(batch.file.copy, "pause")

write.table(batch.file.copy, "path/sitename_batch_downsize_copy.bat", sep="\t", row.names=F, col.names=F, quote=F)

###################
#run batch file
###################

###################
#now make sure that all videos and all still images (if using) are in the same folder


################################################################
######                   make manifests                  #######
################################################################

#these manifests are necessary for uploading to zooniverse using their CLI (command line interface)

#get all stills using command line

cd path/folder_where_everything_is

dir > "path/all_stills_and_clips.csv"

###############
#in R
all.stills<-read.table("path/all_stills_and_clips.csv", sep=",", header=T)
nrow(all.stills)
#

#keep only the jpgs
colnames(all.stills)<-"path"
all.stills<-all.stills[grepl(all.stills$path, pattern="jpg"),]
#turns into a vector
length(all.stills)
#
#get rid of the random numbers at the beginning of everything
parts<-strsplit(all.stills, split=" ")
thing<-unlist(lapply(parts, length))
table(thing)


#take the last part from everything
all.stills<-unlist(lapply(parts, function(x){x[length(x)]}))
all.stills<-sort(all.stills)

bitrate.file<-read.table("path/sitename_bitrate_file.txt", sep="\t", header=T, stringsAsFactors=F)
nrow(bitrate.file)
#
parts<-strsplit(bitrate.file$input.name, split="/")
parts<-unlist(lapply(parts, function(x){x[length(x)]}))
parts<-strsplit(parts, split=".", fixed=T)
parts<-unlist(lapply(parts, function(x){x[1]}))
bitrate.file$match.id<-parts

#set up data frame for all manifest information
still.manifest<-data.frame(matrix(NA, nrow=nrow(bitrate.file), ncol=2))
colnames(still.manifest)<-c("full.video.name","match.id")
still.manifest$full.video.name<-bitrate.file$output.name
still.manifest$match.id<-bitrate.file$match.id
nrow(still.manifest)
#

start.time<-Sys.time()
grouped.stills<-lapply(1:nrow(still.manifest), function(x){
	sub.stills<-all.stills[grepl(x=all.stills, pattern=still.manifest$match.id[x], fixed=T)]
	if(length(sub.stills)>0){
		return(sub.stills)
	} else {
		return(NA)
	}
})
end.time<-Sys.time()
end.time - start.time
#

#we use 16 here because 16 is the highest number of stills for one clip that we have
grouped.stills<-lapply(grouped.stills, function(x){
	return(c(x, rep(NA, times=16-length(x))))
})

grouped.stills<-data.frame(matrix(unlist(grouped.stills), byrow=T, ncol=16))
nrow(grouped.stills)
#

still.manifest<-data.frame(still.manifest, grouped.stills)
colnames(still.manifest)[3:18]<-c("image1","image2","image3","image4","image5","image6","image7","image8","image9","image10","image11","image12","image13","image14","image15","image16")

#if none of them have 16 pictures, omit the last column
ncol(still.manifest)
#18
if(sum(!is.na(still.manifest$image16))==0){
	still.manifest<-still.manifest[,-ncol(still.manifest)]
}
ncol(still.manifest)
#18

#remove the first part of the path from the clip id
still.manifest$full.video.name<-gsub(still.manifest$full.video.name, pattern="path/", replacement="") 
#remove the match.id
still.manifest<-still.manifest[,-which(colnames(still.manifest)=="match.id")]

#make site name
still.manifest$site.name<-"site name"
#put site name as col 2
still.manifest<-still.manifest[,c(18,1:17)]
#put # before the colnames that need to be hidden
colnames(still.manifest)[2]<-"clip.id"
colnames(still.manifest)[2:ncol(still.manifest)]<-paste("#", colnames(still.manifest)[2:ncol(still.manifest)], sep="")

#currently, zooniverse only lets you upload videos that have the same number of parts being uploaded
#for us, that means the same number of stills, so knowing how many columns there are is a way to know how many stills there are
#so all videos that have 3 stills will be uploaded together, and all that have 15 stills can be uploaded with each other
still.manifest$num.cols<-apply(still.manifest, MAR=1, function(x){sum(!is.na(x))})
man.table<-table(still.manifest$num.cols)
man.table



#make ID number based on order of clips
still.manifest<-still.manifest[order(still.manifest[,"#clip.id"]),]
still.manifest$id.num<-1:nrow(still.manifest)

#order based on numcols
still.manifest<-still.manifest[order(still.manifest$num.cols, still.manifest[,"#clip.id"]),]
rownames(still.manifest)<-NULL
#put ID number at beginning
ncol(still.manifest)
#20
still.manifest<-still.manifest[,c(20, 1:19)]
#add 1 to num.cols to account for ID num being added to df
still.manifest$num.cols<-still.manifest$num.cols+1

write.table(still.manifest, "path/sitename_complete_manifest_info.txt", sep="\t", row.names=F, col.names=T)

#break up manifests into lengths of 1000 with only videos included that have the same number of stills
manifest.length<-1000

#set up df that matches id num with manifest num
#this is so you know which manifest a video was uploaded with
id.manifest<-data.frame(id.num=NA, manifest.num=NA)
man.num<-1

start.time<-Sys.time()
for(num.cols in unique(still.manifest$num.cols)){
	sub.data<-still.manifest[still.manifest$num.cols==num.cols,1:num.cols]
	if(nrow(sub.data)<manifest.length){
        write.table(sub.data, paste("path/sitename_manifest_",man.num, ".csv", sep=""), sep=",", row.names=F, col.names=T)

		id.manifest<-rbind(id.manifest, data.frame(id.num=sub.data$id.num, manifest.num=man.num))
		man.num<-man.num+1
	} else {
		num.manifests.to.do<-ceiling(nrow(sub.data)/manifest.length)
		for(i in 1:num.manifests.to.do){
			if(i<num.manifests.to.do){	
				split.data<-sub.data[((i-1)*manifest.length+1):(i*manifest.length),]

                write.table(split.data, paste("path/sitename_manifest_",man.num, ".csv", sep=""), sep=",", row.names=F, col.names=T)

				id.manifest<-rbind(id.manifest, data.frame(id.num=split.data$id.num, manifest.num=man.num))
				man.num<-man.num+1
				
			} else {
				split.data<-sub.data[((i-1)*manifest.length+1):nrow(sub.data),]

                write.table(split.data, paste("path/sitename_manifest_",man.num, ".csv", sep=""), sep=",", row.names=F, col.names=T)

				id.manifest<-rbind(id.manifest, data.frame(id.num=split.data$id.num, manifest.num=man.num))
				man.num<-man.num+1
			}
		}
	}
}
end.time<-Sys.time()
end.time - start.time
#

#remove first row with NAs
id.manifest<-id.manifest[!is.na(id.manifest$id.num),] 

#how many total manifests were there
max(id.manifest$manifest.num)
#

write.table(id.manifest, "path/sitename_idnum_manifestnum.txt", sep="\t", row.names=F, col.names=T)


save.image("path/sitename_downsizing_manifest.RData")


