##### script 8 for preparing Chimp&See videos for upload to Zooniverse
#upload videos using the CLI

#To mount the network drive in WSL
# sudo mkdir /mnt/[your drive]
#You only ever have to do this part once. This is the mount point.

### we do this in cmdr
wsl

sudo mount -t drvfs V: /mnt/v 

cd "/mnt/[path to folder]" 

#do this for the first runthrough, with only consecutive manifests to try
#if there were 14 total manifests, it would look like this
for i in {1..14}; do
    if nc -zw1 google.com 443; then
      echo "we have connectivity"
	  echo "Uploading manifest $i"
	  panoptes subject-set upload-subjects xxxxx ./sitename_manifest_$i.csv
	else
	  echo "Check that manifest $i uploaded" &>> upload_failed.txt
	  sleep 30s
    fi
done




#do this if there are nonconsecutive numbers to try once the initial runthrough is done
#sometime the upload fails, either partway through or at the beginning
#you  may need to determine which videos have already been uploaded and remove them from the corresponding manifest before trying that manifest again
#if manifests 2, 5, and 8 failed then the second round of uploading would look like this
manifests=( 2 5 8)
for i in "${manifests[@]}"; do
    if nc -zw1 google.com 443; then
      echo "we have connectivity"
	  echo "Uploading manifest $i"
	  panoptes subject-set upload-subjects xxxx ./sitename_manifest_$i.csv
	else
	  echo "Check that manifest $i uploaded" &>> upload_failed.txt
	  sleep 30s
    fi
done



##################################
##################################
#see which subjects are not uploaded
#THIS IS IN R
#need to export the subjects file

subjects<-read.table("path/chimp-and-see-subjects.csv", sep=",", header=T)
nrow(subjects)
#
subjects<-subjects[subjects$subject_set_id==xxxxx,]
ids<-subjects$metadata
ids<-strsplit(as.character(ids), split=",")
ids<-unlist(lapply(ids, function(x){x[which(grepl(x, pattern="id.num"))]}))
ids<-strsplit(as.character(ids), split=":")
ids<-unlist(lapply(ids, function(x){x[2]}))
ids<-gsub(ids, pattern="\"", replace="")
ids<-as.numeric(ids)
length(ids)
#17533

table(table(ids))
#     1 
# 17553 
#no duplicates

######################## start duplicates section
#create file with iDs of duplicates for deletion
#this may not be necessary if there are no duplicates
subjects$id<-ids
id.table<-table(ids)
dups<-subjects[subjects$id %in% names(id.table[id.table==2]),]
nrow(dups)
#
dups<-dups[duplicated(dups$id),]
nrow(dups)
#
write.table(dups$subject_id, "path/subjects_to_delete_because_of_duplicates.txt", sep="\t", col.names=F, row.names=F)
######################## end duplicates section

#get remaining clips
id.man<-read.table("path/sitename_idnum_manifestnum.txt", sep="\t", header=T, stringsAsFactors=F)
nrow(id.man)
#

ids.remaining<-id.man[!id.man$id.num %in% ids,]
nrow(ids.remaining)
#

#this shows you how many are not yet uploaded in each manifest
table(ids.remaining$manifest.num)


#take out the videos that were already uploaded from relevant manifests
for(manifest.num in unique(ids.remaining$manifest.num)){

	sub.manifest<-read.table(paste("path/sitename_manifest_", manifest.num, ".csv", sep=""), sep=",", header=T) 

	#remove rows that have already been uploaded
	sub.manifest<-sub.manifest[sub.manifest$id.num %in% ids.remaining$id.num[ids.remaining$manifest.num==manifest.num],]

	## RENAME COLUMNS BECAUSE THE #S AREN'T READ IN PROPERLY
	colnames(sub.manifest)<-gsub(colnames(sub.manifest), pattern="X.", replacement="#")

    write.table(sub.manifest, paste("path/sitename_manifest_", manifest.num, ".csv", sep=""), sep=",", row.names=F, col.names=T) 
}


##############################
# remove duplicated id.nums if necessary
# this is done in the CLI

panoptes subject-set remove-subjects -f "/mnt/path/subjects_to_delete_because_of_duplicates.txt" xxxxx







