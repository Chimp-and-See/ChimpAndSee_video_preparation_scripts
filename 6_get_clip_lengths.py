##### script 6 for preparing Chimp&See videos for upload to Zooniverse
#get the lengths for all clips
#this is necessary because we adjust the bitrate to get the size down, and to calculate the bitrate we need the video duration

import glob
import pandas as pd
import csv
import subprocess
import os
import numpy as np

def get_video_length(filename):

    output = subprocess.check_output(("path/ffprobe.exe", "-v", "error", "-show_entries", "format=duration", "-of", "default=noprint_wrappers=1:nokey=1", filename)).strip()
    video_length = float(output)
    print("Video length in seconds: "+str(video_length))

    return video_length

#path where clips are located
local_dir = r"path"

os.chdir(local_dir)
filenames = glob.glob(local_dir + "/**/*.mp4", recursive=True)

len(filenames)
#

#sort by name
filenames.sort()

#check function
get_video_length(filenames[0])

#get the durations
df = pd.DataFrame([])
for f in filenames:
    try:
        df = df.append([get_video_length(f)])
    except:
        df = df.append([np.nan])
        pass

#add the paths to the df
df = df.assign(path=filenames)

df.to_csv("path/sitename_clip_lengths.csv", sep=",")




