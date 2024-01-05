##### script 1 for preparing Chimp&See videos for upload to Zooniverse
#get video lengths
#this is necessary for knowing how many clips to split the videos into

import glob
import pandas as pd
import csv
import subprocess
import os
import numpy as np


def get_video_length(filename):

    output = subprocess.check_output(("path/ffprobe.exe",
                                      "-v", "error", "-show_entries", "format=duration", "-of", "default=noprint_wrappers=1:nokey=1", filename)).strip()
    video_length = float(output)
    print("Video length in seconds: "+str(video_length))

    return video_length


local_dir = r"path/1_sitename_downloaded"

os.chdir(local_dir)
#find all file names for all file types
filenames1 = glob.glob(local_dir + "/**/*.AVI", recursive=True)
filenames2 = glob.glob(local_dir + "/**/*.ASF", recursive=True)

filenames = filenames1 + filenames2

len(filenames)

# sort by name
filenames.sort()

# check function
get_video_length(filenames[0])


# get the video duration per file
df = pd.DataFrame([])
for f in filenames:
    try:
        df = df.append([get_video_length(f)])
    except:
        df = df.append([np.nan])
        pass

# add the paths to the df
df = df.assign(path=filenames)

df.to_csv("path/sitename_video_lengths.csv", sep=",")


