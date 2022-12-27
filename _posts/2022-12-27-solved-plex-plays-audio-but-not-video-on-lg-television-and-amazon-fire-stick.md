---
title: 'Solved: Plex Video Plays Audio but Not Video on Amazon FireTV'
subtitle: 'Mass converting media to a common format'
description: 'A brief tutorial on how I bulk-converted media from various formats to a single, compatible, format to allow Plex to serve on my Amazon FireTV and LG television'
date: 2022-12-27
featured_image: '/images/post/sallyride.webp'
categories: 
  - "Programming#Web"
published: true
---

{% include toc.html %}

<h2>Why Doesn't it Work?</h2>

Over the holidays we made an effort to clean up our family movie collection. Cutting multi-hour videos into smaller chunks, catagorizing by people, renaming from MAH1982983.MPG to something sensible like "Evelyn is Sally Ride.MPG".

Along the way we noticed there were playback issues that seemed inconsistent across devices.  For example some files worked great everywhere

| Media  |  LG TV  |  FireTV |  Mac  |  PC  |
|--------|---------|---------|-------|------|
| Audio  | &#10004;|&#10004;|&#10004;|&#10004;|
| Video  | &#10004;|&#10004;|&#10004;|&#10004;|
| Preview | &#10004;|&#10004;|&#10004;|&#10004;|

But other videos were less consistent - they partially rendered or only rendered on some platforms

| Media  |  LG TV  |  FireTV |  Mac  |  PC  |
|--------|---------|---------|-------|------|
| Audio  | &#10004;|&#10004;|X|&#10004;|
| Video  | X|X|X|&#10004;|
| Preview | X|&#10004;|X|&#10004;|

This is obviously some issue where the video or audio codec is incompatible with the player but I didn't really want to spend any time thinking about it. So decided to bulk convert everything to H.264 MP4's with a 128bit AAC audio stream.

To do this I used [ffmpeg](https://ffmpeg.org/).

<h2>Converting a Single File</h2>

Here is an example of an [ffprobe](https://ffmpeg.org/ffprobe.html) output for one of the videos that would play audio, but not video, over the FireTV Plex client:

```
Input #0, mpeg, from 'Evelyn is Sally Ride.MPG':
  Duration: 00:00:33.80, start: 0.110000, bitrate: 2914 kb/s
  Stream #0:0[0x1c0]: Audio: mp2, 32000 Hz, mono, s16p, 64 kb/s
  Stream #0:1[0x1e0]: Video: mpeg1video, yuv420p(tv), 640x480 [SAR 1:1 DAR 4:3], 104857 kb/s, 25 fps, 25 tbr, 90k tbn
```

To convert this file to something that would play both video and audio, I ran the following command:


```
ffmpeg -i "Evelyn is Sally Ride.MPG" -c:v libx264 -preset slow -crf 20 -c:a aac -b:a 128k -vf format=yuv420p -movflags +faststart "converted\Evelyn is Sally Ride.mp4"
```

Which created an file whose ffprobe output is:

```
Input #0, mov,mp4,m4a,3gp,3g2,mj2, from 'Evelyn is Sally Ride.mp4':
  Metadata:
    major_brand     : isom
    minor_version   : 512
    compatible_brands: isomiso2avc1mp41
    encoder         : Lavf59.16.100
  Duration: 00:00:33.84, start: 0.000000, bitrate: 1645 kb/s
  Stream #0:0[0x1](und): Video: h264 (High) (avc1 / 0x31637661), yuv420p(progressive), 640x480 [SAR 1:1 DAR 4:3], 1509 kb/s, 25 fps, 25 tbr, 12800 tbn (default)
    Metadata:
      handler_name    : VideoHandler
      vendor_id       : [0][0][0][0]
  Stream #0:1[0x2](und): Audio: aac (LC) (mp4a / 0x6134706D), 32000 Hz, mono, fltp, 129 kb/s (default)
    Metadata:
      handler_name    : SoundHandler
      vendor_id       : [0][0][0][0]
```

And now when added back to the NAS and synced to Plex, everything works exactly as desired with no noticable change in audio or video quality.

<h2>Bulk Converting using ffmpeg</h2>

to do this in bulk, I wrote the following batch file:

```bat
if not exist converted mkdir converted
if exist converted\temp.mp4 del converted\temp.mp4

for %%I in (*) do (
  if exist "converted\%%~nI.mp4" del "converted\%%~nI.mp4"

  ffmpeg -i "%%I" -c:v libx264 -preset slow -crf 20 -c:a aac -b:a 128k -vf format=yuv420p -movflags +faststart converted\temp.mp4

  move converted\temp.mp4 "converted\%%~nI.mp4"
)
```

This is pretty straight-forward. 

Basically conversion is done for every file in a source folder and a temporary output file is created. Using a temp output file means I can kill the process mid-way through a conversation and I won't leave a partially rendered artifact in place. Should I have needed to cancel the operation, or if it failed, it would be easier to be able to restart the process and skip things that had already successfully converted.

