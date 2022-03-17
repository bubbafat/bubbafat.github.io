---
title: 'Generating an MP4 Video From a Directory of Images'
subtitle: 'Using find, xargs, and ffmpeg to create an mp4 video from a folder of images'
date: 2022-03-08
description: 'Using find, xargs, and ffmpeg to create an mp4 video from a folder of images'
featured_image: '/images/post/2022-03-10/preview.webp'
categories: 
  - "Programming#Web"
---

{% include toc.html %}

As part of a personal project I've been toying with I wanted to be able to generate images using a naming convention where the image name would describe the frame order.  For example: `frame-1.png`, `frame-2.png`, ... `frame-100.png`. 

The directory the frame images were stored in were, themselves, named according to an ordered convention as well.  The entire structure would look like:

    + gen-1
        - frame-1.png
        - frame-2.png
        - frame-3.png
    + gen-50
        - frame-1.png
        - frame-2.png
        - frame-3.png
    + gen-100
        - frame-1.png
        - frame-2.png
        - frame-3.png

This structure would serve as the input to a tool that will create 3 videos: gen-1.mp4, gen-50.mp4, and gen-100.mp4 each of which are 3 frames long (in reality the videos are longer but it just means more frame files).

I'm not the command line guru so this is the approach I took to figure this out:

<h2>Generate a Video From a Directory of Images</h2>

I started by trying to find a [command](https://ffmpeg.org/ffmpeg.html), using [ffmpeg](https://ffmpeg.org/), that would create a video for the directory gen-1 with the following properties:

| Feature | Desired |
|---------|---------|
| FPS     | 30      |
| Format  | mp4     |
| Resolution | 1280x1280 |

```zsh
ffmpeg -framerate 30 -f image2 -s 1280x1280 -i gen-1/frame-%d.png -vcodec libx264 -pix_fmt yuv420p -y gen-1.mp4
```

| Argument | Value | Outcome |
|----------|-------|---------|
| -framerate | 30 | Sets the framerate to 30 |
| -f | image2 | The input format is images |
| -s | 1280x1280 | The frame size of the input images |
| -i | gen-1/frame-%d.png | The input frame images |
| -vcodec | libx264 | Encodes using the x264 video format (mp4) |
| -pix_fmt | yuv420p | Without this the video [wouldn't load in Quicktime](https://superuser.com/a/820137) | 
| -y | _none_ | Overwrites output files |
| gen-1.mp4 | _none_ | The name of the output file |

<h2>List all the Directories</h2>

```zsh
find gen-* -type d
```

This command will identify the three directories following the convention described above.

```zsh
% find gen-* -type d 
gen-1
gen-50
gen-100
```
<h2>Tie Them Together</h2>

Now that I can produce the video - but need the directory names - and I can produce a list of directory names, I want to tie them together.

```zsh
find gen-* -type d | xargs -I {} echo {} 
gen-1
gen-50
gen-100
```

I used xargs to pipe the output of find into the input of xargs and then kick off the child process - in this example simply echoing the inputs.

So let's stitch them together

```zsh
find gen-* -type d | xargs -I {} ffmpeg -framerate 30 -f image2 -s 1280x1280 -i {}/frame-%d.png -vcodec libx264 -pix_fmt yuv420p -y {}.mp4
```

And the output is a video that does exactly what I had hoped.

<iframe src="https://player.vimeo.com/video/686693998" width="640" height="640" frameborder="0" allowfullscreen></iframe>


