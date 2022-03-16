---
title: 'Creating Animated WebP Files Using img2webp'
subtitle: 'Is it pronounced jwebp or gwebp?'
description: 'Animating collections of images into the animated webp format'
date: 2022-03-17
featured_image: '/images/post/gen100.webp'
categories: 
  - "Programming#Web"
---

I frequently find myself needing to create videos from a series of rendered frames. I have been using ffmpeg to generated mp4's but it would be nice to be able to create GIF's - or in this case animated WebP files.  The biggest benefits of this are that I don't need to rely on an external hosting service for the video and that they can be used anywhere an image is used.

To do this I'll be using [img2webp](https://developers.google.com/speed/webp/docs/img2webp) from the Google WebP image format tools.

Say I have a collection of images named in a sortable manner - for example:


    frame-001.png    frame-002.png   frame-003.png   ...
    ...
    frame-075.png    frame-076.png   frame-077.png   ...
    ...
    frame-298.png    frame-299.png   frame-300.png

In this case I want to create a 10 second video (30 FPS) with the 300 frames listed above.

{% include tip.html content="The input files must be lexically sorted for img2web to properly order them. A simple way to do this is using leading zeros." %}

The arguments I'll use are:

| argument | value | meaning |
|----------|-------|---------|
| -d       | 33    | 33 ms per frame (1000 / 30) |
| -loop    | 0     | Loops (0 is continuous) |
|          | frame-* | The file pattern (leading zero matters for sorting) |
| -o       | gen100.webp | Output file name | 


```zsh
img2webp -d 33 -loop 0 frame-* -o gen100.webp
```

{% include tip.html content="It is not possible to resize an animated webp file using cwebp. If you want to resize the output you can either resize the inputs before creation or use `webpmux` to extract the individual frames from the animated webp file." %}

And the output is an animated WebP image file shown here:

{% include image.html url="/images/post/gen100.webp" description="An example animated WebP file" %}

