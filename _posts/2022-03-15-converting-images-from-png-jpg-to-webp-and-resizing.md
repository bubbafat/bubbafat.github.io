---
title: 'Batch Converting JPG and PNG to WebP and Resizing for the Web'
subtitle: 'Using the cwebp tool to automate image optimization on individual files and entire folder structures'
description: 'Using the cwebp tool to automate image optimization on individual files and entire folder structures'
date: 2022-03-15
featured_image: '/images/post/2022-03-15/matt-artz-7_zxKAWCDQI-unsplash.webp'
categories: 
  - "Programming#Misc"
---

I'm not overly concerned with SEO or my blog's performance but there is no need to make someone download a 350k image when a 17k one will do. To make that happen I have converted all my images from their previous png and jpg formats, and using Google's WebP format.  I've been doing this using the [cwebp](https://developers.google.com/speed/webp/docs/cwebp) command line tool.

My goals were to:

1. Convert all jpg and png files to webp
2. Resize all those files to a maximum width of 800 (maintaining aspect ratios)
3. Batch up #1 and #2 for entire directory structures

<h2>Converting a Single File</h2>

Converting a single file to webp using cwebp is simple:

```zsh
cwebp infile.png -o outfile.webp
```

This converts the png file to webp using the [defaults](https://developers.google.com/speed/webp/docs/cwebp).

```zsh
cwebp -resize 800 0 infile.webp
```

This resizes the webp file to a maximum width of 800 with 0 indicating "maintain the aspect ratio for the height".

And you can put these both together like:

```zsh
cwebp -resize 800 0 infile.png -o outfile.webp
```

<h2>Batch Converting a Directory</h2>

Now that we can convert a single file, I used a combination of find, grep, sed, and xargs to do what I needed for the directory.  This example converts and resizes all the png files in a directory structure:

```zsh
find . * | grep -E "(png)" | sed 'p;s/\.png/\.webp/' | xargs -n2 bash -c 'cwebp -resize 800 0 $0 -o $1'
```

Basically the pipeline is this:

1. Find all the files in the current directory and it's children
2. Use grep to filter on the png files
3. Use sed to output both the original name (infile.png) and the new name (infile.webp) on seperate lines
4. Use xargs specificying "-n2" to say "read two lines"
5. Kick off a bash script providing both args calling cwebp

This seems super hacky but it works and that's all I really cared about.  If you can improve on this then please feel free to [submit a PR to this blog post](https://github.com/bubbafat/bubbafat.github.io/blob/main/_posts/2022-03-15-converting-images-from-png-jpg-to-webp-and-resizing.md) to help (really, please do!)

When this runs each png file in the directory tree will have a cooresponding webp at the same location and that webp file will have been resized to a max width of 800px.

Now my steps were:

1. Delete the original files (I used the same command as above but replaced the bash command with an `rm` command)
2. Mass-update the blog posts (I used Find/Replace in [Sublime](https://www.sublimetext.com/))
3. Run jekyll and test

And I think the outputs speak for themselves.  Here is an example input image:

{% include image.html url="/images/post/2022-03-15/matt-artz-7_zxKAWCDQI-unsplash.jpg" description="A black c-clamp on a black background" unsplash_profile="mattartz" unsplash_name="Matt Artz" %}


And here is the WebP compressed and resized version

{% include image.html url="/images/post/2022-03-15/matt-artz-7_zxKAWCDQI-unsplash.webp" description="A black c-clamp on a black background" unsplash_profile="mattartz" unsplash_name="Matt Artz" %}

The original file was 338712 bytes and the webp version is 16544 bytes - roughly 95% savings with very little change in the overall visual experience (as needed for a personal blog).

