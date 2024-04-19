---
title: 'Bambulab P1S Braindump'
subtitle: 'A running history of issues and how I addressed them'
description: 'Notes I've taken for myself that others might find helpful as well'
date: 2024-04-19
featured_image: '/images/post/bambulab-p1s-with-ams.webp'
categories: 
  - "3DPrinting#P1S"
published: true
---

{% include toc.html %}

<h2>Error Message: HMS_WARN_07ff_2000_0002_0004</h2>

I have an AMS unit but wanted to print some TPU. While there are situations where TPU can be loaded into the AMS, it was easier (I thought) to just use the rear feeder. So I unhooked the AMS and loaded the TPU. No problem. After the print I went through the unload process, removing the TPU, and then I got this message.

For some people it seems benign - they just reboot the printer and update Bambu Studio to reset the external filament and then everything is great. But for me there was something else going on.

I tried a bunch of stuff (power cycling, hitting "Retry", even factory resetting the printer) but nothing would convince the printer that it was OK to print with the AMS. It would not even load filament if I did it manually.

To solve this, I loaded PLA in the rear and went through the complete load cycle, extruding until the new filament came out (there was still quite a bit of TPU in there). Then I went through the unload process again and finally I reset the rear filament in Bambu studio.

Once that was done, I was then able to load filament via the AMS and am back to printing.
