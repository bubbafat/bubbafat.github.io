---
title: 'Audio Recording Setup'
subtitle: 'My current audio recording configuration for Pluralsight, streaming, and music.'
date: 2022-03-09
description: 'My current audio recording configuration and related posts.'
featured_image: '/images/projects/recording/dbx286s.webp'
---

I do a meaningful amount of audio recording for personal and professional projects - notably my work with Pluralsight. When this topic comes up one of the first questions is usually "What is your setup like" so I thought it might be useful (for myself, even) to document my current configuration. I'll try and keep this up to date over time.


<h3>Signal Chain</h3>

![dbx286s and Saphire 2i2](/images/projects/recording/dbx286s.webp)

| Device | Connects To |
|--------|-------------|
| Shure SM7B | dbx286s |
| dbx286s | Focusrite Saphire 2i2 |
| Focusrite Saphire 2i2 | TESmart Dual Monitor KVM |
| TESmart Dual Monitor KVM | Windows 10 PC or Macbook |
| Cables | Sweetwater XLR-XLR |
| Cables | Sweetwater 1/4" TRS |


<h3>Computer</h3>

| Part | Model |
|------|-------|
| CPU  | Intel i7-12700K |
| RAM  | Corsair Vengenance LPX 32GB (2x16) |
| GPU  | EVGA GeForce RTX 3050 XC GAMING (08G-P5-3553-KR) |
| Motherboard | MSI Pro Z690-A Wifi DDR4 |
| Storage | Samsung 970 EVO+ SSD (2TB) |
| PSU  | Seasonic FOCUS PX-750 80+ Platinum |
| Case | be quiet! Silent Base 802 Black Mid-Tower |
| Cooling | Artic Liquid Freezer II 280 AIO |
| Monitor 1 | ASUS VP249 |
| Monitor 2 | Dell SE2419HX |
| Camera | Elgato Facecam |
| Keyboard | Corsair K55 (wired) |
| Mouse | Logitech M705 |
| Tablet | Wacom Intuos |
| Speakers | Amazon Basics Trash |

Also a 2017 Macbook Pro.

<h3>Physical Space</h3>

![Photo of my working space - desk, monitors and microphone](/images/projects/recording/room.webp)

| Part | Model |
|------|-------|
| Room | Spare bedroom - roughly 12'x16' with one exterior window |
| Desk | Uplift V2 48" Walnut Laminate |
| Monitor Arms | WALI Premium Single (GSDM001)
| Boom Arm | Rode PSA1 |
| Mic Extension Arm | Shure A26X 3" Extension Tube |
| Treatment | Gator Frameworks 12" Foam |
| Isolation | sE Electronics Reflecion Filter X Portable |

I have treatments and isolation available but with the dbx286s I have not needed them in my current space.


<div id="archives">
  
{% for category in site.categories %}
  <div class="archive-group">
    {% capture category_name %}{{ category | first }}{% endcapture %}
    {% if category_name contains "Recording" %}
    {% assign subcategory = category_name | split: "#" | slice: 1 %}
    <div id="#{{ subcategory | slugize }}"></div>
    <p></p>

    <h3 class="category-head">{{ subcategory }}</h3>
    <a name="{{ category_name | slugize }}"></a>
    {% for post in site.categories[category_name] %}
    <article class="archive-item">
      <h4><a href="{{ site.baseurl }}{{ post.url }}">{{post.title}}</a> ({{post.date | split: " " | slice: 0}})</h4>
    </article>
    {% endfor %}
  </div>
  {% endif %}

{% endfor %}

</div>