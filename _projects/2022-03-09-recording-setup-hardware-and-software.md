---
title: 'DRAFT: Audio Recording Setup'
subtitle: 'My current audio recording configuration for Pluralsight, streaming, and music.'
date: 2022-03-09
description: 'My current audio recording configuration and related posts.'
featured_image: '/images/archive/screenshot1.jpg'
---

<h2>This is an unfinished draft</h2>



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