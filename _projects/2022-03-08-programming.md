---
title: 'Programming'
#subtitle: 'An index of my posts related to programming and software engineering.'
date: 2018-06-30 00:00:00
description: An index of my posts related to programming and software engineering.
featured_image: '/images/archive/screenshot1.jpg'
published: false
---


<div id="archives">
  
{% for category in site.categories %}
  <div class="archive-group">
    {% capture category_name %}{{ category | first }}{% endcapture %}
    {% if category_name contains page.title %}
    {% assign subcategory = category_name | split: "#" | slice: 1 %}
    <div id="#{{ subcategory | slugize }}"></div>
    <p/>
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