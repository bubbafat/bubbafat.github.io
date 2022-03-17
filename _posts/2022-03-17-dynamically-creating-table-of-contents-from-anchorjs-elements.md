---
title: 'Dynamically Creating a Table of Contents Using AnchorJS Elements'
subtitle: 'Creating a table of contents from h2 and h3 headers'
description: 'Integrating AnchorJS and dynamic table of content creation into my Jekyll blog hosted on GitHub pages'
date: 2022-03-17
featured_image: '/images/post/tableofcontents.webp'
categories: 
  - "Programming#Web"
---

{% include toc.html %}

When I'm creating blog posts that describe a process I like to use headers to seperate the main steps. I use &lt;h2&gt; for my groups and &lt;h3&gt; for sub-topics. So when I recently began using [Bryan Braun's](https://twitter.com/BryanEBraun) [AnchorJS](https://www.bryanbraun.com/anchorjs/) to automatically create linkable anchors, it did not take long for me to want to also add a table of contents based on those anchors.

<h2>Using AnchorJS</h2>

After looking at a few options I landed on [AnchorJS](https://www.bryanbraun.com/anchorjs/) because I wanted something simple that "just worked" and did not require any Jekyll plugins.

I wanted to add the AnchorJS reference right at the end of the body section so I opened `_layouts/default.html` and [added the following](https://github.com/bubbafat/bubbafat.github.io/blob/9ad78db0e23f0691eac0aa949f7f3e7d9534416d/_layouts/default.html#L124) before &lt;/body&gt;

```html
<script src="https://cdn.jsdelivr.net/npm/anchor-js/anchor.min.js"></script>
```

Next I added a file named [`js/anchorjs-toc.js`](https://github.com/bubbafat/bubbafat.github.io/blob/9ad78db0e23f0691eac0aa949f7f3e7d9534416d/js/anchorjs-toc.js) and I added the following content to is:

```js
anchors.options.visible = 'always';
anchors.add('h2, h3');
```

And in [`_layouts/default.html`](https://github.com/bubbafat/bubbafat.github.io/blob/9ad78db0e23f0691eac0aa949f7f3e7d9534416d/_layouts/default.html#L125) I added the following immediately after the anchor.min.js included a moment ago.

```html
<script src="{{ '/js/anchorjs-toc.js' | relative_url }}"></script>
```

I choose these options because I like the look of always-on link icons and I only wanted these on the h2 and h3 headers. The post/page title in the hero area is the h1 header and h4+ are too granular.


<h3>Optional: Disabling Dynamic Content Rendering</h3>

Sometimes the header links would render and sometimes they wouldn't. I set a breakpoint and found that sometimes the content was rendered already (allowing anchorjs to find the headers) and sometimes it wasn't (so it couldn't). Assuming it was some lazy rendering thing, I found the `_data/settings.yml` [ajax_loading](https://github.com/bubbafat/bubbafat.github.io/blob/9ad78db0e23f0691eac0aa949f7f3e7d9534416d/_data/settings.yml) setting was `true` so I changed it to `false` and now it was reliably rendering anchor links correctly. 

This is likely only relevant to Jekyll and possibly only to the theme I'm using. Just keep it in mind, though, if your links are not rendering consistently.

<h2>Creating the Table of Contents</h2>

<h3>Using JavaScript to Generate the Content</h3>

Now that AnchorJS is installed and working correctly, I followed the [jsfiddle](https://jsfiddle.net/bryanbraun/nc6rL9hk/) template that Bryan created as an example. I copied the functions `generatedTableofContents` and `addNavItem` into the [js/anchor-toc.js](https://github.com/bubbafat/bubbafat.github.io/blob/9ad78db0e23f0691eac0aa949f7f3e7d9534416d/js/anchorjs-toc.js) file I created in the previous steps (I did change them slightly - see the note below the code).

I also added a call to [generateTableOfContents](https://github.com/bubbafat/bubbafat.github.io/blob/9ad78db0e23f0691eac0aa949f7f3e7d9534416d/js/anchorjs-toc.js#L3).

The contents of the file are:

```js
anchors.options.visible = 'always';
anchors.add('h2, h3');
generateTableOfContents(anchors.elements);

// External code for generating a simple dynamic Table of Contents
function generateTableOfContents(els) {
  var anchoredElText,
      anchoredElHref,
      ul = document.createElement('UL');

  document.getElementById('table-of-contents').appendChild(ul);

  for (var i = 0; i < els.length; i++) {
    anchoredElText = els[i].textContent;
    anchoredElHref = els[i].querySelector('.anchorjs-link').getAttribute('href');
    addNavItem(ul, anchoredElHref, anchoredElText, 'toc-li-'.concat(els[i].tagName));
  }
}

function addNavItem(ul, href, text, listClass) {
  var listItem = document.createElement('LI'),
      anchorItem = document.createElement('A'),
      textNode = document.createTextNode(text);
      listItem.className = listClass;
  
  anchorItem.href = href;
  ul.appendChild(listItem);
  listItem.appendChild(anchorItem);
  anchorItem.appendChild(textNode);
}
```

Please note that I have made two changes to this file.

1. I changed [line 16](https://github.com/bubbafat/bubbafat.github.io/blob/9ad78db0e23f0691eac0aa949f7f3e7d9534416d/js/anchorjs-toc.js#L16) to pass `'toc-li-'.concat(els[i].tagName)` as a fourth parameter to addNavItem. This string will be toc-li-h2 or toc-li-h3 depending on whether we are at an h2 or h3 tag.
2. I added [line 24](https://github.com/bubbafat/bubbafat.github.io/blob/9ad78db0e23f0691eac0aa949f7f3e7d9534416d/js/anchorjs-toc.js#L24) `listItem.className = listClass;` to addNavItem

These two changes ensure that the list items for the table of content include different classes for h2 and h3 tags. I'll be using this information to format them differently.

<h3>Creating the Table of Contents Include</h3>

Knowing I would want a repeatable ToC, I next added `_includes/toc.html` which contains the following:

```html
<div id="table-of-contents">
  <div id="toc-header">On this page:</div>
</div>
```

And this can be included on [any page](https://github.com/bubbafat/bubbafat.github.io/blob/126832a92b787a757979edbed90804c4ab944417/_posts/2022-03-17-creating-animated-webp-files-using-img2webp.md?plain=1#L11) like so:

```liquid
{% raw %}
{% include toc.html %}
{% endraw %}
```

Now when page is rendered, the table of contents display - but they are kind of ugly.

<h2>Formatting the Table of Contents</h2>

To format the table of contents I started with the example [here](https://www.tipsandtricks-hq.com/simple-table-of-contents-toc-using-pure-html-and-css-code-9217) and created the file [`_scss/_includes/_toc.sccs`](https://github.com/bubbafat/bubbafat.github.io/blob/9ad78db0e23f0691eac0aa949f7f3e7d9534416d/_sass/_includes/_toc.scss) which included the following:

```css
#table-of-contents {
    float: right;
    background: #f9f9f9 none repeat scroll 0 0;
    border: 1px solid #aaa;
    display: table;
    margin: 20px 1em 1em 1em;
    padding: 10px;
    width: auto;
    max-width: 35%;
}

#toc-header {
    font-weight: 900;
    font-size: 125%;
}

#table-of-contents li, #table-of-contents ul, #table-of-contents ul li{
    list-style: outside none none !important;
}

li.toc-li-H2 {
    margin-left: 1em;    
}

li.toc-li-H3 {
    margin-left: 2em;
    font-size: 85%;
    filter: brightness(2.50);
}
```

Basically, this creates a box in which the table of contents will exist. It floats that box to the right and applies some margin and padding. Next, I apply some formatting to the "On this page" header.  Finally, I use the li's class (toc-li-h2 or toc-li-h3) to conditionally format the entries to approximate the look of a hierarchy. The h2's are slightly larger than the h3's, the h3's are slightly more indented, and the h3's are slightly lighter in color.

I also needed to make sure this file was included when the site was built so I added:

```sass
@import "_includes/toc"
```

To [css/style.scss](https://github.com/bubbafat/bubbafat.github.io/blob/9ad78db0e23f0691eac0aa949f7f3e7d9534416d/css/style.scss#L139).

And this pulled it all together into a table of contents that renders the way I preferred.

{% include image.html url='/images/post/tableofcontents.webp' description='Screenshot of the table of contents' %}

