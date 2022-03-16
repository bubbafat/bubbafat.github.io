---
title: 'Migrating From WordPress to Jekyll on Github Pages'
subtitle: ''
date: 2022-03-08
categories: 
  - "Programming#Web"
description: 'An overview of the steps I took to migrate from self-hosted Wordpress to Jekyll on Github pages.'
featured_image: '/images/unsplash/c-dustin-91AQt9p4Mo8-unsplash.webp'
---

After hosting for WordPress on Digital Ocean for years, I've decided to move to Jekyll on Github Pages.  The decision came down a few key points:

1. My content is infrequently updated and static - Wordpress was overkill
2. It took more time to keep Wordpress and all the plugins updated than I spent on writing
3. It's $11/month I don't need to pay
4. Anyone who finds a mistake is welcome to [submit a PR](https://github.com/bubbafat/bubbafat.github.io)

The migration was pretty straight-forward once I understood what to do.

My process was:

- Export WordPress to XML
- Create github.io repo
- Install Ruby 2.7 (that specific version)
- Install Jekyll
- Convert WordPress XML to Jekyll-compatible markdown
- Pick a Jekyll theme
- Update DNS
- Generate and install certificate
- Defaulting to the blog instead of projects
- Wiring up Google Analytics
- Make changes and keep on pushing
- Setting up Auto-Publishing Future Posts

<h3>Export WordPress to XML</h3>

My first attempt was to use the [Jekyll Exporter](https://wordpress.org/plugins/jekyll-exporter/). It installed without issues but would not run successfully - every time I'd work through one issue there would be a another. I finally gave up on this one:

    Fatal error: Uncaught Error: Class 'ZipArchive' not found in /var/www/roberthorvick.com/wp-content/plugins/jekyll-exporter/jekyll-exporter.php:420 
    Stack trace: 
    #0 /var/www/roberthorvick.com/wp-content/plugins/jekyll-exporter/jekyll-exporter.php(447): Jekyll_Export->zip_folder() 
    #1 /var/www/roberthorvick.com/wp-content/plugins/jekyll-exporter/jekyll-exporter.php(337): Jekyll_Export->zip() 
    #2 /var/www/roberthorvick.com/wp-content/plugins/jekyll-exporter/jekyll-exporter.php(102): Jekyll_Export->export() 
    #3 /var/www/roberthorvick.com/wp-includes/class-wp-hook.php(307): Jekyll_Export->callback() 
    #4 /var/www/roberthorvick.com/wp-includes/class-wp-hook.php(331): WP_Hook->apply_filters() 
    #5 /var/www/roberthorvick.com/wp-includes/plugin.php(474): WP_Hook->do_action() 
    #6 /var/www/roberthorvick.com/wp-admin/includes/class-wp-screen.php(421): do_action() 
    #7 /var/www/roberthorvick.com/wp-admin/includes/screen.php(243): WP_Screen->set_current_screen() 
    #8 /var/www/roberthorvick.com/wp-admin/admin.php(212): set_current_screen() 
    #9 /var/w in /var/www/roberthorvick.com/wp-content/plugins/jekyll-exporter/jekyll-exporter.php on line 420
    There has been a critical error on this website. Please check your site admin email inbox for instructions.

The fix was going to involve not just updating what PHP features were enabled, but actually re-compiling PHP to support zip. No thanks.

Instead I simply exported to XML and decided I would revisit the XML->Markdown problem later.

The steps for this are to

1. Log into your WP Admin Dashboard
2. Hover on Tools and select 
3. Choose "All content" (this is the default)
4. Click "Download Export File"

Take note of the name - we'll call it roberthorvick-export.xml for now.

<h3>Create github.io repo</h3>

1. Log in to github
2. Create a public repo named "bubbfat.github.io" - [follow these instructions](https://pages.github.com/).

<h3>Install Ruby 2.7</h3>

I use [Homebrew](https://brew.sh/) on MacOS so I typed:

```zsh
brew install ruby@2.7
```

Then I modified my .zshrc file to include:

```zsh
export PATH="/usr/local/lib/ruby/gems/2.7.0/bin:/Users/robert/.gem/ruby/2.7.0/bin:$PATH"
```

I then reloaded my shell and confirmed the correct version of ruby was being used.

<h3>Install Jekyll</h3>

With Ruby installed it was time to install Jekyll.  I followed the instructions on the [Jekyll homepage](https://jekyllrb.com/).

My commands were:

```zsh
gem install bundler Jekyll
jekyll new bubbafat.github.io
```

I then tested by going into that directory and executing:

```zsh
bundle install
bundle exec jekyll serve
```

The output (ignoring warnings) was:

	done in 2.07 seconds.
	Auto-regeneration: enabled for '/Users/robert/src/bubbafat.github.io'
	Server address: http://127.0.0.1:4000
	Server running... press ctrl-c to stop.

Then I opened a browser and headed to http://127.0.0.1:4000 to check that things were good (they were).

<h3>Convert WordPress XML to Jekyll-compatible markdown</h3>

I followed the instructions on [Kev Quirk's blog](https://kevq.uk/how-to-convert-wordpress-to-markdown/). It suggests using [wordpress-export-to-markdown](https://github.com/lonekorean/wordpress-export-to-markdown).

My steps were:

1. Clone https://github.com/lonekorean/wordpress-export-to-markdown
2. Copy roberthorvick-export.xml from Downloads to the wordpress-export-to-markdown
3. Rename roberthorvick-export.xml to export.xml
4. Run `node index.js --post-folders=false --prefix-date=true --wizard=false`
5. Copy the output to the Jekyll \_posts directory.

The command to use was documented in the [README.md](https://github.com/lonekorean/wordpress-export-to-markdown/blob/master/README.md) - so big thanks to [Will Boyd](https://twitter.com/lonekorean) for not just creating the tool but also making it easy to use with Jekyll.

<h3>Pick a Jekyll theme</h3>

Jekyll ships with a default theme which was fine to make sure things were working but I wanted something a little better so I googled "jekyll themes" and ended up on [Jekyll Themes](https://jekyllthemes.io/).

I picked the theme I liked, paid, downloaded and followed the instructions in it's README.  It was basically "copy it into your repo and set some basic config"

So I did.

There was an issue there the index.markdown references a layout that does not exist (read your warnings when starting Jekyll). I don't think I need index.markdown so I removed it and let index.html do the heavy lifting. Seems to be working fine.

<h3>Update DNS</h3>

My domain is registered through [namecheap.com](https://namecheap.com) and used Cloud Flare (free) for caching.  To reconfigure my domain I needed to 

1. Change the DNS on namecheap back to their BasicDNS setting
2. Removed the domain from Cloud Flare
3. Create a CNAME record as described [here](https://docs.github.com/en/pages/configuring-a-custom-domain-for-your-github-pages-site/managing-a-custom-domain-for-your-github-pages-site)
4. Create the 4 A Record entries defined [here](https://docs.github.com/en/pages/configuring-a-custom-domain-for-your-github-pages-site/managing-a-custom-domain-for-your-github-pages-site)
5. Wait about an hour for everything to update

Once this was done [{{ site.url }}]({{ site.url }}) began properly serving the Jekyll content.

<h3>Generate and install certificate</h3>

Once the website was up and running at the correct domain, I wanted to install a cert so I could enforce https.

1. Open the repo in github (while logged in)
2. Click on "Settings"
3. Click on "Pages" (URL is https://github.com/<user>/<repo>/settings/pages)
4. Click on Enforce HTTPS.
5. Wait ... wait ... wait

Eventually it just started working.

<h3>Wiring up Google Analytics</h3>

I followed the instructions on [Michael Lee's blog](https://michaelsoolee.com/google-analytics-jekyll/).

<h3>Defaulting to the blog instead of projects</h3>

The template I choose defaulted the root URL to projects, not blog posts.  I wanted the opposite.  So I basically did:

```zsh
mkdir projects
mv index.html projects
mv blog/index.html .
```

Then edited the \_data\settings.yml to point the Blog menu to / (root) and the Projects menu to /projects/ - now it rendered the way I wanted.

<h3>Make changes and keep on pushing</h3>

With the theme applied I started making changes. Things like 

- Updating all the image references to the /images folder instead of _posts/images (the output from the exporter tool).
- Updating the hero images
- Updating the post preview images that show up in the index pages and as the hero image for the post
- Created a category page with multi-level categories.

That last one was kind of hacky.  I wanted to be able to create a template where I could have all the posts in a specific category be displayed but related posts should be together. I don't know anything about how this _should_ be done but what I did was create post categories that look like this:

```yaml
categories: 
  - "Programming#Erlang"
```

The idea is that this is the "Programming" category and the "Erlang" sub-category.

Then I used the idea for categorized post lists described [here](https://blog.webjeda.com/jekyll-categories/).  But that displays every post for every category.  I made some changes.

For example my page title is:

```yaml
title: Programming
```

Then I modified the suggested page source (from the linked article) to be this:

```html
{% raw %}
<div id="archives">
  
{% for category in site.categories %}
  <div class="archive-group">
    {% capture category_name %}{{ category | first }}{% endcapture %}
    {% if category_name contains page.title %}
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
{% endraw %}
``` 

There are few changes that matter:

1. I added a condition that makes sure the category contains the page.title (in this example "Programming"). Yes, it should be a startswith but I haven't figured that out in Liquid yet. I gave this 5 minutes of effort.
2. I assigned a new variable named subcategory which splits on "#" and picks the second word ("Programming#Erlang" -> "Erlang")
3. I display the subcategory but continue using the category_name in links and slugs
4. I print the date the post was written with the title

<h3>Setting up Auto-Publishing Future Posts</h3>

And that's where I am now.  If I do anything else interesting I will add it here.

<hr/>

<div>
	Photo by <a href="https://unsplash.com/@dianamia?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText">C Dustin</a> on <a href="https://unsplash.com/s/photos/construction?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText">Unsplash</a>
</div>