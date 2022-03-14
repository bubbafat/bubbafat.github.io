---
title: "Adding Hood.ie storage to the EmberJS TodoMVC app"
date: "2013-07-02"
categories: 
  - "hoodie"
featured_image: '/images/archive/emberjs1-700x300.jpg'
---

[Yesterday]({{ site.url }}/blog/todomvc-angularjs-hood-ie-60-minutes-to-awesome "TodoMVC (AngularJS) + Hood.ie = 60 minutes to awesome") I took the AngularJS TodoMVC example and converted it to using Hood.ie for data storage and making it multi-user in the process. I was pretty impressed. Not with myself - but rather that despite myself, the task wasn't too bad.

But there is this other kinda popular front-end MVC framework: [EmberJS](http://emberjs.com/ "This is where EmberJS comes from"). I was curious about Ember so I did a little reading. Watched a few videos. Witnessed an [absolute smackdown](http://vimeo.com/68215606 "Set aside an hour to watch this. "). Go watch that. It's better than my blog post - I promise you that.

#### The code and sample app

The code is available here: [https://github.com/bubbafat/hoodietodoemberjs](https://github.com/bubbafat/hoodietodoemberjs "This is where I put up a bunch of embarrassing hacks")

And there is a running version here: [http://bubbafat.github.io/hoodietodoemberjs/](http://bubbafat.github.io/hoodietodoemberjs/ "Github is the seriously the best")

### Wait ... a running version? Like with hood.ie and everything?

Yeah. I setup a hood.ie server on one of those $5 Digital Ocean systems they have been ad-spamming me about. Like they said, it took about 55 seconds to setup. Pretty slick.

_Oh, funny story ... Bank of America locked my debit card after running the Digital Ocean charge. Apparently using Digital Ocean is a fraud signal._

### So what did I do?

It came down to a few things.

- Copy the LocalStorage adapter and rename to HoodieAdapter
- Update the model to use the new adapter
- Change the adapter to write to hoodie instead of localstorage
- Add the user signin/up/out code from the [hood.ie template site](http://192.241.199.15:6001/ "I put a hood.ie template site up too") to index.html
- Spend a bunch of time learning how about 5% of [ember-data](https://github.com/emberjs/data "Caution: Contents may have shifted during shipping.") works

My changes (minus the rename) to [localstorage adapter](https://github.com/rpflorence/ember-localstorage-adapter "Read this code.  It is better.").

```js
  _getNamespace: function() {
    return this.namespace || 'HoodieAdaptor';
  },
  
  _getInstanceId: function() {
  return this._data.id || this.instanceId || 'default';
  },

  _loadData: function() {  
    var _this = this;
    _this._data = {};
  
    var def = $.Deferred();
  
    hoodie.store.find(this._getNamespace(), this._getInstanceId())
                .then(function(loaded) {
                    def.resolve(loaded);
                });
        
    def.done(function(data) { 
      _this._data = data;
    })
    .fail(function(err) { 
      throw err; 
    });
  },

  _didSaveRecords: function(store, type, records) {
    this._data = this._data || {}
  
    var promise = this._saveData();
    promise.done(function(saved) {
      store.didSaveRecords(records);
    })
	.fail(function(err) {
	  throw err;
	});
  },

  _saveData: function() {
    return hoodie.store.update(this._getNamespace(), this._getInstanceId(), this._data).promise();
  },
```

The login code can be seen here: [https://github.com/bubbafat/hoodietodoemberjs/blob/master/index.html#L12](https://github.com/bubbafat/hoodietodoemberjs/blob/master/index.html#L12 "There is no point in showing this in the blog post.")

I also added this code to the [Todos controller](https://github.com/bubbafat/hoodietodoemberjs/blob/master/js/controllers/todos_controller.js#L60 "This was not really a good idea.") to try and force a refresh when logging in/out. I'm not saying this was a good idea but it got me about 50% of the way to what I wanted.

```js
init: function() {
  var self = this;

  // user signed-up or signed-in
  hoodie.account.on('signin', function (user) {
    location.reload();
  });

  hoodie.account.on('signup', function (user) {
    location.reload();
  });

  // user has signed out
  hoodie.account.on('signout', function (user) {
    location.reload();
  });
},
````

### Conclusions?

At first I thought the port to EmberJS was more difficult than with Angular. I even said so.

<blockquote class="twitter-tweet"><p>Changing the <a href="https://twitter.com/search?q=%23emberjs&amp;src=hash">#emberjs</a> <a href="https://twitter.com/search?q=%23todomvc&amp;src=hash">#todomvc</a> app to use as <a href="https://twitter.com/hoodiehq">@hoodiehq</a> backend was more challenging than <a href="https://twitter.com/search?q=%23angularjs&amp;src=hash">#angularjs</a></p>— Robert Horvick (@bubbafat) <a href="https://twitter.com/bubbafat/statuses/351789098563940353">July 1, 2013</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

But then I started thinking about it - mostly because Tom Dale was all "Um, that should be easy" and I was all "It didn't feel easy" - yeah, I'm paraphrasing - he was quite generous to offer his time to help and I genuinely appreciated it. What happened was this...

When I did the Angular port, the localstorage code was in the todoService.js file. Very isolated. A simple get and put method. Nothing fancy. Making the change was trivial.

The EmberJS todo, on the other hand, used ember-data which is WAY more complex then a simple 10 line service.

So I'm struggling to get up to speed on that. Spelunking through unfamiliar code. There are some conventions I need to get up to speed on. And then there was this bug I wrote that made it seem harder than it needed to. I got all bogged down in some crap that didn't move me forward with the task ... and in the end it seemed much more difficult.

**But here's the upside:** Now that I've made the hoodie data adapter, as crappy as it may be, it can be used by anyone. I mean, it shouldn't be used by anyone. **Seriously**. But it _could_ be. The reward of that extra hassle was a reusable thing. That's pretty cool.

**Angular**: really quickly solve my immediate problem once. **Ember**: moderately quickly solve everyone's problem, ever.

I think Rob Conery says it pretty well in the video I linked earlier. [Here](http://vimeo.com/68215606#t=3303 "His soul has been crushed here."). Steep curve. Big payoff.

At the end of the day these are both pretty trivial examples. I changed 30 lines of code and then wrote like 1400 words talking about it.
