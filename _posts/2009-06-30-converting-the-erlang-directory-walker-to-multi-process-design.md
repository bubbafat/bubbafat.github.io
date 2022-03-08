---
title: "Converting the Erlang directory walker to multi-process design"
date: "2009-06-30"
categories: 
  - "erlang"
tags: 
  - "erlang"
---

In my [previous post](http://www.roberthorvick.com/2009/06/29/walking-the-directory-tree-in-erlang/) I wrote a simple file system walker using Erlang for the purposes of getting my head around the syntax.  To start thinking more in an Erlang mindset (though in a _very_ contrived manner) in this post I convert the file system walker to use two processes.  The first process ("walk") performs the file system walking and the second ("visit") processes each found item (i.e. prints the full item path name).

Something along the lines of this...

![walk-visit](/images/archive/walk-visit1.png "walk-visit")

Since walk and visit are the process entry points they need to be in the export list (by the way - forgetting to do this does not cause a compiler error in erl - why is that?) and we need a new entry function to create the new processes (start/1).

```erlang
start(Path) ->
	Visit\_PID = spawn(walkproc, visit, \[\]),
	spawn(walkproc, walk, \[Path, Visit\_PID\]).
```

On line 2 we create the process whose pid is assigned to Visit\_PID. This process calls the visit() function with 0 parameters. At this point visit is running and waiting to receive a message.

On line 3 we spawn off another process. This process calls the walk/2 function as walk(Path, Visit\_PID). Since the walk function is doing the file system walking it makes sense that it will be the one sending the first message. Because of this it needs to know the PID of the process to send the message to.

visit is a very straight forward function. It receives a message, processes it, sends a response and recurses (rinse and repeat).

```erlang
visit() ->
	receive
		{Path, Walk\_PID} ->
			io:format("~s~n", \[Path\]),
			Walk\_PID ! next,
			visit()
	end.
```

walk is very similar (and has not changed substantially from our previous version. it starts by firing a message off to visit with the Path passed as a parameter. Next it waits for the "next" message. Once it receives that it gets the next file system entry and recurses.

```erlang
walk(Path, Visit\_PID) ->
	Visit\_PID ! {Path, self()},
	receive
		next ->
			FileType = file\_type(Path),
			case FileType of
				file ->
					ok;
				symlink ->
					ok;
				directory ->
					Children = filelib:wildcard(Path ++ "/\*"),
					lists:foreach(fun(P) -> walk(P, Visit\_PID) end, Children)
			end
	end.
```

The other methods (file\_type and is\_symlink) have not changed.

I enjoyed how easy it was to convert to a multi-process approach and am looking forward to moving to a solution that uses RabbitMQ.
