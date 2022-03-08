---
title: "Firebase .NET client updated"
date: "2015-04-21"
categories: 
  - "Programming#Firebase"
---

I updated the [.NET client for Firebase](https://www.nuget.org/packages/FirebaseSharp/) to fix a bug related to async calls and synchronization contexts.

The specific case was that a user was writing an application that connected to the [Nest API](https://developer.nest.com/) (which uses Firebase) and the call to GetStreaming was never firing events. While I don't know for sure, I suspect that, in fact, the call to GetStreaming was never returning and that they were calling it from their UI thread.

The bug was that the awaits were not using ConfigureAwait(false) - which [they should have been](https://msdn.microsoft.com/en-us/magazine/jj991977.aspx).

In addition to the bug fix, there is a new sample app that connects to Nest to verify that streaming is working.
