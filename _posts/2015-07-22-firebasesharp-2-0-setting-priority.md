---
title: "FirebaseSharp 2.0 - Setting Priority"
date: "2015-07-22"
categories: 
  - "Programming#Firebase"
---

Since we can [order by priority](http://www.roberthorvick.com/blog/firebasesharp-2-0-orderbypriority/), it sure would be nice to be able to set priorities with FirebaseSharp.

And now we can!

```csharp
var root = app.Child("/");

// now update the priorites
root.Child("aaa").SetWithPriority("{}", 3);
root.Child("bbb").SetWithPriority("{}", 2);
root.Child("ccc").SetWithPriority("{}", 1)
```

In this example we end up with a tree that looks like this:

```json
{
  'aaa': {
    '.priority': 3
  },
  'bbb': {
    '.priority': 2
  },
  'ccc': {
    '.priority': 1
  },
}
```

Now we can order the values by priority and see that they are returned in priority, not key lexical, order.

```csharp
root.OrderByPriority().Once("value", (snap, child, context) => {
  var children = snap.Children.ToArray();
  Assert.AreEqual("ccc", children\[0\].Key);
  Assert.AreEqual(1, float.Parse(children\[0\].GetPriority().Value));
  Assert.AreEqual("bbb", children\[1\].Key);
  Assert.AreEqual(2, float.Parse(children\[1\].GetPriority().Value));
  Assert.AreEqual("aaa", children\[2\].Key);
  Assert.AreEqual(3, float.Parse(children\[2\].GetPriority().Value));
});
```

Also notice that you can now use the IDataSnapshot::GetPriority method to get the priority information from the snapshot (and it's children).

In this example I used SetWithPriority - but you can also use SetPriority to add a priority to an existing query location. For example:

```csharp
root.Child("aaa").SetPriority(3);
root.Child("bbb").SetPriority(2);
root.Child("ccc").SetPriority(1);
```

In this example we are setting the priority, over-writing any existing priority, at the query location.

See Also:

[getPriority](https://www.firebase.com/docs/web/api/datasnapshot/getpriority.html)

[setWithPriority](https://www.firebase.com/docs/web/api/firebase/setwithpriority.html)

[setPriority](https://www.firebase.com/docs/web/api/firebase/setpriority.html)
