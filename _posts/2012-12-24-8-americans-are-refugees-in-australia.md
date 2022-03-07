---
title: "8 Americans are Refugees in Australia"
date: "2012-12-24"
categories: 
  - "data"
tags: 
  - "data"
  - "tableau"
---

**Update:** I learned how to do what I wanted but the answer sucks. Thankfully in version 8 the story is much better. So for version 7 I used a duplicated data source with distinct global filters.

While learning more about [Tableau](http://tableausoftware.com "Tableau") I have been putting together some visualizations that make use of actions and filters. Basic stuff, I know.

To that end I used a [UNHCR dataset](http://data.un.org/Data.aspx?d=UNHCR&f=indID%3aType-Ref#UNHCR) from the UNdata site. This data set shows the number of refugees by country or origin and asylum.

My biggest surprise so far? 8 Americans are refugees in Australia (which answers what I'll be googling about tonight).

Here is the current visualization. At the moment I'm only half-happy with it. Right now I can answer the question "Where do the refugees in XXX come from?" but I also want to also be able to answer "Where do the refugees from XXX go to?" without creating a second dashboard or chart on the dashboard. (I'm open to suggestions).

So here it is (updated)

<iframe src="http://public.tableausoftware.com/views/Asylum2/RefugeesbyCountryofAsylum?:embed=y" height="875" width="650" frameborder="0" scrolling="no"></iframe>

Data source: [data.un.org](http://data.un.org/Data.aspx?d=UNHCR&f=indID%3aType-Ref#UNHCR)

Feedback greatly appreciated.
