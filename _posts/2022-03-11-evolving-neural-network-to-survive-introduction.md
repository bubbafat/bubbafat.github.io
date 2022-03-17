---
title: 'Trying to Survive: An Evolving Neural Network'
subtitle: 'The first post in a series where we''ll work through building an evolving network of nodes fighting for survival.'
description: 'The first post in a series where we''ll work through building an evolving network of nodes fighting for survival.'
date: 2022-03-11
featured_image: '/images/post/evolve/gen-1000-graph.webp'
categories: 
  - "Programming#Evolve"
---

_This is the first in a short series of posts heavily inspired by the YouTube video [I programmed some creatures. They Evolved.](https://www.youtube.com/watch?v=N3tRFayqVtk) by [davidrandallmiller](https://www.youtube.com/channel/UCfqPst3x4TrBUsYAF83uOBg)._

{% include toc.html %}

<h2>The Big Idea</h2>

We start with a 128x128 grid populated with 1000 [nodes](https://github.com/bubbafat/evolve/blob/master/evolve/Node.cs).  Each node has a randomly generated neural network comprised of [sensors](https://github.com/bubbafat/evolve/blob/master/evolve/Sensor.cs) like "distance from eastern wall" and "local population", [inner neurons](https://github.com/bubbafat/evolve/blob/master/evolve/InnerNeuron.cs), and [actions](https://github.com/bubbafat/evolve/blob/master/evolve/Action.cs) such as "move north", "move random", or "kill node in the way".  

Those sensors, inner neurons, and actions are joined into pairs we'll call [genes](https://github.com/bubbafat/evolve/blob/master/evolve/Gene.cs) and a collection of genes will be a [genome](https://github.com/bubbafat/evolve/blob/master/evolve/Genome.cs).  Each [generation](https://github.com/bubbafat/evolve/blob/master/evolve/Simulation.cs) will last 300 "steps" (this is to allow creating 10-second long videos at 30 frames per second).  At the end of each generation the nodes that are in the "breeding area" survive and the rest do not.  Surviving nodes are randomly combined into pairs, their [genes are mixed](https://github.com/bubbafat/evolve/blob/master/evolve/NetworkBuilder.cs) (and rarely mutated), and then children are created and randomly placed throughout the grid.  Sometimes there will be obstructions ("walls") that get in the way of nodes.

The nodes that make it to the breeding zone get to pass along their genetics and over time those traits will become more and more prevelent causing the survival rates to increase.

Here's a video of generation 1 - notice that there is a breeding zone highlighted on the left side of the grid, there are 1000 nodes, and two walls.  As the video runs notice that some nodes move to the walls and to the center (actions), some don't move (their "stay put" action might be heavily weighted), or they might move randomly.  Notice, also, that the nodes have a variety of colors.  The color of the node is determined by it's genetics so two nodes similar colors are (probably) similar in genetics.

<iframe src="https://player.vimeo.com/video/687328945?h=05d261afea&amp;badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479" width="1080" height="1080" frameborder="0" allow="autoplay; fullscreen; picture-in-picture" allowfullscreen title="gen-1"></iframe>

| Stat | Value |
|------|-------|
| Population | 1000 |
| Survivers | 132 (13.2%) |
| Steps | 300 |
| Genes | 4 |
| Inner Neurons (max) | 1 |

With high genetic diversity there very few similar nodes, but the most common (2% of nodes) was:

![Most common node structure in generation 1](/images/post/evolve/gen-1-graph.webp)

And after more generations (this is generation 1000 but it stablized far earlier).

<iframe src="https://player.vimeo.com/video/687328951?h=916f7eb644&amp;badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479" width="1080" height="1080" frameborder="0" allow="autoplay; fullscreen; picture-in-picture" allowfullscreen title="gen-1000"></iframe>

| Stat | Value |
|------|-------|
| Population | 1000 |
| Survivers | 824 (82.4%) |
| Steps | 300 |
| Genes | 4 |
| Inner Neurons (max) | 1 |

A few things to notice in this video:

- The node colors have far less variability (meaning less genetic diversity)
- The nodes move quickly towards the breeding area
- Some nodes that get stuck behind the wall will move randomly until they get around it but some will not

The most common genetic structure is:

![Most common node structure in generation 1](/images/post/evolve/gen-1000-graph.webp)

This makes sense - to survive the nodes need to move to the west.  For most nodes that is the only survival trait that matters so it becomes the predominate trait quickly.

With this brief introduce into the project, over the next couple of posts I'll talk a little more about the implementation of the solution. I'm letting you know now, though, I did not do this using matrices. The nodes, genes, and genomes are all just normal .NET objects (implemented in C#) - so if you're hoping to learn about how neural networks work I would suggest another blog series. But if you're curious how we get from a small number of randomly surviving nodes to a generation of survivers that do what it takes to live please join me!

