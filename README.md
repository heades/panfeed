# Panfeed - An RSS Feed Manager for Pandoc-Based Websites

If one builds their site using pure Pandoc and not a more hefty
solution like Hakyll, Heckle or Jeykll then they are left wondering
how to manage their RSS feed with as little fuss as possible.  Born
out of this frustration was Panfeed.

## Create a new empty RSS feed:

```panfeed --new --title="The most bestest website" --url=http://bestest.com bestestFeed.rss```

## Add a new entry to an existing RSS feed:

Suppose we have written an amazing blog-post in Markdown using a Yaml
block (this assumes you generate your HTML using the `-s` option with
Pandoc) with at least the following:

```
---
title: "The most amazingest post"
date: 2020-12-04
abstract: "About my amazingest advertures through the ether."
--

content
```

Then we can use panfeed to add this new post to your existing feed `bestestFeed.rss`:

```
panfeed --add=2020-12-04-Amazingest-Adventures.md --post-path=posts bestestFeed.rss
```

The option `--post-path` adds the releative path to where
`2020-12-04-Amazingest-Adventures.html` is located on your site.
Panfeed first grabs the URL to your website from `bestestFeed.rss`
and records the new entry as located on your site at
`http://bestest.com/posts/2020-12-04-Amazingest-Adventures.html`.

## Install Panfeed

Simply clone this repo and run `stack build && stack install`, but
this assumes [https://docs.haskellstack.org/en/stable/README/](stack)
in installed.

Requires that `pkg-config` is installed on the build system.